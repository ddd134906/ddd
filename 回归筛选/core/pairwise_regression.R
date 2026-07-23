#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop("Usage: Rscript pairwise_regression.R <data.csv> <dep_var> <ind_vars(comma)> <output.csv>")
}
data_file <- args[1]
dep_var <- args[2]
ind_vars <- strsplit(args[3], ",")[[1]]
output_file <- args[4]

cat("\n========== 成对删除 OLS 回归（SPSS 精确版，使用成对协方差）==========\n")
cat("数据文件:", data_file, "\n")
cat("因变量:", dep_var, "\n")
cat("自变量:", paste(ind_vars, collapse=", "), "\n")

# 读取数据
data <- read.csv(data_file, stringsAsFactors = FALSE)
for (col in c(dep_var, ind_vars)) {
    if (!is.numeric(data[[col]])) {
        data[[col]] <- as.numeric(as.character(data[[col]]))
    }
}

n_total <- nrow(data)
k <- length(ind_vars)
p <- k + 1

# 1. 成对协方差矩阵和均值（基于全部数据，使用所有可用的成对观测）
cov_mat <- cov(data, use = "pairwise.complete.obs")
mean_vec <- colMeans(data, na.rm = TRUE)

# 2. 提取子矩阵
Sxx <- cov_mat[ind_vars, ind_vars]
Sxy <- cov_mat[ind_vars, dep_var]
var_y <- cov_mat[dep_var, dep_var]
x_means <- mean_vec[ind_vars]
y_mean <- mean_vec[dep_var]

# 3. 求解系数（成对删除）
beta <- solve(Sxx, Sxy)
intercept <- y_mean - sum(x_means * beta)
coefs <- c(intercept, beta)
names(coefs) <- c("const", ind_vars)

# 4. 计算每个系数的有效样本量 n_i
n_eff <- sum(!is.na(data[[dep_var]]))  # 截距的有效样本
n_eff_x <- sapply(ind_vars, function(v) sum(!is.na(data[[dep_var]]) & !is.na(data[[v]])))

# 5. 计算每个系数的残差方差 MSE_i
# 构建预测值（基于所有样本，但残差只对有效子集计算）
X_all <- cbind(1, as.matrix(data[, ind_vars]))
y_all <- data[[dep_var]]

# 全局预测（用于计算子集残差）
pred_all <- X_all %*% coefs

# 截距的 MSE
valid_const <- !is.na(y_all)
n_const <- sum(valid_const)
res_const <- y_all[valid_const] - pred_all[valid_const]
MSE_const <- sum(res_const^2) / (n_const - p)

# 每个自变量的 MSE
MSE_x <- sapply(ind_vars, function(v) {
    valid <- !is.na(y_all) & !is.na(X_all[, v])
    n_i <- sum(valid)
    if (n_i <= p) return(NA)
    res_i <- y_all[valid] - pred_all[valid]
    sum(res_i^2) / (n_i - p)
})
names(MSE_x) <- ind_vars

# 6. 计算标准误
Sxx_inv <- solve(Sxx)
diag_Sxx_inv <- diag(Sxx_inv)

# 截距标准误
se_intercept <- sqrt(MSE_const * (1/n_const + x_means %*% Sxx_inv %*% x_means / (n_const - 1)))
se_beta <- sqrt(MSE_x * diag_Sxx_inv / (n_eff_x - 1))
se <- c(se_intercept, se_beta)
names(se) <- c("const", ind_vars)

# 打印调试信息
cat("\n--- 调试信息 ---\n")
cat("n_eff (截距):", n_const, "\n")
cat("n_eff_x:\n")
print(n_eff_x)
cat("MSE_const:", MSE_const, "\n")
cat("MSE_x:\n")
print(MSE_x)
cat("diag(Sxx_inv):\n")
print(diag_Sxx_inv)
cat("se_intercept:", se_intercept, "\n")
cat("se_beta:\n")
print(se_beta)

# 7. 检验统计量
df <- n_const - p
tvals <- coefs / se
pvals <- 2 * pt(-abs(tvals), df = df)
ci_lower <- coefs - qt(0.975, df) * se
ci_upper <- coefs + qt(0.975, df) * se

# 8. 标准化系数
y_sd <- sqrt(var_y)
x_sd <- sqrt(diag(Sxx))
beta_std <- beta * (x_sd / y_sd)
beta_std_full <- c(NA, beta_std)
names(beta_std_full) <- c("const", ind_vars)

# 9. 模型统计量（R² 等）
R2 <- as.numeric(t(beta) %*% Sxx %*% beta / var_y)
adj_R2 <- 1 - (1 - R2) * (n_const - 1) / (n_const - p)
f_stat <- (R2 / k) / ((1 - R2) / (n_const - p))
f_pvalue <- pf(f_stat, k, n_const - p, lower.tail = FALSE)
std_error <- sqrt(MSE_const)  # 使用截距的 MSE 作为整体估计

# 10. 输出结果
result <- data.frame(
    variable = names(coefs),
    coef = coefs,
    se = se,
    t = tvals,
    p = pvals,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    beta = beta_std_full,
    stringsAsFactors = FALSE
)
write.csv(result, file = output_file, row.names = FALSE)

stats_file <- sub("\\.[^.]*$", "_stats.csv", output_file)
stats_df <- data.frame(
    r2 = R2,
    adj_r2 = adj_R2,
    f_value = f_stat,
    f_pvalue = f_pvalue,
    nobs = n_const,
    std_error = std_error
)
write.csv(stats_df, file = stats_file, row.names = FALSE)

cat("\n===== 回归结果 =====\n")
cat("Intercept:", coefs["const"], "\n")
cat("R²:", R2, "\n")
cat("Adj R²:", adj_R2, "\n")
cat("F statistic:", f_stat, ", p-value:", f_pvalue, "\n")
cat("Std. Error of estimate:", std_error, "\n")
cat("有效样本量 (n_eff):", n_const, "\n")
cat("\n系数与标准误:\n")
print(data.frame(variable = names(coefs), coef = coefs, se = se))

cat("\n========== 完成 ==========\n")
