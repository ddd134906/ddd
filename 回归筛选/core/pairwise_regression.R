#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop("Usage: Rscript pairwise_regression.R <data.csv> <dep_var> <ind_vars(comma)> <output.csv>")
}
data_file <- args[1]
dep_var <- args[2]
ind_vars <- strsplit(args[3], ",")[[1]]
output_file <- args[4]

cat("\n========== 成对删除 OLS 回归（SPSS 精确匹配版）==========\n")
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

# 提取有效样本（因变量非缺失）
valid_y <- !is.na(data[[dep_var]])
n_eff <- sum(valid_y)
k <- length(ind_vars)
p <- k + 1

cat("因变量有效样本数 (n_eff):", n_eff, "\n")

# 子样本数据
sub_data <- data[valid_y, ]
y <- sub_data[[dep_var]]
X_sub <- sub_data[, ind_vars]   # 不包含截距

# 1. 协方差矩阵和均值（基于子样本）
Sxx <- cov(X_sub)
Sxy <- cov(X_sub, y)
var_y <- var(y)
x_means <- colMeans(X_sub)
y_mean <- mean(y)

# 2. 回归系数
beta <- solve(Sxx, Sxy)
intercept <- y_mean - sum(x_means * beta)
coefs <- c(intercept, beta)
names(coefs) <- c("const", ind_vars)

# 3. 预测值与残差
pred <- as.matrix(cbind(1, X_sub)) %*% coefs
res <- y - pred
MSE <- sum(res^2) / (n_eff - p)
cat("MSE =", MSE, "\n")

# 4. 标准误（关键修正）
# se = sqrt( MSE * diag(Sxx^{-1}) / (n_eff - 1) )
Sxx_inv <- solve(Sxx)
se_beta <- sqrt(MSE * diag(Sxx_inv) / (n_eff - 1))
# 截距的标准误
se_intercept <- sqrt(MSE * (1/n_eff + x_means %*% Sxx_inv %*% x_means / (n_eff - 1)))
se <- c(se_intercept, se_beta)
names(se) <- c("const", ind_vars)

# 5. 检验统计量
df <- n_eff - p
tvals <- coefs / se
pvals <- 2 * pt(-abs(tvals), df = df)
ci_lower <- coefs - qt(0.975, df) * se
ci_upper <- coefs + qt(0.975, df) * se

# 6. 标准化系数 Beta
y_sd <- sqrt(var_y)
x_sd <- sqrt(diag(Sxx))
beta_std <- beta * (x_sd / y_sd)
beta_std_full <- c(NA, beta_std)
names(beta_std_full) <- c("const", ind_vars)

# 7. 模型统计量
R2 <- as.numeric(t(beta) %*% Sxx %*% beta / var_y)
adj_R2 <- 1 - (1 - R2) * (n_eff - 1) / (n_eff - p)
f_stat <- (R2 / k) / ((1 - R2) / (n_eff - p))
f_pvalue <- pf(f_stat, k, n_eff - p, lower.tail = FALSE)
std_error <- sqrt(MSE)

# 8. 输出结果
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
    nobs = n_eff,
    std_error = std_error
)
write.csv(stats_df, file = stats_file, row.names = FALSE)

cat("\n===== 回归结果 =====\n")
cat("Intercept:", coefs["const"], "\n")
cat("R²:", R2, "\n")
cat("Adj R²:", adj_R2, "\n")
cat("F statistic:", f_stat, ", p-value:", f_pvalue, "\n")
cat("Std. Error of estimate:", std_error, "\n")
cat("有效样本量 (n_eff):", n_eff, "\n")
cat("\n系数与标准误:\n")
print(data.frame(variable = names(coefs), coef = coefs, se = se))

cat("\n========== 完成 ==========\n")
