#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop("用法: Rscript pairwise_regression.R <数据文件.csv> <因变量> <自变量列表(逗号分隔)> <输出文件.csv>")
}
data_file <- args[1]
dep_var <- args[2]
ind_vars <- strsplit(args[3], ",")[[1]]
output_file <- args[4]

cat("\n========== 成对删除回归（逐系数标准误，精确匹配 SPSS）==========\n")
cat("数据文件:", data_file, "\n")
cat("因变量:", dep_var, "\n")
cat("自变量:", paste(ind_vars, collapse=", "), "\n")

# ---------- 读取数据 ----------
data <- read.csv(data_file, stringsAsFactors = FALSE)
for (col in c(dep_var, ind_vars)) {
    if (!is.numeric(data[[col]])) {
        data[[col]] <- as.numeric(as.character(data[[col]]))
    }
}

# ---------- 1. 成对协方差矩阵和均值 ----------
cov_matrix <- cov(data, use = "pairwise.complete.obs")
mean_vec <- colMeans(data, na.rm = TRUE)

# ---------- 2. 提取子矩阵 ----------
Sxx <- cov_matrix[ind_vars, ind_vars]
Sxy <- cov_matrix[ind_vars, dep_var]
var_y <- cov_matrix[dep_var, dep_var]
x_means <- mean_vec[ind_vars]
y_mean <- mean_vec[dep_var]

# ---------- 3. 回归系数 ----------
beta <- solve(Sxx, Sxy)
intercept <- y_mean - sum(x_means * beta)
coefs <- c(intercept, beta)
names(coefs) <- c("const", ind_vars)

# ---------- 4. 有效样本量（因变量非缺失） ----------
n_eff <- sum(!is.na(data[, dep_var]))
n_total <- nrow(data)
k <- length(ind_vars)
p <- k + 1  # 参数个数（含截距）

cat("\n--- 基本统计量 ---\n")
cat("总样本数 (n_total):", n_total, "\n")
cat("因变量有效样本数 (n_eff):", n_eff, "\n")
cat("自变量个数 (k):", k, "\n")
cat("模型参数个数 (p):", p, "\n")

# ---------- 5. 全局 R² ----------
pred_var <- t(beta) %*% Sxx %*% beta
r2 <- as.numeric(pred_var / var_y)
if (r2 < 0) r2 <- 0
if (r2 > 1) r2 <- 1
cat("R² =", r2, "\n")

# ---------- 6. 构造 X'X 矩阵（使用 n_total-1 缩放，与SPSS常见做法一致） ----------
# X'X = (n_total - 1) * [1, x_mean'; x_mean, Sxx]
XpX <- (n_total - 1) * rbind(
    c(1, x_means),
    cbind(x_means, Sxx)
)
rownames(XpX) <- colnames(XpX) <- c("const", ind_vars)
invXpX <- solve(XpX)   # 全局 (X'X)^{-1}

cat("\n--- X'X 矩阵 (缩放到 n_total-1) 的对角元素 ---\n")
print(diag(XpX))

cat("\n--- (X'X)^{-1} 的对角元素 ---\n")
print(diag(invXpX))

# ---------- 7. 逐系数计算 MSE 和标准误 ----------
se <- numeric(p)
names(se) <- c("const", ind_vars)

# 7a. 截距的标准误：使用因变量非缺失样本（无自变量缺失限制）
valid_y <- !is.na(data[[dep_var]])
n_y <- sum(valid_y)
y_sub <- data[[dep_var]][valid_y]
X_sub <- cbind(1, as.matrix(data[valid_y, ind_vars]))
pred_y <- X_sub %*% coefs
res_y <- y_sub - pred_y
MSE_y <- sum(res_y^2) / (n_y - p)   # 自由度 n_y - p
se["const"] <- sqrt(MSE_y * invXpX["const", "const"])

cat("\n--- 截距计算 ---\n")
cat("有效样本数 (y非缺失):", n_y, "\n")
cat("残差平方和:", sum(res_y^2), "\n")
cat("MSE =", MSE_y, "\n")
cat("invXpX[const,const] =", invXpX["const", "const"], "\n")
cat("标准误 =", se["const"], "\n")

# 7b. 每个自变量
for (i in seq_along(ind_vars)) {
    var <- ind_vars[i]
    # 筛选因变量和该自变量均非缺失
    valid <- !is.na(data[[dep_var]]) & !is.na(data[[var]])
    n_i <- sum(valid)
    if (n_i == 0) {
        warning(paste("变量", var, "没有有效样本，标准误设为NA"))
        se[var] <- NA
        next
    }
    # 使用同样的系数预测该子集
    X_i <- cbind(1, as.matrix(data[valid, ind_vars]))
    y_i <- data[[dep_var]][valid]
    pred_i <- X_i %*% coefs
    res_i <- y_i - pred_i
    MSE_i <- sum(res_i^2) / (n_i - p)   # 自由度 n_i - p
    se[var] <- sqrt(MSE_i * invXpX[var, var])
    
    cat("\n--- 自变量", var, " ---\n")
    cat("有效样本数 (y和", var, "均非缺失):", n_i, "\n")
    cat("残差平方和:", sum(res_i^2), "\n")
    cat("MSE =", MSE_i, "\n")
    cat("invXpX[", var, ",", var, "] =", invXpX[var, var], "\n")
    cat("标准误 =", se[var], "\n")
}

# ---------- 8. t 检验（自由度使用 n_eff - p，与SPSS习惯一致） ----------
df <- n_eff - p   # 因变量有效样本量减去参数个数
tvals <- coefs / se
pvals <- 2 * pt(-abs(tvals), df = df)
ci_lower <- coefs - qt(0.975, df) * se
ci_upper <- coefs + qt(0.975, df) * se

# ---------- 9. 标准化系数 ----------
y_sd <- sqrt(var_y)
x_sd <- sqrt(diag(Sxx))
beta_std <- beta * (x_sd / y_sd)
beta_std_full <- c(NA, beta_std)
names(beta_std_full) <- c("const", ind_vars)

# ---------- 10. 模型统计量（调整R²，F） ----------
adj_r2 <- 1 - (1 - r2) * (n_eff - 1) / df
f_stat <- (r2 / k) / ((1 - r2) / df)
f_pvalue <- pf(f_stat, k, df, lower.tail = FALSE)
std_error_global <- sqrt(var_y * (1 - r2))   # 全局残差标准差，仅参考

# ---------- 11. 输出结果 ----------
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

# 统计量汇总
stats_file <- sub("\\.[^.]*$", "_stats.csv", output_file)
stats_df <- data.frame(
    r2 = r2,
    adj_r2 = adj_r2,
    f_value = f_stat,
    f_pvalue = f_pvalue,
    nobs = n_eff,
    std_error = std_error_global
)
write.csv(stats_df, file = stats_file, row.names = FALSE)

cat("\n========== 完成 ==========\n")
cat("结果已写入:", output_file, "\n")
cat("统计量已写入:", stats_file, "\n")
