#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop("用法: Rscript pairwise_regression.R <数据文件.csv> <因变量> <自变量列表(逗号分隔)> <输出文件.csv>")
}
data_file <- args[1]
dep_var <- args[2]
ind_vars <- strsplit(args[3], ",")[[1]]
output_file <- args[4]

cat("\n========== 手动成对删除 OLS 回归 ==========\n")
cat("数据文件:", data_file, "\n")
cat("因变量:", dep_var, "\n")
cat("自变量:", paste(ind_vars, collapse=", "), "\n")

data <- read.csv(data_file)
cat("数据维度:", dim(data), "\n")
cat("缺失值统计:\n")
print(colSums(is.na(data)))

# 1. 计算成对协方差矩阵和均值向量
cov_matrix <- cov(data, use = "pairwise.complete.obs")
mean_vec <- colMeans(data, na.rm = TRUE)

# 2. 提取相关部分
# 自变量协方差矩阵 Sxx
Sxx <- cov_matrix[ind_vars, ind_vars]
# 因变量与自变量的协方差向量 Sxy
Sxy <- cov_matrix[ind_vars, dep_var]
# 因变量方差
var_y <- cov_matrix[dep_var, dep_var]
# 自变量均值向量
x_means <- mean_vec[ind_vars]
y_mean <- mean_vec[dep_var]

# 3. 求解回归系数（无截距）
beta <- solve(Sxx, Sxy)  # OLS 系数

# 4. 计算截距
intercept <- y_mean - sum(x_means * beta)

# 5. 有效样本数（因变量非缺失）
n_eff <- sum(!is.na(data[, dep_var]))
k <- length(ind_vars)
df <- n_eff - k - 1

# 6. 计算残差方差（基于因变量非缺失样本，自变量缺失时用均值填充）
# 这里使用所有因变量非缺失的样本，对于缺失的自变量，用其均值填充（成对删除逻辑）
y_obs <- data[!is.na(data[, dep_var]), dep_var]
X_obs <- data[!is.na(data[, dep_var]), ind_vars]
# 用均值填充缺失
for (i in 1:k) {
    col_mean <- x_means[i]
    X_obs[is.na(X_obs[, i]), i] <- col_mean
}
# 添加常数项
X_obs <- cbind(1, X_obs)
coefs <- c(intercept, beta)
y_pred <- X_obs %*% coefs
resid <- y_obs - y_pred
SSE <- sum(resid^2)
MSE <- SSE / df

# 7. 系数协方差矩阵（OLS 标准误）
# 使用 Sxx 和 MSE 计算
var_beta <- MSE * solve(Sxx)
se_beta <- sqrt(diag(var_beta))
var_intercept <- MSE * (1/n_eff + x_means %*% solve(Sxx) %*% x_means)
se_intercept <- sqrt(var_intercept)

# 8. 组合标准误
se <- c(se_intercept, se_beta)

# 9. 计算 t 值和 p 值
tvals <- coefs / se
pvals <- 2 * pt(-abs(tvals), df = df)

# 10. 置信区间 (95%)
ci_lower <- coefs - qt(0.975, df) * se
ci_upper <- coefs + qt(0.975, df) * se

# 11. R² 和调整 R²
SST <- sum((y_obs - mean(y_obs))^2)
SSR <- SST - SSE
r2 <- SSR / SST
adj_r2 <- 1 - (1 - r2) * (n_eff - 1) / df

# 12. F 统计量
f_stat <- (r2 / k) / ((1 - r2) / df)
f_pvalue <- pf(f_stat, k, df, lower.tail = FALSE)

# 13. 回归标准误
std_error <- sqrt(MSE)

# 14. 标准化系数 Beta (如果需要)
# 使用有效样本的标准差
y_sd <- sd(y_obs)
x_sd <- apply(X_obs[, -1], 2, sd, na.rm = TRUE)
beta_std <- beta * (x_sd / y_sd)
# 截距的标准化系数不定义
beta_std_full <- c(NA, beta_std)

# 15. 输出结果
cat("\n===== 回归结果 =====\n")
cat("Intercept:", intercept, "\n")
cat("Coefficients:\n")
print(beta)
cat("R²:", r2, "\n")
cat("Adj R²:", adj_r2, "\n")
cat("F statistic:", f_stat, ", p-value:", f_pvalue, "\n")
cat("Std. Error:", std_error, "\n")
cat("Effective sample size:", n_eff, "\n")

# 构建输出数据框
coef_names <- c("const", ind_vars)
result <- data.frame(
  variable = coef_names,
  coef = coefs,
  se = se,
  t = tvals,
  p = pvals,
  ci_lower = ci_lower,
  ci_upper = ci_upper,
  beta = beta_std_full,
  stringsAsFactors = FALSE
)
write.csv(result, output_file, row.names = FALSE)

# 模型统计量
stats_file <- sub(".csv", "_stats.csv", output_file)
stats_df <- data.frame(
  r2 = r2,
  adj_r2 = adj_r2,
  f_value = f_stat,
  f_pvalue = f_pvalue,
  nobs = n_eff,
  std_error = std_error
)
write.csv(stats_df, stats_file, row.names = FALSE)

cat("\n========== 完成 ==========\n")
