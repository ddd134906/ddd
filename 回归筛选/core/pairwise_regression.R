#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop("用法: Rscript pairwise_regression.R <数据文件.csv> <因变量> <自变量列表(逗号分隔)> <输出文件.csv>")
}
data_file <- args[1]
dep_var <- args[2]
ind_vars <- strsplit(args[3], ",")[[1]]
output_file <- args[4]

cat("\n========== 手动成对删除 OLS 回归（协方差矩阵法，标准误使用 n_eff-1）==========\n")
cat("数据文件:", data_file, "\n")
cat("因变量:", dep_var, "\n")
cat("自变量:", paste(ind_vars, collapse=", "), "\n")

data <- read.csv(data_file, stringsAsFactors = FALSE)
for (col in c(dep_var, ind_vars)) {
    if (!is.numeric(data[[col]])) {
        data[[col]] <- as.numeric(as.character(data[[col]]))
    }
}

# 计算成对协方差矩阵和均值向量
cov_matrix <- cov(data, use = "pairwise.complete.obs")
mean_vec <- colMeans(data, na.rm = TRUE)

# 提取相关部分
Sxx <- cov_matrix[ind_vars, ind_vars]
Sxy <- cov_matrix[ind_vars, dep_var]
var_y <- cov_matrix[dep_var, dep_var]
x_means <- mean_vec[ind_vars]
y_mean <- mean_vec[dep_var]

# 系数
beta <- solve(Sxx, Sxy)
intercept <- y_mean - sum(x_means * beta)

# 有效样本量（因变量非缺失）
n_eff <- sum(!is.na(data[, dep_var]))
k <- length(ind_vars)
df <- n_eff - k - 1

# R²
pred_var <- t(beta) %*% Sxx %*% beta
r2 <- as.numeric(pred_var / var_y)
if (r2 < 0) r2 <- 0
if (r2 > 1) r2 <- 1

# 残差方差
MSE <- var_y * (1 - r2)

# 标准误（使用 n_eff - 1）
var_beta <- MSE * solve(Sxx) / (n_eff - 1)
se_beta <- sqrt(diag(var_beta))
var_intercept <- MSE * (1/n_eff + x_means %*% solve(Sxx) %*% x_means / (n_eff - 1))
se_intercept <- sqrt(var_intercept)

coefs <- c(intercept, beta)
se <- c(se_intercept, se_beta)

# t 检验
tvals <- coefs / se
pvals <- 2 * pt(-abs(tvals), df = df)

# 置信区间
ci_lower <- coefs - qt(0.975, df) * se
ci_upper <- coefs + qt(0.975, df) * se

# 标准化系数
y_sd <- sqrt(var_y)
x_sd <- sqrt(diag(Sxx))
beta_std <- beta * (x_sd / y_sd)
beta_std_full <- c(NA, beta_std)

adj_r2 <- 1 - (1 - r2) * (n_eff - 1) / df
f_stat <- (r2 / k) / ((1 - r2) / df)
f_pvalue <- pf(f_stat, k, df, lower.tail = FALSE)

cat("\n===== 回归结果 =====\n")
cat("Intercept:", intercept, "\n")
cat("Coefficients:\n")
print(beta)
cat("R²:", r2, "\n")
cat("Adj R²:", adj_r2, "\n")
cat("F statistic:", f_stat, ", p-value:", f_pvalue, "\n")
cat("Std. Error of estimate:", sqrt(MSE), "\n")
cat("Effective sample size (n_eff):", n_eff, "\n")

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
write.csv(result, file = output_file, row.names = FALSE)

stats_file <- sub("\\.[^.]*$", "_stats.csv", output_file)
stats_df <- data.frame(
  r2 = r2,
  adj_r2 = adj_r2,
  f_value = f_stat,
  f_pvalue = f_pvalue,
  nobs = n_eff,
  std_error = sqrt(MSE)
)
write.csv(stats_df, file = stats_file, row.names = FALSE)

cat("\n========== 完成 ==========\n")
