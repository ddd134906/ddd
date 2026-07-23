#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop("Usage: Rscript pairwise_regression.R <data.csv> <dep_var> <ind_vars(comma)> <output.csv>")
}
data_file <- args[1]
dep_var <- args[2]
ind_vars <- strsplit(args[3], ",")[[1]]
output_file <- args[4]

options(digits = 15)
cat("\n========== 成对删除 OLS（调试版）==========\n")
cat("数据文件:", data_file, "\n")
cat("因变量:", dep_var, "\n")
cat("自变量:", paste(ind_vars, collapse=", "), "\n")

data <- read.csv(data_file, stringsAsFactors = FALSE)
for (col in c(dep_var, ind_vars)) {
    if (!is.numeric(data[[col]])) data[[col]] <- as.numeric(as.character(data[[col]]))
}

valid_y <- !is.na(data[[dep_var]])
n_eff <- sum(valid_y)
n_total <- nrow(data)
k <- length(ind_vars)

cat("总样本量 (n_total):", n_total, "\n")
cat("因变量有效样本 (n_eff):", n_eff, "\n")

sub_data <- data[valid_y, ]
y_sub <- sub_data[[dep_var]]
X_sub <- as.matrix(sub_data[, ind_vars])
X_total <- as.matrix(data[, ind_vars])

x_means_total <- colMeans(X_total)
y_mean_sub <- mean(y_sub)

Sxx <- cov(X_total)
Sxy <- cov(X_sub, y_sub)
Syy <- var(y_sub)

beta <- solve(Sxx, Sxy)
intercept <- y_mean_sub - sum(x_means_total * beta)
coefs <- c(intercept, beta)
names(coefs) <- c("const", ind_vars)

pred_sub <- cbind(1, X_sub) %*% coefs
res_sub <- y_sub - pred_sub
MSE <- sum(res_sub^2) / (n_eff - k - 1)

Sxx_inv <- solve(Sxx)

# === 计算两种标准误 ===
# 方法1：分母 n_eff - 1 (当前)
se_beta1 <- sqrt(MSE * diag(Sxx_inv) / (n_eff - 1))
se_intercept1 <- sqrt(MSE * (1/n_eff + x_means_total %*% Sxx_inv %*% x_means_total / (n_eff - 1)))
se1 <- c(se_intercept1, se_beta1)

# 方法2：分母 n_eff
se_beta2 <- sqrt(MSE * diag(Sxx_inv) / n_eff)
se_intercept2 <- sqrt(MSE * (1/n_eff + x_means_total %*% Sxx_inv %*% x_means_total / n_eff))
se2 <- c(se_intercept2, se_beta2)

cat("\n--- 方法1 (n_eff-1) 标准误 ---\n")
print(se1)
cat("\n--- 方法2 (n_eff) 标准误 ---\n")
print(se2)

# 输出中间矩阵（用于SPSS对比）
write.csv(Sxx, "debug_Sxx.csv", row.names = FALSE)
write.csv(Sxy, "debug_Sxy.csv", row.names = FALSE)
write.csv(Sxx_inv, "debug_Sxx_inv.csv", row.names = FALSE)
write.csv(data.frame(MSE = MSE, n_eff = n_eff), "debug_MSE.csv", row.names = FALSE)

# 继续使用方法1作为最终输出（您可改为方法2）
se <- se1
names(se) <- c("const", ind_vars)

df <- n_eff - k - 1
tvals <- coefs / se
pvals <- 2 * pt(-abs(tvals), df = df)
ci_lower <- coefs - qt(0.975, df) * se
ci_upper <- coefs + qt(0.975, df) * se

y_sd <- sqrt(Syy)
x_sd <- sqrt(diag(Sxx))
beta_std <- beta * (x_sd / y_sd)
beta_std_full <- c(NA, beta_std)
names(beta_std_full) <- c("const", ind_vars)

R2 <- as.numeric(t(beta) %*% Sxx %*% beta / Syy)
adj_R2 <- 1 - (1 - R2) * (n_eff - 1) / (n_eff - k - 1)
f_stat <- (R2 / k) / ((1 - R2) / (n_eff - k - 1))
f_pvalue <- pf(f_stat, k, n_eff - k - 1, lower.tail = FALSE)
std_error <- sqrt(MSE)

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
cat("Std. Error of estimate:", std_error, "\n")
cat("\n系数与标准误 (方法1):\n")
print(data.frame(variable = names(coefs), coef = coefs, se = se))
cat("\n调试文件已生成：debug_Sxx.csv, debug_Sxy.csv, debug_Sxx_inv.csv, debug_MSE.csv\n")
cat("\n========== 完成 ==========\n")
