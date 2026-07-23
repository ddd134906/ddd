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
cat("\n========== 成对删除 OLS（SPSS 精确版）==========\n")
cat("数据文件:", data_file, "\n")
cat("因变量:", dep_var, "\n")
cat("自变量:", paste(ind_vars, collapse=", "), "\n")

data <- read.csv(data_file, stringsAsFactors = FALSE)
for (col in c(dep_var, ind_vars)) {
    if (!is.numeric(data[[col]])) data[[col]] <- as.numeric(as.character(data[[col]]))
}

# 有效样本（因变量非缺失）
valid_y <- !is.na(data[[dep_var]])
n_eff <- sum(valid_y)
n_total <- nrow(data)
k <- length(ind_vars)

cat("总样本量 (n_total):", n_total, "\n")
cat("因变量有效样本 (n_eff):", n_eff, "\n")

# 子样本（用于 Sxy, Syy, y均值, 残差）
sub_data <- data[valid_y, ]
y_sub <- sub_data[[dep_var]]
X_sub <- as.matrix(sub_data[, ind_vars])

# 全样本（用于 Sxx, x均值）
X_total <- as.matrix(data[, ind_vars])
x_means_total <- colMeans(X_total)
y_mean_sub <- mean(y_sub)

# 协方差矩阵
Sxx <- cov(X_total)                 # 自变量协方差（全样本）
Sxy <- cov(X_sub, y_sub)            # 自变量-因变量协方差（子样本）
Syy <- var(y_sub)                   # 因变量方差（子样本）

# 系数
beta <- solve(Sxx, Sxy)
intercept <- y_mean_sub - sum(x_means_total * beta)
coefs <- c(intercept, beta)
names(coefs) <- c("const", ind_vars)

# 残差与 MSE（基于子样本）
pred_sub <- cbind(1, X_sub) %*% coefs
res_sub <- y_sub - pred_sub
MSE <- sum(res_sub^2) / (n_eff - k - 1)

# 标准误（使用全样本 Sxx_inv，分母 n_eff-1）
Sxx_inv <- solve(Sxx)
se_beta <- sqrt(MSE * diag(Sxx_inv) / (n_eff - 1))
se_intercept <- sqrt(MSE * (1/n_eff + x_means_total %*% Sxx_inv %*% x_means_total / (n_eff - 1)))
se <- c(se_intercept, se_beta)
names(se) <- c("const", ind_vars)

# t 检验
df <- n_eff - k - 1
tvals <- coefs / se
pvals <- 2 * pt(-abs(tvals), df = df)
ci_lower <- coefs - qt(0.975, df) * se
ci_upper <- coefs + qt(0.975, df) * se

# 标准化系数
y_sd <- sqrt(Syy)
x_sd <- sqrt(diag(Sxx))
beta_std <- beta * (x_sd / y_sd)
beta_std_full <- c(NA, beta_std)
names(beta_std_full) <- c("const", ind_vars)

# 模型统计量
R2 <- as.numeric(t(beta) %*% Sxx %*% beta / Syy)
adj_R2 <- 1 - (1 - R2) * (n_eff - 1) / (n_eff - k - 1)
f_stat <- (R2 / k) / ((1 - R2) / (n_eff - k - 1))
f_pvalue <- pf(f_stat, k, n_eff - k - 1, lower.tail = FALSE)
std_error <- sqrt(MSE)

# 输出结果
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
cat("\n系数与标准误:\n")
print(data.frame(variable = names(coefs), coef = coefs, se = se))

cat("\n========== 完成 ==========\n")
