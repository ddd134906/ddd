#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop("Usage: Rscript pairwise_regression.R <data.csv> <dep_var> <ind_vars(comma)> <output.csv>")
}
data_file <- args[1]
dep_var <- args[2]
ind_vars <- strsplit(args[3], ",")[[1]]
output_file <- args[4]

cat("\n========== 成对删除 OLS 回归（精确匹配 SPSS，统一有效样本）==========\n")
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

# 1. 提取有效样本（因变量非缺失）
valid_y <- !is.na(data[[dep_var]])
n_eff <- sum(valid_y)
cat("因变量非缺失样本数 (n_eff):", n_eff, "\n")

# 2. 构建有效样本的设计矩阵 X 和 y
X <- cbind(1, as.matrix(data[valid_y, ind_vars]))
y <- data[[dep_var]][valid_y]

# 3. 计算 X'X 和 X'y（均基于有效样本）
XpX <- t(X) %*% X
Xpy <- t(X) %*% y

# 4. 求解系数
coefs <- solve(XpX, Xpy)
names(coefs) <- c("const", ind_vars)

# 5. 残差与 MSE（统一基于有效样本）
pred <- X %*% coefs
res <- y - pred
MSE <- sum(res^2) / (n_eff - p)
cat("MSE =", MSE, "\n")

# 6. 标准误（统一使用 XpX 的逆）
XpX_inv <- solve(XpX)
se <- sqrt(diag(MSE * XpX_inv))
names(se) <- c("const", ind_vars)

# 7. 检验统计量
df <- n_eff - p
tvals <- coefs / se
pvals <- 2 * pt(-abs(tvals), df = df)
ci_lower <- coefs - qt(0.975, df) * se
ci_upper <- coefs + qt(0.975, df) * se

# 8. 标准化系数（Beta）
y_sd <- sd(y)
x_sd <- apply(data[valid_y, ind_vars], 2, sd)
beta_std <- coefs[-1] * (x_sd / y_sd)
beta_std_full <- c(NA, beta_std)
names(beta_std_full) <- c("const", ind_vars)

# 9. 模型统计量（R² 等）
Sxx <- cov(data[valid_y, ind_vars])
Sxy <- cov(data[valid_y, ind_vars], y)
var_y <- var(y)
R2 <- as.numeric(t(coefs[-1]) %*% Sxx %*% coefs[-1] / var_y)
adj_R2 <- 1 - (1 - R2) * (n_eff - 1) / (n_eff - p)
f_stat <- (R2 / k) / ((1 - R2) / (n_eff - p))
f_pvalue <- pf(f_stat, k, n_eff - p, lower.tail = FALSE)
std_error <- sqrt(MSE)

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
