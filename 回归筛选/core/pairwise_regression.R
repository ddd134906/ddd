#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop("用法: Rscript pairwise_regression.R <数据文件.csv> <因变量> <自变量列表(逗号分隔)> <输出文件.csv>")
}
data_file <- args[1]
dep_var <- args[2]
ind_vars <- strsplit(args[3], ",")[[1]]
output_file <- args[4]

if (!require(lavaan)) {
    install.packages("lavaan", repos = "http://cran.r-project.org")
    library(lavaan)
}

data <- read.csv(data_file)
cat("数据维度:", dim(data), "\n")
cat("因变量:", dep_var, "\n")
cat("自变量:", paste(ind_vars, collapse=", "), "\n")

# 1. 成对协方差和均值
cov_matrix <- cov(data, use = "pairwise.complete.obs")
mean_vec <- colMeans(data, na.rm = TRUE)
cat("协方差矩阵维度:", dim(cov_matrix), "\n")
cat("协方差矩阵 (前几行):\n")
print(cov_matrix[1:min(5, nrow(cov_matrix)), 1:min(5, ncol(cov_matrix))])

formula <- paste(dep_var, "~", paste(ind_vars, collapse = " + "))
cat("模型公式:", formula, "\n")

# 2. 拟合模型
fit <- sem(formula,
           sample.cov = cov_matrix,
           sample.mean = mean_vec,
           sample.nobs = nrow(data),
           meanstructure = TRUE,
           estimator = "ML",
           test = "standard")

if (is.null(fit)) {
    stop("lavaan 拟合失败")
}

# 3. 提取原始系数
cat("\n--- 原始系数 (coef(fit)) ---\n")
print(coef(fit))

# 4. 使用 parameterEstimates 提取
est <- parameterEstimates(fit, remove.system.eq = TRUE)
cat("\n--- parameterEstimates 表 (前几行) ---\n")
print(head(est))

# 5. 构建系数向量（截距 + 自变量）
intercept_est <- est$est[est$op == "~1"]
if (length(intercept_est) == 0) {
    # 如果没有 ~1，尝试从 coef 中取 "(Intercept)"
    intercept_est <- coef(fit)["(Intercept)"]
    if (is.na(intercept_est)) intercept_est <- NA
}
names(intercept_est) <- "const"

coef_inds <- sapply(ind_vars, function(v) {
    val <- est$est[est$op == "~" & est$rhs == v]
    if (length(val) == 0) NA else val
})
names(coef_inds) <- ind_vars
coefs <- c(intercept_est, coef_inds)
cat("\n--- 组合后的系数向量 ---\n")
print(coefs)

# 6. 标准误
se_intercept <- est$se[est$op == "~1"]
if (length(se_intercept) == 0) se_intercept <- NA
names(se_intercept) <- "const"
se_inds <- sapply(ind_vars, function(v) {
    val <- est$se[est$op == "~" & est$rhs == v]
    if (length(val) == 0) NA else val
})
names(se_inds) <- ind_vars
se <- c(se_intercept, se_inds)
cat("\n--- 标准误 ---\n")
print(se)

# 7. p 值
p_intercept <- est$pvalue[est$op == "~1"]
if (length(p_intercept) == 0) p_intercept <- NA
names(p_intercept) <- "const"
p_inds <- sapply(ind_vars, function(v) {
    val <- est$pvalue[est$op == "~" & est$rhs == v]
    if (length(val) == 0) NA else val
})
names(p_inds) <- ind_vars
pvals <- c(p_intercept, p_inds)

# 8. CI
ci_low_intercept <- est$ci.lower[est$op == "~1"]
if (length(ci_low_intercept) == 0) ci_low_intercept <- NA
names(ci_low_intercept) <- "const"
ci_low_inds <- sapply(ind_vars, function(v) {
    val <- est$ci.lower[est$op == "~" & est$rhs == v]
    if (length(val) == 0) NA else val
})
names(ci_low_inds) <- ind_vars
ci_lower <- c(ci_low_intercept, ci_low_inds)

ci_up_intercept <- est$ci.upper[est$op == "~1"]
if (length(ci_up_intercept) == 0) ci_up_intercept <- NA
names(ci_up_intercept) <- "const"
ci_up_inds <- sapply(ind_vars, function(v) {
    val <- est$ci.upper[est$op == "~" & est$rhs == v]
    if (length(val) == 0) NA else val
})
names(ci_up_inds) <- ind_vars
ci_upper <- c(ci_up_intercept, ci_up_inds)

# 9. 标准化系数
std_sol <- standardizedSolution(fit)
beta_intercept <- std_sol$est.std[std_sol$op == "~1"]
if (length(beta_intercept) == 0) beta_intercept <- NA
names(beta_intercept) <- "const"
beta_inds <- sapply(ind_vars, function(v) {
    val <- std_sol$est.std[std_sol$op == "~" & std_sol$rhs == v]
    if (length(val) == 0) NA else val
})
names(beta_inds) <- ind_vars
beta_std <- c(beta_intercept, beta_inds)

# 10. R²
r2 <- lavInspect(fit, "r2")[dep_var]
cat("\n--- R² (lavInspect) ---\n")
print(r2)
if (is.null(r2) || is.na(r2)) {
    var_y <- cov_matrix[dep_var, dep_var]
    beta_no_const <- coef_inds
    Sxx <- cov_matrix[ind_vars, ind_vars]
    pred_var <- t(beta_no_const) %*% Sxx %*% beta_no_const
    r2 <- as.numeric(pred_var / var_y)
    if (r2 < 0) r2 <- 0
    if (r2 > 1) r2 <- 1
    cat("后备 R²:", r2, "\n")
}

# 11. 模型统计量
n <- nrow(data)
k <- length(ind_vars)
adj_r2 <- 1 - (1 - r2) * (n - 1) / (n - k - 1)
f_stat <- (r2 / k) / ((1 - r2) / (n - k - 1))
f_pvalue <- pf(f_stat, k, n - k - 1, lower.tail = FALSE)
resid_var <- (1 - r2) * cov_matrix[dep_var, dep_var]
std_error <- sqrt(resid_var)

cat("\n--- 模型统计量 ---\n")
cat("R² =", r2, "\n")
cat("调整R² =", adj_r2, "\n")
cat("F统计量 =", f_stat, "\n")
cat("F p值 =", f_pvalue, "\n")
cat("回归标准误 =", std_error, "\n")

# 12. 输出结果
result <- data.frame(
  variable = names(coefs),
  coef = coefs,
  se = se,
  t = coefs / se,
  p = pvals,
  ci_lower = ci_lower,
  ci_upper = ci_upper,
  beta = beta_std,
  stringsAsFactors = FALSE
)
write.csv(result, output_file, row.names = FALSE)

stats_file <- sub(".csv", "_stats.csv", output_file)
stats_df <- data.frame(
  r2 = r2,
  adj_r2 = adj_r2,
  f_value = f_stat,
  f_pvalue = f_pvalue,
  nobs = n,
  std_error = std_error
)
write.csv(stats_df, stats_file, row.names = FALSE)

cat("\nR 成对删除回归完成。结果已写入:", output_file, "和", stats_file, "\n")
