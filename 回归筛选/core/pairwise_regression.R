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

cat("\n========== 成对删除回归诊断 ==========\n")
cat("数据文件:", data_file, "\n")
cat("因变量:", dep_var, "\n")
cat("自变量:", paste(ind_vars, collapse=", "), "\n")

data <- read.csv(data_file)
cat("数据维度:", dim(data), "\n")
cat("缺失值统计:\n")
print(colSums(is.na(data)))

# 成对协方差矩阵和均值
cov_matrix <- cov(data, use = "pairwise.complete.obs")
mean_vec <- colMeans(data, na.rm = TRUE)

cat("\n协方差矩阵 (前5行5列):\n")
print(cov_matrix[1:min(5,nrow(cov_matrix)), 1:min(5,ncol(cov_matrix))])

cat("\n均值向量:\n")
print(mean_vec)

formula <- paste(dep_var, "~", paste(ind_vars, collapse = " + "))
cat("\n模型公式:", formula, "\n")

# 拟合模型
fit <- sem(formula,
           sample.cov = cov_matrix,
           sample.mean = mean_vec,
           sample.nobs = nrow(data),
           meanstructure = TRUE,
           estimator = "ML",
           test = "standard")

if (is.null(fit)) stop("lavaan 拟合失败")

cat("\n===== 模型拟合摘要 =====\n")
summary(fit, fit.measures=TRUE, standardized=TRUE)

# 提取因变量的截距和系数
est <- parameterEstimates(fit, remove.system.eq = TRUE)

# 截距 (因变量)
intercept_row <- est[est$lhs == dep_var & est$op == "~1", ]
if (nrow(intercept_row) == 0) {
    intercept_est <- NA; intercept_se <- NA; intercept_p <- NA; 
    intercept_ci_low <- NA; intercept_ci_up <- NA
} else {
    intercept_est <- intercept_row$est
    intercept_se <- intercept_row$se
    intercept_p <- intercept_row$pvalue
    intercept_ci_low <- intercept_row$ci.lower
    intercept_ci_up <- intercept_row$ci.upper
}

# 回归系数 (因变量)
coef_rows <- est[est$lhs == dep_var & est$op == "~", ]
rownames(coef_rows) <- coef_rows$rhs
# 按自变量顺序排列
coef_rows <- coef_rows[ind_vars, ]

if (nrow(coef_rows) != length(ind_vars)) {
    cat("警告: 系数行数与自变量数不匹配。\n")
}

cat("\n===== 因变量回归系数 (按自变量顺序) =====\n")
print(coef_rows)

# 组合成最终向量
coefs <- c(intercept_est, coef_rows$est)
se <- c(intercept_se, coef_rows$se)
pvals <- c(intercept_p, coef_rows$pvalue)
ci_lower <- c(intercept_ci_low, coef_rows$ci.lower)
ci_upper <- c(intercept_ci_up, coef_rows$ci.upper)
tvals <- coefs / se
names_all <- c("const", ind_vars)

cat("\n===== 最终系数向量 (长度 = ", length(coefs), ") =====\n")
print(coefs)

# 标准化系数
std_sol <- standardizedSolution(fit)
std_intercept <- std_sol[std_sol$lhs == dep_var & std_sol$op == "~1", "est.std"]
if (length(std_intercept) == 0) std_intercept <- NA
std_coef <- std_sol[std_sol$lhs == dep_var & std_sol$op == "~", "est.std"]
names(std_coef) <- std_sol[std_sol$lhs == dep_var & std_sol$op == "~", "rhs"]
std_coef <- std_coef[ind_vars]
beta_std <- c(std_intercept, std_coef)

cat("\n===== 标准化系数 =====\n")
print(beta_std)

# R²
r2 <- lavInspect(fit, "r2")[dep_var]
cat("\n===== R² (lavaan) =", r2, "=====\n")
if (is.null(r2) || is.na(r2)) {
    cat("R²缺失，手动计算...\n")
    var_y <- cov_matrix[dep_var, dep_var]
    beta_no_const <- coef_rows$est
    Sxx <- cov_matrix[ind_vars, ind_vars]
    pred_var <- t(beta_no_const) %*% Sxx %*% beta_no_const
    r2 <- as.numeric(pred_var / var_y)
    if (r2 < 0) r2 <- 0
    if (r2 > 1) r2 <- 1
    cat("手动计算 R² =", r2, "\n")
}

n <- nrow(data)
k <- length(ind_vars)
adj_r2 <- 1 - (1 - r2) * (n - 1) / (n - k - 1)
f_stat <- (r2 / k) / ((1 - r2) / (n - k - 1))
f_pvalue <- pf(f_stat, k, n - k - 1, lower.tail = FALSE)
resid_var <- (1 - r2) * cov_matrix[dep_var, dep_var]
std_error <- sqrt(resid_var)

cat("\n===== 模型统计量 =====\n")
cat("R²:", r2, "\n")
cat("调整R²:", adj_r2, "\n")
cat("F统计量:", f_stat, "\n")
cat("F p值:", f_pvalue, "\n")
cat("回归标准误:", std_error, "\n")
cat("样本数:", n, "\n")

# 输出系数表
result <- data.frame(
  variable = names_all,
  coef = coefs,
  se = se,
  t = tvals,
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

cat("\n========== 成对删除回归完成 ==========\n")
