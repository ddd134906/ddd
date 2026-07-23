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

# 成对协方差和均值
cov_matrix <- cov(data, use = "pairwise.complete.obs")
mean_vec <- colMeans(data, na.rm = TRUE)
cat("协方差矩阵维度:", dim(cov_matrix), "\n")

formula <- paste(dep_var, "~", paste(ind_vars, collapse = " + "))
cat("模型公式:", formula, "\n")

# 拟合模型
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

# --- 直接从 parameterEstimates 提取 ---
est <- parameterEstimates(fit, remove.system.eq = TRUE)

# 截距 (op == "~1")
intercept_row <- est[est$op == "~1", ]
if (nrow(intercept_row) == 0) {
    intercept_est <- NA
    intercept_se <- NA
    intercept_p <- NA
    intercept_ci_low <- NA
    intercept_ci_up <- NA
} else {
    intercept_est <- intercept_row$est
    intercept_se <- intercept_row$se
    intercept_p <- intercept_row$pvalue
    intercept_ci_low <- intercept_row$ci.lower
    intercept_ci_up <- intercept_row$ci.upper
}

# 回归系数 (op == "~")
coef_rows <- est[est$op == "~", ]
# 按 ind_vars 顺序排序
coef_rows <- coef_rows[match(ind_vars, coef_rows$rhs), ]

# 提取各统计量
coef_vals <- coef_rows$est
se_vals <- coef_rows$se
p_vals <- coef_rows$pvalue
ci_low_vals <- coef_rows$ci.lower
ci_up_vals <- coef_rows$ci.upper

# 组合（截距在前）
coefs <- c(intercept_est, coef_vals)
se <- c(intercept_se, se_vals)
pvals <- c(intercept_p, p_vals)
ci_lower <- c(intercept_ci_low, ci_low_vals)
ci_upper <- c(intercept_ci_up, ci_up_vals)
tvals <- coefs / se
names_all <- c("const", ind_vars)

# 标准化系数
std_sol <- standardizedSolution(fit)
std_intercept <- std_sol[std_sol$op == "~1", "est.std"]
if (length(std_intercept) == 0) std_intercept <- NA
std_coef <- std_sol[std_sol$op == "~", "est.std"]
names(std_coef) <- std_sol[std_sol$op == "~", "rhs"]
std_coef <- std_coef[ind_vars]  # 按顺序
beta_std <- c(std_intercept, std_coef)

# R²
r2 <- lavInspect(fit, "r2")[dep_var]
if (is.null(r2) || is.na(r2)) {
    # 后备：用协方差计算
    var_y <- cov_matrix[dep_var, dep_var]
    Sxx <- cov_matrix[ind_vars, ind_vars]
    beta_no_const <- coef_vals
    pred_var <- t(beta_no_const) %*% Sxx %*% beta_no_const
    r2 <- as.numeric(pred_var / var_y)
    if (r2 < 0) r2 <- 0
    if (r2 > 1) r2 <- 1
}

n <- nrow(data)
k <- length(ind_vars)
adj_r2 <- 1 - (1 - r2) * (n - 1) / (n - k - 1)
f_stat <- (r2 / k) / ((1 - r2) / (n - k - 1))
f_pvalue <- pf(f_stat, k, n - k - 1, lower.tail = FALSE)
resid_var <- (1 - r2) * cov_matrix[dep_var, dep_var]
std_error <- sqrt(resid_var)

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

# 输出统计量
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

cat("R 成对删除回归完成。结果已写入:", output_file, "和", stats_file, "\n")
