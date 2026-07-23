#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop("用法: Rscript pairwise_regression.R <数据文件.csv> <因变量> <自变量列表(逗号分隔)> <输出文件.csv>")
}
data_file <- args[1]
dep_var <- args[2]
ind_vars <- strsplit(args[3], ",")[[1]]
output_file <- args[4]

cat("========== R 成对删除回归诊断 ==========\n")
cat("数据文件:", data_file, "\n")
cat("因变量:", dep_var, "\n")
cat("自变量:", paste(ind_vars, collapse=", "), "\n")

# 加载 lavaan
if (!require(lavaan)) {
    install.packages("lavaan", repos = "http://cran.r-project.org")
    library(lavaan)
}

# 读取数据
data <- read.csv(data_file)
cat("原始数据维度:", dim(data), "\n")
cat("数据前5行:\n")
print(head(data, 5))

# 检查缺失情况
missing_counts <- colSums(is.na(data))
cat("各变量缺失数量:\n")
print(missing_counts)

# 计算成对协方差矩阵和均值向量
cat("\n计算成对协方差矩阵...\n")
cov_matrix <- cov(data, use = "pairwise.complete.obs")
mean_vec <- colMeans(data, na.rm = TRUE)
cat("协方差矩阵维度:", dim(cov_matrix), "\n")
cat("协方差矩阵 (前5行5列):\n")
print(cov_matrix[1:min(5, nrow(cov_matrix)), 1:min(5, ncol(cov_matrix))])
cat("均值向量:\n")
print(mean_vec)

# 构建模型公式
formula <- paste(dep_var, "~", paste(ind_vars, collapse = " + "))
cat("\n模型公式:", formula, "\n")

# 拟合模型
cat("开始拟合 lavaan 模型...\n")
fit <- sem(formula,
           sample.cov = cov_matrix,
           sample.mean = mean_vec,
           sample.nobs = nrow(data),
           meanstructure = TRUE,
           estimator = "ML",
           test = "standard")

if (is.null(fit)) {
    cat("错误: lavaan 拟合失败\n")
    stop("lavaan 拟合失败")
}
cat("模型拟合成功。\n")

# 提取模型摘要
cat("\n===== 模型摘要 =====\n")
summary(fit, fit.measures = TRUE, standardized = TRUE)

# 提取 parameterEstimates
est <- parameterEstimates(fit, remove.system.eq = TRUE)
cat("\n===== 参数估计表 =====\n")
print(est)

# 提取系数和标准误
# 截距 (op == "~1")
intercept_row <- est[est$op == "~1", ]
if (nrow(intercept_row) == 0) {
    cat("警告: 未找到截距项\n")
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
    cat("截距估计:", intercept_est, " 标准误:", intercept_se, "\n")
}

# 回归系数 (op == "~")
coef_rows <- est[est$op == "~", ]
cat("回归系数行数:", nrow(coef_rows), "\n")
# 按自变量顺序匹配
coef_rows <- coef_rows[match(ind_vars, coef_rows$rhs), ]

coef_vals <- coef_rows$est
se_vals <- coef_rows$se
p_vals <- coef_rows$pvalue
ci_low_vals <- coef_rows$ci.lower
ci_up_vals <- coef_rows$ci.upper

cat("系数向量 (按自变量顺序):\n")
names(coef_vals) <- ind_vars
print(coef_vals)

# 组合成最终向量
coefs <- c(intercept_est, coef_vals)
se <- c(intercept_se, se_vals)
pvals <- c(intercept_p, p_vals)
ci_lower <- c(intercept_ci_low, ci_low_vals)
ci_upper <- c(intercept_ci_up, ci_up_vals)
tvals <- coefs / se
names_all <- c("const", ind_vars)

cat("\n组合后的系数 (包含截距):\n")
print(coefs)
cat("标准误:\n")
print(se)

# 标准化系数
std_sol <- standardizedSolution(fit)
cat("\n===== 标准化解 =====\n")
print(std_sol)

std_intercept <- std_sol[std_sol$op == "~1", "est.std"]
if (length(std_intercept) == 0) std_intercept <- NA
std_coef <- std_sol[std_sol$op == "~", "est.std"]
names(std_coef) <- std_sol[std_sol$op == "~", "rhs"]
std_coef <- std_coef[ind_vars]  # 按顺序
beta_std <- c(std_intercept, std_coef)
cat("标准化系数:\n")
print(beta_std)

# R²
cat("\n===== R² =====\n")
r2 <- lavInspect(fit, "r2")[dep_var]
cat("lavaan 计算的 R²:", r2, "\n")
if (is.null(r2) || is.na(r2)) {
    cat("R² 缺失，使用协方差矩阵手动计算...\n")
    var_y <- cov_matrix[dep_var, dep_var]
    Sxx <- cov_matrix[ind_vars, ind_vars]
    beta_no_const <- coef_vals
    pred_var <- t(beta_no_const) %*% Sxx %*% beta_no_const
    r2 <- as.numeric(pred_var / var_y)
    if (r2 < 0) r2 <- 0
    if (r2 > 1) r2 <- 1
    cat("手动计算 R²:", r2, "\n")
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
cat("调整 R²:", adj_r2, "\n")
cat("F 统计量:", f_stat, "\n")
cat("F p 值:", f_pvalue, "\n")
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

cat("\n===== R 成对删除回归完成 =====\n")
cat("系数表已写入:", output_file, "\n")
cat("统计量表已写入:", stats_file, "\n")
