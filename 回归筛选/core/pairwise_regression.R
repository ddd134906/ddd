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

# 成对协方差矩阵和均值
cov_matrix <- cov(data, use = "pairwise.complete.obs")
mean_vec <- colMeans(data, na.rm = TRUE)

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

# 直接提取 parameterEstimates（包含所有需要的统计量）
est <- parameterEstimates(fit, remove.system.eq = TRUE)

# 过滤出回归系数（含截距）
est_reg <- est[est$op == "~1" | est$op == "~", ]

# 构建变量名列表
var_names <- c("const", est_reg$rhs[est_reg$op == "~"])

# 检查长度是否一致
if (length(var_names) != nrow(est_reg)) {
    # 如果长度不一致，直接用 est_reg 的 lhs/rhs 组合
    var_names <- paste(est_reg$lhs, est_reg$op, est_reg$rhs)
}

# 创建输出数据框
result <- data.frame(
  variable = var_names,
  coef = est_reg$est,
  se = est_reg$se,
  t = est_reg$est / est_reg$se,
  p = est_reg$pvalue,
  ci_lower = est_reg$ci.lower,
  ci_upper = est_reg$ci.upper,
  stringsAsFactors = FALSE
)

# 添加标准化系数
std_sol <- standardizedSolution(fit)
std_est <- std_sol[std_sol$op == "~1" | std_sol$op == "~", ]
# 按 est_reg 的行顺序匹配标准化系数
result$beta <- std_est$est.std[match(paste(est_reg$lhs, est_reg$rhs), paste(std_est$lhs, std_est$rhs))]

# 写入系数表
write.csv(result, output_file, row.names = FALSE)

# 输出模型统计量
r2 <- lavInspect(fit, "r2")[dep_var]
if (is.null(r2) || is.na(r2)) {
    var_y <- cov_matrix[dep_var, dep_var]
    beta_no_const <- est_reg$est[est_reg$op == "~"]
    Sxx <- cov_matrix[ind_vars, ind_vars]
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
