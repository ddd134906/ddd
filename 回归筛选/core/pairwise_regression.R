#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop("用法: Rscript pairwise_regression.R <数据文件.csv> <因变量> <自变量列表(逗号分隔)> <输出文件.csv>")
}
data_file <- args[1]
dep_var <- args[2]
ind_vars <- strsplit(args[3], ",")[[1]]
output_file <- args[4]

# 加载包
if (!require(lavaan)) {
    install.packages("lavaan", repos = "http://cran.r-project.org")
    library(lavaan)
}

# 读取数据
data <- read.csv(data_file)

# 打印数据摘要（调试）
cat("数据维度:", dim(data), "\n")
cat("因变量:", dep_var, "\n")
cat("自变量:", paste(ind_vars, collapse=", "), "\n")

# 计算成对协方差矩阵
cov_matrix <- cov(data, use = "pairwise.complete.obs")
cat("协方差矩阵维度:", dim(cov_matrix), "\n")

# 构建公式
formula <- paste(dep_var, "~", paste(ind_vars, collapse = " + "))
cat("模型公式:", formula, "\n")

# 拟合模型（使用协方差矩阵）
fit <- sem(formula, sample.cov = cov_matrix, sample.nobs = nrow(data),
           estimator = "ML", test = "standard")

# 检查拟合是否成功
if (is.null(fit)) {
    stop("lavaan 拟合失败")
}

# 提取未标准化结果
est <- parameterEstimates(fit)
# 系数（含截距）
coefs <- est$est[est$op == "~"]
names(coefs) <- est$rhs[est$op == "~"]
intercept <- est$est[est$op == "~1"]
names(intercept) <- "const"
coefs <- c(intercept, coefs)

# 标准误
se <- est$se[est$op == "~"]
names(se) <- est$rhs[est$op == "~"]
se_intercept <- est$se[est$op == "~1"]
names(se_intercept) <- "const"
se <- c(se_intercept, se)

# p值
pvals <- est$pvalue[est$op == "~"]
names(pvals) <- est$rhs[est$op == "~"]
p_intercept <- est$pvalue[est$op == "~1"]
names(p_intercept) <- "const"
pvals <- c(p_intercept, pvals)

# 95% CI
ci_lower <- est$ci.lower[est$op == "~"]
names(ci_lower) <- est$rhs[est$op == "~"]
ci_lower_intercept <- est$ci.lower[est$op == "~1"]
names(ci_lower_intercept) <- "const"
ci_lower <- c(ci_lower_intercept, ci_lower)

ci_upper <- est$ci.upper[est$op == "~"]
names(ci_upper) <- est$rhs[est$op == "~"]
ci_upper_intercept <- est$ci.upper[est$op == "~1"]
names(ci_upper_intercept) <- "const"
ci_upper <- c(ci_upper_intercept, ci_upper)

# t值
tvals <- coefs / se

# 标准化系数
std_sol <- standardizedSolution(fit)
beta_std <- std_sol$est.std[std_sol$op == "~"]
names(beta_std) <- std_sol$rhs[std_sol$op == "~"]
beta_intercept <- std_sol$est.std[std_sol$op == "~1"]
names(beta_intercept) <- "const"
beta_std <- c(beta_intercept, beta_std)

# R²
r2 <- inspect(fit, "r2")[dep_var]

# 调整R²
n <- nrow(data)
k <- length(ind_vars)
adj_r2 <- 1 - (1 - r2) * (n - 1) / (n - k - 1)

# F统计量
f_stat <- (r2 / k) / ((1 - r2) / (n - k - 1))
f_pvalue <- pf(f_stat, k, n - k - 1, lower.tail = FALSE)

# 回归标准误
resid_var <- sum(resid(fit)^2) / (n - k - 1)
std_error <- sqrt(resid_var)

# 输出系数表
result <- data.frame(
  variable = names(coefs),
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

# 打印成功信息
cat("R 成对删除回归完成。结果已写入:", output_file, "和", stats_file, "\n")
