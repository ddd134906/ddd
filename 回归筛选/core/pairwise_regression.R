#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop("用法: Rscript pairwise_regression.R <数据文件.csv> <因变量> <自变量列表(逗号分隔)> <输出文件.csv>")
}
data_file <- args[1]
dep_var <- args[2]
ind_vars <- strsplit(args[3], ",")[[1]]
output_file <- args[4]

# 加载 lavaan（若未安装则自动安装）
if (!require(lavaan)) {
    install.packages("lavaan", repos = "http://cran.r-project.org")
    library(lavaan)
}

data <- read.csv(data_file)
cat("数据维度:", dim(data), "\n")
cat("因变量:", dep_var, "\n")
cat("自变量:", paste(ind_vars, collapse=", "), "\n")

# 1. 成对协方差矩阵和均值向量
cov_matrix <- cov(data, use = "pairwise.complete.obs")
mean_vec <- colMeans(data, na.rm = TRUE)
cat("协方差矩阵维度:", dim(cov_matrix), "\n")

# 2. 构建模型公式
formula <- paste(dep_var, "~", paste(ind_vars, collapse = " + "))
cat("模型公式:", formula, "\n")

# 3. 拟合模型（使用成对协方差和均值，启用均值结构）
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

# 4. 提取参数估计（含系数、标准误、p 值、置信区间）
est <- parameterEstimates(fit, remove.system.eq = TRUE)

# 5. 提取 R²
r2 <- lavInspect(fit, "r2")[dep_var]
if (is.null(r2) || is.na(r2)) {
    # 极少数情况下 lavInspect 返回 NULL，则手动计算（后备）
    # 但这几乎不会发生，保留以防万一
    var_y <- cov_matrix[dep_var, dep_var]
    coefs <- coef(fit)
    beta_no_const <- coefs[ind_vars]  # 确保顺序
    Sxx <- cov_matrix[ind_vars, ind_vars]
    pred_var <- t(beta_no_const) %*% Sxx %*% beta_no_const
    r2 <- as.numeric(pred_var / var_y)
    if (r2 < 0) r2 <- 0
    if (r2 > 1) r2 <- 1
}

# 6. 准备输出表（与 Python 的输出格式对齐）
# 系数（含截距）
coefs <- est$est[est$op == "~1" | est$op == "~"]
names(coefs) <- c("const", est$rhs[est$op == "~"])
# 确保顺序：截距在前，自变量按 ind_vars 顺序
coefs <- coefs[c("const", ind_vars)]

# 标准误
se <- est$se[est$op == "~1" | est$op == "~"]
names(se) <- c("const", est$rhs[est$op == "~"])
se <- se[c("const", ind_vars)]

# p 值
pvals <- est$pvalue[est$op == "~1" | est$op == "~"]
names(pvals) <- c("const", est$rhs[est$op == "~"])
pvals <- pvals[c("const", ind_vars)]

# 置信区间
ci_lower <- est$ci.lower[est$op == "~1" | est$op == "~"]
names(ci_lower) <- c("const", est$rhs[est$op == "~"])
ci_lower <- ci_lower[c("const", ind_vars)]

ci_upper <- est$ci.upper[est$op == "~1" | est$op == "~"]
names(ci_upper) <- c("const", est$rhs[est$op == "~"])
ci_upper <- ci_upper[c("const", ind_vars)]

# t 值
tvals <- coefs / se

# 标准化系数
std_sol <- standardizedSolution(fit)
beta_std <- std_sol$est.std[std_sol$op == "~1" | std_sol$op == "~"]
names(beta_std) <- c("const", std_sol$rhs[std_sol$op == "~"])
beta_std <- beta_std[c("const", ind_vars)]

# 7. 模型统计量
n <- nrow(data)
k <- length(ind_vars)
adj_r2 <- 1 - (1 - r2) * (n - 1) / (n - k - 1)
f_stat <- (r2 / k) / ((1 - r2) / (n - k - 1))
f_pvalue <- pf(f_stat, k, n - k - 1, lower.tail = FALSE)

# 回归标准误（从模型隐含的残差方差计算）
resid_var <- (1 - r2) * cov_matrix[dep_var, dep_var]
std_error <- sqrt(resid_var)

# 8. 输出系数表
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

# 9. 输出模型统计量
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
