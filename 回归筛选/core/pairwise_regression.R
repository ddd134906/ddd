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
cat("数据维度:", dim(data), "\n")
cat("因变量:", dep_var, "\n")
cat("自变量:", paste(ind_vars, collapse=", "), "\n")

# 计算成对协方差矩阵和均值向量（成对删除）
cov_matrix <- cov(data, use = "pairwise.complete.obs")
mean_vec <- colMeans(data, na.rm = TRUE)   # 成对均值（非缺失值的平均值）
cat("协方差矩阵维度:", dim(cov_matrix), "\n")

# 构建公式
formula <- paste(dep_var, "~", paste(ind_vars, collapse = " + "))
cat("模型公式:", formula, "\n")

# 拟合模型：使用协方差矩阵和均值向量，启用均值结构
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

# 提取未标准化结果
est <- parameterEstimates(fit, remove.system.eq = TRUE)

# 系数（含截距）
coefs <- est$est[est$op == "~"]
names(coefs) <- est$rhs[est$op == "~"]
intercept <- est$est[est$op == "~1"]
if (length(intercept) == 0) {
    # 如果截距不存在，可能是模型设置问题，尝试从拟合对象中手动提取
    # 或者设置为NA
    intercept <- NA
    names(intercept) <- "const"
} else {
    names(intercept) <- "const"
}
coefs <- c(intercept, coefs)

# 标准误
se <- est$se[est$op == "~"]
names(se) <- est$rhs[est$op == "~"]
se_intercept <- est$se[est$op == "~1"]
if (length(se_intercept) == 0) {
    se_intercept <- NA
    names(se_intercept) <- "const"
} else {
    names(se_intercept) <- "const"
}
se <- c(se_intercept, se)

# p值
pvals <- est$pvalue[est$op == "~"]
names(pvals) <- est$rhs[est$op == "~"]
p_intercept <- est$pvalue[est$op == "~1"]
if (length(p_intercept) == 0) {
    p_intercept <- NA
    names(p_intercept) <- "const"
} else {
    names(p_intercept) <- "const"
}
pvals <- c(p_intercept, pvals)

# 95% CI
ci_lower <- est$ci.lower[est$op == "~"]
names(ci_lower) <- est$rhs[est$op == "~"]
ci_lower_intercept <- est$ci.lower[est$op == "~1"]
if (length(ci_lower_intercept) == 0) {
    ci_lower_intercept <- NA
    names(ci_lower_intercept) <- "const"
} else {
    names(ci_lower_intercept) <- "const"
}
ci_lower <- c(ci_lower_intercept, ci_lower)

ci_upper <- est$ci.upper[est$op == "~"]
names(ci_upper) <- est$rhs[est$op == "~"]
ci_upper_intercept <- est$ci.upper[est$op == "~1"]
if (length(ci_upper_intercept) == 0) {
    ci_upper_intercept <- NA
    names(ci_upper_intercept) <- "const"
} else {
    names(ci_upper_intercept) <- "const"
}
ci_upper <- c(ci_upper_intercept, ci_upper)

# t值
tvals <- coefs / se

# 标准化系数
std_sol <- standardizedSolution(fit)
beta_std <- std_sol$est.std[std_sol$op == "~"]
names(beta_std) <- std_sol$rhs[std_sol$op == "~"]
beta_intercept <- std_sol$est.std[std_sol$op == "~1"]
if (length(beta_intercept) == 0) {
    beta_intercept <- NA
    names(beta_intercept) <- "const"
} else {
    names(beta_intercept) <- "const"
}
beta_std <- c(beta_intercept, beta_std)

# R²
r2 <- inspect(fit, "r2")[dep_var]
if (is.null(r2) || is.na(r2)) {
    # 若R²缺失，手动计算
    # 使用有效样本计算
    y_obs <- data[, dep_var]
    y_pred <- fitted(fit)[, dep_var]
    # 仅使用非缺失值
    mask <- !is.na(y_obs)
    y_obs <- y_obs[mask]
    y_pred <- y_pred[mask]
    ss_tot <- sum((y_obs - mean(y_obs))^2)
    ss_res <- sum((y_obs - y_pred)^2)
    r2 <- 1 - ss_res / ss_tot
}

# 调整R²
n <- nrow(data)
k <- length(ind_vars)
adj_r2 <- 1 - (1 - r2) * (n - 1) / (n - k - 1)

# F统计量
f_stat <- (r2 / k) / ((1 - r2) / (n - k - 1))
f_pvalue <- pf(f_stat, k, n - k - 1, lower.tail = FALSE)

# 回归标准误
# 使用拟合值残差的标准差
resid_var <- var(resid(fit), na.rm = TRUE)
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

cat("R 成对删除回归完成。结果已写入:", output_file, "和", stats_file, "\n")
