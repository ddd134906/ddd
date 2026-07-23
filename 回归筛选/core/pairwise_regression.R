#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop("Usage: Rscript pairwise_regression.R <data.csv> <dep_var> <ind_vars(comma)> <output.csv>")
}
data_file <- args[1]
dep_var <- args[2]
ind_vars <- strsplit(args[3], ",")[[1]]
output_file <- args[4]

cat("\n========== 成对删除 OLS 回归（直接计算 X'X，精确匹配 SPSS）==========\n")
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
p <- k + 1  # 包含截距

# 1. 构建设计矩阵 X（包含截距）
X <- cbind(1, as.matrix(data[, ind_vars]))
y <- data[[dep_var]]

# 2. 计算成对删除的 X'X 和 X'y
# 使用循环，但对小数据直接向量化
XpX <- matrix(0, p, p)
Xpy <- numeric(p)

# 为每个变量对计算有效样本和乘积和
for (i in 1:p) {
    for (j in i:p) {
        # 找出两列均非缺失的行
        if (i == 1 && j == 1) {
            # 截距*截距：所有行（因变量无限制？实际上回归中截距只要求因变量非缺失？但这里X'X的(1,1)应该是n_eff，即因变量非缺失数，因为残差计算是基于y非缺失的子集）
            # 对于成对删除，X'X的(1,1)等于因变量有效样本数（因为截距列全为1，且与自身配对，只需y非缺失）
            valid <- !is.na(y)
        } else if (i == 1) {
            # 截距与变量j：需要y和xj均非缺失
            valid <- !is.na(y) & !is.na(X[, j])
        } else if (j == 1) {
            valid <- !is.na(y) & !is.na(X[, i])
        } else {
            # 两自变量均非缺失
            valid <- !is.na(X[, i]) & !is.na(X[, j])
        }
        n_ij <- sum(valid)
        if (n_ij == 0) {
            XpX[i,j] <- XpX[j,i] <- 0
        } else {
            XpX[i,j] <- XpX[j,i] <- sum(X[valid, i] * X[valid, j])
        }
    }
}

# 计算 X'y
for (i in 1:p) {
    if (i == 1) {
        valid <- !is.na(y)  # 截距与y配对：只需要y非缺失
    } else {
        valid <- !is.na(y) & !is.na(X[, i])
    }
    n_i <- sum(valid)
    if (n_i == 0) {
        Xpy[i] <- 0
    } else {
        Xpy[i] <- sum(X[valid, i] * y[valid])
    }
}

cat("\n--- 成对样本量（用于 X'X 元素） ---\n")
# 打印对角线有效样本数（即每列与自身配对的有效数）
for (i in 1:p) {
    if (i == 1) {
        n_ii <- sum(!is.na(y))  # 截距对角线为因变量有效数
    } else {
        n_ii <- sum(!is.na(y) & !is.na(X[, i]))
    }
    cat("X'X[", i, ",", i, "] 有效样本数 =", n_ii, "\n")
}

# 3. 解系数
coefs <- solve(XpX, Xpy)
names(coefs) <- c("const", ind_vars)

# 4. 因变量有效样本量（用于整体统计）
n_eff <- sum(!is.na(y))

# 5. R²（基于协方差矩阵，更稳定）
cov_mat <- cov(data, use = "pairwise.complete.obs")
Sxx <- cov_mat[ind_vars, ind_vars]
Sxy <- cov_mat[ind_vars, dep_var]
var_y <- cov_mat[dep_var, dep_var]
beta <- coefs[-1]  # 去掉截距
r2 <- as.numeric(t(beta) %*% Sxx %*% beta / var_y)
if (r2 < 0) r2 <- 0
if (r2 > 1) r2 <- 1

# 6. 逐系数标准误
se <- numeric(p)
names(se) <- c("const", ind_vars)
XpX_inv <- solve(XpX)

# 6a. 截距标准误：使用y非缺失的子集计算MSE
valid_y <- !is.na(y)
n_y <- sum(valid_y)
if (n_y > p) {
    X_sub <- X[valid_y, ]
    y_sub <- y[valid_y]
    pred_y <- X_sub %*% coefs
    res_y <- y_sub - pred_y
    MSE_const <- sum(res_y^2) / (n_y - p)   # 使用同样的系数
    se["const"] <- sqrt(MSE_const * XpX_inv[1,1])
} else {
    se["const"] <- NA
}

# 6b. 每个自变量标准误
for (i in 1:k) {
    var_name <- ind_vars[i]
    valid <- !is.na(y) & !is.na(X[, i+1])  # 注意X列索引：1是截距，i+1是该自变量
    n_i <- sum(valid)
    if (n_i > p) {
        X_i <- X[valid, ]
        y_i <- y[valid]
        pred_i <- X_i %*% coefs
        res_i <- y_i - pred_i
        MSE_i <- sum(res_i^2) / (n_i - p)
        se[var_name] <- sqrt(MSE_i * XpX_inv[i+1, i+1])
    } else {
        se[var_name] <- NA
    }
}

# 7. t检验、p值、置信区间（自由度使用n_eff - p，与SPSS一致）
df <- n_eff - p
tvals <- coefs / se
pvals <- 2 * pt(-abs(tvals), df = df)
ci_lower <- coefs - qt(0.975, df) * se
ci_upper <- coefs + qt(0.975, df) * se

# 8. 标准化系数
y_sd <- sd(y, na.rm = TRUE)
x_sd <- sapply(data[, ind_vars], sd, na.rm = TRUE)
beta_std <- beta * (x_sd / y_sd)
beta_std_full <- c(NA, beta_std)
names(beta_std_full) <- c("const", ind_vars)

# 9. 模型统计量
adj_r2 <- 1 - (1 - r2) * (n_eff - 1) / df
f_stat <- (r2 / k) / ((1 - r2) / df)
f_pvalue <- pf(f_stat, k, df, lower.tail = FALSE)
std_error_global <- sqrt(var_y * (1 - r2))

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
    r2 = r2,
    adj_r2 = adj_r2,
    f_value = f_stat,
    f_pvalue = f_pvalue,
    nobs = n_eff,
    std_error = std_error_global
)
write.csv(stats_df, file = stats_file, row.names = FALSE)

cat("\n===== 回归结果 =====\n")
cat("Intercept:", coefs["const"], "\n")
cat("R²:", r2, "\n")
cat("Adj R²:", adj_r2, "\n")
cat("F statistic:", f_stat, ", p-value:", f_pvalue, "\n")
cat("Std. Error of estimate:", std_error_global, "\n")
cat("因变量有效样本量 (n_eff):", n_eff, "\n")
cat("\n系数与标准误:\n")
print(data.frame(variable = names(coefs), coef = coefs, se = se))

cat("\n========== 完成 ==========\n")
