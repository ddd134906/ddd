pairwise_lm <- function(data, dep_var, ind_vars) {
  # 1. 确保所有变量为数值型
  for (col in c(dep_var, ind_vars)) {
    if (!is.numeric(data[[col]])) {
      data[[col]] <- as.numeric(as.character(data[[col]]))
    }
  }
  
  # 2. 计算成对协方差矩阵和均值向量
  cov_matrix <- cov(data, use = "pairwise.complete.obs")
  mean_vec <- colMeans(data, na.rm = TRUE)
  
  # 3. 提取相关部分
  Sxx <- cov_matrix[ind_vars, ind_vars]
  Sxy <- cov_matrix[ind_vars, dep_var]
  x_means <- mean_vec[ind_vars]
  y_mean <- mean_vec[dep_var]
  
  # 4. 求解回归系数（无截距）
  beta <- solve(Sxx, Sxy)
  intercept <- y_mean - sum(x_means * beta)
  
  # 5. 有效样本数
  n_eff <- sum(!is.na(data[, dep_var]))
  k <- length(ind_vars)
  df <- n_eff - k - 1
  
  # 6. 计算残差方差（用于标准误）
  # 使用因变量非缺失的样本，自变量缺失时用均值填充
  y_obs <- data[!is.na(data[, dep_var]), dep_var]
  X_obs <- as.matrix(data[!is.na(data[, dep_var]), ind_vars])
  
  for (i in 1:k) {
    X_obs[is.na(X_obs[, i]), i] <- x_means[i]
  }
  
  # 添加截距列并计算残差
  X_obs <- cbind(1, X_obs)
  coefs <- c(intercept, beta)
  y_pred <- X_obs %*% coefs
  resid <- y_obs - y_pred
  SSE <- sum(resid^2)
  MSE <- SSE / df
  
  # 7. 计算标准误
  var_beta <- MSE * solve(Sxx)
  se_beta <- sqrt(diag(var_beta))
  var_intercept <- MSE * (1/n_eff + x_means %*% solve(Sxx) %*% x_means)
  se_intercept <- sqrt(var_intercept)
  se <- c(se_intercept, se_beta)
  
  # 8. 计算 t 值和 p 值
  tvals <- coefs / se
  pvals <- 2 * pt(-abs(tvals), df = df)
  
  # 9. 计算 R² 和调整 R²
  SST <- sum((y_obs - mean(y_obs))^2)
  SSR <- SST - SSE
  r2 <- SSR / SST
  adj_r2 <- 1 - (1 - r2) * (n_eff - 1) / df
  
  # 10. 计算 F 统计量
  f_stat <- (r2 / k) / ((1 - r2) / df)
  f_pvalue <- pf(f_stat, k, df, lower.tail = FALSE)
  
  # 11. 返回结果列表
  coef_names <- c("(Intercept)", ind_vars)
  results <- list(
    coefficients = setNames(coefs, coef_names),
    se = setNames(se, coef_names),
    t = setNames(tvals, coef_names),
    p = setNames(pvals, coef_names),
    r.squared = r2,
    adj.r.squared = adj_r2,
    fstatistic = c(value = f_stat, numdf = k, dendf = df, p.value = f_pvalue),
    nobs = n_eff,
    df = df,
    sigma = sqrt(MSE)
  )
  class(results) <- "pairwise_lm"
  return(results)
}
