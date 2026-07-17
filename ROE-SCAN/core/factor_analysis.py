import numpy as np
import pandas as pd

def _procrustes_vba(A, C, log_callback):
    """
    完全仿照 VBA 的 S_mRotationProcrustes 和 S_mRotationProcrustesStd
    A: 源载荷矩阵 (n, k)
    C: 目标矩阵 (n, k)，包含 0/1
    返回旋转后的矩阵 B
    """
    n, k = A.shape
    # 第一步：初始化 InMatrixC，C 中非数值用 0.3 替代（但我们的 C 都是 0/1，所以直接复制）
    InC = C.copy()
    
    # 第二步：调用 ProcrustesStd
    def _procrustes_std(A, C):
        n, k = A.shape
        InC = np.zeros_like(C)
        for i in range(n):
            Hj2 = np.sum(A[i, :]**2)
            Kj2 = np.sum(C[i, :]**2)
            if Kj2 == 0:
                scale = 0
            else:
                scale = np.sqrt(Hj2 / Kj2)
            InC[i, :] = C[i, :] * scale
        
        ACMatrix = A.T @ InC
        S = ACMatrix.T @ ACMatrix
        eigvals, P = np.linalg.eigh(S)
        idx = np.argsort(eigvals)[::-1]
        eigvals = eigvals[idx]
        P = P[:, idx]
        
        D = np.zeros((k, k))
        for i in range(k):
            if eigvals[i] > 1e-12:
                D[i, i] = eigvals[i] ** (-0.5)
            else:
                D[i, i] = 0
        Alpha = P @ D @ P.T
        T = ACMatrix @ Alpha
        B = A @ T
        return B
    
    B = _procrustes_std(A, InC)
    # 由于我们的 C 没有未知值，无需迭代
    return B

def get_max_component_order(rotated_loadings, orderby=True):
    n_vars = rotated_loadings.shape[0]
    if not orderby:
        return list(range(n_vars))
    max_abs = np.abs(rotated_loadings).max(axis=1)
    max_idx = np.argmax(np.abs(rotated_loadings), axis=1)
    sorted_indices = sorted(range(n_vars), key=lambda i: (max_idx[i], -max_abs[i]))
    return sorted_indices

def perform_factor_analysis(df, config, log_callback=print):
    # ---- 1. 读取目标矩阵 ----
    target_df = config.get('factor_target')
    if target_df is None:
        raise ValueError("config 中缺少 'factor_target'")
    used_vars = target_df.index.tolist()
    n_vars = len(used_vars)
    k = target_df.shape[1]
    
    # 过滤非空行
    mask = (target_df != 0).any(axis=1)
    active_vars = target_df.index[mask].tolist()
    active_target = target_df.loc[active_vars]
    n_active = len(active_vars)
    if n_active == 0:
        raise ValueError("目标矩阵中没有任何变量属于因子")
    used_vars = active_vars
    n_vars = n_active
    
    # ---- 2. 提取数据计算协方差和相关系数矩阵 ----
    X = df[used_vars].values.astype(float)
    cov_matrix = np.cov(X, rowvar=False, ddof=1)
    std = np.sqrt(np.diag(cov_matrix))
    corr_matrix = cov_matrix / np.outer(std, std)
    
    # ---- 3. PCA 特征分解 ----
    eigvals, eigvecs = np.linalg.eigh(corr_matrix)
    idx_sort = np.argsort(eigvals)[::-1]
    eigvals = eigvals[idx_sort]
    eigvecs = eigvecs[:, idx_sort]
    full_eigvals = eigvals
    full_var_ratio = eigvals / eigvals.sum()
    full_cum_var = np.cumsum(full_var_ratio)
    
    eigvals_k = eigvals[:k]
    eigvecs_k = eigvecs[:, :k]
    unrot_loadings = eigvecs_k * np.sqrt(np.maximum(eigvals_k, 0))
    
    # ---- 4. 规则矩阵 ----
    rule_matrix = active_target.values.astype(float)
    
    # ---- 5. Procrustes 旋转（使用自定义 VBA 兼容函数） ----
    rotation = config.get('factor_rotation', 'procrustes').lower()
    if rotation == 'procrustes':
        rot_loadings = _procrustes_vba(unrot_loadings, rule_matrix, log_callback)
        log_callback("✅ Procrustes 旋转完成。")
    else:
        raise ValueError(f"不支持的旋转方法：'{rotation}'")
    
    # ---- 6. 符号调整 ----
    for j in range(k):
        mask_col = rule_matrix[:, j] != 0
        belong_vars_idx = np.where(mask_col)[0]
        if len(belong_vars_idx) == 0:
            continue
        group_load = rot_loadings[belong_vars_idx, j]
        max_abs_pos = np.argmax(np.abs(group_load))
        max_val = group_load[max_abs_pos]
        if max_val < 0:
            rot_loadings[:, j] = -rot_loadings[:, j]
    
    # ---- 7. 标准化得分系数 ----
    gram = rot_loadings.T @ rot_loadings
    try:
        inv_gram = np.linalg.inv(gram)
    except np.linalg.LinAlgError:
        inv_gram = np.linalg.pinv(gram)
    score_coeff_std = rot_loadings @ inv_gram
    
    # ---- 8. 两个版本的未标准化得分系数 ----
    std_original = np.sqrt(np.diag(cov_matrix))          # 所有变量的标准差
    std_factor = std_original[:k]                        # 前 k 个变量的标准差
    # 版本1：用于因子得分和回归（与 SPSS 语法一致，每个变量除自身标准差）
    score_coeff_unstd_spss = score_coeff_std / std_original[:, None]
    # 版本2：用于 Excel 表格输出（除以因子编号对应的变量标准差）
    score_coeff_unstd_table = score_coeff_std / std_factor
    
    # ---- 9. 因子得分（使用 SPSS 版本） ----
    factor_scores = X @ score_coeff_unstd_spss
    factor_cols = [f'Factor{i+1}' for i in range(k)]
    factor_df = pd.DataFrame(factor_scores, columns=factor_cols, index=df.index)
    for col in factor_cols:
        df[col] = factor_df[col]
    
    # ---- 10. 因子相关系数矩阵 ----
    factor_corr = np.corrcoef(factor_scores.T)
    
    # ---- 11. 排序索引（用于输出时排序，但最终输出我们不再使用，保留仅兼容） ----
    orderby = config.get('orderby', True)
    order_indices = get_max_component_order(rot_loadings, orderby)
    
    # ---- 12. SPSS 语法（使用 SPSS 版本系数） ----
    syntax_lines = []
    for i in range(k):
        coeffs = score_coeff_unstd_spss[:, i]
        terms = []
        for var, coef in zip(used_vars, coeffs):
            if abs(coef) < 1e-10:
                continue
            sign = '+' if coef >= 0 else '-'
            abs_coef = abs(coef)
            terms.append(f"{sign}{abs_coef:.4f}*{var}")
        expr = ''.join(terms)
        if expr.startswith('+'):
            expr = expr[1:]
        syntax_lines.append(f"Compute Factor{i+1} = {expr}.")
    spss_syntax = '\n'.join(syntax_lines)
    
    # ---- 13. 打包输出 ----
    fa_extra = {
        'eigenvals': full_eigvals,
        'var_ratio': full_var_ratio,
        'cum_var': full_cum_var,
        'unrot_loadings': unrot_loadings,
        'rot_loadings': rot_loadings,
        'score_coeff_std': score_coeff_std,
        'score_coeff_unstd': score_coeff_unstd_spss,      # 用于得分和回归
        'score_coeff_unstd_table': score_coeff_unstd_table,  # 用于表格输出
        'target': active_target,
        'order_indices': order_indices,
        'sig_break': config.get('sig_break', 0.3),
        'used_vars': used_vars,
        'n_factors': k,
        'corr_matrix': corr_matrix,
        'spss_syntax': spss_syntax,
        'rotation': rotation,
    }
    
    return None, rot_loadings, score_coeff_unstd_spss, factor_df, df, factor_corr, fa_extra