import pandas as pd
import numpy as np
import statsmodels.api as sm

def parse_factor_indicator(ind_list, n_factors):
    """
    将自变量列表中的 'FactorN' 替换为实际的列名
    例如：['Factor1', 'Factor2', 'dLevel_T2_1'] -> ['Factor1', 'Factor2', 'dLevel_T2_1']
    如果编号超出范围，打印警告并忽略该项
    """
    parsed = []
    for item in ind_list:
        if item.startswith('Factor'):
            try:
                num = int(item.replace('Factor', ''))
                if 1 <= num <= n_factors:
                    parsed.append(f'Factor{num}')
                else:
                    print(f"警告：因子编号 {num} 超出范围，忽略")
            except ValueError:
                print(f"警告：无法解析因子名 '{item}'，忽略")
        else:
            parsed.append(item)
    return parsed


def run_regression(df, dep_vars, ind_vars, weight_var=None, center=False):
    """
    执行线性回归（强制包含截距），支持加权最小二乘法（WLS）
    
    参数:
        df: 数据框
        dep_vars: 因变量列表（已重编码后的列名）
        ind_vars: 自变量列表（已解析为实际列名）
        weight_var: 权重变量名（可选），若为None或空字符串，则不使用权重
        center: 是否对自变量进行中心化（减去均值），默认为False
    """
    results = {}
    avail_ind = [v for v in ind_vars if v in df.columns]
    if not avail_ind:
        print("警告：没有可用的自变量，回归跳过")
        return results

    print("\n" + "="*60)
    print("【回归诊断】开始回归分析")
    print("="*60)
    print(f"因变量列表: {dep_vars}")
    print(f"自变量列表: {avail_ind}")
    print(f"权重变量: {weight_var if weight_var else '无'}")
    print(f"中心化: {'启用' if center else '关闭'}")
    print(f"数据框总样本数: {len(df)}")

    for dep in dep_vars:
        print(f"\n--- 处理因变量: {dep} ---")
        if dep not in df.columns:
            print(f"警告：因变量 {dep} 不在数据中，跳过")
            continue

        # 1. 检查因变量缺失
        y = df[dep]
        y_missing = y.isna().sum()
        y_total = len(y)
        y_valid = y_total - y_missing
        print(f"因变量 {dep}: 总样本 {y_total}, 缺失 {y_missing}, 有效 {y_valid}")

        # 2. 自变量缺失
        X = df[avail_ind]
        X_missing = X.isna().sum()
        X_missing_total = X.isna().any(axis=1).sum()
        print(f"自变量缺失情况: 任意自变量缺失的样本数 = {X_missing_total}")
        if X_missing_total > 0:
            print("  各变量缺失数量:")
            for col in avail_ind:
                if X_missing[col] > 0:
                    print(f"    {col}: {X_missing[col]}")


        #3. 中心化处理（如果启用）
        if center:
            X = X.copy()
            for col in X.columns:
                X[col] = X[col] - X[col].mean()
            print("  已对自变量进行中心化")

        # 4. 权重处理
        weights = None
        if weight_var and weight_var in df.columns:
            weights = df[weight_var]
            w_missing = weights.isna().sum()
            print(f"权重变量 {weight_var}: 缺失 {w_missing}")
            if (weights <= 0).any():
                print(f"警告：权重变量包含非正值，将忽略权重")
                weights = None

        # 5. 构建完整样本掩码（剔除缺失）
        mask = y.notna() & X.notna().all(axis=1)
        if weights is not None:
            mask = mask & weights.notna()

        full_n = len(mask)
        used_n = mask.sum()
        removed_n = full_n - used_n
        print(f"剔除缺失前样本数: {full_n}")
        print(f"剔除缺失后有效样本数: {used_n} (剔除了 {removed_n} 个样本)")

        if used_n == 0:
            print(f"警告：因变量 {dep} 无有效样本，跳过")
            continue

        y_clean = y[mask]
        X_clean = X.loc[mask]
        weights_clean = weights[mask] if weights is not None else None

        # 6. 输出清理后的变量均值（用于对比）
        print(f"\n清理后数据均值:")
        print(f"  因变量 {dep} 均值: {y_clean.mean():.6f}")
        print("  自变量均值:")
        for col in avail_ind:
            print(f"    {col}: {X_clean[col].mean():.6f}")
        if weights_clean is not None:
            print(f"  权重均值: {weights_clean.mean():.6f}")

        # 7. 强制添加截距并拟合
        X_clean = sm.add_constant(X_clean)
        if weights_clean is not None:
            model = sm.WLS(y_clean, X_clean, weights=weights_clean).fit()
        else:
            model = sm.OLS(y_clean, X_clean).fit()

        # 8. 提取系数
        coeff = model.params
        ci = model.conf_int(alpha=0.05)

        # 9. 计算标准化系数 Beta（正确方法，不干扰模型）
        beta = pd.Series(index=coeff.index, dtype=float)
        has_const = 'const' in coeff.index
        if has_const:
            vars_no_const = [v for v in coeff.index if v != 'const']
        else:
            vars_no_const = list(coeff.index)
        
        y_std = y_clean.std(ddof=1)
        for v in vars_no_const:
            x_std = X_clean[v].std(ddof=1) if v in X_clean else 0
            if x_std != 0:
                beta[v] = coeff[v] * (x_std / y_std)
            else:
                beta[v] = np.nan
        if has_const:
            beta['const'] = np.nan

        # 10. 保存结果
        results[dep] = {
            'R_squared': model.rsquared,
            'adj_R_squared': model.rsquared_adj,
            'R': np.sqrt(model.rsquared),
            'std_error': np.sqrt(model.mse_resid),
            'fvalue': model.fvalue,
            'f_pvalue': model.f_pvalue,
            'nobs': model.nobs,
            'coeff': coeff,
            'bse': model.bse,
            'tvalues': model.tvalues,
            'pvalues': model.pvalues,
            'ci_lower': ci[0],
            'ci_upper': ci[1],
            'beta': beta,
            'var_names': list(coeff.index)
        }

        # 11. 输出关键系数（方便对比）
        print(f"\n回归结果 (因变量 {dep}):")
        print(f"  R² = {model.rsquared:.4f}, 调整R² = {model.rsquared_adj:.4f}")
        print(f"  样本数 (nobs) = {model.nobs}")
        print("  系数:")
        for name, val in coeff.items():
            print(f"    {name}: {val:.6f} (标准误 {model.bse[name]:.6f})")
        print("-"*60)

    print("\n【回归诊断结束】\n")
    return results