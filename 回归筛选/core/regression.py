import pandas as pd
import numpy as np
import statsmodels.api as sm
from scipy import stats
import subprocess
import tempfile
import os
import shutil

def parse_factor_indicator(ind_list, n_factors):
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


def _find_rscript():
    """查找系统 Rscript 可执行文件路径"""
    rscript_env = os.environ.get('RSCRIPT_PATH')
    if rscript_env and os.path.exists(rscript_env):
        return rscript_env
    rscript_path = shutil.which('Rscript')
    if rscript_path:
        return rscript_path
    common_paths = [
        "/usr/bin/Rscript",
        "/usr/local/bin/Rscript",
        "/opt/R/current/bin/Rscript",
        "C:\\Program Files\\R\\R-4.5.2\\bin\\Rscript.exe",
        "C:\\Program Files\\R\\R-4.5.1\\bin\\Rscript.exe",
        "C:\\Program Files\\R\\R-4.5.0\\bin\\Rscript.exe",
        "C:\\Program Files\\R\\R-4.4.3\\bin\\Rscript.exe",
        "C:\\Users\\ID0511084\\AppData\\Local\\Programs\\R\\R-4.5.2\\bin\\Rscript.exe",
    ]
    for path in common_paths:
        if os.path.exists(path):
            return path
    raise RuntimeError("未找到 Rscript，请安装 R 并将 Rscript 所在目录添加到 PATH，或设置环境变量 RSCRIPT_PATH")


def _pairwise_regression_with_r(df, dep_var, ind_vars):
    """
    使用 R (lavaan) 通过 subprocess 进行成对删除回归，包含标准化系数。
    自动查找 Rscript 路径，并适配新的 R 脚本输出格式。
    """
    data = df[[dep_var] + ind_vars].dropna(how='all')
    if data.empty:
        raise ValueError("数据全部缺失，无法回归")

    print(f"【R成对删除】因变量: {dep_var}, 自变量数: {len(ind_vars)}, 数据行数: {len(data)}")

    # 查找 Rscript 路径
    try:
        rscript_path = _find_rscript()
        print(f"【R成对删除】使用 Rscript: {rscript_path}")
    except RuntimeError as e:
        print(f"【R成对删除】查找 Rscript 失败: {e}")
        raise

    # 创建临时文件
    with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False, encoding='utf-8') as f_data:
        data.to_csv(f_data, index=False)
        data_path = f_data.name

    with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False, encoding='utf-8') as f_out:
        coef_path = f_out.name

    stats_path = coef_path.replace('.csv', '_stats.csv')

    # 调用 R 脚本
    r_script = os.path.join(os.path.dirname(__file__), 'pairwise_regression.R')
    if not os.path.exists(r_script):
        raise FileNotFoundError(f"R 脚本不存在: {r_script}")
    ind_vars_str = ','.join(ind_vars)
    cmd = [rscript_path, r_script, data_path, dep_var, ind_vars_str, coef_path]
    print(f"【R成对删除】执行命令: {' '.join(cmd)}")

    result = subprocess.run(cmd, capture_output=True, text=True)
    print(f"【R成对删除】返回码: {result.returncode}")
    if result.stdout:
        print("【R成对删除】R 脚本 stdout:\n", result.stdout)
    if result.stderr:
        print("【R成对删除】R 脚本 stderr:\n", result.stderr)

    if result.returncode != 0:
        raise RuntimeError(f"R 脚本执行失败: {result.stderr}")

    # 检查输出文件
    if not os.path.exists(coef_path):
        raise FileNotFoundError(f"系数文件未生成: {coef_path}")
    if not os.path.exists(stats_path):
        raise FileNotFoundError(f"统计量文件未生成: {stats_path}")

    # 读取结果（R 脚本现在输出 variable 列）
    res_df = pd.read_csv(coef_path)
    stats_df = pd.read_csv(stats_path)

    # 直接使用 variable 列作为索引
    coeff = pd.Series(res_df['coef'].values, index=res_df['variable'])
    bse = pd.Series(res_df['se'].values, index=res_df['variable'])
    tvals = pd.Series(res_df['t'].values, index=res_df['variable'])
    pvals = pd.Series(res_df['p'].values, index=res_df['variable'])
    ci_low = pd.Series(res_df['ci_lower'].values, index=res_df['variable'])
    ci_up = pd.Series(res_df['ci_upper'].values, index=res_df['variable'])
    beta = pd.Series(res_df['beta'].values, index=res_df['variable'])

    results = {
        'R_squared': stats_df['r2'].iloc[0],
        'adj_R_squared': stats_df['adj_r2'].iloc[0],
        'R': np.sqrt(stats_df['r2'].iloc[0]),
        'std_error': stats_df['std_error'].iloc[0],
        'fvalue': stats_df['f_value'].iloc[0],
        'f_pvalue': stats_df['f_pvalue'].iloc[0],
        'nobs': stats_df['nobs'].iloc[0],
        'coeff': coeff,
        'bse': bse,
        'tvalues': tvals,
        'pvalues': pvals,
        'ci_lower': ci_low,
        'ci_upper': ci_up,
        'beta': beta,
        'var_names': list(coeff.index)
    }

    # 清理临时文件
    os.unlink(data_path)
    os.unlink(coef_path)
    os.unlink(stats_path)

    return results


def run_regression(df, dep_vars, ind_vars, weight_var=None, center=False,
                   pairwise=False, add_constant=True):
    """
    执行线性回归，支持整行删除（Python）和成对删除（R）。
    """
    results = {}
    avail_ind = [v for v in ind_vars if v in df.columns]
    if not avail_ind:
        print("警告：没有可用的自变量，回归跳过")
        return results

    for dep in dep_vars:
        if dep not in df.columns:
            print(f"警告：因变量 {dep} 不在数据中，跳过")
            continue

        if pairwise:
            print(f"使用 R 成对删除回归，因变量: {dep}")
            try:
                res = _pairwise_regression_with_r(df, dep, avail_ind)
                results[dep] = res
                continue
            except Exception as e:
                print(f"R 成对删除回归失败，回退到整行删除：{e}")
                # 回退到整行删除

        # ---- 整行删除（Python） ----
        y = df[dep]
        y_missing = y.isna().sum()
        y_total = len(y)
        y_valid = y_total - y_missing
        print(f"因变量 {dep}: 总样本 {y_total}, 缺失 {y_missing}, 有效 {y_valid}")

        X = df[avail_ind]
        X_missing = X.isna().sum()
        X_missing_total = X.isna().any(axis=1).sum()
        print(f"自变量缺失情况: 任意自变量缺失的样本数 = {X_missing_total}")
        if X_missing_total > 0:
            print("  各变量缺失数量:")
            for col in avail_ind:
                if X_missing[col] > 0:
                    print(f"    {col}: {X_missing[col]}")

        if center:
            X = X.copy()
            for col in X.columns:
                X[col] = X[col] - X[col].mean()
            print("  已对自变量进行中心化")

        weights = None
        if weight_var and weight_var in df.columns:
            weights = df[weight_var]
            w_missing = weights.isna().sum()
            print(f"权重变量 {weight_var}: 缺失 {w_missing}")
            if (weights <= 0).any():
                print(f"警告：权重变量包含非正值，将忽略权重")
                weights = None

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

        print(f"\n清理后数据均值:")
        print(f"  因变量 {dep} 均值: {y_clean.mean():.6f}")
        print("  自变量均值:")
        for col in avail_ind:
            print(f"    {col}: {X_clean[col].mean():.6f}")
        if weights_clean is not None:
            print(f"  权重均值: {weights_clean.mean():.6f}")

        X_clean = sm.add_constant(X_clean) if add_constant else X_clean
        if weights_clean is not None:
            model = sm.WLS(y_clean, X_clean, weights=weights_clean).fit()
        else:
            model = sm.OLS(y_clean, X_clean).fit()

        coeff = model.params
        ci = model.conf_int(alpha=0.05)

        # 标准化系数 Beta
        beta_std = pd.Series(index=coeff.index, dtype=float)
        if not center and weights is None:
            y_std = y_clean.std(ddof=1)
            if 'const' in coeff.index:
                vars_no_const = [v for v in coeff.index if v != 'const']
            else:
                vars_no_const = list(coeff.index)
            for v in vars_no_const:
                x_std = X_clean[v].std(ddof=1) if v in X_clean else 0
                if x_std != 0:
                    beta_std[v] = coeff[v] * (x_std / y_std)
                else:
                    beta_std[v] = np.nan
            if 'const' in coeff.index:
                beta_std['const'] = np.nan
        else:
            beta_std[:] = np.nan

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
            'beta': beta_std,
            'var_names': list(coeff.index)
        }

        print(f"\n回归结果 (因变量 {dep}):")
        print(f"  R² = {model.rsquared:.4f}, 调整R² = {model.rsquared_adj:.4f}")
        print(f"  样本数 (nobs) = {model.nobs}")
        print("  系数:")
        for name, val in coeff.items():
            print(f"    {name}: {val:.6f} (标准误 {model.bse[name]:.6f})")
        print("-"*60)

    print("\n【回归诊断结束】\n")
    return results
