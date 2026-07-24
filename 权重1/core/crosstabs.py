import pandas as pd
import numpy as np

def seen_vs_noseen(df, x_vars, y_vars, seen_vars=None):
    """
    完全使用原始变量（不尝试重编码列）。
    若某个变量在 df 中不存在，则跳过并给出警告。
    参数：
        df: DataFrame
        x_vars: list，所有渠道的原始变量名（用于输出和分组）
        y_vars: list，Y变量的原始变量名
        seen_vars: list，可选，用于构建qd的变量（原始名）
    返回：
        qd_stats: DataFrame (describe结果)
        qd_desc: DataFrame (频数百分比)
        qd_means: DataFrame (行=Y, 列=qd=0/1)
        channel_means_dict: dict {原始渠道名: DataFrame(行=0/1, 列=Y)}
        diff_table: DataFrame (行=Y, 列=差值)
    """
    # ---- 1. 确定用于qd的变量（仅原始列） ----
    qd_vars = []
    if seen_vars:
        candidates = seen_vars
    else:
        candidates = x_vars
    for v in candidates:
        if v in df.columns:
            qd_vars.append(v)
        else:
            print(f"警告：qd变量 {v} 不在数据中，已忽略")
    if not qd_vars:
        empty = pd.DataFrame()
        return empty, empty, empty, {}, empty

    # ---- 2. 确定所有渠道的实际列名（仅原始列） ----
    channel_map = {}  # 原始名 -> 实际列名（与原始名相同）
    for orig in x_vars:
        if orig in df.columns:
            channel_map[orig] = orig
        else:
            print(f"警告：渠道变量 {orig} 不在数据中，已忽略")
    if not channel_map:
        empty = pd.DataFrame()
        return empty, empty, empty, {}, empty

    # ---- 3. 计算qd ----
    # 提取qd相关的列，填充NaN为0
    qd_df = df[qd_vars].fillna(0)
    if len(qd_vars) == 1:
        qd = (qd_df.iloc[:, 0] == 1).astype(int)
    else:
        qd = (qd_df == 1).any(axis=1).astype(int)
    df['qd'] = qd

    # ---- 4. qd描述统计 ----
    qd_stats = df['qd'].describe().to_frame().T
    qd_stats.index = ['qd']

    qd_counts = df['qd'].value_counts().sort_index()
    qd_desc = qd_counts.to_frame('Frequency')
    qd_desc['Percent'] = (qd_counts / len(df) * 100).round(2)

    # ---- 5. Y变量 ----
    exist_y = [y for y in y_vars if y in df.columns]

    # ---- 6. 按qd分组的Y均值 ----
    if exist_y and 0 in df['qd'].unique() and 1 in df['qd'].unique():
        qd_means = df.groupby('qd')[exist_y].mean().T
    else:
        qd_means = pd.DataFrame(index=exist_y)

    # ---- 7. 按每个渠道分组的Y均值 ----
    channel_means_dict = {}
    for orig, col in channel_map.items():
        if col in df.columns:
            gm = df.groupby(col)[exist_y].mean()
            # 确保索引包含0和1（若缺少则补NaN）
            if 0 not in gm.index:
                gm.loc[0] = np.nan
            if 1 not in gm.index:
                gm.loc[1] = np.nan
            gm = gm.sort_index()  # 0在前，1在后
            channel_means_dict[orig] = gm

    # ---- 8. 差值表 ----
    diff_data = {}
    if not qd_means.empty and 0 in qd_means.columns and 1 in qd_means.columns:
        diff_data['qd (Diff)'] = qd_means[1] - qd_means[0]
    for orig, gm in channel_means_dict.items():
        if 0 in gm.index and 1 in gm.index:
            diff_data[f'{orig} (Diff)'] = gm.loc[1] - gm.loc[0]
    diff_table = pd.DataFrame(diff_data, index=exist_y)

    # 清理临时列
    df.drop('qd', axis=1, inplace=True)

    return qd_stats, qd_desc, qd_means, channel_means_dict, diff_table