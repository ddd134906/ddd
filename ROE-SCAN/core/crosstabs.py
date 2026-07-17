import pandas as pd
import numpy as np

def seen_vs_noseen(df, x_vars, y_vars, seen_vars=None):
    """
    计算按qd（是否接触）分组的Y均值，以及按各渠道接触分组的Y均值
    """
    # 确定使用哪些变量计算qd
    if seen_vars:
        used_x = []
        for x in seen_vars:
            if f'd{x}' in df.columns:
                used_x.append(f'd{x}')
            elif x in df.columns:
                used_x.append(x)
    else:
        used_x = []
        for x in x_vars:
            if f'd{x}' in df.columns:
                used_x.append(f'd{x}')
            elif x in df.columns:
                used_x.append(x)

    if not used_x:
        return pd.DataFrame(), pd.DataFrame()

    # 计算qd
    qd = (df[used_x].fillna(0) == 1).any(axis=1).astype(int)
    df['qd'] = qd

    # 按qd分组的均值
    qd_means = df.groupby('qd')[y_vars].mean().T

    # 按每个渠道接触分组的均值（只取接触=1的组）
    channel_means = {}
    for x in used_x:
        if x in df.columns:
            mask = df[x] == 1
            if mask.any():
                channel_means[x] = df.loc[mask, y_vars].mean()
    channel_means_df = pd.DataFrame(channel_means)

    return qd_means, channel_means_df