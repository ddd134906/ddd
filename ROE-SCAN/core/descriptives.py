import pandas as pd
import numpy as np

def get_descriptives(df, var_list):
    exist_vars = [v for v in var_list if v in df.columns]
    if not exist_vars:
        return pd.DataFrame()
    desc = df[exist_vars].describe(percentiles=[]).T[['count', 'min', 'max', 'mean', 'std']]
    desc.columns = ['N', 'Min', 'Max', 'Mean', 'Std']
    return desc

def perform_sample_check(desc_raw_y, desc_d_y, desc_raw_x, channel_df, kpi_df, name_label_map, y_vars, x_vars, log_callback=print):
    # ---- KPI 核对 ----
    kpi_check_rows = []
    for i, (row_kpi, y_var) in enumerate(zip(kpi_df.itertuples(), y_vars)):
        # 特殊处理 Level_B4 -> BUMO
        if y_var == 'Level_B4':
            recoded_var = 'BUMO'
        else:
            recoded_var = f"d{y_var}"
        
        if recoded_var in desc_d_y.index:
            mean_val = desc_d_y.loc[recoded_var, 'Mean']
            n_val = desc_d_y.loc[recoded_var, 'N']
            calc_sample = mean_val * n_val
        else:
            calc_sample = np.nan
            log_callback(f"警告：变量 {recoded_var} 不在 recoded KPIs 描述统计中")
        full_sample = row_kpi.sample
        diff = calc_sample - full_sample if not pd.isna(calc_sample) and not pd.isna(full_sample) else np.nan
        kpi_check_rows.append({
            'TableName': row_kpi.table_name,
            'KPI_Label': row_kpi.kpi_label,
            'FullTable_Sample': full_sample,
            'Calc_Mean*N': calc_sample,
            'Diff': diff
        })
    kpi_check_df = pd.DataFrame(kpi_check_rows)

    # ---- 渠道核对 ----
    channel_check_rows = []
    for row_ch in channel_df.itertuples():
        var_name = row_ch.var_name
        if var_name in desc_raw_x.index:
            mean_val = desc_raw_x.loc[var_name, 'Mean']
            n_val = desc_raw_x.loc[var_name, 'N']
            calc_sample = mean_val * n_val
        else:
            calc_sample = np.nan
            log_callback(f"警告：变量 {var_name} 不在原始渠道描述统计中")
        full_sample = row_ch.sample
        diff = calc_sample - full_sample if not pd.isna(calc_sample) and not pd.isna(full_sample) else np.nan
        channel_check_rows.append({
            'TableName': row_ch.table_name,
            'Channel': row_ch.channel_label,
            'FullTable_Sample': full_sample,
            'Calc_Mean*N': calc_sample,
            'Diff': diff
        })
    channel_check_df = pd.DataFrame(channel_check_rows)

    # ---- 判断是否全部差异为 0 ----
    all_diffs = []
    all_diffs.extend(kpi_check_df['Diff'].dropna().tolist())
    all_diffs.extend(channel_check_df['Diff'].dropna().tolist())
    
    if len(all_diffs) == 0:
        success = False
        log_callback("警告：没有任何差异值可检查，可能所有样本量计算失败")
    else:
        success = all(abs(d) < 1e-6 for d in all_diffs)
        if success:
            log_callback("样本量校对成功")
        else:
            log_callback("样本量校对失败，存在差异")

    return kpi_check_df, channel_check_df, success