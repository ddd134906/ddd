import pandas as pd
import re

def load_definition(def_path):
    # 读取 Config 表（前3列，用于竖排扫描和类型获取）
    config_raw = pd.read_excel(def_path, sheet_name='Config', header=None, usecols=[0, 1, 2])
    # 读取 Config 表（前2列，用于键值对读取）
    config_df = pd.read_excel(def_path, sheet_name='Config', header=None, index_col=0)
    params = config_df[1].to_dict()

    def parse_list(s):
        if isinstance(s, str):
            items = re.split(r',\s*', s)
            return [x.strip() for x in items if x.strip()]
        return []

    # ==================== 核心参数 ====================
    config = {
        'y_vars': parse_list(params.get('Y_Var', '')),
        'x_vars': parse_list(params.get('X_Var', '')),
        'weight_var': params.get('Weight', ''),
        'filter_var': params.get('Filter_Var', ''),
        'filter_val': params.get('Filter_Value', None),
        'filter_type': 'numeric',  # 默认值
        'factor_rotation': params.get('Factor_Rotation', 'procrustes'),
        'seen_vars': parse_list(params.get('Seen_Var', '')),
        'sample_channel_keyword': params.get('Sample_Channel_Keyword', ''),
        'sample_top2box_label': parse_list(params.get('Sample_Top2Box_Label', 'Net : Top 2 Box')),
    }

    # ---- 从 config_raw 中提取 Filter_Var 行的 C 列作为 filter_type ----
    # 遍历 config_raw，查找 A 列或 B 列包含 'Filter_Var' 的行
    filter_type = 'numeric'  # 默认
    for idx, row in config_raw.iterrows():
        a_col = str(row.iloc[0]).strip().replace('\n', '').replace('\r', '') if pd.notna(row.iloc[0]) else ''
        b_col = str(row.iloc[1]).strip().replace('\n', '').replace('\r', '') if pd.notna(row.iloc[1]) else ''
        # 如果 A 列包含 'Filter_Var'（不区分大小写）
        if 'Filter_Var' in a_col or 'Filter_Var' in b_col:
            # 读取该行的 C 列（索引2）
            c_val = str(row.iloc[2]).strip() if pd.notna(row.iloc[2]) else ''
            if c_val.lower() in ['category', '分类']:
                filter_type = 'category'
            else:
                filter_type = 'numeric'
            break
    config['filter_type'] = filter_type

    # ---- 动态扫描所有回归配置 ----
    reg_configs = []
    for key, value in params.items():
        str_key = str(key) if key is not None else ''
        if str_key.startswith('reg') and str_key.endswith('_dep'):
            reg_num = str_key.replace('_dep', '')
            ind_key = f'{reg_num}_ind'
            if ind_key in params:
                reg_configs.append({
                    'dep': parse_list(value),
                    'ind': parse_list(params.get(ind_key, ''))
                })
    config['reg_configs'] = reg_configs

    # ---- 特殊处理：多行竖排的 Sample_KPI_Keywords ----
    kpi_keywords = []
    kpi_types = []
    kpi_start_row = None

    print("===== 正在扫描Config表查找Sample_KPI_Keywords标题 =====")
    for idx, row in config_raw.iterrows():
        a_col = str(row.iloc[0]).strip().replace('\n', '').replace('\r', '') if pd.notna(row.iloc[0]) else ''
        b_col = str(row.iloc[1]).strip().replace('\n', '').replace('\r', '') if pd.notna(row.iloc[1]) else ''
        print(f"行{idx+1} | A列: {a_col} | B列: {b_col}")
        if 'Sample_KPI_Keywords' in a_col or 'Sample_KPI_Keywords' in b_col:
            kpi_start_row = idx
            print(f"✅ 找到Sample_KPI_Keywords标题行，行号：{idx+1}")
            break

    if kpi_start_row is not None:
        print("===== 开始读取KPI关键词数据 =====")
        for idx in range(kpi_start_row + 1, len(config_raw)):
            row = config_raw.iloc[idx]
            b_col_val = str(row.iloc[1]).strip() if pd.notna(row.iloc[1]) else ''
            c_col_val = str(row.iloc[2]).strip() if pd.notna(row.iloc[2]) else ''
            if b_col_val == '':
                print(f"行{idx+1} B列为空，KPI数据读取结束")
                break
            kpi_keywords.append(b_col_val)
            kpi_type = c_col_val if c_col_val != '' else 'summary'
            kpi_types.append(kpi_type)
            print(f"行{idx+1} | 读取到KPI: {b_col_val} | 类型: {kpi_type}")

    if not kpi_keywords:
        raise ValueError(
            "❌ 未读取到Sample_KPI_Keywords的竖排数据！\n"
            "可能原因：\n"
            "1. 标题行名称错误，需严格包含'Sample_KPI_Keywords'；\n"
            "2. 标题行下方的B列没有有效KPI关键词；\n"
            "3. 表格列顺序错误（需A列标题、B列关键词、C列类型）"
        )

    config['sample_kpi_keywords'] = kpi_keywords
    config['sample_kpi_types'] = kpi_types
    print(f"✅ KPI数据读取完成，共读取{len(kpi_keywords)}个KPI")

    # ---- Factor_Target 读取 ----
    try:
        target = pd.read_excel(
            def_path,
            sheet_name='Factor_Target',
            header=1,
            index_col=2
        )
        factor_cols = [col for col in target.columns if col.startswith('Factor')]
        target = target[factor_cols]
        target = target.fillna(0)
        target = target.apply(lambda series: series.map(lambda x: 1 if x == 1 else 0))
        config['factor_target'] = target
        config['extract_factor_num'] = len(factor_cols)
        if len(factor_cols) == 0:
            raise ValueError("Factor_Target工作表不存在Factor开头的列，请检查Excel！")
        print(f"✅ Factor_Target读取完成，共{len(factor_cols)}个因子")
    except Exception as e:
        print(f"读取Factor_Target工作表异常：{str(e)}")
        config['factor_target'] = None
        config['extract_factor_num'] = None

    return config