import pandas as pd
import numpy as np
import re

def normalize(s):
    return re.sub(r'[\s_\-]+', '', str(s)).lower()

def find_table_start(df, keyword, log_callback=print):
    col_a = df.iloc[:, 0].astype(str).str.strip()
    for idx, val in col_a.items():
        if isinstance(val, str) and keyword.lower() in val.lower():
            log_callback(f"找到表头行：行号 {idx}，内容：{val[:50]}...")
            return idx
    log_callback(f"警告：未找到包含 '{keyword}' 的表头行")
    return None

def extract_channel_data(df, start_row, log_callback=print):
    table_name = df.iloc[start_row, 0] if start_row < len(df) else ''
    log_callback(f"表名: {table_name}")

    brand_row_idx = start_row - 1
    brand_row = df.iloc[brand_row_idx] if brand_row_idx >= 0 else None
    brands = []
    if brand_row is not None:
        for val in brand_row.iloc[1:]:
            if pd.notna(val) and str(val).strip() != '':
                brands.append(str(val).strip())
    brands = list(dict.fromkeys(brands))
    log_callback(f"品牌行提取品牌：{brands[:10]}{'...' if len(brands)>10 else ''}")

    channel_rows = []
    for i in range(start_row+1, len(df)):
        row = df.iloc[i]
        a_val = row.iloc[0]
        if pd.isna(a_val) or str(a_val).strip() == '':
            log_callback(f"跳过空行：行号 {i}")
            continue
        a_str = str(a_val).strip()
        if 'base' in a_str.lower():
            log_callback(f"跳过 Base 行：{a_str}")
            continue
        if 'others' in a_str.lower():
            log_callback(f"遇到 Others 行，停止：{a_str}")
            break
        channel_rows.append(row)
        log_callback(f"收集渠道行：{a_str}")

    if not channel_rows:
        raise ValueError("未找到任何渠道行")

    data_df = pd.DataFrame(channel_rows)
    log_callback(f"共收集到 {len(data_df)} 个渠道行")
    return data_df, table_name, brands

def extract_kpi_data(df, start_row, log_callback=print):
    table_name = df.iloc[start_row, 0] if start_row < len(df) else ''
    log_callback(f"KPI 表名: {table_name}")

    brand_row_idx = start_row - 1
    brand_row = df.iloc[brand_row_idx] if brand_row_idx >= 0 else None
    if brand_row is not None:
        log_callback(f"品牌行（表头上一行）A列内容：{brand_row.iloc[0] if pd.notna(brand_row.iloc[0]) else '空'}")

    data_rows = []
    empty_count = 0
    for i in range(start_row+1, len(df)):
        row = df.iloc[i]
        is_empty = row.isnull().all() or all(str(v).strip() == '' for v in row)
        if is_empty:
            empty_count += 1
            if empty_count >= 3:
                log_callback("遇到连续三行空行，停止收集")
                break
            else:
                continue
        else:
            empty_count = 0
            data_rows.append(row)

    if not data_rows:
        raise ValueError("数据区域为空")

    data_df = pd.DataFrame(data_rows)
    log_callback(f"数据区域共 {len(data_df)} 行")
    return data_df, table_name, brand_row

def extract_samples_from_full_table(df_full, config, name_label_map, log_callback=print):
    # --- 渠道表 ---
    channel_keyword = config.get('sample_channel_keyword', '')
    if not channel_keyword:
        raise ValueError("配置缺少 'sample_channel_keyword'")

    log_callback(f"开始搜索渠道表，关键词：{channel_keyword}")
    start_ch = find_table_start(df_full, channel_keyword, log_callback)
    if start_ch is None:
        raise ValueError(f"未找到包含 '{channel_keyword}' 的渠道表头行")

    data_ch, table_name_ch, brands = extract_channel_data(df_full, start_ch, log_callback)
    if data_ch is None or data_ch.empty:
        raise ValueError("渠道表数据区域为空")

    x_vars = config.get('x_vars', [])
    # 获取 SPSS 变量对应的 label
    channel_labels = [name_label_map.get(var, var) for var in x_vars]
    n_spss_channels = len(x_vars)
    log_callback(f"SPSS 渠道变量数量：{n_spss_channels}")
    log_callback(f"Full Table 渠道行数量：{len(data_ch)}")

    if len(data_ch) != n_spss_channels:
        log_callback(f"⚠️ 警告：渠道行数 ({len(data_ch)}) 与 SPSS 变量数 ({n_spss_channels}) 不一致，将按顺序取前 {min(len(data_ch), n_spss_channels)} 个对应")

    n_match = min(len(data_ch), n_spss_channels)
    channel_rows = []
    for i in range(n_match):
        row = data_ch.iloc[i]
        row_name = str(row.iloc[0]).strip()
        spss_var = x_vars[i]
        spss_label = channel_labels[i]

        # 检查包含匹配，输出警告
        is_match = (spss_label.lower() in row_name.lower()) or (row_name.lower() in spss_label.lower())
        if not is_match:
            log_callback(f"⚠️ 警告：第 {i+1} 个渠道行 '{row_name}' 与 SPSS 变量 '{spss_var}' 的 label '{spss_label}' 不匹配（包含匹配失败），但将按顺序对应")

        nums = pd.to_numeric(row.iloc[1:], errors='coerce')
        total = nums.sum(skipna=True)
        channel_rows.append({
            'table_name': table_name_ch,
            'channel_label': row_name,
            'sample': total,
            'var_name': spss_var
        })
        log_callback(f"顺序匹配渠道 {i+1}: {row_name} (对应变量 {spss_var})，样本量 = {total}")

    if len(data_ch) > n_spss_channels:
        log_callback(f"注意：Full Table 中有 {len(data_ch) - n_spss_channels} 个渠道行未被使用（因为 SPSS 变量数不足）")

    channel_df = pd.DataFrame(channel_rows)
    log_callback(f"共匹配 {len(channel_df)} 个渠道")

    # --- KPI 六个表（保持不变）---
    kpi_keywords = config.get('sample_kpi_keywords', [])
    kpi_types = config.get('sample_kpi_types', [])
    if len(kpi_keywords) != 6 or len(kpi_types) != 6:
        raise ValueError(f"KPI 关键词和类型必须各有 6 个，当前关键词数量：{len(kpi_keywords)}，类型数量：{len(kpi_types)}")

    top2_labels = config.get('sample_top2box_label', ['Net : Top 2 Box'])
    kpi_rows = []

    for idx, (kw, typ) in enumerate(zip(kpi_keywords, kpi_types)):
        log_callback(f"处理第 {idx+1} 个 KPI 表，关键词：{kw}，类型：{typ}")
        start_k = find_table_start(df_full, kw, log_callback)
        if start_k is None:
            raise ValueError(f"未找到 KPI 表头: {kw}")
        data_k, table_name_k, brand_row_k = extract_kpi_data(df_full, start_k, log_callback)

        typ = typ.lower()
        if typ == 'summary':
            total_sample = 0
            for row in data_k.itertuples(index=False):
                row_brand = str(row[0]).strip()
                if not row_brand:
                    continue
                for b in brands:
                    if normalize(b) in normalize(row_brand) or normalize(row_brand) in normalize(b):
                        val = pd.to_numeric(row[1], errors='coerce')
                        if not pd.isna(val):
                            total_sample += val
                        break
            kpi_rows.append({
                'table_name': table_name_k,
                'kpi_label': '',
                'sample': total_sample
            })
            log_callback(f"Summary 表 '{kw}' 样本量 = {total_sample}")

        elif typ == 'top2':
            found_row = None
            for row in data_k.itertuples(index=False):
                row_name = str(row[0]).strip()
                if not row_name:
                    continue
                for label in top2_labels:
                    if normalize(label) in normalize(row_name):
                        found_row = row
                        break
                if found_row is not None:
                    break
            if found_row is None:
                raise ValueError(f"在表 '{kw}' 中未找到包含 {top2_labels} 中任一标签的行")

            if brand_row_k is not None:
                col_names = [str(c).strip() for c in brand_row_k.iloc[1:]]
            else:
                col_names = [f"col_{j}" for j in range(1, len(found_row))]

            numeric_vals = []
            for j, val in enumerate(found_row[1:]):
                if j < len(col_names) and 'total' in col_names[j].lower():
                    continue
                num = pd.to_numeric(val, errors='coerce')
                if not pd.isna(num):
                    numeric_vals.append(num)
            total_sample = sum(numeric_vals)
            kpi_rows.append({
                'table_name': table_name_k,
                'kpi_label': found_row[0],
                'sample': total_sample
            })
            log_callback(f"Top2 表 '{kw}' 样本量 = {total_sample}")

        else:
            raise ValueError(f"未知的 KPI 类型: {typ}")

    kpi_df = pd.DataFrame(kpi_rows)
    log_callback("KPI 表提取完成")
    return brands, channel_df, kpi_df