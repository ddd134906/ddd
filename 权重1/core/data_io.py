import pyreadstat
import pandas as pd
import numpy as np

def get_name_label_mapping(meta):
    name_list = meta.column_names
    label_list = meta.column_labels
    name_label_dict = {}
    for name, label in zip(name_list, label_list):
        label = label if (label is not None and str(label).strip()) else name
        name_label_dict[name] = label
    return name_label_dict

def build_var_to_value_labels(meta):
    """
    构建变量名到值标签映射的字典。
    优先使用 meta.variable_value_labels（如果存在），
    否则通过 meta.column_index 与 meta.value_labels 的键（如 'labels0'）匹配。
    """
    if hasattr(meta, 'variable_value_labels') and meta.variable_value_labels:
        return meta.variable_value_labels

    var_to_vl = {}
    if hasattr(meta, 'column_index') and hasattr(meta, 'value_labels'):
        value_labels = meta.value_labels
        keys = list(value_labels.keys())
        if keys and all(k.startswith('labels') and k[6:].isdigit() for k in keys):
            for var, idx in meta.column_index.items():
                key1 = f'labels{idx}'
                key2 = f'labels{idx+1}'
                if key1 in value_labels:
                    var_to_vl[var] = value_labels[key1]
                elif key2 in value_labels:
                    var_to_vl[var] = value_labels[key2]
        else:
            sorted_keys = sorted(value_labels.keys())
            column_names = meta.column_names
            if len(sorted_keys) == len(column_names):
                for idx, var in enumerate(column_names):
                    var_to_vl[var] = value_labels[sorted_keys[idx]]
    return var_to_vl

def read_spss(file_path, filter_var=None, filter_val=None, filter_type='numeric'):
    df, meta = pyreadstat.read_sav(file_path)
    name_label_map = get_name_label_mapping(meta)
    raw_total = meta.number_rows
    print(f"SPSS原始文件总样本量：{raw_total}")

    var_to_value_labels = build_var_to_value_labels(meta)

    # 如果是分类变量且 filter_val 非空，尝试转换筛选值为数值编码
    if filter_var and filter_type == 'category' and filter_val is not None and filter_var in df.columns:
        value_labels = var_to_value_labels.get(filter_var, {})
        if value_labels:
            # 构建反向映射：标签 -> 数值（处理浮点键）
            label_to_value = {str(v).strip(): k for k, v in value_labels.items()}
            def convert_val(v):
                # 如果 v 是数字字符串，先尝试转为数值
                try:
                    num = int(v) if v.isdigit() else float(v)
                    return num
                except ValueError:
                    pass
                # 否则，在反向映射中查找
                if v in label_to_value:
                    return label_to_value[v]
                # 若找不到，保留原值
                return v

            if isinstance(filter_val, list):
                filter_val = [convert_val(v) for v in filter_val]
            else:
                filter_val = convert_val(filter_val)
            print(f"转换后的筛选值：{filter_val}")
        else:
            print(f"警告：变量 '{filter_var}' 声明为分类变量，但无值标签，无法转换筛选值")

    # 打印值标签（调试）
    if filter_var and filter_type == 'category' and filter_var in var_to_value_labels:
        print(f"变量 '{filter_var}' 的值标签：{var_to_value_labels[filter_var]}")
    elif filter_var and filter_type == 'category':
        print(f"变量 '{filter_var}' 没有值标签或未找到（但被声明为 category）")

    # 样本过滤（后续代码不变）
    if filter_var and filter_val is not None and filter_var in df.columns:
        col_dtype = df[filter_var].dtype
        if isinstance(filter_val, list):
            converted_vals = []
            for v in filter_val:
                if isinstance(v, str):
                    try:
                        if 'int' in str(col_dtype):
                            v = int(v)
                        elif 'float' in str(col_dtype):
                            v = float(v)
                    except ValueError:
                        pass
                converted_vals.append(v)
            df = df[df[filter_var].isin(converted_vals)].copy()
            print(f"已过滤：保留 {filter_var} in {converted_vals}，过滤后剩余样本数 {len(df)}")
        else:
            if isinstance(filter_val, str):
                try:
                    if 'int' in str(col_dtype):
                        filter_val = int(filter_val)
                    elif 'float' in str(col_dtype):
                        filter_val = float(filter_val)
                except ValueError:
                    pass
            df = df[df[filter_var] == filter_val].copy()
            print(f"已过滤：保留 {filter_var}={filter_val}，过滤后剩余样本数 {len(df)}")
    else:
        if filter_var and filter_var not in df.columns:
            print(f"警告：筛选变量 '{filter_var}' 不在数据中，未进行筛选")
        elif filter_val is None:
            print("未设置筛选值，保留全部样本")

    return df, meta, name_label_map, var_to_value_labels

def recode_binary(series):
    return series.fillna(0).apply(lambda x: 1 if x == 1 else 0)

def recode_variables(df, var_list):
    for var in var_list:
        if var not in df.columns:
            print(f"警告：变量 {var} 不在数据中，跳过重编码")
            continue
        if var == 'Level_B4':
            new_name = 'BUMO'
        else:
            new_name = f'd{var}'
        df[new_name] = recode_binary(df[var])
    return df