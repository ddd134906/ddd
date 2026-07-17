import pyreadstat
import pandas as pd
import numpy as np

def get_name_label_mapping(meta):
    """
    基于SPSS元数据生成 Name(变量英文名) - Label(变量中文标签) 一一映射字典
    用于样本量核对、变量匹配、结果表格展示，Name与Label一一对应
    """
    name_list = meta.column_names
    label_list = meta.column_labels
    name_label_dict = {}
    for name, label in zip(name_list, label_list):
        # 空标签自动填充为变量名，避免空值匹配异常
        label = label if (label is not None and str(label).strip()) else name
        name_label_dict[name] = label
    return name_label_dict

def read_spss(file_path, filter_var=None, filter_val=None):
    """
    读取SPSS .sav文件，可选按指定变量值过滤样本行
    输出三类核心数据：
    1. df：数据表，单元格数值=Value，唯一用于所有数值计算
    2. meta：SPSS元数据，存储变量名、标签、原始样本总数等文本信息
    3. name_label_map：Name-Label对应字典，用于变量核对、报表展示
    """
    # 读取SPSS原始数据与元信息（原有逻辑完全保留）
    df, meta = pyreadstat.read_sav(file_path)
    # 生成变量名-中文标签映射
    name_label_map = get_name_label_mapping(meta)
    # 打印过滤前原始总样本量（新增需求）
    raw_total = meta.number_rows
    print(f"SPSS原始文件总样本量：{raw_total}")

    # 样本过滤分支（原始代码逻辑无修改）
    if filter_var and filter_val is not None and filter_var in df.columns:
        # 自动转换筛选值类型，解决配置读取字符串与数字不匹配问题
        try:
            if isinstance(filter_val, str) and df[filter_var].dtype in (int, float):
                filter_val = float(filter_val) if '.' in filter_val else int(filter_val)
        except:
            pass
        # 筛选目标样本并复制数据，消除pandas视图警告
        df = df[df[filter_var] == filter_val].copy()
        print(f"已过滤：保留 {filter_var}={filter_val}，过滤后剩余样本数 {len(df)}")

    # 返回三大配套数据：数据表、元数据、变量名标签映射
    return df, meta, name_label_map

def recode_binary(series):
    """
    单列二值重编码工具：仅原值=1输出1，空值NaN、其他数字全部转为0
    输入输出均为数值Value，供给因子、回归模型计算使用
    """
    return series.fillna(0).apply(lambda x: 1 if x == 1 else 0)

def recode_variables(df, var_list):
    """
    批量对变量列表做二值重编码，生成新的0/1数值列（Value）
    命名规则：普通变量前缀d；特殊变量Level_B4固定命名BUMO
    """
    for var in var_list:
        # 容错：变量不存在则跳过并打印警告
        if var not in df.columns:
            print(f"警告：变量 {var} 不在数据中，跳过重编码")
            continue
        # 定义新列名称
        if var == 'Level_B4':
            new_name = 'BUMO'
        else:
            new_name = f'd{var}'
        # 调用单列编码函数生成新数值列
        df[new_name] = recode_binary(df[var])
    return df