# -*- coding: utf-8 -*-
from .config import load_definition
from .data_io import read_spss, recode_variables
from .descriptives import get_descriptives, perform_sample_check
from .factor_analysis import perform_factor_analysis
from .regression import run_regression, parse_factor_indicator
from .crosstabs import seen_vs_noseen
from .output import save_results
from .sample_extract import extract_samples_from_full_table
import pandas as pd
import numpy as np

def run_analysis(spss_path, def_path, output_path="ROE_Results", log_callback=print):
    # 第1步：加载配置
    log_callback("开始加载定义文件...")
    config = load_definition(def_path)

    # 品牌筛选值解析（支持多个品牌，逗号分隔）
    filter_var = config.get('filter_var', '')
    filter_type = config.get('filter_type', 'numeric')
    filter_val_raw = config.get('filter_val', None)
    if filter_val_raw is not None:
        filter_val_str = str(filter_val_raw).strip()
        if filter_val_str:
            filter_values = [v.strip() for v in filter_val_str.split(',') if v.strip()]
        else:
            filter_values = []
    else:
        filter_values = []
    log_callback(f"筛选参数：变量='{filter_var}'，值={filter_values}，类型='{filter_type}'")

    # 第2步：读取SPSS数据
    log_callback("读取SPSS数据...")
    filter_val_for_spss = filter_values if len(filter_values) > 1 else (filter_values[0] if filter_values else None)
    df, meta, name_label_map, var_to_value_labels = read_spss(spss_path, filter_var, filter_val_for_spss, filter_type)
    log_callback(f"数据读取成功，样本数: {len(df)}")
    log_callback(f"从SPSS读取到的变量-值标签映射键：{list(var_to_value_labels.keys())}")


    # ============================================================
    # 第 3 步：重编码变量（二值化：1→1，其他→0，空值→0）
    # ============================================================
    log_callback("重编码变量...")
    all_vars = config['y_vars'] + config['x_vars']
    df = recode_variables(df, all_vars)

    # ============================================================
    # 第 4 步：处理 Factor_Target 行索引映射（确定 used_x_vars）
    # ============================================================
    target = config.get('factor_target')
    if target is not None:
        # 构建原始变量名 -> 重编码列名的映射
        orig_to_recoded = {}
        for orig in config['x_vars']:
            if orig == 'Level_B4':
                recoded = 'BUMO'
            else:
                recoded = f'd{orig}'
            orig_to_recoded[orig] = recoded

        # 构建变量名到标签的映射（用于反向查找）
        name_to_label = dict(zip(meta.column_names, meta.column_labels))
        label_to_name = {label: name for name, label in zip(meta.column_names, meta.column_labels)}

        mapped_index = []
        for item in target.index:
            mapped_name = None
            # 1. 直接匹配重编码列名（现在 df 已包含这些列）
            if item in df.columns:
                mapped_name = item
            # 2. 匹配原始变量名（然后转重编码）
            elif item in orig_to_recoded:
                mapped_name = orig_to_recoded[item]
            # 3. 匹配标签（通过标签找到原始变量名，再转重编码）
            elif item in label_to_name:
                orig_name = label_to_name[item]
                if orig_name in orig_to_recoded:
                    mapped_name = orig_to_recoded[orig_name]
            # 4. 如果 item 是原始变量名但不含在 x_vars 中（但直接匹配列名已处理）
            else:
                if item in df.columns:
                    mapped_name = item

            if mapped_name is None:
                raise ValueError(f"目标矩阵行标签 '{item}' 无法对应到任何数据列，请检查")
            if mapped_name not in df.columns:
                raise ValueError(f"映射后的列名 '{mapped_name}' 不在数据中，请检查")
            mapped_index.append(mapped_name)

        # 更新目标矩阵的索引为映射后的列名，保持行顺序
        target.index = mapped_index
        config['factor_target'] = target
        used_x_vars = mapped_index  # 使用目标矩阵的行顺序作为变量顺序
    else:
        # 如果没有目标矩阵，则回退到原始 x_vars 顺序（此时重编码列已存在）
        used_x_vars = []
        for x in config['x_vars']:
            if x == 'Level_B4':
                var = 'BUMO'
            else:
                var = f'd{x}'
            if var in df.columns:
                used_x_vars.append(var)
            elif x in df.columns:
                used_x_vars.append(x)
            else:
                log_callback(f"警告：变量 {x} 重编码后不存在，已忽略")

    # ============================================================
    # 第 5 步：构建 Y 变量清单
    # ============================================================
    # ---- Y 变量 ----
    # 原始名清单（过滤掉不存在的列，用于样本核对）
    used_y_vars_orig = [y for y in config['y_vars'] if y in df.columns]

    # 重编码名清单：Level_B4 → BUMO，其他 → d{原始名}
    used_y_vars_recoded = []
    for y in config['y_vars']:
        if y == 'Level_B4':
            var = 'BUMO'
        else:
            var = f'd{y}'
        if var in df.columns:
            used_y_vars_recoded.append(var)

    # ---- 存入 config 供后续使用 ----
    config['used_x_vars'] = used_x_vars
    config['used_y_vars_orig'] = used_y_vars_orig
    config['used_y_vars_recoded'] = used_y_vars_recoded

    # ============================================================
    # 第 6 步：描述统计
    # ============================================================
    desc_raw_y = get_descriptives(df, used_y_vars_orig)       # 原始 Y
    desc_d_y = get_descriptives(df, used_y_vars_recoded)      # 重编码 Y
    desc_raw_x = get_descriptives(df, config['x_vars'])       # 原始 X

    desc_list = []
    if not desc_raw_y.empty:
        desc_list.append(('Original KPIs', desc_raw_y))
    if not desc_d_y.empty:
        desc_list.append(('Recoded KPIs', desc_d_y))
    if not desc_raw_x.empty:
        desc_list.append(('Original Channels', desc_raw_x))

    log_callback("描述统计完成")

    # ============================================================
    # 第 7 步：样本量核对（Full_Table 对比）
    # ============================================================
    try:
        log_callback("开始样本量核对...")
        full_table_df = pd.read_excel(def_path, sheet_name='Full_Table', header=None)
        brands, channel_df, kpi_df = extract_samples_from_full_table(
    full_table_df, config, name_label_map,
    filter_values=filter_values,
    var_to_value_labels=var_to_value_labels,
    filter_type=filter_type,
    log_callback=log_callback
        )

        kpi_check_df, channel_check_df, success = perform_sample_check(
            desc_raw_y, desc_d_y, desc_raw_x, channel_df, kpi_df, name_label_map,
            config['y_vars'], config['x_vars']
        )

        if success:
            log_callback("样本量校对成功")
        else:
            log_callback("样本量校对失败，请检查差异")

        config['kpi_check_df'] = kpi_check_df
        config['channel_check_df'] = channel_check_df
        config['sample_check_success'] = success

    except Exception as e:
        log_callback(f"样本量核对过程中发生错误: {e}")
        kpi_check_df = None
        channel_check_df = None
        success = False
        config['kpi_check_df'] = None
        config['channel_check_df'] = None
        config['sample_check_success'] = False

    # ============================================================
    # 第 8 步：因子分析（PCA + 旋转）
    # ============================================================
    log_callback("执行因子分析...")
    if not used_x_vars:
        raise ValueError("没有可用的X变量用于因子分析")

    #corr_matrix = df[used_x_vars].corr()
    #config['corr_matrix'] = corr_matrix

    # 执行因子分析，生成 Factor 列追加到 df
    _, loadings, score_coeff, factor_scores, df, factor_corr, fa_extra = perform_factor_analysis(df, config)
    n_factors = fa_extra['n_factors'] if fa_extra else 0
    log_callback(f"因子分析完成，因子数: {n_factors}")

    # ============================================================
    # 第 9 步：回归分析（动态支持任意数量）【修改：直接使用配置中的因变量列名】
    # ============================================================
    reg_results = {}
    weight_var = config.get('weight_var', '')

    for idx, reg_cfg in enumerate(config.get('reg_configs', []), start=1):
        dep_orig = reg_cfg.get('dep', [])
        ind_orig = reg_cfg.get('ind', [])

        if not dep_orig or not ind_orig:
            log_callback(f"警告：回归 {idx} 配置不完整（缺因变量或自变量），跳过")
            continue

        # ---- 9.1 因变量：直接使用配置中的列名（不做任何转换） ----
        dep_vars = [d for d in dep_orig if d in df.columns]
        if not dep_vars:
            log_callback(f"警告：回归 {idx} 的因变量均不存在于数据中，跳过")
            continue

        # ---- 9.2 自变量：解析 Factor 编号 ----
        ind_parsed = parse_factor_indicator(ind_orig, n_factors)
        if not ind_parsed:
            log_callback(f"警告：回归 {idx} 的自变量解析后为空，跳过")
            continue

        # ---- 9.3 执行回归 ----
        log_callback(f"运行回归 {idx}...")
        reg_result = run_regression(
            df=df,
            dep_vars=dep_vars,
            ind_vars=ind_parsed,
            weight_var=weight_var,
            center=False   # 启用中心化
        )
        reg_results[f'Reg{idx}'] = reg_result

    # ============================================================
    # 第 10 步：Seen vs No Seen（看过 vs 没看过分组对比）
    # ============================================================
    log_callback("计算Seen vs No Seen...")
    qd_stats, qd_desc, qd_means, channel_means_dict, diff_table = seen_vs_noseen(
        df=df,
        x_vars=config['x_vars'],             #✅原始变量名
        y_vars=config['y_vars'],
        seen_vars=config.get('seen_vars', None)
    )

    # ============================================================
    # 第 11 步：保存结果
    # ============================================================
    log_callback("保存结果...")
    save_results(
        output_path=output_path,
        desc_list=desc_list,
        loadings=loadings,
        score_coeff=score_coeff,
        factor_corr=factor_corr,
        reg_results=reg_results,
	qd_stats=qd_stats,
	qd_desc=qd_desc,
        qd_means=qd_means,
        channel_means_dict=channel_means_dict,
	diff_table=diff_table,
        fa_extra=fa_extra,
        kpi_check_df=kpi_check_df,
        channel_check_df=channel_check_df,
        sample_check_success=success
    )
    log_callback(f"分析完成！结果保存至: {output_path}")