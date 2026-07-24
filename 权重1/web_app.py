import streamlit as st
import sys
import os
import tempfile
import pandas as pd
from core import run_analysis

# 设置页面配置（必须先于其他st命令）
st.set_page_config(
    page_title="ROE SCAN 自动化分析",
    page_icon="🌿",
    layout="wide"
)

# ---------- 自定义 CSS 样式（绿色主题） ----------
st.markdown("""
<style>
    /* 全局背景色 */
    .stApp {
        background-color: #E8F5E9;
    }
    /* 主标题 */
    h1, h2, h3, h4, h5, h6 {
        color: #1B5E20;
    }
    /* 侧边栏背景色 */
    .css-1d391kg, .css-1d391kg .st-emotion-cache-1wmy9hl {
        background-color: #2E7D32;
    }
    /* 侧边栏文字颜色 */
    .css-1d391kg .st-emotion-cache-1wmy9hl, .css-1d391kg label, .css-1d391kg .st-emotion-cache-1wmy9hl p {
        color: #FFFFFF;
    }
    /* 侧边栏标题 */
    .css-1d391kg .st-emotion-cache-1wmy9hl h1, .css-1d391kg .st-emotion-cache-1wmy9hl h2, .css-1d391kg .st-emotion-cache-1wmy9hl h3 {
        color: #E8F5E9;
    }
    /* 主按钮颜色 */
    .stButton > button {
        background-color: #388E3C;
        color: white;
        border-radius: 8px;
        border: none;
        padding: 0.5rem 2rem;
        font-weight: bold;
    }
    .stButton > button:hover {
        background-color: #2E7D32;
        color: white;
        border: 1px solid #A5D6A7;
    }
    /* 文件上传区域边框 */
    .stFileUploader > div {
        border: 2px dashed #4CAF50;
        border-radius: 10px;
        background-color: #C8E6C9;
    }
    /* 日志区域背景 */
    .stTextArea > div {
        background-color: #FFFFFF;
        border-radius: 8px;
        border: 1px solid #A5D6A7;
    }
    /* 成功/错误消息 */
    .stAlert {
        border-radius: 8px;
    }
    .stAlert .st-emotion-cache-1wmy9hl {
        background-color: #C8E6C9;
    }
    /* 下载按钮 */
    .stDownloadButton > button {
        background-color: #1B5E20;
        color: white;
    }
    .stDownloadButton > button:hover {
        background-color: #0D3B0E;
    }
    /* 进度条等 */
    .stProgress > div {
        background-color: #4CAF50;
    }
    /* 通用文字 */
    p, li, label, .stMarkdown {
        color: #1B5E20;
    }
    /* 侧边栏中文字颜色覆盖 */
    .css-1d391kg p, .css-1d391kg label, .css-1d391kg .stMarkdown {
        color: #FFFFFF;
    }
    /* 侧边栏中链接文字 */
    .css-1d391kg a {
        color: #A5D6A7;
    }
</style>
""", unsafe_allow_html=True)

# ---------- 页面标题 ----------
st.title("ROE SCAN 自动化分析")
st.markdown("上传 SPSS 数据文件和配置文件，点击运行即可获得完整分析结果。")

# ---------- 侧边栏：文件上传 ----------
with st.sidebar:
    st.header("📁 文件上传")
    spss_file = st.file_uploader("上传 SPSS 数据文件 (.sav)", type=["sav"])
    config_file = st.file_uploader("上传配置文件 (.xlsx)", type=["xlsx"])

# ---------- 主区域：运行按钮和日志 ----------
run_btn = st.button("运行", disabled=(spss_file is None or config_file is None))

# 日志显示区域
log_area = st.empty()

# ---------- 运行逻辑 ----------
if run_btn:
    if spss_file is None or config_file is None:
        st.error("请先上传两个文件！")
    else:
        # 将上传的文件保存到临时文件
        with tempfile.NamedTemporaryFile(delete=False, suffix=".sav") as tmp_spss:
            tmp_spss.write(spss_file.getvalue())
            spss_path = tmp_spss.name

        with tempfile.NamedTemporaryFile(delete=False, suffix=".xlsx") as tmp_config:
            tmp_config.write(config_file.getvalue())
            config_path = tmp_config.name

        # 输出结果文件路径
        output_path = tempfile.NamedTemporaryFile(delete=False, suffix=".xlsx").name

        # 自定义日志捕获函数，实时更新显示
        log_messages = []
        def log_callback(msg):
            log_messages.append(msg)
            # 更新显示最新的日志（只显示最后20条，避免过长）
            log_area.text_area("📝 运行日志", "\n".join(log_messages[-20:]), height=300)

        # 执行分析
        try:
            run_analysis(spss_path, config_path, output_path, log_callback=log_callback)
            st.success("✅ 分析完成！点击下方按钮下载结果。")
            # 提供下载链接
            with open(output_path, "rb") as f:
                st.download_button(
                    label="📥 下载结果 Excel",
                    data=f,
                    file_name="ROE_Results.xlsx",
                    mime="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                )
        except Exception as e:
            st.error(f"❌ 分析失败: {e}")
            log_callback(str(e))
        finally:
            # 清理临时文件（可选）
            os.unlink(spss_path)
            os.unlink(config_path)
            os.unlink(output_path)