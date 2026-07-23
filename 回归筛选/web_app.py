import streamlit as st
import sys
import os
import tempfile
from core import run_analysis

# 设置页面配置
st.set_page_config(
    page_title="ROE SCAN 自动化分析",
    page_icon="🌿",
    layout="wide"
)

# ---------- 自定义 CSS 样式（仿 app.py 绿色主题） ----------
st.markdown("""
<style>
    .stApp {
        background-color: #E8F5E9;
    }
    h1, h2, h3, h4, h5, h6 {
        color: #1B5E20;
    }
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
    .stFileUploader > div {
        border: 2px dashed #4CAF50;
        border-radius: 10px;
        background-color: #C8E6C9;
        padding: 10px;
    }
    .stTextArea > div {
        background-color: #FFFFFF;
        border-radius: 8px;
        border: 1px solid #A5D6A7;
    }
    p, li, label, .stMarkdown {
        color: #1B5E20;
    }
    /* 下载按钮样式 */
    .stDownloadButton > button {
        background-color: #1B5E20;
        color: white;
    }
    .stDownloadButton > button:hover {
        background-color: #0D3B0E;
    }
    /* 主容器内边距 */
    .block-container {
        padding-top: 2rem;
        padding-bottom: 2rem;
    }
</style>
""", unsafe_allow_html=True)

# ---------- 标题 ----------
st.title("ROE SCAN 自动化分析")
st.markdown("上传 SPSS 数据文件和配置文件，点击运行即可获得完整分析结果。")

# ---------- 初始化 session_state ----------
if 'log_messages' not in st.session_state:
    st.session_state.log_messages = []
if 'analysis_done' not in st.session_state:
    st.session_state.analysis_done = False
if 'output_path' not in st.session_state:
    st.session_state.output_path = None

# ---------- 主区域布局（仿 app.py） ----------
# 使用 columns 模拟 app.py 的网格布局
col1, col2 = st.columns([1, 4])

with col1:
    st.markdown("**文件选择**")

with col2:
    spss_file = st.file_uploader("SPSS 数据文件 (.sav)", type=["sav"], label_visibility="collapsed")
    config_file = st.file_uploader("定义文件 (.xlsx)", type=["xlsx"], label_visibility="collapsed")

# 运行按钮居中
col_btn1, col_btn2, col_btn3 = st.columns([1, 2, 1])
with col_btn2:
    run_btn = st.button("运行分析", use_container_width=True, disabled=(spss_file is None or config_file is None))

# 日志显示区域（占大部分空间）
st.markdown("---")
log_placeholder = st.empty()

def update_log_display():
    """更新日志显示区域，显示全部日志"""
    if st.session_state.log_messages:
        full_log = "\n".join(st.session_state.log_messages)
    else:
        full_log = "等待运行..."
    log_placeholder.text_area("📝 运行日志", full_log, height=400, disabled=True, label_visibility="collapsed")

# 初始化显示
update_log_display()

# ---------- 运行逻辑 ----------
if run_btn:
    # 添加分隔线，区分不同运行
    st.session_state.log_messages.append("="*50)
    st.session_state.log_messages.append("开始新的分析任务...")
    update_log_display()

    if spss_file is None or config_file is None:
        st.error("请先上传两个文件！")
    else:
        # 保存上传文件到临时文件
        with tempfile.NamedTemporaryFile(delete=False, suffix=".sav") as tmp_spss:
            tmp_spss.write(spss_file.getvalue())
            spss_path = tmp_spss.name

        with tempfile.NamedTemporaryFile(delete=False, suffix=".xlsx") as tmp_config:
            tmp_config.write(config_file.getvalue())
            config_path = tmp_config.name

        output_path = tempfile.NamedTemporaryFile(delete=False, suffix=".xlsx").name
        st.session_state.output_path = output_path

        # 日志回调
        def log_callback(msg):
            st.session_state.log_messages.append(msg)
            update_log_display()

        # 执行分析
        try:
            run_analysis(spss_path, config_path, output_path, log_callback=log_callback)
            st.session_state.analysis_done = True
            st.success("✅ 分析完成！")
        except Exception as e:
            st.error(f"❌ 分析失败: {e}")
            log_callback(str(e))
        finally:
            # 清理临时文件（可选）
            # os.unlink(spss_path)
            # os.unlink(config_path)
            pass

# 如果分析已完成且结果文件存在，显示下载按钮
if st.session_state.analysis_done and st.session_state.output_path:
    if os.path.exists(st.session_state.output_path):
        with open(st.session_state.output_path, "rb") as f:
            st.download_button(
                label="📥 下载结果 Excel",
                data=f,
                file_name="ROE_Results.xlsx",
                mime="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                use_container_width=True
            )
    else:
        st.warning("结果文件已丢失，请重新运行分析。")
        st.session_state.analysis_done = False
