import streamlit as st
from gui.utils import load_csv, coerce_categories, text_filter, download_df_button, page_header, inject_css

st.set_page_config(page_title="04 – Indice di Moran", page_icon="🌍", layout="wide")
inject_css()
page_header("04 – Indice di Moran", "v1.0")

st.markdown("""
**Moran I** misura l'autocorrelazione spaziale globale:
- positivo: clustering di valori simili
- negativo: dissimilarità tra vicini
- vicino a 0: assenza di autocorrelazione
""")

df = coerce_categories(load_csv("moran_summary_all.csv"))
if df is None:
    st.error("File 'moran_summary_all.csv' non trovato."); st.stop()

st.markdown("---")
st.subheader("🔍 Filtri")
col1, col2 = st.columns(2)
areas = ["Tutte"] + sorted(list(df["Area"].dropna().unique())) if "Area" in df.columns else ["Tutte"]
sizes = ["Tutte"] + sorted(list(df["Dimensione"].dropna().unique())) if "Dimensione" in df.columns else ["Tutte"]

with col1: area = st.selectbox("Area", areas)
with col2: size = st.selectbox("Dimensione", sizes)

dfv = df.copy()
if area != "Tutte" and "Area" in dfv.columns: dfv = dfv[dfv["Area"]==area]
if size != "Tutte" and "Dimensione" in dfv.columns: dfv = dfv[dfv["Dimensione"]==size]
dfv = text_filter(dfv, "Cerca subset...")

st.markdown("---")
st.subheader("📋 Tabella")
st.dataframe(dfv, use_container_width=True)
download_df_button(dfv, "moran_filtrato.csv")

st.markdown("---")
st.subheader("📊 Grafici")
import plotly.express as px, plotly.graph_objects as go

if {'Moran_I_strict','Moran_I_border'}.issubset(dfv.columns):
    fig_sc = px.scatter(
        dfv, x='Moran_I_strict', y='Moran_I_border',
        hover_data=[c for c in ['Subset','Area','Dimensione'] if c in dfv.columns],
        title="Confronto Moran I: Strict vs Border", labels={'Moran_I_strict':'Moran I (Strict)','Moran_I_border':'Moran I (Border)'},
        color='Dimensione' if 'Dimensione' in dfv.columns else None, template="plotly_white"
    )
    mn = min(dfv['Moran_I_strict'].min(), dfv['Moran_I_border'].min())
    mx = max(dfv['Moran_I_strict'].max(), dfv['Moran_I_border'].max())
    fig_sc.add_trace(go.Scatter(x=[mn,mx], y=[mn,mx], mode="lines", name="y=x", line=dict(color="red", dash="dash")))
    fig_sc.update_layout(height=520)
    st.plotly_chart(fig_sc, use_container_width=True)

if {'p_strict','p_border'}.issubset(dfv.columns):
    c1, c2 = st.columns(2)
    with c1:
        fig_p1 = px.histogram(dfv, x='p_strict', nbins=30, title='Distribuzione P-value (Strict)', labels={'p_strict':'P-value'}, template="plotly_white")
        fig_p1.add_vline(x=0.05, line_dash="dash", line_color="red", annotation_text="α=0.05")
        st.plotly_chart(fig_p1, use_container_width=True)
    with c2:
        fig_p2 = px.histogram(dfv, x='p_border', nbins=30, title='Distribuzione P-value (Border)', labels={'p_border':'P-value'}, template="plotly_white")
        fig_p2.add_vline(x=0.05, line_dash="dash", line_color="red", annotation_text="α=0.05")
        st.plotly_chart(fig_p2, use_container_width=True)
