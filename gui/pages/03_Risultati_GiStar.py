import streamlit as st, pandas as pd
from gui.utils import load_csv, coerce_categories, text_filter, download_df_button, page_header, inject_css

st.set_page_config(page_title="03 – Risultati Gi*", page_icon="🔥", layout="wide")
inject_css()
page_header("03 – Risultati Gi*", "v1.0")

st.markdown("""
L'analisi **Gi\\*** (Getis-Ord) evidenzia hotspot e coldspot significativi.
""")

df = coerce_categories(load_csv("riepilogo_cluster_GI_completo.csv"))
if df is None:
    st.error("File 'riepilogo_cluster_GI_completo.csv' non trovato."); st.stop()

st.markdown("---")
st.subheader("🔍 Filtri")
variant = st.selectbox("Variante", ["Tutti","W_strict","W_border"])
dfv = df if variant=="Tutti" else df[df["variante"]==variant]
dfv = text_filter(dfv, "Cerca subset/area/dimensione...")

st.markdown("---")
st.subheader("📋 Tabella")
st.dataframe(dfv, use_container_width=True)
download_df_button(dfv, "gi_filtrato.csv")

st.markdown("---")
st.subheader("📊 Grafici")
import plotly.express as px, plotly.graph_objects as go

numeric_cols = [c for c in dfv.columns if c not in ["Subset","variante","Area","Dimensione","model"] and pd.api.types.is_numeric_dtype(dfv[c])]
if not numeric_cols:
    st.info("Nessuna colonna numerica trovata.")
else:
    fig = go.Figure()
    for col in numeric_cols:
        fig.add_trace(go.Bar(name=col, x=dfv["Subset"], y=dfv[col], text=dfv[col], textposition="auto"))
    fig.update_layout(title="Distribuzione Hotspot Gi* per Subset", xaxis_title="Subset", yaxis_title="Numero",
                      barmode="group", height=520, hovermode="x unified", template="plotly_white")
    st.plotly_chart(fig, use_container_width=True)
