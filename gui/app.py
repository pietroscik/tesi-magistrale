"""
Dashboard Interattiva per Analisi Spaziale delle Performance delle Imprese Italiane
Tesi di Laurea Magistrale - Pietro Maietta
"""

import os
from pathlib import Path

import streamlit as st
import pandas as pd

# Plotly (opzionale). Se manca, usiamo Matplotlib come fallback.
try:
    import plotly.express as px
    import plotly.graph_objects as go
    PLOTLY_OK = True
except Exception:
    PLOTLY_OK = False
    import matplotlib.pyplot as plt  # fallback


# -----------------------------------------------------------------------------
# Configurazione pagina
# -----------------------------------------------------------------------------
st.set_page_config(
    page_title="Analisi Spaziale ISP",
    page_icon="📊",
    layout="wide",
    initial_sidebar_state="expanded",
)


# -----------------------------------------------------------------------------
# Percorsi (robusti dalla root del repo, anche su Streamlit Cloud)
# -----------------------------------------------------------------------------
# __file__ = .../gui/app.py  → repo root = parents[1]
ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = ROOT / "05_analysis_spatial" / "03_tables"
MAPS_DIR = ROOT / "05_analysis_spatial" / "02_maps"


# -----------------------------------------------------------------------------
# Loader & utilità
# -----------------------------------------------------------------------------
@st.cache_data
def load_csv(filename: str) -> pd.DataFrame | None:
    """Carica un CSV da 05_analysis_spatial/03_tables; ritorna None se assente."""
    fp = DATA_DIR / filename
    if fp.exists():
        try:
            return pd.read_csv(fp)
        except Exception as e:
            st.error(f"Errore nel leggere {fp.name}: {e}")
            return None
    return None


@st.cache_data
def get_available_pdfs() -> list[str]:
    """Elenco **ricorsivo** dei PDF (path relativi a MAPS_DIR)."""
    if not MAPS_DIR.exists():
        return []
    files = sorted([p for p in MAPS_DIR.rglob("*.pdf")])
    return [str(p.relative_to(MAPS_DIR)) for p in files]


# -----------------------------------------------------------------------------
# Sidebar
# -----------------------------------------------------------------------------
st.sidebar.title("📊 Dashboard ISP")
st.sidebar.markdown("---")

page = st.sidebar.radio(
    "Navigazione",
    ["🏠 Overview", "📍 Analisi LISA", "🔥 Hotspot Gi*", "📈 Modelli Econometrici", "🌍 Indice di Moran", "🗺️ Mappe Spaziali"],
)

st.sidebar.markdown("---")
st.sidebar.info(
    "**Tesi di Laurea Magistrale**\n\n"
    "Analisi Spaziale delle Performance delle Imprese Italiane\n\n"
    "**Autore:** Pietro Maietta\n\n"
    "---\n\n"
    "🌐 **Dashboard Online:**\n"
    "[pietromaietta.streamlit.app](https://pietromaietta.streamlit.app/)"
)


# =============================================================================
# PAGINA: OVERVIEW
# =============================================================================
if page == "🏠 Overview":
    st.title("🏠 Overview dell'Analisi Spaziale")

    st.markdown(
        """
        ## Benvenuto nella Dashboard Interattiva

        Questa dashboard presenta i risultati dell'analisi spaziale delle performance delle imprese italiane,
        utilizzando l'**Indice di Sostenibilità Potenziale (ISP)** come variabile dipendente.

        ### 📋 Struttura dell'Analisi
        """
    )

    col1, col2 = st.columns(2)
    with col1:
        st.markdown(
            r"""
            #### 🔍 Analisi dei Cluster
            - **LISA (Local Indicators of Spatial Association)**: identifica cluster locali di valori simili
            - **Gi\*** (Getis-Ord): identifica hotspot e coldspot statisticamente significativi

            #### 📊 Modelli Econometrici
            - **OLS**: Ordinary Least Squares (baseline)
            - **SAR**: Spatial Autoregressive Model
            - **SDM**: Spatial Durbin Model
            - **GMM**: Generalized Method of Moments
            """
        )
    with col2:
        st.markdown(
            """
            #### 🌐 Confronto Strict vs Border
            - **Strict**: matrice di prossimità senza considerare i confini regionali
            - **Border**: matrice di prossimità che considera i confini regionali

            #### 📏 Segmentazione
            - Per **Dimensione**: Micro, Piccola, Media, Grande
            - Per **Area Geografica**: Nord-Ovest, Nord-Est, Centro, Sud, Isole
            """
        )

    st.markdown("---")

    # Statistiche generali
    st.subheader("📊 Statistiche Generali")

    lisa_complete = load_csv("riepilogo_cluster_LISA_completo.csv")
    gi_complete = load_csv("riepilogo_cluster_GI_completo.csv")
    model_detail = load_csv("riepilogo_modello_dettaglio.csv")

    c1, c2, c3, c4 = st.columns(4)
    with c1:
        if isinstance(lisa_complete, pd.DataFrame) and "Subset" in lisa_complete.columns:
            st.metric("Subset Analizzati", int(lisa_complete["Subset"].nunique()))
        else:
            st.metric("Subset Analizzati", "N/A")

    with c2:
        if isinstance(model_detail, pd.DataFrame):
            st.metric("Modelli Stimati", len(model_detail))
        else:
            st.metric("Modelli Stimati", "N/A")

    with c3:
        st.metric("Mappe Disponibili", len(get_available_pdfs()))

    with c4:
        csv_files = list(DATA_DIR.glob("*.csv")) if DATA_DIR.exists() else []
        st.metric("File CSV", len(csv_files))

    st.markdown("---")

    # Dataset disponibili
    st.subheader("📁 Dataset Disponibili")
    if DATA_DIR.exists():
        csv_files = sorted([f.name for f in DATA_DIR.glob("*.csv")])
        if csv_files:
            st.markdown("**File CSV principali:**")
            for i, csv_file in enumerate(csv_files, 1):
                st.write(f"{i}. `{csv_file}`")
        else:
            st.warning("Nessun file CSV trovato nella directory dei dati.")
    else:
        st.error(f"Directory non trovata: {DATA_DIR}")


# =============================================================================
# PAGINA: ANALISI LISA
# =============================================================================
elif page == "📍 Analisi LISA":
    st.title("📍 Analisi dei Cluster LISA")

    st.markdown(
        """
        L'analisi **LISA (Local Indicators of Spatial Association)** identifica cluster spaziali locali:
        - **High-High (HH)**: cluster di valori alti circondati da valori alti
        - **Low-Low (LL)**: cluster di valori bassi circondati da valori bassi
        - **High-Low (HL)**: valori alti circondati da valori bassi (outlier)
        - **Low-High (LH)**: valori bassi circondati da valori alti (outlier)
        """
    )

    lisa_complete = load_csv("riepilogo_cluster_LISA_completo.csv")
    if isinstance(lisa_complete, pd.DataFrame):
        st.markdown("---")

        st.subheader("🔍 Filtri")
        selected_variant = st.selectbox("Seleziona Variante", ["Tutti", "W_strict", "W_border"])

        df_filtered = lisa_complete.copy()
        if selected_variant != "Tutti" and "variante" in df_filtered.columns:
            df_filtered = df_filtered[df_filtered["variante"] == selected_variant]

        st.markdown("---")
        st.subheader("📋 Tabella Completa dei Cluster LISA")
        st.dataframe(df_filtered, use_container_width=True)

        st.markdown("---")
        st.subheader("📊 Visualizzazioni")

        cluster_cols = ["High-High", "Low-Low", "High-Low", "Low-High"]
        cluster_cols = [c for c in cluster_cols if c in df_filtered.columns]

        # Grafico a barre per subset
        if PLOTLY_OK:
            fig_bars = go.Figure()
            for col in cluster_cols:
                fig_bars.add_trace(
                    go.Bar(
                        name=col,
                        x=df_filtered["Subset"],
                        y=df_filtered[col],
                        text=df_filtered[col],
                        textposition="auto",
                    )
                )
            fig_bars.update_layout(
                title="Distribuzione dei Cluster LISA per Subset",
                xaxis_title="Subset",
                yaxis_title="Numero di Osservazioni",
                barmode="group",
                height=500,
                hovermode="x unified",
            )
            st.plotly_chart(fig_bars, use_container_width=True)
        else:
            fig, ax = plt.subplots(figsize=(10, 4))
            df_plot = df_filtered.set_index("Subset")[cluster_cols]
            df_plot.plot(kind="bar", ax=ax)
            ax.set_title("Distribuzione dei Cluster LISA per Subset")
            st.pyplot(fig)

        # Confronto Strict vs Border
        if selected_variant == "Tutti" and "variante" in lisa_complete.columns:
            st.markdown("---")
            st.subheader("🔄 Confronto Strict vs Border")

            strict_data = lisa_complete[lisa_complete["variante"] == "W_strict"].copy()
            border_data = lisa_complete[lisa_complete["variante"] == "W_border"].copy()

            comparison_rows = []
            for _, r_s in strict_data.iterrows():
                subset = r_s["Subset"]
                r_b = border_data[border_data["Subset"] == subset]
                if not r_b.empty:
                    for col in cluster_cols:
                        val_s = r_s[col] if pd.notna(r_s[col]) else 0
                        val_b = r_b[col].values[0] if pd.notna(r_b[col].values[0]) else 0
                        comparison_rows.append(
                            {"Subset": subset, "Tipo Cluster": col, "Strict": val_s, "Border": val_b}
                        )

            if comparison_rows:
                df_comparison = pd.DataFrame(comparison_rows)
                selected_cluster = st.selectbox("Seleziona Tipo di Cluster", cluster_cols)

                dfc = df_comparison[df_comparison["Tipo Cluster"] == selected_cluster]
                if PLOTLY_OK:
                    fig_comp = go.Figure()
                    fig_comp.add_trace(go.Bar(name="Strict", x=dfc["Subset"], y=dfc["Strict"]))
                    fig_comp.add_trace(go.Bar(name="Border", x=dfc["Subset"], y=dfc["Border"]))
                    fig_comp.update_layout(
                        title=f"Confronto Strict vs Border - {selected_cluster}",
                        xaxis_title="Subset",
                        yaxis_title="Numero di Osservazioni",
                        barmode="group",
                        height=500,
                    )
                    st.plotly_chart(fig_comp, use_container_width=True)
                else:
                    fig, ax = plt.subplots(figsize=(10, 4))
                    dfp = dfc.set_index("Subset")[["Strict", "Border"]]
                    dfp.plot(kind="bar", ax=ax)
                    ax.set_title(f"Confronto Strict vs Border - {selected_cluster}")
                    st.pyplot(fig)

    else:
        st.error("Impossibile caricare i dati LISA. Verificare 'riepilogo_cluster_LISA_completo.csv'.")


# =============================================================================
# PAGINA: HOTSPOT GI*
# =============================================================================
elif page == "🔥 Hotspot Gi*":
    st.title("🔥 Analisi Hotspot Gi*")

    st.markdown(
        r"""
        L'analisi **Gi\*** (Getis-Ord) identifica hotspot e coldspot statisticamente significativi:
        - **Hotspot**: aree con valori significativamente alti
        - **Coldspot**: aree con valori significativamente bassi
        """
    )

    gi_complete = load_csv("riepilogo_cluster_GI_completo.csv")
    if isinstance(gi_complete, pd.DataFrame):
        st.markdown("---")

        st.subheader("🔍 Filtri")
        selected_variant = st.selectbox("Seleziona Variante", ["Tutti", "W_strict", "W_border"], key="gi_variant")

        df_filtered = gi_complete.copy()
        if selected_variant != "Tutti" and "variante" in df_filtered.columns:
            df_filtered = df_filtered[df_filtered["variante"] == selected_variant]

        st.markdown("---")
        st.subheader("📋 Tabella Completa degli Hotspot Gi*")
        st.dataframe(df_filtered, use_container_width=True)

        st.markdown("---")
        st.subheader("📊 Visualizzazioni")

        numeric_cols = [c for c in df_filtered.columns if c not in ["Subset", "variante"]]
        if numeric_cols:
            if PLOTLY_OK:
                fig_gi = go.Figure()
                for col in numeric_cols:
                    fig_gi.add_trace(
                        go.Bar(
                            name=col,
                            x=df_filtered["Subset"],
                            y=df_filtered[col],
                            text=df_filtered[col],
                            textposition="auto",
                        )
                    )
                fig_gi.update_layout(
                    title="Distribuzione degli Hotspot Gi* per Subset",
                    xaxis_title="Subset",
                    yaxis_title="Numero di Osservazioni",
                    barmode="group",
                    height=500,
                    hovermode="x unified",
                )
                st.plotly_chart(fig_gi, use_container_width=True)
            else:
                fig, ax = plt.subplots(figsize=(10, 4))
                dfp = df_filtered.set_index("Subset")[numeric_cols]
                dfp.plot(kind="bar", ax=ax)
                ax.set_title("Distribuzione degli Hotspot Gi* per Subset")
                st.pyplot(fig)
    else:
        st.error("Impossibile caricare i dati Gi*. Verificare 'riepilogo_cluster_GI_completo.csv'.")


# =============================================================================
# PAGINA: MODELLI ECONOMETRICI
# =============================================================================
elif page == "📈 Modelli Econometrici":
    st.title("📈 Confronto Modelli Econometrici")

    st.markdown(
        """
        Confronto tra diversi modelli di regressione spaziale:
        - **OLS**: Ordinary Least Squares (modello baseline senza componenti spaziali)
        - **SAR**: Spatial Autoregressive Model (dipendenza spaziale in Y)
        - **SDM**: Spatial Durbin Model (ritardi spaziali in Y e X)
        - **GMM**: Generalized Method of Moments
        """
    )

    model_detail = load_csv("riepilogo_modello_dettaglio.csv")
    gwr_summary = load_csv("riepilogo_regressivo_gwr.csv")

    if isinstance(model_detail, pd.DataFrame):
        st.markdown("---")

        st.subheader("🔍 Filtri")
        c1, c2, c3 = st.columns(3)

        with c1:
            areas = ["Tutte"]
            if "Area" in model_detail.columns:
                areas += sorted(model_detail["Area"].dropna().unique().tolist())
            selected_area = st.selectbox("Area Geografica", areas)

        with c2:
            sizes = ["Tutte"]
            if "Dimensione" in model_detail.columns:
                sizes += sorted(model_detail["Dimensione"].dropna().unique().tolist())
            selected_size = st.selectbox("Dimensione", sizes)

        with c3:
            models = ["Tutti"]
            if "model" in model_detail.columns:
                models += sorted(model_detail["model"].dropna().unique().tolist())
            selected_model = st.selectbox("Modello", models)

        df_filtered = model_detail.copy()
        if selected_area != "Tutte" and "Area" in df_filtered.columns:
            df_filtered = df_filtered[df_filtered["Area"] == selected_area]
        if selected_size != "Tutte" and "Dimensione" in df_filtered.columns:
            df_filtered = df_filtered[df_filtered["Dimensione"] == selected_size]
        if selected_model != "Tutti" and "model" in df_filtered.columns:
            df_filtered = df_filtered[df_filtered["model"] == selected_model]

        st.markdown("---")
        st.subheader("📋 Dettaglio dei Modelli")
        st.dataframe(df_filtered, use_container_width=True)

        st.markdown("---")
        st.subheader("📊 Confronto AIC per Modello")

        if "AIC" in df_filtered.columns and "model" in df_filtered.columns:
            df_aic = df_filtered[df_filtered["AIC"].notna()].copy()
            if not df_aic.empty:
                if PLOTLY_OK:
                    fig_aic = px.box(
                        df_aic,
                        x="model",
                        y="AIC",
                        color="model",
                        title="Distribuzione AIC per Modello",
                        labels={"model": "Modello", "AIC": "AIC"},
                    )
                    fig_aic.update_layout(height=500, showlegend=False)
                    st.plotly_chart(fig_aic, use_container_width=True)
                else:
                    fig, ax = plt.subplots(figsize=(8, 4))
                    df_aic.boxplot(column="AIC", by="model", ax=ax)
                    ax.set_title("Distribuzione AIC per Modello")
                    ax.set_xlabel("Modello")
                    ax.set_ylabel("AIC")
                    st.pyplot(fig)

                st.markdown("### 📊 Statistiche AIC")
                aic_stats = df_aic.groupby("model")["AIC"].agg(["count", "mean", "std", "min", "max"]).round(2)
                aic_stats.columns = ["N. Modelli", "Media", "Dev. Std.", "Min", "Max"]
                st.dataframe(aic_stats, use_container_width=True)

        st.markdown("---")
        st.subheader("🏆 Modelli Migliori per Subset")
        if "best_by_AIC" in df_filtered.columns:
            best_models = df_filtered[df_filtered["best_by_AIC"] == True]
            if not best_models.empty:
                cols = [c for c in ["Subset", "Area", "Dimensione", "model", "AIC"] if c in best_models.columns]
                st.dataframe(best_models[cols], use_container_width=True)
            else:
                st.info("Nessun modello marcato come migliore nei dati filtrati.")

        if isinstance(gwr_summary, pd.DataFrame):
            st.markdown("---")
            st.subheader("🗺️ Analisi GWR (Geographically Weighted Regression)")
            st.dataframe(gwr_summary, use_container_width=True)
    else:
        st.error("Impossibile caricare i dati dei modelli. Verificare 'riepilogo_modello_dettaglio.csv'.")


# =============================================================================
# PAGINA: INDICE DI MORAN
# =============================================================================
elif page == "🌍 Indice di Moran":
    st.title("🌍 Analisi dell'Indice di Moran")

    st.markdown(
        """
        L'**Indice di Moran (I)** misura l'autocorrelazione spaziale globale:
        - Valori positivi → clustering positivo (valori simili vicini)
        - Valori negativi → clustering negativo (valori dissimili vicini)
        - Valori ~0 → assenza di autocorrelazione
        Il **p-value** indica la significatività statistica.
        """
    )

    moran_summary = load_csv("moran_summary_all.csv")
    if isinstance(moran_summary, pd.DataFrame):
        st.markdown("---")

        st.subheader("🔍 Filtri")
        c1, c2 = st.columns(2)

        with c1:
            if "Area" in moran_summary.columns:
                areas = ["Tutte"] + sorted(moran_summary["Area"].dropna().unique().tolist())
                selected_area = st.selectbox("Area Geografica", areas, key="moran_area")
            else:
                selected_area = "Tutte"

        with c2:
            if "Dimensione" in moran_summary.columns:
                sizes = ["Tutte"] + sorted(moran_summary["Dimensione"].dropna().unique().tolist())
                selected_size = st.selectbox("Dimensione", sizes, key="moran_size")
            else:
                selected_size = "Tutte"

        df_filtered = moran_summary.copy()
        if selected_area != "Tutte" and "Area" in df_filtered.columns:
            df_filtered = df_filtered[df_filtered["Area"] == selected_area]
        if selected_size != "Tutte" and "Dimensione" in df_filtered.columns:
            df_filtered = df_filtered[df_filtered["Dimensione"] == selected_size]

        st.markdown("---")
        st.subheader("📋 Tabella Completa dell'Indice di Moran")
        st.dataframe(df_filtered, use_container_width=True)

        st.markdown("---")
        st.subheader("📊 Visualizzazioni")

        # Scatter: Moran I strict vs border
        if {"Moran_I_strict", "Moran_I_border"}.issubset(df_filtered.columns):
            if PLOTLY_OK:
                fig_scatter = px.scatter(
                    df_filtered,
                    x="Moran_I_strict",
                    y="Moran_I_border",
                    hover_data=[c for c in ["Subset", "Area", "Dimensione"] if c in df_filtered.columns],
                    title="Confronto Moran I: Strict vs Border",
                    labels={"Moran_I_strict": "Moran I (Strict)", "Moran_I_border": "Moran I (Border)"},
                    color="Dimensione" if "Dimensione" in df_filtered.columns else None,
                )
                min_val = min(df_filtered["Moran_I_strict"].min(), df_filtered["Moran_I_border"].min())
                max_val = max(df_filtered["Moran_I_strict"].max(), df_filtered["Moran_I_border"].max())
                fig_scatter.add_trace(
                    go.Scatter(x=[min_val, max_val], y=[min_val, max_val], mode="lines", name="y=x",
                               line=dict(color="red", dash="dash"))
                )
                fig_scatter.update_layout(height=500)
                st.plotly_chart(fig_scatter, use_container_width=True)
            else:
                fig, ax = plt.subplots(figsize=(5, 5))
                ax.scatter(df_filtered["Moran_I_strict"], df_filtered["Moran_I_border"], s=12)
                min_val = min(df_filtered["Moran_I_strict"].min(), df_filtered["Moran_I_border"].min())
                max_val = max(df_filtered["Moran_I_strict"].max(), df_filtered["Moran_I_border"].max())
                ax.plot([min_val, max_val], [min_val, max_val], "r--")
                ax.set_xlabel("Moran I (Strict)")
                ax.set_ylabel("Moran I (Border)")
                st.pyplot(fig)

        # Distribuzione p-value
        if {"p_strict", "p_border"}.issubset(df_filtered.columns):
            st.markdown("### 📉 Distribuzione dei P-value")
            c1, c2 = st.columns(2)

            with c1:
                if PLOTLY_OK:
                    fig_ps = px.histogram(
                        df_filtered, x="p_strict", nbins=30, title="Distribuzione P-value (Strict)", labels={"p_strict": "P-value"}
                    )
                    fig_ps.add_vline(x=0.05, line_dash="dash", line_color="red", annotation_text="α=0.05")
                    st.plotly_chart(fig_ps, use_container_width=True)
                else:
                    fig, ax = plt.subplots(figsize=(5, 4))
                    ax.hist(df_filtered["p_strict"].dropna(), bins=30)
                    ax.axvline(0.05, color="red", linestyle="--")
                    st.pyplot(fig)

            with c2:
                if PLOTLY_OK:
                    fig_pb = px.histogram(
                        df_filtered, x="p_border", nbins=30, title="Distribuzione P-value (Border)", labels={"p_border": "P-value"}
                    )
                    fig_pb.add_vline(x=0.05, line_dash="dash", line_color="red", annotation_text="α=0.05")
                    st.plotly_chart(fig_pb, use_container_width=True)
                else:
                    fig, ax = plt.subplots(figsize=(5, 4))
                    ax.hist(df_filtered["p_border"].dropna(), bins=30)
                    ax.axvline(0.05, color="red", linestyle="--")
                    st.pyplot(fig)
    else:
        st.error("Impossibile caricare i dati di Moran. Verificare 'moran_summary_all.csv'.")


# =============================================================================
# PAGINA: MAPPE SPAZIALI
# =============================================================================
elif page == "🗺️ Mappe Spaziali":
    st.title("🗺️ Mappe Spaziali")

    st.markdown(
        """
        Visualizzazione delle mappe PDF generate dall'analisi spaziale.
        Queste mappe mostrano i risultati geografici dei cluster LISA e degli hotspot Gi*.
        """
    )

    available_pdfs = get_available_pdfs()
    if available_pdfs:
        st.markdown("---")
        st.subheader("📁 Seleziona una Mappa")

        # Categorie in base al filename (usa solo il nome, non il path)
        def fname(p: str) -> str:
            return Path(p).name

        pdf_categories = {
            "🗺️ Mappe Finali Aggregate": [p for p in available_pdfs if "MAPPA_FINALE" in fname(p)],
            "📍 Analisi Nazionale": [p for p in available_pdfs if any(x in fname(p) for x in ["nazionale", "grafo", "imprese"])],
            "📊 Altri": [
                p
                for p in available_pdfs
                if "MAPPA_FINALE" not in fname(p) and not any(x in fname(p) for x in ["nazionale", "grafo", "imprese"])
            ],
        }

        for category, pdfs in pdf_categories.items():
            if not pdfs:
                continue

            st.markdown(f"### {category}")
            selected_pdf = st.selectbox(f"Seleziona mappa da {category}", pdfs, key=f"pdf_{category}")
            if selected_pdf:
                pdf_path = MAPS_DIR / selected_pdf  # path relativo a MAPS_DIR (può includere sottocartelle)

                if pdf_path.exists():
                    size_kb = pdf_path.stat().st_size / 1024
                    st.info(f"📄 **File:** `{selected_pdf}` | **Dimensione:** {size_kb:.1f} KB")

                    with open(pdf_path, "rb") as fh:
                        st.download_button(
                            label="⬇️ Scarica PDF",
                            data=fh,
                            file_name=fname(selected_pdf),
                            mime="application/pdf",
                        )

                    st.markdown("---")
                    st.info(
                        "💡 **Nota:** scarica il PDF per visualizzarlo nel browser. "
                        "Alcuni ambienti Cloud non consentono l'embed diretto dei PDF."
                    )
                else:
                    st.error(f"File non trovato: {pdf_path}")

            st.markdown("---")

        with st.expander("📋 Visualizza tutte le mappe disponibili"):
            for i, p in enumerate(available_pdfs, 1):
                st.write(f"{i}. {p}")
    else:
        st.warning("Nessuna mappa PDF trovata nella directory delle mappe.")
        if MAPS_DIR.exists():
            st.info(f"Directory verificata: `{MAPS_DIR}`")
        else:
            st.error(f"Directory non trovata: `{MAPS_DIR}`")


# -----------------------------------------------------------------------------
# Footer
# -----------------------------------------------------------------------------
st.markdown("---")
st.markdown(
    "<div style='text-align: center; color: gray; padding: 20px;'>"
    "Dashboard creata con Streamlit | Tesi di Laurea Magistrale - Pietro Maietta"
    "</div>",
    unsafe_allow_html=True,
)
