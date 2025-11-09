"""
Dashboard Interattiva per Analisi Spaziale delle Performance delle Imprese Italiane
Tesi di Laurea Magistrale - Pietro Maietta
"""

import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from pathlib import Path
import os

# Configurazione della pagina
st.set_page_config(
    page_title="Analisi Spaziale ISP",
    page_icon="📊",
    layout="wide",
    initial_sidebar_state="expanded"
)

# Percorsi dei dati
DATA_DIR = Path("05_analysis_spatial/03_tables")
MAPS_DIR = Path("05_analysis_spatial/02_maps")

# Funzioni di utilità per caricare i dati
@st.cache_data
def load_csv(filename):
    """Carica un file CSV dalla directory dei dati"""
    filepath = DATA_DIR / filename
    if filepath.exists():
        return pd.read_csv(filepath)
    return None

@st.cache_data
def get_available_pdfs():
    """Ottiene la lista dei PDF disponibili"""
    if MAPS_DIR.exists():
        return sorted([f.name for f in MAPS_DIR.glob("*.pdf")])
    return []

# Sidebar per la navigazione
st.sidebar.title("📊 Dashboard ISP")
st.sidebar.markdown("---")

page = st.sidebar.radio(
    "Navigazione",
    [
        "🏠 Overview",
        "📍 Analisi LISA",
        "🔥 Hotspot Gi*",
        "📈 Modelli Econometrici",
        "🌍 Indice di Moran",
        "🗺️ Mappe Spaziali"
    ]
)

st.sidebar.markdown("---")
st.sidebar.info(
    "**Tesi di Laurea Magistrale**\n\n"
    "Analisi Spaziale delle Performance delle Imprese Italiane\n\n"
    "**Autore:** Pietro Maietta"
)

# ============================================================================
# PAGINA: OVERVIEW
# ============================================================================
if page == "🏠 Overview":
    st.title("🏠 Overview dell'Analisi Spaziale")
    
    st.markdown("""
    ## Benvenuto nella Dashboard Interattiva
    
    Questa dashboard presenta i risultati dell'analisi spaziale delle performance delle imprese italiane, 
    utilizzando l'**Indice di Sostenibilità Potenziale (ISP)** come variabile dipendente.
    
    ### 📋 Struttura dell'Analisi
    
    L'analisi è organizzata in diverse sezioni:
    """)
    
    col1, col2 = st.columns(2)
    
    with col1:
        st.markdown("""
        #### 🔍 Analisi dei Cluster
        - **LISA (Local Indicators of Spatial Association)**: identifica cluster locali di valori simili
        - **Gi\* (Getis-Ord)**: identifica hotspot e coldspot statisticamente significativi
        
        #### 📊 Modelli Econometrici
        - **OLS**: Ordinary Least Squares (baseline)
        - **SAR**: Spatial Autoregressive Model
        - **SDM**: Spatial Durbin Model
        - **GMM**: Generalized Method of Moments
        """)
    
    with col2:
        st.markdown("""
        #### 🌐 Confronto Strict vs Border
        - **Strict**: matrice di prossimità senza considerare i confini regionali
        - **Border**: matrice di prossimità che considera i confini regionali
        
        #### 📏 Segmentazione
        - Per **Dimensione**: Micro, Piccola, Media, Grande
        - Per **Area Geografica**: Nord-Ovest, Nord-Est, Centro, Sud, Isole
        """)
    
    st.markdown("---")
    
    # Statistiche generali
    st.subheader("📊 Statistiche Generali")
    
    # Carica alcuni dati per le statistiche
    lisa_complete = load_csv("riepilogo_cluster_LISA_completo.csv")
    gi_complete = load_csv("riepilogo_cluster_GI_completo.csv")
    model_detail = load_csv("riepilogo_modello_dettaglio.csv")
    
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        if lisa_complete is not None:
            n_subsets = len(lisa_complete['Subset'].unique())
            st.metric("Subset Analizzati", n_subsets)
        else:
            st.metric("Subset Analizzati", "N/A")
    
    with col2:
        if model_detail is not None:
            n_models = len(model_detail)
            st.metric("Modelli Stimati", n_models)
        else:
            st.metric("Modelli Stimati", "N/A")
    
    with col3:
        available_pdfs = get_available_pdfs()
        st.metric("Mappe Disponibili", len(available_pdfs))
    
    with col4:
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

# ============================================================================
# PAGINA: ANALISI LISA
# ============================================================================
elif page == "📍 Analisi LISA":
    st.title("📍 Analisi dei Cluster LISA")
    
    st.markdown("""
    L'analisi **LISA (Local Indicators of Spatial Association)** identifica cluster spaziali locali:
    - **High-High (HH)**: cluster di valori alti circondati da valori alti
    - **Low-Low (LL)**: cluster di valori bassi circondati da valori bassi
    - **High-Low (HL)**: valori alti circondati da valori bassi (outlier)
    - **Low-High (LH)**: valori bassi circondati da valori alti (outlier)
    """)
    
    # Carica i dati LISA completi
    lisa_complete = load_csv("riepilogo_cluster_LISA_completo.csv")
    
    if lisa_complete is not None:
        st.markdown("---")
        
        # Filtri
        st.subheader("🔍 Filtri")
        col1, col2 = st.columns(2)
        
        with col1:
            selected_variant = st.selectbox(
                "Seleziona Variante",
                ["Tutti", "W_strict", "W_border"]
            )
        
        # Filtra i dati
        df_filtered = lisa_complete.copy()
        if selected_variant != "Tutti":
            df_filtered = df_filtered[df_filtered['variante'] == selected_variant]
        
        # Tabella completa
        st.markdown("---")
        st.subheader("📋 Tabella Completa dei Cluster LISA")
        st.dataframe(df_filtered, use_container_width=True)
        
        # Visualizzazioni
        st.markdown("---")
        st.subheader("📊 Visualizzazioni")
        
        # Prepara i dati per il grafico
        cluster_cols = ['High-High', 'Low-Low', 'High-Low', 'Low-High']
        
        # Grafico a barre per subset
        fig_bars = go.Figure()
        
        for col in cluster_cols:
            if col in df_filtered.columns:
                fig_bars.add_trace(go.Bar(
                    name=col,
                    x=df_filtered['Subset'],
                    y=df_filtered[col],
                    text=df_filtered[col],
                    textposition='auto'
                ))
        
        fig_bars.update_layout(
            title="Distribuzione dei Cluster LISA per Subset",
            xaxis_title="Subset",
            yaxis_title="Numero di Osservazioni",
            barmode='group',
            height=500,
            hovermode='x unified'
        )
        
        st.plotly_chart(fig_bars, use_container_width=True)
        
        # Confronto Strict vs Border
        if selected_variant == "Tutti":
            st.markdown("---")
            st.subheader("🔄 Confronto Strict vs Border")
            
            # Pivot per confronto
            strict_data = lisa_complete[lisa_complete['variante'] == 'W_strict'].copy()
            border_data = lisa_complete[lisa_complete['variante'] == 'W_border'].copy()
            
            if not strict_data.empty and not border_data.empty:
                comparison_data = []
                for _, row_s in strict_data.iterrows():
                    subset = row_s['Subset']
                    row_b = border_data[border_data['Subset'] == subset]
                    if not row_b.empty:
                        for col in cluster_cols:
                            if col in strict_data.columns and col in border_data.columns:
                                val_s = row_s[col] if pd.notna(row_s[col]) else 0
                                val_b = row_b[col].values[0] if pd.notna(row_b[col].values[0]) else 0
                                comparison_data.append({
                                    'Subset': subset,
                                    'Tipo Cluster': col,
                                    'Strict': val_s,
                                    'Border': val_b,
                                    'Differenza': val_b - val_s
                                })
                
                if comparison_data:
                    df_comparison = pd.DataFrame(comparison_data)
                    
                    # Selettore per tipo di cluster
                    selected_cluster = st.selectbox(
                        "Seleziona Tipo di Cluster",
                        cluster_cols
                    )
                    
                    df_comp_filtered = df_comparison[df_comparison['Tipo Cluster'] == selected_cluster]
                    
                    fig_comparison = go.Figure()
                    fig_comparison.add_trace(go.Bar(
                        name='Strict',
                        x=df_comp_filtered['Subset'],
                        y=df_comp_filtered['Strict'],
                        marker_color='lightblue'
                    ))
                    fig_comparison.add_trace(go.Bar(
                        name='Border',
                        x=df_comp_filtered['Subset'],
                        y=df_comp_filtered['Border'],
                        marker_color='lightcoral'
                    ))
                    
                    fig_comparison.update_layout(
                        title=f"Confronto Strict vs Border - {selected_cluster}",
                        xaxis_title="Subset",
                        yaxis_title="Numero di Osservazioni",
                        barmode='group',
                        height=500
                    )
                    
                    st.plotly_chart(fig_comparison, use_container_width=True)
    
    else:
        st.error("Impossibile caricare i dati LISA. Verificare che il file 'riepilogo_cluster_LISA_completo.csv' esista.")

# ============================================================================
# PAGINA: HOTSPOT GI*
# ============================================================================
elif page == "🔥 Hotspot Gi*":
    st.title("🔥 Analisi Hotspot Gi*")
    
    st.markdown("""
    L'analisi **Gi\* (Getis-Ord)** identifica hotspot e coldspot statisticamente significativi:
    - **Hotspot**: aree con valori significativamente alti
    - **Coldspot**: aree con valori significativamente bassi
    """)
    
    # Carica i dati Gi*
    gi_complete = load_csv("riepilogo_cluster_GI_completo.csv")
    
    if gi_complete is not None:
        st.markdown("---")
        
        # Filtri
        st.subheader("🔍 Filtri")
        selected_variant = st.selectbox(
            "Seleziona Variante",
            ["Tutti", "W_strict", "W_border"],
            key="gi_variant"
        )
        
        # Filtra i dati
        df_filtered = gi_complete.copy()
        if selected_variant != "Tutti":
            df_filtered = df_filtered[df_filtered['variante'] == selected_variant]
        
        # Tabella completa
        st.markdown("---")
        st.subheader("📋 Tabella Completa degli Hotspot Gi*")
        st.dataframe(df_filtered, use_container_width=True)
        
        # Visualizzazioni
        st.markdown("---")
        st.subheader("📊 Visualizzazioni")
        
        # Identifica le colonne numeriche (escludendo Subset e variante)
        numeric_cols = [col for col in df_filtered.columns if col not in ['Subset', 'variante']]
        
        if numeric_cols:
            # Grafico a barre
            fig_gi = go.Figure()
            
            for col in numeric_cols:
                if col in df_filtered.columns:
                    fig_gi.add_trace(go.Bar(
                        name=col,
                        x=df_filtered['Subset'],
                        y=df_filtered[col],
                        text=df_filtered[col],
                        textposition='auto'
                    ))
            
            fig_gi.update_layout(
                title="Distribuzione degli Hotspot Gi* per Subset",
                xaxis_title="Subset",
                yaxis_title="Numero di Osservazioni",
                barmode='group',
                height=500,
                hovermode='x unified'
            )
            
            st.plotly_chart(fig_gi, use_container_width=True)
    
    else:
        st.error("Impossibile caricare i dati Gi*. Verificare che il file 'riepilogo_cluster_GI_completo.csv' esista.")

# ============================================================================
# PAGINA: MODELLI ECONOMETRICI
# ============================================================================
elif page == "📈 Modelli Econometrici":
    st.title("📈 Confronto Modelli Econometrici")
    
    st.markdown("""
    Confronto tra diversi modelli di regressione spaziale:
    - **OLS**: Ordinary Least Squares (modello baseline senza componenti spaziali)
    - **SAR**: Spatial Autoregressive Model (cattura la dipendenza spaziale nella variabile dipendente)
    - **SDM**: Spatial Durbin Model (include ritardi spaziali sia di Y che di X)
    - **GMM**: Generalized Method of Moments (stima robusta per modelli spaziali)
    """)
    
    # Carica i dati dei modelli
    model_detail = load_csv("riepilogo_modello_dettaglio.csv")
    gwr_summary = load_csv("riepilogo_regressivo_gwr.csv")
    
    if model_detail is not None:
        st.markdown("---")
        
        # Filtri
        st.subheader("🔍 Filtri")
        col1, col2, col3 = st.columns(3)
        
        with col1:
            areas = ["Tutte"] + sorted(model_detail['Area'].unique().tolist())
            selected_area = st.selectbox("Area Geografica", areas)
        
        with col2:
            sizes = ["Tutte"] + sorted(model_detail['Dimensione'].unique().tolist())
            selected_size = st.selectbox("Dimensione", sizes)
        
        with col3:
            models = ["Tutti"] + sorted(model_detail['model'].unique().tolist())
            selected_model = st.selectbox("Modello", models)
        
        # Filtra i dati
        df_filtered = model_detail.copy()
        if selected_area != "Tutte":
            df_filtered = df_filtered[df_filtered['Area'] == selected_area]
        if selected_size != "Tutte":
            df_filtered = df_filtered[df_filtered['Dimensione'] == selected_size]
        if selected_model != "Tutti":
            df_filtered = df_filtered[df_filtered['model'] == selected_model]
        
        # Tabella completa
        st.markdown("---")
        st.subheader("📋 Dettaglio dei Modelli")
        st.dataframe(df_filtered, use_container_width=True)
        
        # Metriche chiave
        st.markdown("---")
        st.subheader("📊 Confronto AIC per Modello")
        
        # Raggruppa per modello e calcola statistiche AIC
        if 'AIC' in df_filtered.columns:
            # Rimuovi valori NA per il calcolo
            df_aic = df_filtered[df_filtered['AIC'].notna()].copy()
            
            if not df_aic.empty:
                # Box plot per AIC
                fig_aic = px.box(
                    df_aic,
                    x='model',
                    y='AIC',
                    color='model',
                    title='Distribuzione AIC per Modello',
                    labels={'model': 'Modello', 'AIC': 'AIC'}
                )
                fig_aic.update_layout(height=500, showlegend=False)
                st.plotly_chart(fig_aic, use_container_width=True)
                
                # Tabella riassuntiva AIC
                st.markdown("### 📊 Statistiche AIC")
                aic_stats = df_aic.groupby('model')['AIC'].agg(['count', 'mean', 'std', 'min', 'max']).round(2)
                aic_stats.columns = ['N. Modelli', 'Media', 'Dev. Std.', 'Min', 'Max']
                st.dataframe(aic_stats, use_container_width=True)
        
        # Modelli migliori per AIC
        st.markdown("---")
        st.subheader("🏆 Modelli Migliori per Subset")
        
        if 'best_by_AIC' in df_filtered.columns:
            best_models = df_filtered[df_filtered['best_by_AIC'] == True]
            if not best_models.empty:
                st.dataframe(
                    best_models[['Subset', 'Area', 'Dimensione', 'model', 'AIC']],
                    use_container_width=True
                )
            else:
                st.info("Nessun modello marcato come migliore nei dati filtrati.")
        
        # Informazioni GWR
        if gwr_summary is not None:
            st.markdown("---")
            st.subheader("🗺️ Analisi GWR (Geographically Weighted Regression)")
            st.dataframe(gwr_summary, use_container_width=True)
    
    else:
        st.error("Impossibile caricare i dati dei modelli. Verificare che il file 'riepilogo_modello_dettaglio.csv' esista.")

# ============================================================================
# PAGINA: INDICE DI MORAN
# ============================================================================
elif page == "🌍 Indice di Moran":
    st.title("🌍 Analisi dell'Indice di Moran")
    
    st.markdown("""
    L'**Indice di Moran (I)** misura l'autocorrelazione spaziale globale:
    - Valori positivi indicano clustering positivo (valori simili sono vicini)
    - Valori negativi indicano clustering negativo (valori dissimili sono vicini)
    - Valori vicini a zero indicano assenza di autocorrelazione spaziale
    
    Il **p-value** indica la significatività statistica dell'autocorrelazione.
    """)
    
    # Carica i dati di Moran
    moran_summary = load_csv("moran_summary_all.csv")
    
    if moran_summary is not None:
        st.markdown("---")
        
        # Filtri
        st.subheader("🔍 Filtri")
        col1, col2 = st.columns(2)
        
        with col1:
            if 'Area' in moran_summary.columns:
                areas = ["Tutte"] + sorted(moran_summary['Area'].unique().tolist())
                selected_area = st.selectbox("Area Geografica", areas, key="moran_area")
            else:
                selected_area = "Tutte"
        
        with col2:
            if 'Dimensione' in moran_summary.columns:
                sizes = ["Tutte"] + sorted(moran_summary['Dimensione'].unique().tolist())
                selected_size = st.selectbox("Dimensione", sizes, key="moran_size")
            else:
                selected_size = "Tutte"
        
        # Filtra i dati
        df_filtered = moran_summary.copy()
        if selected_area != "Tutte" and 'Area' in df_filtered.columns:
            df_filtered = df_filtered[df_filtered['Area'] == selected_area]
        if selected_size != "Tutte" and 'Dimensione' in df_filtered.columns:
            df_filtered = df_filtered[df_filtered['Dimensione'] == selected_size]
        
        # Tabella completa
        st.markdown("---")
        st.subheader("📋 Tabella Completa dell'Indice di Moran")
        st.dataframe(df_filtered, use_container_width=True)
        
        # Visualizzazioni
        st.markdown("---")
        st.subheader("📊 Visualizzazioni")
        
        # Scatter plot: Moran I Strict vs Border
        if 'Moran_I_strict' in df_filtered.columns and 'Moran_I_border' in df_filtered.columns:
            fig_scatter = px.scatter(
                df_filtered,
                x='Moran_I_strict',
                y='Moran_I_border',
                hover_data=['Subset', 'Area', 'Dimensione'] if 'Subset' in df_filtered.columns else None,
                title="Confronto Moran I: Strict vs Border",
                labels={'Moran_I_strict': 'Moran I (Strict)', 'Moran_I_border': 'Moran I (Border)'},
                color='Dimensione' if 'Dimensione' in df_filtered.columns else None
            )
            
            # Aggiungi linea di riferimento (y=x)
            min_val = min(df_filtered['Moran_I_strict'].min(), df_filtered['Moran_I_border'].min())
            max_val = max(df_filtered['Moran_I_strict'].max(), df_filtered['Moran_I_border'].max())
            fig_scatter.add_trace(go.Scatter(
                x=[min_val, max_val],
                y=[min_val, max_val],
                mode='lines',
                name='y=x',
                line=dict(color='red', dash='dash')
            ))
            
            fig_scatter.update_layout(height=500)
            st.plotly_chart(fig_scatter, use_container_width=True)
        
        # Distribuzione p-value
        if 'p_strict' in df_filtered.columns and 'p_border' in df_filtered.columns:
            st.markdown("### 📉 Distribuzione dei P-value")
            
            col1, col2 = st.columns(2)
            
            with col1:
                # Histogram p-value strict
                fig_p_strict = px.histogram(
                    df_filtered,
                    x='p_strict',
                    nbins=30,
                    title='Distribuzione P-value (Strict)',
                    labels={'p_strict': 'P-value'}
                )
                fig_p_strict.add_vline(x=0.05, line_dash="dash", line_color="red", 
                                      annotation_text="α=0.05")
                st.plotly_chart(fig_p_strict, use_container_width=True)
            
            with col2:
                # Histogram p-value border
                fig_p_border = px.histogram(
                    df_filtered,
                    x='p_border',
                    nbins=30,
                    title='Distribuzione P-value (Border)',
                    labels={'p_border': 'P-value'}
                )
                fig_p_border.add_vline(x=0.05, line_dash="dash", line_color="red",
                                       annotation_text="α=0.05")
                st.plotly_chart(fig_p_border, use_container_width=True)
    
    else:
        st.error("Impossibile caricare i dati di Moran. Verificare che il file 'moran_summary_all.csv' esista.")

# ============================================================================
# PAGINA: MAPPE SPAZIALI
# ============================================================================
elif page == "🗺️ Mappe Spaziali":
    st.title("🗺️ Mappe Spaziali")
    
    st.markdown("""
    Visualizzazione delle mappe PDF generate dall'analisi spaziale.
    Queste mappe mostrano i risultati geografici dei cluster LISA e degli hotspot Gi*.
    """)
    
    # Ottieni lista PDF
    available_pdfs = get_available_pdfs()
    
    if available_pdfs:
        st.markdown("---")
        st.subheader("📁 Seleziona una Mappa")
        
        # Organizza i PDF per categoria
        pdf_categories = {
            "🗺️ Mappe Finali Aggregate": [p for p in available_pdfs if "MAPPA_FINALE" in p],
            "📍 Analisi Nazionale": [p for p in available_pdfs if any(x in p for x in ["nazionale", "grafo", "imprese"])],
            "📊 Altri": [p for p in available_pdfs if "MAPPA_FINALE" not in p and not any(x in p for x in ["nazionale", "grafo", "imprese"])]
        }
        
        for category, pdfs in pdf_categories.items():
            if pdfs:
                st.markdown(f"### {category}")
                selected_pdf = st.selectbox(
                    f"Seleziona mappa da {category}",
                    pdfs,
                    key=f"pdf_{category}"
                )
                
                if selected_pdf:
                    pdf_path = MAPS_DIR / selected_pdf
                    
                    # Informazioni sul file
                    if pdf_path.exists():
                        file_size = pdf_path.stat().st_size / 1024  # KB
                        st.info(f"📄 **File:** `{selected_pdf}` | **Dimensione:** {file_size:.1f} KB")
                        
                        # Pulsante download
                        with open(pdf_path, "rb") as file:
                            btn = st.download_button(
                                label="⬇️ Scarica PDF",
                                data=file,
                                file_name=selected_pdf,
                                mime="application/pdf"
                            )
                        
                        st.markdown("---")
                        st.info("""
                        💡 **Nota:** Per visualizzare il PDF, scaricalo utilizzando il pulsante sopra.
                        I browser moderni supportano la visualizzazione diretta dei PDF.
                        """)
                    else:
                        st.error(f"File non trovato: {pdf_path}")
                
                st.markdown("---")
        
        # Lista completa
        with st.expander("📋 Visualizza tutte le mappe disponibili"):
            for i, pdf in enumerate(available_pdfs, 1):
                st.write(f"{i}. {pdf}")
    
    else:
        st.warning("Nessuna mappa PDF trovata nella directory delle mappe.")
        if MAPS_DIR.exists():
            st.info(f"Directory verificata: `{MAPS_DIR}`")
        else:
            st.error(f"Directory non trovata: `{MAPS_DIR}`")

# Footer
st.markdown("---")
st.markdown(
    "<div style='text-align: center; color: gray; padding: 20px;'>"
    "Dashboard creata con Streamlit | Tesi di Laurea Magistrale - Pietro Maietta"
    "</div>",
    unsafe_allow_html=True
)
