"""
Home – Dashboard Interattiva per Analisi Spaziale (ISP)
Tesi di Laurea Magistrale - Pietro Maietta
"""
# --- PATH GUARD: assicura che la root del repo sia in sys.path ---
import sys, pathlib, os
APP_DIR = pathlib.Path(__file__).resolve().parent           # .../gui
ROOT_DIR = APP_DIR.parent                                   # repo root
if str(ROOT_DIR) not in sys.path:
    sys.path.insert(0, str(ROOT_DIR))
# ---------------------------------------------------------------
# bootstrap leggero per sicurezza su Cloud
import sys, subprocess, importlib
def _ensure(spec, import_name=None):
    name = import_name or spec.split("==")[0].split(">=")[0].split("<")[0]
    try:
        importlib.import_module(name)
    except ImportError:
        subprocess.check_call([sys.executable, "-m", "pip", "install", spec])
        importlib.invalidate_caches()
_ensure("plotly>=5.24,<6", "plotly")

import streamlit as st
import pandas as pd
from pathlib import Path
from gui.utils import DATA_DIR, MAPS_DIR, load_csv, get_available_pdfs, coerce_categories, page_header, inject_css

st.set_page_config(page_title="Analisi Spaziale ISP – Overview", page_icon="📊", layout="wide", initial_sidebar_state="expanded")
inject_css()
page_header("Analisi Spaziale ISP – Overview", "v1.0")

st.sidebar.info(
    "**Tesi di Laurea Magistrale**\n\n"
    "Analisi Spaziale delle Performance delle Imprese Italiane\n\n"
    "**Autore:** Pietro Maietta\n\n"
    "---\n\n"
    "🌐 **Dashboard Online:**\n"
    "[pietromaietta.streamlit.app](https://pietromaietta.streamlit.app/)"
)

# Metriche generali
with st.spinner("Caricamento metriche..."):
    lisa_complete = coerce_categories(load_csv("riepilogo_cluster_LISA_completo.csv"))
    model_detail  = coerce_categories(load_csv("riepilogo_modello_dettaglio.csv"))

n_subsets = int(lisa_complete["Subset"].nunique()) if isinstance(lisa_complete, pd.DataFrame) and "Subset" in lisa_complete.columns else 0
n_models  = len(model_detail) if isinstance(model_detail, pd.DataFrame) else 0
n_maps    = len(get_available_pdfs())
n_csv     = len(list(DATA_DIR.glob("*.csv"))) if DATA_DIR.exists() else 0

c1, c2, c3, c4 = st.columns(4)
c1.metric("Subset analizzati", n_subsets)
c2.metric("Modelli stimati", n_models)
c3.metric("Mappe disponibili", n_maps)
c4.metric("File CSV", n_csv)

st.markdown("---")
colL, colR = st.columns(2)
with colL:
    st.markdown(r"""
### Obiettivi
- Valutare la **struttura e la consistenza** finanziaria delle imprese
- Evidenziare **pattern spaziali** (LISA, Gi\*) e **dipendenza globale** (Moran I)
- Confrontare modelli **OLS / SAR / SDM / GMM / GWR**

### Dati & Segmentazioni
- Dimensione: Micro, Piccola, Media, Grande  
- Area: Nord-Ovest, Nord-Est, Centro, Sud, Isole  
- Matrici: **W_strict** vs **W_border**
""")
with colR:
    st.markdown("""
### Navigazione (pagine a sinistra)
1. Dati & Metodi  
2. Risultati LISA  
3. Risultati Gi*  
4. Indice di Moran  
5. Modelli Econometrici  
6. Mappe  
7. Appendici (reproducibilità)
""")

st.markdown("---")
st.subheader("📁 Dataset disponibili (cartella 05_analysis_spatial/03_tables)")
if DATA_DIR.exists():
    csv_files = sorted([f.name for f in DATA_DIR.glob("*.csv")])
    st.dataframe(pd.DataFrame({"file": csv_files}), use_container_width=True, hide_index=True) if csv_files else st.warning("Nessun CSV trovato.")
else:
    st.error(f"Directory non trovata: {DATA_DIR}")

st.markdown("---")
st.caption("Dashboard creata con Streamlit • Università Parthenope – Tesi Pietro Maietta")
