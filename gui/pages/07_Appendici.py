import streamlit as st, platform, sys, importlib.metadata as im
from gui.utils import page_header, inject_css

st.set_page_config(page_title="07 – Appendici", page_icon="🧩", layout="wide")
inject_css()
page_header("07 – Appendici (Reproducibilità)", "v1.0")

st.markdown("""
## Reproducibilità
- Snapshot dati: cartella `05_analysis_spatial/03_tables` (CSV) e `02_maps` (PDF).
- Versioni software: riportate di seguito.
- Suggerito: indicare data di accesso e commit hash del repository nella tesi.

## Come citare la dashboard
Maietta, P. (2025). *Dashboard interattiva per l’analisi spaziale delle performance delle imprese italiane*. URL: pietromaietta.streamlit.app (accesso: gg mese 2025).
""")

st.markdown("---")
st.subheader("Versioni pacchetti principali")
def ver(pkg):
    try: return im.version(pkg)
    except Exception: return "n/d"

rows = [
    ("Python", sys.version.split()[0]),
    ("Streamlit", ver("streamlit")),
    ("Pandas", ver("pandas")),
    ("NumPy", ver("numpy")),
    ("PyArrow", ver("pyarrow")),
    ("Plotly", ver("plotly")),
    ("Matplotlib", ver("matplotlib")),
    ("PyMuPDF", ver("PyMuPDF")),
]
st.table(rows)

st.markdown("---")
st.subheader("Ambiente")
st.json({
    "platform": platform.platform(),
    "python_implementation": platform.python_implementation(),
})
