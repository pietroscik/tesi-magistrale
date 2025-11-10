from __future__ import annotations
import streamlit as st
import pandas as pd
from pathlib import Path

ROOT = Path(".")
DATA_DIR = ROOT / "05_analysis_spatial" / "03_tables"
MAPS_DIR = ROOT / "05_analysis_spatial" / "02_maps"

@st.cache_data(ttl=600)
def load_csv(filename: str) -> pd.DataFrame | None:
    fp = DATA_DIR / filename
    if not fp.exists(): return None
    try:
        return pd.read_csv(fp, engine="pyarrow")
    except Exception:
        return pd.read_csv(fp)

@st.cache_data(ttl=600)
def get_available_pdfs() -> list[str]:
    if MAPS_DIR.exists():
        return sorted([f.name for f in MAPS_DIR.glob("*.pdf")])
    return []

def coerce_categories(df: pd.DataFrame | None) -> pd.DataFrame | None:
    if df is None: return None
    for c in ["Subset","Area","Dimensione","model","variante"]:
        if c in df.columns:
            df[c] = df[c].astype("category")
    return df

def text_filter(df: pd.DataFrame, placeholder="Cerca...") -> pd.DataFrame:
    q = st.text_input(placeholder).strip().lower()
    if not q: return df
    mask = pd.Series(False, index=df.index)
    for c in df.columns:
        mask |= df[c].astype(str).str.lower().str.contains(q, na=False)
    return df[mask]

def download_df_button(df: pd.DataFrame, filename: str):
    st.download_button(
        "📥 Scarica CSV (filtrato)",
        df.to_csv(index=False).encode("utf-8"),
        file_name=filename,
        mime="text/csv",
    )

def page_header(title: str, version: str = "v1.0"):
    st.title(title)
    st.caption(f"Tesi – build main • {version}")

def inject_css():
    st.markdown("""
    <style>
    .block-container{padding-top:1rem;padding-bottom:1.5rem;max-width:1180px}
    [data-testid="stSidebar"] .block-container{padding-top:1rem}
    h1,h2{margin-bottom:.4rem}
    [data-testid="stMetricValue"]{font-size:1.4rem}
    [data-testid="stDataFrame"] div[data-testid="stVerticalBlock"]{gap:.25rem}
    </style>
    """, unsafe_allow_html=True)
