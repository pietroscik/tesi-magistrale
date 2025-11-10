import streamlit as st
from pathlib import Path
from gui.utils import MAPS_DIR, get_available_pdfs, page_header, inject_css

st.set_page_config(page_title="06 – Mappe", page_icon="🗺️", layout="wide")
inject_css()
page_header("06 – Mappe", "v1.0")

st.markdown("Preview e download delle mappe PDF (LISA, Gi*, grafi, mappe finali).")

pdfs = get_available_pdfs()
if not pdfs:
    st.warning("Nessuna mappa PDF trovata.")
    if MAPS_DIR.exists(): st.info(f"Directory verificata: `{MAPS_DIR}`")
else:
    st.markdown("---")
    st.subheader("📁 Seleziona una mappa")
    cats = {
        "🗺️ Mappe Finali Aggregate": [p for p in pdfs if "MAPPA_FINALE" in p],
        "📍 Analisi Nazionale": [p for p in pdfs if any(x in p for x in ["nazionale","grafo","imprese"])],
        "📊 Altri": [p for p in pdfs if "MAPPA_FINALE" not in p and not any(x in p for x in ["nazionale","grafo","imprese"])]
    }
    for cat, items in cats.items():
        if not items: continue
        st.markdown(f"### {cat}")
        sel = st.selectbox(f"Seleziona mappa da {cat}", items, key=f"pdf_{cat}")
        if sel:
            pdf_path = MAPS_DIR / sel
            if pdf_path.exists():
                size_kb = pdf_path.stat().st_size/1024
                st.info(f"📄 **File:** `{sel}` | **Dimensione:** {size_kb:.1f} KB")

                # Anteprima prima pagina se PyMuPDF disponibile
                try:
                    import fitz
                    doc = fitz.open(pdf_path)
                    if len(doc):
                        page = doc[0]
                        pix = page.get_pixmap(dpi=150)
                        st.image(pix.tobytes("png"), caption=f"Anteprima: {Path(sel).name}", use_container_width=True)
                    doc.close()
                except Exception:
                    st.caption("Anteprima non disponibile (PyMuPDF mancante o PDF non renderizzabile).")

                with open(pdf_path, "rb") as fh:
                    st.download_button("⬇️ Scarica PDF", data=fh, file_name=Path(sel).name, mime="application/pdf")
            st.markdown("---")

    with st.expander("📋 Elenco completo"):
        for i, p in enumerate(pdfs, 1):
            st.write(f"{i}. {p}")
