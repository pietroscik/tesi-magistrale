# Guida all'uso della Dashboard Streamlit

## 📋 Panoramica

Questa dashboard interattiva permette di esplorare i risultati dell'analisi spaziale delle performance delle imprese italiane attraverso diverse visualizzazioni e filtri dinamici.

## 🚀 Avvio Rapido

### 1. Installazione delle dipendenze

```bash
pip install -r requirements.txt
```

### 2. Avvio della dashboard

```bash
streamlit run app.py
```

La dashboard sarà disponibile nel browser all'indirizzo: **http://localhost:8501**

### 3. Test della dashboard

Per verificare che tutto funzioni correttamente:

```bash
python3 test_dashboard.py
```

## 📊 Funzionalità delle Pagine

### 🏠 Overview
- Statistiche generali dell'analisi
- Numero di subset analizzati, modelli stimati, mappe disponibili
- Lista completa dei dataset CSV disponibili

### 📍 Analisi LISA
- Visualizzazione dei cluster LISA (High-High, Low-Low, High-Low, Low-High)
- Filtri per variante (Strict vs Border)
- Grafici interattivi a barre per subset
- Confronto diretto tra Strict e Border

### 🔥 Hotspot Gi*
- Analisi degli hotspot e coldspot statisticamente significativi
- Filtri per variante
- Visualizzazioni per subset

### 📈 Modelli Econometrici
- Confronto tra modelli OLS, SAR, SDM e GMM
- Filtri per area geografica, dimensione e tipo di modello
- Box plot della distribuzione AIC
- Tabella dei modelli migliori per subset
- Informazioni su GWR (Geographically Weighted Regression)

### 🌍 Indice di Moran
- Analisi dell'autocorrelazione spaziale globale
- Scatter plot confronto Strict vs Border
- Distribuzione dei p-value
- Filtri per area e dimensione

### 🗺️ Mappe Spaziali
- Visualizzazione e download delle mappe PDF
- Organizzazione per categorie (Mappe Finali, Analisi Nazionale, Altri)
- Download diretto dei file PDF

## 🎯 Suggerimenti per l'uso

1. **Navigazione**: Usa il menu laterale per spostarti tra le diverse sezioni
2. **Filtri**: Applica i filtri per analizzare sottoinsiemi specifici dei dati
3. **Visualizzazioni**: Passa il mouse sui grafici per dettagli interattivi
4. **Download**: Usa i pulsanti di download per salvare le mappe PDF
5. **Tabelle**: Le tabelle sono interattive e permettono ricerca e ordinamento

## 📁 Struttura dei Dati

La dashboard legge i dati da:
- **CSV**: `05_analysis_spatial/03_tables/`
- **Mappe PDF**: `05_analysis_spatial/02_maps/`

## 🔧 Personalizzazione

Il file `app.py` può essere modificato per:
- Aggiungere nuove visualizzazioni
- Modificare i filtri disponibili
- Personalizzare l'aspetto grafico
- Aggiungere nuove pagine di analisi

## ⚙️ Requisiti di Sistema

- Python 3.8 o superiore
- Browser web moderno (Chrome, Firefox, Safari, Edge)
- Almeno 512MB di RAM disponibile

## 📞 Supporto

Per problemi o domande relative alla dashboard, consultare:
- La documentazione di Streamlit: https://docs.streamlit.io/
- Il README principale del progetto
