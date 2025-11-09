# Analisi Spaziale delle Performance delle Imprese Italiane
# Tesi di Laurea Magistrale - Pietro Maietta

Questo repository contiene il codice R per un'analisi econometrica e spaziale delle performance delle imprese italiane, utilizzando l'indice ISP (Indice di Sostenibilità Potenziale) come variabile dipendente.

La pipeline è suddivisa in fasi logiche, progettate per essere eseguite in sequenza.

---

## Architettura della Pipeline

### Fase 1: Unione e Pulizia
**Script:** `unione, pulizia e formattazione.R`
* **Scopo:** Caricare i dati grezzi da AIDA e altre fonti, pulire i valori mancanti, correggere i formati e unire i dataset.
* **Output:** Un dataset pulito (`dataset_pulito.rds`).

### Fase 2: Creazione ISP (Base)
**Script:** `divisione e creazione ISP.R`
* **Scopo:** Applicare la metodologia iniziale per calcolare una prima versione dell'ISP.
* **Output:** Dataset con ISP base (`matrice.rds`).

### Fase 3: Validazione e Finalizzazione ISP
**Script:** `ISP validazione e inferenza.R`
* **Scopo:** Testare diverse metodologie di pesatura (PCA, LM, LASSO) per i sotto-componenti dell'ISP. Analizza la robustezza dell'indice per settore ATECO.
* **Output:** Seleziona l'ISP finale (es. `ISP_sett_lasso_norm`), lo normalizza (`ISP_bn`) e sovrascrive il file `matrice.rds` finale.

### Fase 4: Modelli Econometrici (Selezione Variabili)
**Script:** `modelli econometrici.R`
* **Scopo:** Eseguire modelli OLS non spaziali con selezione `stepwise` per identificare il set ottimale di variabili (X) da utilizzare nell'analisi spaziale.
* **Output:** Elenchi di variabili (es. `vars_model_stepwise.rds`).

### Fase 5: Analisi Spaziale (Pipeline Principale)
**Script:** `analisi spaziale.R`
* **Scopo:** Il cuore dell'analisi. Utilizza le funzioni di `utils_spatial.R` per eseguire l'intera pipeline spaziale.
* **Sezioni Chiave:**
    * **Sez. 3-4:** Analisi Nazionale (il "telescopio").
    * **Sez. 6-7:** Analisi Locale per subset (il "microscopio"). Esegue il confronto **Strict vs. Border** per misurare l'effetto confine.
    * **Sez. 8:** Pipeline di regressione (OLS $\rightarrow$ SAR/SDM $\rightarrow$ GWR). Esegue GWR solo se giustificato (`gwr_eligible`) dai residui dei modelli globali.
    * **Sez. 9:** Creazione delle **Mappe Aggregate Finali** (es. `MAPPA_FINALE_LISA_AGG_STRICT.pdf`).
    * **Sez. 10:** Test di robustezza (es. correzione FDR e analisi dei residui GWR).

---

## Output Principali

Gli output finali per la tesi si trovano in `05_analysis_spatial/`:

### 1. Riepiloghi Tabellari (`03_tables/`)
* `riepilogo_cluster_LISA_completo.csv`: Conteggi LISA (HH, LL) per ogni subset (Strict vs. Border).
* `riepilogo_cluster_GI_completo.csv`: Conteggi Gi\* (Hotspot) per ogni subset (Strict vs. Border).
* `riepilogo_modello_dettaglio.csv`: Confronto AIC di OLS, SAR, SDM, GMM per ogni subset.
* `riepilogo_regressivo_gwr.csv`: Tabella decisionale GWR (indica se GWR è stato eseguito).
* `riepilogo_accordo_cluster_strict_vs_border.csv`: Test di robustezza che quantifica la differenza tra Strict e Border.
* `riepilogo_cluster_LISA_FDR_corretto.csv`: Test di robustezza sui p-value dei cluster LISA.

### 2. Mappe Finali (`02_maps/`)
* `MAPPA_FINALE_LISA_AGG_STRICT.pdf`: Mappa nazionale aggregata dei cluster LISA (Strict).
* `MAPPA_FINALE_LISA_AGG_BORDER.pdf`: Mappa nazionale aggregata dei cluster LISA (Border).
* `MAPPA_FINALE_GI_AGG_STRICT.pdf`: Mappa nazionale aggregata degli hotspot Gi\* (Strict).
* `MAPPA_FINALE_GI_AGG_BORDER.pdf`: Mappa nazionale aggregata degli hotspot Gi\* (Border).

### 3. Dati RDS (`01_rds/`)
Ogni sottocartella (es. `Media_Centro/`) contiene i file di dati intermedi:
* `matrici_W.rds` (Contiene `W_strict` e `W_border`)
* `lisa_results.rds`
* `regressioni_sar_sdm_gmm.rds`
* `gwr_out.rds` (Solo se GWR è stato eseguito per quel subset)

---

## Validazione

Per controllare l'integrità degli output della pipeline (Sezioni 1-10), eseguire lo script:
`check_output_full.R`