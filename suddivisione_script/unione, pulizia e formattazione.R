# =================== FASE 1: UNIONE, PULIZIA E FORMATTAZIONE ===================
fase_nome <- "01_cleaning"
cat(paste0("\n========== Avvio FASE: ", fase_nome, " ==========\n"))

# Carica le librerie
# Vettore di pacchetti richiesti
packages <- c(
  "leaflet.extras", "leaflet", "ggplot2", "psych", "openxlsx", "dplyr", 
  "readxl", "plotly", "pheatmap", "tidyr", "purrr", "MASS", "reshape2", 
  "DT", "nortest", "bestNormalize", "e1071", "moments", "rstatix", 
  "car", "lmtest", "corrplot", "tidyverse","irr","glmnet","kableExtra",
  "sandwich","caret","sf","here"
)

# Funzione per installare e caricare i pacchetti
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

# Applica la funzione a tutti i pacchetti
invisible(lapply(packages, install_and_load))

# Cartella output dedicata
output_dir <- here(fase_nome)
if (!dir.exists(output_dir)) dir.create(output_dir)

##### Script Unione ####
# Lista dei percorsi dei file
file_paths <- c(
  here("data_raw", "aida tesi nord-est3.xlsx"),
  here("data_raw", "aida tesi nord-est2.xlsx"),
  here("data_raw", "aida tesi nord-est1.xlsx"),
  here("data_raw", "aida tesi isole.xlsx"),
  here("data_raw", "aida tesi sud2.xlsx"),
  here("data_raw", "aida tesi monza.xlsx"),
  here("data_raw", "aida tesi sud.xlsx"),
  here("data_raw", "aida tesi milano2.xlsx"),
  here("data_raw", "aida tesi milano1.xlsx"),
  here("data_raw", "aziende tesi piemonte2.xlsx"),
  here("data_raw", "aziende tesi piemonte1.xlsx"),
  here("data_raw", "aziende tesi piemonte.xlsx"),
  here("data_raw", "aida centro1.xlsx"),
  here("data_raw", "aida tesi centro.xlsx")
)

# Funzione per leggere la seconda scheda di ciascun file
read_second_sheet <- function(file) {
  read_excel(file, sheet = 2)
}

# Leggi tutti i file dalla seconda scheda
datasets <- lapply(file_paths, read_second_sheet)

# Funzione aggiornata per standardizzare i tipi di dati
standardize_columns <- function(datasets) {
  datasets[] <- lapply(datasets, as.character)  # Converti ogni colonna in character
  return(datasets)
}

# Applica la funzione a tutti i dataset
datasets <- lapply(datasets, standardize_columns)

# Unisci i dataset
combined_data <- bind_rows(datasets)

# Verifica la struttura del dataset unito
cat("Dimensioni dataset unito:\n")
print(dim(combined_data))
cat("Sintesi dataset unito:\n")
print(summary(combined_data))
cat("Prime righe dataset unito:\n")
print(head(combined_data))
cat("Valori NA per colonna:\n")
print(colSums(is.na(combined_data)))

# Conversione dei valori n.d e n.s. in NA
combined_data[combined_data == "n.d." | combined_data == "n.s."] <- NA

# Rimuove righe con tutti i valori NA
combined_data <- combined_data[rowSums(is.na(combined_data)) != ncol(combined_data), ]
# Rimuovi tutte le righe che non presentano valore sulla colonna Ragione sociale
combined_data <- combined_data[!is.na(combined_data$`Ragione sociale`) & combined_data$`Ragione sociale`!= "", ]

# Verifica le dimensioni dopo la rimozione
cat("Dimensioni dataset dopo rimozione NA e ragione sociale mancante:\n")
print(dim(combined_data))

# Scrivi il dataset su file Excel
cat("Salvataggio dataset unito pulito in Excel...\n")
write.xlsx(combined_data, file.path(output_dir, "dataset.xlsx"))

##### Pulizia e formattazione ####
dataset <- combined_data[, -1]  # Rimuove la prima colonna
View(dataset)
str(dataset)
# Contare i valori mancanti per colonna
cat("Valori non NA per colonna:\n")
print(colSums(!is.na(dataset)))

# Visualizzare le prime righe del dataset
cat("Prime righe dataset:\n")
print(head(dataset))

# Dimensioni del dataset
cat("Dimensioni dataset dopo rimozione prima colonna:\n")
print(dim(dataset))

# Rimuovi le colonne con nomi NA
dataset <- dataset[, !is.na(names(dataset))]

# Ridefinizione variabili e suddivisione settore di interesse per l'analisi
cat("Classe dataset:\n")
print(class(dataset))
cat("Sintesi dataset:\n")
print(summary(dataset))

nomi_colonne<-colnames(dataset)
cat("Nomi colonne originali:\n")
print(nomi_colonne)

colnames(dataset) <- c(
  "ragione_sociale", 
  "numero_CCIAA", 
  "provincia", 
  "codice_fiscale", 
  "chiusura_bilancio_ultimo_anno", 
  "ricavi_vendite_migl_EUR", 
  "dipendenti", 
  "CAP", 
  "regione", 
  "ISTAT_regione", 
  "ISTAT_provincia", 
  "ISTAT_comune", 
  "longitudine", 
  "latitudine", 
  "stato_giuridico", 
  "anno_fondazione", 
  "forma_giuridica", 
  "ricavi_vendite_ultimo_anno_migl_EUR", 
  "utile_netto_ultimo_anno_migl_EUR", 
  "totale_attivita_ultimo_anno_migl_EUR", 
  "dipendenti_ultimo_anno", 
  "dipendenti_fonte_ultimo_anno", 
  "capitalizzazione_corrente_migl_EUR", 
  "data_capitalizzazione_corrente", 
  "fatturato_estimato_migl_EUR", 
  "capitale_sociale_migl_EUR", 
  "indicatore_indipendenza_BvD", 
  "num_societa_gruppo", 
  "num_azionisti", 
  "num_partecipate", 
  "ateco_2007_codice", 
  "ateco_2007_descrizione", 
  "nace_rev2_codice", 
  "nace_rev2_descrizione", 
  "nome_gruppo_pari", 
  "descrizione_gruppo_pari", 
  "dimensione_gruppo_pari", 
  "overview_completa", 
  "storico", 
  "linea_business_principale", 
  "linea_business_secondaria", 
  "attivita_principale", 
  "attivita_secondaria", 
  "prodotti_servizi_principali", 
  "stima_dimensione", 
  "strategia_organizzazione_policy", 
  "alleanze_strategiche", 
  "socio_network", 
  "marchi_principali", 
  "stato_principale", 
  "stati_reg_stranieri_principali", 
  "siti_produzione_principali", 
  "siti_distribuzione_principali", 
  "siti_rappresentanza_principali", 
  "clienti_principali", 
  "indicatore_info_concise", 
  "societa_artigiana", 
  "startup_innovativa", 
  "PMI_innovativa", 
  "operatore_estero", 
  "EBITDA_migl_EUR", 
  "utile_netto_migl_EUR", 
  "totale_attivita_migl_EUR", 
  "patrimonio_netto_migl_EUR", 
  "posizione_finanziaria_netta_migl_EUR", 
  "EBITDA_su_vendite_percentuale", 
  "ROS_percentuale", 
  "ROA_percentuale", 
  "ROE_percentuale", 
  "debt_equity_ratio_percentuale", 
  "debiti_banche_fatt_percentuale", 
  "debt_EBITDA_ratio_percentuale", 
  "rotazione_cap_investito_volte", 
  "indice_liquidita", 
  "indice_corrente", 
  "indice_indebitamento_breve_percentuale", 
  "indice_indebitamento_lungo_percentuale", 
  "indice_copertura_immob_patrimoniale_percentuale", 
  "grado_ammortamento_percentuale", 
  "rapporto_indebitamento", 
  "indice_copertura_immob_finanziario_percentuale", 
  "debiti_banche_fatt_percentuale", 
  "costo_denaro_prestito_percentuale", 
  "grado_copertura_interessi_percentuale", 
  "oneri_finanziari_fatt_percentuale", 
  "indice_indipendenza_finanziaria_percentuale", 
  "grado_indipendenza_terzi_percentuale", 
  "posizione_finanziaria_netta", 
  "debt_equity_ratio_percentuale", 
  "debt_EBITDA_ratio_percentuale", 
  "rotazione_cap_investito", 
  "rotazione_cap_circolante_lordo", 
  "incidenza_circolante_operativo_percentuale", 
  "giacenza_media_scorte_gg", 
  "giorni_copertura_scorte_gg", 
  "durata_media_crediti_lordo_IVA_gg", 
  "durata_media_debiti_lordo_IVA_gg", 
  "durata_ciclo_commerciale_gg", 
  "EBITDA", 
  "EBITDA_su_vendite_percentuale", 
  "ROA_percentuale", 
  "ROI_percentuale", 
  "ROS_percentuale", 
  "ROE_percentuale", 
  "incidenza_oneri_extrag_percentuale", 
  "dipendenti_1", 
  "ricavi_pro_capite_EUR", 
  "valore_aggiunto_pro_capite_EUR", 
  "costo_lavoro_addetto_EUR", 
  "rendimento_dipendenti", 
  "capitale_circolante_netto_migl_EUR", 
  "margine_consumi_migl_EUR", 
  "margine_tesoreria_migl_EUR", 
  "margine_struttura_migl_EUR", 
  "flusso_cassa_gestione_migl_EUR",
  "azioni_quote_titoli_migl_EUR",
  "crediti_verso enti_creditizi_migl_EUR",
  "totale_attivo_migl_EUR",
  "debiti_verso enti_creditizi_migl_EUR",
  "capitale_mgl_EUR",
  "commissioni_attive_migl_EUR",
  "commissionipassive_migl_EUR",
  "profitti_perdite_opfin_migl_EUR",
  "utile_perdita_esercizio_migl_EUR",
  "margine_interesse_migl_EUR",
  "margine_intermediazione_migl_EUR",
  "risultato_lordo_gestione_migl_EUR",
  "dipendenti_2",
  "dm_compenso_salario",
  "dm_compenso_totale",
  "dm_data_compenso_salario",
  "dm_data_compenso_totale"
)
cat("Nomi colonne ridefiniti:\n")
print(colnames(dataset))

# Identifica le colonne con nomi duplicati ed elimina quelle 
nomi_colonne <- names(dataset)
# Calcola la percentuale di valori mancanti per colonna
na_percentage <- sapply(dataset, function(x) mean(is.na(x)) * 100)

# Filtra le colonne con più del 30% di dati mancanti
colonne_da_rimuovere <- names(na_percentage)[na_percentage >= 30]
dataset_ridotto <- dataset[, !(names(dataset) %in% colonne_da_rimuovere)]

comparison_matrix <- matrix(FALSE, nrow = ncol(dataset_ridotto), ncol = ncol(dataset_ridotto))
colnames(comparison_matrix) <- colnames(dataset_ridotto)
rownames(comparison_matrix) <- colnames(dataset_ridotto)

# Confronta tutte le colonne, escludendo il confronto con se stessa
for (i in 1:ncol(dataset_ridotto)) {
  for (j in 1:ncol(dataset_ridotto)) {
    comparison_matrix[i, j] <- all(dataset_ridotto[[i]] == dataset_ridotto[[j]])
  }
}

# Visualizza le colonne uguali
equal_columns <- which(comparison_matrix == TRUE, arr.ind = TRUE)
if (length(equal_columns) > 0) {
  cat("Colonne uguali:\n")
  for (idx in 1:nrow(equal_columns)) {
    # Aggiungi il filtro per escludere i confronti tra la stessa colonna
    if (equal_columns[idx, 1] != equal_columns[idx, 2]) {
      cat(colnames(dataset_ridotto)[equal_columns[idx, 1]], "<->", colnames(dataset_ridotto)[equal_columns[idx, 2]], "\n")
    }
  }
} else {
  cat("Nessuna colonna uguale trovata.\n")
}
# Esclusione colonne uguali manualmente su indicazione equal_colums
colonne_da_escludere <- c("dipendenti_ultimo_anno","dipendenti_1",
                          "dipendenti_2","totale_attivita_ultimo_anno_migl_EUR",
                          "totale_attivo_migl_EUR",
                          "utile_perdita_esercizio_migl_EUR",
                          "ricavi_vendite_ultimo_anno_migl_EUR",
                          "utile_netto_ultimo_anno_migl_EUR")

# Escludi le colonne dal dataset_ridotto
dataset_ridotto <- dataset_ridotto[, !(colnames(dataset_ridotto) %in% colonne_da_escludere)]

# Visualizza il dataset ridotto con le colonne escludenti
cat("Dataset ridotto:\n")
print(dataset_ridotto)
cat("Nomi colonne dataset ridotto:\n")
print(colnames(dataset_ridotto))

# Calcola la percentuale di dati mancanti per ciascuna colonna
na_percentage <- sapply(dataset_ridotto, function(x) mean(is.na(x)) * 100)
na_percentage <- sort(na_percentage, decreasing = TRUE)
cat("Percentuale di valori mancanti per colonna:\n")
print(na_percentage)

# Rimuovi le righe che contengono almeno un NA
dataset_completo <- na.omit(dataset_ridotto)
# Rimuovi le colonne duplicate per nome specifico
cat("Colonne duplicate per nome:\n")
print(names(dataset_completo)[duplicated(names(dataset_completo))])
dataset_completo <- dataset_completo[, !duplicated(colnames(dataset_completo))]
cat("Nomi colonne dataset completo:\n")
print(colnames(dataset_completo))

# Cambio tipo variabili
cat("Cambio tipo variabili e rimozione NA...\n")
dataset_completo <- dataset_completo %>%
  mutate(
    ragione_sociale=as.character(ragione_sociale),
    numero_CCIAA=as.character(numero_CCIAA),
    provincia = factor(provincia),
    codice_fiscale=as.character(codice_fiscale),
    chiusura_bilancio_ultimo_anno=as.character(chiusura_bilancio_ultimo_anno),
    ricavi_vendite_migl_EUR=as.numeric(ricavi_vendite_migl_EUR),
    dipendenti=as.numeric(dipendenti),
    CAP = factor(CAP),
    regione = factor(regione),
    ISTAT_regione = factor(ISTAT_regione),
    ISTAT_provincia = factor(ISTAT_provincia),
    ISTAT_comune = factor(ISTAT_comune),
    longitudine=as.numeric(longitudine),
    latitudine=as.numeric(latitudine),
    stato_giuridico = factor(stato_giuridico),
    forma_giuridica = factor(forma_giuridica),
    ricavi_vendite_migl_EUR = as.numeric(ricavi_vendite_migl_EUR),
    utile_netto_migl_EUR = as.numeric(utile_netto_migl_EUR),
    dipendenti_fonte_ultimo_anno=as.character(dipendenti_fonte_ultimo_anno),
    capitale_sociale_migl_EUR=as.numeric(capitale_sociale_migl_EUR),
    indicatore_indipendenza_BvD = factor(indicatore_indipendenza_BvD),
    num_societa_gruppo = factor(num_societa_gruppo),
    num_azionisti = factor(num_azionisti),
    num_partecipate = factor(num_partecipate),
    ateco_2007_codice = factor(ateco_2007_codice),
    ateco_2007_descrizione = as.character(ateco_2007_descrizione),
    nace_rev2_codice = factor(nace_rev2_codice),
    nace_rev2_descrizione = as.character(nace_rev2_descrizione),
    nome_gruppo_pari   = factor(nome_gruppo_pari),
    descrizione_gruppo_pari=as.character(descrizione_gruppo_pari),
    dimensione_gruppo_pari = as.numeric(dimensione_gruppo_pari),
    overview_completa = as.character(overview_completa),
    linea_business_principale = as.character(linea_business_principale),
    attivita_principale=as.character(attivita_principale),
    prodotti_servizi_principali=as.character(prodotti_servizi_principali),
    stato_principale = factor(stato_principale),
    societa_artigiana = factor(societa_artigiana),
    startup_innovativa = factor(startup_innovativa),
    PMI_innovativa = factor(PMI_innovativa),
    operatore_estero=factor(operatore_estero),
    EBITDA_migl_EUR = as.numeric(EBITDA_migl_EUR),
    utile_netto_migl_EUR=as.numeric(utile_netto_migl_EUR),
    totale_attivita_migl_EUR=as.numeric(totale_attivita_migl_EUR),
    patrimonio_netto_migl_EUR=as.numeric(patrimonio_netto_migl_EUR),
    posizione_finanziaria_netta_migl_EUR=as.numeric(posizione_finanziaria_netta_migl_EUR),
    EBITDA_su_vendite_percentuale=as.numeric(EBITDA_su_vendite_percentuale),
    ROE_percentuale = as.numeric(ROE_percentuale),
    ROS_percentuale = as.numeric(ROS_percentuale),
    ROA_percentuale = as.numeric(ROA_percentuale),
    debiti_banche_fatt_percentuale = as.numeric(debiti_banche_fatt_percentuale),
    debt_equity_ratio_percentuale = as.numeric(debt_equity_ratio_percentuale),
    debt_EBITDA_ratio_percentuale = as.numeric(debt_EBITDA_ratio_percentuale),
    rotazione_cap_investito_volte = as.numeric(rotazione_cap_investito_volte),
    indice_liquidita = as.numeric(indice_liquidita),
    indice_corrente = as.numeric(indice_corrente),
    indice_indebitamento_breve_percentuale=as.numeric(indice_indebitamento_breve_percentuale),
    indice_indebitamento_lungo_percentuale=as.numeric(indice_indebitamento_lungo_percentuale),
    indice_copertura_immob_patrimoniale_percentuale = as.numeric(indice_copertura_immob_patrimoniale_percentuale),
    rapporto_indebitamento=as.numeric(rapporto_indebitamento),
    indice_copertura_immob_finanziario_percentuale = as.numeric(indice_copertura_immob_finanziario_percentuale),
    grado_copertura_interessi_percentuale = as.numeric(grado_copertura_interessi_percentuale),
    oneri_finanziari_fatt_percentuale = as.numeric(oneri_finanziari_fatt_percentuale),
    indice_indipendenza_finanziaria_percentuale = as.numeric(indice_indipendenza_finanziaria_percentuale),
    grado_indipendenza_terzi_percentuale = as.numeric(grado_indipendenza_terzi_percentuale),
    posizione_finanziaria_netta=as.numeric(posizione_finanziaria_netta),
    rotazione_cap_investito = as.numeric(rotazione_cap_investito),
    rotazione_cap_circolante_lordo = as.numeric(rotazione_cap_circolante_lordo),
    incidenza_circolante_operativo_percentuale = as.numeric(incidenza_circolante_operativo_percentuale),
    giacenza_media_scorte_gg = as.numeric(giacenza_media_scorte_gg),
    durata_media_crediti_lordo_IVA_gg = as.numeric(durata_media_crediti_lordo_IVA_gg),
    durata_media_debiti_lordo_IVA_gg = as.numeric(durata_media_debiti_lordo_IVA_gg),
    EBITDA=as.numeric(EBITDA),
    ROI_percentuale = as.numeric(ROI_percentuale),
    incidenza_oneri_extrag_percentuale = as.numeric(incidenza_oneri_extrag_percentuale),
    ricavi_pro_capite_EUR = as.numeric(ricavi_pro_capite_EUR),
    valore_aggiunto_pro_capite_EUR = as.numeric(valore_aggiunto_pro_capite_EUR),
    costo_lavoro_addetto_EUR = as.numeric(costo_lavoro_addetto_EUR),
    rendimento_dipendenti = as.numeric(rendimento_dipendenti),
    capitale_circolante_netto_migl_EUR=as.numeric(capitale_circolante_netto_migl_EUR),
    margine_consumi_migl_EUR=as.numeric(margine_consumi_migl_EUR),
    margine_tesoreria_migl_EUR=as.numeric(margine_tesoreria_migl_EUR),
    margine_struttura_migl_EUR = as.numeric(margine_struttura_migl_EUR),
    flusso_cassa_gestione_migl_EUR=as.numeric(flusso_cassa_gestione_migl_EUR),
    commissioni_attive_migl_EUR=as.numeric(commissioni_attive_migl_EUR)
  ) %>%
  drop_na()  # Rimuovi righe con NA se necessario

cat("Colonne finali dataset_completo:\n")
print(colnames(dataset_completo))
cat("Sintesi dataset_completo:\n")
print(summary(dataset_completo))

cat("Salvataggio dataset completo in Excel...\n")
write.xlsx(dataset_completo, file.path(output_dir, "dataset_completo.xlsx"))

cat("Salvataggio dataset completo in RDS...\n")
saveRDS(dataset_completo, file.path(output_dir, "dataset_completo.rds"))

cat(paste0("\n========== FINE FASE: ", fase_nome, " ==========\n"))