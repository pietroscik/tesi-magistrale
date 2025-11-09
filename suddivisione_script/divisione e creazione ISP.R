# =================== FASE 2: FEATURE ENGINEERING & ISP CALCULATION ===================
fase_nome <- "02_feature"
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

##### Divisione del set per categoria####
cat("Controllo e caricamento del dataset completo...\n")
if (!exists("dataset_completo") || !("codice_fiscale" %in% names(dataset_completo))) {
  message("Caricamento dataset_completo.rds...")
  # Usa here() per trovare il file RDS
  dataset_completo <- readRDS(here("01_cleaning", "dataset_completo.rds")) 
}

cat("Suddivisione dataset per area geografica...\n")
dataset_ridotto <- dataset_completo %>%
  mutate(macroarea = case_when(
    regione %in% c("Valle d'Aosta/Vallée d'Aoste", "Piemonte", "Liguria", "Lombardia") ~ "Nord-Ovest",
    regione %in% c("Veneto", "Trentino-Alto Adige", "Friuli-Venezia Giulia", "Emilia-Romagna") ~ "Nord-Est",
    regione %in% c("Toscana", "Umbria", "Marche", "Lazio") ~ "Centro",
    regione %in% c("Campania", "Molise", "Puglia", "Basilicata", "Calabria","Abruzzo") ~ "Sud",
    regione %in% c("Sicilia","Sardegna") ~ "Isole",
    TRUE ~ "Errore"
  ))%>%
  mutate(macroarea=as.factor(macroarea))%>%
  select(macroarea,latitudine,longitudine,codice_fiscale)

cat("Lettura shapefile comuni ISTAT...\n")
comuni <- st_read(here("data","Com01012025_WGS84.shp"), quiet=TRUE)

cat("Conversione imprese in sf, trasferimento CRS...\n")
imprese_sf_tmp <- st_as_sf(dataset_ridotto,
                           coords = c("longitudine","latitudine"),
                           crs = 4326)

imprese_sf_tmp <- st_transform(imprese_sf_tmp, st_crs(comuni))
gc()  # libera memoria intermedia

cat("Join con COD_RIP dei poligoni...\n")
imprese_sf_tmp <- st_join(imprese_sf_tmp, comuni, join = st_within)
gc()

cat("Aggiunta macroarea_shape e flag conflitti...\n")
imprese_sf_tmp <- imprese_sf_tmp %>%
  mutate(macroarea_shape = case_when(
    COD_RIP == 1 ~ "Nord-Ovest",
    COD_RIP == 2 ~ "Nord-Est",
    COD_RIP == 3 ~ "Centro",
    COD_RIP == 4 ~ "Sud",
    COD_RIP == 5 ~ "Isole",
    TRUE ~ NA_character_
  ))
imprese_sf_tmp <- imprese_sf_tmp %>%
  mutate(conflitto_macro = macroarea != macroarea_shape)
cat("Tabella conflitti macroarea:\n")
print(table(imprese_sf_tmp$conflitto_macro))

cat("Salvataggio imprese in conflitto...\n")
conflitti <- st_drop_geometry(imprese_sf_tmp) %>%
  filter(conflitto_macro)
write.csv(conflitti, file.path(output_dir, "conflitti_macroaree.csv"), row.names = FALSE)

cat("Correzione macroarea secondo shape ISTAT...\n")
imprese_sf_tmp <- imprese_sf_tmp %>%
  mutate(macroarea = ifelse(conflitto_macro & !is.na(macroarea_shape),
                            macroarea_shape, macroarea))

cat("Statistiche per macroarea dopo correzione:\n")
print(
  imprese_sf_tmp %>%
    group_by(macroarea_shape, macroarea) %>%
    summarise(n_imprese = n(), .groups = 'drop') %>%
    arrange(macroarea)
)

imprese_sf_tmp <- imprese_sf_tmp %>%
  mutate( macroarea = macroarea_shape)

# Aggiungi le colonne al data.frame "puro"
macro_corr <- imprese_sf_tmp %>%
  st_drop_geometry() %>%
  select(codice_fiscale, macroarea)

cat("Aggiorno macroarea nel dataset completo...\n")
dataset_completo <- dataset_completo %>%
  left_join(macro_corr, by = "codice_fiscale") %>%
  mutate(macroarea = as.factor(macroarea))

dataset_completo <- dataset_completo %>%
  filter(!is.na(macroarea))


### Sono risultate 69 imprese con dati difformi alla loro localizzazione
#   e 13 imprese che ricadono al di fuori dei confini

# Subset Completo di tutte le variabili
aree_di_interesse <- list(
  Identificativa = c("ragione_sociale", "numero_CCIAA", "codice_fiscale",
                     "ateco_2007_codice", "ateco_2007_descrizione",
                     "nome_gruppo_pari", "descrizione_gruppo_pari", "dimensione_gruppo_pari", "overview_completa"),
  
  Finanziaria = c("ricavi_vendite_migl_EUR", "utile_netto_migl_EUR", 
                  "EBITDA_migl_EUR", "patrimonio_netto_migl_EUR", 
                  "posizione_finanziaria_netta_migl_EUR",
                  "ROE_percentuale", "ROI_percentuale", "ROS_percentuale", 
                  "ROA_percentuale", "debiti_banche_fatt_percentuale",
                  "debt_equity_ratio_percentuale", "debiti_banche_fatt_percentuale",
                  "debt_EBITDA_ratio_percentuale"),
  
  Operativa = c("dipendenti", "giacenza_media_scorte_gg", 
                "durata_media_crediti_lordo_IVA_gg", "durata_media_debiti_lordo_IVA_gg",
                "incidenza_circolante_operativo_percentuale", "flusso_cassa_gestione_migl_EUR"),
  
  Geografica = c("macroarea","regione", "provincia", "ISTAT_regione", "ISTAT_provincia",
                 "ISTAT_comune", "longitudine", "latitudine", "CAP"),
  
  Giuridica = c("stato_giuridico", "forma_giuridica", "societa_artigiana", 
                "startup_innovativa", "PMI_innovativa", "operatore_estero"),
  
  Attività = c("linea_business_principale", "attivita_principale", "prodotti_servizi_principali",
               "nace_rev2_codice", "nace_rev2_descrizione"),
  
  Indicatori = c("indice_liquidita", "indice_corrente", "indice_indebitamento_breve_percentuale",
                 "indice_indebitamento_lungo_percentuale", "indice_copertura_immob_patrimoniale_percentuale",
                 "rapporto_indebitamento", "indice_copertura_immob_finanziario_percentuale",
                 "grado_indipendenza_terzi_percentuale", "posizione_finanziaria_netta",
                 "grado_copertura_interessi_percentuale", "oneri_finanziari_fatt_percentuale",
                 "indice_indipendenza_finanziaria_percentuale")
)
#Redditività e Performance Operativa
variabili_redditivita <- c("ROE_percentuale", "ROI_percentuale", "ROS_percentuale", 
                           "ROA_percentuale", "EBITDA_migl_EUR", "utile_netto_migl_EUR")
#Solidità Finanziaria e Indebitamento
variabili_solidita <- c("patrimonio_netto_migl_EUR", "posizione_finanziaria_netta_migl_EUR",
                         "indice_indipendenza_finanziaria_percentuale", "rapporto_indebitamento")
#Produttività e Gestione del Capitale Umano
variabili_produttivita <- c("dipendenti","ricavi_pro_capite_EUR","valore_aggiunto_pro_capite_EUR",
                            "rendimento_dipendenti")
# Liquidità e Gestione Finanziaria a Breve Termine
variabili_liquidita <- c("indice_liquidita", "indice_corrente",   "flusso_cassa_gestione_migl_EUR")
#Capitale Circolante e Gestione delle Scorte
variabili_circolante <- c("giacenza_media_scorte_gg","durata_media_crediti_lordo_IVA_gg",
                          "durata_media_debiti_lordo_IVA_gg", "incidenza_circolante_operativo_percentuale")
variabili_rischio_finanziario<- c("debt_equity_ratio_percentuale","debt_EBITDA_ratio_percentuale",
                                  "oneri_finanziari_fatt_percentuale","grado_copertura_interessi_percentuale",
                                  "debiti_banche_fatt_percentuale","incidenza_oneri_extrag_percentuale")
# Lista delle categorie di variabili
categorie_variabili <- list(
  Redditività = variabili_redditivita,
  Solidità = variabili_solidita,
  Produttività = variabili_produttivita,
  Liquidità = variabili_liquidita,
  Capitale_Circolante = variabili_circolante,
  Rischio_finanziario = variabili_rischio_finanziario
)

cat("Classificazione per settore (ATECO)...\n")
#Suddivisione codice ATECO xx.xx.xx
dataset_completo <- dataset_completo %>%
  mutate(
    sezione = substr(ateco_2007_codice, 1, 1),
    divisione = substr(ateco_2007_codice, 1, 2),
    gruppo = substr(ateco_2007_codice, 1, 3),
    classe = substr(ateco_2007_codice, 1, 5),
    categoria = substr(ateco_2007_codice, 1, 6)
  )
dataset_completo <- dataset_completo %>%
  mutate(divisione_ateco = as.numeric(substr(as.character(ateco_2007_codice), 1, 2))) %>%
  mutate(sezione = case_when(
    divisione_ateco >= 1  & divisione_ateco <= 3  ~ "A - Agricoltura, silvicoltura e pesca",
    divisione_ateco >= 5  & divisione_ateco <= 9  ~ "B - Estrazione di minerali da cave e miniere",
    divisione_ateco >= 10 & divisione_ateco <= 33 ~ "C - Attività manifatturiere",
    divisione_ateco == 35                        ~ "D - Fornitura di energia elettrica, gas, vapore e aria condizionata",
    divisione_ateco >= 36 & divisione_ateco <= 39 ~ "E - Fornitura di acqua; gestione rifiuti e risanamento",
    divisione_ateco >= 41 & divisione_ateco <= 43 ~ "F - Costruzioni",
    divisione_ateco >= 45 & divisione_ateco <= 47 ~ "G - Commercio all’ingrosso e al dettaglio",
    divisione_ateco >= 49 & divisione_ateco <= 53 ~ "H - Trasporto e magazzinaggio",
    divisione_ateco >= 55 & divisione_ateco <= 56 ~ "I - Servizi di alloggio e ristorazione",
    divisione_ateco >= 58 & divisione_ateco <= 63 ~ "J - Servizi di informazione e comunicazione",
    divisione_ateco >= 64 & divisione_ateco <= 66 ~ "K - Attività finanziarie e assicurative",
    divisione_ateco == 68                         ~ "L - Attività immobiliari",
    divisione_ateco >= 69 & divisione_ateco <= 75 ~ "M - Attività professionali, scientifiche e tecniche",
    divisione_ateco >= 77 & divisione_ateco <= 82 ~ "N - Noleggio, agenzie di viaggio, servizi di supporto",
    divisione_ateco == 84                         ~ "O - Amministrazione pubblica e difesa",
    divisione_ateco == 85                         ~ "P - Istruzione",
    divisione_ateco >= 86 & divisione_ateco <= 88 ~ "Q - Sanità e assistenza sociale",
    divisione_ateco >= 90 & divisione_ateco <= 93 ~ "R - Attività artistiche, sportive, di intrattenimento",
    divisione_ateco >= 94 & divisione_ateco <= 96 ~ "S - Altre attività di servizi",
    divisione_ateco >= 97 & divisione_ateco <= 98 ~ "T - Attività di famiglie e convivenze",
    divisione_ateco == 99                         ~ "U - Organizzazioni ed organismi extraterritoriali",
    TRUE ~ "Non classificato"
  ))

cat("Classificazione dimensione impresa...\n")
dataset_completo <- dataset_completo %>%
  mutate(
    dimensione_impresa = case_when(
      (dipendenti < 10 & ricavi_vendite_migl_EUR <= 2500 & totale_attivita_migl_EUR <= 2500) |
        (dipendenti < 10 & ricavi_vendite_migl_EUR <= 2500) |
        (dipendenti < 10 & totale_attivita_migl_EUR <= 2500) ~ "Micro",
      (dipendenti < 50 & ricavi_vendite_migl_EUR <= 12500 & totale_attivita_migl_EUR <= 12500) |
        (dipendenti < 50 & ricavi_vendite_migl_EUR <= 12500) |
        (dipendenti < 50 & totale_attivita_migl_EUR <= 12500) ~ "Piccola",
      (dipendenti < 250 & ricavi_vendite_migl_EUR <= 62500 & totale_attivita_migl_EUR <= 53750) |
        (dipendenti < 250 & ricavi_vendite_migl_EUR <= 62500) |
        (dipendenti < 250 & totale_attivita_migl_EUR <= 53750)  ~ "Media",
      TRUE ~ "Grande"
    )
  ) %>%
  mutate(
    dimensione_impresa = factor(dimensione_impresa, levels = c("Micro", "Piccola", "Media", "Grande"))
  )

# Statistiche descrittive
cat("Statistiche dimensione impresa...\n")
n_dimensione_imprese <- dataset_completo %>%
  group_by(dimensione_impresa) %>%
  summarise(
    n_imprese = n(),
    media_dipendenti = mean(dipendenti),
    media_fatturato = mean(ricavi_vendite_migl_EUR),
    media_valore_bilancio = mean(totale_attivita_migl_EUR),
    .groups = "drop"
  ) %>%
  mutate(percentuale = n_imprese/sum(n_imprese)*100)

print(n_dimensione_imprese)
cat("Salvataggio tabella classificazione_UE_2023.xlsx...\n")
write.xlsx(n_dimensione_imprese, file.path(output_dir, "classificazione_UE_2023.xlsx"))

# Numero di imprese per settore
n_imprese_settore <- dataset_completo %>%
  group_by(sezione) %>%
  summarise(n_imprese = n(), .groups = 'drop') %>%
  mutate(percentuale = (n_imprese / sum(n_imprese)) * 100) %>%
  arrange(desc(n_imprese)) 

# Numero di imprese per settore, dimensione e macroarea
n_imprese_settore_dimensione <- dataset_completo %>%
  group_by(sezione, dimensione_impresa, macroarea) %>%
  summarise(n_imprese = n(), .groups = "drop") %>%
  group_by(sezione) %>%
  mutate(percentuale = (n_imprese / sum(n_imprese)) * 100) %>%
  pivot_wider(
    names_from = dimensione_impresa,
    values_from = c(n_imprese, percentuale),
    names_glue = "{dimensione_impresa}_{.value}"
  ) 

# Numero di imprese per settore e macroarea
n_imprese_macroarea_settore <- dataset_completo %>%
  group_by(sezione, macroarea) %>%
  summarise(n_imprese = n(), .groups = 'drop') %>%
  mutate(percentuale = (n_imprese / sum(n_imprese)) * 100) %>%
  arrange(desc(n_imprese))

# Numero di imprese per macroarea
n_imprese_macroarea <- dataset_completo %>%
  group_by(macroarea) %>%
  summarise(n_imprese = n(), .groups = "drop") %>%
  mutate(percentuale = (n_imprese / sum(n_imprese)) * 100) %>%
  arrange(desc(n_imprese))

# Numero di imprese per regione
n_imprese_regione <- dataset_completo %>%
  group_by(regione) %>%
  summarise(n_imprese = n(), .groups = "drop") %>%
  mutate(percentuale = (n_imprese / sum(n_imprese)) * 100) %>%
  arrange(desc(n_imprese))

# Numero di imprese per forma giuridica
n_imprese_forma_giuridica <- dataset_completo %>%
  group_by(forma_giuridica) %>%
  summarise(n_imprese = n(), .groups = "drop") %>%
  mutate(percentuale = (n_imprese / sum(n_imprese)) * 100) %>%
  arrange(desc(n_imprese))

n_imprese_conteggio_stato <- dataset_completo %>%
  summarise(`Società artigiane`=sum(tolower(societa_artigiana)%in%c("si","sì","true","1"),na.rm=TRUE),
            `PMI innovative`=sum(tolower(PMI_innovativa)%in%c("si","sì","true","1"),na.rm=TRUE),
            `Operatori esteri`=sum(tolower(operatore_estero)%in%c("si","sì","true","1"),na.rm=TRUE),
            `In procedura di insolvenza`=sum(stato_giuridico=="Attiva (con procedure di insolvenza)",na.rm=TRUE),
            `In stato di insolvenza`=sum(stato_giuridico=="Attiva (Stato di insolvenza)",na.rm=TRUE),
            `Startup innovative`=sum(tolower(startup_innovativa)%in%c("si","sì","true","1"),na.rm=TRUE)) %>%
  tidyr::pivot_longer(everything(), names_to="metrica", values_to="n") %>%
  mutate(percentuale = round(100*n/nrow(dataset_completo), 2))


# Statistiche descrittive variabili
# --- Calcolo statistiche descrittive per ciascun gruppo ---
descrittive_tab <- lapply(names(categorie_variabili), function(gruppo) {
  vars <- categorie_variabili[[gruppo]]
  imp <- dataset_completo %>% select(any_of(vars))
  
  stats <- describe(imp)[, c("mean", "sd", "min", "median", "max")]
  stats <- round(stats, 2)
  stats$Gruppo <- gruppo
  stats$Variabile <- rownames(stats)
  stats$Coeff_Variabilita <- round(stats$sd / abs(stats$mean), 2)
  stats
}) %>%
  bind_rows() %>%
  select(Gruppo, Variabile, Minimo = min, Massimo = max, Media = mean,
         Mediana = median, Dev_standard = sd, `Coeff Variabilità` = Coeff_Variabilita)

descrittive_tab <- descrittive_tab %>%
  mutate(Variabile = factor(Variabile, levels = unlist(categorie_variabili))) %>%
  arrange(Variabile)

# --- Esporta in Excel e Word ---
cat("Salvataggio tabelle aggregazione imprese...\n")
write.xlsx(descrittive_tab,file.path(output_dir, "Tabella_statistiche_descrittive.xlsx"))
write.xlsx(n_imprese_regione, file.path(output_dir, "numero_imprese_regione.xlsx"))
write.xlsx(n_imprese_macroarea, file.path(output_dir, "numero_imprese_macroarea.xlsx"))
write.xlsx(n_imprese_macroarea_settore, file.path(output_dir, "numero_imprese_macroarea_settore.xlsx"))
write.xlsx(n_imprese_settore, file.path(output_dir, "numero_imprese_per_settore.xlsx"))
write.xlsx(n_imprese_settore_dimensione, file.path(output_dir, "numero_dimensione_imprese_per_settore&area.xlsx"))
write.xlsx(n_imprese_forma_giuridica, file.path(output_dir, "numero_imprese_forma_giuridica.xlsx"))
write.xlsx(n_imprese_conteggio_stato, file.path(output_dir, "numero_imprese_conteggio_stato.xlsx"))
write.xlsx(dataset_completo, file.path(output_dir, "dataset.xlsx"))

##### Indicatore Sintetico Posizione(media ponderata degli indici di categoria)#####
cat("Calcolo PFN_EBITDA_migl_EUR...\n")
dataset_completo$PFN_EBITDA_migl_EUR <- ifelse(
  dataset_completo$EBITDA_migl_EUR == 0,0,
  dataset_completo$posizione_finanziaria_netta_migl_EUR / dataset_completo$EBITDA_migl_EUR
)

cat("Normalizzazione variabili e calcolo ISP...\n")
normalize <- function(x, scale = 1) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0, length(x)))  # evita divisione per zero
  (x - rng[1]) / (rng[2] - rng[1]) * scale
}
dataset_normalizzato <- dataset_completo
dataset_normalizzato[, unlist(categorie_variabili)] <- lapply(
  dataset_completo[, unlist(categorie_variabili)],
  function(x) if (is.numeric(x)) normalize(x, scale = 1000) else x
)

variabili_da_normalizzare <- c(
  "capitale_sociale_migl_EUR", "utile_netto_migl_EUR","PFN_EBITDA_migl_EUR",
  "debt_equity_ratio_percentuale", "debiti_banche_fatt_percentuale",
  "rotazione_cap_investito_volte", "indice_indebitamento_breve_percentuale",
  "indice_indebitamento_lungo_percentuale",
  "indice_copertura_immob_patrimoniale_percentuale",
  "rapporto_indebitamento", "indice_copertura_immob_finanziario_percentuale",
  "grado_copertura_interessi_percentuale", "oneri_finanziari_fatt_percentuale",
  "indice_indipendenza_finanziaria_percentuale",
  "grado_indipendenza_terzi_percentuale",
  "posizione_finanziaria_netta", "rotazione_cap_circolante_lordo",
  "costo_lavoro_addetto_EUR","margine_consumi_migl_EUR",
  "margine_tesoreria_migl_EUR","margine_struttura_migl_EUR",
  "commissioni_attive_migl_EUR","ricavi_vendite_migl_EUR", 
  "capitale_sociale_migl_EUR","dimensione_gruppo_pari", 
  "totale_attivita_migl_EUR","rotazione_cap_investito"
)
variabili_non_trovate <- variabili_da_normalizzare[!variabili_da_normalizzare %in% names(dataset_completo)]
if(length(variabili_non_trovate) > 0) {
  cat("Attenzione, variabili non trovate per normalizzazione:\n")
  print(variabili_non_trovate)
}
for (v in variabili_da_normalizzare) {
  dataset_normalizzato[[v]] <- normalize(dataset_completo[[v]],scale = 1000)
}

cat("Calcolo ISP per i due gruppi...\n")
ISP_a <- (dataset_normalizzato$ROE_percentuale* 0.2727) + 
  (dataset_normalizzato$EBITDA_su_vendite_percentuale * 0.3560) + 
  (dataset_normalizzato$ROI_percentuale * 0.2386) + 
  (dataset_normalizzato$rotazione_cap_investito * 0.1327)

ISP_b <- (dataset_normalizzato$debt_equity_ratio_percentuale * 0.3162) +
  (dataset_normalizzato$debt_EBITDA_ratio_percentuale * 0.2703) + 
  (dataset_normalizzato$totale_attivita_migl_EUR * 0.1583) +
  (dataset_normalizzato$PFN_EBITDA_migl_EUR * 0.2552)

cat("Visualizzo istogrammi ISP_a e ISP_b...\n")
hist(ISP_a * 0.4311) 
hist(ISP_b * 0.5689)

cat("Calcolo ISP totale e salvataggio...\n")
dataset_completo$ISP <- (ISP_a * 0.4311) + (ISP_b * 0.5689)
dataset_normalizzato$ISP <- ((ISP_a * 0.4311) + (ISP_b * 0.5689))

cat("Salvataggio matrice.rds del dataset completo...\n")
saveRDS(dataset_completo, file.path(output_dir, "matrice.rds"))
saveRDS(categorie_variabili, file.path(output_dir, "categorie_variabili.rds"))

cat(paste0("\n========== FINE FASE: ", fase_nome, " ==========\n"))
