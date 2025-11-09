# =================== FASE 3: VALIDAZIONE ISP & ANALISI INFERENZIALE ===================
fase_nome <- "03_validation"
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
# Funzione di normalizzazione 
normalize <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0, length(x)))  # evita divisione per zero se tutti i valori uguali
  (x - rng[1]) / (rng[2] - rng[1]) * 1000
}


# Cartella output&input dedicata
input_dir<- here("02_feature")
output_dir <- here(fase_nome)
if (!dir.exists(output_dir)) dir.create(output_dir)

##### Inferenza pesi ISP ####
cat("Caricamento dataset_completo...\n")
if (!exists("dataset_completo") || !("ISP" %in% names(dataset_completo))) {
  dataset_completo <- readRDS(file.path(here("02_feature"), "matrice.rds"))}
categorie_variabili <- readRDS(file.path(here("02_feature"),"categorie_variabili.rds")) 
  
#aggiunta categoria variabili performance
vars_model <- unname(unlist(categorie_variabili))

dataset_z_score<-as.data.frame(scale(dataset_completo[c(vars_model,"ISP","EBITDA_su_vendite_percentuale","rotazione_cap_investito",
                                                        "totale_attivita_migl_EUR","PFN_EBITDA_migl_EUR","indice_indipendenza_finanziaria_percentuale",
                                                        "grado_copertura_interessi_percentuale","indice_indebitamento_breve_percentuale",
                                                        "rapporto_indebitamento","oneri_finanziari_fatt_percentuale")]))
dataset_z_score$codice_fiscale<-dataset_completo$codice_fiscale
dataset_z_score$sezione<-dataset_completo$sezione
# Variabili del gruppo ISP_a
vars_a <- dataset_z_score[, c("ROE_percentuale", 
                              "EBITDA_su_vendite_percentuale", 
                              "ROI_percentuale", 
                              "rotazione_cap_investito")]

# PCA
cat("Eseguo PCA su ISP_a...\n")
pca_a <- prcomp(vars_a, scale. = TRUE)
summary(pca_a)

# Pesi dalla prima componente
pesi_pca_a <- pca_a$rotation[, 1]
pesi_pca_a_norm <- pesi_pca_a / sum(abs(pesi_pca_a))  # normalizzati in somma assoluta

# Calcolo del nuovo ISP_a con pesi ottimizzati
ISP_a_pca <- as.matrix(vars_a) %*% pesi_pca_a_norm
head(ISP_a_pca)

# Istogramma del nuovo ISP_a
hist(ISP_a_pca, main = "ISP_a - Pesi PCA", col = "skyblue", xlab = "ISP_a PCA")
# Regressione lineare su ISP
cat("Eseguo regressione lineare su ISP_a...\n")
lm_a <- lm(ISP ~ ROE_percentuale + EBITDA_su_vendite_percentuale + ROI_percentuale + rotazione_cap_investito,
           data = dataset_z_score)

summary(lm_a)  # visualizza i coefficienti stimati

# Estrazione dei coefficienti (escludendo l'intercetta)
pesi_lm_a <- coef(lm_a)[-1]
pesi_lm_a_norm <- pesi_lm_a / sum(abs(pesi_lm_a))  # normalizzazione in somma assoluta

# Calcolo ISP_a ottimizzato
vars_a <- dataset_z_score[, names(pesi_lm_a)]
ISP_a_lm <- as.matrix(vars_a) %*% pesi_lm_a_norm
summary(ISP_a_lm)
head(ISP_a_lm)

# Istogramma del nuovo ISP_a
hist(ISP_a_lm, main = "ISP_a - Pesi Regressione", col = "tomato", xlab = "ISP_a LM")

vars_b <- dataset_z_score[, c(
  "debt_equity_ratio_percentuale", 
  "debt_EBITDA_ratio_percentuale", 
  "totale_attivita_migl_EUR", 
  "PFN_EBITDA_migl_EUR"
)]
# PCA
cat("Eseguo PCA su ISP_b...\n")
pca_b <- prcomp(vars_b, scale. = TRUE)
summary(pca_b)

# Pesi dalla prima componente
pesi_pca_b <- pca_b$rotation[, 1]
pesi_pca_b_norm <- pesi_pca_b / sum(abs(pesi_pca_b))  # normalizzati in somma assoluta

# Calcolo del nuovo ISP_a con pesi ottimizzati
ISP_b_pca <- as.matrix(vars_b) %*% pesi_pca_b_norm
head(ISP_b_pca)

# Istogramma del nuovo ISP_a
hist(ISP_b_pca, main = "ISP_b - Pesi PCA", col = "skyblue", xlab = "ISP_b PCA")
# Regressione lineare su ISP
cat("Eseguo regressione lineare su ISP_b...\n")
lm_b <- lm(ISP ~debt_equity_ratio_percentuale+debt_EBITDA_ratio_percentuale+
             totale_attivita_migl_EUR+PFN_EBITDA_migl_EUR,
           data = dataset_z_score)

summary(lm_b)  # visualizza i coefficienti stimati

# Estrazione dei coefficienti (escludendo l'intercetta)
pesi_lm_b <- coef(lm_b)[-1]
pesi_lm_b_norm <- pesi_lm_b / sum(abs(pesi_lm_b))  # normalizzazione in somma assoluta

# Calcolo ISP_a ottimizzato
vars_b <- dataset_z_score[, names(pesi_lm_b)]
ISP_b_lm <- as.matrix(vars_b) %*% pesi_lm_b_norm
summary(ISP_b_lm)
head(ISP_b_lm)
# Istogramma del nuovo ISP_a
hist(ISP_b_lm, main = "ISP_b - Pesi Regressione", col = "tomato", xlab = "ISP_b LM")

# Min-max normalization (range 0–1000)
cat("Normalizzazione min-max...\n")
ISP_a_pca_norm <- normalize(ISP_a_pca)
ISP_b_pca_norm <- normalize(ISP_b_pca)

# ISP prima componente principale
ISP_pca_totale <- ISP_a_pca_norm * 0.5 + ISP_b_pca_norm * 0.5  # oppure pesi diversi
hist(ISP_pca_totale)

# Z-score standardization
standardize <- function(x) scale(x)[, 1]

ISP_a_lm_std <- standardize(ISP_a_lm)
ISP_b_lm_std <- standardize(ISP_b_lm)

ISP_lm_totale <- ISP_a_lm_std * 0.5 + ISP_b_lm_std * 0.5
hist(ISP_lm_totale)

# Normalizzazione
ISP_a_lm_norm  <- normalize(ISP_a_lm)
ISP_b_lm_norm  <- normalize(ISP_b_lm)

# Combinazioni con pesi originali
pesa <- 0.4311
pesb <- 0.5689

ISP_totale_pca <- ISP_a_pca_norm * pesa + ISP_b_pca_norm * pesb
ISP_totale_lm  <- ISP_a_lm_norm  * pesa + ISP_b_lm_norm  * pesb

# Standardizzazione (z-score)
ISP_a_pca_std <- standardize(ISP_a_pca)
ISP_b_pca_std <- standardize(ISP_b_pca)

ISP_totale_pca_std <- ISP_a_pca_std * pesa + ISP_b_pca_std * pesb
ISP_totale_lm_std  <- ISP_a_lm_std  * pesa + ISP_b_lm_std  * pesb

# CONFRONTO VISIVO
par(mfrow = c(2, 2))  # 2x2 layout

hist(ISP_totale_pca, main = "ISP Totale (PCA normalizzato)", col = "skyblue", xlab = "ISP PCA Norm")
hist(ISP_totale_pca_std, main = "ISP Totale (PCA standardizzato)", col = "skyblue3", xlab = "ISP PCA Std")
hist(ISP_totale_lm,  main = "ISP Totale (Regressione normalizzato)", col = "tomato", xlab = "ISP LM Norm")
hist(ISP_totale_lm_std,  main = "ISP Totale (Regressione standardizzato)", col = "tomato3", xlab = "ISP LM Std")

par(mfrow = c(1, 1))  # ripristina layout

# CONFRONTO NUMERICO
df_isp <- data.frame(
  ISP=dataset_completo$ISP,
  ISP_pca_norm = ISP_totale_pca,
  ISP_lm_norm = ISP_totale_lm,
  ISP_std= dataset_z_score$ISP,
  ISP_pca_std = ISP_totale_pca_std,
  ISP_lm_std = ISP_totale_lm_std
)

summary(df_isp)

##### Stima di ulteriori variabili nel gruppo B####
cat("Stima ulteriori variabili gruppo B...\n")

# --- Variabili di base ---
vars_a <- c("ROE_percentuale", "EBITDA_su_vendite_percentuale", "ROI_percentuale", "rotazione_cap_investito")
vars_b <- c("debt_equity_ratio_percentuale", "debt_EBITDA_ratio_percentuale", "totale_attivita_migl_EUR", "PFN_EBITDA_migl_EUR")

# --- Variabili estese per gruppo B ---
vars_b_esteso <- c(vars_b,
                   "indice_liquidita", "indice_corrente", "indice_indebitamento_breve_percentuale",
                   "grado_copertura_interessi_percentuale", "indice_indipendenza_finanziaria_percentuale",
                   "rapporto_indebitamento", "oneri_finanziari_fatt_percentuale")
# Vedi tutte le variabili originali selezionate
names(dataset_z_score)

# --- Funzione per stima pesi LM e LASSO ---
stima_pesi <- function(data, y_var, x_vars) {
  # Linear model
  formula <- as.formula(paste(y_var, "~", paste(x_vars, collapse = "+")))
  lm_mod <- lm(formula, data=data)
  coefs_lm <- coef(lm_mod)[x_vars]
  coefs_lm_norm <- coefs_lm / sum(abs(coefs_lm))
  ISP_lm <- as.matrix(data[, x_vars]) %*% coefs_lm_norm
  
  # LASSO
  X <- model.matrix(formula, data)[, -1]
  y <- data[[y_var]]
  cv_lasso <- cv.glmnet(X, y, alpha=1)
  best_lambda <- cv_lasso$lambda.min
  model_lasso <- glmnet(X, y, alpha=1, lambda=best_lambda)
  coefs_lasso <- as.vector(coef(model_lasso))[-1]
  coefs_lasso_norm <- coefs_lasso / sum(abs(coefs_lasso))
  ISP_lasso <- as.matrix(data[, x_vars]) %*% coefs_lasso_norm
  
  list(
    lm_mod = lm_mod,
    coefs_lm_norm = coefs_lm_norm,
    ISP_lm = ISP_lm,
    model_lasso = model_lasso,
    coefs_lasso_norm = coefs_lasso_norm,
    ISP_lasso = ISP_lasso,
    r2_lm = summary(lm_mod)$r.squared
  )
}
# --- Stima pesi e ISP gruppo A ---
cat("Stimo pesi/ISP gruppo A...\n")
res_a <- stima_pesi(dataset_z_score, "ISP", vars_a)
ISP_a_lm_norm <- normalize(res_a$ISP_lm)
ISP_a_lasso_norm <- normalize(res_a$ISP_lasso)

# --- Stima pesi e ISP gruppo B base ---
cat("Stimo pesi/ISP gruppo B base...\n")
res_b <- stima_pesi(dataset_z_score, "ISP", vars_b)
ISP_b_lm_norm <- normalize(res_b$ISP_lm)
ISP_b_lasso_norm <- normalize(res_b$ISP_lasso)

# --- Stima pesi e ISP gruppo B esteso ---
cat("Stimo pesi/ISP gruppo B esteso...\n")
res_b_ext <- stima_pesi(dataset_z_score, "ISP", vars_b_esteso)
ISP_b_ext_lm_norm <- normalize(res_b_ext$ISP_lm)
ISP_b_ext_lasso_norm <- normalize(res_b_ext$ISP_lasso)

# --- Calcolo pesi combinati basati su R² ---
peso_a <- res_a$r2_lm / (res_a$r2_lm + res_b_ext$r2_lm)
peso_b <- res_b_ext$r2_lm / (res_a$r2_lm + res_b_ext$r2_lm)

# --- ISP totali combinati gruppo B ext ---
ISP_tot_lm_ext <- peso_a * ISP_a_lm_norm + peso_b * ISP_b_ext_lm_norm
ISP_tot_lasso_ext <- peso_a * ISP_a_lasso_norm + peso_b * ISP_b_ext_lasso_norm

# --- ISP totali combinati  ---
ISP_tot_lm <- peso_a * ISP_a_lm_norm + peso_b * ISP_b_lm_norm
ISP_tot_lasso <- peso_a * ISP_a_lasso_norm + peso_b * ISP_b_lasso_norm

par(mfrow=c(2,2))
hist(ISP_tot_lm, main="ISP Totale - Linear Model", col="blue", xlab="ISP LM Normalizzato")
hist(ISP_tot_lasso, main="ISP Totale - LASSO", col="green", xlab="ISP LASSO Normalizzato")
hist(ISP_tot_lm_ext, main="ISP Totale - Linear Model", col="lightblue", xlab="ISP LM Normalizzato ext")
hist(ISP_tot_lasso_ext, main="ISP Totale - LASSO", col="lightgreen", xlab="ISP LASSO Normalizzato ext")
par(mfrow=c(1,1))

# --- Output riepilogativo ---
cat("R² gruppo A (LM):", round(res_a$r2_lm, 4), "\n")
cat("R² gruppo B esteso (LM):", round(res_b_ext$r2_lm, 4), "\n")
cat("Peso gruppo A:", round(peso_a, 4), "Peso gruppo B:", round(peso_b, 4), "\n")

# CONFRONTO NUMERICO
# Aggiungo i vettori ISP_tot_lm e ISP_tot_lasso normalizzati e standardizzati al dataframe

ISP_tot_lm_std <- scale(ISP_tot_lm)[,1]        # standardizzazione z-score (media=0, sd=1)
ISP_tot_lasso_std <- scale(ISP_tot_lasso)[,1]
ISP_tot_lm_std_ext <- scale(ISP_tot_lm_ext)[,1]        # standardizzazione z-score (media=0, sd=1)
ISP_tot_lasso_std_ext <- scale(ISP_tot_lasso_ext)[,1]

# Creo il dataframe di confronto
df_isp <- data.frame(
  ISP = dataset_completo$ISP,
  ISP_pca_norm = ISP_pca_totale,
  ISP_lm_norm = ISP_totale_lm,
  ISP_lm_ext_norm = ISP_tot_lm_ext,
  ISP_lm_tot_norm = ISP_tot_lm,
  ISP_lasso_ext_norm = ISP_tot_lasso_ext,
  ISP_std = dataset_z_score$ISP,
  ISP_pca_std = ISP_totale_pca_std,
  ISP_lm_std = ISP_totale_lm_std,
  ISP_lm_ext_std = ISP_tot_lm_std_ext,
  ISP_lm_tot_std = ISP_tot_lm_std,
  ISP_lasso_ext_std = ISP_tot_lasso_std_ext
)

# Riepilogo statistico sintetico
summary(df_isp)

# Visualizzazione grafica comparativa (optional)
par(mfrow=c(1,4))
hist(df_isp$ISP_pca_norm, main="ISP PCA Normalizzato", col="skyblue", xlab="ISP PCA Norm")
hist(df_isp$ISP_lm_norm, main="ISP LM Normalizzato", col="lightgreen", xlab="ISP LM Norm")
hist(df_isp$ISP_lasso_ext_norm, main="ISP LASSO Normalizzato", col="salmon", xlab="ISP LASSO Norm")
hist(df_isp$ISP, main="ISP Base Normalizzato", col="lightgray", xlab="ISP Base Norm")
par(mfrow=c(1,1))
# Visualizzazione grafica comparativa aggiornata
par(mfrow = c(2, 4), mar = c(4, 4, 2, 1))

hist(df_isp$ISP_pca_norm, main = "ISP PCA (norm)", col = "skyblue", xlab = "", breaks = 30)
hist(df_isp$ISP_lm_norm, main = "ISP LM base (norm)", col = "lightgreen", xlab = "", breaks = 30)
hist(df_isp$ISP_lm_ext_norm, main = "ISP LM esteso (norm)", col = "khaki", xlab = "", breaks = 30)
hist(df_isp$ISP_lasso_ext_norm, main = "ISP LASSO esteso (norm)", col = "salmon", xlab = "", breaks = 30)

hist(df_isp$ISP_pca_std, main = "ISP PCA (std)", col = "steelblue", xlab = "", breaks = 30)
hist(df_isp$ISP_lm_std, main = "ISP LM base (std)", col = "forestgreen", xlab = "", breaks = 30)
hist(df_isp$ISP_lm_ext_std, main = "ISP LM esteso (std)", col = "goldenrod", xlab = "", breaks = 30)
hist(df_isp$ISP_lasso_ext_std, main = "ISP LASSO esteso (std)", col = "firebrick", xlab = "", breaks = 30)

par(mfrow = c(1, 1))  # Reset layout

##### Analisi settoriale con LASSO e LM ####
results_settoriali <- list()
settori <- unique(dataset_z_score$sezione)

for (settore in unique(dataset_z_score$sezione)) {
  dati <- subset(dataset_z_score, sezione == settore)
  dati_test <- dati[, unique(c("ISP", vars_a, vars_b_esteso))]
  cat(settore, ": totale =", nrow(dati), 
      " -> complete.cases =", sum(complete.cases(dati_test)), "\n")
}

for (settore in settori) {
  dati <- subset(dataset_z_score, sezione == settore)
  
  if (nrow(dati) < 30) {
    message(paste("Settore", settore, "saltato: solo", nrow(dati), "osservazioni"))
    next
  }
  
  res_a_sett <- stima_pesi(dati, "ISP", vars_a)
  res_b_sett <- stima_pesi(dati, "ISP", vars_b_esteso)
  
  peso_a_sett <- res_a_sett$r2_lm / (res_a_sett$r2_lm + res_b_sett$r2_lm)
  peso_b_sett <- res_b_sett$r2_lm / (res_a_sett$r2_lm + res_b_sett$r2_lm)
  
  ISP_sett_lm <- peso_a_sett * normalize(res_a_sett$ISP_lm) + peso_b_sett * normalize(res_b_sett$ISP_lm)
  ISP_sett_lasso <- peso_a_sett * normalize(res_a_sett$ISP_lasso) + peso_b_sett * normalize(res_b_sett$ISP_lasso)
  
  results_settoriali[[settore]] <- list(
    ISP_sett_lm = ISP_sett_lm,
    ISP_sett_lasso = ISP_sett_lasso,
    peso_a_sett = peso_a_sett,
    peso_b_sett = peso_b_sett,
    r2_a = res_a_sett$r2_lm,
    r2_b = res_b_sett$r2_lm
  )
}

# Costruzione tabella riassuntiva dei risultati settoriali
tabella_risultati <- do.call(rbind, lapply(names(results_settoriali), function(settore) {
  r <- results_settoriali[[settore]]
  data.frame(
    sezione = settore,
    r2_gruppo_a = round(r$r2_a, 4),
    r2_gruppo_b = round(r$r2_b, 4),
    peso_gruppo_a = round(r$peso_a_sett, 4),
    peso_gruppo_b = round(r$peso_b_sett, 4)
  )
}))

# Visualizza i risultati
print(tabella_risultati)

# Facoltativo: salva su file
cat("Salvataggio tabella riassuntiva settoriale...\n")
write.csv(tabella_risultati, file.path(output_dir, "risultati_settoriali_pesi.csv"), row.names = FALSE)

# Riorganizza per plot
tabella_plot <- pivot_longer(tabella_risultati, 
                             cols = starts_with("peso_gruppo"), 
                             names_to = "gruppo", 
                             values_to = "peso")

# Rinomina gruppi per leggibilità
tabella_plot$gruppo <- factor(tabella_plot$gruppo, 
                              levels = c("peso_gruppo_a", "peso_gruppo_b"),
                              labels = c("Redditività/Performance", "Patrimoniale/Finanziario"))

# Ordina le sezioni in base al peso del gruppo A (decrescente)
ordine_sezioni <- tabella_risultati %>%
  arrange(desc(peso_gruppo_a)) %>%
  pull(sezione)

tabella_plot$sezione <- factor(tabella_plot$sezione, levels = ordine_sezioni)

# Grafico
ggplot(tabella_plot, aes(x = sezione, y = peso, fill = gruppo)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Pesi stimati per gruppo di variabili per settore (ordinati per peso A decrescente)",
       x = "Sezione ATECO",
       y = "Peso stimato",
       fill = "Gruppo") +
  theme_minimal()

for (settore in names(results_settoriali)) {
  index <- dataset_z_score$sezione == settore
  dati_settore <- subset(dataset_z_score, sezione == settore)
  
  ISP_lm <- results_settoriali[[settore]]$ISP_sett_lm
  ISP_lasso <- results_settoriali[[settore]]$ISP_sett_lasso
  
  dataset_z_score$ISP_sett_lm[index] <- ISP_lm
  dataset_z_score$ISP_sett_lasso[index] <- ISP_lasso
}
ls()
# Standardizzazione z-score dei modelli settoriali
ISP_sett_lm_std <- scale(dataset_z_score$ISP_sett_lm)[,1]
ISP_sett_lasso_std <- scale( dataset_z_score$ISP_sett_lasso)[,1]

# Aggiunta al dataframe di confronto
df_isp$ISP_sett_lm_norm <- dataset_z_score$ISP_sett_lm
df_isp$ISP_sett_lm_std <- ISP_sett_lm_std
df_isp$ISP_sett_lasso_norm <-  dataset_z_score$ISP_sett_lasso
df_isp$ISP_sett_lasso_std <- ISP_sett_lasso_std

summary(df_isp)

# Visualizzazione grafica degli ulterio ISP
par(mfrow = c(3, 4), mar = c(4, 4, 2, 1))

hist(df_isp$ISP_sett_lm_norm, main = "ISP LM sett (norm)", col = "lightblue", xlab = "", breaks = 30)
hist(df_isp$ISP_sett_lasso_norm, main = "ISP LASSO sett (norm)", col = "lightsalmon", xlab = "", breaks = 30)
hist(df_isp$ISP_sett_lm_std, main = "ISP LM sett (std)", col = "dodgerblue", xlab = "", breaks = 30)
hist(df_isp$ISP_sett_lasso_std, main = "ISP LASSO sett (std)", col = "indianred", xlab = "", breaks = 30)

hist(df_isp$ISP_pca_norm, main = "ISP PCA (norm)", col = "skyblue", xlab = "", breaks = 30)
hist(df_isp$ISP_lm_norm, main = "ISP LM base (norm)", col = "lightgreen", xlab = "", breaks = 30)
hist(df_isp$ISP_pca_std, main = "ISP PCA (std)", col = "steelblue", xlab = "", breaks = 30)
hist(df_isp$ISP_lm_std, main = "ISP LM base (std)", col = "forestgreen", xlab = "", breaks = 30)

hist(df_isp$ISP_lm_ext_norm, main = "ISP LM esteso (norm)", col = "khaki", xlab = "", breaks = 30)
hist(df_isp$ISP_lasso_ext_norm, main = "ISP LASSO esteso (norm)", col = "salmon", xlab = "", breaks = 30)
hist(df_isp$ISP_lm_ext_std, main = "ISP LM esteso (std)", col = "goldenrod", xlab = "", breaks = 30)
hist(df_isp$ISP_lasso_ext_std, main = "ISP LASSO esteso (std)", col = "firebrick", xlab = "", breaks = 30)

par(mfrow = c(1, 1))
isp_mat <- df_isp[, grep("_std$", names(df_isp))]
cor_isp <- cor(isp_mat, use = "complete.obs", method = "spearman")
corrplot(cor_isp, method = "color", type = "upper", tl.cex = 0.8)

# inversione di PCA
df_isp$ISP_pca_norm<-1000-df_isp$ISP_pca_norm
df_isp$ISP_pca_std<-scale(df_isp$ISP_pca_norm)
summary(df_isp)

# CORRELAZIONI
cor(df_isp, use = "complete.obs")

df_isp_norm <- df_isp[, grep("_norm$", names(df_isp))]
df_isp_norm$ISP <- df_isp$ISP
# Calcola la matrice dei rank decrescenti
rank_matrix <- apply(df_isp_norm, 2, function(x) rank(-x))

# Usa solo le colonne dei ranking
rank_matrix <- apply(df_isp_norm, 2, function(x) rank(-x))
kendall_result <- kendall(rank_matrix)
print(kendall_result)

# Aggiungi media e deviazione standard del rank per ciascuna impresa
rank_summary <- as.data.frame(rank_matrix)
rank_summary$media_rank <- rowMeans(rank_matrix)
rank_summary$sd_rank <- apply(rank_matrix, 1, sd)
rank_summary$media_rank <- apply(rank_matrix, 1, mean, na.rm = TRUE)

# Definiamo soglie
top_soglia <- 1000   # top 100 aziende su tutte
outlier_sd_soglia <- 5000  # soglia empirica per forte distorsione

# Filtra top performer e outlier
top_aziende <- rank_summary[rank_summary$media_rank <= top_soglia, ]
outlier_aziende <- rank_summary[rank_summary$sd_rank >= outlier_sd_soglia, ]

# Visualizza
cat("== Migliori aziende (rank medio <= 100) ==\n")
print(head(top_aziende[order(top_aziende$media_rank), ], 10))

cat("\n== Aziende distorte (sd rank >= 5000) ==\n")
print(head(outlier_aziende[order(-outlier_aziende$sd_rank), ], 10))

# Valutazione Mappa rischio-performance

ggplot(rank_summary, aes(x = sd_rank, y = media_rank)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = median(rank_summary$media_rank), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = median(rank_summary$sd_rank), linetype = "dashed", color = "red") +
  labs(title = "Mappa Rischio-Performance delle Imprese",
       x = "Incertezza tra ISP (SD rank)",
       y = "Posizione media (rank)") +
  theme_minimal()

# Clusterizzazione
kmeans_input <- rank_summary %>% select(media_rank, sd_rank)
set.seed(2025)
kmeans_input<-scale(kmeans_input)
cluster <- kmeans(kmeans_input, centers = 4)
rank_summary$cluster <- cluster$cluster

# Visualizzare i cluster nella mappa rischio-performance
ggplot(rank_summary, aes(x = sd_rank, y = media_rank, color = as.factor(cluster))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = median(rank_summary$media_rank), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = median(rank_summary$sd_rank), linetype = "dashed", color = "red") +
  labs(
    title = "Mappa Rischio-Performance con Cluster",
    x = "Deviazione Standard dei Rank (Incertezza)",
    y = "Media dei Rank (Performance)",
    color = "Cluster"
  ) +
  theme_minimal()

ggplot(rank_summary, aes(x = media_rank, y = sd_rank, color = factor(cluster))) +
  geom_point(alpha = 0.7, size = 3) +
  geom_vline(xintercept = median(rank_summary$media_rank), linetype = "dashed") +
  geom_hline(yintercept = median(rank_summary$sd_rank), linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Cluster di imprese su media e deviazione dei ranking",
    x = "Media ranking (performance)",
    y = "Deviazione standard (incertezza)",
    color = "Cluster"
  ) +
  theme_minimal()

# Seleziona e ristruttura i dati per i boxplot
df_plot <- df_isp %>%
  select(ends_with("_norm"), ends_with("_std")) %>%
  pivot_longer(
    everything(),
    names_to = "versione",
    values_to = "valore"
  ) %>%
  mutate(
    tipo = ifelse(str_detect(versione, "_norm$"), "Normalizzato", "Standardizzato"),
    versione = str_remove(versione, "_norm$|_std$")
  )

# 2. Boxplot con facet per tipo
ggplot(df_plot, aes(x = versione, y = valore, fill = versione)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  coord_flip() +
  facet_wrap(~ tipo, scales = "free_x") +
  labs(
    title = "Boxplot per versione normalizzata e standardizzata",
    x = "Versione ISP",
    y = "Valore",
    fill = "Versione"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# Sostituzione della variabile dipendente con la versione settoriale 
# in quanto specifica della multisettorialità della matrice
dataset_completo$ISP <- df_isp$ISP_sett_lasso_norm

# Aggiorno dataset_completo
cat("Salvataggio matrice ISP aggiornata...\n")
saveRDS(dataset_completo, file.path(output_dir, "matrice.rds"))

##### Indicatore di integrazione verticale à la buzzell####
cat("Calcolo integrazione verticale...\n")
dataset_completo %>%
  group_by(divisione) %>%
  summarise(
    ROA_medio = mean(ROA_percentuale, na.rm = TRUE),
    ROA_mediano = median(ROA_percentuale, na.rm = TRUE),
    sd_ROA = sd(ROA_percentuale, na.rm = TRUE),
    n = n()
  )

# Calcolo del ROA medio per settore
dataset_completo <- dataset_completo %>%
  group_by(divisione) %>%  # Usa direttamente la colonna "settore" (ATECO 2 digit)
  mutate(ROA_medio_settore = mean(ROA_percentuale, na.rm = TRUE)) %>%  
  ungroup()
# Sostituzione alla variabile dipendenti valore 0 con 1
dataset_completo$dipendenti[dataset_completo$dipendenti == 0] <- 1
# Calcolare il valore aggiunto in migl di EUR
dataset_completo <- dataset_completo %>%
  mutate(valore_aggiunto_migl_EUR = (valore_aggiunto_pro_capite_EUR * dipendenti) / 1e3)

# Calcolo dell'indicatore di integrazione verticale à la Buzzell
dataset_completo <- dataset_completo %>%
  mutate(
    numeratore = valore_aggiunto_migl_EUR - utile_netto_migl_EUR + (ROA_medio_settore * totale_attivita_migl_EUR),
    denominatore = ricavi_vendite_migl_EUR - utile_netto_migl_EUR + (ROA_medio_settore * totale_attivita_migl_EUR),
    integrazione_verticale = numeratore / denominatore
  ) %>%
  mutate(
    integrazione_verticale = ifelse(is.nan(integrazione_verticale) | is.infinite(integrazione_verticale), NA, integrazione_verticale)
  )

# Esportazione dei risultati
cat("Salvataggio dataset con integrazione verticale...\n")
write.xlsx(dataset_completo, file.path(output_dir, "dataset_con_integrazione_verticale_completo.xlsx"))
saveRDS(dataset_completo, file.path(output_dir, "matrice.rds"))

#visualizzazione
dataset_completo <- dataset_completo[order(dataset_completo$integrazione_verticale, decreasing = TRUE), ]

int<-dataset_completo %>%
  filter(integrazione_verticale > 1) %>%
  dplyr::select(ragione_sociale, integrazione_verticale, dimensione_impresa, macroarea, sezione) %>%
  head(6)
# Filtro e selezione colonne
osservazioni_filtrate <- dataset_completo %>%
  filter(integrazione_verticale > 1) %>%
  dplyr::select(ragione_sociale, integrazione_verticale, dimensione_impresa, macroarea, sezione)

# Tabella interattiva
datatable(osservazioni_filtrate, 
          options = list(pageLength = 10, scrollX = TRUE),
          caption = "Imprese con integrazione verticale > 1")
par(mfrow=c(1,3))
hist(dataset_completo$integrazione_verticale)
boxplot(dataset_completo$integrazione_verticale)
summary(dataset_completo$integrazione_verticale)
qqnorm(dataset_completo$integrazione_verticale, main = "Q-Q Plot: Integrazione Verticale")
qqline(dataset_completo$integrazione_verticale, col = "red", lwd = 2)

#confronti tra indicatori
par(mfrow = c(1, 1))

plot(dataset_completo$ISP, dataset_completo$integrazione_verticale,
     xlab = "ISP", ylab = "Integrazione verticale",
     main = "Scatterplot: ISP vs Integrazione verticale")

# Salvataggio dell'istogramma per l'Indicatore Sintetico
cat("Salvataggio istogramma Indicatore Sintetico...\n")
p1 <- ggplot(dataset_completo, aes(x = ISP)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribuzione dell'Indicatore Sintetico", x = "Indicatore Sintetico", y = "Frequenza")
ggsave(file.path(output_dir, "distribuzione_indice_sintetico.png"), plot = p1, width = 8, height = 6)

# Salvataggio dell'istogramma per l'Integrazione Verticale
cat("Salvataggio istogramma Integrazione Verticale...\n")
p2 <- ggplot(dataset_completo, aes(x = integrazione_verticale)) +
  geom_histogram(binwidth = 0.05, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribuzione Integrazione Verticale", x = "Integrazione Verticale", y = "Frequenza")
ggsave(file.path(output_dir, "distribuzione_integrazione_verticale.png"), plot = p2, width = 8, height = 6)

# Boxplot per l'Indice Sintetico per Settore
cat("Salvataggio boxplot Indice Sintetico per Settore...\n")
p3 <- ggplot(dataset_completo, aes(x = divisione, y = ISP)) +
  geom_boxplot() +
  labs(title = "Distribuzione dell'Indice Sintetico per Settore", x = "Settore", y = "Indice Sintetico")
ggsave(file.path(output_dir, "boxplot_indice_sintetico_per_settore.png"), plot = p3, width = 10, height = 6)

# Boxplot per l'Integrazione Verticale per Settore
cat("Salvataggio boxplot Integrazione Verticale per Settore...\n")
p4 <- ggplot(dataset_completo, aes(x = divisione, y = integrazione_verticale)) +
  geom_boxplot() +
  labs(title = "Distribuzione dell'Integrazione Verticale per Settore", x = "Settore", y = "Integrazione Verticale")
ggsave(file.path(output_dir, "boxplot_integrazione_verticale_per_settore.png"), plot = p4, width = 10, height = 6)
# Test di correlazione tra Indice Sintetico e Integrazione Verticale
cat("Test di correlazione tra ISP e integrazione verticale...\n")
cor_test_result <- cor.test(scale(dataset_completo$ISP),scale( dataset_completo$integrazione_verticale), use = "complete.obs")
# Estrazione dei risultati dal test di correlazione
cor_test_result_df <- data.frame(
  correlazione = cor_test_result$estimate,
  p_value = cor_test_result$p.value,
  conf_int_lower = cor_test_result$conf.int[1],
  conf_int_upper = cor_test_result$conf.int[2]
)

# Calcolo e salvataggio della heatmap delle correlazioni
cat("Calcolo heatmap correlazioni...\n")
cor_matrix <- cor(dataset_completo[, c("ISP", "integrazione_verticale", "valore_aggiunto_migl_EUR", "ROA_percentuale")], use = "complete.obs")
heatmap_data <- melt(cor_matrix)

p5 <- ggplot(heatmap_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heatmap delle Correlazioni")
ggsave(file.path(output_dir, "heatmap_correlazioni.png"), plot = p5, width = 8, height = 6)

##### Statistiche descrittive & trasformazioni####
cat("Statistiche descrittive & trasformazioni...\n")
dataset_completo<-readRDS(file.path(output_dir, "matrice.rds"))

# Controllo della struttura
print(categorie_variabili)
par(mfrow = c(2, 3))
variabili <- c("ISP","integrazione_verticale")

for (variabile in variabili) {
  cat("\n### Variabile:", variabile, "\n")
  
  x <- na.omit(dataset_completo[[variabile]])
  
  unique_vals <- unique(x)
  is_discrete <- length(unique_vals) <= 10 && all(unique_vals == floor(unique_vals))
  
  if (is_discrete) {
    breaks <- seq(min(x) - 0.5, max(x) + 0.5, by = 1)
  } else {
    breaks <- "FD"
  }
  
  h <- hist(x,
            breaks = breaks,
            freq = TRUE,
            col = "lightblue",
            border = "white",
            main = paste("Istogramma di", variabile),
            xlab = variabile)
  
  if (!is_discrete) {
    d <- density(x)
    bin_width <- if (is.numeric(h$breaks)) diff(h$breaks[1:2]) else 1
    y_scaled <- d$y * length(x) * bin_width
    lines(d$x, y_scaled, col = "red", lwd = 2)
    legend("topright", legend = "Densità (scala conteggi)", col = "red", lwd = 2, bty = "n")
  }
  
  boxplot(x,
          horizontal = TRUE,
          main = paste("Boxplot di", variabile),
          col = "orange")
  
  qqnorm(x, main = paste("QQ Plot di", variabile))
  qqline(x, col = "blue", lwd = 2)
}

for (categoria in names(categorie_variabili)) {
  cat("\n## Categoria:", categoria, "\n")
  
  for (variabile in categorie_variabili[["Capitale_Circolante"]]) {
    cat("\n### Variabile:", variabile, "\n")
    
    x <- dataset_completo[[variabile]]
    
    # 1. Riconosci variabili discrete (<= 10 valori unici interi)
    unique_vals <- unique(x)
    is_discrete <- length(unique_vals) <= 10 && all(unique_vals == floor(unique_vals))
    
    if (is_discrete) {
      breaks <- seq(min(x) - 0.5, max(x) + 0.5, by = 1)
    } else {
      breaks <- "FD"  # Freedman–Diaconis
    }
    
    # 2. Istogramma
    h <- hist(x,
              breaks = breaks,
              freq = TRUE,
              col = "lightblue",
              border = "white",
              main = paste("Istogramma di", variabile),
              xlab = variabile)
    
    # 3. Densità: solo se variabile continua
    if (!is_discrete) {
      d <- density(x)
      bin_width <- if (is.numeric(h$breaks)) diff(h$breaks[1:2]) else 1
      y_scaled <- d$y * length(x) * bin_width  # Scala la densità
      
      lines(d$x, y_scaled, col = "red", lwd = 2)
      legend("topright", legend = "Densità (scala conteggi)", col = "red", lwd = 2, bty = "n")
    }
    
    # 4. Boxplot
    boxplot(x,
            horizontal = TRUE,
            main = paste("Boxplot"),
            col = "orange")
    
    # 5. QQ plot
    qqnorm(x)
    qqline(x, col = "blue", lwd = 2)
  }
}

summary(dataset_completo$ISP)

sum(is.na(dataset_completo$ISP))

dataset_completo <- na.omit(dataset_completo)

cat("Normalizzazione robusta ISP...\n")
bn_result <- bestNormalize(dataset_completo$ISP, standardize = FALSE)
dataset_completo$ISP_bn <- predict(bn_result)
print(bn_result)

par(mfrow = c(1, 3))
hist(dataset_completo$ISP, main = "Originale", xlab = "ISP")
hist(dataset_completo$ISP_bn, main = "OrderNorm", xlab = "ISP ordernorm")

qqnorm(dataset_completo$ISP_bn)
qqline(dataset_completo$ISP_bn, col = "red")
summary(dataset_completo$ISP)
sd(dataset_completo$ISP)
skewness(dataset_completo$ISP)   # asimmetria
kurtosis(dataset_completo$ISP)   # curtosi (eccesso)

##### Matrice correlazione tra variabili e nei settori####
cat("Matrice di correlazione tra variabili e nei settori...\n")
par(mfrow = c(1, 1)) 
categorie_variabili$Performance <- c("ISP_bn", "integrazione_verticale")
# Standardizza le variabili selezionate nel dataset (per ogni categoria)
dataset_z_score <- as.data.frame(scale(dataset_completo[, unlist(categorie_variabili)]))
# Matrice di correlazione
cor_matrix <- cor(dataset_z_score[, unlist(categorie_variabili)])
# Crea la heatmap della matrice di correlazione
pheatmap(cor_matrix, 
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         clustering_method = "complete", 
         display_numbers = TRUE, 
         main = "Matrice di Correlazione")


dataset_z_score$sezione <- dataset_completo$sezione  # Aggiunta la colonna 

# Filtra i gruppi con almeno 2 osservazioni per il calcolo della correlazione
cor_settori_interesse <- dataset_z_score %>%
  group_by(sezione) %>%
  filter(n() > 2) %>%  # Mantieni solo i gruppi con più di 2 osservazione
  summarise(
    cor_matrix = list(
      cor(across(where(is.numeric)), use = "pairwise.complete.obs")
    ),
    .groups = "drop"
  )

# Visualizza il risultato
print(cor_settori_interesse)
##### Controllo la significatività delle correlazioni tra variabili e settore####
cat("Controllo la significatività delle correlazioni tra variabili e settore...\n")
# Lista delle variabili da testare
vars_model <- unname(unlist(categorie_variabili))

# Funzione principale
confronto_settori_variabili <- function(dataset, variabili, variabile_settore = "sezione", soglia_normalita = 0.05, soglia_significativita = 0.05) {
  
  risultati <- map_dfr(variabili, function(var) {
    
    # Costruisci dataset temporaneo
    dati_var <- dataset %>%
      select(all_of(variabile_settore), !!sym(var)) %>%
      filter(!is.na(.[[var]])) %>%
      group_by(!!sym(variabile_settore)) %>%
      mutate(n_settore = n()) %>%
      ungroup() %>%
      filter(n_settore >= 3)  # il test di Shapiro richiede almeno 3 osservazioni
    
    # Se ci sono abbastanza dati
    if (n_distinct(dati_var[[variabile_settore]]) < 2) {
      return(tibble(variabile = var, test = NA, p_value = NA, significativo = NA))
    }
    
    # Test di normalità per ogni settore
    normalita <- dati_var %>%
      group_by(!!sym(variabile_settore)) %>%
      summarise(p_shapiro = tryCatch(shapiro.test(.[[var]])$p.value, error = function(e) NA), .groups = "drop")
    
    tutti_normali <- all(normalita$p_shapiro > soglia_normalita, na.rm = TRUE)
    
    # Test globale (ANOVA o Kruskal-Wallis)
    formula_test <- as.formula(paste(var, "~", variabile_settore))
    test_risultato <- tryCatch({
      if (tutti_normali) {
        res <- aov(formula_test, data = dati_var)
        p_val <- summary(res)[[1]][["Pr(>F)"]][1]
        list(test = "ANOVA", p_value = p_val)
      } else {
        res <- kruskal.test(formula_test, data = dati_var)
        list(test = "Kruskal-Wallis", p_value = res$p.value)
      }
    }, error = function(e) list(test = NA, p_value = NA))
    
    # Ritorna riga di risultati
    tibble(
      variabile = var,
      test = test_risultato$test,
      p_value = test_risultato$p_value,
      significativo = ifelse(!is.na(test_risultato$p_value) & test_risultato$p_value < soglia_significativita, "Significativo", "Non significativo")
    )
  })
  
  return(risultati)
}

#  Applicazione funzione
cat("Applico il test di confronto tra variabili e settori...\n")
risultati_test_settore <- confronto_settori_variabili(dataset = dataset_completo, variabili = vars_model)

# Esporta in Excel
cat("Salvataggio risultati test settore...\n")
write.xlsx(risultati_test_settore, file.path(output_dir, "risultati_test_settore.xlsx"))

# Visualizza risultati significativi
risultati_test_settore %>%
  filter(significativo == "Significativo") %>%
  ggplot(aes(x = reorder(variabile, p_value), y = p_value, fill = test)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(title = "Variabili con differenze significative tra settori",
       x = "Variabile", y = "p-value", fill = "Test usato") +
  theme_minimal()

##### Visualizzazione grafica dei dati ####
cat("Visualizzazione grafica e mappe interattive...\n")
## Creazione Mappa Iterattiva 
# Verifico se ci sono NA nelle colonne latitudine e longitudine
dataset_map <- dataset_completo[!is.na(dataset_completo$longitudine) & !is.na(dataset_completo$latitudine), ]
attach(dataset_map)

# Parto sempre da sf in UTM -> trasformo -> estraggo le coordinate in 4326
ripgeo <- st_read(here("data", "RipGeo01012025_WGS84.shp"))

breaks <- c(min(ISP_bn), mean(ISP_bn), max(ISP_bn))  # Classi personalizzate

# Creiamo una palette con transizione graduale tra i colori
pal <- colorBin(
  palette = c("darkblue", "blue", "white", "pink", "darkred"), 
  domain = dataset_map$ISP_bn, 
  bins = breaks
)
# Crea la mappa con i marker colorati in base ai ricavi, e popup con informazioni aggiuntive
mappa <- leaflet(dataset_map) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitudine, 
    lat = ~latitudine, 
    radius = ISP_bn, 
    color = ~pal(ISP_bn),  # Applica la palette di colori 
    stroke = FALSE, 
    fillOpacity = 0.8,
    popup = ~paste(
      "Azienda: ", ragione_sociale, "<br>", 
      "Ricavi: ", ricavi_vendite_migl_EUR, " Migliaia EUR", "<br>",
      "EBITDA: ", EBITDA_migl_EUR, " Migliaia EUR", "<br>",
      "Indice sintetico Performance: ", ISP_bn, "<br>",
      "Settore: ", ateco_2007_descrizione  
    )
  ) %>%
  addLegend("bottomright", pal = pal, values = ~ricavi_vendite_migl_EUR,
            title = "ISP", opacity = 1)

# Visualizza la mappa
mappa

breaks <- c(min(utile_netto_migl_EUR), -5000, -1000, -100, 0, 100, 1000, 5000, max(utile_netto_migl_EUR))  # Classi personalizzate

# Creiamo una palette con transizione graduale tra i colori
pal <- colorBin(
  palette = c("darkblue", "blue", "lightblue", "white", "pink", "red", "darkred"), 
  domain = dataset_map$utile_netto_migl_EUR, 
  bins = breaks
)
# Creiamo la mappa
mappa <- leaflet(dataset_map) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitudine, 
    lat = ~latitudine, 
    radius = 1,  # Aumentato per visibilità
    color = ~pal(utile_netto_migl_EUR),  # Colore in base all'utile netto
    stroke = FALSE, 
    fillOpacity = 0.8,
    popup = ~paste(
      "Azienda: ", ragione_sociale, "<br>", 
      "Ricavi: ", ricavi_vendite_migl_EUR, " Migliaia EUR", "<br>",
      "EBITDA: ", EBITDA_migl_EUR, " Migliaia EUR", "<br>",
      "Utile Netto: ", utile_netto_migl_EUR, " Migliaia EUR", "<br>",  # Qui ho corretto il nome della variabile
      "Settore: ", ateco_2007_descrizione  
    )
  ) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = dataset_map$utile_netto_migl_EUR,  # Uso l'utile netto per la legenda
            title = "Utile/Perdita (in migliaia EUR)", 
            opacity = 1)

# Mostra la mappa
mappa

# Mappa clustering
# Crea una palette basata su bin
pal_custom <- colorBin(
  palette = "RdYlBu", 
  domain = dataset_map$ricavi_vendite_migl_EUR,
  bins = c(min(dataset_map$ricavi_vendite_migl_EUR), 100000, 500000, 1000000, 5000000, max(dataset_map$ricavi_vendite_migl_EUR))  # Limiti personalizzati
)
# Creazione della mappa con i marker colorati per bin e clustering per regione
mappa_bin <- leaflet(dataset_map) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitudine, 
    lat = ~latitudine, 
    radius = 1,  # Modifica la dimensione dei marker
    color = ~pal_custom(ricavi_vendite_migl_EUR),
    stroke = FALSE, 
    fillOpacity = 0.8,
    popup = ~paste(
      "<b>Azienda: </b>", ragione_sociale, "<br>", 
      "<b>Settore: </b>", ateco_2007_descrizione, "<br>",
      "<b>Ricavi: </b>", ricavi_vendite_migl_EUR, " Migliaia EUR", "<br>",
      "<b>EBITDA: </b>", EBITDA_migl_EUR, " Migliaia EUR", "<br>",
      "<b>Dimensione Aziendale: </b>", dimensione_impresa
    )) %>%
  addLegend("bottomright", pal = pal_custom, values = ~ricavi_vendite_migl_EUR,
            title = "Ricavi (in migliaia EUR)", opacity = 1) %>%
  setView(lng = mean(dataset_map$longitudine, na.rm = TRUE), 
          lat = mean(dataset_map$latitudine, na.rm = TRUE), zoom = 6)

# Visualizza la mappa
mappa_bin

cat("Salvataggio finale matrice RDS...\n")
saveRDS(dataset_completo, file.path(output_dir, "matrice.rds"))
saveRDS(categorie_variabili, file.path(output_dir, "categorie_variabili.rds"))


cat(paste0("\n========== FINE FASE: ", fase_nome, " ==========\n"))

# Rimuovi tutte le variabili tranne dataset, dataset_ridotto e dataset_completo
rm(list = setdiff(ls(), c( "dataset_completo","categorie_variabili","vars_model")))

# Verifica che solo le variabili desiderate siano rimaste
ls()
