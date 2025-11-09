# =============================================================
# ANALISI SPAZIALE ISP FASE – VERSIONE PULITA (macroarea × dimensione)
# =============================================================
# Requisiti: oggetti file presenti nella working dir:
# - "matrice.rds" (dataset completo)
# - "vars_model_stepwise.rds" e "vars_per_dim_stepwise.rds" (se usati)
# - "Com01012025_WGS84.shp" e "RipGeo01012025_WGS84.shp" (confini)
# Output in sottocartelle 01_rds / 02_maps / 03_tables / 04_html / 05_logs
# =============================================================

# -------------------------------------------------------------
# 0. LIBRERIE E CARTELLE
# -------------------------------------------------------------
suppressPackageStartupMessages({
  libs <- c(
    "car","classInt","dplyr","ggplot2","kableExtra","leaflet","lmtest",
    "maptools","mapview","moments","nortest","patchwork","RColorBrewer",
    "readxl","rcompanion","sf","spData","spdep","spatialreg","stargazer",
    "tripack","FNN","future","future.apply","GWmodel","spgwr",
    "htmlwidgets","viridis","tmap","purrr","tidyr","stringr","pandoc"
  )
  invisible(lapply(libs, require, character.only = TRUE))
})

root_dir <- here::here()
input_dir <- here::here("04_econometric_models")
output_dir <- here::here("05_analysis_spatial")
data_dir <- here::here("data") # Percorso alla cartella che contiene i file SHP
# Definisco le sottodirectory
dir_rds <- "01_rds"; dir_maps <- "02_maps"; dir_tbl <- "03_tables"; dir_html <- "04_html"; dir_logs <- "05_logs"
for (d in c(dir_rds, dir_maps, dir_tbl, dir_html, dir_logs)) {
  if (!dir.exists(file.path(output_dir, d))) dir.create(file.path(output_dir, d), recursive = TRUE)
}
# Richiamo file Utility

source(here::here("suddivisione_script", "utils_spatial.R"))

# -------------------------------------------------------------
# Wrapper salvataggi + log (UNIFORMATI)
# -------------------------------------------------------------
save_plot <- function(plot, name, w=8, h=6, dpi=300, ext="png")
  ggsave(file.path(output_dir, "02_maps", paste0(name, ".", ext)), plot=plot, width=w, height=h, dpi=dpi)

save_pdf  <- function(plot, name, w=8, h=6)
  ggsave(file.path(output_dir, "02_maps", paste0(name, ".pdf")), plot=plot, width=w, height=h)

save_html <- function(widget, name)
  htmlwidgets::saveWidget(widget, file.path(output_dir, "04_html", paste0(name, ".html")), selfcontained=TRUE)

save_rds  <- function(obj, name) saveRDS(obj, file.path(output_dir, "01_rds", paste0(name, ".rds")))

save_table <- function(df, name, row.names=FALSE)
  write.csv(df, file.path(output_dir, "03_tables", paste0(name, ".csv")), row.names=row.names)

open_log <- function(name) {
  logfile <- file.path(output_dir, "05_logs", paste0(name, ".txt"))
  sink(logfile, split = TRUE)
  cat("=== LOG:", name, "===\n")
  cat("Data/Ora avvio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
}
close_log <- function() sink(NULL)

# -------------------------------------------------------------
# 1. IMPORT DATI E PREPARAZIONE
# -------------------------------------------------------------
# Logica Condizionale per l'Input
if (!exists("dataset_completo") ||
    !("ISP" %in% names(dataset_completo))) {
  # Carica il dataset finale con gli ISP solo se non è in memoria
  # CORREZIONE: Usa input_dir
  dataset_completo <- readRDS(file.path(input_dir, "matrice.rds"))
}
# (opzionale) variabili stepwise
# CORREZIONE: Usa input_dir per file.exists e readRDS
vars_model   <- if (file.exists(file.path(input_dir, "vars_model_stepwise.rds"))) readRDS(file.path(input_dir, "vars_model_stepwise.rds")) else NULL # nolint
vars_per_dim <- if (file.exists(file.path(input_dir, "vars_per_dim_stepwise.rds"))) readRDS(file.path(input_dir, "vars_per_dim_stepwise.rds")) else NULL # nolint

# Logica di controllo per l'analisi spaziale:
if (is.null(vars_model)) {
  stop("ERRORE: Impossibile trovare 'vars_model_stepwise.rds'.
   Eseguire prima 'modelli econometrici.R'.") # nolint
}

# Prepara lista effettiva di variabili da scalare (se presenti)
numeric_vars <- names(dataset_completo)[sapply(dataset_completo, is.numeric)]
all_vars <- unique(na.omit(c(vars_model)))
vars_to_scale <- all_vars[all_vars %in% numeric_vars] # Seleziona solo le variabili numeriche che sono nel tuo modello
 

# Z-score variabili selezionate + ISP_bn separatamente
if (length(vars_to_scale) > 0) {
  dataset_z <- as.data.frame(scale(dataset_completo[vars_to_scale]))
} else {
  # Se non ci sono variabili da scalare, crea un dataframe vuoto
  dataset_z <- dataset_completo[NULL]
}
# ISP_bn (variabile dipendente scalata) e le altre colonne di dataset_z
dataset_z$ISP_bn <- as.numeric(dataset_completo$ISP_bn)  # evita l'effetto matrix di scale()

# Mantieni colonne descrittive non scalate
cols_keep <- c("macroarea","codice_fiscale","sezione","dimensione_impresa",
               "longitudine","latitudine")
# Unione in unico datset_z
for(cl in cols_keep){
  if (cl %in% names(dataset_completo)) {
    dataset_z[[cl]] <- dataset_completo[[cl]]
  } else {
    warning(paste("Colonna mancante nel dataset_completo:", cl))
  }
}

# Conversione sf + CRS metrico
imprese_sf   <- st_as_sf(dataset_z, coords=c("longitudine","latitudine"), crs=4326)
imprese_sf_tr<- st_transform(imprese_sf, 32632)

# Join con confini
comuni <- st_read(file.path(data_dir, "Com01012025_WGS84.shp"), quiet=TRUE)
comuni_tr <- st_transform(comuni, 32632)
ripgeo <- st_read(file.path(data_dir, "RipGeo01012025_WGS84.shp"), quiet=TRUE)
ripgeo_tr <- st_transform(ripgeo, st_crs(imprese_sf_tr))
imprese_sf_tr <- st_join(imprese_sf_tr, comuni_tr, join=st_within)
imprese_sf_tr_joined <- st_join(imprese_sf_tr, ripgeo_tr, join=st_within)
save_rds(imprese_sf_tr, "imprese_sf_tr")

# -------------------------------------------------------------
# 2. MAPPA DESCRITTIVE (opzionali)
# -------------------------------------------------------------
p0 <- ggplot() +
  # Usiamo ripgeo_tr come sfondo. Colour="black" per bordi più visibili.
  geom_sf(data=ripgeo_tr, fill="grey90", colour="black", linewidth=.3) +
  # Aggiungi le imprese
  geom_sf(data=imprese_sf_tr, aes(colour=dimensione_impresa), size=.10, alpha=.8) +
  scale_colour_viridis_d(option="D", name="Dimensione\nimpresa") +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  # coord_sf assicura che il CRS sia gestito correttamente
  coord_sf(crs = st_crs(imprese_sf_tr)) + 
  theme_minimal(base_size=11) +
  labs(title="Imprese italiane per classe dimensionale") +
  theme(panel.grid=element_blank())

save_pdf(p0, "mappa_imprese_macroarea_final")

# -------------------------------------------------------------
# 3. ANALISI k OTTIMALE (NAZIONALE) + W nazionale
# -------------------------------------------------------------
open_log("analisi_k_nazionale")

# jitter per duplicati
imprese_sf_jit <- jitter_coords(imprese_sf_tr, 1e-4)
coords_jit     <- st_coordinates(imprese_sf_jit)

# curva distanza ai k-vicini
a <- nrow(imprese_sf_jit)
max_k <- max(1, floor(sqrt(a) * 0.5))
knn_res <- FNN::get.knn(coords_jit, k = max_k)
dist_median <- apply(knn_res$nn.dist, 2, median)
dist_mean   <- apply(knn_res$nn.dist, 2, mean)
dist_sd     <- apply(knn_res$nn.dist, 2, sd)
plot_df <- data.frame(k = 1:max_k, mean = dist_mean, median = dist_median, sd = dist_sd)

p_kdist <- ggplot(plot_df) +
  geom_line(aes(k, mean,   color="Media"),   linewidth = .6) +
  geom_line(aes(k, median, color="Mediana", linetype="Mediana"), linewidth = .6) +
  geom_line(aes(k, sd,     color="Deviazione standard", linetype="Deviazione standard"), linewidth = .6) +
  scale_color_manual(values = c("Media"="blue","Mediana"="darkgreen","Deviazione standard"="red")) +
  labs(title="Distanza ai k-vicini", y="Distanza", x="k", color="Statistiche") +
  theme_minimal()
save_pdf(p_kdist, "plot_k_distanza")

# validazione Moran vs k (parallelo)
ris_k <- validazione_moransI_k_parallel(imprese_sf_jit)
save_table(ris_k, "k_moran")

p_kmor <- ggplot(ris_k, aes(k, moran_i, color=p<.05))+
  geom_line() + geom_point() +
  scale_color_manual(values=c("TRUE"="green","FALSE"="red")) +
  theme_minimal() + labs(title="Moran's I vs k", color="Significativo")
save_pdf(p_kmor, "plot_k_moran")

# criterio combinato unico
tmp <- scegli_k_knn(imprese_sf_jit, k_min=5, k_max=max_k, step=2)
k_opt <- tmp$best_k
cat("\nk ottimale selezionato:", k_opt, "\n")
close_log()

p_k   <- plot_scelta_k(tmp)
save_pdf(p_k, "plot scelta k")
open_log("matrice")
knn_list <- spdep::knearneigh(coords_jit, k = k_opt)
(knn_nb     <- spdep::knn2nb(knn_list))
(listw_knn  <- spdep::nb2listw(knn_nb, style="W", zero.policy=TRUE))
save_rds(listw_knn, "listw_knn")

close_log()

# 1. Definisci il file di output per la mappa
pdf_file <- file.path(output_dir, "02_maps", "grafo_vicinato_nazionale_migliorato.pdf")
k_grafo <- k_opt

tryCatch({
  pdf(pdf_file, width=10, height=12)
  par(mar=c(0, 0, 3, 0), bg="white")
  
  # Colore archi semi-trasparente (rosso scuro)
  col_archi <- grDevices::rgb(153, 0, 13, maxColorValue=255, alpha=80)
  
  # 1) Confini amministrativi (prima, senza riempimento)
  plot(sf::st_geometry(ripgeo),
       border="grey45", col=NA, lwd=0.4,
       axes=FALSE)
  
  # 2) Punti imprese (molto leggeri)
  # Questi ora saranno visibili perché non coperti dai cerchi neri
  plot(imprese_sf_jit$geometry,
       add=TRUE, pch=6, cex=0.05, col="gray70")
  
  # 3) Archi del grafo KNN (sottili + trasparenza)
  coords_matrix <- sf::st_coordinates(imprese_sf_jit)
  
  # --- MODIFICA QUI ---
  # Aggiungi 'points = FALSE' per rimuovere i cerchi neri invadenti
  plot(listw_knn, coords_matrix,
       add=TRUE, col=col_archi, lwd=0.1,
       points = FALSE) # <--- AGGIUNTA
  # --- FINE MODIFICA ---
  
  # Titolo
  title(main=paste0("Grafo di Vicinato Spaziale (k=", k_grafo, ") - Nazionale"),
        cex.main=1.1)
  
}, finally = { if (dev.cur() != 1) { dev.off() } })
cat("\n✅ Grafo di Vicinato Nazionale salvato in:", file.path(output_dir, "02_maps", "grafo_vicinato_nazionale.pdf"), "\n")

# -------------------------------------------------------------
# 4. AUTOCORRELAZIONE GLOBALE: calcola e salva statistiche di autocorrelazione
#          spaziale (Moran, Geary, LISA, Gi*) e relative mappe e tabelle
# -------------------------------------------------------------
open_log("autocorrelazione_globale")

var_int <- as.numeric(imprese_sf_jit$ISP_bn)

# Moran e Geary
mt  <- spdep::moran.test(var_int, listw_knn, zero.policy=TRUE); print(mt)
ge  <- spdep::geary.test(var_int, listw_knn, zero.policy=TRUE); print(ge)

# Moran Montecarlo (diagnostica addizionale)
mc  <- spdep::moran.mc(var_int, listw_knn, nsim=999, zero.policy=TRUE); print(mc)

# Scatterplot
pdf(file.path(output_dir,"02_maps","moran_scatterplot_knn.pdf"), width=8, height=6)
par(mar=c(4,4,2,1))
spdep::moran.plot(var_int, listw_knn, zero.policy=TRUE,
                  pch=20, col="#3182bd", cex=.2, labels=FALSE)
abline(h=0, v=0, lty=2); dev.off()

# --- STILE 1: smoothScatter ---
pdf(file.path(output_dir,"02_maps","moran_scatterplot_smooth.pdf"), width=8, height=7)
par(mar=c(4.5, 4.5, 3, 2)) # Margini per etichette

# Dati già calcolati (dalle righe 291-292 del tuo script)
x_val <- imprese_sf_jit$ISP_std 
y_val <- imprese_sf_jit$lag_ISP_std

# Paletta di colori (bianco -> blu)
pal <- colorRampPalette(c("white", "#B0E0E6", "#3182bd", "#08306B"))

# Crea il plot di densità
smoothScatter(x_val, y_val, 
              colramp = pal,        # Usa la nostra paletta
              nrpoints = 1000,      # Mostra i 1000 punti più "outlier"
              pch = 19, cex = 0.3, col = "grey50", # Stile degli outlier
              main = "Moran Scatterplot (Densità)",
              xlab = "ISP Standardizzato (z-score)",
              ylab = "Lag Spaziale ISP (z-score)")

# Aggiungi gli assi a zero
abline(h=0, v=0, lty=2, col="black") 

# Aggiungi la linea di regressione (Moran's I)
m <- lm(y_val ~ x_val) 
abline(m, col="#ca0020", lty=1, lwd=2) # Linea rossa più visibile

dev.off()
# --- FINE STILE 1 ---
# --- STILE 2: ggplot2 (hexbin) ---
# Assicurati che ggplot2 sia caricato
library(ggplot2)
library(viridis) # Per una paletta di colori robusta

# Prepara il dataframe per ggplot
plot_data <- data.frame(
  ISP_std     = imprese_sf_jit$ISP_std,     # Dalla riga 291
  lag_ISP_std = imprese_sf_jit$lag_ISP_std  # Dalla riga 292
)

# Crea il plot
p_moran_hex <- ggplot(plot_data, aes(x = ISP_std, y = lag_ISP_std)) +
  # Binning esagonale: "bins = 80" crea 80 esagoni sull'asse x
  geom_hex(bins = 80) + 
  
  # Assi a zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  
  # Linea di regressione (Moran's I)
  geom_smooth(method = "lm", se = FALSE, color = "#ca0020", linewidth = 0.8) +
  
  # Scala di colori (cividis o magma sono ottime)
  scale_fill_viridis_c(option = "magma", name = "Conteggio\nPunti", trans = "log10") +
  
  labs(title = "Moran Scatterplot (Hexbin Density)",
       x = "ISP Standardizzato (z-score)",
       y = "Lag Spaziale ISP (z-score)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank()) # Rimuove la griglia di sfondo

# Salva il plot
ggsave(file.path(output_dir,"02_maps","moran_scatterplot_hexbin.pdf"), 
       plot = p_moran_hex, width = 8.5, height = 7)
# --- FINE STILE 2 ---
install.packages("hexbin")
# LISA nazionale
lisa   <- spdep::localmoran(var_int, listw_knn)
lag_ISP<- spdep::lag.listw(listw_knn, var_int)
imprese_sf_jit <- imprese_sf_jit %>%
  mutate(
    lisa_p  = lisa[,5],
    lisa_I  = lisa[,1],
    ISP_std     = as.numeric(scale(ISP_bn)),
    lag_ISP_std = as.numeric(scale(lag_ISP)),
    LISA_cluster = case_when(
      lisa_p <= .05 & ISP_std > 0 & lag_ISP_std > 0 ~ "High-High",
      lisa_p <= .05 & ISP_std < 0 & lag_ISP_std < 0 ~ "Low-Low",
      lisa_p <= .05 & ISP_std > 0 & lag_ISP_std < 0 ~ "High-Low",
      lisa_p <= .05 & ISP_std < 0 & lag_ISP_std > 0 ~ "Low-High",
      TRUE ~ "Non signif"
    ) %>% factor(levels=c("High-High","Low-Low","High-Low","Low-High","Non signif"))
  )

# Quota osservazioni significative (diagnostica aggiuntiva)
cat("Quota cluster significativi:", round(mean(imprese_sf_jit$lisa_p<=0.05)*100,2), "%\n")
# Conteggi diagnostici Gi*
print(table(imprese_sf_jit$LISA_cluster))

# Mappe
p_lisa_nat <- plot_lisa(imprese_sf_jit, ripgeo, "LISA – Nazionale")
save_pdf(p_lisa_nat, "lisa_cluster_nazionale")

# Gi* nazionale
Gi <- spdep::localG(var_int, listw_knn, zero.policy=TRUE)
imprese_sf_jit$Gi <- as.numeric(Gi)
imprese_sf_jit$Gi_bin <- cut(imprese_sf_jit$Gi,
                             breaks=c(-Inf,-1.96,-1.65,1.65,1.96,Inf),
                             labels=c("Coldspot 99%","Coldspot 95%","Non signif","Hotspot 95%","Hotspot 99%"))


# Conteggi diagnostici Gi*
print(table(imprese_sf_jit$Gi_bin))

p_gi_nat <- plot_gi(imprese_sf_jit, ripgeo, "Gi* – Nazionale")
save_pdf(p_gi_nat, "gi_star_nazionale")

# Leaflet preview (opzionali)
leaflet_lisa_map(imprese_sf_jit, "mappa_cluster_completa")
leaflet_gi_map(imprese_sf_jit,   "mappa_gi_completa")

# Salvataggi
save_rds(imprese_sf_jit, "imprese_sf_jit")  # con LISA/Gi*
save_table(st_drop_geometry(imprese_sf_jit), "imprese_con_lisa_gi") # tabella completa
close_log()

# Richiede risorse computazionali non disponibili
#  (allocazione vettore di dimensione 4.7Gb)
# SAR/SDM/GMM/Impacts
#reg_res <- analisi_regressiva_spaziale(imprese_sf_jit, listw_knn, vars_model)

#dir.create(file.path(output_dir,"03_tables"), recursive=TRUE, showWarnings=FALSE)

# Output modelli
#capture.output(summary(reg_res$sar), file=file.path(output_dir,"03_tables", "sar.txt"))
#capture.output(summary(reg_res$sdm), file=file.path(output_dir,"03_tables", "sdm.txt"))
#if (!is.null(reg_res$impacts)) {
#  capture.output(reg_res$impacts, file=file.path(output_dir,"03_tables", "impacts_sdm.txt"))
#}
#capture.output(summary(reg_res$gmm), file=file.path(output_dir,"03_tables", "gmm.txt"))

# Diagnostiche (se hai fatto la funzione diagnostiche_spaziali)
#diag_sar <- diagnostiche_spaziali(reg_res$sar, listw_knn)
#diag_sdm <- diagnostiche_spaziali(reg_res$sdm, listw_knn)

#capture.output(diag_sar, file=file.path(output_dir,"03_tables", "diagn_sar.txt"))
#capture.output(diag_sdm, file=file.path(output_dir,"03_tables", "diagn_sdm.txt"))

# Salvataggio oggetto completo
#saveRDS(reg_res, file.path(output_dir,"01_rds", "regressioni_sar_sdm_gmm.rds"))
#saveRDS(diag_sar, file.path(output_dir,"01_rds", "diagnostiche_sar.rds"))
#saveRDS(diag_sdm, file.path(output_dir,"01_rds", "diagnostiche_sdm.rds"))  
# GWR (opzionale, se vuoi)
#gwr_res <- run_gwr(imprese_sf_jit, vars_model, samp_size=5000, seed=123)
#saveRDS(gwr_res, file.path(output_dir,"01_rds", "gwr_results.rds"))
#p_gwr <- plot_gwr_coef(gwr_res$coeff_sf, coef_name="inter cept", ripgeo,
#                       title="GWR Coefficiente Intercetta", legend=TRUE)  
#save_pdf(p_gwr, "gwr_intercept")

# -------------------------------------------------------------
# 6. LOOP AREA × DIMENSIONE (STRICT / BORDER)
#
# Esecuzione Analisi Macroarea*dimensione (es. "Nord-Ovest" × "Media")####
#---------------------------------------------------------------------------
results_all <- list()
areas <- unique(imprese_sf_tr$macroarea)
if (is.factor(imprese_sf_tr$dimensione_impresa)) {
  dims <- levels(imprese_sf_tr$dimensione_impresa)
}
cat("Livelli Macroaree (areas):\n"); print(areas)
cat("Livelli Dimensione Impresa (dims):\n"); print(dims)

open_log("analisi_dimensione_macroarea")

for (a in areas) {
  for (d in dims) {
    nm <- paste0(d, "_", gsub(" ", "_", a))
    cat("\n=== Analisi:", a, "×", d, "===\n")
    sf_all_dimensione <- imprese_sf_tr %>% 
      filter(dimensione_impresa == d)
    
    sf_sub <- imprese_sf_tr %>% filter(macroarea==a, dimensione_impresa==d)
    n_sub  <- nrow(sf_sub)
    if (n_sub < 30) {
      message("Campione troppo piccolo (", n_sub, "), salto.")
      next
    }
    
    set.seed(1234)
    
    sf_sub <- jitter_coords(sf_sub, 1e-4)
    sf_sub <- sf_sub %>% filter(!is.na(ISP_bn))
    n_sub <- nrow(sf_sub) # Aggiorna n_sub dopo la rimozione
    # k ottimale sul subset (criterio unico)
    k_res   <- scegli_k_knn(sf_sub, k_min=10, k_max=k_opt, step=2)
    k_best  <- min(k_res$best_k, n_sub-1)
    cat("k* =", k_best, "\n")
    
    # Matrici W (unificate)
    Wv <- build_W_variants(sf_all_dimensione, sf_sub, k_best)
    
    # Moran globali
    mor_s <- moran.test(sf_sub$ISP_bn, Wv$W_strict, zero.policy=TRUE)
    mor_b <- moran.test(sf_sub$ISP_bn, Wv$W_border, zero.policy=TRUE)
    
    # LISA per 3 varianti
    lisa_s <- localmoran(sf_sub$ISP_bn, Wv$W_strict, zero.policy=TRUE)
    lisa_b <- localmoran(sf_sub$ISP_bn, Wv$W_border, zero.policy=TRUE)
    
    # Aggiunte per mappe (STRICT come riferimento)
    add_lisa_cols <- function(sfx, lisa_mat, lw) {
      lagx <- spdep::lag.listw(lw, sfx$ISP_bn, zero.policy = TRUE)  # <- QUI
      sfx$ISP_std     <- as.numeric(scale(sfx$ISP_bn))
      sfx$lag_ISP_std <- as.numeric(scale(lagx))
      cl <- rep("Non signif", nrow(sfx))
      p  <- as.numeric(lisa_mat[,5]); Ii <- as.numeric(lisa_mat[,1])
      cl[Ii>0 & sfx$ISP_std>0 & sfx$lag_ISP_std>0 & p<=.05] <- "High-High"
      cl[Ii>0 & sfx$ISP_std<0 & sfx$lag_ISP_std<0 & p<=.05] <- "Low-Low"
      cl[Ii<0 & sfx$ISP_std>0 & sfx$lag_ISP_std<0 & p<=.05] <- "High-Low"
      cl[Ii<0 & sfx$ISP_std<0 & sfx$lag_ISP_std>0 & p<=.05] <- "Low-High"
      sfx$LISA_cluster <- factor(cl, levels=c("High-High","Low-Low","High-Low","Low-High","Non signif"))
      sfx
    }
    # Aggiunte per mappe (STRICT come riferimento)
    sf_s <- add_lisa_cols(sf_sub, lisa_s, Wv$W_strict)
    sf_s$Gi <- as.numeric(localG(sf_s$ISP_bn, Wv$W_strict, zero.policy=TRUE))
    sf_s$Gi_bin <- cut(sf_s$Gi,
                       breaks=c(-Inf,-1.96,-1.65,1.65,1.96,Inf),
                       labels=c("Coldspot 99%","Coldspot 95%","Non signif","Hotspot 95%","Hotspot 99%"))
    # BORDER
    sf_b <- add_lisa_cols(sf_sub, lisa_b, Wv$W_border)  # <- usa lisa_b
    sf_b$Gi <- as.numeric(localG(sf_b$ISP_bn, Wv$W_border, zero.policy=TRUE))
    sf_b$Gi_bin <- cut(sf_b$Gi,
                       breaks=c(-Inf,-1.96,-1.65,1.65,1.96,Inf),
                       labels=c("Coldspot 99%","Coldspot 95%","Non signif","Hotspot 95%","Hotspot 99%"))
    
    
    
    # Mappe (STRICT e BORDER sempre)
    dir.create(file.path(output_dir,"02_maps", nm), showWarnings = FALSE, recursive = TRUE)
    save_pdf(plot_lisa(sf_s, ripgeo, titolo=paste("LISA – STRICT –", nm)), file.path(nm, "lisa_strict"))
    save_pdf(plot_gi  (sf_s, ripgeo, titolo=paste("Gi* – STRICT –",  nm)), file.path(nm, "gi_strict"))
    save_pdf(plot_lisa(sf_b, ripgeo, titolo=paste("LISA – BORDER –", nm)), file.path(nm, "lisa_border"))
    save_pdf(plot_gi  (sf_b, ripgeo, titolo=paste("Gi* – BORDER –",  nm)), file.path(nm, "gi_border"))
    
    
    # Salvataggi risultati
    dir.create(file.path(output_dir,"01_rds", nm), showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(output_dir,"03_tables", nm), showWarnings = FALSE, recursive = TRUE)
    
    saveRDS(list(W_strict=Wv$W_strict, W_border=Wv$W_border,
                 k_strict=Wv$k_strict),
            file.path(output_dir,"01_rds", nm, "matrici_W.rds"))
    capture.output(print(mor_s), file=file.path(output_dir,"03_tables", nm, "moran_strict.txt"))
    capture.output(print(mor_b), file=file.path(output_dir,"03_tables", nm, "moran_border.txt"))
     
    saveRDS(list(lisa_strict=lisa_s, lisa_border=lisa_b),
            file.path(output_dir,"01_rds", nm, "lisa_results.rds"))
    saveRDS(sf_s, file.path(output_dir,"01_rds", nm, "sf_strict_with_stats.rds"))
    invisible(make_clusters_tidy(nm, alpha = 0.05))
    # Cerchiamo i 5 elementi chiave che build_W_variants produce
    edges_obj_names <- c("edges_strict_internal", "edges_border_internal",
                         "edges_external_border", # <-- Aggiunto
                         "coords_sub", "coords_all")
    
    edges_obj <- intersect(names(Wv), edges_obj_names)
    
    # La condizione ora è 5
    if (length(edges_obj) == 5) { 
      saveRDS(list(
        edges_strict_internal = Wv$edges_strict_internal,
        edges_border_internal = Wv$edges_border_internal,
        edges_external_border = Wv$edges_external_border, # <-- Aggiunto
        coords_sub = Wv$coords_sub,
        coords_all = Wv$coords_all
      ), file.path(output_dir,"01_rds", nm, "edges_plot.rds"))
    } else {
      warning(paste("Salvataggio edges_plot.rds fallito per", nm, "- elementi mancanti."))
    }
    
    # Contatore cluster
    tab <- table(sf_s$LISA_cluster)
    writeLines(
      paste("Cluster counts:", paste(names(tab), tab, collapse=", ")),
      con=file.path(output_dir,"03_tables", nm, "cluster_counts_strict.txt")
    )
    
    # Contenitore in memoria
    results_all[[nm]] <- list(
      area=a, dimensione=d, k_best=k_best,
      moran=list(strict=mor_s, border=mor_b)
    )
    # Rimuovi gli oggetti del subset per liberare la RAM per l'iterazione successiva
    rm(sf_sub, Wv, mor_s, mor_b, lisa_s, lisa_b) 
    
    # Forza la pulizia della memoria
    gc()
    cat("Completato:", nm, "\n")
  }
}

close_log()

# -------------------------------------------------------------
# 6.1 LOOP SEPARATO: Generazione Grafo Comparativo W 
# -------------------------------------------------------------
open_log("loop_grafo_comparativo")

# Assicurati che ripgeo_tr sia caricato in memoria (es. dalla Sezione 2)
# ripgeo_tr <- readRDS(file.path(output_dir,"01_rds", "ripgeo_tr.rds")) # Se lo avevi salvato

# Esegui la generazione dei grafi per ogni combinazione
for (a in areas) {
  for (d in dims) {
    nm <- paste0(d, "_", gsub(" ", "_", a))
    invisible(plot_W_comparison_full(nm, ripgeo_tr))
  }
 
}

close_log()
# -------------------------------------------------------------
# 7. RIEPILOGHI GLOBALI
# -------------------------------------------------------------
# Moran summary (da results_all)
moran_summary <- purrr::map_dfr(names(results_all), function(nm){
  res <- results_all[[nm]]
  data.frame(
    Subset      = nm,
    Area        = res$area,
    Dimensione  = res$dimensione,
    k_best      = res$k_best,
    Moran_I_strict = res$moran$strict$estimate[["Moran I statistic"]],
    p_strict       = res$moran$strict$p.value,
    Moran_I_border = res$moran$border$estimate[["Moran I statistic"]],
    p_border       = res$moran$border$p.value
  )
})
moran_summary <- moran_summary %>% arrange(Area, Dimensione)
save_table(moran_summary, "moran_summary_all")
# Questo crea/aggiorna 'cluster_counts_all_variants.csv' in ogni sottocartella
cat("\nAvvio post-processing (make_clusters_tidy)...\n")
nms <- names(results_all)
invisible(lapply(nms, make_clusters_tidy, alpha = 0.05)) #
cat("Aggregazione di tutti i conteggi LISA locali...\n")
all_lisa_counts <- purrr::map_dfr(nms, function(nm) {
  csv_file <- file.path(output_dir, "03_tables", nm, "lisa_cluster_counts.csv") 
  if (file.exists(csv_file)) {
    df <- utils::read.csv(csv_file, check.names = FALSE) # check.names=FALSE per High-High
    df$Subset <- nm
    return(df)
  } else {
    warning(paste("File conteggi LISA mancante per:", nm))
    return(NULL)
  }
})

cat("Aggregazione di tutti i conteggi Gi* locali...\n")
all_gi_counts <- purrr::map_dfr(nms, function(nm) {
  csv_file <- file.path(output_dir, "03_tables", nm, "gi_cluster_counts.csv")
  if (file.exists(csv_file)) {
    df <- utils::read.csv(csv_file, check.names = FALSE) # check.names=FALSE per 99%
    df$Subset <- nm
    return(df)
  } else {
    warning(paste("File conteggi Gi* mancante per:", nm))
    return(NULL)
  }
})

# Riorganizza e salva il riepilogo completo
if (nrow(all_lisa_counts) > 0) {
  all_lisa_counts <- all_lisa_counts %>%
    dplyr::select(Subset, variante, everything()) %>%
    dplyr::arrange(Subset, variante)
  save_table(all_lisa_counts, "riepilogo_cluster_LISA_completo")
}

if (nrow(all_gi_counts) > 0) {
  all_gi_counts <- all_gi_counts %>%
    dplyr::select(Subset, variante, everything()) %>%
    dplyr::arrange(Subset, variante)
  save_table(all_gi_counts, "riepilogo_cluster_GI_completo") # <-- NUOVO FILE
}#Riepilogo nazionale (come prima, ma con nome più chiaro)

conteggio_totale <- imprese_sf_jit |> st_drop_geometry() |> count(LISA_cluster)
save_table(conteggio_totale, "conteggio_totale_cluster_nazionale")
cat("\n=== PIPELINE SPAZIALE COMPLETATA ===\n")

# -------------------------------------------------------------
# 8. MODELLI REGRESSIVI + GWR CONDIZIONATA (CONFORME TESI)
#    - Eseguiamo i modelli su un subset (area_sel × dim_sel)
#    - Lancia GWR solo se conviene (autocorr. residua + ΔAIC)
# -------------------------------------------------------------

open_log("analisi_regressiva_dimensione_macroarea")

# === Soglie/euristiche per GWR ===
gwr_rules <- list(
  min_n       = 200,    # sotto: skip
  max_n_gwr   = 12000,  # sopra: skip
  alpha       = 0.05,   # soglia p-value Moran(residui SDM)
  aic_gain_min= 10,     # AIC(OLS) - AIC(SDM) minimo
  samp_cap    = 6000,   # massimo campione per GWR
  seed        = 123
)

for (a in areas) {
  for (d in dims) {
    nm <- paste0(d, "_", gsub(" ", "_", a))
    cat("\n=== Analisi:", a, "×", d, "===\n")
    
    sf_sub <- imprese_sf_tr %>% filter(macroarea==a, dimensione_impresa==d)
    n_sub  <- nrow(sf_sub)
    if (n_sub < 30) {
      message("Campione troppo piccolo (", n_sub, "), salto.")
      next
    }
    # (Assicurati che 'k_opt' sia definito dalla tua Sezione 3, es. k_opt <- 59)
    if (!exists("k_opt")) k_opt <- 59 # Fallback di sicurezza
    
    if (nrow(sf_sub) >= 50) {
  # k ottimale e W STRICT (coerente con le analisi precedenti)
  k_res  <- scegli_k_knn(sf_sub, k_min=10, k_max=k_opt, step=2)
  k_best <- min(k_res$best_k, nrow(sf_sub)-1)
  coords <- sf::st_coordinates(sf_sub)
  lw     <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(coords, k=k_best)), style="W", zero.policy=TRUE)
  
  # Formulazione
  fml    <- as.formula(paste("ISP_bn ~", paste(vars_to_scale, collapse=" + ")))
  lm_b   <- lm(fml, data=sf_sub)
  sar_m  <- spatialreg::errorsarlm(fml, data=sf_sub, listw=lw, zero.policy=TRUE)
  sdm_m  <- spatialreg::lagsarlm(fml, data=sf_sub, listw=lw, type="mixed", zero.policy=TRUE)
  gmm_m  <- spatialreg::GMerrorsar(fml, data=sf_sub, listw=lw, zero.policy=TRUE)
  imp_m  <- tryCatch(spatialreg::impacts(sdm_m, listw=lw, R=500), error=function(e) NULL)
  
  # Salvataggi
  dir.create(file.path(output_dir,"03_tables", nm), recursive=TRUE, showWarnings=FALSE)
  capture.output(summary(sar_m), file=file.path(output_dir,"03_tables", nm, "sar.txt"))
  capture.output(summary(sdm_m), file=file.path(output_dir,"03_tables", nm, "sdm.txt"))
  if (!is.null(imp_m)) capture.output(imp_m, file=file.path(output_dir,"03_tables", nm, "impacts_sdm.txt"))
  capture.output(summary(gmm_m), file=file.path(output_dir,"03_tables", nm, "gmm.txt"))
  
  # Diagnostiche
  diag_lm  <- diagnostiche_spaziali(lm_b,  lw, data=sf_sub, formula=fml)
  diag_sar <- diagnostiche_spaziali(sar_m, lw, data=sf_sub, formula=fml)
  diag_sdm <- diagnostiche_spaziali(sdm_m, lw, data=sf_sub, formula=fml)
  
  dump_diagnostics(diag_lm,  file.path(output_dir,"03_tables", nm, "diagn_lm.txt"),
                   file_csv_summary = file.path(output_dir,"03_tables", "diagn_summary.csv"))
  dump_diagnostics(diag_sar, file.path(output_dir,"03_tables", nm, "diagn_sar.txt"),
                   file_csv_summary = file.path(output_dir,"03_tables", "diagn_summary.csv"))
  dump_diagnostics(diag_sdm, file.path(output_dir,"03_tables", nm, "diagn_sdm.txt"),
                   file_csv_summary = file.path(output_dir,"03_tables", "diagn_summary.csv"))
  
  # Decisione GWR (residui SDM + ΔAIC)
  mor_res_sdm <- residual_moran(sdm_m, lw)
  aic_lm  <- AIC(lm_b); aic_sdm <- AIC(sdm_m)
  gate <- gwr_eligible(
    n = nrow(sf_sub),
    moran_resid_sdm_p = mor_res_sdm$p,
    aic_lm = aic_lm, aic_sdm = aic_sdm,
    min_n = gwr_rules$min_n,
    max_n_gwr = gwr_rules$max_n_gwr,
    alpha = gwr_rules$alpha,
    aic_gain_min = gwr_rules$aic_gain_min)
  dec_path <- file.path(output_dir,"03_tables", nm, "gwr_decision.txt")
  if (!gate$ok) {
    writeLines(paste("GWR SKIP –", ifelse(nchar(gate$why)>0, gate$why, "motivo non specificato")), con=dec_path)
  } else {
    # Esegui GWR con cap campione
    samp_size <- min(nrow(sf_sub), gwr_rules$samp_cap)
    gwr_out <- tryCatch(
      run_gwr(sf_sub, vars_to_scale, samp_size=samp_size, seed=gwr_rules$seed),
      error=function(e) e
    )
    if (inherits(gwr_out, "error")) {
      writeLines(paste("GWR ERRORE –", gwr_out$message), con=dec_path)
    } else {
      saveRDS(gwr_out, file.path(output_dir,"01_rds", nm, "gwr_out.rds"))
      # Mappa coefficiente (primo disponibile tra le regressori usati)
      coef_show <- names(gwr_out$coeff_sf)[names(gwr_out$coeff_sf) %in% vars_to_scale][1]
      if (!is.na(coef_show) && length(coef_show)>0) {
        p_coef <- plot_gwr_coef(gwr_out$coeff_sf, coef_show, ripgeo,
                                title=paste("GWR –", coef_show, "–", nm))
        save_pdf(p_coef, file.path(nm, paste0("gwr_coef_", coef_show)))
      }
      writeLines("GWR RUN – OK", con=dec_path)
    }
  }
  
  # Riepilogo sintetico per tesi
  riepilogo <- data.frame(
    Subset = nm,
    Area = a,
    Dimensione = d,
    n = nrow(sf_sub),
    k_best = k_best,
    AIC_OLS = aic_lm,
    AIC_SDM = aic_sdm,
    DeltaAIC_OLS_SDM = aic_lm - aic_sdm,
    MoranResidui_SDM_I = round(mor_res_sdm$I, 5),
    MoranResidui_SDM_p = signif(mor_res_sdm$p, 4),
    GWR_Eseguita = gate$ok,
    stringsAsFactors = FALSE
  )
  
  # append su file globale
  riepilogo_file <- file.path(output_dir, "03_tables", "riepilogo_regressivo_gwr.csv")
  if (!file.exists(riepilogo_file)) {
    write.csv(riepilogo, riepilogo_file, row.names=FALSE)
  } else {
    utils::write.table(riepilogo, riepilogo_file, sep=",", col.names=FALSE, row.names=FALSE, append=TRUE)
  }
  
  # Salvataggio oggetti completi (opzionale)
  saveRDS(list(sar=sar_m, sdm=sdm_m, gmm=gmm_m, impacts=imp_m),
          file.path(output_dir,"01_rds", nm, "regressioni_sar_sdm_gmm.rds"))
  
  # === RIEPILOGO PER MODELLO (tidy, append cumulativo) ===
  summarize_model <- function(name, mod, lw, diag) {
    # AIC
    aic_val <- tryCatch(AIC(mod), error=function(e) NA_real_)
    # Moran residui (p)
    mor_p <- tryCatch({
      mt <- spdep::moran.test(residuals(mod), listw = lw, zero.policy = TRUE)
      mt$p.value
    }, error=function(e) NA_real_)
    # BP p-value
    bp_p <- tryCatch({
      if (inherits(mod, "sarlm")) spatialreg::bptest.sarlm(mod)$p.value else lmtest::bptest(mod)$p.value
    }, error=function(e) NA_real_)
    # AD normality p-value
    ad_p <- tryCatch(nortest::ad.test(residuals(mod))$p.value, error=function(e) NA_real_)
    data.frame(model=name, AIC=aic_val, Moran_resid_p=mor_p, BP_p=bp_p, AD_p=ad_p, check.names=FALSE)
  }
  
  # costruisci tabella per i 3/4 modelli
  tab_models <- rbind(
    summarize_model("OLS",      lm_b,  lw, diag_lm),
    summarize_model("SAR(err)", sar_m, lw, diag_sar),
    summarize_model("SDM(mix)", sdm_m, lw, diag_sdm),
    summarize_model("GMM(err)", gmm_m, lw, NULL)
  )
  
  # identifica il migliore per AIC
  best_idx <- which.min(tab_models$AIC)
  tab_models$best_by_AIC <- FALSE
  tab_models$best_by_AIC[best_idx] <- TRUE
  
  # aggiungi metadati subset
  tab_models$Subset     <- nm
  tab_models$Area       <- a
  tab_models$Dimensione <- d
  tab_models$n          <- nrow(sf_sub)
  tab_models$k_best     <- k_best
  
  # ordina colonne e salva (append cumulativo)
  tab_models <- tab_models[, c("Subset","Area","Dimensione","n","k_best","model","AIC","Moran_resid_p","BP_p","AD_p","best_by_AIC")]
  file_models <- file.path(output_dir,"03_tables","riepilogo_modello_dettaglio.csv")
  if (!file.exists(file_models)) {
    write.csv(tab_models, file_models, row.names = FALSE)
  } else {
    utils::write.table(tab_models, file_models, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
  }
  
  # (opzionale) segnale veloce a console se SDM non migliora OLS in modo “materiale”
  deltaAIC <- AIC(lm_b) - AIC(sdm_m)
  if (is.finite(deltaAIC)) {
    if (deltaAIC < 0) {
      message(sprintf("[%s] SDM peggiore di OLS (ΔAIC=%.2f): mantieni OLS o SAR.", nm, deltaAIC))
    } else if (deltaAIC < 2) {
      message(sprintf("[%s] SDM ~ OLS (ΔAIC=%.2f): miglioramento debole, verifica residui e parsimony.", nm, deltaAIC))
    } else if (deltaAIC >= 10) {
      message(sprintf("[%s] SDM >> OLS (ΔAIC=%.2f): miglioramento sostanziale.", nm, deltaAIC))
    }
  }
  
  # --- INIZIO BLOCCO PULIZIA MEMORIA ---
  
  # 1. Rimuovi tutti gli oggetti pesanti creati nel loop
  # (Lascia solo 'nm', 'a', 'd' che servono al loop stesso)
  oggetti_da_rimuovere <- c(
    "sf_sub", "n_sub", "k_res", "k_best", "coords", "lw", 
    "fml", "lm_b", "sar_m", "sdm_m", "gmm_m", "imp_m", 
    "diag_lm", "diag_sar", "diag_sdm", "mor_res_sdm", 
    "aic_lm", "aic_sdm", "gate", "dec_path", "samp_size",
    "riepilogo", "riepilogo_file", "tab_models", "best_idx", 
    "file_models", "deltaAIC"
  )
  
  # Rimuovi gwr_out solo se è stato creato
  if (exists("gwr_out")) {
    oggetti_da_rimuovere <- c(oggetti_da_rimuovere, "gwr_out", "coef_show")
  }
  
  rm(list = oggetti_da_rimuovere)
  
  # 2. Forza il Garbage Collector
  gc()
  
  # --- FINE BLOCCO PULIZIA MEMORIA ---
  cat("\n=== REGRESSIONI COMPLETATE PER:", nm, "===\n")
} else {
  cat("\nSubset troppo piccolo per regressioni:", nm, " (n=", nrow(sf_sub), ")\n")
}
}# fine loop 'd'
}# fine loop 'a'

close_log()

# -------------------------------------------------------------
# 9. MAPPA AGGREGATA (NAZIONALE) DEI RISULTATI LOCALI
# -------------------------------------------------------------
open_log("mappe_aggregate_locali")
cat("Creazione mappe aggregate dei cluster locali...\n")

#  Recupera i nomi di tutti i subset analizzati con successo
# (results_all è l'oggetto in memoria dal Loop 6)
nms <- names(results_all)
if (is.null(nms) || length(nms) == 0) {
  stop("Nessun risultato trovato in 'results_all'. Eseguire prima la Sezione 6.")
}

# Carica e combina tutti i file 'clusters_tidy' locali
# (Questi sono i file creati da 'make_clusters_tidy')
all_local_clusters_list <- purrr::map_dfr(nms, function(nm) {
  rds_file <- file.path(output_dir, "01_rds", nm, "clusters_tidy_all_variants.rds")
  if (file.exists(rds_file)) {
    return(readRDS(rds_file))
  } else {
    cat("File non trovato (salto):", rds_file, "\n")
    return(NULL)
  }
})

# Unisci tutti gli sf in un unico oggetto
all_local_clusters_sf <- all_local_clusters_list
if (nrow(all_local_clusters_sf) == 0) {
  stop("Nessun file cluster locale è stato caricato. Impossibile aggregare.")
}

# Dividi per W_strict e W_border
sf_strict_agg <- all_local_clusters_sf %>% 
  dplyr::filter(variante == "W_strict")

sf_border_agg <- all_local_clusters_sf %>% 
  dplyr::filter(variante == "W_border")

# Crea i plot finali aggregati (LISA)
# (Usando le funzioni 'plot_lisa' e 'plot_gi' definite in utils_spatial.R)
p_agg_lisa_strict <- plot_lisa(sf_strict_agg, ripgeo_tr,
                               titolo = "Mappa Aggregata: Cluster LISA Locali (W_strict)",
                               size_ns = .1, alpha_ns = .05) 

p_agg_lisa_border <- plot_lisa(sf_border_agg, ripgeo_tr,
                               titolo = "Mappa Aggregata: Cluster LISA Locali (W_border)",
                               size_ns = .1, alpha_ns = .05)

# Crea i plot finali aggregati (Gi*)
p_agg_gi_strict <- plot_gi(sf_strict_agg, ripgeo_tr,
                           titolo = "Mappa Aggregata: Hotspot Gi* Locali (W_strict)",
                           size_ns = .1, alpha_ns = .05)

p_agg_gi_border <- plot_gi(sf_border_agg, ripgeo_tr,
                           titolo = "Mappa Aggregata: Hotspot Gi* Locali (W_border)",
                           size_ns = .1, alpha_ns = .05)

# 6. Salva le 4 mappe principali per la tesi
save_pdf(p_agg_lisa_strict, "MAPPA_FINALE_LISA_AGG_STRICT")
save_pdf(p_agg_lisa_border, "MAPPA_FINALE_LISA_AGG_BORDER")
save_pdf(p_agg_gi_strict, "MAPPA_FINALE_GI_AGG_STRICT")
save_pdf(p_agg_gi_border, "MAPPA_FINALE_GI_AGG_BORDER")

cat("Mappe aggregate finali salvate in:", file.path(output_dir, "02_maps"), "\n")
close_log()


# -------------------------------------------------------------
# 10. ROBUSTEZZA E RIEPILOGHI FINALI
# -------------------------------------------------------------
open_log("robustezza_e_riepiloghi_finali")
cat("Esecuzione controlli di robustezza finali...\n")

# Riepilogo Accordo Cluster (Strict vs. Border) 
# (Adattato da 'cluster_agreement' dello script di test)

calculate_cluster_agreement <- function(nm) {
  rds_file <- file.path(output_dir, "01_rds", nm, "clusters_tidy_all_variants.rds")
  if (!file.exists(rds_file)) return(NULL)
  
  x <- readRDS(rds_file)
  
  # Confronta LISA
  wide_lisa <- x %>% sf::st_drop_geometry() %>%
    dplyr::select(codice_fiscale, variante, LISA_cluster) %>%
    tidyr::pivot_wider(names_from = variante, values_from = LISA_cluster)
  
  # Confronta Gi*
  wide_gi <- x %>% sf::st_drop_geometry() %>%
    dplyr::select(codice_fiscale, variante, Gi_bin) %>%
    tidyr::pivot_wider(names_from = variante, values_from = Gi_bin)
  
  # Calcola l'accordo
  data.frame(
    Subset = nm,
    agree_LISA = mean(wide_lisa$W_strict == wide_lisa$W_border, na.rm = TRUE),
    agree_Gi   = mean(wide_gi$W_strict == wide_gi$W_border, na.rm = TRUE)
  )
}

# Esegui su tutti i subset
agreement_tbl <- purrr::map_dfr(nms, calculate_cluster_agreement)
save_table(agreement_tbl, "riepilogo_accordo_cluster_strict_vs_border")


# Controllo Residui GWR (Moran's I) ---
# (Adattato da 'check_gwr_resid_moran' dello script di test)

# Trova solo i subset dove GWR è stato ESEGUITO con successo
gwr_targets <- readr::read_csv(
  file.path(output_dir, "03_tables", "riepilogo_regressivo_gwr.csv")
) %>%
  dplyr::filter(GWR_Eseguita == TRUE) %>%
  pull(Subset)

check_gwr_resid_moran <- function(nm, W_names = c("W_strict", "W_border")) {
  # Carica i file necessari per questo subset
  sf_file <- file.path(output_dir, "01_rds", nm, "sf_strict_with_stats.rds")
  W_file  <- file.path(output_dir, "01_rds", nm, "matrici_W.rds")
  gwr_file <- file.path(output_dir, "01_rds", nm, "gwr_out.rds") # Nome corretto da Sez. 8
  
  if (!all(file.exists(sf_file, W_file, gwr_file))) return(NULL)
  
  sf_s <- readRDS(sf_file)
  Wv   <- readRDS(W_file)
  gwr  <- readRDS(gwr_file)
  
  # Trova i residui GWR
  rname <- intersect(c("gwr.e", "e", "residual", "residuals"), names(gwr$coeff_sf))
  if (length(rname) == 0) {
    warning(nm, ": residui GWR non trovati.")
    return(NULL)
  }
  res <- gwr$coeff_sf[[rname[1]]]
  
  # Testa i residui contro le matrici W
  out_list <- list()
  for (W_name in W_names) {
    if (!W_name %in% names(Wv)) next
    lw <- Wv[[W_name]]
    mt <- spdep::moran.test(res, lw, zero.policy = TRUE)
    
    out_list[[W_name]] <- tibble(
      Subset = nm,
      W_resid_test = W_name,
      Moran_I_resid = unname(mt$estimate[["Moran I statistic"]]),
      p_value = mt$p.value
    )
  }
  return(bind_rows(out_list))
}

# Esegui solo sui target GWR
gwr_resid_moran_tbl <- purrr::map_dfr(gwr_targets, check_gwr_resid_moran)
save_table(gwr_resid_moran_tbl, "riepilogo_gwr_residui_moran")


#  Riepilogo Cluster con Correzione FDR 
# (Adattato da 'lisa_fdr_counts' dallo script di test)
# Questo è un test di robustezza finale per i p-value di LISA

lisa_fdr_counts <- function(nm, alpha = 0.05) {
  # (Carica i file)
  lis_file <- file.path(output_dir, "01_rds", nm, "lisa_results.rds")
  W_file <- file.path(output_dir, "01_rds", nm, "matrici_W.rds")
  sf_file <- file.path(output_dir, "01_rds", nm, "sf_strict_with_stats.rds")
  
  if (!all(file.exists(lis_file, W_file, sf_file))) return(NULL)
  
  lis <- readRDS(lis_file)
  Wv  <- readRDS(W_file)
  sf_s <- readRDS(sf_file)
  
  # Applica correzione FDR (Benjamini-Hochberg) ai p-value
  fdr <- function(p) p.adjust(p, method = "BH")
  lis$lisa_strict[,5] <- fdr(lis$lisa_strict[,5])
  lis$lisa_border[,5] <- fdr(lis$lisa_border[,5])
  
  # Riclassifica usando i p-value corretti
  classify_lisa <- function(sf_base, lisa_mat, lw, alpha=0.05){
    out <- sf_base
    lagx <- spdep::lag.listw(lw, out$ISP_bn, zero.policy = TRUE) # Aggiunto zero.policy
    out$ISP_std     <- as.numeric(scale(out$ISP_bn))
    out$lag_ISP_std <- as.numeric(scale(lagx))
    Ii <- lisa_mat[,1]; p <- lisa_mat[,5]
    cl <- rep("Non signif", nrow(out))
    cl[p<=alpha & Ii>0 & out$ISP_std>0 & out$lag_ISP_std>0] <- "High-High"
    cl[p<=alpha & Ii>0 & out$ISP_std<0 & out$lag_ISP_std<0] <- "Low-Low"
    cl[p<=alpha & Ii<0 & out$ISP_std>0 & out$lag_ISP_std<0] <- "High-Low"
    cl[p<=alpha & Ii<0 & out$ISP_std<0 & out$lag_ISP_std>0] <- "Low-High"
    out$LISA_cluster <- factor(cl, levels=c("High-High","Low-Low","High-Low","Low-High","Non signif"))
    out
  }
  
  s_str <- classify_lisa(sf_s, lis$lisa_strict, Wv$W_strict, alpha)
  s_bor <- classify_lisa(sf_s, lis$lisa_border, Wv$W_border, alpha)
  
  s_str$variante <- "W_strict"; s_bor$variante <- "W_border"
  
  long <- rbind(
    s_str[, c("variante","LISA_cluster")],
    s_bor[, c("variante","LISA_cluster")]
  ) %>% sf::st_drop_geometry()
  
  # Conteggi finali
  levs <- c("High-High","Low-Low","High-Low","Low-High","Non signif")
  counts <- long %>%
    dplyr::mutate(LISA_cluster = factor(LISA_cluster, levels = levs)) %>%
    dplyr::count(variante, LISA_cluster, .drop = FALSE) %>%
    tidyr::pivot_wider(names_from = LISA_cluster, values_from = n) %>%
    dplyr::arrange(variante)
  
  counts$Subset <- nm
  return(counts)
}

# Esegui su tutti i subset
fdr_counts_all <- purrr::map_dfr(nms, lisa_fdr_counts, alpha = 0.05)
save_table(fdr_counts_all, "riepilogo_cluster_LISA_FDR_corretto")

cat("Script di robustezza completati.\n")
close_log()

# -------------------------------------------------------------
# 11. ROBUSTEZZA FINALE (FDR SU ANALISI NAZIONALE)
# -------------------------------------------------------------
open_log("robustezza_finale_fdr_nazionale")
cat("Esecuzione test di robustezza FDR sull'analisi nazionale...\n")

# 1. Carica i risultati nazionali (creati nella Sezione 4)
file_nazionale <- file.path(output_dir, "01_rds", "imprese_sf_jit.rds")
if (!file.exists(file_nazionale)) {
  stop("File nazionale 'imprese_sf_jit.rds' non trovato. Eseguire prima la Sezione 4.")
}
imprese_sf_jit <- readRDS(file_nazionale)

# 2. Applica correzione FDR (Benjamini-Hochberg) ai p-value
fdr <- function(p) p.adjust(p, method = "BH")
imprese_sf_jit$lisa_p_fdr <- fdr(imprese_sf_jit$lisa_p)

# 3. Riclassifica i cluster usando i p-value corretti (alpha = 0.05)
# (Usando la logica della Sezione 4)
alpha_fdr <- 0.05
imprese_sf_jit <- imprese_sf_jit %>%
  mutate(
    LISA_cluster_FDR = case_when(
      lisa_p_fdr <= alpha_fdr & ISP_std > 0 & lag_ISP_std > 0 ~ "High-High",
      lisa_p_fdr <= alpha_fdr & ISP_std < 0 & lag_ISP_std < 0 ~ "Low-Low",
      lisa_p_fdr <= alpha_fdr & ISP_std > 0 & lag_ISP_std < 0 ~ "High-Low",
      lisa_p_fdr <= alpha_fdr & ISP_std < 0 & lag_ISP_std > 0 ~ "Low-High",
      TRUE ~ "Non signif"
    ) %>% factor(levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Non signif"))
  )

# 4. Confronta i conteggi
cat("\n--- Confronto Conteggi Cluster Nazionali (Standard vs. FDR) ---\n")

conteggio_standard <- imprese_sf_jit %>%
  sf::st_drop_geometry() %>%
  count(LISA_cluster) %>%
  rename(Cluster = LISA_cluster, Conteggio_Standard = n)

conteggio_fdr <- imprese_sf_jit %>%
  sf::st_drop_geometry() %>%
  count(LISA_cluster_FDR) %>%
  rename(Cluster = LISA_cluster_FDR, Conteggio_FDR = n)

# Unisci e stampa i risultati
confronto_fdr <- dplyr::full_join(conteggio_standard, conteggio_fdr, by = "Cluster") %>%
  dplyr::arrange(Cluster)

print(confronto_fdr)

# 5. Salva i risultati
save_table(confronto_fdr, "riepilogo_cluster_nazionale_confronto_FDR")
save_rds(imprese_sf_jit, "imprese_sf_jit_con_fdr") # Salva l'oggetto finale

cat("\nTest FDR sull'analisi nazionale completato.\n")
close_log()

cat("\n\n=== TUTTA L'ANALISI E LA SINTESI SONO COMPLETATE ===\n")

# ===================== FINE SCRIPT FINALE ===========================
cat("\n\n=== TUTTA L'ANALISI E LA SINTESI SONO COMPLETATE ===\n")

# ===================== FINE SCRIPT ===========================
cat("\nSCRIPT COMPLETATO – output nella gerarchia delle cartelle.\n")
# --- Esecuzione Sezione 8 (Regressiva) con Formula Corretta e Pulita ---
