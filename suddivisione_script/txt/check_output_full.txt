# ======================================================
# CHECK OUTPUT PIPELINE - Allineato a Sezioni 1-10
# ======================================================
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

check_outputs_full <- function(output_dir = "05_analysis_spatial") {
  
  message("=== CHECK OUTPUT COMPLETO ===")
  
  fpath <- function(p) file.path(output_dir, p)
  
  # 1) CHECK GLOBALE (invariato)
  expected_files <- c(
    fpath("01_rds/imprese_sf_tr.rds"), fpath("01_rds/listw_knn.rds"), 
    fpath("01_rds/imprese_sf_jit.rds"), fpath("03_tables/k_moran.csv"),
    fpath("03_tables/conteggio_totale_cluster_nazionale.csv"),
    fpath("03_tables/moran_summary_all.csv"),
    fpath("03_tables/riepilogo_cluster_LISA_completo.csv"),
    fpath("03_tables/riepilogo_cluster_GI_completo.csv"),
    fpath("03_tables/diagn_summary.csv"),
    fpath("03_tables/riepilogo_regressivo_gwr.csv"),
    fpath("03_tables/riepilogo_modello_dettaglio.csv"),
    fpath("02_maps/MAPPA_FINALE_LISA_AGG_STRICT.pdf"),
    fpath("02_maps/MAPPA_FINALE_LISA_AGG_BORDER.pdf"),
    fpath("02_maps/MAPPA_FINALE_GI_AGG_STRICT.pdf"),
    fpath("02_maps/MAPPA_FINALE_GI_AGG_BORDER.pdf"),
    fpath("03_tables/riepilogo_accordo_cluster_strict_vs_border.csv"),
    fpath("03_tables/riepilogo_gwr_residui_moran.csv"),
    fpath("03_tables/riepilogo_cluster_LISA_FDR_corretto.csv")
  )
  missing_static <- expected_files[!file.exists(expected_files)]
  
  if (length(missing_static) == 0) {
    message("✅ Check globale: tutti i file di riepilogo principali sono presenti.")
  } else {
    print(data.frame(File_Mancante = missing_static))
    warning("⚠️ Mancano file globali della pipeline.")
  }
  
  # -------------------------------
  # 2) CHECK DINAMICO PER SUBSET (Logica aggiornata)
  # -------------------------------
  subset_dirs <- list.dirs(fpath("01_rds"), full.names = FALSE, recursive = FALSE)
  subset_dirs <- subset_dirs[!grepl("\\.rds$", subset_dirs, ignore.case = TRUE)] 
  
  if (length(subset_dirs) == 0) {
    warning("Nessuna cartella di subset trovata in 01_rds. Check dinamico saltato.")
    return(invisible(NULL))
  }
  
  checks <- list()
  
  for (nm in subset_dirs) {
    subset_path_rds <- file.path(fpath("01_rds"), nm)
    
    # --- Leggi N per questo subset ---
    n_sub <- 0
    sf_stats_file <- file.path(subset_path_rds, "sf_strict_with_stats.rds")
    if (file.exists(sf_stats_file)) {
      # Leggiamo il file per contare n (costoso ma accurato)
      # Nota: se 'results_all' esiste in memoria, potremmo prenderlo da lì
      temp_sf <- readRDS(sf_stats_file)
      n_sub <- nrow(temp_sf)
      rm(temp_sf) # Libera memoria
    }
    # ---
    
    # Controlla Sez. 6 (Cluster) - Richiesti se n >= 30
    files_sez6 <- c("matrici_W.rds", "lisa_results.rds", 
                    "sf_strict_with_stats.rds", "edges_plot.rds",
                    "clusters_tidy_all_variants.rds")
    
    note_sez6 <- ifelse(n_sub >= 30, "Richiesto (n >= 30)", "Non richiesto (n < 30)")
    
    for (req in files_sez6) {
      checks[[length(checks)+1]] <- data.frame(
        Subset = nm, n = n_sub, File = req,
        Exists = file.exists(file.path(subset_path_rds, req)),
        Note = note_sez6
      )
    }
    
    # Controlla Sez. 8 (Regressioni) - Richiesti se n >= 50
    file_sez8 <- "regressioni_sar_sdm_gmm.rds"
    note_sez8 <- ifelse(n_sub >= 50, "Richiesto (n >= 50)", "Non richiesto (n < 50)")
    checks[[length(checks)+1]] <- data.frame(
      Subset = nm, n = n_sub, File = file_sez8,
      Exists = file.exists(file.path(subset_path_rds, file_sez8)),
      Note = note_sez8
    )
    
    # Controlla Sez. 8 (GWR) - Condizionale
    gwr_decision_file <- file.path(fpath("03_tables"), nm, "gwr_decision.txt")
    gwr_out_file <- file.path(subset_path_rds, "gwr_out.rds")
    
    note_gwr <- "Non richiesto (n < 50)" # Default
    if (n_sub >= 50) {
      if (file.exists(gwr_decision_file)) {
        decision <- readLines(gwr_decision_file, n = 1, warn = FALSE)
        note_gwr <- ifelse(grepl("GWR RUN – OK", decision), 
                           "Richiesto (GWR RUN OK)", 
                           "Non richiesto (GWR SKIP)")
      } else {
        note_gwr <- "Richiesto (Decisione mancante?)"
      }
    }
    
    checks[[length(checks)+1]] <- data.frame(
      Subset = nm, n = n_sub, File = "gwr_out.rds",
      Exists = file.exists(gwr_out_file),
      Note = note_gwr
    )
  }
  
  df_dyn <- dplyr::bind_rows(checks)
  summary_df <- df_dyn %>%
    dplyr::group_by(Subset) %>%
    dplyr::summarise(
      n_obs = max(n), # Aggiunge n al riepilogo
      Totali = n(),
      Presenti = sum(Exists),
      Mancanti = sum(!Exists),
      .groups = "drop"
    ) %>%
    dplyr::arrange(n_obs) # Ordina per n
  
  message("=== CHECK DINAMICO PER SUBSET (su 01_rds) ===")
  print(summary_df, n = 50) # Mostra tutte le righe
  
  # Filtra solo i file "Richiesti" che sono "Mancanti"
  missing_dyn <- df_dyn %>% 
    dplyr::filter(!Exists & grepl("Richiesto", Note))
  
  if (nrow(missing_dyn) > 0) {
    message("⚠️ File *richiesti* mancanti nei subset:")
    print(missing_dyn)
  } else {
    message("✅ Check dinamico: tutti i file core presenti nei subset (secondo le soglie n=30 e n=50).")
  }
  
  invisible(list(global_missing = if (length(missing_static)) missing_static else NULL,
                 subset_summary = summary_df,
                 subset_missing = missing_dyn))
}

# ESEGUI IL CHECK
check_outputs_full()