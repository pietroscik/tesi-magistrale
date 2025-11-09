# -------------------------------------------------------------
# 1. FUNZIONI UTILITY
# -------------------------------------------------------------
# 1.1 Jitter centralizzato
jitter_coords <- function(sf_obj, amount = 1e-4) {
  sf_obj %>% mutate(geometry = st_jitter(geometry, amount = amount))
}

# 1.2 Palette diverging rapida
pal_div <- function(n) scales::div_gradient_pal(low="#2166ac", mid="white", high="#b2182b")(seq(0,1,len=n))

# 1.3 Plot LISA
plot_lisa <- function(sf_obj, ripgeo, titolo="Cluster LISA",
                      size_ns=.3, size_sig=.9, alpha_ns=.15, alpha_sig=.90) {
  sf_obj$LISA_cluster <- factor(sf_obj$LISA_cluster,
                                levels=c("Non signif","High-High","Low-Low","High-Low","Low-High"))
  ggplot() +
    geom_sf(data=ripgeo, fill="grey95", colour="grey60", linewidth=.12) +
    geom_sf(data = sf_obj |> dplyr::filter(LISA_cluster == "Non signif"),
            colour="grey70", size=size_ns, alpha=alpha_ns, show.legend=FALSE) +
    geom_sf(data = sf_obj |> dplyr::filter(LISA_cluster != "Non signif"),
            aes(colour=LISA_cluster), size=size_sig, alpha=alpha_sig, show.legend="point") +
    scale_colour_manual(values=c("High-High"="#ca0020","Low-Low"="#0571b0","High-Low"="#fdae61","Low-High"="#92c5de")) +
    guides(colour = guide_legend(override.aes = list(size = 4))) +
    labs(title=titolo, colour="Cluster") +
    theme_minimal(base_size=11) + theme(panel.grid=element_blank())
}

# 1.4 Plot Gi*
# 1.4 Plot Gi* (Versione aggiornata con logica a due strati)
plot_gi <- function(sf_obj, ripgeo, titolo="Hotspot (Gi*)",
                    size_ns=.3, size_sig=.9, alpha_ns=.15, alpha_sig=.90){ # Aggiunti parametri
  
  # Assicura che la colonna Gi_bin esista
  if(!"Gi_bin" %in% names(sf_obj)){
    sf_obj$Gi_bin <- cut(sf_obj$Gi,
                         breaks=c(-Inf,-1.96,-1.65,1.65,1.96,Inf),
                         labels=c("Coldspot 99%","Coldspot 95%","Non signif","Hotspot 95%","Hotspot 99%"))
  }
  
  # Assicura l'ordine corretto dei livelli
  sf_obj$Gi_bin <- factor(sf_obj$Gi_bin, levels = c("Coldspot 99%", "Coldspot 95%", 
                                                    "Non signif", 
                                                    "Hotspot 95%", "Hotspot 99%"))
  
  ggplot() +
    # Sfondo
    geom_sf(data=ripgeo, fill="grey95", colour="grey60", linewidth=.15) +
    
    # --- MODIFICA INIZIA QUI ---
    
    # 1. Disegna i punti NON significativi (sotto)
    geom_sf(data = sf_obj |> dplyr::filter(Gi_bin == "Non signif"),
            colour="grey70", size=size_ns, alpha=alpha_ns, show.legend=FALSE) +
    
    # 2. Disegna i punti SIGNIFICATIVI (sopra)
    geom_sf(data = sf_obj |> dplyr::filter(Gi_bin != "Non signif"),
            aes(color=Gi_bin), size=size_sig, alpha=alpha_sig, show.legend="point") +
    
    # 3. Rimuovi "Non signif" dalla scala manuale (ora è gestito sopra)
    scale_color_manual(values=c(
      "Coldspot 99%"="#08306B","Coldspot 95%"="#4292C6",
      # "Non signif"="grey75", <- Rimosso
      "Hotspot 95%"="#FC4E2A","Hotspot 99%"="#99000D"
    )) +
    
    # --- MODIFICA FINISCE QUI ---
    
    guides(color = guide_legend(override.aes = list(size = 4))) +
    labs(title=titolo, color="Gi*") +
    theme_minimal(base_size=11) + theme(panel.grid=element_blank())
}
# 1.5 Leaflet rapidi
leaflet_lisa_map <- function(sf_obj, file_out) {
  sf_obj <- st_transform(sf_obj, 4326)
  m <- leaflet(sf_obj) %>% addTiles() %>%
    addCircleMarkers(
      color = ~case_when(
        LISA_cluster=="High-High" ~ "red",
        LISA_cluster=="Low-Low"   ~ "blue",
        LISA_cluster=="High-Low"  ~ "orange",
        LISA_cluster=="Low-High"  ~ "lightblue",
        TRUE                      ~ "grey"
      ),
      radius = 3, stroke = FALSE, fillOpacity = .6,
      popup = ~paste0(
        "<b>ISP (z):</b> ", round(ISP_std, 3), "<br>",
        "<b>Lag ISP:</b> ", round(lag_ISP_std, 3), "<br>",
        "<b>LISA I:</b> ", round(lisa_I, 3), "<br>",
        "<b>p-value:</b> ", signif(lisa_p, 3), "<br>",
        "<b>Cluster:</b> ", LISA_cluster
      )
    )
  save_html(m, file_out)
}

leaflet_gi_map <- function(sf_obj, file_out) {
  sf_obj <- st_transform(sf_obj, 4326)
  m <- leaflet(sf_obj) %>% addTiles() %>%
    addCircleMarkers(
      color = ~case_when(
        Gi_bin=="Hotspot 99%" ~ "#99000D",
        Gi_bin=="Hotspot 95%" ~ "#FC4E2A",
        Gi_bin=="Coldspot 99%" ~ "#08306B",
        Gi_bin=="Coldspot 95%" ~ "#4292C6",
        TRUE ~ "grey75"
      ),
      radius=3, stroke=FALSE, fillOpacity=.6,
      popup = ~paste0("<b>ISP (z):</b> ", round(ISP_std,3), "<br>",
                      "<b>Gi*:</b> ", round(Gi,3), "<br>",
                      "<b>Classif.:</b> ", Gi_bin)
    )
  save_html(m, file_out)
}

# 1.6 Plot coefficiente GWR (UNA SOLA VERSIONE, gradient2)
plot_gwr_coef <- function(sf_obj, coef_name, ripgeo,
                          title=paste("Coeff. GWR:", coef_name),
                          pal_low="#0571b0", pal_mid="white", pal_high="#ca0020",
                          limits=NULL, pt_size=.35, legend=TRUE) {
  if (!coef_name %in% names(sf_obj)) stop("Colonna mancante: ", coef_name)
  if (!is.numeric(sf_obj[[coef_name]])) stop("Colonna non numerica: ", coef_name)
  v <- sf_obj[[coef_name]]
  if (is.null(limits)) { max_abs <- max(abs(v), na.rm=TRUE); limits <- c(-max_abs, max_abs) }
  ggplot() +
    geom_sf(data=ripgeo, fill="grey95", colour="grey60", linewidth=.15) +
    geom_sf(data=sf_obj, aes(colour=.data[[coef_name]]),
            size=pt_size, alpha=.85, show.legend = if (legend) "point" else FALSE) +
    scale_colour_gradient2(low=pal_low, mid=pal_mid, high=pal_high, midpoint=0, limits=limits, name="Coef") +
    theme_minimal(base_size=11) + labs(title=title) +
    theme(panel.grid=element_blank(), legend.position=if (legend) "right" else "none")
}

# 1.7 Scelta k ottimale (criterio UNICO: combinato ΔMoran + ΔDistanza)
# Scegli k con criterio combinato (ΔMoran + ΔDistanza) e restituisci anche la tabella completa
scegli_k_knn <- function(imprese_cat,
                         k_min = 5,
                         k_max = NULL,          # se NULL, calcolato in base a n
                         step  = 2,
                         dist_fun = median,
                         alpha = 0.05,          # per filtrare su significatività
                         verbose = FALSE) {
  
  stopifnot("ISP_bn" %in% names(imprese_cat))
  coords <- as.matrix(sf::st_coordinates(imprese_cat))
  x      <- imprese_cat$ISP_bn
  n      <- nrow(imprese_cat)
  
  # Bound sicuri per k: mai oltre n-1 e n/3 (evita warning spdep)
  if (is.null(k_max)) k_max <- floor(n/3)
  k_max <- max(2, min(k_max, n - 1, floor(n/3)))
  k_min <- max(2, min(k_min, k_max))
  
  # Sequenza k; se troppo corta, crea almeno due punti contigui
  k_seq <- seq(k_min, k_max, by = step)
  if (length(k_seq) < 2) {
    k_seq <- unique(pmin(n - 1, c(k_min, min(k_max, k_min + 1))))
  }
  
  # Calcolo robusto: salta i k che dovessero fallire
  rows <- lapply(k_seq, function(k){
    out <- try({
      nb <- spdep::knn2nb(spdep::knearneigh(coords, k = k))
      lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
      mt <- spdep::moran.test(x, lw, zero.policy = TRUE)
      kd <- FNN::get.knn(coords, k = k)$nn.dist[, k]
      data.frame(k = k,
                 moran_i = mt$estimate[["Moran I statistic"]],
                 p_value = mt$p.value,
                 dist_k  = dist_fun(kd))
    }, silent = TRUE)
    if (inherits(out, "try-error")) return(NULL) else return(out)
  })
  res <- dplyr::bind_rows(rows)
  if (nrow(res) == 0) stop("[scegli_k_knn] nessun k valido nel range.")
  
  res <- dplyr::arrange(res, k)
  
  # Scaling 0-1 robusto
  scale01 <- function(v){
    out <- rep(NA_real_, length(v)); ok <- is.finite(v)
    if (sum(ok) > 1) {
      r <- range(v[ok]); out[ok] <- if (diff(r) == 0) 0 else (v[ok] - r[1]) / diff(r)
    }
    out
  }
  
  # Derivate e combo (criterio 'stability' → MIN)
  res <- res %>%
    dplyr::mutate(
      d_moran  = c(NA, abs(diff(moran_i))),
      d_dist   = c(NA, abs(diff(dist_k))),
      sc_moran = scale01(d_moran),
      sc_dist  = scale01(d_dist),
      combo    = sc_moran + sc_dist,
      sig      = p_value <= alpha
    )
  
  # Scegli il minimo della combo tra i k significativi; fallback sul minimo globale
  k_best <- {
    cand <- dplyr::filter(res, sig)
    if (nrow(cand) > 0) cand$k[which.min(cand$combo)] else res$k[which.min(res$combo)]
  }
  
  if (verbose) {
    message(sprintf("[scegli_k_knn] n=%d, range k=%d–%d, step=%d → k*=%d",
                    n, min(res$k), max(res$k), step, k_best))
  }
  
  list(best_k = k_best, risultati = res)
}

validazione_moransI_k_parallel <- function(sf_data, var_name="ISP_bn", k_seq=seq(5,max_k,2), workers=4){
  future::plan(multisession, workers=workers)
  coords <- st_coordinates(sf_data); varvec <- sf_data[[var_name]]
  out <- future.apply::future_lapply(k_seq, function(k){
    nb <- spdep::knn2nb(spdep::knearneigh(coords, k=k))
    lw <- spdep::nb2listw(nb, style="W", zero.policy=TRUE)
    mt <- spdep::moran.test(varvec, lw, zero.policy=TRUE)
    data.frame(k=k, moran_i=mt$estimate[["Moran I statistic"]], p=mt$p.value)
  }, future.seed = TRUE)
  do.call(rbind, out)
}

# Visualizza (i) Moran's I vs k, (ii) distanza k-esimo vicino, (iii) score combinato

plot_scelta_k <- function(k_out, titolo="Selezione k (criterio combinato)") {
  df <- k_out$risultati; k_best <- k_out$best_k
  p1 <- ggplot(df, aes(k, moran_i)) + geom_line() + geom_point(aes(color=p_value<.05)) +
    scale_color_manual(values=c(`TRUE`="#2ca25f", `FALSE`="#de2d26"), name="p < .05") +
    geom_vline(xintercept=k_best, linetype=2) + labs(title="Moran's I vs k", x="k", y="Moran's I") +
    theme_minimal(base_size=11)
  p2 <- ggplot(df, aes(k, dist_k)) + geom_line() + geom_point() +
    geom_vline(xintercept=k_best, linetype=2) + labs(title="Distanza al k-esimo vicino", x="k", y="Distanza") +
    theme_minimal(base_size=11)
  p3 <- ggplot(df, aes(k, combo)) + geom_line() + geom_point() +
    geom_vline(xintercept=k_best, linetype=2) +
    annotate("label", x=k_best, y=min(df$combo,na.rm=TRUE), label=paste0("k* = ", k_best),
             vjust=-0.5, size=3) +
    labs(title="Score combinato (Δ Diff Moran norm. + Δ Diff Distanza norm.)", x="k", y="Score") +
    theme_minimal(base_size=11)
  
  ((p1 + p2) / p3) + patchwork::plot_annotation(title = titolo)
}

# 1.8 Costruzione W unificata: STRICT / BORDER / HALO
# STRICT: k-NN calcolato solo sul subset
# BORDER: k-NN su tutto il dataset; si estrae la sotto-matrice riferita al subset
# HALO: STRICT + aggiunta k_add di vicini su confini (euristica semplice)
# DROP-IN: sostituisci la tua build_W_variants con questa
build_W_variants <- function(sf_all, sf_sub, k_best, halo_k_add = 5) {
  # chiavi e coordinate
  key_all <- sf::st_drop_geometry(sf_all)$codice_fiscale
  key_sub <- sf::st_drop_geometry(sf_sub)$codice_fiscale
  idx_sub <- match(key_sub, key_all)
  
  # jitter minimo per duplicati
  sf_all_j   <- jitter_coords(sf_all, 1e-4)
  coords_all <- sf::st_coordinates(sf_all_j)
  coords_sub <- sf::st_coordinates(sf_sub)
  
  n_all <- nrow(coords_all); n_sub <- nrow(coords_sub)
  k_s   <- max(1L, min(k_best, n_sub - 1L))
  k_b   <- max(1L, min(k_best, n_all - 1L))
  
  # STRICT: KNN sul subset
  nb_s <- spdep::knn2nb(spdep::knearneigh(coords_sub, k = k_s))
  W_strict <- spdep::nb2listw(nb_s, style = "W", zero.policy = TRUE)
  
  # BORDER: KNN sul full, proiettato sul subset (full-aware)
  nb_full <- spdep::knn2nb(spdep::knearneigh(coords_all, k = k_b))
  is_in_sub  <- rep(FALSE, n_all); is_in_sub[idx_sub] <- TRUE
  pos_in_sub <- integer(n_all);    pos_in_sub[idx_sub] <- seq_along(idx_sub)
  
  nb_b <- vector("list", length(idx_sub))
  edges_ext <- list(from = integer(0), to = integer(0))  # per plot: subset -> fuori
  for (ii in seq_along(idx_sub)) {
    i_full <- idx_sub[ii]
    nei    <- nb_full[[i_full]]
    in_keep  <- nei[ is_in_sub[nei] ]
    out_keep <- nei[!is_in_sub[nei] ]
    nb_b[[ii]] <- if (length(in_keep)) pos_in_sub[in_keep] else integer(0L)
    if (length(out_keep)) {
      edges_ext$from <- c(edges_ext$from, rep(ii, length(out_keep)))
      edges_ext$to   <- c(edges_ext$to,   out_keep)
    }
  }
  class(nb_b) <- "nb"; attr(nb_b, "region.id") <- as.character(key_sub)
  # sicurezza: garantisci ≥1 vicino usando strict come fallback
  nb_b_fix <- nb_b
  for (i in seq_along(nb_b_fix)) if (!length(nb_b_fix[[i]]) && length(nb_s[[i]])) nb_b_fix[[i]] <- nb_s[[i]][1L]
  W_border <- spdep::nb2listw(nb_b_fix, style = "W", zero.policy = TRUE)
  
  # edge list interni per plot
  edges_from_nb <- function(nb_obj) {
    from <- unlist(mapply(function(i, v) rep(i, length(v)), seq_along(nb_obj), nb_obj))
    to   <- unlist(nb_obj)
    list(from = from, to = to)
  }
  
  list(
    W_strict = W_strict,
    W_border = W_border,
    k_strict = k_s, k_border = k_s,         # per coerenza con i nomi nei salvataggi
    edges_strict_internal = edges_from_nb(nb_s),
    edges_border_internal = edges_from_nb(nb_b_fix),
    edges_external_border = edges_ext,      # solo per plot, opzionale
    coords_sub = coords_sub, coords_all = coords_all
  )
}

# 1.9 Analisi regressiva modelli (SAR(error),SDM(mixed Durbin),
#  Impatti(bootstrap R= 500), GMM(Spatial error-two-step))
analisi_regressiva_spaziale <- function(sf_obj, listw, vars_model) {
  # formula
  fml <- as.formula(paste("ISP_bn ~", paste(vars_model, collapse=" + ")))
  
  # SAR (error)
  sar <- spatialreg::errorsarlm(fml, data=sf_obj, listw=listw, zero.policy=TRUE)
  
  # SDM (mixed Durbin)
  sdm <- spatialreg::lagsarlm(fml, data=sf_obj, listw=listw, type="mixed", zero.policy=TRUE)
  
  # Impatti (bootstrap R=500)
  imp <- tryCatch(spatialreg::impacts(sdm, listw=listw, R=500), error=function(e) NULL)
  
  # GMM (spatial error – two-step)
  gmm <- spatialreg::GMerrorsar(fml, data=sf_obj, listw=listw, zero.policy=TRUE)
  
  list(sar=sar, sdm=sdm, impacts=imp, gmm=gmm)
}
#  Helper compatti (diagnostiche + gating GWR) 
residual_moran <- function(model, listw) {
  res <- residuals(model)
  mt  <- spdep::moran.test(res, listw=listw, zero.policy=TRUE)
  list(I = unname(mt$estimate[["Moran I statistic"]]), p = mt$p.value)
}
gwr_eligible <- function(n, moran_resid_sdm_p, aic_lm, aic_sdm,
                         min_n, max_n_gwr, alpha, aic_gain_min) {
  size_ok   <- (n >= min_n) && (n <= max_n_gwr)
  resid_sig <- !is.na(moran_resid_sdm_p) && (moran_resid_sdm_p <= alpha)
  aic_gain  <- is.finite(aic_lm) && is.finite(aic_sdm) && ((aic_lm - aic_sdm) >= aic_gain_min)
  list(ok = (size_ok && resid_sig && aic_gain),
       why = c(
         if (!size_ok)   "n fuori soglia"           else NULL,
         if (!resid_sig) "Moran(residui) non sig."  else NULL,
         if (!aic_gain)  "ΔAIC (OLS→SDM) insuff."   else NULL
       ) |> paste(collapse="; "))
}

#  Diagnostiche complete (lm/sarlm) 
#  Ricava un LM "base" dai parametri passati
get_baseline_lm <- function(model, data=NULL, formula=NULL) {
  if (!is.null(data) && !is.null(formula)) return(lm(formula, data=data))
  # fallback: prova a ricostruire da formula(model) se disponibile
  fml <- tryCatch(formula(model), error=function(e) NULL)
  dat <- tryCatch(model.frame(model), error=function(e) NULL)
  if (!is.null(fml) && !is.null(dat)) return(lm(fml, data = dat))
  NULL
}

diagnostiche_spaziali <- function(model, listw, data=NULL, formula=NULL, nsim=199, seed=123) {
  res <- tryCatch(residuals(model), error=function(e) NULL)
  out <- list()
  
  if (!is.null(res)) {
    # Autocorrelazione residui
    out$moran <- tryCatch(spdep::moran.test(res, listw=listw, zero.policy=TRUE), error=function(e) NULL)
    set.seed(seed)
    out$moran_mc <- tryCatch(spdep::moran.mc(res, listw=listw, nsim=nsim, zero.policy=TRUE), error=function(e) NULL)
    out$geary <- tryCatch(spdep::geary.test(res, listw=listw, zero.policy=TRUE), error=function(e) NULL)
    # Normalità (evita Shapiro su n grande)
    out$normality <- tryCatch(nortest::ad.test(res), error=function(e) NULL)
  }
  
  # Eteroschedasticità
  if (inherits(model, "sarlm")) {
    out$bp <- tryCatch(spatialreg::bptest.sarlm(model), error=function(e) NULL)
  } else if (inherits(model, "lm")) {
    out$bp <- tryCatch(lmtest::bptest(model), error=function(e) NULL)
  }
  
  # LM robust su OLS (sostituisce lm.LMtests deprecato)
  lm_base <- tryCatch(get_baseline_lm(model, data=data, formula=formula), error=function(e) NULL)
  if (!is.null(lm_base)) {
    out$lm_tests <- tryCatch(spdep::lm.RStests(lm_base, listw=listw), error=function(e) NULL)
    out$reset    <- tryCatch(lmtest::resettest(lm_base, power=2:3, type="fitted"), error=function(e) NULL)
  }
  
  out
}

# Funzione dump_diagnostics (Versione ultra-robusta)
dump_diagnostics <- function(diag, file, file_csv_summary = NULL) {
  
  # --- (Parte di scrittura file .txt invariata) ---
  dir.create(dirname(file), showWarnings=FALSE, recursive=TRUE)
  capture.output({
    cat("=== AUTOCORRELAZIONE RESIDUI ===\n")
    if (!is.null(diag$moran))    print(diag$moran)
    if (!is.null(diag$moran_mc)) print(diag$moran_mc)
    if (!is.null(diag$geary))    print(diag$geary)
    
    cat("\n=== NORMALITÀ ===\n")
    if (!is.null(diag$normality)) print(diag$normality)
    
    cat("\n=== ETEROSCHEDASTICITÀ ===\n")
    if (!is.null(diag$bp)) print(diag$bp)
    
    if (!is.null(diag$lm_tests)) {
      cat("\n=== LM ROBUST (su OLS) ===\n"); print(diag$lm_tests)
    }
    if (!is.null(diag$reset)) {
      cat("\n=== RESET (su OLS) ===\n"); print(diag$reset)
    }
  }, file=file)
  # --- (Fine parte .txt) ---
  
  
  # Mini-riepilogo CSV
  if (!is.null(file_csv_summary)) {
    
    # --- BLOCCO MODIFICATO (Versione 2, ultra-robusta) ---
    
    # Helper robusto: esegue un'espressione, restituisce un singolo NA_real_
    # in caso di qualsiasi fallimento (errore, NULL, lunghezza 0).
    val_or_na <- function(expr) {
      val <- suppressWarnings(try(expr, silent = TRUE))
      
      # Controlla tutti i possibili fallimenti
      if (inherits(val, "try-error") || is.null(val) || length(val) == 0) {
        return(NA_real_)
      }
      
      # Assicura che sia un singolo numero
      val <- as.numeric(val[1]) 
      if (is.na(val) || !is.finite(val)) {
        return(NA_real_)
      }
      return(val)
    }
    
    # Ora ogni chiamata a data.frame è protetta
    df <- data.frame(
      moran_I        = val_or_na(diag$moran$estimate[["Moran I statistic"]]),
      moran_p        = val_or_na(diag$moran$p.value),
      moran_mc_p     = val_or_na(diag$moran_mc$p.value),
      geary_C        = val_or_na(diag$geary$estimate[["Geary C statistic"]]),
      geary_p        = val_or_na(diag$geary$p.value),
      AD_p           = val_or_na(diag$normality$p.value),
      BP_p           = val_or_na(diag$bp$p.value),
      LMlag_robust_p = val_or_na(diag$lm_tests$RLMlag$p.value),
      LMerr_robust_p = val_or_na(diag$lm_tests$RLMerr$p.value),
      RESET_p        = val_or_na(diag$reset$p.value)
    )
    
    # --- FINE BLOCCO MODIFICATO ---
    
    utils::write.table(df, file_csv_summary, sep=",", row.names=FALSE, col.names=!file.exists(file_csv_summary), append=file.exists(file_csv_summary))
  }
  
  invisible(file)
}

# GWR (facoltativo: riduci samp_size se serve)
run_gwr <- function(sf_obj, vars_model, samp_size=Inf, seed=NULL) {
  set.seed(seed)
  if (nrow(sf_obj) > samp_size) sf_obj <- sf_obj[sample(seq_len(nrow(sf_obj)), samp_size), ]
  fml    <- as.formula(paste("ISP_bn ~", paste(vars_model, collapse=" + ")))
  coords <- as.matrix(st_coordinates(sf_obj))
  bw     <- spgwr::gwr.sel(fml, data=sf_obj, coords=coords, adapt=FALSE,
                           gweight=spgwr::gwr.Gauss, longlat=FALSE)
  gwr_m  <- spgwr::gwr(fml, data=sf_obj, coords=coords, bandwidth=bw,
                       hatmatrix=TRUE, gweight=spgwr::gwr.Gauss)
  coeff_sf <- cbind(sf_obj, as.data.frame(gwr_m$SDF))
  list(model=gwr_m, coeff_sf=coeff_sf, bw=bw)
}

# Ricostruisce i cluster per tutte le varianti e salva tidy + conteggi
# Ricostruisce i cluster per tutte le varianti e salva tidy + conteggi (LISA e Gi*)
make_clusters_tidy <- function(nm, alpha = 0.05) {
  
  # --- Caricamento (invariato) ---
  w_file <- file.path(output_dir,"01_rds", nm, "matrici_W.rds")
  sf_file <- file.path(output_dir,"01_rds", nm, "sf_strict_with_stats.rds")
  
  if (!file.exists(w_file) || !file.exists(sf_file)) {
    warning(paste("File non trovato:", nm, "- funzione make_clusters_tidy interrotta."))
    return(invisible(NULL))
  }
  
  w_variants <- readRDS(w_file)
  lis  <- readRDS(file.path(output_dir,"01_rds", nm, "lisa_results.rds"))
  sf_s <- readRDS(sf_file)
  Wv <- w_variants
  
  # --- Classify LISA (invariato) ---
  classify_lisa <- function(sf_base, lisa_mat, lw, alpha = 0.05) {
    sf_out <- sf_base
    lagx  <- spdep::lag.listw(lw, sf_out$ISP_bn)
    sf_out$ISP_std     <- as.numeric(scale(sf_out$ISP_bn))
    sf_out$lag_ISP_std <- as.numeric(scale(lagx))
    Ii  <- lisa_mat[,1]; p <- lisa_mat[,5]
    cl <- rep("Non signif", nrow(sf_out))
    cl[p<=alpha & Ii>0 & sf_out$ISP_std>0 & sf_out$lag_ISP_std>0] <- "High-High"
    cl[p<=alpha & Ii>0 & sf_out$ISP_std<0 & sf_out$lag_ISP_std<0] <- "Low-Low"
    cl[p<=alpha & Ii<0 & sf_out$ISP_std>0 & sf_out$lag_ISP_std<0] <- "High-Low"
    cl[p<=alpha & Ii<0 & sf_out$ISP_std<0 & sf_out$lag_ISP_std>0] <- "Low-High"
    sf_out$LISA_cluster <- factor(cl, levels=c("High-High","Low-Low","High-Low","Low-High","Non signif"))
    sf_out$lisa_I <- Ii; sf_out$lisa_p <- p
    sf_out
  }
  
  # --- Ricostruzione (invariato) ---
  sf_border <- classify_lisa(sf_s, lis$lisa_border, Wv$W_border, alpha)
  
  sf_s$Gi      <- as.numeric(spdep::localG(sf_s$ISP_bn, Wv$W_strict, zero.policy=TRUE))
  sf_border$Gi <- as.numeric(spdep::localG(sf_border$ISP_bn, Wv$W_border, zero.policy=TRUE))
  
  cut_gi <- function(v) cut(v, breaks=c(-Inf,-1.96,-1.65,1.65,1.96,Inf),
                            labels=c("Coldspot 99%","Coldspot 95%","Non signif","Hotspot 95%","Hotspot 99%"))
  
  sf_s$Gi_bin      <- cut_gi(sf_s$Gi); sf_s$variante      <- "W_strict"
  sf_border$Gi_bin <- cut_gi(sf_border$Gi); sf_border$variante <- "W_border"
  
  # --- Creazione sf_long (invariato) ---
  keep_cols <- c("codice_fiscale","macroarea","dimensione_impresa","ISP_bn","ISP_std","lag_ISP_std",
                 "lisa_I","lisa_p","LISA_cluster","Gi","Gi_bin","geometry", "variante")
  
  sf_long <- dplyr::bind_rows(
    sf_s |> dplyr::select(any_of(keep_cols)),
    sf_border |> dplyr::select(any_of(keep_cols))
  )
  sf_long$variante <- factor(sf_long$variante, levels=c("W_strict","W_border"))
  
  saveRDS(sf_long, file.path(output_dir,"01_rds", nm, "clusters_tidy_all_variants.rds"))
  
  # --- INIZIO MODIFICA: CONTEGGI SEPARATI ---
  
  # 1. Conteggi per tabella LISA
  counts_lisa <- sf_long |>
    sf::st_drop_geometry() |>
    dplyr::count(variante, LISA_cluster) |>
    tidyr::pivot_wider(names_from=LISA_cluster, values_from=n, values_fill=0) |>
    dplyr::arrange(variante)
  
  dir.create(file.path(output_dir,"03_tables", nm), showWarnings = FALSE, recursive = TRUE)
  # Rinomina il file per chiarezza
  write.csv(counts_lisa, file.path(output_dir,"03_tables", nm, "lisa_cluster_counts.csv"), row.names=FALSE)
  
  # 2. Conteggi per tabella Gi* (NUOVO BLOCCO)
  counts_gi <- sf_long |>
    sf::st_drop_geometry() |>
    # Assicura che tutti i livelli siano presenti anche se 0
    dplyr::mutate(Gi_bin = factor(Gi_bin, levels = c("Hotspot 99%", "Hotspot 95%", "Non signif", "Coldspot 95%", "Coldspot 99%"))) |>
    dplyr::count(variante, Gi_bin, .drop = FALSE) |> # .drop=FALSE mantiene i conteggi 0
    tidyr::pivot_wider(names_from=Gi_bin, values_from=n, values_fill=0) |>
    dplyr::arrange(variante)
  
  # Salva il nuovo file Gi*
  write.csv(counts_gi, file.path(output_dir,"03_tables", nm, "gi_cluster_counts.csv"), row.names=FALSE)
  
  # --- FINE MODIFICA ---
  
  invisible(list(sf_long = sf_long, counts_lisa = counts_lisa, counts_gi = counts_gi))
}
# Codice per il Confronto W_Strict vs W_Border (Subset) 
# --- Confronto W_Strict vs W_Border (Ordine di plot corretto) ---
plot_W_comparison_full <- function(nm, ripgeo_tr) {
  base_dir <- file.path(output_dir, "01_rds", nm)
  wfile    <- file.path(base_dir, "matrici_W.rds")
  efile    <- file.path(base_dir, "edges_plot.rds")
  
  # Caricamento robusto...
  if (!file.exists(wfile) || !file.exists(efile)) {
    message(sprintf("[%s] File mancanti (W o Edges), plot saltato.", nm))
    return(invisible(NULL))
  }
  W <- readRDS(wfile); E <- readRDS(efile)
  if (is.null(E$coords_sub)) {
    message(sprintf("[%s] Coordinate mancanti (E$coords_sub), plot saltato.", nm))
    return(invisible(NULL))
  }
  coords_sub <- E$coords_sub; coords_all <- E$coords_all 
  
  # Palette e spessori (come da tua modifica)
  col_strict <- grDevices::rgb(0, 90, 181, 70, maxColorValue = 255)  # blu (alpha 70)
  col_border <- grDevices::rgb(0, 135, 90, 70, maxColorValue = 255)  # verde (alpha 70)
  lwd_internal <- 0.15 
  col_outer  <- grDevices::rgb(0, 0, 0, 80, maxColorValue = 255)      # grigio (alpha 80)
  lwd_external <- 0.3  
  col_points <- grDevices::rgb(0, 0, 0, 50, maxColorValue = 255)
  
  # Salvataggio...
  out_pdf <- file.path(output_dir, "02_maps", nm, "W_Comparison_Strict_Border.pdf")
  dir.create(dirname(out_pdf), recursive = TRUE, showWarnings = FALSE)
  
  grDevices::pdf(out_pdf, width = 10, height = 12, useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mar=c(0,0,3,0), bg="white")
  
  # 1) Confini
  plot(sf::st_geometry(ripgeo_tr), border = "grey45", col = NA, lwd = 0.4, axes = FALSE)
  
  # 2) Punti
  points(coords_sub, pch = 16, cex = 0.3, col = col_points)
  
  # --- INIZIO MODIFICA: ORDINE DI PLOT INVERTITO ---
  
  # 3) Archi STRICT (blu, sottili) - LA BASE "ARTIFICIALE", SOTTO
  es <- if (!is.null(E)) E$edges_strict_internal else NULL
  if (!is.null(es) && length(es$from)) {
    segments(coords_sub[es$from,1], coords_sub[es$from,2],
             coords_sub[es$to,  1], coords_sub[es$to,  2],
             col=col_strict, lwd=lwd_internal) # <-- BLU DISEGNATO PRIMA
  }
  
  # 4) Archi BORDER (verde, sottili) - LA BASE "REALE", SOPRA
  eb <- if (!is.null(E)) E$edges_border_internal else NULL
  if (!is.null(eb) && length(eb$from)) {
    segments(coords_sub[eb$from,1], coords_sub[eb$from,2],
             coords_sub[eb$to,  1], coords_sub[eb$to,  2],
             col=col_border, lwd=lwd_internal) # <-- VERDE DISEGNATO DOPO
  }
  
  # --- FINE MODIFICA ---
  
  # 5) Archi ESTERNI (grigio, più spessi)
  ef <- E$edges_external_border
  if (!is.null(E) && !is.null(ef) && length(ef$from) && !is.null(coords_all)) {
    segments(coords_sub[ef$from,1], coords_sub[ef$from,2],
             coords_all[ef$to,  1], coords_all[ef$to,  2],
             col=col_outer, lwd=lwd_external, lty=3) 
  }
  
  # 6) Titolo
  k_show <- if (!is.null(W$k_strict)) W$k_strict else NA_integer_
  title(main = sprintf("W–Matrix Comparison (k=%s) – %s", as.character(k_show), nm), cex.main=1.05)
  mtext(side=3, line=0.2, adj=1, 
        text="Interni: strict(blu)/border(verde) — Esterni: border(grigio)", 
        cex=.8, col="grey30")
}
