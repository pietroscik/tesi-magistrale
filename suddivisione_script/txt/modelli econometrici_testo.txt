##### FASE 4: MODELLI ECONOMETRICI (NON-SPAZIALI) #####

cat("\n===== INIZIO FASE 4: MODELLI ECONOMETRICI NON-SPAZIALI =====\n")

# ------------------ Pacchetti e utility install -----------------------------
packages <- c(
  "leaflet.extras", "leaflet", "ggplot2", "psych", "openxlsx", "dplyr", 
  "readxl", "plotly", "pheatmap", "tidyr", "purrr", "MASS", "reshape2", 
  "DT", "nortest", "bestNormalize", "e1071", "moments", "rstatix", 
  "car", "lmtest", "corrplot", "tidyverse","irr","glmnet","kableExtra",
  "sandwich","caret","sf","here","broom","stringr","writexl"
)

install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

invisible(lapply(packages, install_and_load))

# ------------------ Path di lavoro ------------------------------------------
input_dir  <- here::here("03_validation")
output_dir <- here::here("04_econometric_models")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ------------------ Dati di input -------------------------------------------
cat("Caricamento dataset_completo da fase precedente...\n")
dataset_completo <- readRDS(file.path(input_dir, "matrice.rds"))
categorie_variabili <- readRDS(file.path(input_dir, "categorie_variabili.rds"))

# ------------------ Trasformazioni e fattori --------------------------------
dataset_completo <- dataset_completo %>%
  dplyr::mutate(
    log_ricavi_vendite_migl_EUR = log(ricavi_vendite_migl_EUR + 1),
    log_EBITDA_migl_EUR         = log(EBITDA_migl_EUR + 1),
    macroarea                   = as.factor(macroarea),
    integrazione_verticale_fatt = cut(
      integrazione_verticale,
      breaks = quantile(integrazione_verticale, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Molto Basso", "Basso", "Medio", "Alto", "Molto Alto")
    )
  )

categorie_variabili$Log <- c("log_ricavi_vendite_migl_EUR", "log_EBITDA_migl_EUR")

# ------------------ Campionamento (per modello di sviluppo) -----------------
cat("Campionamento dataset (n=5000)...\n")
set.seed(2025)
dataset_sample <- dplyr::sample_n(dataset_completo, 5000)

# ------------------ Variabili modello (coerenti col resto della tesi) -------
cat("Preparazione variabili indipendenti...\n")
blocchi_num <- c("Redditività","Solidità","Produttività","Liquidità",
                 "Capitale_Circolante","Rischio_Finanziario","Log","Performance")
vars_num <- unlist(categorie_variabili[blocchi_num], use.names = FALSE)
vars_cat <- c("integrazione_verticale_fatt","macroarea")
vars_model <- c("ISP_bn", vars_num,vars_cat)  # includo esplicitamente anche le categoriche

# ------------------ Utilities: VIF & p-value filters ------------------------
# --- DROP-IN: filtro VIF robusto (fattori + interazioni, anti-loop) ----------
filter_vif <- function(model, threshold = 5, max_iter = 50) {
  data <- model$model
  resp <- all.vars(formula(model))[1]
  terms_now <- attr(terms(model), "term.labels")
  
  # normalizzatore: rimuove "as.factor(...)" per confronto nomi
  norm_term <- function(x) gsub("^as\\.factor\\(|\\)$", "", x)
  
  iter <- 0
  repeat {
    iter <- iter + 1
    if (iter > max_iter || length(terms_now) == 0) break
    
    frm <- as.formula(paste(resp, "~", paste(terms_now, collapse = " + ")))
    m   <- lm(frm, data = data)
    
    v <- tryCatch(car::vif(m), error = function(e) NA)
    
    # gestisci GVIF (matrice) -> usa GVIF^(1/(2*Df)) come metrica comparabile
    if (is.matrix(v)) {
      if ("GVIF^(1/(2*Df))" %in% colnames(v)) {
        vnum <- v[, "GVIF^(1/(2*Df))"]
      } else if (all(c("GVIF", "Df") %in% colnames(v))) {
        vnum <- v[, "GVIF"]^(1 / (2 * v[, "Df"]))
      } else {
        vnum <- rep(NA_real_, nrow(v)); names(vnum) <- rownames(v)
      }
      names(vnum) <- rownames(v)
      v <- vnum
    }
    
    # se tutti NA/Inf o sotto soglia -> esci
    if (all(!is.finite(v)) || max(v, na.rm = TRUE) < threshold) break
    
    # prendi il peggiore e normalizza il nome
    worst_name <- names(which.max(v))
    worst_base <- norm_term(worst_name)
    
    # rimuovi il termine base e QUALSIASI interazione che lo contenga
    tn_norm <- norm_term(terms_now)
    drop_idx <- grepl(paste0("(^|:)", worst_base, "(:|$)"), tn_norm)
    
    # safety: se non ha trovato match, prova match esatto; se niente, fermati
    if (!any(drop_idx)) {
      drop_idx <- terms_now == worst_name
      if (!any(drop_idx)) break
    }
    
    terms_now <- terms_now[!drop_idx]
  }
  
  # modello finale (se nessun termine rimasto -> solo intercetta)
  frm_final <- as.formula(
    paste(resp, "~", if (length(terms_now)) paste(terms_now, collapse = " + ") else 1)
  )
  m_final <- lm(frm_final, data = data)
  
  list(
    model_final = m_final,
    terms = terms_now,
    vifs = tryCatch(car::vif(m_final), error = function(e) NA)
  )
}

remove_high_pval <- function(model, pval_thresh = 0.10, max_iter = 50, keep_n_min = 1) {
  data_all <- model$model
  resp <- all.vars(formula(model))[1]
  
  iter <- 0
  repeat {
    iter <- iter + 1
    if (iter > max_iter) break
    
    terms_now <- attr(terms(model), "term.labels")
    if (length(terms_now) < keep_n_min) break
    
    frm_now <- as.formula(paste(resp, "~", paste(terms_now, collapse = " + ")))
    m_now   <- lm(frm_now, data = data_all)
    
    sm <- summary(m_now)$coefficients
    if (is.null(sm)) break
    
    # matrix di progetto e assegnazioni (0 = intercetta)
    mm  <- model.matrix(m_now)
    asg <- attr(mm, "assign")
    
    # allinea rownames(sm) con colonne di mm
    coef_names <- colnames(mm)
    idx <- match(rownames(sm), coef_names)
    pvals <- sm[, "Pr(>|t|)"]
    
    # term labels correnti
    tl <- attr(terms(m_now), "term.labels")
    term_ids <- unique(asg[asg > 0])
    
    # p-value di termine = max p delle colonne appartenenti a quel termine
    term_p <- rep(NA_real_, length(tl)); names(term_p) <- tl
    for (k in term_ids) {
      cols_k <- which(asg == k)
      rows_k <- which(idx %in% cols_k)
      pk <- suppressWarnings(pvals[rows_k])
      if (length(pk)) term_p[k] <- max(pk, na.rm = TRUE)
    }
    
    # stop se nulla da rimuovere
    if (all(!is.finite(term_p)) || all(term_p <= pval_thresh, na.rm = TRUE)) break
    
    # peggiore
    worst_term <- names(which.max(term_p))
    worst_p    <- max(term_p, na.rm = TRUE)
    if (!is.finite(worst_p) || worst_p <= pval_thresh) break
    
    # rimuovi termine peggiore
    terms_new <- setdiff(terms_now, worst_term)
    if (length(terms_new) < keep_n_min) break
    
    frm_new <- as.formula(paste(resp, "~", paste(terms_new, collapse = " + ")))
    message("p alto: rimuovo termine '", worst_term, "' (p_max = ", signif(worst_p, 3), ")")
    model <- lm(frm_new, data = data_all)
  }
  
  list(
    model_final = model,
    terms       = if (exists("terms_new")) terms_new else attr(terms(model), "term.labels")
  )
}
# ------------------ Diagnostica uniforme (sample, full, gruppi) -------------
.compute_diagnostics <- function(model, df, group_label){
  res <- residuals(model); fit <- fitted(model)
  n <- nobs(model); p <- length(coef(model))
  jb <- tryCatch(tseries::jarque.bera.test(res), error = function(e) NULL)
  sh <- if (n <= 5000) tryCatch(shapiro.test(res), error = function(e) NULL) else NULL
  ad <- tryCatch(nortest::ad.test(res), error = function(e) NULL)
  bp <- tryCatch(lmtest::bptest(model), error = function(e) NULL)
  bp_white <- tryCatch(lmtest::bptest(model, ~ poly(fit, 2, raw = TRUE)), error = function(e) NULL)
  ncv <- tryCatch(car::ncvTest(model), error = function(e) NULL)
  dw  <- tryCatch(lmtest::dwtest(model), error = function(e) NULL)
  rs  <- tryCatch(lmtest::resettest(model, power = 2:3, type = "fitted"), error = function(e) NULL)
  rb  <- tryCatch(lmtest::raintest(model), error = function(e) NULL)
  ck  <- tryCatch(cooks.distance(model), error = function(e) rep(NA_real_, n))
  hv  <- tryCatch(hatvalues(model), error = function(e) rep(NA_real_, n))
  thr_cook <- 4 / n; thr_hat <- 2 * p / n
  
  tibble::tibble(
    gruppo = group_label,
    test = c("Jarque-Bera","Shapiro-Wilk (n<=5000)","Anderson-Darling",
             "Breusch-Pagan","White-like (BP poly(fit,2))","ncvTest (score)",
             "Durbin-Watson","RESET (2:3)","Rainbow",
             "Max Cook's D","N(Cook>4/n)","N(hat>2p/n)"),
    statistic = c(
      if (!is.null(jb)) unname(jb$statistic) else NA_real_,
      if (!is.null(sh)) unname(sh$statistic) else NA_real_,
      if (!is.null(ad)) unname(ad$statistic) else NA_real_,
      if (!is.null(bp)) unname(bp$statistic) else NA_real_,
      if (!is.null(bp_white)) unname(bp_white$statistic) else NA_real_,
      if (!is.null(ncv)) unname(ncv$ChiSquare) else NA_real_,
      if (!is.null(dw)) dw$statistic[[1]] else NA_real_,
      if (!is.null(rs)) unname(rs$statistic) else NA_real_,
      if (!is.null(rb)) unname(rb$statistic) else NA_real_,
      if (all(is.na(ck))) NA_real_ else max(ck, na.rm = TRUE),
      if (all(is.na(ck))) NA_real_ else sum(ck > thr_cook, na.rm = TRUE),
      if (all(is.na(hv))) NA_real_ else sum(hv > thr_hat, na.rm = TRUE)
    ),
    p_value = c(
      if (!is.null(jb)) jb$p.value else NA_real_,
      if (!is.null(sh)) sh$p.value else NA_real_,
      if (!is.null(ad)) ad$p.value else NA_real_,
      if (!is.null(bp)) bp$p.value else NA_real_,
      if (!is.null(bp_white)) bp_white$p.value else NA_real_,
      if (!is.null(ncv)) ncv$p else NA_real_,
      if (!is.null(dw)) dw$p.value else NA_real_,
      if (!is.null(rs)) rs$p.value else NA_real_,
      if (!is.null(rb)) rb$p.value else NA_real_,
      NA_real_, NA_real_, NA_real_
    )
  )
}

# ------------------ PREP stepwise su CAMPIONE --------------------------------
cat("Esecuzione stepwise regression (campione)...\n")
variabili_ind_sample <- dataset_sample %>% dplyr::select(all_of(vars_model))

# scale solo numeriche tranne la risposta
vars_numeriche <- names(dataset_completo)[sapply(dataset_completo, is.numeric)]
vars_ind_numeriche <- intersect(vars_model, vars_numeriche)
num_cols_sample <- intersect(names(variabili_ind_sample), setdiff(vars_ind_numeriche, "ISP_bn"))

variabili_scaled <- as.data.frame(variabili_ind_sample)
if (length(num_cols_sample)) {
  variabili_scaled[num_cols_sample] <- scale(variabili_scaled[num_cols_sample])
}

model_null <- lm(ISP_bn ~ 1, data = variabili_scaled)
model_full <- lm(ISP_bn ~ ., data = variabili_scaled)
model_stepwise <- MASS::stepAIC(model_null, scope = list(upper = model_full), direction = "both")

cat("Risultati modello stepwise (campione):\n")
print(summary(model_stepwise))
print(car::vif(model_stepwise))

cat("Filtro VIF (campione)...\n")
result <- filter_vif(model_stepwise, threshold = 3)
print(summary(result$model_final))
print(result$vifs)

cat("Filtro p-value (campione)...\n")
result_pval <- remove_high_pval(model = result$model_final, pval_thresh = 0.10)
model_reduced <- result_pval$model_final
print(summary(model_reduced))

# diagnostici grafici campione
cat("Salvataggio plot diagnostici (campione)...\n")
png(file.path(output_dir, "diagnostic_plots_sample.png"), width = 2000, height = 2000, res = 300)
par(mfrow = c(2, 2)); plot(model_reduced); dev.off()

# metriche campione
summary_sample <- summary(model_reduced)
model_metrics_sample <- data.frame(
  Metric = c("R-squared", "Adjusted R-squared", "AIC", "BIC", "F-statistic", "Model p-value", "N osservazioni"),
  Value = c(
    round(summary_sample$r.squared, 4),
    round(summary_sample$adj.r.squared, 4),
    round(AIC(model_reduced), 2),
    round(BIC(model_reduced), 2),
    round(summary_sample$fstatistic[1], 2),
    signif(pf(summary_sample$fstatistic[1], summary_sample$fstatistic[2], summary_sample$fstatistic[3], lower.tail = FALSE), 4),
    nobs(model_reduced)
  )
)

# diagnostici numerici uniformi (campione)
tests_sample <- .compute_diagnostics(model_reduced, variabili_scaled, "SAMPLE")

# ------------------ Coefficienti ROBUSTI (HC3) campione ---------------------
tidy_robust <- function(model) {
  Vhc3 <- sandwich::vcovHC(model, type = "HC3")
  ct   <- lmtest::coeftest(model, vcov = Vhc3)
  tb   <- broom::tidy(ct)
  # CI robuste con t critico classico
  se <- tb$std.error
  tcrit <- qt(0.975, df = nobs(model) - length(coef(model)))
  tb$conf.low  <- tb$estimate - tcrit*se
  tb$conf.high <- tb$estimate + tcrit*se
  tb
}

coef_hc3_sample <- tidy_robust(model_reduced)
write.csv(coef_hc3_sample, file.path(output_dir, "Coefficienti_robusti_HC3_sample.csv"), row.names = FALSE)
# ------------------ STEPWISE su DATASET COMPLETO -----------------------------
cat("Stepwise regression su dataset completo...\n")
variabili_ind_full <- dataset_completo %>% dplyr::select(all_of(vars_model))

# scaling numeriche (corretto: uso names(variabili_ind_full))
cols_full <- setdiff(names(variabili_ind_full), c("ISP_bn","EBITDA_migl_EUR","ricavi_vendite_migl_EUR"))
cols_full_num <- cols_full[sapply(variabili_ind_full[cols_full], is.numeric)]

variabili_scaled_full <- as.data.frame(variabili_ind_full)
if (length(cols_full_num)) {
  variabili_scaled_full[cols_full_num] <- scale(variabili_scaled_full[cols_full_num])
}

model_null_full <- lm(ISP_bn ~ 1, data = variabili_scaled_full)
model_full_full <- lm(ISP_bn ~ ., data = variabili_scaled_full)
model_stepwise_full <- MASS::stepAIC(model_null_full, scope = list(upper = model_full_full), direction = "both")

print(summary(model_stepwise_full))
print(car::vif(model_stepwise_full))

result_full <- filter_vif(model_stepwise_full, threshold = 3)
print(summary(result_full$model_final))
print(result_full$vifs)

result_pval_full <- remove_high_pval(model = result_full$model_final, pval_thresh = 0.10)
model_reduced_full <- result_pval_full$model_final
print(summary(model_reduced_full))
print(car::vif(model_reduced_full))

# diagnostici grafici full
cat("Salvataggio plot diagnostici (completo)...\n")
png(file.path(output_dir, "diagnostic_plots_full.png"), width = 2000, height = 2000, res = 300)
par(mfrow = c(2, 2)); plot(model_reduced_full); dev.off()

# metriche full
summary_full <- summary(model_reduced_full)
model_metrics_full <- data.frame(
  Metric = c("R-squared", "Adjusted R-squared", "AIC", "BIC", "F-statistic", "Model p-value", "N osservazioni"),
  Value = c(
    round(summary_full$r.squared, 4),
    round(summary_full$adj.r.squared, 4),
    round(AIC(model_reduced_full), 2),
    round(BIC(model_reduced_full), 2),
    round(summary_full$fstatistic[1], 2),
    signif(pf(summary_full$fstatistic[1], summary_full$fstatistic[2], summary_full$fstatistic[3], lower.tail = FALSE), 4),
    nobs(model_reduced_full)
  )
)

# diagnostici numerici uniformi (full)
tests_full <- .compute_diagnostics(model_reduced_full, variabili_scaled_full, "FULL")

# Coefficienti ROBUSTI (HC3) full
coef_hc3_full <- tidy_robust(model_reduced_full)
write.csv(coef_hc3_full, file.path(output_dir, "Coefficienti_robusti_HC3_full.csv"), row.names = FALSE)

# ------------------ Confronti campione vs full -------------------------------
cat("Confronto tra modelli (campione vs completo)...\n")
model_comparison <- data.frame(
  Metrica = model_metrics_sample$Metric,
  `Modello su Campione` = model_metrics_sample$Value,
  `Modello su Dataset Completo` = model_metrics_full$Value
)
kable(model_comparison, digits = 4, caption = "Confronto: stepwise campione vs completo") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

coef_campione <- broom::tidy(model_reduced) %>% dplyr::mutate(Modello = "Campione")
coef_completo <- broom::tidy(model_reduced_full) %>% dplyr::mutate(Modello = "Completo")
coef_comparison <- dplyr::bind_rows(coef_campione, coef_completo) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::select(Modello, term, estimate, std.error, p.value) %>%
  dplyr::arrange(term)
kable(coef_comparison, digits = 4, caption = "Confronto coefficienti stimati (senza intercetta)") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))

# ------------------ Cook's distance (sample & full) --------------------------
plot_save_cooks <- function(model, file_png, main_title = "Cook's distance") {
  cks <- cooks.distance(model); n <- nobs(model)
  thr1 <- 4/n
  thr2 <- mean(cks, na.rm=TRUE) + 3*sd(cks, na.rm=TRUE)
  png(file.path(output_dir, file_png), width = 1200, height = 650, res = 120)
  par(mar = c(5,5,3,2))
  plot(cks, pch = 16, cex = .6, ylab = "cooks", xlab = "Index", main = main_title)
  abline(h = thr1, col = "red", lty = 2)
  abline(h = thr2, col = "red", lty = 3)
  lab_idx <- order(cks, decreasing = TRUE)[1:8]
  text(lab_idx, cks[lab_idx], labels = lab_idx, pos = 3, cex = .9)
  dev.off()
}
plot_save_cooks(model_reduced,       "cooks_sample.png", "Cook's distance – sample")
plot_save_cooks(model_reduced_full,  "cooks_full.png",   "Cook's distance – full")

# ------------------ Analisi per DIMENSIONE d'impresa -------------------------
cat("Analisi per sottogruppo di dimensione impresa...\n")
ADD_INTERACTION <- TRUE  # include cand. integrazione_verticale_fatt:macroarea

run_stepwise_block <- function(df, group_label) {
  cat(">> Stepwise su gruppo:", group_label, " | n =", nrow(df), "\n")
  stopifnot("ISP_bn" %in% vars_model)
  
  # selezione colonne coerente con vars_model
  variabili_ind <- df %>% dplyr::select(all_of(vars_model))
  
  # scaling delle sole numeriche (escludo risposta e grandezze da lasciare in scala)
  exclude_no_scale <- intersect(c("ISP_bn","EBITDA_migl_EUR","ricavi_vendite_migl_EUR"),
                                names(variabili_ind))
  cols_candidate <- setdiff(names(variabili_ind), exclude_no_scale)
  cols_to_scale <- cols_candidate[sapply(variabili_ind[cols_candidate], is.numeric)]
  
  variabili_scaled <- variabili_ind
  if (length(cols_to_scale)) variabili_scaled[cols_to_scale] <- scale(variabili_scaled[cols_to_scale])
  
  model_null_g <- lm(ISP_bn ~ 1, data = variabili_scaled)
  if (ADD_INTERACTION && all(c("integrazione_verticale_fatt","macroarea") %in% names(variabili_scaled))) {
    fm_upper <- as.formula("ISP_bn ~ . + as.factor(integrazione_verticale_fatt)*as.factor(macroarea)")
  } else {
    fm_upper <- as.formula("ISP_bn ~ .")
  }
  model_full_g <- lm(fm_upper, data = variabili_scaled)
  model_step_g <- MASS::stepAIC(model_null_g, scope = list(upper = model_full_g), direction = "both", trace = FALSE)
  
  res_vif <- filter_vif(model_step_g, threshold = 3)
  model_after_vif <- res_vif$model_final
  res_pval <- remove_high_pval(model = model_after_vif, pval_thresh = 0.10)
  model_reduced_g <- res_pval$model_final
  
  # diagnostici base
  png(file.path(output_dir, paste0("diagnostic_plots_", group_label, ".png")),
      width = 2000, height = 2000, res = 300)
  par(mfrow = c(2, 2)); plot(model_reduced_g); dev.off()
  
  # metriche
  s <- summary(model_reduced_g)
  metrics <- tibble::tibble(
    dimensione_impresa = group_label,
    `R-squared` = round(s$r.squared, 4),
    `Adj. R-squared` = round(s$adj.r.squared, 4),
    AIC = round(AIC(model_reduced_g), 2),
    BIC = round(BIC(model_reduced_g), 2),
    `F-statistic` = round(s$fstatistic[1], 2),
    `Model p-value` = signif(pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE), 4),
    `N osservazioni` = nobs(model_reduced_g)
  )
  
  # coefficienti OLS e VIF finali
  coefs <- broom::tidy(model_reduced_g) %>% dplyr::mutate(dimensione_impresa = group_label)
  vifs_final <- tryCatch(car::vif(model_reduced_g), error = function(e) NA)
  vifs_tbl <- if (all(is.na(vifs_final))) {
    tibble::tibble(term = NA_character_, VIF = NA_real_, dimensione_impresa = group_label)
  } else {
    tibble::tibble(term = names(vifs_final), VIF = as.numeric(vifs_final), dimensione_impresa = group_label)
  }
  
  # coefficienti robusti HC3
  VHC3 <- sandwich::vcovHC(model_reduced_g, type = "HC3")
  ct   <- lmtest::coeftest(model_reduced_g, vcov. = VHC3)
  ci   <- lmtest::coefci(model_reduced_g, vcov. = VHC3, level = 0.95)
  coefs_robust <- tibble::tibble(
    term               = rownames(ct),
    estimate           = unname(ct[,1]),
    std.error_robust   = unname(ct[,2]),
    statistic_robust   = unname(ct[,3]),
    p.value_robust     = unname(ct[,4]),
    conf.low_robust    = ci[,1],
    conf.high_robust   = ci[,2],
    dimensione_impresa = group_label
  )
  
  tests_tbl <- .compute_diagnostics(model_reduced_g, variabili_scaled, group_label)
  
  list(
    model = model_reduced_g,
    metrics = metrics,
    coefs = coefs,
    vifs = vifs_tbl,
    tests = tests_tbl,
    coefs_robust = coefs_robust
  )
}

dataset_split <- split(dataset_completo, dataset_completo$dimensione_impresa)
results_list <- purrr::imap(dataset_split, ~ run_stepwise_block(.x, .y))

# aggregazioni
all_metrics      <- dplyr::bind_rows(lapply(results_list, `[[`, "metrics"))
all_coefs        <- dplyr::bind_rows(lapply(results_list, `[[`, "coefs"))
all_vifs         <- dplyr::bind_rows(lapply(results_list, `[[`, "vifs"))
all_tests        <- dplyr::bind_rows(lapply(results_list, `[[`, "tests"))
all_coefs_robust <- dplyr::bind_rows(lapply(results_list, `[[`, "coefs_robust"))

# oggetti compatibili col vecchio flusso
nomi_dimensioni <- names(dataset_split)
modelli_per_dim <- setNames(lapply(results_list, `[[`, "model"), nomi_dimensioni)
model_fit_stepwise <- purrr::imap_dfr(
  modelli_per_dim,
  ~ broom::glance(.x) %>% dplyr::mutate(dimensione_impresa = .y)
) %>% dplyr::select(dimensione_impresa, r.squared, adj.r.squared, AIC, BIC, n = df.residual)
vars_per_dim <- lapply(modelli_per_dim, function(m) attr(terms(m), "term.labels"))
vars_model <- attr(terms(model_reduced_full), "term.labels")
# ------------------ Tabelle wide & export workbook --------------------------
coef_wide <- all_coefs %>%
  dplyr::select(term, estimate, dimensione_impresa) %>%
  tidyr::pivot_wider(names_from = dimensione_impresa, values_from = estimate)

pval_wide <- all_coefs %>%
  dplyr::select(term, p.value, dimensione_impresa) %>%
  tidyr::pivot_wider(names_from = dimensione_impresa, values_from = p.value, names_prefix = "pval_")

coef_summary <- dplyr::left_join(coef_wide, pval_wide, by = "term")

# workbook unico
writexl::write_xlsx(
  list(
    "metrics_by_dim"     = all_metrics,
    "coefs_long"         = all_coefs,
    "coefs_summary"      = coef_summary,
    "vif_final"          = all_vifs,
    "diagnostic_tests"   = all_tests,
    "coefs_robust_HC3"   = all_coefs_robust,
    "fit_by_dim_glance"  = model_fit_stepwise,
    "vars_per_dim"       = tibble::tibble(
      dimensione_impresa = rep(names(vars_per_dim), times = lengths(vars_per_dim)),
      term = unlist(vars_per_dim, use.names = FALSE)
    ),
    "vars_model_full"    = tibble::tibble(term = vars_model),
    "diagnostics_SAMPLE" = tests_sample,
    "diagnostics_FULL"   = tests_full
  ),
  path = file.path(output_dir, "stepwise_by_dimensione.xlsx")
)

# ------------------ Grafici riepilogo coefficienti --------------------------
coef_df_stepwise <- all_coefs

# (A) grafico lungo (tutto, rotato)
g_long <- coef_df_stepwise %>%
  dplyr::filter(term != "(Intercept)") %>%
  ggplot2::ggplot(aes(x = reorder(term, estimate), y = estimate, fill = dimensione_impresa)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::coord_flip() +
  ggplot2::labs(title = "Confronto coefficienti stimati per dimensione impresa (Stepwise)",
                x = "Variabile", y = "Valore coefficiente (β, standardizzati)") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "right")

ggplot2::ggsave(file.path(output_dir, "coef_per_dimensione_A3_600dpi.png"),
                g_long, width = 21*1.15, height = 11.7*1.15, units = "in", dpi = 600)
ggplot2::ggsave(file.path(output_dir, "coef_per_dimensione_A3.pdf"),
                g_long, width = 16.5, height = 11.7, units = "in")

# (B) facets per blocchi
blocchi_map <- tibble::tibble(
  term = unique(coef_df_stepwise$term),
  Blocco = dplyr::case_when(
    stringr::str_detect(term, "ROI|ROE|ROA|ROS|EBITDA|utile") ~ "Redditività / Margini",
    stringr::str_detect(term, "indice_liquidita|indice_corrente|flusso_cassa") ~ "Liquidità",
    stringr::str_detect(term, "giacenza|crediti|debiti|circolante") ~ "Capitale circolante",
    stringr::str_detect(term, "ricavi_pro_capite|valore_aggiunto|rendimento_dipendenti|dipendenti") ~ "Scala / Produttività",
    stringr::str_detect(term, "debt_equity|debiti_banche|oneri_finanziari|copertura_interessi|rapporto_indebitamento|PFN|posizione_finanziaria_netta") ~ "Struttura / Costo del debito",
    stringr::str_detect(term, "integrazione_verticale") ~ "Organizzazione",
    TRUE ~ "Altre"
  )
)

coef_df_stepwise_blk <- coef_df_stepwise %>% dplyr::left_join(blocchi_map, by = "term")

g_facet <- coef_df_stepwise_blk %>%
  dplyr::filter(term != "(Intercept)") %>%
  ggplot2::ggplot(aes(x = estimate, y = term, fill = dimensione_impresa)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::facet_wrap(~ Blocco, scales = "free_y") +
  ggplot2::geom_vline(xintercept = 0, linetype = 3) +
  ggplot2::labs(title = "Confronto coefficienti per dimensione d’impresa (Stepwise – Facets)",
                x = "Valore coefficiente (β, standardizzati)", y = NULL) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(file.path(output_dir, "coeff_per_dimensione_A3_600dpi.png"),
                g_facet, width = 21*1.15, height = 13*1.15, units = "in", dpi = 600)
ggplot2::ggsave(file.path(output_dir, "coeff_per_dimensione_A3.pdf"),
                g_facet, width = 16.5, height = 11.7, units = "in")

# ------------------ Sink riassunto modelli (come prima, ma aggiornato) ------
sink(file.path(output_dir, "risultati_modelli_dimensioni_stepwise.txt"))
cat("===== ANALISI MODELLI: CAMPIONE, COMPLETO, PER DIMENSIONE =====\n\n")

cat("----- MODELLO COMPLETO (RIDOTTO STEPWISE) -----\n\n")
print(summary(model_reduced_full))
cat("\n--- Statistiche di adattamento (glance) ---\n")
print(broom::glance(model_reduced_full))
cat("\n\n")

cat("----- MODELLO CAMPIONE -----\n\n")
print(summary(model_reduced))
cat("\n--- Statistiche di adattamento (glance) ---\n")
print(broom::glance(model_reduced))
cat("\n\n")

for (nm in names(modelli_per_dim)) {
  modello <- modelli_per_dim[[nm]]
  cat("---- MODELLO PER:", nm, "----\n\n")
  print(summary(modello))
  cat("\n--- Statistiche di adattamento (glance) ---\n")
  print(broom::glance(modello))
  cat("\nVariabili selezionate:", paste(attr(terms(modello), "term.labels"), collapse = ", "), "\n")
  cat("\n===========================================\n\n")
}
sink()

# ------------------ Salvataggi finali coerenti -------------------------------
cat("Salvataggio output FASE 4...\n")
saveRDS(dataset_completo, file.path(output_dir, "matrice.rds"))
saveRDS(vars_model,       file.path(output_dir, "vars_model_stepwise.rds"))
saveRDS(vars_per_dim,     file.path(output_dir, "vars_per_dim_stepwise.rds"))

cat("\n===== FINE FASE 4 =====\n")
