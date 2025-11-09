###############################################################################
############ TESI MAGISTRALE - UN'ANALISI SPAZIALE DELLE ######################
############ PERFORMANCE ECONOMICHE E FINANZIARIE DELLE #######################
########################## IMPRESE ITALIANE ###################################
###############################################*X*PIETRO MAIETTA*X*############
###############################################################################
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


# Script principale per l'analisi della tesi.
# Esegue tutte le fasi in sequenza.

# --- FASE 1: INGESTION & CLEANING ---
# Output: dataset_completo.rds
source("unione, pulizia e formattazione.R") 

# --- FASE 2: FEATURE ENGINEERING & ISP CALCULATION ---
# Input: dataset_completo.rds. Output: matrice.rds
source("divisione e creazione ISP.R")        

# --- FASE 3: VALIDAZIONE ISP & ANALISI DESCRITTIVA ---
# Include PCA, descrittive e grafici (leaflet, ecc.). Input: matrice.rds
source("ISP validazione e inferenza.R")

# --- FASE 4: MODELLI ECONOMETRICI (NON-SPAZIALI) ---
# Include Stepwise Regression, trasformazioni logaritmiche. Input: matrice.rds
source("modelli econometrici.R")

# --- FASE 5: ANALISI SPAZIALE BASE (SAR/SDM, LISA base, Matrice W) ---
# Questo è il primo contatto con l'analisi spaziale. Output: matrici W e regressioni intermedie.
source("analisi spaziale.R") 

# --- FASE 6: TEST E CONVALIDA (Diagnostica sui risultati Spaziali) ---
# Test sui residui, confronti tra varianti di matrici W, verifica robustezza. Input: output di FASE 5.
source("test e convalida.R") 

# --- FASE 7: PIPELINE COMPLETA (GWR Condizionale) ---
# Esegue GWR solo sui subset più adatti, basandosi sui test di convalida della FASE 6.
source("pipeline completa.R")
