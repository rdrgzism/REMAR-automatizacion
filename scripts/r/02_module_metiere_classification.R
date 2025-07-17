#!/usr/bin/env Rscript

# ---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   Script 2/6 - Métier classification from species composition
# ---------------------------------------

# Author: Miquel Palmer Vidal
# Reviewed and documented: Ismael Rodríguez
# Date: 2025-04-30

# OBJECTIVE:
# Predict the most probable fishing metier for each voyage, using species composition.
# The classification is based on a sequential model with:
#   - 5 binary classifiers (k-means + PCA or CA)
#   - 1 multi-label classifier to distinguish "tresmall" from "palangre"

# INPUT:
# - OUT: matrix of landed weights [voyage x species]
# - journey_list: vector of unique fishing trip identifiers
# - classifiers: list of predefined classifier parameters

# OUTPUT:
# - predicted.RData:
#   > OUT_<metier>, journey_<metier>

# --- Libraries
library(dotenv)
library(mldr)
library(RWeka)

rm(list = ls())

load_dot_env(file = file.path("~/REMAR-automatizacion/config/.env"))

# --- Folder paths
# rdata_dir     <- "../../data/rdata"
# processed_dir <- "../../data/processed"

setwd(Sys.getenv("WORKING_DIR"))
rdata_dir      <- Sys.getenv("RDATA_DIR")

# --- Utility function to extract output per metier
extract_metier <- function(name, results, OUT, journey_list) {
  idx <- which(results$class == name)
  list(
    OUT = OUT[idx, , drop = FALSE],
    journey = journey_list[idx]
  )
}

if (file.exists(file.path(rdata_dir, "sample.RData"))) {
  load(file.path(rdata_dir, "sample.RData"))         # OUT and journey_list
} else{
  stop("El archivo 'sample.RData' no se encuentra en el directorio especificado.")
}

# --- Load cleaned data and classifier models
load(file.path(rdata_dir, "classificadors.RData"))      # classifiers list

if (!is.data.frame(OUT)) OUT <- as.data.frame(OUT)  # Ensure data.frame format
results <- list()
results$class <- rep(NA, length(journey_list))

# ---------------------------------------
# CLASSIFICATION LOOP with Progress Bar
# ---------------------------------------

pb <- txtProgressBar(min = 0, max = length(journey_list), style = 3)

# Apply classifiers sequentially to each fishing trip
for (i in seq_along(journey_list)) {
  sample <- OUT[i, ]
  classified <- FALSE
  
  for (class_name in names(classifiers)) {
    clf <- classifiers[[class_name]]
    
    # Select only species used by the current classifier
    selected_species <- which(names(sample) %in% clf$sp.names)
    new <- sample[selected_species]
    
    # Apply classification function
    label <- classification.function(
    sp.names   = clf$sp.names,
    new        = new,
    pca        = clf$pca,
    n.axes     = clf$n.axes,
    sco.names  = clf$sco.names,
    classifier = clf$classifier
    )
    
    # Store classification and break if a label is assigned
    if (label == "1") {
      results$class[i] <- class_name
      classified <- TRUE
      break
    } else if (label == "10") {
      results$class[i] <- "tresmall"
      classified <- TRUE
      break
    } else if (label == "01") {
      results$class[i] <- "palangre"
      classified <- TRUE
      break
    }
  }
  
  setTxtProgressBar(pb, i)
}

close(pb)

# ---------------------------------------
# SPLIT RESULTS BY METIER
# ---------------------------------------

metiers <- c("jonquillera", "tresmall", "palangre", "nasa", "cercol", "llampuguera")
for (metier in metiers) {
  res <- extract_metier(metier, results, OUT, journey_list)
  assign(paste0("OUT_", metier), res$OUT)
  assign(paste0("journey_", metier), res$journey)
}

# ---------------------------------------
# SAVE CLASSIFIED OUTPUTS
# ---------------------------------------

save_list <- list(results = results)
for (metier in metiers) {
  save_list[[paste0("OUT_", metier)]] <- get(paste0("OUT_", metier))
  save_list[[paste0("journey_", metier)]] <- get(paste0("journey_", metier))
}
save(list = names(save_list), file = file.path(rdata_dir, "predicted.RData"))
