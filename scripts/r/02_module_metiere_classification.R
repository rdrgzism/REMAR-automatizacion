#!/usr/bin/env Rscript

# ---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   Script 3/6 - Métier classification from species composition
# ---------------------------------------

# Author: Miquel Palmer Vidal
# Reviewed and documented: Ismael Rodríguez
# Date: 2025-04-30

# OBJECTIVE:
# Predict the most probable fishing métier for each voyage, using species composition.
# The classification is based on a sequential model with:
#   - 5 binary classifiers (k-means + PCA or CA)
#   - 1 multilabel classifier for distinguishing "tresmall" vs "palangre"

# INPUT:
# - OUT: matrix of species weights [voyage x species]
# - journey_list: vector of unique voyage identifiers
# - classifiers: list of pretrained classifier parameters (from classificadors.RData)

# OUTPUT:
# - predicted.RData:
#   > OUT_jonquillo, journey_jonquillo
#   > OUT_tresmall, journey_tresmall

# --- Libraries
library(mldr)
library(RWeka)

rm(list = ls())

# --- Folder paths
setwd("C:/Users/UIB/Desktop/REMAR-automatizacion/scripts/r")
input_dir     <- "../../data/ventaslonja"
rdata_dir     <- "../../data/rdata"
processed_dir <- "../../data/processed"
reference_dir <- "../../data/reference"

# --- Load cleaned sales and classifiers
load(file.path(rdata_dir, "sample.RData"))         # OUT and journey_list
load(file.path(rdata_dir, "classificadors.RData")) # classifiers list

OUT <- data.frame(OUT)  # Ensure data frame format
results <- list()
results$class <- rep(NA, length(journey_list))

# ---------------------------------------
# CLASSIFICATION LOOP
# ---------------------------------------

# For each voyage, apply classifiers sequentially
for (i in seq_along(journey_list)) {
  sample <- OUT[i, ]
  classified <- FALSE

  for (class_name in names(classifiers)) {
    clf <- classifiers[[class_name]]

    # Select only columns corresponding to classifier species
    temp_names <- make.names(colnames(sample), unique = TRUE)
    selected_species <- which(temp_names %in% clf$sp.names)
    new <- sample[selected_species]

    # Apply classification
    label <- classification.function(
      sp.names   = clf$sp.names,
      new        = new,
      pca        = clf$pca,
      n.axes     = clf$n.axes,
      sco.names  = clf$sco.names,
      classifier = clf$classifier
    )

    # Check label and store classification
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
}

# ---------------------------------------
# SPLIT RESULTS BY METIER
# ---------------------------------------

# Jonquillera
idx_jonquillo <- which(results$class == "jonquillera")
OUT_jonquillo <- OUT[idx_jonquillo, , drop = FALSE]
journey_jonquillo <- journey_list[idx_jonquillo]

# Tresmall
idx_tresmall <- which(results$class == "tresmall")
OUT_tresmall <- OUT[idx_tresmall, , drop = FALSE]
journey_tresmall <- journey_list[idx_tresmall]

# ---------------------------------------
# SAVE CLASSIFIED OUTPUTS
# ---------------------------------------

save(
  OUT_jonquillo, journey_jonquillo,
  OUT_tresmall, journey_tresmall,
  file = file.path(rdata_dir, "predicted.RData")
)
