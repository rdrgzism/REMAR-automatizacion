#!/usr/bin/env Rscript

# ---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   Script 2/6 - Cleaning and transforming sales bills
# ---------------------------------------

# Author: Ismael Rodriguez
# Date: 2025-04-28

# Objective:
# Preprocess daily sales bills and generate the required objects for:
#   - fleet classification
#   - linking with GPS tracks
#   - species weight matrix

# OUTPUTS:
# - DATA: Cleaned sales bill dataset
# - journey_list: Unique trip identifiers
# - OUT: Matrix of weights by trip and species

# --- Libraries
library(stringr)
library(dplyr)

rm(list = ls())

# --- Folder paths
setwd("C:/Users/UIB/Desktop/REMAR-automatizacion/scripts/r")
input_dir      <- "../../data/ventaslonja"
rdata_dir      <- "../../data/rdata"
processed_dir  <- "../../data/processed"
reference_dir  <- "../../data/reference"

# --- Load all CSV sales bill files
list_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(list_files) == 0) stop("No CSV files found in input directory.")

# Read first file to fix column order
first_file    <- read.csv(list_files[1], header = TRUE, sep = ",")
column_names  <- names(first_file)

# Load and reorder all datasets based on first file
data_list <- lapply(list_files, function(file) {
  df <- read.csv(file, header = TRUE, sep = ",")
  df <- df[, match(column_names, names(df))]
  return(df)
})

# Combine all into one data frame
DATA <- bind_rows(data_list)
DATA$FECHA <- as.Date(DATA$FECHA)

# ---------------------------------------
# VESSEL NAMES: Normalize and match
# ---------------------------------------

# Load official list of boats
boats <- read.csv(file.path(reference_dir, "boats.csv"), sep = ",", encoding = "latin1")

# Log unknown vessels not present in the reference file
unknown_boats <- setdiff(DATA$CODCENSO, boats$CODCENSO)
if(length(unknown_boats) > 0) {
  log_unknown_boats <- data.frame(
    FECHA = Sys.Date(),
    NEMBARCACION = unique(DATA$NEMBARCACION[DATA$CODCENSO %in% unknown_boats]),
    CODCENSO = unknown_boats
  )
  today_str <- format(Sys.Date(), "%Y-%m-%d")
  write.csv(log_unknown_boats,
            paste0("../../logs/unknown_boats_", today_str, ".csv"),
            row.names = FALSE)
}

# Standardize vessel names
DATA$NEMBARCACION <- boats$NEMBARCACIONs[match(DATA$CODCENSO, boats$CODCENSO)]

# Create unique trip identifier: "YYYY-MM-DD / VESSEL / CFR"
temp <- paste0("ESP", sprintf("%09d", DATA$CODCENSO))
DATA$JOURNEY <- paste(DATA$FECHA, "/", DATA$NEMBARCACION, "/", temp)
journey_list <- unique(DATA$JOURNEY)

# ---------------------------------------
# SPECIES NAMES: Normalize and group
# ---------------------------------------

# Load official species list
species <- read.csv(file.path(reference_dir, "sp.csv"), sep = ";", encoding = "latin1")
species_list <- sort(species$CONCEPTO)
n_species <- length(species_list)

# Remove rows with missing species codes
empty_species <- which(DATA$CONCEPTO == "")
if(length(empty_species) > 0) DATA <- DATA[-empty_species, ]

# Fix encoding issues
corrections <- c(
  "JONQ./CABOTÃ-" = "JONQ./CABOTÍ",
  "LLUÃ‡ Gros"    = "LLUÇ Gros",
  "LLUÃ‡ Mitja"   = "LLUÇ Mitja",
  "LLUÃ‡ Petit"   = "LLUÇ Petit",
  "SIPIÃ“"        = "SIPIÓ"
)
DATA$CONCEPTO <- ifelse(DATA$CONCEPTO %in% names(corrections),
                        corrections[DATA$CONCEPTO],
                        DATA$CONCEPTO)

# Map synonyms to canonical species names
DATA$temp <- 0
for (i in seq_len(n_species)) {
  temp <- species[i, which(species[i, ] != "")]
  temp2 <- which(DATA$CONCEPTO %in% temp)
  DATA$temp[temp2] <- 999
}
non_matched <- which(DATA$temp != 999)
if (length(non_matched) > 0) {
  unmatched_names <- sort(unique(DATA$CONCEPTO[non_matched]))
}

for (i in seq_len(n_species)) {
  temp <- species[i, which(species[i, ] != "")]
  temp2 <- which(DATA$CONCEPTO %in% temp)
  DATA$CONCEPTO[temp2] <- species[i, 1]
}

# Log new species not present in the species list
new_species <- setdiff(unique(DATA$CONCEPTO), species_list)
if(length(new_species) > 0) {
  today_str <- format(Sys.Date(), "%Y-%m-%d")
  write.csv(data.frame(FECHA = Sys.Date(), NUEVA_ESPECIE = new_species),
            paste0("../../logs/unknown_species_", today_str, ".csv"),
            row.names = FALSE)
}

# ---------------------------------------
# SALES / RETURNS: Remove refund pairs
# ---------------------------------------

negative <- which(DATA$IMPORTE < 0)
while(length(negative) > 0){
  to_remove <- integer(0)
  for(j in seq_along(negative)){
    neg_idx <- negative[j]
    candidates <- which(
      DATA$IMPORTE == -DATA$IMPORTE[neg_idx] &
        DATA$CONCEPTO == DATA$CONCEPTO[neg_idx] &
        DATA$JOURNEY == DATA$JOURNEY[neg_idx]
    )
    if(length(candidates) > 0){
      nearest_idx <- candidates[which.min(abs(candidates - neg_idx))]
      to_remove <- c(to_remove, neg_idx, nearest_idx)
    } else{
      to_remove <- c(to_remove, neg_idx)
    }
  }
  DATA <- DATA[-unique(to_remove), ]
  negative <- which(DATA$IMPORTE < 0)
}

# ---------------------------------------
# BUILD OUT MATRIX: [journey x species]
# ---------------------------------------

OUT <- array(0, dim = c(length(journey_list), length(species_list)))
colnames(OUT) <- species_list
rownames(OUT) <- journey_list

for(i in seq_along(journey_list)){
  temp <- which(DATA$JOURNEY == journey_list[i])
  for(j in seq_along(temp)) {
    temp2 <- which(species_list == DATA$CONCEPTO[temp[j]])
    if(!is.na(temp2)){
      OUT[i, temp2] <- OUT[i, temp2] + DATA$PESONETO[temp[j]]
    }
  }
}

# ---------------------------------------
# EXPORT CLEANED DATA
# ---------------------------------------

write.csv(DATA, file.path(processed_dir,"DATA_final.csv"), row.names = FALSE)
save(OUT, journey_list, file = file.path(rdata_dir,"sample.RData"))
