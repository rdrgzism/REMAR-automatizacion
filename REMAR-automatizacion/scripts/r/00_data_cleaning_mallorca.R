#!/usr/bin/env Rscript

#---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   2/5
#---------------------------------------

# Ismael Rodriguez
# 28/04/2025

# Objective: Pre-processing daily sales bills from fish market in Majorca.
# INPUT:
# (1) Daily sales bills parsed to CSV.

# OUTPUT (saved as sample.RData):
# (1) "DATA": Cleaned data frame of sales bills, containing standardized species
# names, boats names & journey IDs.
# (2) "journey_list": Voyage's ID for the corresponding sale bill. Every entry
# contains the date of the voyage, the name of the vessel and it's Community
# Fleet Register (CFR).
# (1) "OUT": Matrix of species weights.
# A matrix with dimensions = length(journey_list) x n_species.
# This matrix contains the weights of all species for every voyage.
# If a species was not captured during a specific voyage, its entry is set to 0.

# --- Libraries
library(stringr)
library(dplyr)

rm(list = ls())

# --- Folder paths
input_dir <- "../../data/ventaslonja"
output_dir <- "../../data/rdata"
reference_dir <- "../../data/reference"

# Read CSV files from input_dir
list_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

if (length(list_files) == 0) {
  stop("No CSV files found in input directory.")
}

# Read first file to fix column order
first_file <- read.csv(list_files[1], header = TRUE, sep = ",") # Check for separator
column_names <- names(first_file)

# Read and re-order all data sets
data_list <- lapply(list_files, function(file) {
  df <- read.csv(file, header = TRUE, sep = ",")
  df <- df[, match(column_names, names(df))]
  return(df)
})

# Combine all into one data set
DATA <- bind_rows(data_list)

# Date format
DATA$FECHA <- as.Date(DATA$FECHA)

# Cleaning boat names
boats <- read.csv(file.path(reference_dir, "boats.csv"), sep = ",", encoding = "latin1")
# step 1: Find CODCENSO values in DATA that are NOT in boats
unknown_boats <- setdiff(DATA$CODCENSO, boats$CODCENSO)
if(length(unknown_boats) > 0) {
  log_unknown_boats <- data.frame(
      FECHA = Sys.Date(),
      NEMBARCACION = unique(DATA$NEMBARCACION[which(DATA$CODCENSO %in% unknown_boats)]),
      CODCENSO = unknown_boats
  )

  # Build the filename dinamically with date
  today_str <- format(Sys.Date(), "%Y-%m-%d")
  log_filename <- paste0("../../logs/unknown_boats_", today_str, ".csv")

  # Write the log
  write.csv(log_unknown_boats,
            log_filename,
            row.names = FALSE
  )
}

# step 2: Standardize boat names
DATA$NEMBARCACION <- boats$NEMBARCACIONs[match(DATA$CODCENSO, boats$CODCENSO)]

# Create the journey_list vector
temp <- paste0("ESP",sprintf("%09d",DATA$CODCENSO))
DATA$JOURNEY <- paste(DATA$FECHA, "/", DATA$NEMBARCACION, "/", temp)
journey_list <- unique(DATA$JOURNEY)

# Cleaning species names
# 1. Read species list
species <- read.csv(file.path(reference_dir, "sp.csv"), sep = ";", encoding = "latin1")
species_list <- sort(species$CONCEPTO)
n_species <- length(species_list)

# 2. Check for missing values
temp <- which(DATA$CONCEPTO == "")
if(length(temp) > 0) {DATA <- DATA[-temp, ]}

# 3. Check for errors in codification
corrections <- c(
  "JONQ./CABOTÃ­" = "JONQ./CABOTÍ",
  "LLUÃ‡ Gros" = "LLUÇ Gros",
  "LLUÃ‡ Mitja" = "LLUÇ Mitja",
  "LLUÃ‡ Petit" = "LLUÇ Petit",
  "SIPIÃ“" = "SIPIÓ"
)

DATA$temp <- 0
for (i in seq_along(species_list)) {
  temp <- species[i, which(species[i, ] != "")]
  temp2 <- which(DATA$CONCEPTO %in% temp)
  DATA$temp[temp2] <- 999
}
temp <- which(DATA$temp != 999)
temp2 <- sort(unique(DATA$CONCEPTO[temp]))

DATA$CONCEPTO <- ifelse(DATA$CONCEPTO %in% names(corrections),
                        corrections[DATA$CONCEPTO],
                        DATA$CONCEPTO)

# Standardize species names
for(i in seq_along(species_list)) {
  temp <- species[i, which(species[i, ] != "")]
  temp2 <- which(DATA$CONCEPTO %in% temp)
  DATA$CONCEPTO[temp2] <- species[i, 1]
}
unique(DATA$CONCEPTO)
# Once the species names are standardized, check for species not in species_list
new_species <- setdiff(unique(DATA$CONCEPTO), species_list)

if(length(new_species) > 0) {
  today_str <- format(Sys.Date(), "%Y-%m-%d")
  log_filename <- paste0("../../logs/unknown_species_", today_str, ".csv")

  write.csv(data.frame(
    FECHA = Sys.Date(),
    NUEVA_ESPECIE = new_species),
            log_filename,
            row.names = FALSE)
}

# Removing sell-return pairs
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

# Creating the weights matrix
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

save(DATA, OUT, journey_list, species_list, file = file.path(output_dir,"sample.RData"))
