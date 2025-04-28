#!/usr/bin/env Rscript

#---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   2/5
#---------------------------------------

# Ismael Rodriguez
# 28/04/2025

# Objective: Pre-processing daily sales bills from fish market in Mallorca.
# INPUT:
# (1) Daily sales bills parsed to CSV.

# OUTPUT (saved as sample.RData):
# (1) "DATA": Cleaned dataframe of sales bills, containing standardized species
# names and boats names & ID codes.
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

# --- Folder paths
input_dir <- "../../data/ventaslonja"
output_dir <- "../../data/rdata"

# Read CSV files from input_dir
list_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

if (length(list_files) == 0) {
  stop("No CSV files found in input directory.")
}

# Read first file to fix column order
first_file <- read.csv(list_files[1], header = TRUE, sep = ",") # Check for separator
column_names <- names(first_file)

# Read and re-order all datasets
data_list <- lapply(list_files, function(file) {
  df <- read.csv(file, header = TRUE, sep = ",")
  df <- df[, match(column_names, names(df))]
  return(df)
})

# Combine all into one dataset
DATA <- bind_rows(data_list)

# --- Save DATA
if(!dir.exists(output_dir)) {dir.create(output_dir, recursive = TRUE)}
save(DATA, file = file.path(output_dir, "ventas_lonja_procesadas.RData"))

message("All CSV files succesfully processed.")
