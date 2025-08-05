#!/usr/bin/env Rscript

# ---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   Script 1/6 - Cleaning and transforming sales bills
# ---------------------------------------

# Author: Ismael Rodriguez
# Date: 2025-04-28
# Update: 2024-07-21

# Objective:
# Preprocess daily sales bills and generate the required objects for:
#   - fleet classification
#   - linking with GPS tracks
#   - inference of fishing effort for the target metier

# OUTPUTS:
# - DATA: Cleaned sales bill data set
# - journey_list: Unique trip identifiers
# - OUT: matrix of landed weights per fishing trip and species

# --- Libraries
library(dotenv)
library(stringr)
library(dplyr)

rm(list = ls())

# --- Folder paths
# input_dir      <- "../../../data/ventaslonja"
# results_dir      <- "../../../data/rdata/results"
# processed_dir  <- "../../../data/processed"
# logs_dir <- "../../../logs"
# reference_dir  <- "../../../data/reference/mallorca"

load_dot_env(file = file.path("~/REMAR-automatizacion/config/.env"))
setwd(Sys.getenv("WORKING_DIR"))
input_dir      <- Sys.getenv("INPUT_DIR")
results_dir      <- Sys.getenv("results_dir")
processed_dir  <- Sys.getenv("PROCESSED_DIR")
reference_dir  <- Sys.getenv("REFERENCE_DIR")
logs_dir       <- Sys.getenv("LOGS_DIR")

# --- Execution mode ---
mode <- "range" # Options: "cron" or "range"
today_str <- format(Sys.Date(), "%Y-%m-%d")

# --- Load all CSV sales bill files
list_dirs <- list(input_dir = input_dir,
results_dir = results_dir,
processed_dir = processed_dir,
reference_dir = reference_dir,
logs_dir = logs_dir)

for (name in names(list_dirs)) {
  dir_path <- list_dirs[[name]]
  if (!dir.exists(dir_path)) {
    if (name == "input_dir") {
      stop(paste("ERROR: Required input directory does not exist:", name ))
    } else if (name == "reference_dir") {
      stop(paste("ERROR: Required reference directory does not exist:", name ))
    } else {
      message(paste("Creating missing directory:", dir_path)) 
      dir.create(dir_path, recursive = TRUE)
    }
  }
}

all_files <- list.files(input_dir, full.names = TRUE)
existing_files <- all_files[grepl("^IMEDEA_\\d{8}_\\d{8}\\.csv$",
                                  basename(all_files), ignore.case = TRUE)]
if (length(existing_files) == 0) stop("No CSV files found in input directory.")

if (mode == "cron") {
  target_date <- Sys.Date() - 10
  days <- seq(target_date, Sys.Date(), by = 1)
  days_str <- format(days, "%Y%m%d")
  file_names <- paste0("IMEDEA_", days_str, "_", days_str, ".csv")
  
  selected_files <- existing_files[tolower(
    basename(existing_files)) %in% tolower(file_names)]
  
  if (length(selected_files) < 5) {
    warning("At least 5 sales files are required.")
    selected_files <- existing_files
  } else {
    sorted_idx <- order(basename(selected_files), decreasing = FALSE)
    selected_files <- selected_files[sorted_idx][1:5]
  }
} else if (mode == "range") {
  target_dates <- seq(as.Date("2025-07-01"), as.Date("2025-07-31"), 
                         by = "day")
  days_str <- format(target_dates, "%Y%m%d")
  file_names <- paste0("IMEDEA_", days_str, "_", days_str, ".csv")
  selected_files <- existing_files[tolower(basename(existing_files)) %in%
                                     tolower(file_names)]
  
  if (length(selected_files) == 0) {
    stop("No matching files found in the selected range")
  }
}

expected_cols <- c("FECHA", "NEMBARCACION", "NIF", "CONCEPTO", "NUMENVAS",
                   "PESONETO", "PRECIOUNID", "IMPORTE", "CODCLIENTE",
                   "CODCENSO", "NUMVENTA")

# Load and reorder all datasets based on expected_columns
data_list <- lapply(selected_files, function(file) {
  df <- read.csv(file, header = TRUE, sep = ",", encoding = "UTF-8")
  missing_cols <- setdiff(expected_cols, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  df <- df[ ,expected_cols]
  return(df)
})

# Combine all into one data frame
DATA <- do.call(rbind, data_list)
# View(DATA[!complete.cases(DATA), ])
DATA <- DATA[complete.cases(DATA), ]
DATA$FECHA <- as.Date(DATA$FECHA, format = "%Y-%m-%d %H:%M:%S")
DATA$CODCENSO <- as.integer(DATA$CODCENSO)
target_dates <- sort(unique(DATA$FECHA))
# which(is.na(DATA$CODCENSO)) #integer(0)
# which(is.na(DATA$CONCEPTO) | DATA$CONCEPTO == "") # integer(0)

# ---------------------------------------
# VESSEL NAMES: Normalize and match
# ---------------------------------------

DATA$NEMBARCACION <- toupper(DATA$NEMBARCACION)
DATA$NEMBARCACION <- trimws(DATA$NEMBARCACION)
DATA$NEMBARCACION <- gsub("_+$", "", DATA$NEMBARCACION)
DATA$NEMBARCACION <- gsub("^_+", "", DATA$NEMBARCACION)
DATA$NEMBARCACION <- gsub("[ ]{2,}", " ", DATA$NEMBARCACION)
# unique(DATA$NEMBARCACION) # 160
# unique(DATA$CODCENSO) # 156

# Load official list of boats
boats <- read.csv(file.path(reference_dir, "boats.csv"), sep = ",", encoding = "latin1")

missing_censo <- is.na(DATA$CODCENSO)
# DATA$CODCENSO[missing_censo] # integer(0)
if (length(DATA$CODCENSO[missing_censo]) > 0) {
  DATA$CODCENSO[missing_censo] <- boats$CODCENSO[match(
    DATA$NEMBARCACION[missing_censo], boats$NEMBARCACIONs)]
} else {cat('No embarcaciones sin identificar')}

# Log unknown vessels not present in the reference file
unknown_boats <- setdiff(DATA$CODCENSO, boats$CODCENSO)
# unique(DATA$NEMBARCACION[DATA$CODCENSO %in% unknown_boats])
# [1] "LA TAU"          "NA MANDRIA"      "NUEVO MARTINA"   "DES PAS SEGUNDO" "ES FERRE II"     "PEDRO Y BEATRIZ"
if (length(unknown_boats) > 0) {
  # Log new boats not present in the boats list
  log_file <- paste0(logs_dir, "/unknown_boats_", today_str, ".csv")
  log_unknown_boats <- data.frame(
    CODCENSO = unknown_boats,
    NEMBARCACIONs = unique(DATA$NEMBARCACION[DATA$CODCENSO %in% unknown_boats]),
    CFR = paste0("ESP", sprintf("%09d", unknown_boats))
  )
  write.csv(log_unknown_boats, log_file, row.names = FALSE)

  # Warning message for newly found vessels
  warning_log <- paste0(logs_dir, "/warning_", today_str, ".log")
  msg <- paste0("ALERTA: Se han encontrado embarcaciones no registradas.\n",
                "Se recomienda revisar la lista de embarcaciones activas en las subastas")
  writeLines(msg, con = warning_log)    
  
  # Opcional: agregar temporalmente embarcaciones al dataset para evitar NAs
  boats <- rbind(boats, log_unknown_boats)
}
# log_unknown_boats
# > log_unknown_boats
# FECHA    NEMBARCACION CODCENSO
# 1 2025-07-14          LA TAU    25416
# 2 2025-07-14      NA MANDRIA    27158
# 3 2025-07-14   NUEVO MARTINA    24998
# 4 2025-07-14 DES PAS SEGUNDO   100684
# 5 2025-07-14     ES FERRE II    15120
# 6 2025-07-14 PEDRO Y BEATRIZ    23358

# Standardize vessel names
DATA$NEMBARCACION <- boats$NEMBARCACIONs[match(DATA$CODCENSO, boats$CODCENSO)]
# unique(DATA$NEMBARCACION) # 156
# unique(DATA$CODCENSO) # 156
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
n_species <- length(species_list) # 133

# Remove rows with missing species codes
empty_species <- which(DATA$CONCEPTO == "" | is.na(DATA$CONCEPTO))
if (length(empty_species) > 0) DATA <- DATA[-empty_species, ]

# Clean escape characters "\n" y "\t", and trim species names
DATA$CONCEPTO <- gsub("\\\\n|\\n|\\t", " ", DATA$CONCEPTO)   # replace line breaks for blank space
DATA$CONCEPTO <- trimws(DATA$CONCEPTO)                       # remove blank space

DATA$ESPECIE <- NA_character_

# Map synonyms to canonical species names
for (i in seq_len(n_species)) {
  synonyms <- species[i, which(species[i, ] != "")]
  matched <- which(DATA$CONCEPTO %in% synonyms)
  DATA$ESPECIE[matched] <- species[i, 1]
}

non_matched <- which(is.na(DATA$ESPECIE))
if (length(non_matched) > 0) {
  unmatched_names <- sort(unique(DATA$CONCEPTO[non_matched]))

  # Log new species not present in the species list
  log_file <- paste0(logs_dir, "/unknown_species_", today_str, ".csv")
  log_data <- data.frame(NUEVA_ESPECIE = unmatched_names)
  write.csv(log_data, log_file, row.names = FALSE)
  
  # Registrar mensaje de error crítico en un archivo
  error_log <- paste0(logs_dir, "/error_", today_str, ".log")
  msg <- paste0("ERROR CRÍTICO: Se han encontrado especies no registradas.\n",
                "Revisa y actualiza el archivo 'sp.csv'.\n")
  writeLines(msg, con = error_log)
  
  stop("Se han encontrado especies no registradas. Verifica el log de errores.")
}

# ---------------------------------------
# SALES / RETURNS: Remove refund pairs
# ---------------------------------------
process_daily_sales <- function(DATA, species_list, fechas) {
  OUT_list <- list()
  for (fecha_base in fechas) {
    negative <- which(DATA$IMPORTE < 0 & DATA$FECHA == fecha_base)
    to_remove <- integer(0)
    
    while(length(negative) > 0){
      for(j in seq_along(negative)){
        neg_idx <- negative[j]
        neg_fecha <- DATA$FECHA[neg_idx]
        candidates <- which(
          DATA$IMPORTE == -DATA$IMPORTE[neg_idx] &
            DATA$CONCEPTO == DATA$CONCEPTO[neg_idx] &
            DATA$NEMBARCACION == DATA$NEMBARCACION[neg_idx] &
            DATA$FECHA >= neg_fecha & DATA$FECHA <= (neg_fecha + 5)
        )
        if (length(candidates) > 0){
          nearest_idx <- candidates[which.min(abs(candidates - neg_idx))]
          to_remove <- c(to_remove, neg_idx, nearest_idx)
        } else{
          to_remove <- c(to_remove, neg_idx)
        }
      }
      DATA <- DATA[-unique(to_remove), ]
      negative <- which(DATA$IMPORTE < 0 & DATA$FECHA == fecha_base)
    }
    
    # ---------------------------------------
    # BUILD OUT MATRIX: [journey x species]
    # ---------------------------------------
    DATA_sub <- DATA[DATA$FECHA == fecha_base, ]
    journey_list <- unique(DATA_sub$JOURNEY)
    
    OUT <- array(0, dim = c(length(journey_list), length(species_list)))
    colnames(OUT) <- make.names(species_list, unique = TRUE)
    rownames(OUT) <- journey_list
    
    for(i in seq_along(journey_list)){
      temp <- which(DATA_sub$JOURNEY == journey_list[i])
      for(j in seq_along(temp)) {
        temp2 <- which(species_list == DATA_sub$ESPECIE[temp[j]])
        if(length(temp2) > 0){
          OUT[i, temp2] <- OUT[i, temp2] + DATA_sub$PESONETO[temp[j]]
        }
      }
    }
    
    OUT_list[[format(as.Date(fecha_base), "%Y-%m-%d")]] <- OUT
  }
  
  return(OUT_list)
}

OUT_daily <- process_daily_sales(DATA, species_list, target_dates)

if (mode == "cron") {
  # Keep only the OUT matrix corresponding to the earliest date
  OUT <- OUT_daily[[target_date]]
  
  # Convert OUT matrix to dataframe
  journey_parts <- do.call(rbind, strsplit(rownames(OUT), " / "))
  OUT_df <- data.frame(
    FECHA = as.Date(journey_parts[ ,1]),
    NEMBARCACION = journey_parts[ ,2],
    CFR = journey_parts[ ,3],
    OUT,
    row.names = NULL,
    check.names = FALSE
  )
  
} else if (mode == "range") {
  # Combine all OUT matrices into one data frame
  OUT_df <- do.call(rbind, lapply(names(OUT_daily), function(fecha) {
    mat <- OUT_daily[[fecha]]
    if (nrow(mat) == 0) return(NULL)
    parts <- do.call(rbind, strsplit(rownames(mat), " / "))
    data.frame(
      FECHA = as.Date(parts[, 1]),
      NEMBARCACION = parts[, 2],
      CFR = parts[, 3],
      mat,
      row.names = NULL,
      check.names = FALSE
    )
  }))
  
  # Optional: sort the final OUT_df by date
  OUT_df <- OUT_df[order(OUT_df$FECHA), ]
  
  # Optional: save entire list if needed
  OUT <- do.call(rbind, OUT_daily)
}


# ---------------------------------------
# EXPORT CLEANED DATA
# ---------------------------------------

write.csv(DATA, file.path(processed_dir,"DATA_daily.csv"), row.names = FALSE)
write.csv(OUT_df, file.path(processed_dir, "OUT_metadata.csv"), 
          row.names = FALSE)
if (mode == "cron") {writeLines(format(target_date, "%Y-%m-%d"),
                                "fecha_target.txt")}
save(OUT, journey_list, file = file.path(results_dir,"sample.RData"))
