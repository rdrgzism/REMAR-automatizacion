#!/usr/bin/env Rscript

# ---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   Script 3/6 - Link classified fishing journeys to GPS tracks
# ---------------------------------------

# Author: Ismael Rodríguez
# Updated: 2025-05-05

# OBJECTIVE:
# For each classified fishing journey (jonquillo and tresmall), retrieve
# the corresponding GPS track from the "cajas verdes" files and clean it.

# INPUT:
# - predicted.RData: Contains journey_jonquillo and journey_tresmall
# - GPS daily tracks (csv) in /data/cajasverdes
# - Shapefile with port buffers to exclude port positions

# OUTPUT:
# - raw_jonquillo_tracks.RData
# - raw_tresmall_tracks.RData

# --- Load libraries
library(dotenv)
library(data.table)
library(sf)
library(dplyr)
library(lubridate)

rm(list = ls())

# --- Folder paths
setwd("C:/Users/Usuario/Desktop/REMAR/REMAR-automatizacion/scripts/r")
rdata_dir   <- "../../data/rdata"
tracks_dir <- "C:/Users/Usuario/OneDrive - Universitat de les Illes Balears/Archivos de Bernat Morro Cortès - REMAR (REcursos MARins pesquers a Balears)/P. artesanal/05_CaixesVerdes"
shp_dir     <- "../../data/shp"
reference_dir <- "../../data/reference"
processed_dir <- "../../data/processed"
logs_dir <- "../../logs"
# 
# load_dot_env(file = file.path("~/REMAR-automatizacion/config/.env"))
# setwd(Sys.getenv("WORKING_DIR"))
# rdata_dir      <- Sys.getenv("RDATA_DIR")
# tracks_dir <- Sys.getenv("TRACKS_DIR")
# shp_dir <- Sys.getenv("SHP_DIR")
# processed_dir  <- Sys.getenv("PROCESSED_DIR")
# reference_dir  <- Sys.getenv("REFERENCE_DIR")
# logs_dir       <- Sys.getenv("LOGS_DIR")

# --- Load data
if (file.exists(file.path(rdata_dir, "predicted.RData"))) {
  load(file.path(rdata_dir, "predicted.RData"))         # OUT and journey_list
} else{
  stop("El archivo 'predicted.RData' no se encuentra en el directorio especificado.")
}

# ---------------------------------------
# FUNCTION TO CREATE THE GPS DATA SET &
# Extract different metrics.
# ---------------------------------------

# n_days = Sys.Date() - as.Date("2024-01-01"), para el análisis diario set a 1
process_gps_data <- function(n_days = 564, tracks_dir, reference_dir, shp_dir,
                             logs_dir) {
  
  if (!dir.exists(shp_dir)) {
    stop(paste("El directorio shp_dir no existe:", shp_dir))
  }
  
  ports_path <- file.path(shp_dir, "BufferPuertos.shp")
  inland_path <- file.path(shp_dir, "TierraMenos50m_4326.shp")
  
  if (!file.exists(ports_path)){
    stop("No se encuentra el archivo de puertos: ", ports_path)
  }
  
  if (!file.exists(inland_path)){
    stop("No se encuentra el archivo de puertos: ", inland_path)
  }
  
  ports  <- st_read(ports_path, quiet = TRUE)
  inland <- st_read(inland_path, quiet = TRUE)
  
  if (!dir.exists(tracks_dir)) {
    stop(paste("El directorio tracks_dir no existe:", tracks_dir))
  }
  
  today_str <- format(Sys.Date(), "%Y-%m-%d")
  start_year <- year(Sys.Date() - n_days)
  current_year <- year(Sys.Date())
  valid_years <- as.character(seq(start_year, current_year))
  
  all_subdirs <- list.dirs(tracks_dir, recursive = FALSE, full.names = TRUE)
  subdirs <- all_subdirs[basename(all_subdirs) %in% valid_years]
  
  track_files <- unlist(lapply(subdirs, function(p) {
    list.files(p, pattern = "\\.csv$", full.names = TRUE)
  }))
  
  recent_files <- track_files[file.info(track_files)$mtime >= 
                                Sys.Date() - n_days]
  
  if (length(recent_files) == 0) {
    stop("No se encontraron archivos recientes en los últimos ", n_days, " días.")
  }
  
  GPS_DATA <- rbindlist(lapply(track_files, function(file) {
    fread(file,
          select = c("Id", "Latitude", "Longitude", "Speed",
                     "GPSDateTime", "VesselName", "Cfr"),
          check.names = TRUE)
  }), use.names = TRUE, fill = TRUE)
  
  # --- Basic filtering
  # Remove rows with empty Cfr
  GPS_DATA <- GPS_DATA[!(Cfr == ""), ]
  # Convert to datetime
  GPS_DATA[ , GPSDateTime := as.POSIXct(GPSDateTime,
                                        format = "%Y-%m-%dT%H:%M:%SZ",
                                        tz = "UTC")]
  # Safety check
  if (!"Cfr" %in% names(GPS_DATA)) stop("Missing 'Cfr' column in GPS data.")
  # Remove rows with NAs
  GPS_DATA <- GPS_DATA[complete.cases(GPS_DATA), ]
  
  # ---------------------------------------
  # VESSEL NAMES: Normalize and match
  # ---------------------------------------
  # Load official list of boats
  boats <- read.csv(file.path(reference_dir, "boats.csv"), 
                    sep = ",", encoding = "latin1")
  # Set boat names to normalized ones.
  GPS_DATA$VesselName <- boats$NEMBARCACIONs[match(GPS_DATA$Cfr, boats$CFR)]
  temp_na <- which(is.na(GPS_DATA$VesselName))
  GPS_DATA <- GPS_DATA[-temp_na, ]
  
  GPS_DATA$journey <- paste(as.Date(GPS_DATA$GPSDateTime), "/", 
                            GPS_DATA$VesselName, "/", GPS_DATA$Cfr)
  journey_list <- unique(GPS_DATA$journey)
  
  # --- Convert to sf
  sf_data <- st_as_sf(GPS_DATA,
                      coords = c("Longitude", "Latitude"),
                      crs = 4326)
  
  # --- Remove port points
  port_mask <- lengths(st_intersects(sf_data, ports)) > 0
  sf_data$inside_port <- port_mask
  
  journeys_entries_in_port <- sf_data %>%
    st_drop_geometry() %>%
    group_by(journey) %>%
    summarise(all_in_port = all(inside_port)) %>%
    filter(all_in_port) %>%
    pull(journey)
  
  # sf_data_in_port <- sf_data %>%
  #   filter(journey %in% journeys_entries_in_port)
  # if (nrow(sf_data_in_port) > 0) {
  #   
  #   df_data_in_port <- sf_data_to_df(sf_data_in_port)
  #   write.csv(df_data_in_port,
  #             file.path(processed_dir,
  #                       paste0("df_tracks_in_port_", today_str, ".csv")),
  #             row.names = FALSE,
  #             fileEncoding = "ISO-8859-1")
  # }
  
  if (length(journeys_entries_in_port) > 0) {
    df_log <- data.frame(TRACK_ID = journeys_entries_in_port)
    write.csv(df_log,
              file.path(logs_dir,
              paste0("log_tracks_in_port_", today_str, ".csv")),
              row.names = FALSE,
              fileEncoding = "ISO-8859-1")
  }
  
  # --- Remove inland points
  inland_mask <- lengths(st_intersects(sf_data, inland)) > 0
  sf_data$inland <- inland_mask
  
  journeys_entries_inland <- sf_data %>%
    st_drop_geometry() %>%
    group_by(journey) %>%
    summarise(all_inland = all(inland)) %>%
    filter(all_inland) %>%
    pull(journey)
  
  # sf_data_inland <- sf_data %>%
  #   filter(journey %in% journeys_entries_inland)
  # 
  # if (nrow(sf_data_inland) > 0) {
  #   df_data_inland <- sf_data_to_df(sf_data_inland)
  #   write.csv(df_data_inland,
  #             file.path(processed_dir, 
  #                       paste0("df_tracks_inland_", today_str, ".csv")),
  #             row.names = FALSE,
  #             fileEncoding = "ISO-8859-1")
  # }
  
  if (length(journeys_entries_inland) > 0) {
    df_log <- data.frame(TRACK_ID = journeys_entries_inland)
    write.csv(df_log,
              file.path(logs_dir,
                        paste0("log_tracks_inland_", today_str, ".csv")),
              row.names = FALSE,
              fileEncoding = "ISO-8859-1")
  }
  
  sf_data_clean <- sf_data %>%
    filter(!(journey %in% c(journeys_entries_in_port, journeys_entries_inland)))
  
  # --- Remove tracks with less than 4 entries
  journeys_few_bips <- sf_data %>%
    st_drop_geometry() %>%
    count(journey) %>%
    filter(n < 6) %>%
    pull(journey)
  
  # sf_data_few_bips <- sf_data %>%
  #   filter(journey %in% journeys_few_bips)
  # 
  # if (nrow(sf_data_few_bips) > 0) {
  #   df_data_inland <- sf_data_to_df(sf_data_inland)
  #   write.csv(sf_data_inland,
  #             file.path(processed_dir,
  #                       paste0("df_few_bips_", today_str, ".csv")),
  #             row.names = FALSE,
  #             fileEncoding = "ISO-8859-1")
  
  if (length(journeys_few_bips) > 0) {
    df_log <- data.frame(TRACK_ID = journeys_few_bips)
    write.csv(df_log,
              file.path(logs_dir,
                        paste0("log_few_bips_", today_str, ".csv")),
              row.names = FALSE,
              fileEncoding = "ISO-8859-1")
  }
  
  sf_data_clean <- sf_data_clean %>%
    filter(!(journey %in% journeys_few_bips))
  
  df_data <- sf_data_to_df(sf_data_clean)
  return(df_data)
}

# ---------------------------------------
# FUNCTION THAT UNFOLDS POINT GEOMETRIES TO DATA FRAME 
#   Includes columns (Lon, Lat) for crs = 4326 &
#   columns (X, Y) for crs = 25831
# ---------------------------------------

sf_data_to_df <- function(sf_data, df_data = NULL) {
  
  if (!inherits(sf_data, "sf")) {
    stop("El objeto proporcionado no es un 'sf'.")
  }
  
  geom_type <- unique(sf::st_geometry_type(sf_data))
  if (!all(geom_type %in% c("POINT", "MultiPoint"))) {
    stop('Las geometrías deben ser de tipo "POINT" o "MultiPoint".')
  }
  
  sf_data$Lon <- sf::st_coordinates(sf_data)[ , 1]
  sf_data$Lat <- sf::st_coordinates(sf_data)[ , 2]
  
  sf_data_25831 <- sf::st_transform(sf_data, crs = 25831)
  sf_data$X <- sf::st_coordinates(sf_data_25831)[ , 1]
  sf_data$Y <- sf::st_coordinates(sf_data_25831)[ , 2]
  
  sf_data_df <- sf::st_drop_geometry(sf_data)
  
  return(sf_data_df)
}


# ---------------------------------------
# FUNCTION TO PROCESS TRACKS
# ---------------------------------------

process_tracks <- function(journey_vector, df_data, output_filename) {
  
  if (inherits(df_data, "sf")) {
    df_data <- sf_data_to_df(df_data)
  }
  
  journey_vector <- journey_jonquillera
  required_cols <- c("journey", "GPSDateTime", "Cfr", "Lon", "Lat", 
                     "inside_port", "inland")
  missing_cols <- setdiff(required_cols, names(df_data))
  if (length(missing_cols) > 0) {
    stop(paste("Faltan columnas necesarias en df_data:",
               paste(missing_cols,
                     collapse = ", ")))
  }
  
  today_str <- format(Sys.Date(), "%Y-%m-%d")
  # Lista para acumular datasets válidos
  list_tracks <- list()
  duplicated_log <- data.frame()
  
  for (i in seq_along(journey_vector)) {
    journey_id <- journey_vector[i]
    
    # Separar componentes del journey
    temp <- unlist(strsplit(journey_id, " / "))
    track_day <- as.Date(temp[1]) - 1
    vesselName <- temp[2]
    cfr <- temp[3]
    
    # Extraer datos del journey actual
    dataset <- df_data %>% 
      filter(VesselName == vesselName,
             Cfr == cfr,
             as.Date(GPSDateTime) == track_day)
    
    if (nrow(dataset) == 0) {
      next
    }
    
    # Verificar duplicados (en base a columnas clave)
    duplicated_logical <- duplicated(dataset[, .(GPSDateTime, Cfr, Lon, Lat)])
    
    # Guardar duplicados si existen
    if (any(duplicated_logical)) {
      duplicated_file <- paste0(rdata_dir, "duplicated_bips_", today_str, ".csv")
      duplicated_rows <- dataset[duplicated_logical]
      duplicated_log <- rbind(duplicated_log,
                              duplicated_rows %>% 
                                mutate(reason = "duplicated bip"))
      write.csv(duplicated_log, duplicated_file, row.names = FALSE,
                fileEncoding = "ISO-8859-1")
    }
    
    # Eliminar duplicados del dataset
    dataset <- dataset[!duplicated_logical]
    
    list_tracks[[length(list_tracks) + 1]] <- dataset
  }

  # Combinar todos los tracks procesados
  if (length(list_tracks) == 0) {
    warning("No se encontró ningún track válido.")
    df_tracks <- data.frame()
  } else {
    df_tracks <- do.call(rbind, list_tracks)
  }
  
  # Guardar los resultados
  return(df_tracks)
}

df_data <- process_gps_data(n_days = 400, 
                            tracks_dir = tracks_dir,
                            shp_dir = shp_dir,
                            reference_dir = reference_dir,
                            logs_dir = logs_dir)

# save.image("df_data_check.RData")
# load("df_data_check.RData")

metiers <- c("jonquillera", "tresmall", "palangre", "nasa", "cercol", "llampuguera")
list_metier_tracks <- list()
for (metier in metiers) {
  journey_var <- get(paste0("journey_", metier))
  filename <- paste0("raw_", metier, "_tracks.RData")
  tracks <- process_tracks(journey_var, df_data, filename)
  list_metier_tracks[[metier]] <- tracks
  
  if (nrow(tracks) > 0) {
    save(tracks, file = file.path(rdata_dir, filename))
  } else {
    message(paste("No se guardó", metier, "porque el dataset está vacío."))
  }
}

tracks_processed <- rbindlist(list_metier_tracks)
journey_combinado <- c(journey_jonquillera, journey_tresmall)
tracks_jonquillera <- list_metier_tracks[[jonquillera]]
tracks_tresmall <- list_metier_tracks[[tresmall]]

journey_track_jonquillera <- unique(tracks_jonquillera$journey)
journey_track_tresmall <- unique(tracks_tresmall$journey)
gps_journey <- unique(journey_combinado$journey)
# --- CALCULO DE METRICAS ---
# --- Extraer fechas únicas

venta_fechas <- as.Date(sapply(strsplit(journey_combinado, " / "), `[`, 1))
gps_fechas   <- as.Date(sapply(strsplit(gps_journey, " / "), `[`, 1))

fechas_venta <- sort(unique(venta_fechas))
fechas_gps   <- sort(unique(gps_fechas))

# --- Inicializar data.frames para guardar métricas
salidas_sin_venta_df <- data.frame()
ventas_sin_salida_df <- data.frame()

today_str <- format(Sys.Date(), "%Y-%m-%d")
# --- Bucle por cada fecha de venta
salidas_sin_venta_file <- paste0(rdata_dir, "salida_sin_venta_", today_str, ".csv")
ventas_sin_salida_file <- paste0(rdata_dir, "ventas_sin_salida_", today_str, ".csv")
for (dia in fechas_venta) {
  dia <- as.Date(dia)
  dia_gps <- dia - 1

  # CFRs con venta el día actual
  ventas_dia <- journey_combinado[venta_fechas == dia]
  ventas_cfrs <- unique(sapply(strsplit(ventas_dia, " / "), function(x) x[3]))

  # CFRs con GPS el día anterior
  gps_dia <- gps_journey[gps_fechas == dia_gps]
  gps_cfrs <- unique(sapply(strsplit(gps_dia, " / "), function(x) x[3]))

  # --- Comparar
  salida_sin_venta <- setdiff(gps_cfrs, ventas_cfrs)
  venta_sin_salida <- setdiff(ventas_cfrs, gps_cfrs)

  # --- Almacenar resultados
  if (length(salida_sin_venta) > 0) {
    salidas_sin_venta_df <- rbind(
      salidas_sin_venta_df,
      data.frame(fecha = as.character(dia_gps), tipo = "sin_venta", cfr = salida_sin_venta)
    )
  }
  if (length(venta_sin_salida) > 0) {
    ventas_sin_salida_df <- rbind(
      ventas_sin_salida_df,
      data.frame(fecha = as.character(dia), tipo = "sin_salida", cfr = venta_sin_salida)
    )
  }
}

write.csv(salidas_sin_venta_df, salidas_sin_venta_file, row.names = FALSE,
          fileEncoding = "ISO-8859-1")
write.csv(ventas_sin_salida_df, ventas_sin_salida_file, row.names = FALSE,
          fileEncoding = "ISO-8859-1")

cat("Track linking process completed successfully.\n")

# save.image("historico_predicted_gps.RData")
