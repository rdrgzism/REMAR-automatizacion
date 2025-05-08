#!/usr/bin/env Rscript

# ---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   Script 4/6 - Link classified fishing journeys to GPS tracks
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
library(data.table)
library(sf)
library(dplyr)
library(lubridate)

rm(list = ls())

# --- Folder paths
setwd("C:/Users/UIB/Desktop/REMAR-automatizacion/REMAR-automatizacion/scripts/r")
rdata_dir   <- "../../data/rdata"
tracks_dir  <- "../../data/cajasverdes"
shp_ports   <- "../../data/shp/Buffer_Puertos.shp"

# --- Load data
load(file.path(rdata_dir, "predicted.RData"))
ports <- st_read(shp_ports, quiet = TRUE)
tracks_names <- list.files(tracks_dir)

# ---------------------------------------
# FUNCTION TO PROCESS TRACKS
# ---------------------------------------
process_tracks <- function(journey_vector, output_filename) {
  df_tracks <- data.frame()
  na_control <- data.frame()
  duplicated_bips <- data.frame()
  boats_no_track <- character()
  less_than_n_bips <- character()

  for(i in seq_along(journey_vector)) {
    # Parse journey into components
    temp <- unlist(strsplit(journey_vector[i], " / "))
    track_day <- as.Date(temp[1]) - 1
    cfr <- temp[3]

    track_file_name <- paste0("Daily_", gsub("-", "", as.character(track_day)), ".csv")
    match_file <- tracks_names[tracks_names == track_file_name]

    if(length(match_file) == 1){
      dataset <- fread(file.path(tracks_dir, match_file), check.names = TRUE)
      dataset <- dataset[dataset$Cfr == cfr, ]

      if(nrow(dataset) > 0){
        dataset <- dataset[, .(Id, Latitude, Longitude, Speed, GPSDateTime, VesselName, Cfr)]
        dataset$GPSDateTime <- as.POSIXct(dataset$GPSDateTime, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

        if(anyNA(dataset)){
          na_control <- rbind(na_control, dataset[!complete.cases(dataset), ])
          dataset <- dataset[complete.cases(dataset), ]
        }

        # Remove port points
        sf_data <- st_as_sf(dataset, coords = c("Longitude", "Latitude"), crs = 4326)
        inside_port <- lengths(st_intersects(sf_data, ports)) > 0
        sf_data <- sf_data[!inside_port, ]

        sf_data$Lon <- st_coordinates(sf_data)[, 1]
        sf_data$Lat <- st_coordinates(sf_data)[, 2]
        sf_data_25831 <- st_transform(sf_data, crs = 25831)
        sf_data$X <- st_coordinates(sf_data_25831)[, 1]
        sf_data$Y <- st_coordinates(sf_data_25831)[, 2]

        dataset <- st_drop_geometry(sf_data)

        if(nrow(dataset) > 2){
          duplicated_idx <- duplicated(dataset$GPSDateTime)
          if(any(duplicated_idx)){
            duplicated_bips <- rbind(duplicated_bips, dataset[duplicated_idx, ])
            dataset <- dataset[!duplicated_idx, ]
          }

          df_tracks <- rbind(df_tracks, dataset)
        } else {
          less_than_n_bips <- c(less_than_n_bips, journey_vector[i])
        }

      } else {
        boats_no_track <- c(boats_no_track, journey_vector[i])
      }
    }
  }

  save(df_tracks, file = file.path(rdata_dir, output_filename))

  # Optionally, write logs
  # today <- format(Sys.Date(), "%Y-%m-%d")
  # write.csv(na_control, file.path("../../logs", paste0("na_control_", today, ".csv")), row.names = FALSE)
  # write.csv(duplicated_bips, file.path("../../logs", paste0("duplicated_bips_", today, ".csv")), row.names = FALSE)
  # writeLines(boats_no_track, file.path("../../logs", paste0("boats_no_track_", today, ".txt")))
  # writeLines(less_than_n_bips, file.path("../../logs", paste0("less_than_n_bips_", today, ".txt")))
}

# --- Process jonquillo journeys
process_tracks(journey_jonquillo, "raw_jonquillo_tracks.RData")

# --- Process tresmall journeys
process_tracks(journey_tresmall, "raw_tresmall_tracks.RData")

cat("✅ Track linking process completed successfully.\n")
