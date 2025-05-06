#!/usr/bin/env Rscript

#---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   4/5
#---------------------------------------

# Ismael Rodríguez
# 05/05/2025

# Objective: Linkage of classified sales bills to its corresponding GPS track.
# INPUT:
# (1) Classified species weights' matrix.

# OUTPUT:
# (1) "SSM_input.RData": Clean GPS tracks for the given fishing gear.

# --- Libraries
library(data.table)
library(sf)
library(dplyr)
library(lubridate)

rm(list = ls())

# --- Folder paths
root_dir <- file.path(
  "C:/Users/UIB/Desktop/REMAR-automatizacion/REMAR-automatizacion"
  )
tracks_dir <- file.path(root_dir,"data", "cajasverdes")
rdata_dir <- file.path(root_dir, "data", "rdata")   # output directory
shp_ports <- file.path(root_dir, "data", "shp", "Buffer_Puertos.shp")


# Load data
load(file.path(rdata_dir, "OUT.RData"))
ports <- st_read(shp_ports, quiet = TRUE)
file_names <- list.files(tracks_dir)

# Initialize accumulators.
df_tracks <- data.frame()
na_control <- data.frame()
duplicated_bips <- data.frame()
boats_no_track <- data.frame()
less_than_n_bips <- data.frame()

# Processing data
for(i in seq_along(predicted_ID)){
  # Break the predicted ID (journey) into it's components, boat_name, date & CFR
  temp <- unlist(strsplit(predicted_ID[i]," / "))
  track_day <- as.Date(temp[1]) - 1  # sale day - 1, the day prior to the sale
  cfr <- temp[3]
  
  track_file_name <- paste(
    "Daily_", gsub("-","", as.character(track_day)), ".csv", sep=""
  )
  # Match the generated name with the track day to the file name of the 
  # received GPS track's file
  match_file <- file_names[file_names == track_file_name]
  
  if(length(match_file) == 1){
    dataset <- fread(file.path(tracks_dir, match_file), check.names = TRUE)
    dataset <- dataset[dataset$Cfr == cfr,]
    
    if(nrow(dataset) > 0){   # That boat did go fishing the given day
      dataset <- dataset[, .("Id", "Latitude", "Longitude", "Speed", "GPSDateTime", "VesselName", "Cfr")]
      dataset$GPSDateTime <- as.POSIXct(
        dataset$GPSDateTime, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"
      )
      if(anyNA(dataset)){
        na_control <- rbind(na_control, dataset[!complete.case(dataset), ])
        dataset <- dataset[complete.cases(dataset), ]
      }
      
      # --- CLEANING PORTS: removing points inside ports buffers
      sf_data <- st_as_sf(
        dataset, coords = c("Longitude", "Latitude"), crs = 4326
      )
      sf_data <- sf_data[!st_intersects(sf_data, shp_ports, sparse = FALSE), ]
      sf_data$Lon <- st_coordinates(sf_data)[, 1]  
      sf_data$Lat <- st_coordinates(sf_data)[, 2]  
      sf_data_25831 <- st_transform(sf_data, crs = 25831)
      sf_data$X <- st_coordinates(sf_data_25831)[, 1]  
      sf_data$Y <- st_coordinates(sf_data_25831)[, 2]  
      dataset <- st_drop_geometry(sf_data)
      
      if(nrow(dataset) > 2){
        # Remove duplicates
        temp <- duplicated(dataset$GPSDateTime)
        if(any(temp)){
          duplicated_bips <- rbind(duplicated_bips, dataset[temp, ])
          dataset <- dataset[!temp, ]
        }
        # **Original data set without interpolation**
        df_tracks <- rbind(df_tracks, dataset)
      } 
    } else{
      less_than_n_bips <- rbind(less_than_n_bips, predicted_ID[i])
    }
  } else{
    boats_no_track <- c(boats_no_track, predicted_ID[i])
  }
}

# --- Export final data
today_str <- format(Sys.Date(), "%Y-%m-%d")

save(df_tracks, file.path(
  rdata_dir, paste0("predicted_tracks", today_str, ".csv")
  ), row.names = FALSE)

# --- Write logs
write.csv(na_control, 
          file.path("../../logs/", paste0("na_control_", today_str, ".csv"))
          )
write.csv(duplicated_bips,
          file.path("../../logs/", 
                    paste0("bips_duplicados_", today_str, ".csv")
                    )
          )
writeLine(boats_no_track, 
          file.path("../../logs", 
                    paste0("barcos_sin_track_", today_str, ".csv")
                    )
          )
writeLine(less_than_n_bips, 
          file.path("../../logs",
                    paste0("barcos_pocos_puntos_", today_str, ".csv")
                    )
          )

cat("Track linking process completed successfully. \n")