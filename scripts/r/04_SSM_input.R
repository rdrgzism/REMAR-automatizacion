#!/usr/bin/env Rscript

#---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   5/6 - Track Interpolation & Preprocessing for SSM
#---------------------------------------

# Author: Jose María Disder
# Reviewed and documented by: Ismael Rodríguez
# Date: 2025-04-30

# This script prepares cleaned, interpolated GPS tracks to be used as input
# for a State Space Model (SSM). It links voyages with their GPS data,
# interpolates tracks at regular intervals, and removes invalid regions.

# --- Libraries
library(data.table)
library(sf)
library(dplyr)
library(lubridate)
library(stats)

rm(list = ls())

# # --- Folder Paths ---
setwd(file.path(Sys.getenv("HOME"), "REMAR-automatizacion/scripts/r"))
rdata_dir      <- file.path(Sys.getenv("HOME"), "REMAR-automatizacion/data/rdata")
shp_dir   <- file.path(Sys.getenv("HOME"), "REMAR-automatizacion/data/shp")

# rdata_dir <- "../.data./data/rdata"
# shp_dir "../../../../shp" 

# --- Configuration --
metier_config <- list(
  jonquillera = list(period = 2 * 60),
  tresmall = list(period = 4 * 60)
)
metier_names <- names(metier_config)

# --- Load static shapefile ---
morunas_path <- file.path(shp_dir, "Morunas.shp")
morunas <- st_read(morunas_path, quiet = TRUE)
morunas <- st_transform(morunas, crs = 25831)

# --- Loop over metiers ---
for (metier_name in metier_names) {
  period <- metier_config[[metier_name]]$period
  tracks_path <- file.path(
    rdata_dir, paste0("raw_", metier_name, "_tracks.RData"))
  # --- Load Data ---
  if (file.exists(tracks_path)) {
    load(tracks_path)         # OUT and journey_list
  } else{
    stop("El archivo '", tracks_path,"' no se encuentra en el directorio especificado.")
  }
  
  # Ordenar y agrupar
  data <- tracks %>%
    arrange(journey, GPSDateTime) %>%
    group_by(journey) %>%
    mutate(time_diff = as.numeric(difftime(GPSDateTime, lag(GPSDateTime), units = "secs")),
           sortida = cumsum(is.na(time_diff) | time_diff > 3600)) %>%
    ungroup()
  
  journey_list <- unique(data$journey)
  n_journeys <- length(journey_list)
  
  # --- Interpolación ---
  input_list <- list()
  counter <- 1
  
  for (i in seq_along(journey_list)) {
    journey_data <- data[data$journey == journey_list[i], ]
    sortida_list <- unique(journey_data$sortida)
    
    for (j in sortida_list) {
      idx <- which(journey_data$sortida == j)
      
      x_vals <- journey_data$GPSDateTime[idx]
      x_coords <- journey_data$X[idx]
      y_coords <- journey_data$Y[idx]
      
      # Validación: mínimo dos valores únicos y no-NA en X e Y
      if (length(idx) < 2 ||
          sum(!is.na(x_vals)) < 2 ||
          sum(!is.na(x_coords)) < 2 ||
          sum(!is.na(y_coords)) < 2 ||
          length(unique(x_vals[!is.na(x_vals)])) < 2) {
        next
      }
      
      target <- seq(min(x_vals, na.rm = TRUE), max(x_vals, na.rm = TRUE), by = period)
      
      x_hat <- approx(x_vals, x_coords, xout = target)$y
      y_hat <- approx(x_vals, y_coords, xout = target)$y
      
      
      temp_df <- data.frame(
        x = x_hat,
        y = y_hat,
        time = target,
        state = "Transit",
        journey = journey_list[i],
        sortida = j
      )
      
      input_list[[counter]] <- temp_df
      counter <- counter + 1
    }
  }
  
  input <- do.call(rbind, input_list)
  if (nrow(input) == 0) {
    warning(paste("No se generaron datos válidos para el metier:", metier_name))
    next
  }
  
  # --- Limpieza final ---
  input$x <- input$x + runif(nrow(input), -1, 1)
  input$y <- input$y + runif(nrow(input), -1, 1)
  
  sf_input <- st_as_sf(input, coords = c("x", "y"), crs = 25831)
  inside_morunas <- lengths(st_intersects(sf_input, morunas)) > 0
  sf_input <- sf_input[!inside_morunas, ]
  
  sf_input$X <- st_coordinates(sf_input)[, 1]
  sf_input$Y <- st_coordinates(sf_input)[, 2]
  input <- st_drop_geometry(sf_input)
  
  # --- Guardar ---
  journey_list <- unique(input$journey)
  
  save(input, period, journey_list, n_journeys,
       file = file.path(rdata_dir, paste0("input_", metier_name, ".RData")))

  cat("Finalizado:", metier_name, "(", nrow(input), "puntos interpolados )\n\n")
}

# save.image("SSM_input_check.RData")
