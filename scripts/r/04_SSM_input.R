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

# Required Libraries
library(data.table)
library(sf)
library(dplyr)
library(lubridate)
library(stats)

rm(list = ls())

# --- Configuration ---
metier_name <- "tresmall"        # Change to "jonquillo", "gerret", etc.
period <- 4 * 60                 # Seconds between points: e.g. 2*60 for jonquillo

# --- Folder Paths ---
setwd("C:/Users/UIB/Desktop/REMAR-automatizacion/REMAR-automatizacion/scripts/r")
rdata_dir <- "../../data/rdata"
shp_morunas <- "../../data/shp/Morunas.shp"

# --- Load Data ---
load(file.path(rdata_dir, paste0("raw_", metier_name, "_tracks.RData")))
morunas <- st_read(shp_morunas, quiet = TRUE)

# --- Preprocess Tracks ---
df_tracks$GPSDate <- as.Date(df_tracks$GPSDateTime)
df_tracks$journey <- paste(df_tracks$GPSDate, df_tracks$VesselName, df_tracks$Cfr, sep = "//")

# Sort by journey and time
df_tracks <- df_tracks %>%
  arrange(journey, GPSDateTime)

# Calculate time differences and detect separate outings (sortidas)
df_tracks <- df_tracks %>%
  group_by(journey) %>%
  mutate(time_diff = as.numeric(difftime(GPSDateTime, lag(GPSDateTime), units = "secs")),
         sortida = cumsum(is.na(time_diff) | time_diff > 3600)) %>%
  ungroup()

# Filter tracks with at least 5 valid points
valid_journeys <- df_tracks %>%
  group_by(journey) %>%
  summarise(n_points = n(), .groups = "drop") %>%
  filter(n_points >= 5) %>%
  pull(journey)

data <- df_tracks %>%
  filter(journey %in% valid_journeys)

journey_list <- unique(data$journey)
n_journeys <- length(journey_list)

# --- Interpolation ---
input <- data.frame()
for(i in seq_along(journey_list)) {
  journey_data <- data[data$journey == journey_list[i], ]
  sortida_list <- unique(journey_data$sortida)

  for (j in seq_along(sortida_list)) {
    idx <- which(journey_data$sortida == sortida_list[j])
    idx <- idx[order(journey_data$GPSDateTime[idx])]

    # Check minimum valid points
    if (length(idx) < 2 ||
        sum(!is.na(journey_data$GPSDateTime[idx])) < 2 ||
        sum(!is.na(journey_data$Y[idx])) < 2 ||
        sum(!is.na(journey_data$X[idx])) < 2) {
      next
    }

    # Generate regular time series
    target <- seq(min(journey_data$GPSDateTime[idx]),
                  max(journey_data$GPSDateTime[idx]),
                  by = period)

    y_hat <- approx(journey_data$GPSDateTime[idx], journey_data$Y[idx], xout = target)$y
    x_hat <- approx(journey_data$GPSDateTime[idx], journey_data$X[idx], xout = target)$y

    temp_df <- data.frame(
      x = x_hat,
      y = y_hat,
      time = target,
      state = "Transit",
      journey = journey_list[i],
      sortida = sortida_list[j]
    )

    input <- rbind(input, temp_df)
  }
}

# --- Final Processing ---
# Small jitter to avoid overlapping points
input$x <- input$x + runif(nrow(input), -1, 1)
input$y <- input$y + runif(nrow(input), -1, 1)

# Convert to sf and remove points within "morunas"
sf_input <- st_as_sf(input, coords = c("x", "y"), crs = 25831)
morunas <- st_transform(morunas, crs = 25831)

inside_morunas <- lengths(st_intersects(sf_input, morunas)) > 0
sf_input <- sf_input[!inside_morunas, ]

# Extract coords and convert back to df
sf_input$X <- st_coordinates(sf_input)[, 1]
sf_input$Y <- st_coordinates(sf_input)[, 2]
input <- st_drop_geometry(sf_input)

# --- Save Output ---
journey_list <- unique(input$journey)
save(input, period, journey_list, n_journeys,
     file = file.path(rdata_dir, paste0("input_", metier_name, ".RData")))

cat("✅ Interpolation complete for metier:", metier_name, "\n")
