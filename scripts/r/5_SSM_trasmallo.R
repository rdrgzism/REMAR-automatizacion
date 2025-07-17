#!/usr/bin/env Rscript

#---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   6/6 - State estimation via HMM & track segmentation
#---------------------------------------

# Author: Miquel Palmer Vidal
# Reviewed and documented: Ismael Rodríguez
# Date: 2025-05-08

# --- Libraries
library(data.table)
library(sf)
library(dplyr)
library(posterior)
library(cmdstanr)
library(moveHMM)
library(lubridate)
library(eRTG3D)

rm(list = ls())

# --- Configuration

metier <- "tresmall" #cambia por ser el output dle anterior
period <- 4 * 60  # seconds between interpolated points (adjust per metier)

# --- Folder paths
setwd("C:/Users/usuario/OneDrive - Universitat de les Illes Balears/REMAR (REcursos MARins pesquers a Balears)/P. artesanal/14_Automatitzacio/REMAR-automatizacion/scripts/r")
rdata_dir <- "../../data/rdata"
shp_dir <- "../../data/shp"
models_dir <- "../../models"

# Load model and input data
if (file.path(rdata_dir, paste0("input_", metier, ".RData"))) {
  mod <- cmdstan_model(file.path(models_dir, paste0("model_", metier,".stan")))
  load(file.path(rdata_dir, paste0("priors_modelo_", metier, ".RData")))
  load(file.path(rdata_dir, paste0("input_", metier, ".RData")))
} else{
  stop(paste("El archivo", paste0("input_", metier, ".RData"),
             "no se encuentra en el directorio especificado."))
}

# Format timestamp and journey ID
input$time <- as.POSIXct(input$time)

# Track grouping metadata
unique_tracks <- unique(input[, c("sortida", "journey")])
df_output <- list()

# Progress bar
pb <- txtProgressBar(min = 0, max = length(unique_tracks), style = 3)

# Loop through tracks
for (i in seq_len(nrow(unique_tracks))) {
  journey_id <- unique_tracks$journey[i]
  sortida_id <- unique_tracks$sortida[i]

  track_data <- input[input$journey == journey_id & input$sortida == sortida_id, ]

  if (nrow(track_data) < 3) next

  # Remove duplicates
  track_data <- track_data[!duplicated(track_data$time), ]
  if (nrow(track_data) < 3) next

  # Prepare data for step-angle extraction
  prep <- prepData(track_data, type = "UTM", coordNames = c("X", "Y"))
  steps_angles <- prep[2:(nrow(prep) - 1), c("step", "angle")]
  clean_middle <- track_data[2:(nrow(track_data) - 1), ]

  # Remove NAs
  valid <- complete.cases(steps_angles)
  steps_angles <- steps_angles[valid, ]
  clean_middle <- clean_middle[valid, ]
  if (nrow(steps_angles) < 1) next

  # Stan input
  stan_data <- list(
    T = nrow(steps_angles),
    N = 3,
    steps = steps_angles$step,
    angles = steps_angles$angle,
    prior_shape_1 = c(priors[1,1], priors[1,3]),
    prior_shape_2 = c(priors[2,1], priors[2,3]),
    prior_shape_3 = c(priors[3,1], priors[3,3]),
    prior_rate_1 = c(priors[1,2], priors[1,4]),
    prior_rate_2 = c(priors[2,2], priors[2,4]),
    prior_rate_3 = c(priors[3,2], priors[3,4]),
    prior_x1 = c(5, 5), prior_x2 = c(5, 5), prior_x3 = c(5, 5)
  )

  # Init values
  n_chains <- 3
  inits <- replicate(n_chains, list(
    shape = priors[,1],
    rate = priors[,2],
    xangle = rep(5, 3),
    yangle = rep(0, 3)
  ), simplify = FALSE)

  # Run model
  fit <- mod$sample(
    data = stan_data,
    chains = n_chains, parallel_chains = 2,
    iter_warmup = 1000, iter_sampling = 1000,
    init = inits, max_treedepth = 12, adapt_delta = 0.9
  )

  z_rep <- as_draws_df(fit$draws("z_rep"))
  if (ncol(z_rep) == 0) next

  state_seq <- apply(z_rep[, 1:stan_data$T], 2, median)
  labels <- case_when(
    state_seq == 1 ~ "Transit",
    state_seq == 2 ~ "Calada",
    state_seq == 3 ~ "Recollida",
    TRUE ~ NA_character_
  )

  result <- cbind(clean_middle, hat = state_seq, obs = labels)
  df_output[[length(df_output) + 1]] <- result
  setTxtProgressBar(pb, i)
}

# Combine all results
final_df <- bind_rows(df_output)

# Apply smoothing over binary state (Recollida=1)
final_df$state_bin <- ifelse(final_df$hat == 3, 1, 0)
final_df$smoothed <- movingMedian(final_df$state_bin, window = 5)

# Grouping
final_df <- final_df %>%
  mutate(group = cumsum(lag(smoothed, default = first(smoothed)) != smoothed)) %>%
  group_by(group) %>%
  mutate(group_size = n()) %>%
  ungroup()

# Keep segments longer than 6 pts
final_df <- final_df %>%
  mutate(hat_grouped = ifelse(smoothed == 1 & group_size > 6, 1, 0))

# Convert to sf and group to lines
sf_output <- st_as_sf(final_df, coords = c("X", "Y"), crs = 25831)
sf_output <- sf_output %>% arrange(journey, time)

routes <- sf_output %>%
  filter(hat_grouped == 1) %>%
  group_by(journey, group) %>%
  summarise(
    start_time = min(time),
    end_time = max(time),
    duration = as.numeric(difftime(max(time), min(time), units = "min")),
    geometry = st_combine(geometry),
    .groups = "drop"
  ) %>%
  st_cast("LINESTRING")

routes$length_m <- round(st_length(routes), 0)
routes <- routes %>% mutate(tomo = row_number())

# Spatial intersection with planning units
p_units <- st_read(file.path(shp_dir, "P_units_REMAR.shp"), quiet = TRUE)
st_crs(routes) <- st_crs(p_units)
intersected <- st_intersection(routes, p_units[, c("ID_GRID")])
intersected <- intersected %>% mutate(length_segment = st_length(geometry))
intersected <- intersected %>% mutate(pct = (length_segment / length_m) * 100)

# Save shapefile
year_str <- year(min(input$time, na.rm = TRUE))
st_write(routes, paste0(metier, "_routes_", year_str, ".shp"), 
         delete_layer = TRUE)
st_write(intersected, paste0(metier, "_intersected_", year_str, ".shp"),
         delete_layer = TRUE)

save(final_df,routes, intersected, 
     file = paste0("final_SSM_", metier, "_", year_str, ".RData"))
