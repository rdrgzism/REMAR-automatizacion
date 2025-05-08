#!/usr/bin/env Rscript

#---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   4/5
#---------------------------------------

# Ismael Rodríguez
# 06/05/2025

# --- Libraries
library(data.table)
library(sf)
library(dplyr)
library(lubridate)
library(stats)

rm(list = ls())

# --- Folder paths
setwd(
  "C:/Users/UIB/Desktop/REMAR-automatizacion/REMAR-automatizacion/scripts/r"
)
rdata_dir <- "../../data/rdata"
shp_morunas <- "../../data/shp/Morunas.shp"
# Load data
load(file.path(rdata_dir, "raw_tresmall_tracks.RData"))
morunas <- st_read(shp_morunas, quiet = TRUE)

df_tracks$GPSDate <- as.Date(df_tracks$GPSDateTime)
df_tracks$Codi <- paste(df_tracks$GPSDate, df_tracks$VesselName, df_tracks$Cfr, sep = "//")

# Ordenar los datos por tiempo
df_tracks <- df_tracks %>%
  arrange(Codi, GPSDateTime)

# Calcular la diferencia de tiempo en segundos
# Situaciones de vuelta a puerto o fallo de CV durante ~1h.
df_tracks <- df_tracks %>%
  group_by(Codi) %>%
  mutate(time_diff = as.numeric(difftime(GPSDateTime, lag(GPSDateTime), units = "secs")),
         sortida = cumsum(is.na(time_diff) | time_diff > 3600)) %>%
  ungroup()

data <- df_tracks
#----------
# preparing data: input and state.list
#----------

codis_validos <- data %>%
  group_by(Codi) %>%
  summarise(n_puntos = n(), .groups = "drop") %>%
  filter(n_puntos >= 5) %>%  # Subirlo al cambiar el periodo para otras metieres
  arrange(n_puntos) %>%
  pull(Codi)

# Luego filtramos el dataframe original
data <- data %>%
  filter(Codi %in% codis_validos)

# Definir parámetros
period <- 4 * 60 # Aquí cambiaríamos 2 para jonquillo, 4 para gerret
journey_list <- unique(data$Codi)
n_journeys <- length(journey_list)
input <- data.frame()


# Bucle principal
for(i in seq_along(journey_list)) {

  journey_data <- data[data$Codi == journey_list[i], ]
  sortida_list <- unique(journey_data$sortida)
  
  for (j in seq_along(sortida_list)) {
    # Seleccionar puntos para la sortida actual dentro del journey
    temp <- which(journey_data$sortida == sortida_list[j])
    
    # Asegurar que los puntos sean consecutivos en el tiempo
    temp <- temp[order(journey_data$GPSDateTime[temp])]
    
    # Comprobar que haya al menos dos puntos válidos para interpolar
    if (length(temp) < 2 || 
        sum(!is.na(journey_data$GPSDateTime[temp])) < 2 ||
        sum(!is.na(journey_data$Y[temp])) < 2 || 
        sum(!is.na(journey_data$X[temp])) < 2) {
      next  # Saltar este caso si no hay suficientes datos
    }
    
    # Crear una serie temporal distribuida uniformemente
    target <- seq(min(journey_data$GPSDateTime[temp]), 
                 max(journey_data$GPSDateTime[temp]), 
                 by = period)
    
    y_hat <- data.frame(approx(journey_data$GPSDateTime[temp], 
                              journey_data$Y[temp], 
                              xout = target, rule = 2, method = "linear", ties = mean))
    x_hat <- data.frame(approx(journey_data$GPSDateTime[temp], 
                              journey_data$X[temp], 
                              xout = target, rule = 2, method = "linear", ties = mean))
    
    # Definir estados como "Transit" para toda la serie
    state <- rep("Transit", length(target))
    
    # Crear un data frame con las variables
    temp_df <- data.frame(x = x_hat$y, y = y_hat$y, time = target, state = state)
    temp_df$journey <- journey_list[i]
    temp_df$sortida <- sortida_list[j]
    
    # Acumular los datos en input
    input <- rbind(input, temp_df)
  }
}

# Small displacement to avoid same coordinates for different times
input$x <- input$x+runif(length(input$x),-1,1)
input$y <- input$y+runif(length(input$y),-1,1)

#PASAMOS A SF
sf_input <- st_as_sf(input, coords = c("x", "y"), crs = 25831)

#ELIMINAR MORUNAS
# Transformar al sistema de coordenadas EPSG: 25831
morunas <- st_transform(morunas, crs = 25831)
mapview(morunas) + mapview(sf_input, zcol = "journey")
# sum(st_is_empty(sf_data_sin_puertos))  # ¿Cuántos puntos están vacíos?
temp <- lengths(st_intersects(sf_input, morunas)) > 0
sf_input <- sf_input[!temp, ]

#QUITAR GEOMETRIAS para tener data frame
sf_input$X <- st_coordinates(sf_input)[, 1]  
sf_input$Y <- st_coordinates(sf_input)[, 2]  
input <- st_drop_geometry(sf_input)
journey_list <- unique(input$journey)

save(input, period,
     journey_list,n_journeys,file=file.path(rdata_dir, "input_llagosta.RData"))
