
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


remove(list=ls())



library(lubridate)
library(mapview)
library(lubridate)
library(mapview)
library(sf)
library(ggplot2)
library(stringr)
library(dplyr)
library(sf)
library(data.table)
library(tidyr)


puertos = st_read("../shp/Buffer_Puertos.shp")
morunas = st_read("../shp/Morunas.shp")

#PARA CAJAS VERDES---------------------------------------------------------
# data$Codi <- sub("^CV_(.*)$", "\\1", data$layer)# Crear la columna Codi extrayendo los valores después de TP_
# data$GPSDateTim = as.POSIXct(strptime(data$GPSDateTim, format="%Y/%m/%d %H:%M:%S"))
# data$time = data$GPSDateTim
# track = read.csv("CV_Llagosta_2024_v2.csv")
# lista = read.csv("Llista_Llagosta.csv", sep = ";")

# PARA TRACKS--------------------------------------------------------------
# data$Codi <- sub("^TP_(.*)$", "\\1", data$layer)# Crear la columna Codi extrayendo los valores después de TP_
# data$time = as.POSIXct(strptime(data$time, format="%Y/%m/%d %H:%M:%S"))
# data$GPSDateTim = data$time



#PARA EL PREDICTED----------------------------------------------------------
load("llagostaPredicted.RData")
temp <- c("OPMALLORCAMAR")
temp3 <- NULL
for(i in seq_along(temp)){
  temp2 <- which(grepl(temp[i],rownames(predicted)))
  temp3 <- append(temp3,temp2)
}
predicted <- predicted[-temp3,]
predicted.ID <- predicted.ID[-temp3]
# lista de tracks(archivos diarios cajas verdes + buffer puertos)
directory <- "../../05_CaixesVerdes/2024"
file.names <- list.files(directory)

# Inicializar la barra de progreso
pb <- txtProgressBar(min = 0, max = length(predicted.ID), style = 3)
# Iniciar acumuladores.
df_input <- NULL
sf_final <- NULL
barcas_sin_track <- NULL
na_control <- NULL
bips_duplicados <- NULL
barcas_menos_dos_bips <- NULL
df_tracks <- NULL

df_tracks <- data.frame()  # Inicializar si está vacío

for(i in seq_along(predicted.ID)){
  
  temp <- unlist(strsplit(predicted.ID[i]," / ")) # Separa la columna en Día, Nombre y CFR
  day <- as.Date(temp[1])-1  # Día del track
  temp2 <- paste("Daily_", gsub("-","", as.character(day)), ".csv", sep="")  # Nombre del CSV
  temp2 <- which(file.names == temp2)  # Buscar el archivo
  
  if(length(temp2) != 0){
    data = fread(paste0(directory, "/", file.names[temp2]), check.names = TRUE)
    temp2 <- which(data$Cfr == temp[3])  # Filtrar por CFR
    
    if(length(temp2) != 0){  # Si la embarcación aparece en el archivo
      data <- data[temp2, c("Id", "Latitude", "Longitude", "Speed", "GPSDateTime", "VesselName", "Cfr")]
      data$GPSDateTime <- as.POSIXct(data$GPSDateTime, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
      
      # Eliminar valores NA si existen
      if(any(is.na(data))){
        temp <- which(is.na(data))
        na_control <- rbind(na_control, data[temp, ])
        data <- data[-temp, ]
      }
      
      # **LIMPIAR PUERTOS**
      sf_data <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
      sf_data <- sf_data[!st_intersects(sf_data, puertos, sparse = FALSE), ]
      # sf_data$Lon <- st_coordinates(sf_data)[, 1]  
      # sf_data$Lat <- st_coordinates(sf_data)[, 2]  
      sf_data_25831 <- st_transform(sf_data, crs = 25831)
      sf_data$X <- st_coordinates(sf_data_25831)[, 1]  
      sf_data$Y <- st_coordinates(sf_data_25831)[, 2]  
      data <- st_drop_geometry(sf_data)
      #CAMBIAR SEGUN METIER
      if(nrow(data) > 5){
        temp <- which(duplicated(data$GPSDateTime))
        if(length(temp) != 0){
          bips_duplicados <- rbind(bips_duplicados, data[temp, ])
          data <- data[-temp, ]
        }
        
        # **🔴 Agregar datos originales sin interpolación**
        df_tracks <- rbind(df_tracks, data)
        
      } else {
        barcas_menos_dos_bips <- append(barcas_menos_dos_bips, predicted.ID[i])
      }
      
      setTxtProgressBar(pb, i)
      flush.console()
    } else {
      barcas_sin_track <- append(barcas_sin_track, predicted.ID[i])
    }
  }
}


# Aqui eliminamos si  hay algún NA
df_tracks_clean <- df_tracks %>%
  filter(!is.na(Lon) & !is.na(Lat))

sf_data <- st_as_sf(df_tracks_clean, coords = c("Lon", "Lat"), crs = 4326)


#mapview(sf_data)
temp_duplicados <- bips_duplicados[!apply(is.na(bips_duplicados),1,all),]
temp2 <- sample(temp_duplicados$VesselName,1)
sf_duplicados <- st_as_sf(temp_duplicados[which(temp_duplicados$VesselName == temp2)], coords = c("Lon", "Lat"), crs = 4326)
mapview(sf_duplicados)


#ELIMINAR PUERTOS
sum(st_is_empty(sf_data))  # ¿Cuántos puntos están vacíos?
puntos_en_puertos <- st_within(sf_data, puertos, sparse = FALSE)
sf_data_limpio <- sf_data[!apply(puntos_en_puertos, 1, any), ]


mapview(sf_data_limpio) + mapview(puertos)



sf_data_25831 <- st_transform(sf_data_limpio, crs = 25831)

df_tracks = sf_data_25831



df_tracks$GPSDate <- date(df_tracks$GPSDateTime)

df_tracks$Codi <- paste(df_tracks$GPSDate, df_tracks$VesselName, df_tracks$Cfr, sep = "//")





# Ordenar los datos por tiempo
sf_data_25831 <- df_tracks %>%
  arrange(Codi, GPSDateTime)

# Calcular la diferencia de tiempo en segundos
sf_data_25831 <- sf_data_25831 %>%
  group_by(Codi) %>%
  mutate(time_diff = as.numeric(difftime(GPSDateTime, lag(GPSDateTime), units = "secs")),
         sortida = cumsum(is.na(time_diff) | time_diff > 3600) + 1) %>%
  ungroup()


# Extraer las coordenadas transformadas
coords_25831 <- st_coordinates(sf_data_25831)


data = sf_data_25831




#----------
# preparing data: input and state.list
#----------

# Definir parámetros
period = 4 * 60 # Aquí cambiaríamos 2 para jonquillo, 4 para gerret
journey.list = unique(data$Codi)
n.journeys = length(journey.list)
input = NULL

# Iterar sobre cada viaje en la lista de viajes
for (i in seq_along(journey.list)) {
  journey_data = data[data$Codi == journey.list[i], ]
  sortida.list = unique(journey_data$sortida)
  
  for (j in seq_along(sortida.list)) {
    # Seleccionar puntos para la sortida actual dentro del journey
    temp = which(journey_data$sortida == sortida.list[j])
    
    # Asegurar que los puntos sean consecutivos en el tiempo
    temp = temp[order(journey_data$GPSDateTime[temp])]
    
    # Eliminar puntos duplicados (mismo momento)
    diff = diff(journey_data$GPSDateTime[temp])
    units(diff) = "secs"
    temp2 = which(diff == 0)
    if (length(temp2) > 0) {
      temp = temp[-temp2]
    }
    
    # Crear una serie temporal distribuida uniformemente
    target = seq(min(journey_data$GPSDateTime[temp]), 
                 max(journey_data$GPSDateTime[temp]), 
                 by = period)
    
    y.hat = data.frame(approx(journey_data$GPSDateTime[temp], 
                              journey_data$Y[temp], 
                              xout = target, rule = 2, method = "linear", ties = mean))
    x.hat = data.frame(approx(journey_data$GPSDateTime[temp], 
                              journey_data$X[temp], 
                              xout = target, rule = 2, method = "linear", ties = mean))
    
    # Definir estados como "Transit" para toda la serie
    state = rep("Transit", length(target))
    
        # Crear un data frame con las variables
    temp_df = data.frame(x = x.hat$y, y = y.hat$y, time = target, state = state)
    temp_df$journey = journey.list[i]
    temp_df$sortida = sortida.list[j]
    
    # Graficar los resultados
    plot(temp_df$x, temp_df$y, col = "black", pch = 19, 
         main = paste(journey.list[i], sortida.list[j], sep = " - "))
    lines(temp_df$x, temp_df$y)
    points(temp_df$x[1], temp_df$y[1], col = "blue", cex = 2)
    points(temp_df$x[nrow(temp_df)], temp_df$y[nrow(temp_df)], col = "red", cex = 2)
    legend("topright",
           c("Transit", "first", "last"),
           pch = c(19, 1, 1),
           col = c(1, "blue", "red"), cex = 0.5)
    
    # Acumular los datos en input
    input = rbind(input, temp_df)
  }
}

input$Codi = input$journey



input$x=input$x+runif(length(input$x),-1,1)
input$y=input$y+runif(length(input$y),-1,1)


#PASAMOS A SF
sf_input = st_as_sf(input, coords = c("x", "y"), crs = 25831)






#ELIMINAR MORUNAS
# Transformar al sistema de coordenadas EPSG: 25831
morunas <- st_transform(morunas, crs = 25831)

# sum(st_is_empty(sf_data_sin_puertos))  # ¿Cuántos puntos están vacíos?
puntos_morunas <- st_within(sf_input, morunas, sparse = FALSE)
sf_input <- sf_input[!apply(puntos_morunas, 1, any), ]

mapview(sf_input, zcol = "state") + mapview(morunas, col.region="yellow")



#QUITAR GEOMETRIAS para tener data frame
# Extraer coordenadas
coords <- st_coordinates(sf_input) %>%
  as.data.frame() %>%
  rename(x = X, y = Y)

# Convertir sf a data frame y agregar coordenadas
input <- sf_input %>%
  st_drop_geometry() %>%
  bind_cols(coords)

# Verificar el resultado
head(input)











save(input, period,
     journey.list,n.journeys,file="input_Llagosta_2024.RData")





#--------------------------------FIN---------------------------------------------------------
