#-------------------------------------------------------------------------------      
# 0: CARGAR LIBRERIAS Y DEFINIR WORKING DIRECTORY
#-------------------------------------------------------------------------------
rm(list=ls())

library(data.table)
library(stringr)
library(posterior)
library(readr)
library(sf)
library(cmdstanr) #stan
library(moveHMM)
library(dplyr)
library(tibble)
rm(list = ls())

# --- Folder paths
setwd(
  "C:/Users/UIB/Desktop/REMAR-automatizacion/REMAR-automatizacion/scripts/r"
)
rdata_dir <- "../../data/rdata"
shp_dir <- "../../data/shp"
models_dir <- "../../models"

#-----
# 1.1: compiling model
#-----
mod <- cmdstan_model(file.path(models_dir, "model.stan"))

#-----
# 2: CARGAR DATOS E INICIALIZAR VARIABLES.
#-----

load(file.path(rdata_dir, "priors_modelo_langosta.RData"))
load(file.path(rdata_dir, "input_llagosta.RData"))

#filtrado por fecha
# Asegurarse de que la columna 'time' es de tipo POSIXct
input$time <- as.POSIXct(input$time)

# Inicializar la barra de progreso
pb <- txtProgressBar(min = 0, max = length(journey_list), style = 3)


# Iniciar acumuladores.
df_input <- NULL
sf_final <- NULL
barcas_sin_track <- NULL
na_control <- NULL
bips_duplicados <- NULL
barcas_menos_dos_bips <- NULL
df_tracks <- NULL
barcas_con_na <- c()
tracks_menos_de_tres <- NULL

# Obtener combinaciones únicas de Codi y sortida
unique_tracks <- unique(input[, c("sortida", "journey")])
length(unique(input$journey))
length(unique_tracks$journey)



# Crear una lista vacía para almacenar los tracks con menos de 3 datos
tracks_menos_de_tres <- list()

for (i in seq_len(nrow(unique_tracks))) {
  cat("Procesando iteración:", i, "\n")
  
  journey_actual <- unique_tracks$journey[i]
  sortida_actual <- unique_tracks$sortida[i]
  
  # Filtrar datos del track actual
  data <- input[input$journey == journey_actual &
                  input$sortida == sortida_actual, ]
  
  # Condición para saltar si no hay suficientes datos
  if (nrow(data) < 3) {
    # Almacenar el nombre del track (Codi) en la lista
    tracks_menos_de_tres <- append(tracks_menos_de_tres, journey_actual)
    cat("Iteración", i, "con menos de 3 datos. Track", journey_actual, "almacenado.\n")
    next
  }
  
  cat("Datos filtrados para la iteración", i, "tamaño:", nrow(data), "\n")
  
  # 1: Transformar coordenadas
  data_HHM <- prepData(data, type = "UTM", coordNames = c("X", "Y"))
  
  # Filtrar y eliminar duplicados
  temp <- which(duplicated(data$GPSDateTime))
  if (length(temp) > 0) {
    bips_duplicados <- rbind(bips_duplicados, data[temp,])
    data <- data[-temp,]  # Eliminar duplicados
  }
  
  # Si después de eliminar duplicados sigue habiendo pocos datos, saltar iteración
  if (nrow(data) < 3) next
  
  # Parte central del track (para ángulos y pasos)
  input_y <- data_HHM[2:(nrow(data_HHM) - 1), c("step", "angle")]
  data_middle <- data[2:(nrow(data) - 1), ]
  
  # Eliminar filas con NA en 'step' o 'angle'
  na_rows <- apply(input_y, 1, function(row) any(is.na(row)))
  if (any(na_rows)) {
    input_y <- input_y[!na_rows, , drop = FALSE]
    data_middle <- data_middle[!na_rows, , drop = FALSE]
    barcas_con_na <- append(barcas_con_na, paste(journey_actual, sortida_actual, sep = "_"))
  }
  
  # Si no queda suficiente data, saltar
  if (nrow(input_y) < 1) next
  
  input.stan <- list(
    N = nrow(input_y),
    y = input_y
  )
  
  df_input <- rbind(df_input, data)
  
  # 4: Definir priors e inicializaciones
  prior_shape_1 <- c(priors[1,1], priors[1,3]) 
  prior_shape_2 <- c(priors[2,1], priors[2,3])
  prior_shape_3 <- c(priors[3,1], priors[3,3])
  prior_rate_1 <- c(priors[1,2], priors[1,4])
  prior_rate_2 <- c(priors[2,2], priors[2,4])
  prior_rate_3 <- c(priors[3,2], priors[3,4])
  
  n.chains <- 3 
  initializer <- function() list(
    "shape" = priors[,1],
    "rate" = priors[,2],
    "xangle" = c(5,5,5),
    "yangle" = c(0,0,0)
  )
  inits <- lapply(1:n.chains, function(chain) initializer())
  
  # 5: Ejecutar modelo
  fit <- mod$sample(
    data = list(
      T = input.stan$N,
      N = 3,
      steps = input.stan$y[,1],
      angles = input.stan$y[,2],
      prior_shape_1 = prior_shape_1,
      prior_shape_2 = prior_shape_2,
      prior_shape_3 = prior_shape_3,
      prior_rate_1 = prior_rate_1,
      prior_rate_2 = prior_rate_2,
      prior_rate_3 = prior_rate_3,
      prior_x1 = c(5,5),
      prior_x2 = c(5,5),
      prior_x3 = c(5,5)
    ),
    chains = n.chains,
    parallel_chains = 2,
    iter_warmup = 1000,
    iter_sampling = 1000,
    init = inits,
    max_treedepth = 12,
    adapt_delta = 0.9
  )
  
  # Predicción de estados
  z_rep <- as_draws_df(fit$draws("z_rep"))
  str(z_rep)  # Para verificar la estructura
  
  # Verifica que z_rep tiene dimensiones correctas
  if (ncol(z_rep) > 0 && nrow(z_rep) > 0) {
    z_rep <- z_rep[, 1:input.stan$N]
    temp <- apply(z_rep, 2, median)
  } else {
    temp <- rep(NA, input.stan$N)  # Manejar si z_rep está vacío
  }
  
  df <- data.frame(
    hat = temp,
    obs = case_when(
      temp == 1 ~ "Transit",
      temp == 2 ~ "Calada",
      temp == 3 ~ "Recollida",
      TRUE ~ NA_character_
    )
  )
  
  df_completo <- cbind(df, data_middle)
  df_tracks <- rbind(df_tracks, df_completo)
  
  # Actualizar la barra de progreso
  setTxtProgressBar(pb, i)
  flush.console()
}

# Al finalizar, puedes ver qué tracks tienen menos de 3 datos
cat("Tracks con menos de 3 datos:\n")
print(tracks_menos_de_tres)

load("SSM_trasmallo.RData")

sf_output <- st_as_sf(df_tracks, coords = c("X", "Y"), crs =25831)
# mapview(sf_output, zcol = "journey", legend = FALSE)

# Asegúrate de que los puntos estén ordenados por track_id y tiempo
sf_output <- sf_output %>%
  arrange(journey, time)
#BLOQUEO POR SI ES REDUNDANTE
# # Crear líneas (trayectorias) agrupando por track_id
# sf_routes <- sf_output %>%
#   group_by(journey) %>%
#   summarise(do_union = FALSE) %>%  # Evitar combinar geometrías
#   st_cast("LINESTRING")           # Convertir puntos a líneas



# Definir colores para sf_output (columna "obs")
colores <- c(
  "Recollida" = "red",
  "Calada" = "green",
  "Transit" = "white"
  
)

sf_output$obs <- factor(sf_output$obs, levels = c("Recollida", "Calada", "Transit"))

length(unique(sf_output$journey))

#APLICAMOS EL ARREGLO DE MIQUEL
library(eRTG3D)
data = sf_output

# Suponiendo que tu dataframe se llama df
data$hat[data$hat == 2] <- 0 # calada
data$hat[data$hat == 2.5] <- 0 # calada
data$hat[data$hat == 1] <- 0 # transit
data$hat[data$hat == 1.5] <- 0 # transit
data$hat[data$hat == 3] <- 1 # recogida

# Usar la columna 'hat' como datos
y = data$hat
as.integer(y)
window = 5
y <- as.numeric(y)
# Aplicar la mediana móvil
y_smoothed = movingMedian(y, window)

#AQUI LOS PUNTOS SUELTOS PASAN A UN ESTADO U OTRO
# Añadir y_smoothed como nueva columna al dataframe
data$agrupat <- y_smoothed

df = data

# Crear grupos consecutivos
df <- df %>%
  mutate(group = cumsum(lag(agrupat, default = first(agrupat)) != agrupat)) %>%
  group_by(group) %>%
  mutate(group_size = n()) %>%
  ungroup()

df_resumen <- df %>%
  group_by(state) %>%
  summarise(
    mean_group_size = mean(group_size, na.rm = TRUE),
    sd_group_size = sd(group_size, na.rm = TRUE)
  )

# ELIMINAMOS TRACKS PEQUEÑOS
# AQUI AGRUPAMOS Y SI SON MAYORES DE 5 SON DEL MISMO GRUPO

# Crear la nueva columna 'hat_grouped'
df <- df %>%
  mutate(hat_grouped = case_when(
    agrupat == "1" & group_size > 5 ~ "1",
    TRUE ~ "0"
  ))

#aqui si es mayor de 6 lo considera tomo
df <- df %>%
  mutate(
    group_numbered = ifelse(hat_grouped == "1" & group_size > 6, group, NA),  # Lógica condicional
    group_numbered = dense_rank(group_numbered)  # Numeración por journey
  ) %>%
  ungroup()  # Desagrupar al final si ya no necesitas el agrupamiento

#3. De puntos a ruta, a través detime y agrupado por id polígono
sf_output <- df %>%
  arrange(group_numbered,time)

routes <- sf_output %>%
  group_by(journey, group_numbered) %>%
  summarise(geometry = st_combine(geometry)) %>%  # Une los puntos en una geometría única
  st_cast("LINESTRING")  # Convierte en una línea

#CALCULAR TIEMPO Y METROS
routes <- sf_output %>%
  arrange(group, time) %>%  # Ordena por id y tiempo
  group_by(journey, group_numbered) %>%
  summarise(
    journey = first(journey),  # Conserva el valor de 'journey' dentro del grupo
    start_time = min(time),  # Hora de inicio
    end_time = max(time),    # Hora de fin
    duration = as.numeric(difftime(max(time), min(time), units = "min")),  # Duración en minutos
    geometry = st_combine(geometry)  # Une los puntos en una geometría única
  ) %>%
  st_cast("LINESTRING")  # Convierte en una línea

#4. calcular longitud

# Asegurar que la capa está en CRS 25831
routes <- st_transform(routes, 25831)

# Calcular la longitud de cada línea en metros
routes$length_m <- st_length(routes)

# Mostrar las primeras filas con la longitud calculada
head(routes)
routes$length_m <- round(routes$length_m, 0)

routes <- routes %>%
  group_by(journey) %>%
  mutate(tomo = row_number()) %>%
  ungroup()

Tresmall <- routes %>% filter(group_numbered > 0)

Tresmall <- Tresmall %>% rename(longitud_m = length_m)
Tresmall <- Tresmall %>% rename(duracion_min = duration)
sf_output$agrupat <- as.character(sf_output$agrupat)

################################# CAMBIAR NOMBRE AQUIIIIIIIIIIIIIIIIIIIIIIIIII
#Cargamos p units
p_units= st_read(file.path(shp_dir, "P_units_REMAR.shp"))
# mapview(p_units)

# Asegúrate de que ambas capas están en el mismo sistema de coordenadas
st_crs(Tresmall) <- st_crs(p_units)

# Intersección espacial: cada trasmallo recibe el ID de la unidad de planificación
trasmallos_con_id <- st_intersection(Tresmall, p_units[, c("ID_GRID")], left = FALSE)
mapview(trasmallos_con_id, zcol = "ID_GRID") + mapview(p_units)

getwd()

# Calcular longitud de cada fragmento en metros
trasmallos_segmentados <- trasmallos_con_id %>%
  mutate(long_m_partes_tomo = st_length(geometry))

#AHORA CALCULAR EL PORCENTAJE DE CADA FRAGMENTO
trasmallos_segmentados <- trasmallos_segmentados %>%
  mutate(porcentaje = (long_m_partes_tomo / longitud_m) * 100)

mapview(trasmallos_segmentados)

st_write(trasmallos_segmentados, "tresmall_intersected_Llagosta_2024.shp", delete_layer = TRUE)
