library(tidyverse)
library(lubridate)

# Funcion para leer un archivo  y extraer la fecha del nombre
leer_merluza_csv <- function(file_path){
  # Extraer fecha del numbre
  filename <- basename(file_path)
  date_str <- str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
  day <- ymd(date_str)
  
  # Leer datos y agregar la columna day
  read_csv(file_path) %>%
    mutate(day = day, filename = filename)
}

# Directorio de origen 
path <- "C:/Users/Usuario/OneDrive - Universitat de les Illes Balears/estimacionTamañoDB/datos_L_merluza_2021-2024"
files <- list.files(path, pattern = "*.csv", full.names = TRUE)

# Leer y unir todos
merluza_data <- map_dfr(files, leer_merluza_csv)

columns <- c(
  "filename", "pes", "Number of fish", "longitud", "day"
)

merluza_data <- merluza_data[ , columns]
merluza_data <- merluza_data %>%
  mutate(
    pes = as.numeric(str_replace(pes, ",", ".")),
    longitud = as.numeric(str_replace(longitud, ",", ".")),
    day = as.Date(day)
  )

colnames(merluza_data) <- tolower(gsub(" ", "_", colnames(merluza_data)))
write.csv(merluza_data, file = "merluza_data.csv", row.names = FALSE)
