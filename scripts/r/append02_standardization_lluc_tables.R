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
path <- "C:/Users/Usuario/Desktop/REMAR/visorfinal/data/merluza"
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
  ) %>%
  group_by(day) %>%
  summarise(
    mean_pes = mean(pes, na.rm = TRUE),
    sd_pes = sd(pes, na.rm = TRUE),
    n_pes = sum(!is.na(pes)),
    se_pes = sd_pes / sqrt(n_pes),
    lower_ci_pes = mean_pes - 1.96 * se_pes,
    upper_ci_pes = mean_pes + 1.96 * se_pes,
    
    mean_lon = mean(longitud, na.rm = TRUE),
    sd_lon = sd(longitud, na.rm = TRUE),
    n_lon = sum(!is.na(longitud)),
    se_lon = sd_lon / sqrt(n_lon),
    lower_ci_lon = mean_lon - 1.96 * se_lon,
    upper_ci_lon = mean_lon + 1.96 * se_lon,
    
    .groups = "drop"
  )

colnames(merluza_data) <- tolower(gsub(" ", "_", colnames(merluza_data)))
write.csv(merluza_data, file = "merluza_data.csv", row.names = FALSE)
