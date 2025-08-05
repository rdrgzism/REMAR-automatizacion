library(dplyr)
library(stringr)
library(ggplot2)

rm(list = ls())

setwd("C:/Users/Usuario/Desktop/REMAR/REMAR-automatizacion/scripts/r")
dir.create("backup", showWarnings = FALSE)  # crear carpeta backup si no existe
data_dir <- "C:/Users/Usuario/Desktop/REMAR/visorfinal/data/llampuga"


columns <- c(
  "pes", "Number.of.Llampugues","median_Furcal_length", 
  "lowCI_Furcal_length", "uppCI_Furcal_length",
  "median_nfish", "lowCI_nfish", "uppCI_nfish", "day"
)

file_names <- list.files(data_dir, pattern = "llampuga.*\\.csv$",
                         full.names = TRUE)
data_list <- lapply(file_names, function(file) {
  df <- read.csv(file, sep = ",", header = TRUE)
  return(df)
})
data <- do.call(rbind, data_list)
summary_data <- data %>%
  mutate(day = as.Date(day, "%Y-%m-%d")) %>%
  select(any_of(columns)) %>%
  rename_with(~ tolower(gsub("\\.", "_", .x))) %>%
  group_by(day) %>%
  summarise(
    pes_medio = mean(pes, na.rm = TRUE),
    lowerci_pes = pes_medio - 1.96 * 
      sd(pes, na.rm = TRUE) / sqrt(sum(!is.na(pes))),
    uppci_pes = pes_medio + 1.96 * 
      sd(pes, na.rm = TRUE) / sqrt(sum(!is.na(pes))),
    
    median_nfish = mean(median_nfish, na.rm = TRUE),
    uppci_nfish = mean(uppci_nfish, na.rm = TRUE),
    lowci_nfish = mean(lowci_nfish, na.rm = TRUE),
    
    median_furcal_length = mean(median_furcal_length, na.rm = TRUE),
    uppci_furcal_length = mean(uppci_furcal_length, na.rm = TRUE),
    lowci_furcal_length = mean(lowci_furcal_length, na.rm = TRUE)
  )
  
# file.copy(file_name, file.path("backup", file_name), overwrite = TRUE)
# write.csv(data, file = file_name, row.names = FALSE)

ggplot(summary_data, aes(x = day, y = pes_medio)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_ribbon(aes(ymin = lowerci_pes, ymax = uppci_pes), alpha = 0.2, fill = "steelblue") +
  labs(title = "Peso medio diario con intervalo de confianza",
       x = "Día", y = "Peso medio (kg)") +
  theme_minimal()

ggplot(summary_data, aes(x = day, y = median_furcal_length)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_ribbon(aes(ymin = lowci_furcal_length, ymax = uppci_furcal_length), alpha = 0.2, fill = "steelblue") +
  labs(title = "Longitud furcal media por día",
       x = "Día", y = "Longitud furcal media (cm)") +
  theme_minimal()

ggplot(summary_data, aes(x = day, y = median_nfish)) +
  geom_line(color = "orange", linewidth = 1) +
  geom_ribbon(aes(ymin = lowci_nfish, ymax = uppci_nfish), alpha = 0.2, fill = "steelblue") +
  labs(title = "Número medio de ejemplares por día",
       x = "Día", y = "Nº medio de peces") +
  theme_minimal()


