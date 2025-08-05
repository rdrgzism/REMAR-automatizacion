library(dplyr)

rm(list = ls())

setwd("C:/Users/Usuario/Desktop/REMAR/visorfinal/data/llampuga")
dir.create("backup", showWarnings = FALSE)  # crear carpeta backup si no existe

columns <- c(
  "filename", "pes", "Number.of.Llampugues",
  "median_Furcal_length", "lowCI_Furcal_length", "uppCI_Furcal_length",
  "median_nfish", "lowCI_nfish", "uppCI_nfish", "day"
)

file_names <- list.files(pattern = "\\.csv$")
for (file_name in file_names) {
  data <- read.csv(file_name, header = TRUE)
  data <- data[ , columns]
  data <- data %>%
    mutate(
      pes = as.numeric(str_replace(pes, ",", ".")),
      longitud = as.numeric(median_Furcal_length),
      day = as.Date(day)
    ) %>%
    group_by(day) %>%
    summarise(
      mean_n_per_box = mean(number_of_llampugues, na.rm = TRUE),
      sd_n_per_box = sd(mean_n_per_box, na.rm = TRUE),
      n = sum(!is.na(pes)),
      se_n_per_box = sd_n_per_box / sqrt(n),
      lower_ci_n = mean_n_per_box - 1.96 * se_n_per_box,
      upper_ci_n = mean_n_per_box + 1.96 * se_n_per_box,

      .groups = "drop"
    )
  colnames(data) <- tolower(gsub("\\.", "_", colnames(data)))  # renombrar para SQL
  
  file.copy(file_name, file.path("backup", file_name), overwrite = TRUE)
  write.csv(data, file = file_name, row.names = FALSE)
}
