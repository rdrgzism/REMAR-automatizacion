setwd("C:/Users/Usuario/Desktop/REMAR/visorfinal/data")
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
  data$day <- as.Date(data$day)
  
  colnames(data) <- tolower(gsub("\\.", "_", colnames(data)))  # renombrar para SQL
  
  file.copy(file_name, file.path("backup", file_name), overwrite = TRUE)
  write.csv(data, file = file_name, row.names = FALSE)
}
