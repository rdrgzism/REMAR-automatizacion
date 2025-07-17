#!/usr/bin/env Rscript

# ---------------------------------------
# Script de configuración del entorno remar_autom
# Autor: Ismael Rodríguez (ajustado)
# Fecha: 2025-07-03
# ---------------------------------------

cat("Iniciando verificación del entorno R...\n")

# Lista de paquetes requeridos
required_packages <- c(
  "stringr", "dplyr", "mldr", "RWeka",
  "data.table", "sf", "lubridate", "stats",
  "posterior", "moveHMM", "cmdstanr"
)

# Configurar repositorios por defecto
options(repos = c(
  CRAN = "https://cloud.r-project.org",
  cmdstanr = "https://mc-stan.org/r-packages/"
))

# Verificar e instalar paquetes necesarios
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  cat("Instalando paquetes faltantes:\n", paste(missing, collapse = ", "), "\n")
  install.packages(missing)
} else {
  cat("Todos los paquetes R requeridos ya están instalados.\n")
}

# Instalar eRTG3D desde GitHub si no está disponible
if (!requireNamespace("eRTG3D", quietly = TRUE)) {
  cat("Instalando 'eRTG3D' desde GitHub...\n")
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("munterfi/eRTG3D")
}

# Cargar cmdstanr y verificar toolchain
suppressPackageStartupMessages(library(cmdstanr))
cat("Verificando 'cmdstanr' y el toolchain de CmdStan...\n")
library(cmdstanr)
cmdstanr::check_cmdstan_toolchain(fix = TRUE)

# Instalar CmdStan si no está presente
if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
  cat("CmdStan no está instalado. Intentando instalar...\n")
  tryCatch({
    cmdstanr::install_cmdstan()
  }, error = function(e) {
    cat("Error durante la instalación de CmdStan:\n", conditionMessage(e), "\n")
    cat("Reintentando con TBB_CXX_TYPE=gcc...\n")
    
    make_local_path <- file.path(cmdstanr::cmdstan_path(), "make", "local")
    dir.create(dirname(make_local_path), showWarnings = FALSE, recursive = TRUE)
    writeLines("TBB_CXX_TYPE=gcc", con = make_local_path)
    
    cmdstanr::install_cmdstan(force = TRUE)
  })
} else {
  cat("CmdStan ya está instalado: ", cmdstanr::cmdstan_version(), "\n")
}

cat("Entorno 'remar_autom' configurado correctamente.\n")
