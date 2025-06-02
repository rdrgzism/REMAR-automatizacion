#!/bin/bash

# -----------------------
# Script: run_inferencia.sh
# Descripción: Ejecuta la pipeline completa de inferencia de gambas (YOLO + Área + Fusión Lonja)
# -----------------------

# Obtener timestamp
TIMESTAMP=$(date "+%Y-%m-%d_%H-%M-%S")
LOG_DIR="logs"
LOG_FILE="$LOG_DIR/inferencia_$TIMESTAMP.log"
mkdir -p "$LOG_DIR"

# Funciones de log
log_info() {
    echo "[INFO] $1" | tee -a "$LOG_FILE"
}
log_error() {
    echo "[ERROR] $1" | tee -a "$LOG_FILE"
}

# Función general para ejecutar scripts
run_step() {
    STEP_NAME="$1"
    CMD="$2"

    log_info "Starting: $STEP_NAME"
    if eval "$CMD" >> "$LOG_FILE" 2>&1; then
        log_info "Completed: $STEP_NAME"
    else
        log_error "Failed: $STEP_NAME"
        exit 1
    fi
}

log_info "INICIO PIPELINE DE INFERENCIA GAMBA"

# Paso 1: Inferencia YOLO + metadatos
run_step "1_inference_gamba_args.py"          "python scripts/python/1_inference_gamba_args.py"

# Paso 2: Calcular áreas a partir de las etiquetas
run_step "2_calcular_areas_segmentos_gamba.py"     "python scripts/python/2_calcular_areas_segmentos_gamba.py"

# Paso 3: Ajustar biomasa y estimar número de gambas
run_step "3_calculo_peso_medio_num_gamba.py" "python scripts/python/3_calculo_peso_medio_num_gamba.py"

# Paso 4: Fusionar con datos de lonja
run_step "4a_convierte_mdb_a_csv.py" "python scripts/python/4a_convierte_mdb_a_csv.py"

run_step "4b_combina_lonja_imagen.py" "python scripts/python/4b_combina_lonja_imagen.py"

log_info "INFERENCIA COMPLETADA CON ÉXITO"
