#!/bin/bash

# ------------------------------------------------------------
# run_job.sh - Master script to run REMAR pipeline (robust + silent)
# ------------------------------------------------------------

set -e  # Abort on error

TIMESTAMP=$(date "+%Y-%m-%d_%H-%M-%S")
LOG_DIR="$HOME/REMAR-automatizacion/logs"
LOG_FILE="${LOG_DIR}/pipeline_${TIMESTAMP}.log"
mkdir -p "$LOG_DIR"

echo "Ejecutando una mierda"

source ~/miniconda3/etc/profile.d/conda.sh
conda activate remar_autom

# Logging helpers
log_info() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] [INFO] $1" >> "$LOG_FILE"
}
log_error() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] [ERROR] $1" >> "$LOG_FILE"
}

# Run a script if it exists
run_step() {
    STEP_NAME="$1"
    CMD="$2"
    FILE_PATH="$3"

    if [ ! -f "$FILE_PATH" ]; then
        log_error "Missing file for step '$STEP_NAME': $FILE_PATH"
        exit 1
    fi

    log_info "Starting step: $STEP_NAME"
    if eval "$CMD" >> "$LOG_FILE" 2>&1; then
        log_info "Completed: $STEP_NAME"
    else
        log_error "Failed: $STEP_NAME"
        exit 1
    fi
}

log_info "REMAR pipeline started."

# Define steps
run_step "01 - Data cleaning Mallorca" \
         "Rscript scripts/r/01_data_cleaning_mallorca.R" \
         "scripts/r/01_data_cleaning_mallorca.R"

run_step "02 - Metier classification" \
         "Rscript scripts/r/02_module_metier_classification.R" \
         "scripts/r/02_module_metier_classification.R"

run_step "03 - Link with GPS tracks" \
         "Rscript scripts/r/03_link_predicted_cajasverdes.R" \
         "scripts/r/03_link_predicted_cajasverdes.R"

run_step "04 - Prepare SSM input" \
         "Rscript scripts/r/04_SSM_input.R" \
         "scripts/r/04_SSM_input.R"

run_step "05 - SSM Inference: jonquillera" \
         "Rscript scripts/r/05_SSM_jonquillera.R" \
         "scripts/r/05_SSM_jonquillera.R"

log_info "REMAR pipeline completed successfully."

