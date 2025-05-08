#!/bin/bash

# --------------------------------------------
# run_job.sh - Master script to run REMAR pipeline
# --------------------------------------------

# Exit if any command fails
set -e

# Define log file
TIMESTAMP=$(date "+%Y-%m-%d_%H-%M-%S")
LOG_DIR="logs"
LOG_FILE="${LOG_DIR}/pipeline_${TIMESTAMP}.log"

# Ensure log directory exists
mkdir -p "$LOG_DIR"

# Activate Conda
source ~/miniconda3/etc/profile.d/conda.sh
conda activate remarpipeline

log_info() {
	echo "[$(date +"%Y-%m-%d %H:%M:%S")] [INFO] $1" | tee -a "$LOG_FILE"
}

log_error() {                                                                                                                                                                                                                        
        echo "[$(date +"%Y-%m-%d %H:%M:%S")] [ERROR] $1" | tee -a "$LOG_FILE"                                                                                                                                                        
} 

run_step() {
	STEP_NAME="$1"
	CMD="$2"

	log_info "Starting step: $STEP_NAME"
	if eval "$CMD" >> "$LOG_FILE" 2>&1; then
		log_info "Completed: $STEP_NAME"
	else
		log_error "Failed: $STEP_NAME"
		exit 1
	fi
}

log_info "REMAR pipeline started."

# Run steps
run_step "00_read_mdb_files.py"                 "python3 scripts/python/00_read_mdb_files.py"
run_step "01_data_cleaning_mallorca.R"          "Rscript scripts/r/01_data_cleaning_mallorca.R"
run_step "02_module_metiere_classification.R"   "Rscript scripts/r/02_module_metiere_classification.R"
run_step "03_link_predicted_cajasverdes.R"      "Rscript scripts/r/03_link_predicted_cajasverdes.R"
run_step "04_SSM_input.R"                       "Rscript scripts/r/04_SSM_input.R"
# run_step "05_SSM_Trasmallo.R"                   "Rscript scripts/r/05_SSM_Trasmallo.R"
