#!/usr/bin/env python3

# ---------------------------------------
# Estimating fishing effort for small-scale fisheries from vessel monitoring system
#   1/5
# ---------------------------------------

import os
import warnings
import pyodbc
import pandas as pd
import yaml
import logging
from datetime import datetime, timedelta


# -------------------------------
# Load settings from YAML
# -------------------------------
def load_settings(path="../../config/settings.yaml"):
    with open(path, "r") as file:
        return yaml.safe_load(file)


# -------------------------------
# Set up logging
# -------------------------------
def setup_logging(log_path="../../logs/mdb_processing.log"):
    if not os.path.exists(os.path.dirname(log_path)):
        os.makedirs(os.path.dirname(log_path))
    logging.basicConfig(
        filename=log_path,
        level=logging.INFO,
        format="%(asctime)s - %(levelname)s - %(message)s"
    )


# -------------------------------
# Find MDB files
# -------------------------------
def get_mdb_files(directory):
    mdb_files = []
    for root, _, files in os.walk(directory):
        for filename in files:
            if filename.lower().endswith(".mdb"):
                mdb_files.append(os.path.join(root, filename))
    return mdb_files


# -------------------------------
# Filter MDB files by date
# -------------------------------
def filter_files_by_date(mdb_files, days_back=30):
    filtered_files = []
    today = datetime.today()
    cutoff_date = today - timedelta(days=days_back)

    for file in mdb_files:
        filename = os.path.basename(file)
        try:
            date_str = filename.split("_")[1]
            file_date = datetime.strptime(date_str, "%d-%m-%Y")
            if file_date >= cutoff_date:
                filtered_files.append(file)
        except Exception as e:
            logging.warning(f"Cannot parse date from file {filename}: {e}")
    return filtered_files


# -------------------------------
# Process MDB files and save as CSV
# -------------------------------
def process_mdb_files(mdb_files, output_dir):
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    for file in mdb_files:
        try:
            conn_str = r"DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=" + file
            conn = pyodbc.connect(conn_str)
            query = "SELECT * FROM DATOS"
            df = pd.read_sql(query, conn)
            conn.close()

            if df.empty:
                logging.warning(f"The file {file} is empty. Skipping.")
                continue

            if 'FECHA' in df.columns:
                try:
                    df['FECHA'] = pd.to_datetime(df['FECHA'])
                except Exception as e:
                    logging.error(f"Error parsing FECHA in file {file}: {e}")

            csv_filename = os.path.splitext(os.path.basename(file))[0] + ".csv"
            output_csv = os.path.join(output_dir, csv_filename)
            df.to_csv(output_csv, index=False)
            logging.info(f"Created CSV: {csv_filename}")

        except Exception as e:
            logging.error(f"Error processing file {file}: {e}")


# -------------------------------
# MAIN EXECUTION
# -------------------------------
if __name__ == "__main__":
    warnings.filterwarnings("ignore", message="pandas only supports SQLAlchemy connectable")

    settings = load_settings()
    setup_logging()

    input_dir_path = settings['paths']['raw_data']
    output_dir_path = settings['paths']['csv_data']

    all_mdb_files = get_mdb_files(input_dir_path)
    mdb_files_to_process = filter_files_by_date(all_mdb_files, days_back=90)

    logging.info(f"Found {len(all_mdb_files)} MDB files to process.")

    if mdb_files_to_process:
        process_mdb_files(mdb_files_to_process, output_dir_path)
        logging.info("All MDB files processed successfully!")
    else:
        logging.warning("No MDB files to process.")
