#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Estimating fishing effort for small-scale fisheries from vessel monitoring system
Script 1/6 - Reading .mdb auction data and converting to CSV
"""

import os
import re
import time
import warnings
import logging
import traceback
from datetime import datetime, timedelta

import pyodbc
import pandas as pd


# -----------------------------
# Logging
# -----------------------------
def setup_logging(log_path="../../logs/mdb_processing.log"):
    log_path = os.path.abspath(log_path)
    os.makedirs(os.path.dirname(log_path), exist_ok=True)

    # Logger raíz
    logging.getLogger().handlers.clear()
    logging.getLogger().setLevel(logging.INFO)

    fmt = logging.Formatter("%(asctime)s - %(levelname)s - %(message)s")

    # Archivo
    fh = logging.FileHandler(log_path, encoding="utf-8")
    fh.setFormatter(fmt)
    logging.getLogger().addHandler(fh)

    # Consola
    ch = logging.StreamHandler()
    ch.setFormatter(fmt)
    logging.getLogger().addHandler(ch)

    logging.info("==== MDB processing started ====")
    logging.info(f"Log file at: {log_path}")


# -----------------------------
# Utilidades MDB
# -----------------------------
def get_mdb_files(directory):
    mdb_files = []
    for root, _, files in os.walk(directory):
        for filename in files:
            if filename.lower().endswith(".mdb"):
                mdb_files.append(os.path.join(root, filename))
    return mdb_files


def filter_files_by_date(mdb_files, days_back=30):
    filtered_files = []
    today = datetime.today()
    cutoff_date = today - timedelta(days=days_back)

    for file in mdb_files:
        filename = os.path.basename(file)
        logging.info(f"Checking file: {filename}")
        try:
            match = re.search(r"\d{2}-\d{2}-\d{4}", filename)
            if match:
                date_str = match.group(0)
                file_date = datetime.strptime(date_str, "%d-%m-%Y")
                logging.info(f"Found file for date: {file_date:%Y-%m-%d}")
                if file_date >= cutoff_date:
                    filtered_files.append(file)
                    logging.info("Accepted")
                else:
                    logging.info("Rejected (older than cutoff)")
            else:
                logging.warning(f"No date pattern in filename: {filename}")
        except Exception as e:
            logging.warning(f"Cannot parse date from file {filename}: {e}")
    return filtered_files


def check_access_driver(required="Microsoft Access Driver (*.mdb, *.accdb)"):
    drivers = [d.strip() for d in pyodbc.drivers()]
    logging.info(f"Available ODBC drivers: {drivers}")
    if required not in drivers:
        logging.error(
            f"ODBC driver not found: {required}. "
            "Instala el ‘Microsoft Access Database Engine’ que coincida en arquitectura con tu Python."
        )
        return False
    return True


def connect_with_retry(conn_str, retries=3, wait_s=2):
    last_exc = None
    for i in range(1, retries + 1):
        try:
            conn = pyodbc.connect(conn_str)
            return conn
        except pyodbc.Error as e:
            last_exc = e
            logging.warning(f"ODBC connect attempt {i}/{retries} failed: {e}")
            time.sleep(wait_s)
        except Exception as e:
            last_exc = e
            logging.warning(f"Unexpected connect error (attempt {i}/{retries}): {e}")
            time.sleep(wait_s)
    if last_exc:
        logging.error(f"Giving up connecting after {retries} attempts: {last_exc}")
        logging.debug("Traceback:\n" + traceback.format_exc())
    return None


def find_table_name(conn, preferred="DATOS"):
    """
    Busca 'DATOS' o una variante (mayúsculas/minúsculas, posibles comillas)
    """
    try:
        cursor = conn.cursor()
        # cat=None, schem=None, table=None, tableType='TABLE'
        tables = list(cursor.tables(tableType="TABLE"))
        cursor.close()
        names = [t.table_name for t in tables]
        logging.info(f"Tables found in MDB: {names}")

        # Coincidencia exacta insensible a mayúsculas
        for name in names:
            if name.strip().lower() == preferred.lower():
                return name

        # Si no está, devolver la primera por si acaso (y loggear)
        if names:
            logging.warning(f"'{preferred}' not found. Using first table: {names[0]}")
            return names[0]

        logging.error("No tables found in MDB.")
        return None
    except Exception:
        logging.error("Error listing tables from MDB.")
        logging.debug("Traceback:\n" + traceback.format_exc())
        return None


def process_mdb_files(mdb_files, output_dir):
    output_dir = os.path.abspath(output_dir)
    os.makedirs(output_dir, exist_ok=True)

    if not check_access_driver():
        logging.error("Required ODBC driver missing. Aborting processing.")
        return

    for file in mdb_files:
        abs_file = os.path.abspath(file)
        if not os.path.isfile(abs_file):
            logging.error(f"File does not exist: {abs_file}")
            continue

        logging.info(f"Processing file: {abs_file}")

        conn_str = (
            r"DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};"
            f"DBQ={abs_file}"
        )

        conn = connect_with_retry(conn_str, retries=3, wait_s=2)
        if conn is None:
            # Ya se loggeó el error en connect_with_retry
            continue

        try:
            table_name = find_table_name(conn, preferred="DATOS")
            if not table_name:
                conn.close()
                continue

            query = f"SELECT * FROM [{table_name}]"
            logging.info(f"Running query: {query}")

            df = pd.read_sql_query(query, conn)
            logging.info(f"Read {len(df)} rows from table '{table_name}'")
        except Exception as e:
            logging.error(f"Error reading table from {abs_file}: {e}")
            logging.debug("Traceback:\n" + traceback.format_exc())
            try:
                conn.close()
            except Exception:
                pass
            continue
        finally:
            try:
                conn.close()
            except Exception:
                pass

        if df.empty:
            logging.warning(f"Empty table in {abs_file}. Skipping CSV export.")
            continue

        # Normalización de FECHA (si existe)
        if "FECHA" in df.columns:
            try:
                df["FECHA"] = pd.to_datetime(df["FECHA"], errors="coerce")
            except Exception as e:
                logging.warning(f"Error parsing FECHA in {abs_file}: {e}")

        # Construcción del nombre de salida a partir de la fecha en el nombre del archivo
        basename = os.path.splitext(os.path.basename(abs_file))[0]
        match = re.search(r"(\d{2}-\d{2}-\d{4})", basename)
        if match:
            date_str = match.group(0)
            try:
                date_obj = datetime.strptime(date_str, "%d-%m-%Y")
                date_fmt = date_obj.strftime("%Y%m%d")
                csv_filename = f"IMEDEA_{date_fmt}_{date_fmt}.csv"
            except Exception as e:
                logging.warning(f"Bad date in filename '{basename}': {e}")
                csv_filename = basename + ".csv"
        else:
            logging.warning(f"No date in filename. Using raw basename: {basename}")
            csv_filename = basename + ".csv"

        output_csv = os.path.join(output_dir, csv_filename)
        try:
            df.to_csv(output_csv, index=False)
            logging.info(f"Created CSV: {output_csv}")
        except Exception as e:
            logging.error(f"Error writing CSV for {abs_file}: {e}")
            logging.debug("Traceback:\n" + traceback.format_exc())


# -----------------------------
# Main
# -----------------------------
if __name__ == "__main__":
    warnings.filterwarnings("ignore", message="pandas only supports SQLAlchemy connectable")

    setup_logging("../../logs/mdb_processing.log")

    input_dir_path = os.path.abspath("../../data/raw_data")
    output_dir_path = os.path.abspath("../../data/ventaslonja")

    logging.info(f"Input dir: {input_dir_path}")
    logging.info(f"Output dir: {output_dir_path}")

    all_mdb_files = get_mdb_files(input_dir_path)
    logging.info(f"Found {len(all_mdb_files)} .mdb files total.")

    mdb_files_to_process = filter_files_by_date(all_mdb_files, days_back=600)
    logging.info(f"{len(mdb_files_to_process)} files selected by date filter.")

    if mdb_files_to_process:
        process_mdb_files(mdb_files_to_process, output_dir_path)
        logging.info("All MDB files processed (selected set).")
    else:
        logging.warning("No MDB files to process after filtering.")

    logging.info("==== MDB processing finished ====")
