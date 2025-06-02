#!/usr/bin/env python3

import os
import warnings
import pyodbc
import pandas as pd

# ------------------------
# CONFIGURACIÓN
# ------------------------

# Directorio donde se encuentran los archivos .mdb
RAW_DATA_DIR = r"C:\Users\UIB\Desktop\REMAR-automatizacion\data\raw_data"

# Ruta al CSV que utilizará el script de merge (anteriormente “LONJA_FILE”)
LONJA_CSV = r"C:\Users\UIB\Desktop\REMAR-automatizacion\RESULTS\ARA.csv"

# ------------------------
# FUNCIONES AUXILIARES
# ------------------------


def get_mdb_files(directory):
    """
    Encuentra todos los archivos .mdb en el directorio (y subdirectorios).
    """
    mdb_files = []
    for root, _, files in os.walk(directory):
        for filename in files:
            if filename.lower().endswith(".mdb"):
                mdb_files.append(os.path.join(root, filename))
    return mdb_files


def process_and_combine_mdb(raw_dir, output_csv):
    """
    Lee todos los archivos .mdb encontrados en raw_dir, extrae la tabla DATOS
    y los concatena en un único DataFrame. Luego guarda ese DataFrame en output_csv.
    """
    mdb_files = get_mdb_files(raw_dir)
    if not mdb_files:
        print(f"[WARN] No se encontraron archivos .mdb en {raw_dir}.")
        return

    df_list = []
    for mdb in sorted(mdb_files):
        try:
            conn_str = r"DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=" + mdb
            conn = pyodbc.connect(conn_str, autocommit=True)
            query = "SELECT * FROM DATOS"
            df = pd.read_sql(query, conn)
            conn.close()

            if df.empty:
                print(f"[WARN] El archivo {os.path.basename(mdb)} no contiene registros en la tabla DATOS. Se omite.")
                continue

            if 'FECHA' in df.columns:
                df['FECHA'] = pd.to_datetime(df['FECHA'])

            df_list.append(df)

        except Exception as e:
            print(f"[ERROR] No se pudo procesar {os.path.basename(mdb)}: {e}")
            continue

    if not df_list:
        print("[WARN] Ningún archivo .mdb generó datos válidos.")
        return

    # Concatenar todos los DataFrames en uno solo
    df_combined = pd.concat(df_list, ignore_index=True)
    df_combined.to_csv(output_csv, index=False)
    print(f"[OK] CSV combinado guardado en: {output_csv}")


if __name__ == "__main__":
    # Suprimir warnings de pandas sobre DBAPI
    warnings.filterwarnings("ignore", message="pandas only supports SQLAlchemy connectable")

    # Procesar y combinar todos los .mdb en un único CSV
    process_and_combine_mdb(RAW_DATA_DIR, LONJA_CSV)
