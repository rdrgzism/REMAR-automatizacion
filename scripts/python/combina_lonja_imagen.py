#!/usr/bin/env python3

import pandas as pd
import csv
import yaml
import os

# --------------------
# LOAD SETTINGS
# --------------------
def load_settings(path='config/settings.yaml'):
    with open(path, 'r') as stream:
        return yaml.safe_load(stream)

settings = load_settings()
paths = settings["paths"]
YEAR = str(settings["inferencia"]["YEAR"])

# --------------------
# DEFINE PATHS
# --------------------
RESULTADOS_FILE = os.path.join(paths["OUTPUT_DIR"], f"resultados_procesados_{YEAR}.csv")
LONJA_FILE = os.path.join(paths["LONJA_DIR"], f"ARA_{YEAR}.csv")
MERGED_FILE = os.path.join(paths["MERGE_DIR"], f"DATOS_GAMBA_{YEAR}.csv")
NO_MATCH_FILE = os.path.join(paths["MERGE_DIR"], f"imagenes_no_en_merge_{YEAR}.csv")
DUPLICATES_FILE = os.path.join(paths["MERGE_DIR"], f"duplicados_{YEAR}.csv")
LONJA_FILTRADO_FILE = os.path.join(paths["MERGE_DIR"], f"archivo_lonja_filtrado_{YEAR}.csv")

# --------------------
# FUNCIONES AUXILIARES
# --------------------
def cargar_y_procesar_csv(archivo_lonja):
    df = pd.read_csv(archivo_lonja)
    print(f"[INFO] Registros originales en lonja: {len(df)}")

    df = df[(df['NUMENVAS'] != 0) & (df['NUMENVAS'] >= -1) & (df['NUMENVAS'] <= 1)]
    print(f"[INFO] Después de filtro NUMENVAS: {len(df)}")

    df['FECHA'] = pd.to_datetime(df['FECHA'], format='%m/%d/%y %H:%M:%S', errors='coerce')
    df['Fecha_sin_hora'] = df['FECHA'].dt.date

    indices_a_eliminar = set()
    for fecha, group in df.groupby('Fecha_sin_hora'):
        group = group.reset_index(drop=False)
        for i, row in group.iterrows():
            if row['NUMENVAS'] == -1:
                for j, other_row in group.iterrows():
                    if i != j and (
                        abs(row['PESONETO']) == abs(other_row['PESONETO']) and
                        abs(row['IMPORTE']) == abs(other_row['IMPORTE']) and
                        abs(row['NUMVENTA']) == abs(other_row['NUMVENTA'])
                    ):
                        indices_a_eliminar.update([row['index'], other_row['index']])

    df_final = df.drop(index=indices_a_eliminar)
    print(f"[INFO] Final tras eliminar pares venta-devolución: {len(df_final)}")
    df_final.to_csv(LONJA_FILTRADO_FILE, index=False)
    return df_final

# --------------------
# BLOQUE PRINCIPAL
# --------------------
def main():
    print(f"[INFO] Procesando datos de gamba para el año {YEAR}")

    # Cargar resultados de inferencia
    df_resultados = pd.read_csv(RESULTADOS_FILE)
    df_resultados['Fecha'] = df_resultados['Imagen'].str.extract(r'(\d{4}-\d{2}-\d{2})')[0]
    df_resultados['Fecha'] = pd.to_datetime(df_resultados['Fecha'], format='%Y-%m-%d', errors='coerce')

    # Cargar y limpiar datos de lonja
    df_lonja = cargar_y_procesar_csv(LONJA_FILE)
    df_lonja = df_lonja[df_lonja['Fecha_sin_hora'].notna()]
    df_lonja = df_lonja[df_lonja['NUMENVAS'] == 1]

    # Fusionar por fecha, venta y peso
    df_resultados = df_resultados[df_resultados['Fecha'].notna()]
    df_resultados['Fecha'] = df_resultados['Fecha'].dt.date

    df_merged = pd.merge(
        df_resultados,
        df_lonja,
        left_on=['Fecha', 'Vta', 'Pes'],
        right_on=['Fecha_sin_hora', 'NUMVENTA', 'PESONETO'],
        how='inner'
    )

    df_merged.drop(columns=['Fecha'], inplace=True)
    df_merged.rename(columns={'FECHA': 'FECHA_LONJA'}, inplace=True)
    df_merged = df_merged.drop_duplicates()

    if not os.path.exists(paths["MERGE_DIR"]):
        os.makedirs(paths["MERGE_DIR"])

    df_merged.to_csv(MERGED_FILE, index=False, quoting=csv.QUOTE_ALL)
    print(f"✅ Archivo '{MERGED_FILE}' generado correctamente.")

    # --------------------
    # CELDA 2 - DETECTAR DUPLICADOS
    # --------------------
    duplicados = df_merged[df_merged.duplicated(subset=['Imagen'], keep=False)]
    if not duplicados.empty:
        duplicados['Posicion'] = duplicados.index + 1
        duplicados.to_csv(DUPLICATES_FILE, index=False, quoting=csv.QUOTE_ALL)
        print(f"⚠️  Se encontraron duplicados. Guardados en '{DUPLICATES_FILE}'")
    else:
        print("✅ No se encontraron duplicados en la columna 'Imagen'.")

    # --------------------
    # CELDA 3 - IDENTIFICAR NO-MATCHES
    # --------------------
    imagenes_en_merged = set(df_merged['Imagen'])
    imagenes_totales = set(df_resultados['Imagen'])
    no_en_merge = imagenes_totales - imagenes_en_merged

    df_no_match = df_resultados[df_resultados['Imagen'].isin(no_en_merge)]
    df_no_match.to_csv(NO_MATCH_FILE, index=False)
    print(f"📄 Imágenes no enlazadas guardadas en '{NO_MATCH_FILE}'")

# --------------------
if __name__ == "__main__":
    main()
