#!/usr/bin/env python3

import os
import csv
import pandas as pd
import datetime

# ------------------------
# CONFIGURACIÓN
# ------------------------
YEAR = datetime.date.today().year
BASE_DIR = "/home/llotja/procesos_cron/GAMBA_BARRACUDA"
RESULTS_DIR = os.path.join(BASE_DIR, "RESULTADOS")
LONJA_FILE = os.path.join(BASE_DIR, f"ARA_{YEAR}.csv")

archivo_resultados = os.path.join(RESULTS_DIR, f"resultados_procesados_{YEAR}.csv")
archivo_salida = os.path.join(RESULTS_DIR, f"DATOS_GAMBA_{YEAR}.csv")
archivo_duplicados = os.path.join(RESULTS_DIR, "duplicados.csv")
archivo_no_merge = os.path.join(RESULTS_DIR, "imagenes_no_en_merge.csv")
archivo_filtrado_lonja = os.path.join(RESULTS_DIR, f"archivo_lonja_filtrado_{YEAR}.csv")


# ------------------------
# FUNCIONES
# ------------------------
def cargar_y_procesar_csv(path):
    df = pd.read_csv(path)
    print(f"[INFO] Registros originales: {len(df)}")

    df_filtrado = df[(df['NUMENVAS'] != 0) & (df['NUMENVAS'] >= -1) & (df['NUMENVAS'] <= 1)]
    print(f"[INFO] Tras filtrar NUMENVAS inválidos: {len(df_filtrado)}")

    df_filtrado['FECHA'] = pd.to_datetime(df_filtrado['FECHA'], format='%m/%d/%y %H:%M:%S', errors='coerce')
    df_filtrado['Fecha_sin_hora'] = df_filtrado['FECHA'].dt.date

    indices_a_eliminar = set()
    for fecha, grupo in df_filtrado.groupby('Fecha_sin_hora'):
        grupo.reset_index(drop=False, inplace=True)
        for i, fila in grupo.iterrows():
            if fila['NUMENVAS'] == -1:
                for j, otra in grupo.iterrows():
                    if i != j and (
                        abs(fila['PESONETO']) == abs(otra['PESONETO']) and
                        abs(fila['IMPORTE']) == abs(otra['IMPORTE']) and
                        abs(fila['NUMVENTA']) == abs(otra['NUMVENTA'])
                    ):
                        indices_a_eliminar.update([fila['index'], otra['index']])
    df_final = df_filtrado.drop(index=indices_a_eliminar)
    print(f"[INFO] Tras eliminar duplicados sospechosos: {len(df_final)}")
    df_final.to_csv(archivo_filtrado_lonja, index=False)
    return df_final


# ------------------------
# CARGA Y MERGE
# ------------------------
df_resultados = pd.read_csv(archivo_resultados)
df_resultados['Fecha'] = pd.to_datetime(df_resultados['Imagen'].str.extract(r'(\d{4}-\d{2}-\d{2})')[0], errors='coerce')
df_resultados = df_resultados[df_resultados['Fecha'].notna()]
df_resultados['Fecha'] = df_resultados['Fecha'].dt.date

df_lonja = cargar_y_procesar_csv(LONJA_FILE)
df_lonja = df_lonja[df_lonja['Fecha_sin_hora'].notna()]
df_lonja = df_lonja[df_lonja['NUMENVAS'] == 1]

df_merged = pd.merge(
    df_resultados,
    df_lonja,
    left_on=['Fecha', 'Vta', 'Pes'],
    right_on=['Fecha_sin_hora', 'NUMVENTA', 'PESONETO'],
    how='inner'
).drop(columns=['Fecha'])

df_merged.rename(columns={'FECHA': 'FECHA_LONJA'}, inplace=True)
df_merged = df_merged.drop_duplicates()

df_merged.to_csv(archivo_salida, index=False, quoting=csv.QUOTE_ALL)
print(f"[OK] Archivo generado: {archivo_salida}")

# ------------------------
# DUPLICADOS
# ------------------------
duplicados = df_merged[df_merged.duplicated(subset=['Imagen'], keep=False)]
if not duplicados.empty:
    print(f"[WARN] Duplicados encontrados: {len(duplicados)}")
    duplicados['Posicion'] = duplicados.index + 1
    duplicados.to_csv(archivo_duplicados, index=False, quoting=csv.QUOTE_ALL)
else:
    print("[OK] No se encontraron duplicados en la columna 'Imagen'.")


# ------------------------
# NO MERGEADAS
# ------------------------
imagenes_resultados = set(df_resultados['Imagen'])
imagenes_mergeadas = set(df_merged['Imagen'])
imagenes_sin_merge = imagenes_resultados - imagenes_mergeadas

if imagenes_sin_merge:
    df_no_merge = df_resultados[df_resultados['Imagen'].isin(imagenes_sin_merge)]
    df_no_merge.to_csv(archivo_no_merge, index=False)
    print(f"[INFO] Se guardaron {len(df_no_merge)} imágenes no mergeadas en: {archivo_no_merge}")
else:
    print("[OK] Todas las imágenes fueron mergeadas correctamente.")
