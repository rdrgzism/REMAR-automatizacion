#!/usr/bin/env python3

import os
import csv
import pandas as pd
import datetime

# ------------------------
# CONFIGURACIÓN
# ------------------------

# Carpeta principal donde tienes los datos
BASE_DIR = r"C:\Users\UIB\Desktop\REMAR-automatizacion"
RESULTS_DIR = os.path.join(BASE_DIR, "RESULTS")          # contiene archivos resultados_procesados_YYYY-MM-DD.csv
LONJA_FILE = os.path.join(RESULTS_DIR, "ARA.csv")              # fichero anual con todas las fechas
FILTRADO_LONJA = os.path.join(RESULTS_DIR, "archivo_lonja_filtrado.csv")
DUPLICADOS_DIR = os.path.join(RESULTS_DIR, "duplicados")
NO_MERGE_DIR = os.path.join(RESULTS_DIR, "no_merge")
SALIDA_DIR = os.path.join(RESULTS_DIR, "DATOS_GAMBA")

# Asegurarse de que existan los directorios de salida
os.makedirs(RESULTS_DIR, exist_ok=True)
os.makedirs(DUPLICADOS_DIR, exist_ok=True)
os.makedirs(NO_MERGE_DIR, exist_ok=True)
os.makedirs(SALIDA_DIR, exist_ok=True)


# ------------------------
# FUNCIONES AUXILIARES
# ------------------------
def cargar_y_procesar_lonja(path):
    """
    Carga el CSV de lonja (ARA.csv), filtra valores de NUMENVAS entre -1 y 1 (excluyendo 0),
    convierte FECHA a datetime, genera columna Fecha_sin_hora, y elimina duplicados
    con NUMENVAS = -1.
    Devuelve el DataFrame filtrado y un CSV intermedio en FILTRADO_LONJA.
    """
    df = pd.read_csv(path)
    print(f"[INFO] Lonja: registros originales: {len(df)}")

    # Filtrar NUMENVAS fuera de {-1, 0, 1} (se quedan los que son -1 o 1)
    df_filtrado = df[(df['NUMENVAS'] != 0) & (df['NUMENVAS'].abs() == 1)].copy()
    print(f"[INFO] Lonja: tras filtrar NUMENVAS inválidos: {len(df_filtrado)}")

    # Convertir FECHA a datetime y extraer fecha sin hora
    df_filtrado['FECHA'] = pd.to_datetime(df_filtrado['FECHA'], format='%Y-%m-%d %H:%M:%S')
    df_filtrado['Fecha_sin_hora'] = df_filtrado['FECHA'].dt.date

    # Eliminar pares duplicados con NUMENVAS == -1 (manteniendo sólo las filas “positivas”)
    indices_a_eliminar = set()
    for fecha, grupo in df_filtrado.groupby('Fecha_sin_hora'):
        grupo = grupo.reset_index(drop=False)  # preservar índice original en columna “index”
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
    print(f"[INFO] Lonja: tras eliminar duplicados sospechosos: {len(df_final)}")

    df_final.to_csv(FILTRADO_LONJA, index=False)
    print(f"[INFO] Lonja filtrada guardada en: {FILTRADO_LONJA}")
    return df_final


# ------------------------
# CARGA Y PREPARACIÓN DE LONJA UNICA
# ------------------------
df_lonja_full = cargar_y_procesar_lonja(LONJA_FILE)

# ------------------------
# PROCESAMIENTO POR DÍA
# ------------------------
# Buscamos todos los archivos resultados_procesados_YYYY-MM-DD.csv en RESULTS_DIR
for fname in sorted(os.listdir(RESULTS_DIR)):
    if not (fname.startswith("resultados_procesados_") and fname.endswith(".csv")):
        continue

    # Extraer 'YYYY-MM-DD' de 'resultados_procesados_YYYY-MM-DD.csv'
    fecha_str = fname[len("resultados_procesados_") : -len(".csv")]
    try:
        date_obj = datetime.datetime.strptime(fecha_str, "%Y-%m-%d").date()
    except ValueError:
        print(f"[WARN] Ignorando '{fname}': '{fecha_str}' no es fecha válida")
        continue

    print(f"\n[INFO] Procesando fecha: {fecha_str}")

    archivo_resultados = os.path.join(RESULTS_DIR, fname)
    df_resultados = pd.read_csv(archivo_resultados)

    # Extraer fecha de la columna Imagen y filtrar filas válidas
    df_resultados['Fecha'] = pd.to_datetime(
        df_resultados['Imagen'].str.extract(r'(\d{4}-\d{2}-\d{2})')[0], errors='coerce'
    ).dt.date
    df_resultados = df_resultados[df_resultados['Fecha'].notna()]
    df_resultados = df_resultados[df_resultados['Fecha'] == date_obj]

    if df_resultados.empty:
        print(f"[WARN] No hay resultados para {fecha_str} en {fname}")
        continue

    # Filtrar lonja para esa fecha y sólo NUMENVAS == 1
    df_lonja = df_lonja_full[df_lonja_full['Fecha_sin_hora'] == date_obj].copy()
    df_lonja = df_lonja[df_lonja['NUMENVAS'] == 1]

    if df_lonja.empty:
        print(f"[WARN] No hay registros de lonja con NUMENVAS=1 para {fecha_str}")
        continue

    # ------------------------
    # MERGE RESULTADOS <-> LONJA
    # ------------------------
    df_merged = pd.merge(
        df_resultados,
        df_lonja,
        left_on=['Fecha', 'Vta', 'Pes'],
        right_on=['Fecha_sin_hora', 'NUMVENTA', 'PESONETO'],
        how='inner'
    ).drop(columns=['Fecha'])

    if df_merged.empty:
        print(f"[WARN] Merge vacío para {fecha_str}")
        continue

    df_merged.rename(columns={'FECHA': 'FECHA_LONJA'}, inplace=True)
    df_merged = df_merged.drop_duplicates()

    # Guardar CSV diario
    salida_diaria = os.path.join(SALIDA_DIR, f"DATOS_GAMBA_{fecha_str}.csv")
    df_merged.to_csv(salida_diaria, index=False, quoting=csv.QUOTE_ALL)
    print(f"[OK] Archivo generado: {salida_diaria}")

    # ------------------------
    # DUPLICADOS
    # ------------------------
    dup = df_merged[df_merged.duplicated(subset=['Imagen'], keep=False)]
    if not dup.empty:
        dup['Posicion'] = dup.index + 1
        archivo_dup = os.path.join(DUPLICADOS_DIR, f"duplicados_{fecha_str}.csv")
        dup.to_csv(archivo_dup, index=False, quoting=csv.QUOTE_ALL)
        print(f"[WARN] Duplicados para {fecha_str} guardados en: {archivo_dup}")
    else:
        print(f"[OK] No se encontraron duplicados en 'Imagen' para {fecha_str}")

    # ------------------------
    # NO MERGEADAS
    # ------------------------
    imgs_resultados = set(df_resultados['Imagen'])
    imgs_mergeadas = set(df_merged['Imagen'])
    imgs_sin_merge = imgs_resultados - imgs_mergeadas

    if imgs_sin_merge:
        df_no_merge = df_resultados[df_resultados['Imagen'].isin(imgs_sin_merge)]
        archivo_no_merge = os.path.join(NO_MERGE_DIR, f"no_merge_{fecha_str}.csv")
        df_no_merge.to_csv(archivo_no_merge, index=False)
        print(f"[INFO] Imágenes no mergeadas para {fecha_str}: {len(df_no_merge)} -> {archivo_no_merge}")
    else:
        print(f"[OK] Todas las imágenes fueron mergeadas correctamente para {fecha_str}")
