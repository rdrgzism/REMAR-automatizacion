#!/usr/bin/env python3

import os
import datetime
import numpy as np
import pandas as pd

# ------------------------
# CONFIGURACIÓN
# ------------------------
YEAR = datetime.date.today().year
BASE_DIR = "/home/llotja/procesos_cron/GAMBA_BARRACUDA"
RESULTS_DIR = os.path.join(BASE_DIR, "RESULTADOS")
INFERENCIA_DIR = os.path.join(BASE_DIR, "INFERENCE")

archivo_datos = os.path.join(RESULTS_DIR, f"areas_resultados_{YEAR}.txt")
archivo_metadatos = os.path.join(INFERENCIA_DIR, f"resultados_metadatos_{YEAR}.txt")
output_pesos_ind = os.path.join(RESULTS_DIR, f"pesos_individuales_por_imagen_{YEAR}.txt")
archivo_resultados_procesados = os.path.join(RESULTS_DIR, f"resultados_procesados_{YEAR}.csv")

# ------------------------
# PARÁMETROS DEL MODELO
# ------------------------
slo_hat = 1.496995
int_hat = -8.178205

# ------------------------
# LECTURA Y PROCESAMIENTO DE ÁREAS
# ------------------------
with open(archivo_datos, 'r') as f:
    lines = f.readlines()

imagenes = []
identificador_0_2, identificador_2, area_total_2 = [], [], []
pesos_individuales_por_imagen = []

imagen_actual = None
num_id_0_2 = 0
num_id_2 = 0
area_sum_2 = 0
pesos = []

for line in lines:
    line = line.strip()

    if line.startswith("Imagen:"):
        if imagen_actual is not None:
            imagenes.append(imagen_actual[:-4])
            identificador_0_2.append(num_id_0_2)
            identificador_2.append(num_id_2)
            area_total_2.append(area_sum_2 / num_id_2 if num_id_2 > 0 else 0)
            pesos_individuales_por_imagen.append(pesos)

        imagen_actual = line.split(":")[1].strip()
        num_id_0_2 = 0
        num_id_2 = 0
        area_sum_2 = 0
        pesos = []

    elif line.startswith("Identificador:"):
        parts = line.split(",")
        identificador = int(parts[0].split(":")[1].strip())
        area = float(parts[1].split(":")[1].strip())

        if identificador in [0, 2]:
            num_id_0_2 += 1
        if identificador == 2:
            num_id_2 += 1
            area_sum_2 += area
            peso = np.exp(np.log(area) * slo_hat + int_hat)
            pesos.append(peso)

if imagen_actual is not None:
    imagenes.append(imagen_actual[:-4])
    identificador_0_2.append(num_id_0_2)
    identificador_2.append(num_id_2)
    area_total_2.append(area_sum_2 / num_id_2 if num_id_2 > 0 else 0)
    pesos_individuales_por_imagen.append(pesos)

# ------------------------
# CREAR DATAFRAME CON RESULTADOS
# ------------------------
peso_medio_2 = [np.mean(p) if p else 0 for p in pesos_individuales_por_imagen]

df_datos = pd.DataFrame({
    'Imagen': imagenes,
    'Num_ID_0_2': identificador_0_2,
    'Num_ID_2': identificador_2,
    'Area_Media_2': area_total_2,
    'Peso_Medio_2': peso_medio_2
})

# Guardar pesos individuales
with open(output_pesos_ind, 'w') as f_out:
    for img, pesos in zip(imagenes, pesos_individuales_por_imagen):
        f_out.write(f"Imagen: {img}\n")
        for peso in pesos:
            f_out.write(f"{peso}\n")

# ------------------------
# UNIR CON METADATOS
# ------------------------
df_metadatos = pd.read_csv(archivo_metadatos, sep='\t', names=['Imagen', 'Pes', 'Vta'])
df_metadatos['Imagen'] = df_metadatos['Imagen'].str[:-4]

df_final = df_datos.merge(df_metadatos, on='Imagen', how='inner')
df_final['Pes'] = pd.to_numeric(df_final['Pes'], errors='coerce').fillna(0)
df_final['Peso_Medio_2'] = pd.to_numeric(df_final['Peso_Medio_2'], errors='coerce').fillna(0)
df_final['num_gambas'] = df_final['Pes'] / (df_final['Peso_Medio_2'] / 1000)

# ------------------------
# AGRUPAR Y EXPORTAR
# ------------------------
df_agrupado = df_final.groupby('Imagen', as_index=False).agg(
    Num_ID_0_2=('Num_ID_0_2', 'first'),
    Num_ID_2=('Num_ID_2', 'first'),
    Area_media_2=('Area_Media_2', 'first'),
    Peso_Medio_2=('Peso_Medio_2', 'first'),
    Pes=('Pes', 'first'),
    Vta=('Vta', 'first'),
    num_gambas=('num_gambas', 'first')
)

df_agrupado.to_csv(archivo_resultados_procesados, index=False)

print(f"Archivo '{archivo_resultados_procesados}' generado correctamente.")
