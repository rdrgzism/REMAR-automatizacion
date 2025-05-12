#!/usr/bin/env python3

import pandas as pd
import numpy as np
import yaml
import os

# --------------------
# CARGAR SETTINGS
# --------------------
def load_settings(path='config/settings.yaml'):
    with open(path, 'r') as stream:
        return yaml.safe_load(stream)

settings = load_settings()
paths = settings['paths']
YEAR = str(settings['inferencia']['YEAR'])

# --------------------
# PARÁMETROS DEL MODELO
# --------------------
slo_hat = 1.496995  # Pendiente de la regresión
int_hat = -8.178205  # Intersección de la regresión

# --------------------
# RUTAS
# --------------------
OUTPUT_DIR = paths['OUTPUT_DIR']
inferencia_dir = os.path.join(paths['INFERENCIA_DIR'], f"inferencia_{YEAR}")
archivo_datos = os.path.join(OUTPUT_DIR, f"areas_{YEAR}", f"areas_resultados_{YEAR}.txt")
archivo_metadatos = os.path.join(inferencia_dir, f"resultados_metadatos_{YEAR}.txt")
archivo_resultados_procesados = os.path.join(OUTPUT_DIR, f"resultados_procesados_{YEAR}.csv")
archivo_pesos_individuales = os.path.join(OUTPUT_DIR, f"pesos_individuales_por_imagen_{YEAR}.txt")

# --------------------
# PROCESAMIENTO DE ÁREAS
# --------------------
with open(archivo_datos, 'r') as f:
    lines = f.readlines()

imagenes = []
identificador_0_2 = []
identificador_2 = []
area_total_2 = []
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
        num_id_0_2, num_id_2, area_sum_2 = 0, 0, 0
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

peso_medio_2 = [np.mean(pesos) if pesos else 0 for pesos in pesos_individuales_por_imagen]

df_datos = pd.DataFrame({
    'Imagen': imagenes,
    'Num_ID_0_2': identificador_0_2,
    'Num_ID_2': identificador_2,
    'Area_Media_2': area_total_2,
    'Peso_Medio_2': peso_medio_2
})

# --------------------
# GUARDAR PESOS INDIVIDUALES
# --------------------
with open(archivo_pesos_individuales, 'w') as f_out:
    for img, peso_list in zip(imagenes, pesos_individuales_por_imagen):
        f_out.write(f"Imagen: {img}\n")
        for peso in peso_list:
            f_out.write(f"{peso}\n")

# --------------------
# PROCESAMIENTO DE METADATOS
# --------------------
df_metadatos = pd.read_csv(
    archivo_metadatos,
    sep='\t',
    names=['Imagen', 'Pes', 'Vta']
)
df_metadatos['Imagen'] = df_metadatos['Imagen'].str[:-4]

# --------------------
# MERGE Y CALCULO FINAL
# --------------------
df_final = df_datos.merge(df_metadatos, on='Imagen', how='inner')
df_final['Pes'] = pd.to_numeric(df_final['Pes'], errors='coerce').fillna(0)
df_final['Peso_Medio_2'] = pd.to_numeric(df_final['Peso_Medio_2'], errors='coerce').fillna(0)

df_final['num_gambas'] = df_final['Pes'] / (df_final['Peso_Medio_2'] / 1000)

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

print(f"✅ Archivo '{archivo_resultados_procesados}' generado correctamente con un registro por imagen.")
