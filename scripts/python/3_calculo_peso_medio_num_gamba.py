#!/usr/bin/env python3

import os
import datetime
import numpy as np
import pandas as pd

# ------------------------
# CONFIGURACIÓN
# ------------------------

# Carpeta principal donde tienes AREAS/, INFERENCE/, TEMP/ y RESULTS/
BASE_DIR = r"C:\Users\UIB\Desktop\REMAR-automatizacion"
AREAS_DIR = os.path.join(BASE_DIR, "AREAS")       # contiene areas_resultados_YYYY-MM-DD.txt
INFERENCE_DIR = os.path.join(BASE_DIR, "INFERENCE")   # contiene un único resultados_metadatos.txt
TEMP_DIR = os.path.join(AREAS_DIR, "TEMP")        # para guardar los archivos pesos_individuales_por_imagen_<fecha>.txt
RESULTS_DIR = os.path.join(BASE_DIR, "RESULTS")     # para guardar resultados_procesados_<fecha>.csv

# Asegurarse de que existan TEMP_DIR y RESULTS_DIR
os.makedirs(TEMP_DIR, exist_ok=True)
os.makedirs(RESULTS_DIR, exist_ok=True)

# Parámetros del modelo (idénticos a los de scripts anteriores)
SLO_HAT = 1.496995
INT_HAT = -8.178205


def procesar_fecha(fecha_str):
    """
    Dada una fecha 'YYYY-MM-DD', busca:
      - AREAS/areas_resultados_<fecha_str>.txt
      - INFERENCE/resultados_metadatos.txt (fichero único con todas las fechas)

    Filtra en ese metadatos solo las filas cuya 'Imagen' contenga fecha_str,
    y genera:
      - TEMP/pesos_individuales_por_imagen_<fecha_str>.txt
      - RESULTS/resultados_procesados_<fecha_str>.csv
    """
    # 1) Rutas de entrada y salida
    archivo_datos = os.path.join(AREAS_DIR, f"areas_resultados_{fecha_str}.txt")
    if not os.path.isfile(archivo_datos):
        print(f"[{fecha_str}] ERROR: no existe el archivo de áreas:\n  {archivo_datos}")
        return

    # Usamos el único fichero de metadatos global
    archivo_metadatos = os.path.join(INFERENCE_DIR, "resultados_metadatos.txt")
    if not os.path.isfile(archivo_metadatos):
        print(f"[{fecha_str}] ERROR: no existe el fichero global de metadatos:\n  {archivo_metadatos}")
        return

    output_pesos_ind = os.path.join(TEMP_DIR, f"pesos_individuales_por_imagen_{fecha_str}.txt")
    archivo_resultados_procesados = os.path.join(RESULTS_DIR, f"resultados_procesados_{fecha_str}.csv")

    # -------------------------------------------------------------------------
    # 2) Leer y procesar el fichero de ÁREAS
    # -------------------------------------------------------------------------
    # El formato de areas_resultados_<fecha>.txt es:
    #   Imagen: OPMM_Subasta_<fecha>_HH_MM_SS._Imedea.txt
    #   Identificador: <clase>, Area: <valor>
    #   … (varias líneas de Identificador/Area)
    #   (vacío)
    #   Imagen: OPMM_Subasta_<fecha>_…_Imedea.txt
    #   Identificador: …
    #   …
    with open(archivo_datos, 'r', encoding="utf-8") as f:
        lines = f.readlines()

    imagenes = []
    identificador_0_2_list = []
    identificador_2_list = []
    area_media_2_list = []
    pesos_individuales_por_imagen = []

    imagen_actual = None
    num_id_0_2 = 0
    num_id_2 = 0
    area_sum_2 = 0
    pesos_temp = []

    for line in lines:
        line = line.strip()
        if not line:
            # salto de línea, seguimos
            continue

        if line.startswith("Imagen:"):
            # Si ya teníamos una imagen activa, la “cerramos”
            if imagen_actual is not None:
                base = os.path.splitext(imagen_actual)[0]  # quita ".txt"
                imagenes.append(base)
                identificador_0_2_list.append(num_id_0_2)
                identificador_2_list.append(num_id_2)
                area_media_2_list.append(area_sum_2 / num_id_2 if num_id_2 > 0 else 0)
                pesos_individuales_por_imagen.append(pesos_temp)

            # Arrancamos nuevo bloque con el nombre completo del .txt
            imagen_actual = line.split(":", 1)[1].strip()
            num_id_0_2 = 0
            num_id_2 = 0
            area_sum_2 = 0
            pesos_temp = []

        elif line.startswith("Identificador:"):
            parts = line.split(",")
            try:
                identificador = int(parts[0].split(":", 1)[1].strip())
                area = float(parts[1].split(":", 1)[1].strip())
            except Exception as e:
                print(f"[{fecha_str}] WARNING: no pude parsear línea '{line}' → {e}")
                continue

            if identificador in (0, 2):
                num_id_0_2 += 1
            if identificador == 2:
                num_id_2 += 1
                area_sum_2 += area
                peso = np.exp(np.log(area) * SLO_HAT + INT_HAT)
                pesos_temp.append(peso)

    # Cerrar último bloque si queda
    if imagen_actual is not None:
        base = os.path.splitext(imagen_actual)[0]
        imagenes.append(base)
        identificador_0_2_list.append(num_id_0_2)
        identificador_2_list.append(num_id_2)
        area_media_2_list.append(area_sum_2 / num_id_2 if num_id_2 > 0 else 0)
        pesos_individuales_por_imagen.append(pesos_temp)

    # 3) Construir DataFrame con resultados de áreas y pesos
    peso_medio_2_list = [np.mean(p) if p else 0 for p in pesos_individuales_por_imagen]

    df_datos = pd.DataFrame({
        'Imagen': imagenes,
        'Num_ID_0_2': identificador_0_2_list,
        'Num_ID_2': identificador_2_list,
        'Area_Media_2': area_media_2_list,
        'Peso_Medio_2': peso_medio_2_list
    })

    # 4) Guardar pesos individuales en TEMP_DIR
    try:
        with open(output_pesos_ind, 'w', encoding="utf-8") as f_out:
            for img, lista_pesos in zip(imagenes, pesos_individuales_por_imagen):
                f_out.write(f"Imagen: {img}\n")
                for p in lista_pesos:
                    f_out.write(f"{p}\n")
        print(f"[{fecha_str}] Pesos individuales escritos en:\n  {output_pesos_ind}")
    except Exception as e:
        print(f"[{fecha_str}] ERROR al escribir '{output_pesos_ind}': {e}")

    # -------------------------------------------------------------------------
    # 5) Leer METADATOS desde INFERENCE/resultados_metadatos.txt y filtrar
    # -------------------------------------------------------------------------
    try:
        df_meta_full = pd.read_csv(
            archivo_metadatos,
            sep='\t',
            names=['Imagen', 'Pes', 'Vta'],
            dtype={'Imagen': str, 'Pes': str, 'Vta': str},
            encoding='cp1252'
        )
    except Exception as e:
        print(f"[{fecha_str}] ERROR al leer metadatos '{archivo_metadatos}': {e}")
        return

    # Filtrar solo aquellas filas cuya columna 'Imagen' contenga la fecha actual
    # Ejemplo: "OPMM_Subasta_2025-05-30_04_55_15.945_Imedea.jpg"
    df_metadatos = df_meta_full[df_meta_full['Imagen'].str.contains(fecha_str, na=False)].copy()

    if df_metadatos.empty:
        print(f"[{fecha_str}] ADVERTENCIA: no se encontraron metadatos para esta fecha en '{archivo_metadatos}'")
        return

    # Quitar extensión (.jpg, .png, etc.) de la columna Imagen
    df_metadatos['Imagen'] = df_metadatos['Imagen'].apply(lambda x: os.path.splitext(x.strip())[0])

    # Convertir 'Pes' a float (reemplazar coma por punto)
    df_metadatos['Pes'] = (
        df_metadatos['Pes']
        .str.replace(",", ".", regex=False)
        .astype(float, errors='ignore')
        .fillna(0)
    )

    # -------------------------------------------------------------------------
    # 6) Merge entre df_datos (áreas/pesos) y df_metadatos filtrado (Pes/Vta)
    # -------------------------------------------------------------------------

    df_final = df_datos.merge(df_metadatos, on='Imagen', how='inner')
    print(f"[{fecha_str}] Filas tras merge: {len(df_final)}")

    # Excluir las imágenes sin detecciones de clase 2
    df_final = df_final[df_final['Num_ID_2'] > 0]

    # Asegurar que 'Peso_Medio_2' sea numérico
    df_final['Peso_Medio_2'] = pd.to_numeric(df_final['Peso_Medio_2'], errors='coerce').fillna(0)

    # Calcular número de gambas
    df_final['num_gambas'] = df_final['Pes'] / (df_final['Peso_Medio_2'] / 1000)

    # -------------------------------------------------------------------------
    # 7) Agrupar y exportar CSV final en RESULTS_DIR
    # -------------------------------------------------------------------------
    df_agrupado = df_final.groupby('Imagen', as_index=False).agg(
        Num_ID_0_2=('Num_ID_0_2',   'first'),
        Num_ID_2=('Num_ID_2',     'first'),
        Area_media_2=('Area_Media_2', 'first'),
        Peso_Medio_2=('Peso_Medio_2', 'first'),
        Pes=('Pes',          'first'),
        Vta=('Vta',          'first'),
        num_gambas=('num_gambas',   'first')
    )

    try:
        df_agrupado.to_csv(archivo_resultados_procesados, index=False)
        print(f"[{fecha_str}] CSV generado correctamente en:\n  {archivo_resultados_procesados}")
    except Exception as e:
        print(f"[{fecha_str}] ERROR al escribir '{archivo_resultados_procesados}': {e}")


def main():
    """
    Recorre todos los archivos en AREAS_DIR.
    Si el nombre coincide con 'areas_resultados_YYYY-MM-DD.txt',
    extrae la fecha y llama a procesar_fecha().
    """
    for fname in sorted(os.listdir(AREAS_DIR)):
        if not (fname.startswith("areas_resultados_") and fname.endswith(".txt")):
            continue

        fecha_str = fname[len("areas_resultados_"):-len(".txt")]
        try:
            datetime.datetime.strptime(fecha_str, "%Y-%m-%d")
        except ValueError:
            print(f"Ignorando '{fname}': '{fecha_str}' no tiene formato YYYY-MM-DD")
            continue

        procesar_fecha(fecha_str)


if __name__ == "__main__":
    main()
