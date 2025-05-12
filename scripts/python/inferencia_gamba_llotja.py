#!/usr/bin/env python3

import os
import shutil
from PIL import Image
import py7zr
from ultralytics import YOLO
import yaml

# --------------------
# CARGAR SETTINGS
# --------------------
def load_settings(path='config/settings.yaml'):
    with open(path, 'r') as stream:
        return yaml.safe_load(stream)

settings = load_settings()
paths = settings['paths']
# Año de interés (e.g., 2023 o 2024)
YEAR = str(settings['inferencia']['YEAR'])

CARPETA_7Z = os.path.join(paths['CARPETA_7Z'], YEAR)
OUTPUT_DIR = os.path.join(paths['INFERENCIA_DIR'], f"inferencia_{YEAR}")
TEMP_DIR = os.path.join(OUTPUT_DIR, "temporal")
OUTPUT_TXT = os.path.join(OUTPUT_DIR, f"resultados_metadatos_{YEAR}.txt")
MODEL_PATH = paths['MODEL_WEIGHTS']

# --------------------
# FUNCIONES AUXILIARES
# --------------------
def get_metadatos(metadatos_raw):
    metadatos_dict = {}
    for metadato in metadatos_raw.split("*"):
        if ":" in metadato:
            key, value = metadato.split(":")
            metadatos_dict[key.replace('\x00', '')] = value.replace('\x00', '')
    return metadatos_dict

def extraer_y_procesar_7z(ruta_7z, yolo_model, txtfile):
    if os.path.exists(TEMP_DIR):
        shutil.rmtree(TEMP_DIR)
    os.makedirs(TEMP_DIR)

    try:
        with py7zr.SevenZipFile(ruta_7z, mode='r') as z:
            z.extractall(TEMP_DIR)
    except py7zr.Bad7zFile:
        print(f"[!] Archivo corrupto o inválido: {ruta_7z}")
        return

    for root, _, files in os.walk(TEMP_DIR):
        for filename in sorted(files):
            ruta_img = os.path.join(root, filename)

            try:
                with Image.open(ruta_img) as img:
                    exif = img._getexif()
                    if not exif:
                        continue

                    for _, data in exif.items():
                        if isinstance(data, bytes):
                            data = data.decode("utf-8")

                        metadatos = get_metadatos(data)
                        metadatos.setdefault("Ord", "None")

                        if (
                            metadatos.get("FAO") == "ARA" and
                            metadatos.get("Caj") == "001" and
                            (metadatos.get("Ord") == "1" or "Ord" not in metadatos)
                        ):
                            yolo_model.predict(
                                ruta_img,
                                save=True,
                                save_txt=True,
                                save_conf=True,
                                project=OUTPUT_DIR,
                                name=f"yolo_inference_results_{YEAR}"
                            )

                            pes = metadatos.get("Pes", "N/A").replace(",", ".").strip()
                            vta = metadatos.get("Vta", "N/A").replace(",", ".").strip()
                            txtfile.write(f"{filename}\t{pes}\t{vta}\n")

            except Exception as e:
                print(f"Error procesando imagen {ruta_img}: {e}")
                continue

    shutil.rmtree(TEMP_DIR)

# --------------------
# BLOQUE PRINCIPAL
# --------------------
def main():
    print(f"Iniciando inferencia YOLO para imágenes de {YEAR}...")

    if not os.path.exists(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)

    yolo_model = YOLO(MODEL_PATH)

    with open(OUTPUT_TXT, "w") as txtfile:
        txtfile.write("Imagen\tPes\tVta\n")

        archivos = sorted(os.listdir(CARPETA_7Z))
        for archivo in archivos:
            if archivo.startswith(f"OPMM_Subasta_{YEAR}") and archivo.endswith(".7z"):
                ruta = os.path.join(CARPETA_7Z, archivo)
                print(f"Procesando: {archivo}")
                extraer_y_procesar_7z(ruta, yolo_model, txtfile)

    print(f"Metadatos e inferencia completados. Resultados en {OUTPUT_TXT}")

# --------------------
if __name__ == "__main__":
    main()
