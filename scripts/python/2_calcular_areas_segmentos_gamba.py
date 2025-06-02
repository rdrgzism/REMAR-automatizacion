#!/usr/bin/env python3

import os
import cv2
import numpy as np
from datetime import datetime
import shutil

# -------------------------
# CONFIGURACIÓN
# -------------------------
BASE = "C:\\Users\\UIB\\Desktop\\REMAR-automatizacion"
INFERENCE_DIR = os.path.join(BASE, "INFERENCE")
RESULTS_DIR = os.path.join(BASE, "AREAS")
TEMP_DIR = os.path.join(BASE, "TEMP")

IMAGE_WIDTH, IMAGE_HEIGHT = 640, 640


# -------------------------
# FUNCIONES AUXILIARES
# -------------------------
def convertir_txt_a_pixel(entrada, ancho_imagen, alto_imagen):
    with open(entrada, 'r') as file:
        lineas = file.readlines()

    poligonos = []
    for linea in lineas:
        valores = linea.strip().split(' ')
        if len(valores) < 6:
            print(f"Error: coordenadas insuficientes: {linea}")
            continue

        clase_objeto = int(valores[0])
        coordenadas = [float(coord) for coord in valores[1:]]
        coordenadas_pixel = [
            (int(coordenadas[i] * ancho_imagen), int(coordenadas[i + 1] * alto_imagen))
            for i in range(0, len(coordenadas) - 1, 2)
        ]
        poligonos.append((clase_objeto, coordenadas_pixel))
    return poligonos


def calculate_areas(poligonos):
    areas, identificadores = [], []
    for clase_objeto, puntos_pixel in poligonos:
        puntos_array = np.array(puntos_pixel, dtype=np.int32).reshape(-1, 1, 2)
        area = cv2.contourArea(puntos_array)
        areas.append(area)
        identificadores.append(clase_objeto)
    return identificadores, areas


def save_areas_to_file_txt(image_name, identifiers, areas, output_path):
    with open(output_path, 'a') as file:
        file.write(f"Imagen: {image_name}\n")
        for identifier, area in zip(identifiers, areas):
            file.write(f"Identificador: {identifier}, Area: {area}\n")
        file.write("\n")


def extract_datetime(filename):
    try:
        date_str = "_".join(filename.split("_")[2:6])
        return datetime.strptime(date_str, "%Y-%m-%d_%H_%M_%S.%f")
    except Exception as e:
        print(f"Error extrayendo fecha de {filename}: {e}")
        return datetime.now()  # Fallback


# -------------------------
# PROCESAMIENTO PRINCIPAL
# -------------------------
def process_directory(root_path, output_path):
    os.makedirs(output_path, exist_ok=True)
    os.makedirs(TEMP_DIR, exist_ok=True)

    for subdir in sorted(os.listdir(root_path)):
        subdir_path = os.path.join(root_path, subdir)
        if not os.path.isdir(subdir_path):
            continue

        labels_path = os.path.join(subdir_path, "labels")
        if not os.path.exists(labels_path):
            print(f"No hay etiquetas en {subdir_path}")
            continue

        try:
            parts_subdir = subdir.split("_")
            fecha = parts_subdir[-2]
            datetime.strptime(fecha, "%Y-%m-%d")
            fecha_subdir = fecha
        except Exception as e:
            archivos_labels = sorted([f for f in os.listdir(labels_path) if f.endswith(".txt")])
            if archivos_labels:
                dt = extract_datetime(archivos_labels[0])
                fecha_subdir = dt.date().isoformat()
            else:
                print(f"No se pudo determinar fecha para {subdir_path}; no hay .txt: {e}")
                continue

        output_txt = os.path.join(output_path, f"areas_resultados_{fecha_subdir}.txt")
        label_files = sorted(os.listdir(labels_path), key=extract_datetime)

        for label_file in label_files:
            if label_file.endswith(".txt") and label_file.startswith("OPMM_Subasta_"):
                label_file_path = os.path.join(labels_path, label_file)
                poligonos = convertir_txt_a_pixel(label_file_path, IMAGE_WIDTH, IMAGE_HEIGHT)
                identifiers, areas = calculate_areas(poligonos)
                save_areas_to_file_txt(label_file, identifiers, areas, output_txt)

    shutil.rmtree(TEMP_DIR, ignore_errors=True)


# -------------------------
# MAIN
# -------------------------
if __name__ == "__main__":
    process_directory(INFERENCE_DIR, RESULTS_DIR)
    print(f"Análisis de áreas completado. Resultados guardados en: {RESULTS_DIR}")
