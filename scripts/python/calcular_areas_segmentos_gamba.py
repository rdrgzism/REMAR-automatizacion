import os
import cv2
import numpy as np
import csv
from PIL import Image
import py7zr
import shutil
import yaml

# --------------------
# Cargar settings.yaml
# --------------------
def load_settings(path='config/settings.yaml'):
    with open(path, 'r') as stream:
        return yaml.safe_load(stream)

settings = load_settings()
paths = settings['paths']
YEAR = str(settings['inferencia']['YEAR'])

# --------------------
# Rutas
# --------------------
root_dir_path = os.path.join(paths['INFERENCIA_DIR'], f"inferencia_{YEAR}")
output_dir_path = os.path.join(paths['OUTPUT_DIR'], f"areas_{YEAR}")
output_dir_csv = os.path.join(output_dir_path, f"areas_{YEAR}.csv")
temp_dir = "./temp_extracted_images"


# --------------------
# Funciones
# --------------------
def convertir_txt_a_pixel(entrada, ancho_imagen, alto_imagen):
    with open(entrada, 'r') as file:
        lineas = file.readlines()
    poligonos = []
    for linea in lineas:
        valores = linea.strip().split(' ')
        if len(valores) < 6:
            print(f"[!] Línea inválida: {linea}")
            continue
        clase_objeto = int(valores[0])
        coordenadas = [float(coord) for coord in valores[1:]]
        coordenadas_pixel = [(int(coordenadas[i] * ancho_imagen), int(coordenadas[i+1] * alto_imagen)) for i in range(0, len(coordenadas) - 1, 2)]
        poligonos.append((clase_objeto, coordenadas_pixel))
    return poligonos


def calculate_areas(poligonos):
    areas, identificadores = [], []
    for clase_objeto, puntos_pixel in poligonos:
        puntos_array = np.array(puntos_pixel, dtype=np.int32).reshape(-1, 1, 2)
        areas.append(cv2.contourArea(puntos_array))
        identificadores.append(clase_objeto)
    return identificadores, areas


def dibujar_poligonos_en_imagen(imagen, poligonos):
    imagen_dibujada = np.copy(imagen)
    for clase_objeto, puntos_pixel in poligonos:
        puntos_array = np.array(puntos_pixel, np.int32).reshape((-1, 1, 2))
        # print(puntos_pixel)
        imagen_dibujada = cv2.polylines(imagen_dibujada, [puntos_array], isClosed=True, color=(0, 255, 0), thickness=2)
        cv2.putText(imagen_dibujada, str(clase_objeto), (puntos_array[0][0][0], puntos_array[0][0][1]),
                    cv2.FONT_HERSHEY_SIMPLEX, 0.5, (0, 0, 255), 2, cv2.LINE_AA)
    return imagen_dibujada


def save_areas_to_file(image_name, identifiers, areas, output_path):
    with open(output_path, 'a', newline='') as file:
        writer = csv.writer(file)
        for identifier, area in zip(identifiers, areas):
            writer.writerow([identifier, area, image_name])


# --------------------
# Lógica principal
# --------------------
def process_directory(root_path, output_path, output_csv):
    ancho_imagen, alto_imagen = 640, 640

    if not os.path.exists(output_path):
        os.makedirs(output_path)

    files1 = sorted(os.listdir(root_path))

    for subdir in files1:
        subdir_path = os.path.join(root_path, subdir)
        if not os.path.isdir(subdir_path):
            continue

        labels_path = os.path.join(subdir_path, "labels")
        if not os.path.exists(labels_path):
            continue

        if os.path.exists(temp_dir):
            shutil.rmtree(temp_dir)
        os.makedirs(temp_dir)

        files2 = sorted(os.listdir(labels_path))
        for label_file in files2:
            if label_file.endswith(".txt") and label_file.startswith("OPMM_Subasta_"):
                label_file_path = os.path.join(labels_path, label_file)
                poligonos = convertir_txt_a_pixel(label_file_path, ancho_imagen, alto_imagen)
                identifiers, areas = calculate_areas(poligonos)
                save_areas_to_file(label_file, identifiers, areas, output_csv)

        shutil.rmtree(temp_dir)

# --------------------
if __name__ == "__main__":
    process_directory(root_dir_path, output_dir_path, output_dir_csv)
    print(f"Proceso completado. Áreas guardadas en: {output_dir_csv}")
