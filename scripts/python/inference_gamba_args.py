#!/usr/bin/env python3

import os
import shutil
import argparse
import datetime
from PIL import Image
import py7zr
from ultralytics import YOLO


# ------------------------
# CONFIGURACION GLOBAL
# ------------------------
BASE = "/home/llotja/procesos_cron/GAMBA_BARRACUDA/"
BASE_INPUT = "/home/llotja/"
BASE_RESULTS = os.path.join(BASE, "INFERENCE")
BASE_TEMPORAL = os.path.join(BASE_RESULTS, "temporal")
YOLO_WEIGHTS = "best.pt"
YEAR = datetime.date.today().year
OUTPUT_TXT = os.path.join(BASE_RESULTS, f"resultados_metadatos.txt")


# ------------------------
# ARGUMENTOS
# ------------------------
def parser_arguments():
    parser = argparse.ArgumentParser("YOLO Inference")
    parser.add_argument('--today', action="store_true", default=False,
                        help="Procesa el archivo correspondiente al día actual")
    parser.add_argument('--initial_date', type=str, default="",
                        help="Fecha inicial (dd-mm-yyyy)")
    parser.add_argument('--final_date', type=str, default="",
                        help="Fecha final (dd-mm-yyyy)")
    return parser.parse_args()


# ------------------------
# DETERMINAR SI PROCESAR
# ------------------------
def get_date(file_date, args):
    if args.today:
        return file_date == datetime.date.today()
    elif args.initial_date and args.final_date:
        start = datetime.datetime.strptime(args.initial_date, "%d-%m-%Y").date()
        end = datetime.datetime.strptime(args.final_date, "%d-%m-%Y").date()
        return start <= file_date <= end
    return True


# ------------------------
# EXTRAER METADATOS
# ------------------------
def get_metadatos(metadatos_raw):
    metadatos = {}
    for item in metadatos_raw.split("*"):
        if ":" in item:
            key, value = item.split(":")
            metadatos[key.replace('\x00', '')] = value.replace('\x00', '')
    return metadatos


# ------------------------
# MAIN
# ------------------------
def main():
    args = parser_arguments()

    # Preparar directorios
    os.makedirs(BASE_RESULTS, exist_ok=True)
    yolo = YOLO(YOLO_WEIGHTS)

    # Archivo de salida
    with open(OUTPUT_TXT, mode="w") as txtfile:
        txtfile.write("Imagen\tPes\tVta\n")

        for archivo in sorted(os.listdir(BASE_INPUT)):
            if not archivo.startswith(f"OPMM_Subasta_{YEAR}" or not archivo.endswith(".7z")):
                continue

            # Parsear fecha del archivo
            try:
                file_date = datetime.datetime.strptime(archivo.split("_")[2], "%Y-%m-%d").date()
            except (IndexError, ValueError):
                continue

            if not get_date(file_date, args):
                continue

            ruta_7z = os.path.join(BASE_INPUT, archivo)
            print(f"Procesando {archivo}")

            # Reset temporal folder
            if os.path.exists(BASE_TEMPORAL):
                shutil.rmtree(BASE_TEMPORAL)
            os.makedirs(BASE_TEMPORAL)

            try:
                with py7zr.SevenZipFile(ruta_7z, mode='r') as z:
                    z.extractall(BASE_TEMPORAL)
            except py7zr.Bad7zFile:
                print(f"Archivo corrupto: {archivo}")
                continue

            # Iterar sobre imágenes extraídas
            for root, _, files in os.walk(BASE_TEMPORAL):
                for filename in sorted(files):
                    ruta_img = os.path.join(root, filename)
                    try:
                        with Image.open(ruta_img) as img:
                            exifdata = img._getexif()
                            if not exifdata:
                                continue

                            for _, data in exifdata.items():
                                if isinstance(data, bytes):
                                    data = data.decode("utf-8")
                                metadatos = get_metadatos(data)
                                metadatos.setdefault("Ord", "None")

                                # Criterios
                                if (
                                    metadatos.get("FAO") == "ARA" and
                                    metadatos.get("Caj") == "001" and
                                    (metadatos.get("Ord") == "1" or "Ord" not in metadatos)
                                ):
                                    # Inferencia YOLO
                                    yolo.predict(
                                        ruta_img,
                                        save=True,
                                        save_txt=True,
                                        save_conf=True,
                                        project=BASE_RESULTS,
                                        name=f"yolo_inference_results_{YEAR}"
                                    )

                                    pes = metadatos.get("Pes", "N/A").replace(",", ".").strip()
                                    vta = metadatos.get("Vta", "N/A").replace(",", ".").strip()
                                    txtfile.write(f"{filename}\t{pes}\t{vta}\n")
                    except Exception as e:
                        print(f"Error procesando imagen: {ruta_img} -> {e}")
                        continue

            shutil.rmtree(BASE_TEMPORAL)

    print(f"Inferencia finalizada. Resultados en: {OUTPUT_TXT}")


# ------------------------
if __name__ == "__main__":
    main()
