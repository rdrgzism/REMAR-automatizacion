# REMAR - Automatización del Análisis Pesquero

Este proyecto tiene como objetivo la automatización del procesamiento de datos de pesca artesanal 
en las Islas Baleares, incluyendo la estimación de esfuerzo pesquero generado por embarcaciones artesanales y 
recreativas, la inferencia de talla diferentes especies de interés, entre ellas la gamba roja, a partir de 
imágenes mediante redes neuronales.

## 📁 Estructura del Proyecto
```
    REMAR-automatizacion/
    ├── config/                 # Configuración YAML y variables de entorno
    ├── data/                   # Datos brutos, procesados, tablas de referencia y shapefiles
    ├── env/                    # Definición del entorno Conda
    ├── logs/                   # Registros de ejecución
    ├── models/                 # Modelos de Stan y YOLO *** MODELO YOLO POR MOVER ***
    ├── scripts/
    │   ├── r/                  # Scripts R, clasificación de hojas de venta por metier y estimación de esfuerzo pesquero
    │   └── python/             # Scripts Python, inferencia de talla de gamba roja a partir de imágenes
    ├── Makefile                # Automatización del entorno
    ├── install.R               # Instalación de cmdstanr
    ├── run_job.sh              # Script de procesamiento principal (SSM)
    ├── run_inferencia.sh       # Script para inferencia de talla de gamba
    └── README.md               
```

## ⚙️ Configuración Inicial

1. Clonar el repositorio:
```bash
    git clone https://github.com/usuario/REMAR-automatizacion.git
    cd REMAR-automatizacion
```

2. Crear entorno Conda:
```bash
    conda env create -f env/environment.yml
```

3. Instalar `cmdstanr` (solo una vez):
```bash
    make setup-cmdstanr
```

## 🚀 Ejecución del Pipeline

### Procesamiento Principal (Esfuerzo pesquero - SSM)
```bash
    ./run_job.sh
```
Este script lanza los módulos del 1 al 6 del flujo de esfuerzo pesquero.

### Inferencia de talla de gamba (YOLO)
```bash
    ./run_inferencia.sh
```
Este script ejecuta inferencia YOLO y postprocesamiento para estimar pesos y número de individuos.

## 🤖 Automatización con `cron`
```cron
    # Ejecuta el pipeline principal cada 2 días a las 20:00
    0 20 */2 * * /ruta/a/run_job.sh
    
    # Ejecuta la inferencia de gamba cada 7 días (lunes) a las 21:00
    0 21 * * 1 /ruta/a/run_inferencia.sh
```

## 🔐 Buenas Prácticas de Seguridad
- Las credenciales se guardan en `.env`, no en `settings.yaml`.
- `.gitignore` protege carpetas como `data/`, `logs/`, `env/`, y archivos sensibles.

## 📦 Dependencias Principales
**Python**: `ultralytics`, `py7zr`, `pandas`, `pyyaml`, `python-dotenv`, `pyodbc`, `Pillow`

**R**: `dplyr`, `stringr`, `rstan`, `cmdstanr`, `sf`, `lubridate`, `moveHMM`

## 📬 Contacto
Autor: Ismael Rodríguez
Email: ismael.rodriguez@fueib.org