import os
from dotenv import load_dotenv
import smtplib, ssl
from email.message import EmailMessage
from pathlib import Path
from datetime import date
import pandas as pd

load_dotenv(dotenv_path=Path("~/REMAR-automatizacion/config/.env").expanduser())

# --- Configuración ---
SMTP_SERVER = os.getenv("SMTP_SERVER")
PORT = os.getenv("SMTP_PORT")
EMAIL_SENDER = os.getenv("EMAIL_SENDER")
EMAIL_PASSWORD = os.getenv("EMAIL_PASSWORD")
EMAIL_RECEIVER = {
    "MIQUEL_PALMER": "",
    "EVA_MORAGES": "",
    "LLUISA_FLAQUER": "",
    "ISMAEL_RODRIGUEZ": ""
}

today_str = date.today().strftime("%Y-%m-%d")
logs_dir = Path("logs")

# --- Funciones auxiliares ---
def leer_csv(path, col=None):
    if path.exists():
        df = pd.read_csv(path, encoding="iso-8859-1")
        if col is None:
            return df
        if col in df.columns:
            return df[col].dropna().tolist()
    return []

def leer_log(path):
    return path.read_text(encoding="iso-8859-1") if path.exists() else "No se encontró el archivo."

# --- Archivos esperados ---
log_sp = logs_dir / f"error_{today_str}.log"
csv_sp = logs_dir / f"unknown_species_{today_str}.csv"

log_boat = logs_dir / f"unknown_boats_{today_str}.log"
csv_boats = logs_dir / f"unknown_boats_{today_str}.csv"

logs_cv = [
    logs_dir / f"log_few_bips_{today_str}.log", 
    logs_dir / f"log_tracks_in_port_{today_str}.log", 
    logs_dir / f"log_tracks_inland_{today_str}.log"
]

# --- Composición del correo ---
message = EmailMessage()
message["Subject"] = f"REMAR - Reporte de día {today_str}"
message["From"] = EMAIL_SENDER
message["To"] = ", ".join(filter(None, EMAIL_RECEIVER.values()))

partes = []

# --- Bloque 1: Especies desconocidas ---
if log_sp.exists():
    log_txt = leer_log(log_sp)
    especies = leer_csv(csv_sp, col="NUEVA_ESPECIE")
    especies_txt = "\n".join(f"- {e}" for e in especies) if especies else "No hay especies nuevas listadas."
    partes.append(f"""\
[ERROR CRÍTICO] ESPECIES DESCONOCIDAS
Log: {log_sp.name}
{log_txt.strip()}

Especies detectadas:
{especies_txt}
""")

# --- Bloque 2: Embarcaciones desconocidas ---
if csv_boats.exists():
    try:
        df_boats = pd.read_csv(csv_boats, encoding="iso-8859-1")
        if {"CENSO", "NEMBARCACION"}.issubset(df_boats.columns):
            table = "\n".join([f"- {censo:<12} | {nombre}" for censo, nombre in zip(df_boats["CENSO"], df_boats["NEMBARCACION"])])
        else:
            table = "Faltan columnas esperadas (CENSO, NEMBARCACION)."
    except Exception as e:
        table = f"Error al leer archivo de barcos: {str(e)}"
    
    partes.append(f"""\
[ALERTA] EMBARCACIONES DESCONOCIDAS
Log: {log_boat.name}
{table}
""")

# --- Bloque 3: Cajas verdes (GPS logs) ---
for log in logs_cv:
    if log.exists():
        contenido = leer_log(log).strip()
        partes.append(f"""\
[CAJAS VERDES] {log.name}
{contenido}
""")

# --- Final del cuerpo del mensaje ---
cuerpo = "\n\n".join(partes) if partes else "No se detectaron errores ni advertencias."
message.set_content(cuerpo)

# --- ENVÍO DE CORREO ---
context = ssl.create_default_context()
with smtplib.SMTP_SSL(SMTP_SERVER, PORT, context=context) as server:
    server.login(EMAIL_SENDER, EMAIL_PASSWORD)
    server.send_message(message)

print("Correo enviado con el error crítico.")