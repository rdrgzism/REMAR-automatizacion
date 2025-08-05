import os
from dotenv import load_dotenv
import smtplib, ssl
from email.message import EmailMessage
from email.utils import formatdate
from pathlib import Path
from datetime import date, timedelta
from typing import Dict, List
import pandas as pd

# =========================
# Configuración
# =========================
# Carga .env (ajusta la ruta si procede)
load_dotenv(dotenv_path=Path("~/REMAR-automatizacion/config/.env").expanduser())

SMTP_SERVER = os.getenv("SMTP_SERVER", "smtp.gmail.com")
PORT = int(os.getenv("SMTP_PORT", "465"))
EMAIL_SENDER = os.getenv("EMAIL_SENDER", "")
EMAIL_PASSWORD = os.getenv("EMAIL_PASSWORD", "")

EMAIL_RECEIVER: Dict[str, str] = {
    "MIQUEL_PALMER": "", # os.getenv("MAIL_MIQUEL_PALMER", ""),
    "EVA_MORAGES": "", # os.getenv("MAIL_EVA_MORAGES", ""),
    "LLUISA_FLAQUER": "", # os.getenv("MAIL_LLUISA_FLAQUER", ""),
    "ISMAEL_RODRIGUEZ": "ismael.rodriguez@fueib.org", # os.getenv("MAIL_ISMAEL_RODRIGUEZ", ""),
    "JOSE_MARIA_DISDER": "jmdisdier@imedea.uib-csic.es", # os.getenv("MAIL_JOSE_MARIA_DISDER", "")
}

# =========================
# Utilidades
# =========================
def validar_config():
    faltan = [k for k, v in {
        "SMTP_SERVER": SMTP_SERVER,
        "EMAIL_SENDER": EMAIL_SENDER,
        "EMAIL_PASSWORD": EMAIL_PASSWORD,
    }.items() if not v]
    if faltan:
        raise RuntimeError(f"Faltan variables de entorno: {', '.join(faltan)}")
    if not any(EMAIL_RECEIVER.values()):
        raise RuntimeError("No hay destinatarios válidos en EMAIL_RECEIVER.")

def header(fecha: str) -> str:
    return (
        "***************************************\n"
        f"📋 REPORTE AUTOMÁTICO DE REMAR - {fecha}\n"
        "***************************************\n\n"
    )

def footer() -> str:
    return (
        "\n---\n"
        "Este mensaje ha sido generado automáticamente por el sistema REMAR.\n"
        "Por favor, revisa los detalles si hay errores o advertencias.\n"
    )

def leer_csv_col(path: Path, col: str):
    """Devuelve una lista con la columna `col` si existe, o [] si no."""
    if not path.exists():
        return []
    for enc in ("iso-8859-1", "utf-8"):
        try:
            df = pd.read_csv(path, encoding=enc, dtype=str)
            break
        except Exception:
            continue
    else:
        return []
    return df[col].dropna().tolist() if col in df.columns else []

def leer_log_txt(path: Path) -> str:
    if not path.exists():
        return "No se encontró el archivo."
    for enc in ("iso-8859-1", "utf-8"):
        try:
            return path.read_text(encoding=enc)
        except Exception:
            continue
    return "⚠️ No se pudo leer el log."

def resumen_log(path: Path, max_lineas: int = 10) -> str:
    """
    Resumen legible de CSV/LOG:
      - CSV: lee con pandas (dtype=str), limpia espacios/comillas, elimina filas vacías y muestra head.
              Si no hay datos -> '✅ Sin incidencias registradas.'
      - Texto: primeras `max_lineas` líneas no vacías, con contador.
    """
    if not path.exists():
        return "No se encontró el archivo."

    if path.suffix.lower() == ".csv":
        try:
            df = pd.read_csv(path, encoding="iso-8859-1", dtype=str)
        except UnicodeDecodeError:
            try:
                df = pd.read_csv(path, encoding="utf-8", dtype=str)
            except Exception:
                return "⚠️ No se pudo leer el CSV."
        except pd.errors.EmptyDataError:
            return "✅ Sin incidencias registradas."
        except Exception:
            return "⚠️ No se pudo leer el CSV."

        if df.empty:
            return "✅ Sin incidencias registradas."

        # Limpieza ligera y normalización de vacíos
        df = df.map(lambda x: x.strip().strip('"').strip("'") if isinstance(x, str) else x)
        df = df.replace(r'^\s*$', pd.NA, regex=True).dropna(how="all")

        if df.empty:
            return "✅ Sin incidencias registradas."

        head = df.head(max_lineas)
        return f"{len(df)} filas registradas (mostrando primeras {len(head)}):\n" + head.to_string(index=False)

    # Logs de texto
    try:
        with path.open(encoding="iso-8859-1") as f:
            lineas = [ln.strip() for ln in f if ln.strip()]
    except UnicodeDecodeError:
        try:
            with path.open(encoding="utf-8") as f:
                lineas = [ln.strip() for ln in f if ln.strip()]
        except Exception:
            return "⚠️ No se pudo leer el log."
    except Exception:
        return "⚠️ No se pudo leer el log."

    if not lineas:
        return "✅ Sin incidencias registradas."

    total = len(lineas)
    resumen = "\n".join(lineas[:max_lineas])
    if total > max_lineas:
        resumen += f"\n... ({total - max_lineas} líneas más)"
    return resumen

def normalizar_headers(df: pd.DataFrame) -> pd.DataFrame:
    def norm(s: str) -> str:
        s = s.strip().strip('"').strip("'").upper()
        return (s.replace("Á","A").replace("É","E").replace("Í","I")
                 .replace("Ó","O").replace("Ú","U").replace("Ü","U"))
    df = df.copy()
    df.columns = [norm(c) for c in df.columns]
    return df

def tabla_embarcaciones(csv_boats: Path, log_boat: Path, max_rows: int = 20) -> str:
    try:
        dfb = pd.read_csv(csv_boats, encoding="iso-8859-1", dtype=str)
    except UnicodeDecodeError:
        dfb = pd.read_csv(csv_boats, encoding="utf-8", dtype=str)
    except Exception as e:
        return f"Error al leer archivo de barcos: {str(e)}"

    dfb = normalizar_headers(dfb)

    variants = {
        "CENSO": ["CENSO", "CODCENSO", "COD_CENSO", "CODIGO_CENSO"],
        "NEMBARCACION": ["NEMBARCACION", "NEMBARCACIONES", "NEMBARCACIONS", "NOMBRE", "NOMBRE_EMBARCACION"],
    }
    rename = {}
    for canon, opts in variants.items():
        for o in opts:
            if o in dfb.columns:
                rename[o] = canon
                break
    dfb = dfb.rename(columns=rename)

    needed = {"CENSO", "NEMBARCACION"}
    if not needed.issubset(dfb.columns):
        cols_str = ", ".join(dfb.columns)
        return f"Faltan columnas esperadas (CENSO, NEMBARCACION). Encontradas: {cols_str}"

    tdf = dfb.loc[:, ["CENSO", "NEMBARCACION"]].fillna("")
    shown = tdf.head(max_rows)
    table = "\n".join(f"- {c:<12} | {n}" for c, n in zip(shown["CENSO"], shown["NEMBARCACION"]))
    if len(tdf) > max_rows:
        table += f"\n... ({len(tdf) - max_rows} filas más)"

    return (
        "[ALERTA] EMBARCACIONES DESCONOCIDAS\n"
        f"Log: {log_boat.name}\n"
        f"{table}"
    )

# =========================
# Construcción del cuerpo
# =========================
def cuerpo_error_critico(fecha_str: str, log_sp: Path, csv_sp: Path) -> str:
    log_txt = leer_log_txt(log_sp).strip()
    especies = leer_csv_col(csv_sp, col="NUEVA_ESPECIE")
    especies_txt = "\n".join(f"- {e}" for e in especies) if especies else "No hay especies nuevas listadas."
    return (
        header(fecha_str)
        + "[ERROR CRÍTICO] ESPECIES DESCONOCIDAS\n"
        f"Log: {log_sp.name}\n"
        f"{log_txt}\n\n"
        "Especies detectadas:\n"
        f"{especies_txt}\n"
    )

def cuerpo_alertas(
    fecha_str: str,
    log_boat: Path,
    csv_boats: Path,
    logs_cv: List[Path],
) -> str:
    partes: List[str] = []

    if csv_boats.exists():
        partes.append(tabla_embarcaciones(csv_boats, log_boat))

    # Sección única de CAJAS VERDES
    cajas_verdes = []
    for lg in logs_cv:
        if lg.exists():
            contenido = resumen_log(lg, max_lineas=10)
            cajas_verdes.append(f"{lg.name}\n{contenido}")
    if cajas_verdes:
        partes.append("[CAJAS VERDES]\n" + "\n\n".join(cajas_verdes))

    if partes:
        return header(fecha_str) + "\n\n---\n\n".join(partes) + footer()
    else:
        return header(fecha_str) + "✅ No se detectaron errores ni advertencias.\n" + footer()

# =========================
# Main (componer y enviar)
# =========================
def enviar_reporte(logs_dir: Path, fecha: date | None = None):
    validar_config()

    if fecha is None:
        fecha = date.today() - timedelta(days=1)
    fecha_str = fecha.strftime("%Y-%m-%d")

    # Rutas de logs
    log_sp = logs_dir / f"error_{fecha_str}.log"
    csv_sp = logs_dir / f"unknown_species_{fecha_str}.csv"
    log_boat = logs_dir / f"warning_{fecha_str}.log"
    csv_boats = logs_dir / f"unknown_boats_{fecha_str}.csv"
    logs_cv = [
        logs_dir / f"log_few_bips_{fecha_str}.csv",
        logs_dir / f"log_tracks_in_port_{fecha_str}.csv",
        logs_dir / f"log_tracks_inland_{fecha_str}.csv",
        logs_dir / f"salida_sin_venta_{fecha_str}.csv",
        logs_dir / f"venta_sin_salida_{fecha_str}.csv",
    ]

    hay_critico = log_sp.exists()

    # Email
    message = EmailMessage()
    subject_prefix = "[CRÍTICO] " if hay_critico else ""
    message["Subject"] = f"{subject_prefix}REMAR - Reporte de día {fecha_str}"
    message["From"] = EMAIL_SENDER
    message["To"] = ", ".join([e for e in EMAIL_RECEIVER.values() if e])
    message["Date"] = formatdate(localtime=True)

    if hay_critico:
        cuerpo = cuerpo_error_critico(fecha_str, log_sp, csv_sp)
    else:
        cuerpo = cuerpo_alertas(fecha_str, log_boat, csv_boats, logs_cv)

    message.set_content(cuerpo)

    # Envío
    context = ssl.create_default_context()
    with smtplib.SMTP_SSL(SMTP_SERVER, PORT, context=context) as server:
        server.login(EMAIL_SENDER, EMAIL_PASSWORD)
        server.send_message(message)

    print("Correo enviado.")

if __name__ == "__main__":
    # Ajusta la ruta a tus logs según tu estructura
    logs_base = Path("../../logs")
    enviar_reporte(logs_base, fecha=None)  # usa ayer por defecto
