"""
reconstruir_eventos_intensidad.py

Reconstruye una tabla de eventos a nivel de PROYECTO individual (no solo
"primer año de tratamiento" por municipio) a partir de los archivos por
fuente que sí siguen disponibles, para poder calcular intensidad del
tratamiento (número de proyectos activos por municipio-año).

Fuentes incorporadas:
  - Verra (101 proyectos): municipio ya asignado por geometría exacta;
    fechas de inicio y fin de periodo de acreditación (creditPeriodStartDate/
    creditPeriodEndDate), con 98/101 proyectos con fecha válida.
  - Gold Standard (28 proyectos, 12 con coordenadas no nulas): municipio
    reconstruido por geometría exacta (punto en polígono, con corrección de
    signo/intercambio de coordenadas) contra el shapefile real del DANE --
    ver asignar_goldstandard_espacial.R. Los 12 proyectos con coordenadas
    válidas obtuvieron municipio y fecha.
  - Cercarbono (234 proyectos, de los cuales una parte con municipio único
    verificado por texto): fechas directas en "Duration start"/"Duration end".
  - RENARE (281 registros, subconjunto con municipio único verificado por
    texto): el matching de municipio se RECONSTRUYÓ desde cero en
    reconstruir_renare_matching.py (el archivo de matching original
    disponible tenía la columna de municipio vacía); las fechas se
    recuperan desde el archivo crudo (renare_solicitudes_colombia.csv)
    parseando el JSON de la columna "actividades" (fecha_inicial_1).

Con las cuatro fuentes incorporadas, esta reconstrucción se acerca al
universo completo de municipios tratados usado en el resto de la tesis
(88-99 municipios), aunque puede no ser idéntica en el detalle -- el
matching de RENARE es una reconstrucción, no el cálculo original exacto.

Salida: eventos_intensidad.csv, con columnas:
  COD_DANE, fecha_inicio, fecha_fin, fuente, proyecto_id

USO
---
    python3 reconstruir_eventos_intensidad.py
"""

import json
import ast
from pathlib import Path
import pandas as pd

DATA_ROOT = Path(r"C:\Users\USUARIO\Documents\Maestria\Tesis\tesis_gfc\data")
RUTA_VERRA = DATA_ROOT / "interim/verra_platts_colombia_con_municipio.csv"
RUTA_CERCARBONO = DATA_ROOT / "interim/diagnostics/cercarbono_municipio_matching.csv"
RUTA_RENARE_MATCH = DATA_ROOT / "interim/diagnostics/renare_municipio_matching_reconstruido.csv"
RUTA_RENARE_RAW = DATA_ROOT / "interim/renare_solicitudes_colombia.csv"

eventos = []

# =============================================================================
# 1. Verra
# =============================================================================
verra = pd.read_csv(RUTA_VERRA)
verra["COD_DANE"] = verra["COD_DANE"].astype(str).str.zfill(5)
n_verra_sin_fecha = verra["creditPeriodStartDate"].isna().sum()
for _, row in verra.iterrows():
    if pd.isna(row["creditPeriodStartDate"]):
        continue
    eventos.append({
        "COD_DANE": row["COD_DANE"],
        "fecha_inicio": pd.to_datetime(row["creditPeriodStartDate"]).date(),
        "fecha_fin": pd.to_datetime(row["creditPeriodEndDate"]).date() if pd.notna(row["creditPeriodEndDate"]) else None,
        "fuente": "Verra",
        "proyecto_id": row.get("projectId", row.get("id")),
    })
print(f"Verra: {len(verra) - n_verra_sin_fecha} de {len(verra)} proyectos incorporados (con fecha válida)")

# =============================================================================
# 2. Cercarbono (solo los que tienen exactamente un municipio verificado)
# =============================================================================
cerc = pd.read_csv(RUTA_CERCARBONO)
cerc_validos = cerc[cerc["n_matches"] == 1].copy()
cerc_validos["cod_dane"] = cerc_validos["cod_dane"].apply(lambda x: str(int(float(x))).zfill(5))
n_cerc_sin_fecha = 0
for _, row in cerc_validos.iterrows():
    if pd.isna(row["Duration start"]):
        n_cerc_sin_fecha += 1
        continue
    eventos.append({
        "COD_DANE": row["cod_dane"],
        "fecha_inicio": pd.to_datetime(row["Duration start"], errors="coerce"),
        "fecha_fin": pd.to_datetime(row["Duration end"], errors="coerce") if pd.notna(row["Duration end"]) else None,
        "fuente": "Cercarbono",
        "proyecto_id": row.get("Project ID"),
    })
print(f"Cercarbono: {len(cerc_validos) - n_cerc_sin_fecha} de {len(cerc_validos)} proyectos con municipio único incorporados (con fecha válida)")

# =============================================================================
# 3. RENARE (municipio único verificado por texto + fecha extraída del JSON crudo)
# =============================================================================
renare_match = pd.read_csv(RUTA_RENARE_MATCH)
renare_validos = renare_match[renare_match["n_matches"] == 1].copy()
renare_raw = pd.read_csv(RUTA_RENARE_RAW)
renare_raw = renare_raw.set_index("_id")

n_renare_sin_fecha = 0
n_renare_incorporados = 0
for _, row in renare_validos.iterrows():
    id_ = row["_id"]
    if id_ not in renare_raw.index:
        continue
    actividades_raw = renare_raw.loc[id_, "actividades"]
    try:
        actividades = ast.literal_eval(actividades_raw) if isinstance(actividades_raw, str) else actividades_raw
    except (ValueError, SyntaxError):
        n_renare_sin_fecha += 1
        continue

    fecha_inicial = None
    if isinstance(actividades, list) and len(actividades) > 0:
        fecha_inicial = actividades[0].get("fecha_inicial_1")

    if not fecha_inicial:
        n_renare_sin_fecha += 1
        continue

    try:
        fecha_dt = pd.to_datetime(fecha_inicial, format="%d/%m/%Y", errors="coerce")
    except Exception:
        fecha_dt = pd.NaT
    if pd.isna(fecha_dt):
        n_renare_sin_fecha += 1
        continue

    eventos.append({
        "COD_DANE": str(int(float(row["cod_dane"]))).zfill(5),
        "fecha_inicio": fecha_dt,
        "fecha_fin": None,  # RENARE no reporta fecha de fin en el JSON disponible
        "fuente": "RENARE",
        "proyecto_id": id_,
    })
    n_renare_incorporados += 1

print(f"RENARE: {n_renare_incorporados} de {len(renare_validos)} proyectos con municipio único incorporados (con fecha válida)")
print(f"  (sin fecha extraíble: {n_renare_sin_fecha})")

# =============================================================================
# 4. Gold Standard (reconstruido desde el archivo crudo + union espacial
#    contra el shapefile real -- ver asignar_goldstandard_espacial.R)
# =============================================================================
RUTA_GOLDSTANDARD = "goldstandard_con_municipio_reconstruido.csv"
gs = pd.read_csv(RUTA_GOLDSTANDARD)
gs_validos = gs[gs["COD_DANE"].notna()].copy()
gs_validos["COD_DANE"] = gs_validos["COD_DANE"].apply(lambda x: str(int(float(x))).zfill(5))

n_gs_sin_fecha = 0
for _, row in gs_validos.iterrows():
    if pd.isna(row["crediting_period_start_date"]):
        n_gs_sin_fecha += 1
        continue
    eventos.append({
        "COD_DANE": row["COD_DANE"],
        "fecha_inicio": pd.to_datetime(row["crediting_period_start_date"], errors="coerce"),
        "fecha_fin": pd.to_datetime(row["crediting_period_end_date"], errors="coerce") if pd.notna(row["crediting_period_end_date"]) else None,
        "fuente": "Gold Standard",
        "proyecto_id": row.get("id"),
    })
print(f"Gold Standard: {len(gs_validos) - n_gs_sin_fecha} de {len(gs_validos)} proyectos con municipio asignado incorporados (con fecha válida)")

# =============================================================================
# Consolidar y guardar
# =============================================================================
tabla = pd.DataFrame(eventos)
tabla["fecha_inicio"] = pd.to_datetime(tabla["fecha_inicio"])
tabla["anio_inicio"] = tabla["fecha_inicio"].dt.year
tabla["fecha_fin"] = pd.to_datetime(tabla["fecha_fin"])
tabla["anio_fin"] = tabla["fecha_fin"].dt.year

tabla.to_csv(DATA_ROOT / "interim/eventos_intensidad.csv", index=False, encoding="utf-8-sig")

print(f"\nTotal de eventos reconstruidos: {len(tabla)}")
print(f"Municipios distintos con al menos un proyecto: {tabla['COD_DANE'].nunique()}")
print(f"Municipios con más de un proyecto (intensidad > 1 posible): {(tabla.groupby('COD_DANE').size() > 1).sum()}")
print("\nDistribución de proyectos por municipio (municipios con >1 proyecto):")
conteo = tabla.groupby("COD_DANE").size()
print(conteo[conteo > 1].sort_values(ascending=False))
print("\nGuardado: eventos_intensidad.csv")
