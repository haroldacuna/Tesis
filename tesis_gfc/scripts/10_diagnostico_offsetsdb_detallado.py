"""
10_diagnostico_offsetsdb_detallado.py

OffsetsDB (CarbonPlan) agrega 7 registros en un solo dataset: ACR, ART,
Climate Action Reserve, Cercarbono, Gold Standard, Isometric y Verra.
Si esta fuente funciona bien, potencialmente resuelve varias de las columnas
en cero de una sola vez (incluyendo Cercarbono, el registro
colombiano/latinoamericano, que ni siquiera estaba contemplado en el
pipeline original).

Confirmé en la documentación oficial (github.com/carbonplan/offsets-db-data)
que la URL que ya usa 06_enrich_panel_external.py sigue siendo la vigente:
    https://carbonplan-offsets-db.s3.us-west-2.amazonaws.com/production/latest/offsets-db.csv.zip

Es decir: la URL no es el problema. Este script agrega diagnóstico detallado
en cada paso (nombres reales de archivos dentro del zip, columnas, valores
de país, conteos en cada filtro) para encontrar dónde se pierde Colombia.

USO
---
    python 10_diagnostico_offsetsdb_detallado.py

Salida: imprime en consola y guarda un resumen en
data/interim/diagnostics/offsetsdb_diagnostico_detallado.txt
"""

from __future__ import annotations

import io
import zipfile
from pathlib import Path

import pandas as pd
import requests

OFFSETSDB_URL = "https://carbonplan-offsets-db.s3.us-west-2.amazonaws.com/production/latest/offsets-db.csv.zip"
DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)
OUT_REPORT = DIAG_DIR / "offsetsdb_diagnostico_detallado.txt"

lines: list[str] = []


def _log(msg: str = "") -> None:
    print(msg)
    lines.append(msg)


def main() -> None:
    _log(f"Descargando {OFFSETSDB_URL} ...")
    try:
        resp = requests.get(OFFSETSDB_URL, timeout=180)
        resp.raise_for_status()
    except Exception as exc:
        _log(f"ERROR al descargar: {exc}")
        _log(
            "Si esto falla por timeout: el archivo puede pesar varios cientos de MB, "
            "prueba subir el timeout o revisa tu conexión/firewall corporativo."
        )
        OUT_REPORT.write_text("\n".join(lines), encoding="utf-8")
        return

    _log(f"Descarga OK: {len(resp.content) / 1e6:.1f} MB, status={resp.status_code}")

    try:
        zf = zipfile.ZipFile(io.BytesIO(resp.content))
    except Exception as exc:
        _log(f"ERROR: el contenido descargado no es un zip válido ({exc}).")
        _log(f"Primeros 300 bytes de la respuesta: {resp.content[:300]!r}")
        OUT_REPORT.write_text("\n".join(lines), encoding="utf-8")
        return

    _log(f"\nArchivos dentro del zip ({len(zf.namelist())}):")
    for name in zf.namelist():
        info = zf.getinfo(name)
        _log(f"  - {name}  ({info.file_size / 1e6:.2f} MB)")

    # El pipeline original busca archivos cuyo nombre contenga "project" o "credit".
    # Verificamos explícitamente si ese match sigue siendo válido.
    matches_project = [n for n in zf.namelist() if "project" in n.lower() and n.endswith(".csv")]
    matches_credit = [n for n in zf.namelist() if "credit" in n.lower() and n.endswith(".csv")]
    _log(f"\nArchivos que matchean 'project' + .csv: {matches_project}")
    _log(f"Archivos que matchean 'credit' + .csv: {matches_credit}")

    if not matches_project:
        _log(
            "\n*** AQUÍ ESTÁ EL PROBLEMA (probablemente) ***\n"
            "El script original espera un CSV con 'project' en el nombre. "
            "Si la lista de arriba no tiene ninguno así, por eso _load_csv_from_zip "
            "lanza FileNotFoundError silenciosamente capturado como excepción genérica "
            "y la función devuelve un DataFrame vacío. Usa el nombre real de arriba."
        )
        OUT_REPORT.write_text("\n".join(lines), encoding="utf-8")
        return

    projects_name = matches_project[0]
    _log(f"\nUsando: {projects_name}")
    with zf.open(projects_name) as f:
        projects = pd.read_csv(f, low_memory=False)

    _log(f"Shape del CSV de proyectos: {projects.shape}")
    _log(f"Columnas: {list(projects.columns)}")

    # Buscar columna de país con cualquier variante de nombre.
    country_candidates = [c for c in projects.columns if "country" in c.lower()]
    _log(f"\nColumnas candidatas a 'país': {country_candidates}")

    if not country_candidates:
        _log(
            "*** No hay ninguna columna con 'country' en el nombre. "
            "Revisa la lista completa de columnas de arriba para encontrar el campo correcto "
            "(podría llamarse 'location', 'geography', etc.) y ajusta _find_column en el "
            "pipeline con ese nombre. ***"
        )
    else:
        for col in country_candidates:
            _log(f"\nValores únicos en '{col}' (primeros 30):")
            vals = projects[col].astype(str).value_counts().head(30)
            for val, count in vals.items():
                _log(f"    {val!r}: {count}")

            # ¿Cuántas filas matchean Colombia con el filtro actual del pipeline
            # (contains 'colombia' case-insensitive, o == 'CO')?
            mask_contains = projects[col].astype(str).str.contains("colombia", case=False, na=False)
            mask_co_exact = projects[col].astype(str).str.upper().str.strip() == "CO"
            _log(f"    Filas que contienen 'colombia' (case-insensitive): {mask_contains.sum()}")
            _log(f"    Filas que son exactamente 'CO': {mask_co_exact.sum()}")

    # Si hay columna de protocolo/registro, desglosar Colombia por registro de origen.
    registry_candidates = [
        c for c in projects.columns
        if any(k in c.lower() for k in ["protocol", "registry", "program"])
    ]
    if registry_candidates and country_candidates:
        best_country_col = country_candidates[0]
        mask_co = (
            projects[best_country_col].astype(str).str.contains("colombia", case=False, na=False)
            | (projects[best_country_col].astype(str).str.upper().str.strip() == "CO")
        )
        _log(f"\nProyectos Colombia por registro de origen (columna '{registry_candidates[0]}'):")
        if mask_co.sum() > 0:
            _log(str(projects.loc[mask_co, registry_candidates[0]].value_counts()))
        else:
            _log("  (0 proyectos Colombia detectados con el filtro actual)")

    OUT_REPORT.write_text("\n".join(lines), encoding="utf-8")
    _log(f"\n\nReporte guardado en: {OUT_REPORT}")
    _log("Pégame este reporte completo (o al menos las secciones marcadas con ***) para ajustar el pipeline.")


if __name__ == "__main__":
    main()
