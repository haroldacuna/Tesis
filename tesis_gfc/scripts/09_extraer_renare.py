"""
09_extraer_renare.py

RENARE (Registro Nacional de Reducción de las Emisiones de GEI) es la fuente
oficial que la propuesta cita ("RENARE, Verra, Gold Standard") pero que hoy
NO está en el pipeline. Este script asume que RENARE se publica como dataset
Socrata en datos.gov.co, igual que la población DANE que ya usa
06_enrich_panel_external.py (obtener_poblacion_dane) — mismo patrón de URL
`https://www.datos.gov.co/resource/<resource_id>.json`.

ESTO ES UNA PLANTILLA, NO UN EXTRACTOR TERMINADO
--------------------------------------------------
No conozco el resource_id real de RENARE ni el nombre exacto de sus columnas.
Dos caminos para completarlo, en orden de preferencia:

  1. Corre primero 08_diagnostico_fuentes_carbono.py: busca "RENARE" en el
     catálogo de datos.gov.co y prueba URLs directas de MinAmbiente. Si
     encuentra un resource_id de Socrata, pásalo aquí con --resource-id.

  2. Si RENARE NO está en datos.gov.co (portal distinto, PDF, o requiere
     consulta manual en su sitio), este script no aplica tal cual: avísame
     qué encontraste (URL, si hay API, si es HTML con tabla, si es un Excel
     descargable) y te escribo el extractor específico para ese formato.

USO
---
Modo exploración (solo ver qué columnas trae, sin procesar nada):
    python 09_extraer_renare.py --resource-id XXXX-XXXX --explorar

Modo extracción completa:
    python 09_extraer_renare.py --resource-id XXXX-XXXX \
        --campo-municipio nombre_del_campo_municipio \
        --campo-fecha-inicio nombre_del_campo_fecha
"""

from __future__ import annotations

import argparse
import unicodedata
from pathlib import Path
from typing import Any

import pandas as pd
import requests

RENARE_CACHE = Path("data/interim/renare_projects_colombia.csv")
RENARE_BRIDGE = Path("data/raw/auxiliary/renare_project_municipios.csv")
DATOS_GOV_BASE = "https://www.datos.gov.co/resource/{resource_id}.json"
PAGE_SIZE = 1000


def _normalize_text(value: str | None) -> str:
    if not value:
        return ""
    txt = unicodedata.normalize("NFKD", str(value))
    txt = "".join(ch for ch in txt if not unicodedata.combining(ch))
    return txt.upper().strip()


def fetch_socrata_all_rows(resource_id: str, session: requests.Session) -> list[dict[str, Any]]:
    """Descarga todas las filas de un dataset Socrata, paginando con $limit/$offset."""
    url = DATOS_GOV_BASE.format(resource_id=resource_id)
    all_rows: list[dict[str, Any]] = []
    offset = 0
    while True:
        params = {"$limit": PAGE_SIZE, "$offset": offset}
        resp = session.get(url, params=params, timeout=30)
        resp.raise_for_status()
        page = resp.json()
        if not page:
            break
        all_rows.extend(page)
        print(f"  -> descargadas {len(all_rows)} filas hasta ahora (offset={offset})")
        if len(page) < PAGE_SIZE:
            break
        offset += PAGE_SIZE
    return all_rows


def explorar_schema(resource_id: str) -> None:
    """Descarga solo unas pocas filas y muestra las columnas disponibles,
    para decidir qué campo usar como municipio y cuál como fecha."""
    session = requests.Session()
    url = DATOS_GOV_BASE.format(resource_id=resource_id)
    resp = session.get(url, params={"$limit": 5}, timeout=30)
    resp.raise_for_status()
    rows = resp.json()

    if not rows:
        print("El dataset respondió pero no trajo filas con $limit=5. Revisa el resource_id.")
        return

    print(f"\nColumnas disponibles en el dataset {resource_id}:")
    for col in rows[0].keys():
        sample_values = [str(r.get(col, ""))[:40] for r in rows]
        print(f"  - {col}: ej. {sample_values[:3]}")

    print(
        "\nCon esta lista, decide qué columna identifica el municipio "
        "(nombre o código DANE) y cuál es la fecha de inicio/registro del "
        "proyecto, y vuelve a correr el script con --campo-municipio y "
        "--campo-fecha-inicio."
    )


def extraer_renare(
    resource_id: str,
    campo_municipio: str | None,
    campo_fecha_inicio: str | None,
    cache_file: Path = RENARE_CACHE,
) -> pd.DataFrame:
    if cache_file.exists():
        print(f"Usando caché existente: {cache_file}")
        return pd.read_csv(cache_file)

    session = requests.Session()
    print(f"Descargando RENARE (resource_id={resource_id}) desde datos.gov.co...")
    rows = fetch_socrata_all_rows(resource_id, session)

    if not rows:
        print("Advertencia: RENARE no devolvió filas. Revisa el resource_id o si el dataset requiere otros parámetros.")
        return pd.DataFrame()

    df = pd.DataFrame(rows)
    print(f"RENARE: {len(df)} filas descargadas, {len(df.columns)} columnas.")

    if campo_municipio and campo_municipio in df.columns:
        df["municipio_norm"] = df[campo_municipio].apply(_normalize_text)
    else:
        print(
            "Aviso: no se especificó --campo-municipio (o no existe en el dataset). "
            "El archivo se guarda igual, pero sin normalizar municipio todavía."
        )

    if campo_fecha_inicio and campo_fecha_inicio in df.columns:
        df["fecha_inicio_detectada"] = df[campo_fecha_inicio]
    else:
        print("Aviso: no se especificó --campo-fecha-inicio (o no existe en el dataset).")

    df["fuente"] = "renare"
    cache_file.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(cache_file, index=False, encoding="utf-8-sig")
    print(f"Guardado en: {cache_file}")
    return df


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Extractor RENARE (plantilla, pendiente de confirmar resource_id).")
    parser.add_argument("--resource-id", type=str, required=True, help="Resource ID de Socrata en datos.gov.co para RENARE.")
    parser.add_argument("--explorar", action="store_true", help="Solo mostrar columnas disponibles, no extraer todo.")
    parser.add_argument("--campo-municipio", type=str, default=None, help="Nombre de la columna con el municipio del proyecto.")
    parser.add_argument("--campo-fecha-inicio", type=str, default=None, help="Nombre de la columna con la fecha de inicio/registro.")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.explorar:
        explorar_schema(args.resource_id)
        return

    extraer_renare(
        resource_id=args.resource_id,
        campo_municipio=args.campo_municipio,
        campo_fecha_inicio=args.campo_fecha_inicio,
    )


if __name__ == "__main__":
    main()
