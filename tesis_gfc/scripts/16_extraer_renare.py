"""
16_extraer_renare.py

Extractor real de RENARE, usando el endpoint POST que encontraste con DevTools:

    POST https://gestion-solicitudes-api.minambiente.gov.co/pub/api/gestion/solicitudes
    body: {"filtros": [...], "pagina": 1, "limite": N, "ordenar": [...]}

Tu captura filtraba por tipo_iniciativa == "Programa REDD+" (probablemente
porque estabas probando ese filtro específico en la UI). El campo
'tipo_iniciativa' sugiere que hay más de un valor posible (ej. "Proyecto
REDD+" además de "Programa REDD+", y probablemente otros tipos no-REDD).
Este script primero corre SIN filtro (pagina 1, limite alto) para:
  a) confirmar que el endpoint acepta filtros=[] (o hay que omitir la clave)
  b) ver cuántos registros hay en total
  c) ver qué valores de tipo_iniciativa existen realmente
  d) ver la estructura completa de un registro (para saber qué campo trae
     el municipio/ubicación)

No pude probarlo en vivo (minambiente.gov.co no está en mi lista blanca de
red), así que corre primero con --explorar.

USO
---
    python 16_extraer_renare.py --explorar
    python 16_extraer_renare.py                  # extracción completa paginada
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any

import pandas as pd
import requests

URL = "https://gestion-solicitudes-api.minambiente.gov.co/pub/api/gestion/solicitudes"
CACHE = Path("data/interim/renare_solicitudes_colombia.csv")
DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)

HEADERS = {
    "accept": "application/json, text/plain, */*",
    "accept-language": "es-ES,es;q=0.9,en;q=0.8",
    "content-type": "application/json",
    "origin": "https://renare.minambiente.gov.co",
    "referer": "https://renare.minambiente.gov.co/",
    "user-agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
        "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"
    ),
}


def _post(body: dict[str, Any]) -> dict[str, Any] | list[Any]:
    resp = requests.post(URL, headers=HEADERS, json=body, timeout=30)
    resp.raise_for_status()
    return resp.json()


def explorar() -> None:
    print("Probando SIN filtro (filtros=[]) para ver el universo completo...")
    body = {"filtros": [], "pagina": 1, "limite": 20, "ordenar": []}
    try:
        data = _post(body)
    except requests.exceptions.HTTPError as exc:
        print(f"Falló sin filtro ({exc}). Reintentando con la clave 'filtros' omitida...")
        body2 = {"pagina": 1, "limite": 20, "ordenar": []}
        data = _post(body2)

    raw_path = DIAG_DIR / "renare_explorar_raw.json"
    raw_path.write_text(json.dumps(data, indent=2, ensure_ascii=False)[:20000], encoding="utf-8")
    print(f"Respuesta cruda guardada en: {raw_path}")

    if isinstance(data, dict):
        print(f"\nClaves de nivel superior: {list(data.keys())}")
        for key in ["total", "totalRegistros", "totalElements", "count", "totalPaginas"]:
            if key in data:
                print(f"  {key}: {data[key]}")

        items = None
        for key in ["data", "resultados", "items", "content", "registros", "solicitudes"]:
            if key in data and isinstance(data[key], list):
                items = data[key]
                print(f"\nLista de registros encontrada en la clave '{key}': {len(items)} elementos")
                break

        if items:
            print(f"\nCampos del primer registro: {list(items[0].keys())}")
            print(f"Ejemplo completo:\n{json.dumps(items[0], indent=2, ensure_ascii=False)[:2500]}")

            # Buscar valores únicos de tipo_iniciativa para saber qué tipos existen.
            tipos = set()
            for it in items:
                obj = it.get("objeto_negocio", {}) if isinstance(it.get("objeto_negocio"), dict) else {}
                t = obj.get("tipo_iniciativa")
                if t:
                    tipos.add(t)
            if tipos:
                print(f"\nValores de tipo_iniciativa vistos en esta muestra: {tipos}")

            # Buscar cualquier campo que huela a ubicación.
            campos_ubicacion = [
                k for k in items[0].keys()
                if any(w in k.lower() for w in ["municipio", "departamento", "ubicacion", "region", "lugar"])
            ]
            print(f"\nCampos con nombre relacionado a ubicación: {campos_ubicacion}")
            obj_negocio = items[0].get("objeto_negocio", {})
            if isinstance(obj_negocio, dict):
                campos_ubicacion_nested = [
                    k for k in obj_negocio.keys()
                    if any(w in k.lower() for w in ["municipio", "departamento", "ubicacion", "region", "lugar"])
                ]
                print(f"Campos con nombre relacionado a ubicación dentro de 'objeto_negocio': {campos_ubicacion_nested}")

    elif isinstance(data, list):
        print(f"\nLa respuesta es una lista directa de {len(data)} elementos.")
        if data:
            print(f"Campos: {list(data[0].keys())}")

    print(
        "\nCon esto ajusto la extracción completa: pégame la sección de 'Campos del primer "
        "registro' de la consola (o el archivo JSON) si algo no calza con lo que espero."
    )


def extraer_completo(limite: int = 100, max_paginas: int = 200) -> pd.DataFrame:
    if CACHE.exists():
        print(f"Usando caché existente: {CACHE}")
        return pd.read_csv(CACHE)

    all_rows: list[dict[str, Any]] = []
    pagina = 1
    total_esperado: int | None = None

    while pagina <= max_paginas:
        body = {"filtros": [], "pagina": pagina, "limite": limite, "ordenar": []}
        print(f"Página {pagina}...")
        try:
            data = _post(body)
        except requests.exceptions.HTTPError:
            body = {"pagina": pagina, "limite": limite, "ordenar": []}
            data = _post(body)

        items: list[dict[str, Any]] = []
        if isinstance(data, dict):
            if total_esperado is None:
                for key in ["total", "totalRegistros", "totalElements", "count"]:
                    if key in data:
                        total_esperado = data[key]
                        print(f"  Total reportado: {total_esperado}")
                        break
            for key in ["data", "resultados", "items", "content", "registros", "solicitudes"]:
                if key in data and isinstance(data[key], list):
                    items = data[key]
                    break
        elif isinstance(data, list):
            items = data

        if not items:
            print("  Página vacía, terminando.")
            break

        all_rows.extend(items)
        print(f"  {len(items)} registros (acumulado: {len(all_rows)})")

        if total_esperado is not None and len(all_rows) >= total_esperado:
            break
        if len(items) < limite:
            break
        pagina += 1

    if not all_rows:
        print("No se obtuvo ningún registro. Corre con --explorar primero.")
        return pd.DataFrame()

    df = pd.json_normalize(all_rows)
    df["fuente"] = "renare"
    CACHE.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(CACHE, index=False, encoding="utf-8-sig")
    print(f"\nGuardado: {len(df)} registros en {CACHE}")
    print(f"Columnas (aplanadas): {list(df.columns)}")
    return df


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser()
    p.add_argument("--explorar", action="store_true")
    p.add_argument("--limite", type=int, default=100)
    return p.parse_args()


def main() -> None:
    args = parse_args()
    if args.explorar:
        explorar()
    else:
        extraer_completo(limite=args.limite)


if __name__ == "__main__":
    main()
