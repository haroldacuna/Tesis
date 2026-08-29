"""
13_extraer_goldstandard.py

Extractor real de Gold Standard, usando el endpoint público que encontraste
con DevTools (confirmado que existe y responde a countries=CO):

    https://public-api.goldstandard.org/projects?query=&page=1&size=25&sortColumn=&sortDirection=&countries=CO

Nota: registry.goldstandard.org/projects?countries=CO (la URL que ve el
navegador) es solo la SPA; public-api.goldstandard.org es el backend real
que consulta esa SPA. No requiere autenticación para country=CO (a diferencia
de api.goldstandard.org, que sí la pedía).

No pude probar este endpoint en vivo desde mi entorno (no tengo salida de
red hacia goldstandard.org), así que este script trae diagnóstico detallado
de la primera página (estructura del JSON, nombres de campos) antes de
intentar traer todo, para poder ajustar rápido si el schema no es el que
supongo.

USO
---
    python 13_extraer_goldstandard.py --explorar        # solo ver 1 página y su estructura
    python 13_extraer_goldstandard.py                   # extracción completa con paginación
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any

import pandas as pd
import requests

BASE_URL = "https://public-api.goldstandard.org/projects"
CACHE = Path("data/interim/goldstandard_projects_colombia.csv")
DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
        "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"
    ),
    "Accept": "application/json",
    "Referer": "https://registry.goldstandard.org/",
    "Origin": "https://registry.goldstandard.org",
}


def _fetch_page(page: int, size: int = 25) -> dict[str, Any] | list[Any]:
    params = {
        "query": "",
        "page": page,
        "size": size,
        "sortColumn": "",
        "sortDirection": "",
        "countries": "CO",
    }
    resp = requests.get(BASE_URL, params=params, headers=HEADERS, timeout=30)
    resp.raise_for_status()
    return resp.json()


def explorar() -> None:
    print("Consultando página 1 para inspeccionar la estructura del JSON...")
    data = _fetch_page(1)

    raw_path = DIAG_DIR / "goldstandard_page1_raw.json"
    raw_path.write_text(json.dumps(data, indent=2, ensure_ascii=False)[:20000], encoding="utf-8")
    print(f"JSON crudo guardado en: {raw_path}")

    if isinstance(data, list):
        print(f"\nLa respuesta es una lista directa de {len(data)} elementos.")
        if data:
            print(f"Campos del primer elemento: {list(data[0].keys())}")
            print(f"Ejemplo:\n{json.dumps(data[0], indent=2, ensure_ascii=False)[:1000]}")
    elif isinstance(data, dict):
        print(f"\nLa respuesta es un objeto con estas claves de nivel superior: {list(data.keys())}")
        # buscar la lista de proyectos dentro de claves comunes
        for key in ["content", "results", "items", "data", "projects"]:
            if key in data and isinstance(data[key], list):
                items = data[key]
                print(f"\nLista de proyectos encontrada en la clave '{key}': {len(items)} elementos")
                if items:
                    print(f"Campos del primer elemento: {list(items[0].keys())}")
                    print(f"Ejemplo:\n{json.dumps(items[0], indent=2, ensure_ascii=False)[:1000]}")
                break
        # buscar campos de paginación total
        for key in ["totalElements", "total", "totalCount", "count", "totalPages"]:
            if key in data:
                print(f"\nCampo de paginación '{key}': {data[key]}")

    print(
        "\nCon esto ajusto la función de extracción completa: dime qué clave contiene "
        "la lista de proyectos y cuál el conteo total (o pégame el contenido de "
        f"{raw_path} directamente)."
    )


def _extraer_lista_proyectos(data: dict[str, Any] | list[Any]) -> list[dict[str, Any]]:
    """Intenta ubicar automáticamente la lista de proyectos dentro de la respuesta,
    sin importar cuál de las formas comunes de paginación use el backend."""
    if isinstance(data, list):
        return data
    if isinstance(data, dict):
        for key in ["content", "results", "items", "data", "projects"]:
            if key in data and isinstance(data[key], list):
                return data[key]
    return []


def _detectar_total(data: dict[str, Any] | list[Any], size: int) -> int | None:
    if isinstance(data, dict):
        for key in ["totalElements", "total", "totalCount", "count"]:
            if key in data and isinstance(data[key], int):
                return data[key]
    return None


def extraer_completo(size: int = 25, max_paginas: int = 200) -> pd.DataFrame:
    if CACHE.exists():
        print(f"Usando caché existente: {CACHE}")
        return pd.read_csv(CACHE)

    all_projects: list[dict[str, Any]] = []
    page = 1
    total_esperado: int | None = None

    while page <= max_paginas:
        print(f"Página {page}...")
        data = _fetch_page(page, size=size)
        items = _extraer_lista_proyectos(data)

        if total_esperado is None:
            total_esperado = _detectar_total(data, size)
            if total_esperado is not None:
                print(f"  Total de proyectos reportado por la API: {total_esperado}")

        if not items:
            print("  Página vacía, terminando paginación.")
            break

        all_projects.extend(items)
        print(f"  {len(items)} proyectos en esta página (acumulado: {len(all_projects)})")

        if total_esperado is not None and len(all_projects) >= total_esperado:
            break
        if len(items) < size:
            # última página parcial
            break
        page += 1

    if not all_projects:
        print("Advertencia: no se obtuvo ningún proyecto. Corre con --explorar para ver qué está devolviendo la API.")
        return pd.DataFrame()

    df = pd.DataFrame(all_projects)
    df["fuente"] = "goldstandard"
    CACHE.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(CACHE, index=False, encoding="utf-8-sig")
    print(f"\nGuardado: {len(df)} proyectos de Gold Standard Colombia en {CACHE}")
    print(f"Columnas: {list(df.columns)}")
    return df


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser()
    p.add_argument("--explorar", action="store_true", help="Solo inspeccionar la estructura de la página 1.")
    p.add_argument("--size", type=int, default=25, help="Tamaño de página.")
    return p.parse_args()


def main() -> None:
    args = parse_args()
    if args.explorar:
        explorar()
    else:
        extraer_completo(size=args.size)


if __name__ == "__main__":
    main()
