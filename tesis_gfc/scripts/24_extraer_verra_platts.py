"""
24_extraer_verra_platts.py

Verra migró su registro a la plataforma "Platts"/Markit (S&P Global). Esta
es la API real que usa esa plataforma, capturada por DevTools:

    POST https://prod-us.api.platts.com/ci-raas-prod/raas-report-api/es/public/project/publicReportPageSearch

A diferencia de Cercarbono/RENARE, la 'appkey' aquí parece ser una clave de
aplicación fija (no un token de sesión de usuario que expira en minutos),
así que debería ser más estable — pero si empieza a fallar con 401/403,
hay que recapturar por DevTools igual que las otras.

El filtro de país usa un ID interno de Platts, no el código ISO: para
Colombia es "100000000000011" (el valor que ya capturaste). El header
'standardid'/'standardacronym' fija el estándar a VCS (Verified Carbon
Standard) — el principal de Verra. Si existieran otros estándares Verra
relevantes (CCB, SD VISta), necesitarían su propio standardid; no los
probamos aquí.

USO
---
    python 24_extraer_verra_platts.py --explorar     # solo ver estructura de 1 página
    python 24_extraer_verra_platts.py                # extracción completa paginada
"""

from __future__ import annotations

import argparse
import json
import time
import uuid
from pathlib import Path
from typing import Any

import pandas as pd
import requests

URL = "https://prod-us.api.platts.com/ci-raas-prod/raas-report-api/es/public/project/publicReportPageSearch"
COLOMBIA_COUNTRY_ID = "100000000000011"

CACHE = Path("data/interim/verra_platts_colombia.csv")
DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)

HEADERS_BASE = {
    "accept": "application/json",
    "appkey": "wOKHFGuxKApQaujPSKgF",
    "application": "Markit",
    "content-type": "application/json",
    "language": "en",
    "origin": "https://registry.verra.org",
    "referer": "https://registry.verra.org/",
    "registry": "VERRA",
    "standardacronym": "VCS",
    "standardid": "150000000000001",
    "x-xsrf-token": "t20",
    "user-agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
        "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"
    ),
}


def _headers() -> dict[str, str]:
    h = dict(HEADERS_BASE)
    h["x-request-id"] = str(uuid.uuid4())
    return h


def _body(start: int, limit: int = 50) -> dict[str, Any]:
    return {
        "searchFilter": {
            "pagination": {
                "start": start,
                "limit": limit,
                "sortOptions": [{"sort": "projectName.keyword", "dir": "ASC"}],
            },
            "filterModel": {
                "countryId": {
                    "columnFilters": [
                        {"filterType": "Text", "type": "in", "filter": COLOMBIA_COUNTRY_ID}
                    ]
                }
            },
        }
    }


def explorar() -> None:
    print("Consultando página 1 (start=0, limit=5) para ver la estructura...")
    resp = requests.post(URL, headers=_headers(), json=_body(0, limit=5), timeout=30)
    print(f"Status: {resp.status_code}")

    raw_path = DIAG_DIR / "verra_platts_explorar_raw.json"
    raw_path.write_text(resp.text[:20000], encoding="utf-8", errors="replace")
    print(f"Respuesta cruda guardada en: {raw_path}")

    if resp.status_code != 200:
        print(f"\nNo respondió 200. Cuerpo: {resp.text[:500]}")
        print(
            "\nSi es 401/403: el x-xsrf-token ('t20') o la appkey probablemente ya "
            "caducaron — recaptura por DevTools igual que las veces anteriores."
        )
        return

    try:
        data = resp.json()
    except ValueError:
        print(f"\nLa respuesta no es JSON válido. Primeros 500 chars: {resp.text[:500]}")
        return

    print(f"\nClaves de nivel superior: {list(data.keys()) if isinstance(data, dict) else type(data)}")

    items = None
    total = None
    if isinstance(data, dict):
        for key in ["entities", "data", "content", "results", "items", "records", "projects"]:
            if key in data and isinstance(data[key], list):
                items = data[key]
                print(f"Lista de proyectos en la clave '{key}': {len(items)} elementos (de esta página)")
                break
        for key in ["totalEntities", "total", "totalElements", "totalCount", "count", "totalRecords"]:
            if key in data:
                total = data[key]
                print(f"Campo de total: '{key}' = {total}")
                break
        if "totalPages" in data:
            print(f"totalPages = {data['totalPages']}")
        # A veces el total está anidado, ej. data['pagination']['total']
        if total is None and "pagination" in data and isinstance(data["pagination"], dict):
            for key in ["total", "totalElements", "totalCount"]:
                if key in data["pagination"]:
                    print(f"Campo de total (anidado en 'pagination'): '{key}' = {data['pagination'][key]}")

    if items:
        print(f"\nCampos del primer proyecto: {list(items[0].keys())}")
        print(f"Ejemplo completo:\n{json.dumps(items[0], indent=2, ensure_ascii=False)[:2500]}")

        campos_ubicacion = [
            k for k in items[0].keys()
            if any(w in k.lower() for w in ["state", "province", "region", "municip", "location", "city"])
        ]
        print(f"\nCampos con nombre relacionado a ubicación: {campos_ubicacion}")

    print(
        "\nPégame esta salida (o el JSON completo si algo no calza) para ajustar la "
        "extracción completa."
    )


def _post_con_reintentos(body: dict[str, Any], intentos: int = 4, espera_seg: float = 5.0) -> requests.Response:
    """POST con reintentos para aguantar cortes de DNS/red transitorios
    (ya vimos que este host falla intermitentemente en esta red)."""
    ultimo_error: Exception | None = None
    for intento in range(1, intentos + 1):
        try:
            return requests.post(URL, headers=_headers(), json=body, timeout=30)
        except requests.exceptions.RequestException as exc:
            ultimo_error = exc
            print(f"  Intento {intento}/{intentos} falló ({type(exc).__name__}). Reintentando en {espera_seg:.0f}s...")
            time.sleep(espera_seg)
    raise ultimo_error  # type: ignore[misc]


def _guardar_progreso_parcial(items: list[dict[str, Any]]) -> None:
    """Guarda lo acumulado hasta ahora en un archivo aparte, para no perder
    todo el progreso si la descarga se corta a mitad de camino."""
    if not items:
        return
    df = pd.json_normalize(items)
    parcial_path = DIAG_DIR / "verra_platts_progreso_parcial.csv"
    df.to_csv(parcial_path, index=False, encoding="utf-8-sig")


def extraer_completo(limit: int = 50, max_paginas: int = 100) -> pd.DataFrame:
    if CACHE.exists():
        print(f"Usando caché existente: {CACHE}")
        return pd.read_csv(CACHE)

    all_items: list[dict[str, Any]] = []
    start = 0
    total_esperado: int | None = None
    pagina = 0

    while pagina < max_paginas:
        print(f"start={start} (página {pagina + 1})...")
        try:
            resp = _post_con_reintentos(_body(start, limit=limit))
        except requests.exceptions.RequestException as exc:
            print(f"\n  Se agotaron los reintentos ({exc}). Guardando lo acumulado hasta ahora...")
            _guardar_progreso_parcial(all_items)
            print(
                f"  Progreso parcial ({len(all_items)} proyectos) guardado en "
                f"{DIAG_DIR / 'verra_platts_progreso_parcial.csv'}. Corre el script de nuevo "
                f"más tarde para reintentar desde cero (la caché final solo se escribe si "
                f"se completan todas las páginas)."
            )
            break

        if resp.status_code != 200:
            print(f"Status {resp.status_code} en start={start}. Cuerpo: {resp.text[:300]}")
            _guardar_progreso_parcial(all_items)
            break

        try:
            data = resp.json()
        except ValueError:
            print(f"Respuesta no-JSON en start={start}: {resp.text[:300]}")
            _guardar_progreso_parcial(all_items)
            break

        items = []
        if isinstance(data, dict):
            if total_esperado is None:
                for key in ["totalEntities", "total", "totalElements", "totalCount", "count", "totalRecords"]:
                    if key in data:
                        total_esperado = data[key]
                        print(f"  Total reportado: {total_esperado}")
                        break
            for key in ["entities", "data", "content", "results", "items", "records", "projects"]:
                if key in data and isinstance(data[key], list):
                    items = data[key]
                    break

        if not items:
            print("  Página vacía, terminando.")
            break

        all_items.extend(items)
        print(f"  {len(items)} proyectos (acumulado: {len(all_items)})")
        _guardar_progreso_parcial(all_items)  # checkpoint tras cada página exitosa

        if total_esperado is not None and len(all_items) >= total_esperado:
            break
        if len(items) < limit:
            break

        start += limit
        pagina += 1

    if not all_items:
        print("No se obtuvo ningún proyecto. Corre con --explorar primero.")
        return pd.DataFrame()

    df = pd.json_normalize(all_items)
    df["fuente"] = "verra_platts"
    CACHE.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(CACHE, index=False, encoding="utf-8-sig")
    print(f"\nGuardado: {len(df)} proyectos en {CACHE}")
    print(f"Columnas: {list(df.columns)}")
    return df


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser()
    p.add_argument("--explorar", action="store_true")
    p.add_argument("--limit", type=int, default=50)
    return p.parse_args()


def main() -> None:
    args = parse_args()
    if args.explorar:
        explorar()
    else:
        extraer_completo(limit=args.limit)


if __name__ == "__main__":
    main()
