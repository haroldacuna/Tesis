"""
18_explorar_cercarbono.py

El cURL que encontraste revela el dominio real del backend de EcoRegistry/
Cercarbono: api-front.ecoregistry.io (no www.ecoregistry.io, que es solo la
SPA — por eso el escaneo de la ronda 2 no encontró contenido renderizado).

El endpoint específico que capturaste:
    GET https://api-front.ecoregistry.io/platform/projectReport/proyectsIssued/cercarbono-co2

...es un reporte de "proyectos emitidos" (créditos ya emitidos) para el
programa 'cercarbono-co2' específicamente. Puede que no sea la lista
completa de proyectos (solo los que ya emitieron créditos), y puede haber
otros "programas" además de 'cercarbono-co2' (ej. otros gases/metodologías).

Este script:
  1. Prueba ese endpoint tal cual.
  2. Prueba variantes plausibles del mismo patrón (otros programas, un
     listado general de proyectos en vez del reporte de emisiones).
  3. Reporta la estructura completa de lo que responda, para decidir el
     siguiente paso.

No pude probarlo en vivo (ecoregistry.io no está en mi lista blanca de red).

USO
---
    python 18_explorar_cercarbono.py
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import requests

DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)

BASE = "https://api-front.ecoregistry.io"

HEADERS = {
    "accept": "application/json, text/plain, */*",
    "origin": "https://registry.cercarbono.com",
    "referer": "https://registry.cercarbono.com/",
    "platform": "ecoregistry",
    "user-agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
        "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"
    ),
}

CANDIDATOS = [
    "/platform/projectReport/proyectsIssued/cercarbono-co2",
    "/platform/projectReport/proyectsIssued/cercarbono",
    "/platform/projects",
    "/platform/projects?country=CO",
    "/platform/project",
    "/platform/projectReport/projects",
]


def _probe(path: str) -> None:
    url = BASE + path
    print(f"\n--- {url} ---")
    try:
        resp = requests.get(url, headers=HEADERS, timeout=20)
    except Exception as exc:
        print(f"ERROR: {exc}")
        return

    print(f"Status: {resp.status_code}  Content-Type: {resp.headers.get('Content-Type')}")
    safe_name = path.strip("/").replace("/", "_").replace("?", "_")
    out_file = DIAG_DIR / f"cercarbono_{safe_name}.json"

    try:
        data = resp.json()
        out_file.write_text(json.dumps(data, indent=2, ensure_ascii=False)[:20000], encoding="utf-8")
        print(f"JSON guardado en: {out_file}")

        if isinstance(data, list):
            print(f"Lista de {len(data)} elementos.")
            if data:
                print(f"Campos del primer elemento: {list(data[0].keys()) if isinstance(data[0], dict) else type(data[0])}")
                print(json.dumps(data[0], indent=2, ensure_ascii=False)[:1200])
        elif isinstance(data, dict):
            print(f"Claves de nivel superior: {list(data.keys())}")
            for key in ["data", "results", "items", "content", "projects"]:
                if key in data and isinstance(data[key], list):
                    items = data[key]
                    print(f"  Lista en '{key}': {len(items)} elementos")
                    if items:
                        print(f"  Campos: {list(items[0].keys()) if isinstance(items[0], dict) else type(items[0])}")
                        print(json.dumps(items[0], indent=2, ensure_ascii=False)[:1200])
                    break
    except ValueError:
        out_file_txt = DIAG_DIR / f"cercarbono_{safe_name}.txt"
        out_file_txt.write_text(resp.text[:5000], encoding="utf-8", errors="replace")
        print(f"No es JSON. Primeros 300 chars: {resp.text[:300]}")
        print(f"(guardado en {out_file_txt})")


def main() -> None:
    for path in CANDIDATOS:
        _probe(path)

    print(
        "\n\nSi ninguno de estos trajo una lista completa de proyectos: en DevTools, "
        "sobre registry.cercarbono.com, busca específicamente la petición que se dispara "
        "cuando cargas la página de LISTADO de proyectos (no el ícono, no el reporte de "
        "emisiones) — probablemente algo como /platform/projects o /platform/project/search. "
        "Cópiala igual que la anterior (click derecho -> Copy as cURL) y pégamela."
    )


if __name__ == "__main__":
    main()
