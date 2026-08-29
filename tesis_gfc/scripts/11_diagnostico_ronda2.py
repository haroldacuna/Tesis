"""
11_diagnostico_ronda2.py

Segunda ronda de sondeo, ya con los dominios reales encontrados por búsqueda
web (los de la ronda 1 estaban mal adivinados):

  - EcoRegistry: es www.ecoregistry.io (con www), no ecoregistry.io a secas.
    Confirmado que /projects/<id> existe con contenido real (ej. /projects/8).
    Este script escanea un rango de IDs, igual que ya hace el pipeline con
    Verra (_fetch_verra_from_public_details), y registra si el HTML trae el
    contenido real o solo el cascarón vacío de la SPA.

  - ACR: se movieron de acr2.apx.com a acrcarbon.org. Sondeamos su página de
    reportes públicos.

Climate Action Data Trust, Gold Standard y RENARE NO están aquí: son SPAs
(JavaScript) cuyo HTML crudo no trae datos — para esas no hay atajo de
adivinar URLs, hay que mirar qué API llama el navegador realmente (ver
instrucciones que te doy aparte, con DevTools).

USO
---
    python 11_diagnostico_ronda2.py [--eco-id-max 30]
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Any

import requests

DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)

HEADERS_BROWSER = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
        "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"
    ),
    "Accept": "text/html,application/json,*/*",
}
TIMEOUT = 20

lines: list[str] = []


def _log(msg: str = "") -> None:
    print(msg)
    lines.append(msg)


def _probe(name: str, url: str, **kwargs) -> dict[str, Any]:
    result: dict[str, Any] = {"name": name, "url": url, "status": None, "text": "", "error": None}
    try:
        kwargs.setdefault("headers", HEADERS_BROWSER)
        kwargs.setdefault("timeout", TIMEOUT)
        resp = requests.get(url, **kwargs)
        result["status"] = resp.status_code
        result["text"] = resp.text or ""
    except Exception as exc:
        result["error"] = str(exc)
    return result


# =============================================================================
# EcoRegistry — escanear IDs de proyecto con el dominio correcto (www.)
# =============================================================================

def _es_contenido_real_ecoregistry(html: str) -> bool:
    """Heurística simple: si el HTML trae texto real de proyecto (no solo el
    cascarón de React con <div id='root'></div> vacío), asumimos que es SSR
    y lo podemos parsear con requests normal, sin necesidad de un navegador
    headless."""
    lower = html.lower()
    # señales de que SÍ hay contenido renderizado en el servidor
    señales_positivas = ["hectáreas", "hectareas", "municipio", "departamento", "project", "proyecto"]
    # señal de que es el cascarón vacío típico de una SPA sin SSR
    cascaron_vacio = '<div id="root"></div>' in html or '<div id="app"></div>' in html
    tiene_señal = any(s in lower for s in señales_positivas)
    return tiene_señal and not cascaron_vacio


def diagnosticar_ecoregistry_v2(id_max: int) -> None:
    _log("\n" + "=" * 70)
    _log(f"ECOREGISTRY v2 — www.ecoregistry.io/projects/1..{id_max}")
    _log("=" * 70)

    n_ok = 0
    n_contenido_real = 0
    ejemplos_reales: list[int] = []

    for pid in range(1, id_max + 1):
        url = f"https://www.ecoregistry.io/projects/{pid}"
        r = _probe(f"ecoregistry_v2_project_{pid}", url)
        if r["error"]:
            _log(f"  id={pid}: ERROR {r['error']}")
            continue
        if r["status"] != 200:
            _log(f"  id={pid}: status={r['status']}")
            continue
        n_ok += 1
        es_real = _es_contenido_real_ecoregistry(r["text"])
        if es_real:
            n_contenido_real += 1
            ejemplos_reales.append(pid)
        # Guardar el primero y algún otro para inspección manual.
        if pid in (1, id_max):
            (DIAG_DIR / f"ecoregistry_v2_project_{pid}_raw.txt").write_text(
                r["text"][:20000], encoding="utf-8", errors="replace"
            )

    _log(f"\nTotal IDs con status 200: {n_ok}/{id_max}")
    _log(f"IDs donde detecté contenido real (no cascarón vacío): {n_contenido_real}")
    if ejemplos_reales:
        _log(f"Ejemplos: {ejemplos_reales[:10]}")
        _log(
            "-> Si esto es > 0: el pipeline SÍ puede extraer EcoRegistry con requests "
            "normal, igual que hace con Verra. Solo falta escribir el parser de texto "
            "(regex para 'municipio', hectáreas, fecha) y detectar el ID máximo real "
            "(seguir incrementando hasta varios 404 seguidos, como ya hacen con Verra)."
        )
    else:
        _log(
            "-> Si esto es 0: EcoRegistry SÍ requiere JavaScript (no hay SSR). "
            "En ese caso no se puede extraer con requests/BeautifulSoup — se necesitaría "
            "Selenium/Playwright (headless browser), que es una complicación mayor. "
            "Avísame y evaluamos si vale la pena para el número de proyectos que tienen."
        )

    _log(f"\nRevisa manualmente {DIAG_DIR / 'ecoregistry_v2_project_1_raw.txt'} para confirmar a simple vista.")


# =============================================================================
# ACR — nuevo dominio acrcarbon.org
# =============================================================================

def diagnosticar_acr_v2() -> None:
    _log("\n" + "=" * 70)
    _log("ACR v2 — acrcarbon.org (dominio nuevo; el anterior acr2.apx.com ya no existe)")
    _log("=" * 70)

    candidatos = [
        "https://acrcarbon.org",
        "https://acrcarbon.org/acr-registry/public-reports/",
        "https://acrcarbon.org/acr-registry/projects-requesting-registration/",
    ]
    for url in candidatos:
        r = _probe(f"acr_v2_{url.split('//')[1].replace('/', '_')[:50]}", url)
        _log(f"\n--- {url} ---")
        if r["error"]:
            _log(f"ERROR: {r['error']}")
            continue
        _log(f"Status: {r['status']}")
        preview = r["text"][:400].replace("\n", " ").strip()
        _log(f"Primeros 400 chars: {preview}")
        out = DIAG_DIR / f"acr_v2_{url.split('//')[1].replace('/', '_')[:50]}_raw.txt"
        out.write_text(r["text"][:20000], encoding="utf-8", errors="replace")
        _log(f"(guardado en {out})")

    _log(
        "\nSi 'public-reports' trae un link/botón de descarga CSV, ábrelo manualmente "
        "en el navegador, copia la URL final del archivo y pégamela — esa suele ser "
        "una URL de exportación directa (?format=csv o similar) que no requiere scraping HTML."
    )


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser()
    p.add_argument("--eco-id-max", type=int, default=30, help="Hasta qué ID de proyecto de EcoRegistry probar.")
    return p.parse_args()


def main() -> None:
    args = parse_args()
    diagnosticar_ecoregistry_v2(args.eco_id_max)
    diagnosticar_acr_v2()

    out_path = DIAG_DIR / "reporte_ronda2.txt"
    out_path.write_text("\n".join(lines), encoding="utf-8")
    _log(f"\n\nReporte guardado en: {out_path}")


if __name__ == "__main__":
    main()
