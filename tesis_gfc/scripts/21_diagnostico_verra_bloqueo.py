"""
21_diagnostico_verra_bloqueo.py

15_reextraer_verra_desde_offsetsdb.py corrió sin errores de red (si hubiera
habido excepciones, verías una columna 'error' en el CSV de salida — no
está), pero NINGUNA de las 98 filas trajo datos: eso significa que las 98
páginas respondieron con status 200, pero el HTML recibido ya no contiene
los marcadores de texto que el parser busca ("project summary",
"vcs project status"). Verra probablemente cambió de plantilla, o está
sirviendo un muro de cookies/verificación de bot en vez de la ficha real
del proyecto.

Este script pide UNA sola página (VCS/5908, el primer proyecto de tu lista)
y guarda el HTML crudo completo para inspección, en vez de intentar
parsearlo — así vemos exactamente qué está llegando.

USO
---
    python 21_diagnostico_verra_bloqueo.py
"""

from __future__ import annotations

from pathlib import Path

import requests

DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)

URL = "https://registry.verra.org/app/projectDetail/VCS/5908"

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
        "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"
    ),
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
}


def main() -> None:
    print(f"Consultando {URL} ...")
    resp = requests.get(URL, headers=HEADERS, timeout=20)
    print(f"Status: {resp.status_code}")
    print(f"Content-Type: {resp.headers.get('Content-Type')}")
    print(f"Longitud del cuerpo: {len(resp.text)} caracteres")

    out_path = DIAG_DIR / "verra_vcs5908_raw.html"
    out_path.write_text(resp.text, encoding="utf-8", errors="replace")
    print(f"\nHTML completo guardado en: {out_path}")

    text_lower = resp.text.lower()
    print(f"\nContiene 'project summary': {'project summary' in text_lower}")
    print(f"Contiene 'vcs project status': {'vcs project status' in text_lower}")
    print(f"Contiene 'cookie': {'cookie' in text_lower}")
    print(f"Contiene 'captcha' o 'cloudflare' o 'checking your browser': "
          f"{'captcha' in text_lower or 'cloudflare' in text_lower or 'checking your browser' in text_lower}")
    print(f"Contiene '<div id=\"root\">' (posible SPA sin SSR): {'<div id=\"root\">' in text_lower}")
    print(f"Contiene 'login' o 'sign in': {'login' in text_lower or 'sign in' in text_lower}")

    print(f"\nPrimeros 1000 caracteres del cuerpo:\n{resp.text[:1000]}")
    print(
        f"\nPégame la salida de este script (sobre todo la parte de 'Contiene...' y los "
        f"primeros 1000 caracteres) para saber si es bot-block, muro de cookies, cambio de "
        f"plantilla, o requiere JavaScript."
    )


if __name__ == "__main__":
    main()
