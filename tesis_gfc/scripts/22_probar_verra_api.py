"""
22_probar_verra_api.py

El HTML de VCS/5908 confirma que registry.verra.org migró a una SPA pura
(<div id="app"></div> + app.js, bajo infraestructura de "Platts"/S&P Global)
— el scraper de páginas de detalle ya no puede funcionar, sin importar qué
tan bien esté escrito.

PERO: 06_enrich_panel_external.py ya tenía, desde antes, un intento de API
real (_fetch_verra_from_api -> POST a registry.verra.org/api/v1/projectSearch).
Como es una SPA, TIENE que estar llamando a alguna API para traer los datos
que muestra — puede que sea esta misma, o puede que haya cambiado.

Este script llama _fetch_verra_from_api directamente (reutilizando tu código
tal cual) y reporta con detalle qué devuelve, para saber si:
  a) todavía funciona igual que antes (En ese caso no hay nada que arreglar,
     y los 98 proyectos de OffsetsDB se pueden re-consultar por ahí)
  b) devuelve error/vacío (en ese caso necesitamos, igual que con Gold
     Standard, capturar por DevTools la llamada real que hace la SPA nueva)

USO
---
    python 22_probar_verra_api.py
"""

from __future__ import annotations

import importlib.util
import json
import sys
from pathlib import Path

import requests

ENRICH_SCRIPT = Path(__file__).resolve().parent / "06_enrich_panel_external.py"
if not ENRICH_SCRIPT.exists():
    print(f"ERROR: no encuentro {ENRICH_SCRIPT}.")
    sys.exit(1)

_spec = importlib.util.spec_from_file_location("enrich", ENRICH_SCRIPT)
enrich = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(enrich)  # type: ignore[union-attr]

DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)


def main() -> None:
    session = requests.Session()
    print(f"Probando {enrich.VERA_URL} (país=CO) usando la función ya existente en tu pipeline...")

    try:
        df = enrich._fetch_verra_from_api(session, country="CO")
    except Exception as exc:
        print(f"\nLa función lanzó una excepción: {exc}")
        print(
            "\nEsto confirma que la API tampoco responde como antes. Siguiente paso: "
            "captura por DevTools en registry.verra.org (Network -> Fetch/XHR) la petición "
            "real que hace la SPA al buscar/filtrar proyectos de Colombia, igual que "
            "hicimos con Gold Standard y RENARE."
        )
        return

    print(f"\nFilas obtenidas: {len(df)}")
    if df.empty:
        print(
            "\nLa función corrió sin excepción pero devolvió 0 filas. Puede que la API "
            "responda 200 con un cuerpo vacío/distinto al esperado. Revisa si hace falta "
            "actualizar el payload o los headers — captura por DevTools para comparar."
        )
        return

    print(f"Columnas: {list(df.columns)}")
    out_path = DIAG_DIR / "verra_api_resultado.csv"
    df.to_csv(out_path, index=False, encoding="utf-8-sig")
    print(f"\nGuardado: {out_path}")
    print(f"\nPrimeras filas:\n{df.head(10).to_string()}")

    # Comparar contra los 98 project_id de OffsetsDB para ver cuántos coinciden.
    offsetsdb_unmatched = DIAG_DIR / "offsetsdb_sin_municipio_para_revisar.csv"
    if offsetsdb_unmatched.exists():
        import pandas as pd
        import re

        offsetsdb = pd.read_csv(offsetsdb_unmatched)
        offsetsdb_verra_ids = {
            re.search(r"(\d+)", str(pid)).group(1)
            for pid in offsetsdb.loc[offsetsdb["registry"] == "verra", "project_id"]
            if re.search(r"(\d+)", str(pid))
        }
        id_col = None
        for cand in ["id", "projectId", "project_id"]:
            if cand in df.columns:
                id_col = cand
                break
        if id_col:
            api_ids = set(df[id_col].astype(str))
            overlap = offsetsdb_verra_ids & api_ids
            print(
                f"\nDe los {len(offsetsdb_verra_ids)} IDs de Verra-Colombia que ya conocíamos "
                f"por OffsetsDB, {len(overlap)} aparecen también en esta respuesta de la API."
            )


if __name__ == "__main__":
    main()
