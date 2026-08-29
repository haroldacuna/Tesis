"""
15_reextraer_verra_desde_offsetsdb.py

El scraper de Verra que ya existe en 06_enrich_panel_external.py
(_fetch_verra_from_public_details) escanea A CIEGAS todos los IDs de VCS
del 1 al 2800 buscando cuáles mencionan Colombia — lento (hasta 2800
requests) y se corta antes de tiempo si hay una racha larga sin matches
(por eso probablemente solo encontró 8 municipios). Además, ni siquiera
extrae un campo de ubicación (State/Province/región): solo nombre, estado,
fechas.

Ahora ya sabemos exactamente cuáles son los 98 proyectos Verra de Colombia
que aparecen en offsetsdb_sin_municipio_para_revisar.csv (columna registry
== 'verra', project_id tipo 'VCS5908'). Este script:

  1. Va DIRECTO a esos 98 IDs (no escanea 1-2800), mucho más rápido.
  2. Extrae más campos que el parser original: intenta capturar
     "Country/Area" y cualquier texto descriptivo del proyecto, no solo
     nombre/fechas/estado.
  3. Corre el matching por texto contra los nombres de municipio sobre ESE
     texto ampliado (nombre + descripción + Country/Area), que tiene mucha
     más superficie que solo el nombre corto del proyecto.

USO
---
    python 15_reextraer_verra_desde_offsetsdb.py \
        --input data/interim/diagnostics/offsetsdb_sin_municipio_para_revisar.csv

Salidas:
    data/interim/diagnostics/verra_offsetsdb_enriquecido.csv
        (los 98 proyectos con lo que se pudo extraer + municipio si se
        encontró match de texto)
"""

from __future__ import annotations

import argparse
import importlib.util
import re
import sys
import unicodedata
from pathlib import Path
from typing import Any

import pandas as pd
import requests

ENRICH_SCRIPT = Path(__file__).resolve().parent / "06_enrich_panel_external.py"
if not ENRICH_SCRIPT.exists():
    print(f"ERROR: no encuentro {ENRICH_SCRIPT}. Corre este script desde la misma carpeta.")
    sys.exit(1)

_spec = importlib.util.spec_from_file_location("enrich", ENRICH_SCRIPT)
enrich = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(enrich)  # type: ignore[union-attr]

DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)
OUT_ENRIQUECIDO = DIAG_DIR / "verra_offsetsdb_enriquecido.csv"

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
        "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"
    ),
}


def _normalize_text(value: str | None) -> str:
    if not value:
        return ""
    txt = unicodedata.normalize("NFKD", str(value))
    txt = "".join(ch for ch in txt if not unicodedata.combining(ch))
    return txt.upper().strip()


def _extraer_vcs_id(project_id: str) -> int | None:
    m = re.search(r"(\d+)", str(project_id))
    return int(m.group(1)) if m else None


def _extraer_texto_ampliado(html: str) -> dict[str, Any]:
    """Extrae lo que ya sacaba el parser original, más 'Country/Area' y un
    bloque más largo de texto libre alrededor del nombre del proyecto, para
    tener más superficie donde buscar nombres de municipio."""
    text = re.sub(r"<[^>]+>", " ", html)
    text = re.sub(r"\s+", " ", text).strip()
    text_lower = text.lower()

    if "project summary" not in text_lower or "vcs project status" not in text_lower:
        return {}

    registration_date = enrich._extract_label_value(text, "Project Registration Date")
    status = enrich._extract_label_value(text, "VCS Project Status")
    start_date, end_date = enrich._extract_crediting_period_dates(text)
    country_area = enrich._extract_label_value(text, "Country/Area")
    proponent = enrich._extract_label_value(text, "Proponent")

    # Bloque de texto amplio: los primeros ~2000 caracteres después de "Project Summary",
    # donde suele estar la descripción con menciones de región/municipio.
    idx = text_lower.find("project summary")
    bloque = text[idx: idx + 2000] if idx >= 0 else text[:2000]

    return {
        "registrationDate": registration_date,
        "status": status,
        "creditingPeriodStartDate": start_date,
        "creditingPeriodEndDate": end_date,
        "country_area": country_area,
        "proponent_verra": proponent,
        "texto_descripcion": bloque,
    }


def reextraer(input_csv: Path) -> pd.DataFrame:
    df = pd.read_csv(input_csv)
    verra = df[df["registry"] == "verra"].copy()
    print(f"Proyectos Verra a re-consultar: {len(verra)}")

    session = requests.Session()
    session.headers.update(HEADERS)

    resultados = []
    for _, row in verra.iterrows():
        vcs_id = _extraer_vcs_id(row["project_id"])
        if vcs_id is None:
            continue
        url = enrich.VERA_PROJECT_DETAIL_URL.format(project_id=vcs_id)
        try:
            resp = session.get(url, timeout=15)
            resp.raise_for_status()
            extra = _extraer_texto_ampliado(resp.text)
        except Exception as exc:
            extra = {"error": str(exc)}

        fila = {
            "project_id": row["project_id"],
            "name_offsetsdb": row.get("name"),
            "proponent_offsetsdb": row.get("proponent"),
            "project_url": row.get("project_url"),
            **extra,
        }
        resultados.append(fila)
        print(f"  VCS/{vcs_id}: {'OK' if 'error' not in extra else 'ERROR - ' + extra['error']}")

    return pd.DataFrame(resultados)


def matchear_municipios(df: pd.DataFrame) -> pd.DataFrame:
    if not enrich.INPUT_PANEL.exists():
        print(f"Aviso: no encuentro {enrich.INPUT_PANEL}, no se puede hacer matching de municipio.")
        return df

    panel = pd.read_csv(enrich.INPUT_PANEL)
    mun = panel[["COD_DANE", "NOMBRE_MPI"]].drop_duplicates()
    mun["COD_DANE"] = mun["COD_DANE"].astype(str)
    tokens = [
        (_normalize_text(name), cod)
        for cod, name in mun.itertuples(index=False)
        if len(_normalize_text(name)) >= 6
    ]

    matched_codes_list = []
    matched_names_list = []
    for _, row in df.iterrows():
        haystack = " ".join(
            str(row.get(f, "")) for f in ["name_offsetsdb", "proponent_offsetsdb", "country_area", "texto_descripcion"]
            if pd.notna(row.get(f))
        )
        haystack_norm = _normalize_text(haystack)
        codes = set()
        names = set()
        for token_name, cod in tokens:
            if token_name and token_name in haystack_norm:
                codes.add(cod)
                names.add(token_name)
        matched_codes_list.append(";".join(sorted(codes)))
        matched_names_list.append(";".join(sorted(names)))

    df = df.copy()
    df["matched_cod_dane"] = matched_codes_list
    df["matched_municipio"] = matched_names_list
    df["n_matches"] = [len(c.split(";")) if c else 0 for c in matched_codes_list]
    return df


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser()
    p.add_argument(
        "--input",
        type=Path,
        default=DIAG_DIR / "offsetsdb_sin_municipio_para_revisar.csv",
    )
    return p.parse_args()


def main() -> None:
    args = parse_args()
    if not args.input.exists():
        print(f"ERROR: no encuentro {args.input}")
        return

    df = reextraer(args.input)
    if df.empty:
        print("No se obtuvo ningún resultado.")
        return

    df = matchear_municipios(df)
    df.to_csv(OUT_ENRIQUECIDO, index=False, encoding="utf-8-sig")

    con_match = (df["n_matches"] == 1).sum() if "n_matches" in df.columns else 0
    ambiguos = (df["n_matches"] > 1).sum() if "n_matches" in df.columns else 0
    print(f"\nGuardado: {OUT_ENRIQUECIDO}")
    print(f"Con municipio único asignado: {con_match}/{len(df)}")
    print(f"Ambiguos: {ambiguos}/{len(df)}")


if __name__ == "__main__":
    main()
