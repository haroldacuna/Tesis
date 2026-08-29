"""
12_construir_bridge_offsetsdb.py

PROBLEMA: OffsetsDB (270 proyectos de Colombia confirmados por el diagnóstico)
no trae NINGÚN campo de ubicación sub-nacional en projects.csv — solo
'country'. El bridge de municipios (data/raw/auxiliary/offsetsdb_project_municipios.csv,
que _apply_offsetsdb_municipio_bridge necesita) no existe todavía.

QUÉ HACE ESTE SCRIPT
--------------------
1. Descarga OffsetsDB (o usa el caché si ya corriste 10_diagnostico_offsetsdb_detallado.py
   y guardaste el zip — si no, lo descarga de nuevo, pesa ~8.5 MB).
2. Filtra a Colombia (270 filas esperadas según tu diagnóstico).
3. Para cada proyecto, busca nombres de municipio colombiano como substring
   dentro de los campos de texto libre (`name`, `proponent`) — mismo método
   de matching por texto que ya usa el pipeline para Verra/Berkeley.
4. Genera data/raw/auxiliary/offsetsdb_project_municipios.csv con las columnas
   que _apply_offsetsdb_municipio_bridge espera (project_id, cod_dane, municipio),
   SOLO para los proyectos donde encontró un match no ambiguo.
5. Exporta aparte los proyectos SIN match a
   data/interim/diagnostics/offsetsdb_sin_municipio_para_revisar.csv,
   con su nombre, proponente y project_url — para que los busques manualmente
   (o sigas el project_url al registro original, que sí suele tener ubicación).

IMPORTANTE: el matching por texto es propenso a falsos positivos (ej. un
proyecto que se llama "El Dorado Solar" podría matchear con un municipio
que no tiene nada que ver, si el nombre coincide por casualidad). Este
script marca como "ambiguo" (y NO los incluye en el bridge automático)
cualquier proyecto que matchee más de un municipio distinto, para que los
revises tú. Revisa también una muestra de los matches únicos antes de
confiar en ellos ciegamente.

USO
---
    python 12_construir_bridge_offsetsdb.py
"""

from __future__ import annotations

import importlib.util
import io
import re
import sys
import unicodedata
import zipfile
from pathlib import Path

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
OUT_BRIDGE = enrich.OFFSETSDB_MUNICIPIO_BRIDGE
OUT_SIN_MATCH = DIAG_DIR / "offsetsdb_sin_municipio_para_revisar.csv"
OUT_AMBIGUOS = DIAG_DIR / "offsetsdb_matches_ambiguos_para_revisar.csv"


def _normalize_text(value: str | None) -> str:
    if not value:
        return ""
    txt = unicodedata.normalize("NFKD", str(value))
    txt = "".join(ch for ch in txt if not unicodedata.combining(ch))
    return txt.upper().strip()


def descargar_offsetsdb_colombia() -> pd.DataFrame:
    print(f"Descargando {enrich.OFFSETSDB_URL} ...")
    resp = requests.get(enrich.OFFSETSDB_URL, timeout=180)
    resp.raise_for_status()
    zf = zipfile.ZipFile(io.BytesIO(resp.content))
    projects = pd.read_csv(zf.open("projects.csv"), low_memory=False)
    mask = projects["country"].astype(str).str.contains("colombia", case=False, na=False)
    projects_co = projects[mask].copy()
    print(f"Proyectos Colombia: {len(projects_co)}")
    return projects_co


def construir_municipios_lookup() -> tuple[dict[str, set[str]], list[tuple[str, str]]]:
    """Reconstruye el listado de municipios (nombre normalizado -> COD_DANE)
    desde el panel final."""
    if not enrich.INPUT_PANEL.exists():
        print(f"ERROR: no encuentro {enrich.INPUT_PANEL}. Corre el pipeline hasta generar el panel final primero.")
        sys.exit(1)
    panel = pd.read_csv(enrich.INPUT_PANEL)
    mun = panel[["COD_DANE", "NOMBRE_MPI"]].drop_duplicates()
    mun["COD_DANE"] = mun["COD_DANE"].astype(str)

    by_name: dict[str, set[str]] = {}
    tokens: list[tuple[str, str]] = []
    for _, row in mun.iterrows():
        nm = _normalize_text(row["NOMBRE_MPI"])
        by_name.setdefault(nm, set()).add(row["COD_DANE"])
        # Solo usar como token de búsqueda en texto libre los nombres de más de
        # 5 caracteres, para evitar falsos positivos con municipios de nombre
        # muy corto/genérico (ej. 'PAZ', 'SAN LUIS' es más seguro que 'TOLU').
        if len(nm) >= 6:
            tokens.append((nm, row["COD_DANE"]))
    return by_name, tokens


def matchear_proyectos(projects_co: pd.DataFrame, tokens: list[tuple[str, str]]) -> pd.DataFrame:
    rows = []
    text_fields = [c for c in ["name", "proponent"] if c in projects_co.columns]

    for _, row in projects_co.iterrows():
        haystack = " ".join(str(row.get(f, "")) for f in text_fields if pd.notna(row.get(f)))
        haystack_norm = _normalize_text(haystack)

        matched_codes: set[str] = set()
        matched_names: set[str] = set()
        for token_name, cod_dane in tokens:
            if token_name and token_name in haystack_norm:
                matched_codes.add(cod_dane)
                matched_names.add(token_name)

        rows.append({
            "project_id": row.get("project_id"),
            "name": row.get("name"),
            "proponent": row.get("proponent"),
            "project_url": row.get("project_url"),
            "registry": row.get("registry"),
            "matched_cod_dane": ";".join(sorted(matched_codes)),
            "matched_municipio": ";".join(sorted(matched_names)),
            "n_matches": len(matched_codes),
        })

    return pd.DataFrame(rows)


def main() -> None:
    projects_co = descargar_offsetsdb_colombia()
    by_name, tokens = construir_municipios_lookup()
    print(f"Municipios disponibles para matching por texto: {len(tokens)}")

    resultado = matchear_proyectos(projects_co, tokens)

    sin_match = resultado[resultado["n_matches"] == 0]
    match_unico = resultado[resultado["n_matches"] == 1]
    ambiguos = resultado[resultado["n_matches"] > 1]

    print(f"\nTotal proyectos Colombia: {len(resultado)}")
    print(f"  Con match único (no ambiguo): {len(match_unico)}")
    print(f"  Ambiguos (matchean >1 municipio, requieren revisión manual): {len(ambiguos)}")
    print(f"  Sin ningún match por texto: {len(sin_match)}")

    # Construir el bridge SOLO con los matches únicos.
    bridge_rows = []
    for _, row in match_unico.iterrows():
        bridge_rows.append({
            "project_id": row["project_id"],
            "cod_dane": row["matched_cod_dane"],
            "municipio": row["matched_municipio"],
        })
    bridge_df = pd.DataFrame(bridge_rows)

    OUT_BRIDGE.parent.mkdir(parents=True, exist_ok=True)
    bridge_df.to_csv(OUT_BRIDGE, index=False, encoding="utf-8-sig")
    print(f"\nBridge automático (solo matches únicos) guardado en: {OUT_BRIDGE}")
    print("*** Antes de confiar en él: abre este archivo y revisa al menos 15-20 filas al azar")
    print("    contra el project_url real, para medir la tasa de falsos positivos. ***")

    sin_match.drop(columns=["matched_cod_dane", "matched_municipio", "n_matches"]).to_csv(
        OUT_SIN_MATCH, index=False, encoding="utf-8-sig"
    )
    print(f"\nProyectos sin match (revisar manualmente o seguir project_url): {OUT_SIN_MATCH}")

    ambiguos.to_csv(OUT_AMBIGUOS, index=False, encoding="utf-8-sig")
    print(f"Proyectos ambiguos (revisar manualmente): {OUT_AMBIGUOS}")

    print(
        f"\nResumen: de {len(resultado)} proyectos, {len(match_unico)} quedaron con municipio "
        f"asignado automáticamente ({len(match_unico) / len(resultado) * 100:.0f}%), "
        f"{len(ambiguos) + len(sin_match)} necesitan revisión manual. Es un punto de partida, "
        "no un reemplazo de la verificación manual, dado que el matching por texto libre "
        "puede tener falsos positivos y falsos negativos."
    )


if __name__ == "__main__":
    main()
