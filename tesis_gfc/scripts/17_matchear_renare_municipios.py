"""
17_matchear_renare_municipios.py

RENARE (281 solicitudes extraídas con 16_extraer_renare.py) no trae municipio
como campo estructurado en el endpoint de listado — "municipio" y
"departamento" solo aparecen 19 y 14 veces respectivamente, siempre dentro
de texto libre (nombre_iniciativa, o descripciones anidadas dentro del JSON
de 'actividades').

ADVERTENCIA IMPORTANTE sobre el matching por texto libre en este dataset
específico: encontramos falsos positivos claros en la primera pasada —
'CONVENCION' (municipio real en Norte de Santander) matcheaba hasta en una
fila cuyo nombre_iniciativa era literalmente 'hkjhkh' (un registro de
prueba/basura), lo que prueba que esa palabra aparece como texto repetido/
boilerplate dentro del JSON de actividades, no como lugar real. 'RESTREPO'
matcheaba por el apellido del expresidente Carlos Lleras Restrepo, no el
municipio. 'COLOMBIA' (sí, es un municipio real en Huila) matcheaba casi
cualquier proyecto que simplemente mencionaba el país.

Este script excluye una lista negra de nombres de municipio que coinciden
con palabras comunes del español, apellidos frecuentes, o el nombre del
país. La lista NO es exhaustiva — trátala como punto de partida, no como
solución completa. Cualquier match, incluso "único", debe verificarse a
mano antes de usarse en el panel final: esto es una herramienta de triage
(reduce 281 registros a un puñado de candidatos), no un reemplazo de
revisión humana.

USO
---
    python 17_matchear_renare_municipios.py \
        --renare data/interim/renare_solicitudes_colombia.csv \
        --panel data/final/panel_municipio_year.csv
"""

from __future__ import annotations

import argparse
import unicodedata
from pathlib import Path

import pandas as pd

DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)

# Nombres de municipio que en la práctica generan falsos positivos por ser
# palabras comunes del español, apellidos frecuentes, o coincidir con el
# nombre del país. Revisa y amplía esta lista si ves más casos raros al
# inspeccionar los resultados.
BLOCKLIST = {
    "COLOMBIA", "CONVENCION", "PARAMO", "FUNDACION", "RESTREPO", "PROVIDENCIA",
    "UNION", "ARGELIA", "BOLIVAR", "SANTANDER", "SUCRE", "GUADALUPE", "BELEN",
    "CONCEPCION", "MERCEDES", "PLATA", "VICTORIA", "ARGENTINA", "FLORIDA",
    "PALMIRA", "INDEPENDENCIA", "LIBERTAD", "ESPERANZA",
}


def _normalize(value) -> str:
    if pd.isna(value):
        return ""
    txt = unicodedata.normalize("NFKD", str(value))
    txt = "".join(ch for ch in txt if not unicodedata.combining(ch))
    return txt.upper().strip()


def construir_tokens(panel_path: Path) -> list[tuple[str, str]]:
    panel = pd.read_csv(panel_path)
    mun = panel[["COD_DANE", "NOMBRE_MPI"]].drop_duplicates()
    tokens = [
        (_normalize(name), str(cod))
        for cod, name in mun.itertuples(index=False)
        if len(_normalize(name)) >= 6 and _normalize(name) not in BLOCKLIST
    ]
    print(f"Tokens de municipio disponibles (tras excluir {len(BLOCKLIST)} problemáticos): {len(tokens)}")
    return tokens


def matchear(renare_path: Path, tokens: list[tuple[str, str]]) -> pd.DataFrame:
    renare = pd.read_csv(renare_path, low_memory=False)
    haystack = (
        renare.get("nombre_iniciativa", "").fillna("")
        + " "
        + renare.get("actividades", "").fillna("")
    )
    haystack_norm = haystack.apply(_normalize)

    resultados = []
    for texto in haystack_norm:
        codes, names = set(), set()
        for tok, cod in tokens:
            if tok and tok in texto:
                codes.add(cod)
                names.add(tok)
        resultados.append({
            "n_matches": len(codes),
            "cod_dane": ";".join(sorted(codes)),
            "municipio": ";".join(sorted(names)),
        })

    res_df = pd.DataFrame(resultados)
    cols_base = [c for c in ["_id", "nombre_iniciativa", "tipo", "fase"] if c in renare.columns]
    out = pd.concat([renare[cols_base].reset_index(drop=True), res_df], axis=1)
    return out


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser()
    p.add_argument("--renare", type=Path, default=Path("data/interim/renare_solicitudes_colombia.csv"))
    p.add_argument("--panel", type=Path, default=Path("data/final/panel_municipio_year.csv"))
    return p.parse_args()


def main() -> None:
    args = parse_args()
    if not args.renare.exists():
        print(f"ERROR: no encuentro {args.renare}. Corre primero 16_extraer_renare.py.")
        return
    if not args.panel.exists():
        print(f"ERROR: no encuentro {args.panel}.")
        return

    tokens = construir_tokens(args.panel)
    out = matchear(args.renare, tokens)

    unico = out[out["n_matches"] == 1]
    ambiguo = out[out["n_matches"] > 1]
    sin_match = out[out["n_matches"] == 0]

    print(f"\nTotal: {len(out)}")
    print(f"  Match único: {len(unico)}")
    print(f"  Ambiguos: {len(ambiguo)}")
    print(f"  Sin match: {len(sin_match)}")

    out.to_csv(DIAG_DIR / "renare_municipio_matching.csv", index=False, encoding="utf-8-sig")
    print(f"\nGuardado: {DIAG_DIR / 'renare_municipio_matching.csv'}")
    print(
        "\n*** Antes de usar los matches 'únicos' en el panel: ábrelos y verifica cada uno "
        "a mano contra el texto original. Esta lista negra es un punto de partida, no una "
        "garantía — puede haber más palabras problemáticas que no detectamos todavía. ***"
    )


if __name__ == "__main__":
    main()
