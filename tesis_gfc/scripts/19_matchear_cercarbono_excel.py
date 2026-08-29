"""
19_matchear_cercarbono_excel.py

Cercarbono no expuso una API pública utilizable (el endpoint real,
/platform/project, pedía token de sesión que expira en minutos), pero SÍ
tiene un botón de exportación en registry.cercarbono.com que descarga un
Excel con el reporte completo de proyectos ("Projects Report"). Es la
fuente más simple y confiable de las que probamos para esta plataforma.

El Excel no trae columna de municipio/departamento — solo 'Host Country'.
El municipio, cuando aparece, está mencionado como texto libre dentro de
'Project Name' o 'Project description (English)'. Aplicamos el mismo
matching por texto (con lista negra) que ya usamos para RENARE.

Nota sobre el formato del archivo: los encabezados reales empiezan en la
fila 18 (índice 17), después de un bloque de metadata del reporte
('Date and time:', 'Projects Report', avisos legales). Si Cercarbono
cambia el formato del reporte en el futuro, ajusta `HEADER_ROW`.

USO
---
    python 19_matchear_cercarbono_excel.py \
        --excel data/raw/auxiliary/cercarbono_projects_report.xlsx \
        --panel data/final/panel_municipio_year.csv
"""

from __future__ import annotations

import argparse
import unicodedata
from pathlib import Path

import pandas as pd

HEADER_ROW = 17  # fila 18 (1-indexed) del Excel

DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)

# Misma lista negra validada con RENARE — palabras que coinciden con
# nombres de municipio pero que en la práctica son ruido (país, apellidos
# comunes, palabras genéricas del español).
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
    return [
        (_normalize(name), str(cod))
        for cod, name in mun.itertuples(index=False)
        if len(_normalize(name)) >= 6 and _normalize(name) not in BLOCKLIST
    ]


def cargar_cercarbono(excel_path: Path) -> pd.DataFrame:
    df = pd.read_excel(excel_path, sheet_name="Projects Report", header=HEADER_ROW)
    df = df.dropna(how="all")
    if "Host Country" in df.columns:
        antes = len(df)
        df = df[df["Host Country"] == "Colombia"].copy()
        print(f"Filtrado a Host Country == Colombia: {len(df)}/{antes}")
    return df


def matchear(df: pd.DataFrame, tokens: list[tuple[str, str]]) -> pd.DataFrame:
    campos_texto = [c for c in ["Project Name", "Project description (English)"] if c in df.columns]
    haystack_norm = (
        df[campos_texto].fillna("").agg(" ".join, axis=1).apply(_normalize)
    )

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

    res_df = pd.DataFrame(resultados).reset_index(drop=True)
    cols_base = [c for c in ["Project ID", "Project Name", "Project Stage", "Project URL", "Duration start", "Duration end"] if c in df.columns]
    return pd.concat([df[cols_base].reset_index(drop=True), res_df], axis=1)


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser()
    

    p.add_argument(
        "--excel", 
        type=Path, 
        default=Path("../data/raw/auxiliary/cercarbono_projects_report.xlsx"), 
        help="Ruta al Excel exportado desde registry.cercarbono.com"
    )
    p.add_argument(
        "--panel", 
        type=Path, 
        default=Path("../data/final/panel_municipio_year.csv")
    )
    
    return p.parse_args()


def main() -> None:
    args = parse_args()
    if not args.excel.exists():
        print(f"ERROR: no encuentro {args.excel}")
        return
    if not args.panel.exists():
        print(f"ERROR: no encuentro {args.panel}")
        return

    tokens = construir_tokens(args.panel)
    print(f"Tokens de municipio disponibles: {len(tokens)}")

    df = cargar_cercarbono(args.excel)
    out = matchear(df, tokens)

    unico = out[out["n_matches"] == 1]
    ambiguo = out[out["n_matches"] > 1]
    sin_match = out[out["n_matches"] == 0]

    print(f"\nTotal proyectos Colombia: {len(out)}")
    print(f"  Match único: {len(unico)}")
    print(f"  Ambiguos (>1 municipio mencionado): {len(ambiguo)}")
    print(f"  Sin match textual: {len(sin_match)}")

    out_path = DIAG_DIR / "cercarbono_municipio_matching.csv"
    out.to_csv(out_path, index=False, encoding="utf-8-sig")
    print(f"\nGuardado: {out_path}")
    print(
        "\n*** Igual que con RENARE: verifica cada match único a mano antes de usarlo. "
        "Los proyectos 'ambiguos' mencionan más de un municipio (a veces porque el "
        "proyecto real abarca varios, a veces por ruido) — requieren lectura manual "
        "de la descripción para decidir. Los 'sin match' necesitarían visitar el "
        "Project URL individual (requiere JavaScript, no se puede automatizar con "
        "requests simple). ***"
    )


if __name__ == "__main__":
    main()
