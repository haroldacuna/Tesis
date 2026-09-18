#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
30_llenar_pendientes.py
=======================

Evita editar a mano 16 filas dispersas dentro de clasificacion_sectorial.csv
(508 filas). Trabaja sobre un archivo pequeno y lo devuelve a la tabla grande.

    python scripts\\30_llenar_pendientes.py --extraer
        -> data/interim/pendientes_para_llenar.csv
           Solo los proyectos SIN clasificar QUE GENERAN EVENTO, con el
           contexto necesario (tipo, actividades, metodologia) para decidir.

    [editas ese archivo: clasificacion_final + justificacion]

    python scripts\\30_llenar_pendientes.py --importar <hoja_completada.csv>
        -> prellena pendientes_para_llenar.csv desde una hoja de revision ya
           diligenciada (usa clasificacion_final si existe; si no,
           clasificacion_sugerida), recortada a los proyectos que generan
           evento, con una justificacion que deja constancia del metodo.
           NO escribe en clasificacion_sectorial.csv: revisas y luego --aplicar.

    python scripts\\30_llenar_pendientes.py --aplicar
        -> vuelca las decisiones a clasificacion_sectorial.csv
           (deja copia .bak). Valida antes de escribir:
             - clasificacion_final en {AFOLU, NO_AFOLU}
             - justificacion no vacia
             - la clave existe y sigue vacia en la tabla grande

Correr desde tesis_gfc\\.
"""

from __future__ import annotations

import argparse
import datetime as dt
import shutil
import sys
from pathlib import Path

import pandas as pd

from filtro_sectorial import CLASIFICACION_FILE, ETIQUETAS_VALIDAS, cargar_tabla, clave_proyecto

DIAG = Path("data/interim/diagnostics")
RENARE_FILE = DIAG / "renare_municipio_matching.csv"
CERC_FILE = DIAG / "cercarbono_municipio_matching.csv"
SALIDA = Path("data/interim/pendientes_para_llenar.csv")
COLS_CONTEXTO = ["sector__tipo", "sector__actividades", "sector__Sector",
                 "sector__Methodology", "sector__methodology", "sector__projectType",
                 "sector__sectoralScope"]


def _claves_con_evento(tabla: pd.DataFrame) -> set[str]:
    """Claves que efectivamente generan eventos en 26. Solo estas bloquean
    anotar_sector(); clasificar el resto no cambia el panel.
    Verra y Gold Standard: todas (ubicacion por geometria).
    RENARE y Cercarbono: solo match municipal unico (n_matches == 1).
    Misma logica que 29; se repite aqui para no acoplar los dos scripts."""
    claves = set(tabla.loc[tabla["fuente"].isin(["verra", "gold_standard"]), "clave"])
    for ruta, fuente, col in [(RENARE_FILE, "renare", "nombre_iniciativa"),
                              (CERC_FILE, "cercarbono", "Project Name")]:
        if not ruta.exists():
            print(f"  Aviso: no encuentro {ruta}; {fuente} no aporta claves.")
            continue
        m = pd.read_csv(ruta, dtype=str).fillna("")
        m = m[pd.to_numeric(m["n_matches"], errors="coerce") == 1]
        claves |= {clave_proyecto(fuente, n) for n in m[col]}
    return claves


def importar(origen: Path) -> int:
    if not origen.exists():
        print(f"[X] No encuentro {origen}")
        return 1
    hoja = pd.read_csv(origen, dtype=str, encoding="utf-8-sig").fillna("")
    if "clave" not in hoja.columns:
        print("[X] El archivo no tiene columna 'clave'; no es una hoja de revision.")
        return 1
    if "tarea" in hoja.columns:
        hoja = hoja[hoja["tarea"] == "SECTOR"]

    # La decision explicita manda; si no hay, se toma la sugerencia diligenciada
    valor = hoja.get("clasificacion_final", pd.Series("", index=hoja.index)).str.strip().str.upper()
    sugerida = hoja.get("clasificacion_sugerida", pd.Series("", index=hoja.index)).str.strip().str.upper()
    hoja["_valor"] = valor.where(valor != "", sugerida)
    hoja = hoja.drop_duplicates("clave")

    tabla = cargar_tabla()
    relevantes = _claves_con_evento(tabla)
    sin_clasificar = set(tabla.loc[tabla["clasificacion_final"].str.strip() == "", "clave"])

    util = hoja[hoja["_valor"].isin(ETIQUETAS_VALIDAS)
                & hoja["clave"].isin(relevantes)
                & hoja["clave"].isin(sin_clasificar)].copy()
    print(f"Filas en la hoja: {len(hoja)} | con valor valido: {hoja['_valor'].isin(ETIQUETAS_VALIDAS).sum()} "
          f"| que generan evento y siguen pendientes: {len(util)}")
    if util.empty:
        print("[X] Nada que importar.")
        return 1

    hoy = dt.date.today().isoformat()
    def _justificar(r):
        partes = [str(r.get("fuente", ""))]
        tipo = str(r.get("tipo_registro", "")).strip()
        if tipo:
            partes.append(f"tipo={tipo[:80]}")
        idr = str(r.get("id_registro", "")).strip()
        if idr:
            partes.append(f"id={idr[:60]}")
        if str(r.get("fuente", "")) == "renare":
            partes.append("sin campo sectorial estructurado: clasificado por nombre y tipo de iniciativa")
        partes.append(f"revision manual {hoy}")
        return " | ".join(partes)

    out = pd.DataFrame({
        "clave": util["clave"],
        "fuente": util.get("fuente", ""),
        "nombre_proyecto": util.get("nombre_proyecto", ""),
        "clasificacion_final": util["_valor"],
        "justificacion": util.apply(_justificar, axis=1),
        "clasificacion_sugerida": util.get("clasificacion_sugerida", ""),
        "motivo_sugerencia": util.get("motivo_sugerencia", ""),
        "tokens_match": util.get("tokens_match", ""),
        "cod_dane_match": util.get("cod_dane_match", ""),
    }).sort_values(["clasificacion_final", "nombre_proyecto"])

    if SALIDA.exists():
        shutil.copy(SALIDA, SALIDA.with_suffix(".csv.bak"))
    out.to_csv(SALIDA, index=False, encoding="utf-8-sig")
    print(out["clasificacion_final"].value_counts().to_string())
    print(f"\nEscrito {SALIDA} ({len(out)} filas).")
    print("Revisa valores y justificaciones, luego: --aplicar")
    return 0


def extraer() -> int:
    tabla = cargar_tabla()
    relevantes = _claves_con_evento(tabla)
    pend = tabla[(tabla["clasificacion_final"].str.strip() == "")
                 & (tabla["clave"].isin(relevantes))].copy()
    if pend.empty:
        print("[OK] No hay pendientes que generen evento. Corre 26.")
        return 0

    cols = ["clave", "fuente", "nombre_proyecto", "clasificacion_sugerida", "motivo_sugerencia"]
    contexto = [c for c in COLS_CONTEXTO if c in pend.columns]
    out = pend[cols + contexto].copy()
    for c in contexto:                      # recortar JSON largos
        out[c] = out[c].astype(str).str.slice(0, 300)
    out.insert(3, "clasificacion_final", "")
    out.insert(4, "justificacion", "")
    out = out.sort_values(["fuente", "nombre_proyecto"])
    out.to_csv(SALIDA, index=False, encoding="utf-8-sig")
    print(f"{len(out)} pendientes escritos en {SALIDA}")
    print(out["fuente"].value_counts().to_string())
    print("\nLlena clasificacion_final (AFOLU / NO_AFOLU) y justificacion, "
          "luego corre --aplicar.")
    return 0


def aplicar() -> int:
    if not SALIDA.exists():
        print(f"[X] No encuentro {SALIDA}. Corre primero --extraer.")
        return 1
    edit = pd.read_csv(SALIDA, dtype=str, encoding="utf-8-sig").fillna("")
    edit["clasificacion_final"] = edit["clasificacion_final"].str.strip().str.upper()
    edit["justificacion"] = edit["justificacion"].str.strip()

    llenos = edit[edit["clasificacion_final"] != ""]
    malos = llenos[~llenos["clasificacion_final"].isin(ETIQUETAS_VALIDAS)]
    sin_just = llenos[llenos["justificacion"] == ""]
    if len(malos) or len(sin_just):
        if len(malos):
            print("[X] Valores invalidos (usa AFOLU o NO_AFOLU):")
            print(malos[["nombre_proyecto", "clasificacion_final"]].to_string(index=False))
        if len(sin_just):
            print("[X] Sin justificacion:")
            print(sin_just[["nombre_proyecto"]].to_string(index=False))
        return 1
    if llenos.empty:
        print("[X] No llenaste ninguna fila.")
        return 1

    tabla = pd.read_csv(CLASIFICACION_FILE, dtype=str, encoding="utf-8-sig").fillna("")
    faltantes = set(llenos["clave"]) - set(tabla["clave"])
    if faltantes:
        print(f"[X] Claves que ya no existen en la tabla (re-corre --extraer): {sorted(faltantes)[:5]}")
        return 1

    mapa_c = dict(zip(llenos["clave"], llenos["clasificacion_final"]))
    mapa_j = dict(zip(llenos["clave"], llenos["justificacion"]))
    objetivo = tabla["clave"].isin(mapa_c) & (tabla["clasificacion_final"].str.strip() == "")
    pisadas = tabla["clave"].isin(mapa_c).sum() - objetivo.sum()
    if pisadas:
        print(f"  Aviso: {pisadas} filas ya tenian valor y NO se tocan.")

    shutil.copy(CLASIFICACION_FILE, CLASIFICACION_FILE.with_suffix(".csv.bak"))
    tabla.loc[objetivo, "clasificacion_final"] = tabla.loc[objetivo, "clave"].map(mapa_c)
    tabla.loc[objetivo, "justificacion"] = tabla.loc[objetivo, "clave"].map(mapa_j)
    tabla.to_csv(CLASIFICACION_FILE, index=False, encoding="utf-8-sig")
    print(f"[OK] {objetivo.sum()} filas actualizadas en {CLASIFICACION_FILE}")
    print("Ahora: python scripts\\29_hoja_revision_manual.py --validar")
    return 0


if __name__ == "__main__":
    if Path.cwd().name == "scripts" or not Path("data/interim").exists():
        sys.exit("[X] Corre esto desde tesis_gfc\\ (no desde scripts\\).")
    ap = argparse.ArgumentParser()
    ap.add_argument("--extraer", action="store_true")
    ap.add_argument("--aplicar", action="store_true")
    ap.add_argument("--importar", type=Path, metavar="HOJA.csv")
    a = ap.parse_args()
    modos = [a.extraer, a.aplicar, a.importar is not None]
    if sum(bool(m) for m in modos) != 1:
        sys.exit("Usa exactamente uno: --extraer | --importar <archivo> | --aplicar")
    if a.importar is not None:
        sys.exit(importar(a.importar))
    sys.exit(extraer() if a.extraer else aplicar())
