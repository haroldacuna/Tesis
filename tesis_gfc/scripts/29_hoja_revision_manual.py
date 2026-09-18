#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
29_hoja_revision_manual.py
==========================

Prepara y valida la revision manual de septiembre de 2026.

    python scripts\\29_hoja_revision_manual.py              # arma la hoja
    python scripts\\29_hoja_revision_manual.py --validar    # control final

Correr SIEMPRE desde tesis_gfc\\ (no desde scripts\\): las rutas son
relativas y existe una carpeta sombra en scripts\\data\\interim\\.

SALIDAS
-------
data/interim/diagnostics/revision_manual_sep2026.csv
    Una fila por tarea (SECTOR o MUNICIPIO) con ID de registro, Project URL,
    tokens que dispararon el match y sugerencia automatica.

data/interim/correcciones_municipio_PLANTILLA.csv
    Filas prellenadas para los eventos a verificar. Se completa a mano y se
    renombra a correcciones_municipio.csv (no se sobrescribe si ya existe).

--validar sale con codigo 1 si queda algun pendiente sectorial o algun par
de proyectos duplicados/casi duplicados con clasificacion distinta.
"""

from __future__ import annotations

import argparse
import sys
from difflib import SequenceMatcher
from pathlib import Path

import pandas as pd

from filtro_sectorial import (
    CLASIFICACION_FILE,
    ETIQUETAS_VALIDAS,
    cargar_tabla,
    clave_proyecto,
    normalizar,
)

CERC_FILE = Path("data/interim/diagnostics/cercarbono_municipio_matching.csv")
RENARE_FILE = Path("data/interim/diagnostics/renare_municipio_matching.csv")
EVENTOS_FILE = Path("data/interim/eventos_carbono_consolidado.csv")
OUT_HOJA = Path("data/interim/diagnostics/revision_manual_sep2026.csv")
OUT_PLANTILLA = Path("data/interim/correcciones_municipio_PLANTILLA.csv")
CORRECCIONES_FILE = Path("data/interim/correcciones_municipio.csv")

# Eventos cuya ubicacion hay que verificar
MUNICIPIOS_A_VERIFICAR = {
    "19392": "Carbono CAS (CDC-73): token LA SIERRA, probable nombre de la empresa",
    "27050": "Bajo Atrato asignado a Atrato; esperado Riosucio/Unguia",
    "50124": "Cabuyaro: plausible, no verificable por nombre",
    "94888": "Morichal: plausible, no verificable por nombre",
}
PATRON_NOMBRE = r"carbono\s*cas"   # por si CAS quedo en otro codigo
UMBRAL_CASI_DUPLICADO = 0.90


def _dane(s: pd.Series) -> pd.Series:
    return s.astype(str).str.strip().str.replace(r"\.0$", "", regex=True).str.zfill(5)


def _verificar_directorio() -> None:
    if not Path("data/interim").exists() or Path.cwd().name == "scripts":
        sys.exit("[X] Corre esto desde tesis_gfc\\ (no desde scripts\\).")


def _enlaces() -> pd.DataFrame:
    """ID, URL y tokens de match por clave. Los pares duplicados quedan
    visibles como varios IDs separados por ';' en la misma clave."""
    partes = []
    if CERC_FILE.exists():
        c = pd.read_csv(CERC_FILE, dtype=str).fillna("")
        partes.append(pd.DataFrame({
            "clave": [clave_proyecto("cercarbono", n) for n in c["Project Name"]],
            "id_registro": c.get("Project ID", ""),
            "url": c.get("Project URL", ""),
            "tipo_registro": "",
            "tokens_match": c["municipio"],
            "cod_dane_match": c["cod_dane"],
        }))
    if RENARE_FILE.exists():
        r = pd.read_csv(RENARE_FILE, dtype=str).fillna("")
        partes.append(pd.DataFrame({
            "clave": [clave_proyecto("renare", n) for n in r["nombre_iniciativa"]],
            "id_registro": r.get("_id", ""),
            "url": "",
            "tipo_registro": r.get("tipo", ""),
            "tokens_match": r["municipio"],
            "cod_dane_match": r["cod_dane"],
        }))
    if not partes:
        return pd.DataFrame(columns=["clave"])
    e = pd.concat(partes, ignore_index=True)
    unir = lambda s: ";".join(sorted({x for x in s if x}))
    agg = e.groupby("clave").agg(
        id_registro=("id_registro", unir),
        n_registros=("id_registro", "size"),
        url=("url", unir),
        tipo_registro=("tipo_registro", unir),
        tokens_match=("tokens_match", unir),
        cod_dane_match=("cod_dane_match", unir),
    )
    return agg.reset_index()


def verificar_pares(tabla: pd.DataFrame) -> pd.DataFrame:
    """Detecta (a) filas con la misma clave y clasificacion distinta y
    (b) nombres casi identicos dentro de la misma fuente con clasificacion
    distinta. En ambos casos la regla es: mismo valor a ambos."""
    t = tabla.copy()
    t["cf"] = t["clasificacion_final"].str.strip().str.upper()
    problemas = []

    exactos = t.groupby("clave")["cf"].agg(lambda s: sorted(set(s)))
    for clave, vals in exactos.items():
        if len(vals) > 1:
            problemas.append({"tipo": "misma_clave", "a": clave, "b": clave,
                              "valores": "/".join(v or "(vacio)" for v in vals)})

    t = t.drop_duplicates("clave")
    t["n"] = t["nombre_proyecto"].map(normalizar)
    for fuente, g in t.groupby("fuente"):
        filas = g[["clave", "n", "cf"]].to_numpy()
        for i in range(len(filas)):
            for j in range(i + 1, len(filas)):
                (ka, na, ca), (kb, nb, cb) = filas[i], filas[j]
                if ca == cb:
                    continue
                if SequenceMatcher(None, na, nb).ratio() >= UMBRAL_CASI_DUPLICADO:
                    problemas.append({"tipo": "casi_duplicado", "a": ka, "b": kb,
                                      "valores": f"{ca or '(vacio)'}/{cb or '(vacio)'}"})
    return pd.DataFrame(problemas, columns=["tipo", "a", "b", "valores"])


def claves_con_evento(tabla: pd.DataFrame) -> set[str]:
    """Claves que efectivamente generan eventos en 26. Solo estas bloquean
    anotar_sector(); clasificar el resto no cambia el panel.
    Verra y Gold Standard: todas (ubicacion por geometria).
    RENARE y Cercarbono: solo match municipal unico (n_matches == 1)."""
    claves = set(tabla.loc[tabla["fuente"].isin(["verra", "gold_standard"]), "clave"])
    for ruta, fuente, col in [(RENARE_FILE, "renare", "nombre_iniciativa"),
                              (CERC_FILE, "cercarbono", "Project Name")]:
        if ruta.exists():
            m = pd.read_csv(ruta, dtype=str).fillna("")
            m = m[pd.to_numeric(m["n_matches"], errors="coerce") == 1]
            claves |= {clave_proyecto(fuente, n) for n in m[col]}
    return claves


def armar_hoja() -> int:
    tabla = cargar_tabla()
    enl = _enlaces()

    relevantes = claves_con_evento(tabla)
    pend_todos = tabla[tabla["clasificacion_final"].str.strip() == ""].copy()
    pend = pend_todos[pend_todos["clave"].isin(relevantes)].copy()
    resumen = pd.DataFrame({
        "pendientes_total": pend_todos["fuente"].value_counts(),
        "pendientes_que_generan_evento": pend["fuente"].value_counts(),
    }).fillna(0).astype(int)
    print("Pendientes sectoriales:")
    print(resumen.to_string())
    hoja_sector = pend[["clave", "fuente", "nombre_proyecto",
                        "clasificacion_sugerida", "motivo_sugerencia"]].assign(
        tarea="SECTOR", COD_DANE="", nota="")

    if not EVENTOS_FILE.exists():
        sys.exit(f"[X] Falta {EVENTOS_FILE}. Corre 26 una vez con la ruta corregida.")
    ev = pd.read_csv(EVENTOS_FILE, dtype=str).fillna("")
    ev["COD_DANE"] = _dane(ev["COD_DANE"])
    # Todo Cercarbono entra a la cola: su municipio sale de texto libre y,
    # con la fecha de respaldo, decenas de proyectos AFOLU pasan a tratamiento.
    mask = (ev["COD_DANE"].isin(MUNICIPIOS_A_VERIFICAR)
        | ev["nombre_proyecto"].str.contains(PATRON_NOMBRE, case=False, regex=True)
        | ev["metodo"].eq("matching_texto"))
    hoja_mun = ev[mask].copy()
    hoja_mun["clave"] = [clave_proyecto(f, n) for f, n in
                         zip(hoja_mun["fuente"], hoja_mun["nombre_proyecto"])]
    hoja_mun["nota"] = hoja_mun["COD_DANE"].map(MUNICIPIOS_A_VERIFICAR).fillna(
        "Cercarbono: ubicacion por texto, verificar con Project URL")
    # Primero las cohortes tempranas: mas periodos post, mas peso en el ATT
    hoja_mun["_orden"] = pd.to_numeric(hoja_mun["anio_inicio"], errors="coerce")
    hoja_mun = hoja_mun.sort_values("_orden").drop(columns="_orden")
    hoja_mun = hoja_mun.merge(
        tabla[["clave", "clasificacion_sugerida", "motivo_sugerencia"]]
        .drop_duplicates("clave"), on="clave", how="left").assign(tarea="MUNICIPIO")

    cols = ["tarea", "fuente", "nombre_proyecto", "COD_DANE", "nota",
            "clasificacion_sugerida", "motivo_sugerencia", "clave"]
    hoja = pd.concat([hoja_sector[cols], hoja_mun[cols + ["anio_inicio"]]],
                     ignore_index=True).merge(enl, on="clave", how="left")
    OUT_HOJA.parent.mkdir(parents=True, exist_ok=True)
    hoja.to_csv(OUT_HOJA, index=False, encoding="utf-8-sig")
    print(f"\nHoja de trabajo: {OUT_HOJA}  ({len(hoja)} filas)")

    plantilla = pd.DataFrame({
        "clave": hoja_mun["clave"],
        "fuente": hoja_mun["fuente"],
        "nombre_proyecto": hoja_mun["nombre_proyecto"],
        "cod_dane_original": hoja_mun["COD_DANE"],
        "accion": "", "cod_dane_nuevo": "", "nombre_mpi_nuevo": "",
        "evidencia": "", "fecha_consulta": "",
    }).drop_duplicates(["clave", "cod_dane_original"])
    # La plantilla es un archivo aparte: nunca pisa correcciones_municipio.csv.
    # Copia de ella solo las filas ya verificadas.
    plantilla.to_csv(OUT_PLANTILLA, index=False, encoding="utf-8-sig")
    print(f"Plantilla de correcciones: {OUT_PLANTILLA}  ({len(plantilla)} eventos)")
    if CORRECCIONES_FILE.exists():
        ya = pd.read_csv(CORRECCIONES_FILE, dtype=str, encoding="utf-8-sig")
        print(f"  {CORRECCIONES_FILE} ya tiene {len(ya)} filas verificadas.")

    pares = verificar_pares(tabla)
    pares = pares[pares["a"].isin(relevantes) | pares["b"].isin(relevantes)]
    if len(pares):
        print("\nPares a clasificar con el MISMO valor:")
        print(pares.to_string(index=False))
    return 0


def validar() -> int:
    tabla = cargar_tabla()
    relevantes = claves_con_evento(tabla)
    cf = tabla["clasificacion_final"].str.strip().str.upper()
    fuera = ~tabla["clave"].isin(relevantes) & ~cf.isin(ETIQUETAS_VALIDAS)
    print(f"[i] {fuera.sum()} pendientes sin evento (no bloquean 26).")
    invalidas = tabla[tabla["clave"].isin(relevantes) & ~cf.isin(ETIQUETAS_VALIDAS)]
    sin_just = tabla[tabla["clave"].isin(relevantes) & cf.isin(ETIQUETAS_VALIDAS)
                     & (tabla["clasificacion_sugerida"] == "REVISAR")
                     & (tabla["justificacion"].str.strip() == "")]
    pares = verificar_pares(tabla)
    pares = pares[pares["a"].isin(relevantes) | pares["b"].isin(relevantes)]

    ok = True
    if len(invalidas):
        ok = False
        print(f"[X] {len(invalidas)} filas que generan evento sin AFOLU/NO_AFOLU:")
        print(invalidas[["fuente", "nombre_proyecto", "clasificacion_final"]].to_string(index=False))
    if len(sin_just):
        ok = False
        print(f"[X] {len(sin_just)} decisiones manuales sin justificacion:")
        print(sin_just[["fuente", "nombre_proyecto"]].to_string(index=False))
    if len(pares):
        ok = False
        print("[X] Pares con clasificacion inconsistente:")
        print(pares.to_string(index=False))
    if not CORRECCIONES_FILE.exists():
        ok = False
        print(f"[X] Falta {CORRECCIONES_FILE}")
    if ok:
        print(f"[OK] {CLASIFICACION_FILE} y {CORRECCIONES_FILE} listos para 26.")
    return 0 if ok else 1


if __name__ == "__main__":
    _verificar_directorio()
    ap = argparse.ArgumentParser()
    ap.add_argument("--validar", action="store_true")
    sys.exit(validar() if ap.parse_args().validar else armar_hoja())
