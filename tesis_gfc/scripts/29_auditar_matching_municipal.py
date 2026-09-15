#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
29_auditar_matching_municipal.py
================================

DIAGNOSTICO. No modifica nada.

    python 29_auditar_matching_municipal.py

Problema que resuelve
---------------------
El matcher de municipios por texto produce falsos positivos de dos tipos:

  1. NOMBRE DE DEPARTAMENTO TOMADO COMO MUNICIPIO. En Colombia, Caldas,
     Cordoba, Boyaca, Santander, Sucre, Bolivar y otros son a la vez
     departamento y municipio. Un proyecto que dice "en el departamento de
     Caldas" pesca el municipio de Caldas, Antioquia.

  2. SUBCADENA. "San Andres" matchea dentro de "San Andres de Cuerquia";
     "Cordoba" dentro de "Los Cordobas".

Cuando el proyecto pesca DOS o mas municipios, queda marcado como ambiguo y
se revisa a mano: ese es el caso benigno. El caso peligroso es cuando pesca
UNO SOLO y equivocado -- entra al panel como match unico, sin que nada lo
senale.

Este script audita justamente esos matches unicos. Para cada uno compara el
departamento del municipio asignado contra los departamentos que el texto
del proyecto menciona. Si el proyecto nombra un departamento y el municipio
asignado esta en otro, hay una discrepancia que revisar.

Salida
------
    data/interim/diagnostics/auditoria_matching_municipal.csv
"""

from __future__ import annotations

import re
import sys
import unicodedata
from pathlib import Path

import pandas as pd

RAIZ_PROYECTO = Path(__file__).resolve().parent.parent
DIR_INTERIM = RAIZ_PROYECTO / "data" / "interim"
DIR_DIAG = DIR_INTERIM / "diagnostics"

RENARE_FILE = DIR_DIAG / "renare_municipio_matching.csv"
RENARE_RAW = DIR_INTERIM / "renare_solicitudes_colombia.csv"
CERCARBONO_FILE = DIR_DIAG / "cercarbono_municipio_matching.csv"

OUT_FILE = DIR_DIAG / "auditoria_matching_municipal.csv"

# Codigos DANE de departamento -> nombre. Datos de referencia estables.
DEPARTAMENTOS = {
    "05": "Antioquia", "08": "Atlantico", "11": "Bogota", "13": "Bolivar",
    "15": "Boyaca", "17": "Caldas", "18": "Caqueta", "19": "Cauca",
    "20": "Cesar", "23": "Cordoba", "25": "Cundinamarca", "27": "Choco",
    "41": "Huila", "44": "La Guajira", "47": "Magdalena", "50": "Meta",
    "52": "Narino", "54": "Norte de Santander", "63": "Quindio",
    "66": "Risaralda", "68": "Santander", "70": "Sucre", "73": "Tolima",
    "76": "Valle del Cauca", "81": "Arauca", "85": "Casanare",
    "86": "Putumayo", "88": "San Andres", "91": "Amazonas",
    "94": "Guainia", "95": "Guaviare", "97": "Vaupes", "99": "Vichada",
}

# Nombres que son a la vez departamento y municipio: la fuente principal
# de falsos positivos.
TRAMPAS = [
    "caldas", "cordoba", "boyaca", "santander", "sucre", "bolivar",
    "cauca", "magdalena", "antioquia", "narino", "quindio", "guaviare",
    "amazonas", "vichada", "casanare", "arauca", "putumayo",
]

# Alias y formas como el texto suele nombrar un departamento
ALIAS_DEPTO = {
    "Valle del Cauca": ["valle del cauca", "valle"],
    "Norte de Santander": ["norte de santander"],
    "La Guajira": ["la guajira", "guajira"],
    "Bogota": ["bogota", "cundinamarca", "bogota region"],
    "San Andres": ["san andres y providencia", "archipielago"],
}


def norm(texto) -> str:
    if pd.isna(texto):
        return ""
    t = unicodedata.normalize("NFKD", str(texto))
    t = "".join(c for c in t if not unicodedata.combining(c))
    return re.sub(r"\s+", " ", t).strip().lower()


def deptos_mencionados(texto: str) -> list[str]:
    """Departamentos nombrados explicitamente en el texto del proyecto."""
    t = norm(texto)
    encontrados = []
    for codigo, nombre in DEPARTAMENTOS.items():
        formas = ALIAS_DEPTO.get(nombre, [norm(nombre)])
        if any(re.search(rf"\b{re.escape(f)}\b", t) for f in formas):
            encontrados.append(nombre)
    return encontrados


def cargar(fuente: str) -> pd.DataFrame:
    if fuente == "renare":
        if not RENARE_FILE.exists():
            print(f"    [ ] Falta {RENARE_FILE.name}")
            return pd.DataFrame()
        m = pd.read_csv(RENARE_FILE, low_memory=False)
        m["n_matches"] = pd.to_numeric(m["n_matches"], errors="coerce")
        m = m[m["n_matches"] == 1].copy()
        # 'actividades' del archivo crudo da mas texto donde buscar el
        # departamento: el nombre solo suele ser corto.
        if RENARE_RAW.exists() and "_id" in m.columns:
            raw = pd.read_csv(RENARE_RAW, low_memory=False)
            cols = [c for c in ["_id", "actividades"] if c in raw.columns]
            if len(cols) == 2:
                m = m.merge(raw[cols], on="_id", how="left", suffixes=("", "_raw"))
        col_act = "actividades" if "actividades" in m.columns else None
        return pd.DataFrame({
            "fuente": "renare",
            "nombre_proyecto": m["nombre_iniciativa"].astype(str),
            "texto_extra": m[col_act].astype(str) if col_act else "",
            "cod_dane": m["cod_dane"].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5),
            "municipio_asignado": m["municipio"].astype(str),
        })

    if not CERCARBONO_FILE.exists():
        print(f"    [ ] Falta {CERCARBONO_FILE.name}")
        return pd.DataFrame()
    m = pd.read_csv(CERCARBONO_FILE, low_memory=False)
    m["n_matches"] = pd.to_numeric(m["n_matches"], errors="coerce")
    m = m[m["n_matches"] == 1].copy()
    return pd.DataFrame({
        "fuente": "cercarbono",
        "nombre_proyecto": m["Project Name"].astype(str),
        "texto_extra": "",
        "cod_dane": m["cod_dane"].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5),
        "municipio_asignado": m["municipio"].astype(str),
    })


def auditar(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    df["depto_asignado"] = df["cod_dane"].str[:2].map(DEPARTAMENTOS).fillna("?")
    df["deptos_en_texto"] = [
        "; ".join(deptos_mencionados(f"{n} {e}"))
        for n, e in zip(df["nombre_proyecto"], df["texto_extra"])
    ]

    def veredicto(fila) -> str:
        mencionados = [d for d in fila["deptos_en_texto"].split("; ") if d]
        nombre_trampa = norm(fila["municipio_asignado"]) in TRAMPAS

        if mencionados and fila["depto_asignado"] not in mencionados:
            return "DISCREPANCIA" if not nombre_trampa else "DISCREPANCIA + nombre trampa"
        if nombre_trampa and not mencionados:
            return "NOMBRE TRAMPA (municipio homonimo de departamento)"
        if not mencionados:
            return "sin departamento en el texto"
        return "coherente"

    df["veredicto"] = df.apply(veredicto, axis=1)
    return df.drop(columns=["texto_extra"])


def main() -> int:
    print("Auditoria de falsos positivos en el matching municipal")
    print(f"Datos: {DIR_INTERIM}\n")

    partes = []
    for fuente in ["renare", "cercarbono"]:
        print(f"--- {fuente} ---")
        d = cargar(fuente)
        if not d.empty:
            print(f"    {len(d)} matches unicos")
            partes.append(d)

    if not partes:
        print("\n[X] No se pudo leer ninguna fuente.")
        return 1

    resultado = auditar(pd.concat(partes, ignore_index=True))

    print("\n" + "=" * 72)
    for v, n in resultado["veredicto"].value_counts().items():
        print(f"  {n:>3}  {v}")
    print("=" * 72)

    sospechosos = resultado[resultado["veredicto"].str.startswith(
        ("DISCREPANCIA", "NOMBRE TRAMPA")
    )]
    if len(sospechosos):
        print("\nRevisar uno por uno:\n")
        for _, r in sospechosos.iterrows():
            print(f"  [{r['fuente']}] {str(r['nombre_proyecto'])[:60]}")
            print(f"      asignado a: {r['municipio_asignado']} ({r['depto_asignado']}, "
                  f"{r['cod_dane']})")
            if r["deptos_en_texto"]:
                print(f"      texto menciona: {r['deptos_en_texto']}")
            print()

    DIR_DIAG.mkdir(parents=True, exist_ok=True)
    resultado.to_csv(OUT_FILE, index=False, encoding="utf-8-sig")
    print(f"Escrito: {OUT_FILE}")
    print("""
Las filas 'sin departamento en el texto' no son limpias: solo significa que
no habia informacion suficiente para verificarlas. Si alguna de ellas tiene
ano de inicio y entra al tratamiento, conviene abrirla igual.
""")
    return 0


if __name__ == "__main__":
    sys.exit(main())
