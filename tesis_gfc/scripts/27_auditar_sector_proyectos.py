#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
27_auditar_sector_proyectos.py
==============================

DIAGNOSTICO. No modifica ningun archivo del pipeline.

Problema que resuelve
---------------------
La extraccion de registros de carbono (Verra, Gold Standard, RENARE,
Cercarbono) no filtra por sector del proyecto. En consecuencia, la variable
de tratamiento del panel puede estar contaminada con proyectos que no son
AFOLU/forestales -- hidroelectricas, estufas eficientes, renovables, rellenos
sanitarios -- cuyo mecanismo causal no tiene ninguna relacion con la
deforestacion municipal. La aparicion de Medellin y Barranquilla como
municipios tratados es consistente con esa contaminacion.

Un proyecto de estufas eficientes en Medellin no puede reducir la
deforestacion de Medellin, pero entra al estimador como si pudiera, y arrastra
consigo un municipio con dinamica de perdida de cobertura completamente
distinta a la de un municipio de frontera agricola.

Que hace este script
--------------------
  1. Inventaria las columnas de cada archivo interino de registros y marca
     las que son candidatas a contener el sector, alcance, metodologia o
     tipo de proyecto (busqueda por palabra clave en el nombre de columna).
  2. Para cada columna candidata, muestra la distribucion de valores.
  3. Aplica una clasificacion heuristica por palabras clave sobre el NOMBRE
     del proyecto, como respaldo para las fuentes que no expongan un campo
     de sector estructurado (RENARE y Cercarbono, tipicamente).
  4. Cruza contra los eventos consolidados y estima cuantos municipios
     tratados caerian bajo un filtro AFOLU, por definicion de tratamiento.
  5. Escribe un CSV con la clasificacion proyecto por proyecto para revision
     manual antes de tocar 26_consolidar_fuentes_carbono.py.

USO
---
    python 27_auditar_sector_proyectos.py

Se ejecuta desde la raiz del proyecto (tesis_gfc/), igual que el resto del
pipeline. Si lo lanzas desde otra ubicacion, ajusta RAIZ abajo.

Salida
------
    data/interim/diagnostics/auditoria_sector_proyectos.csv
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

import pandas as pd

# ---------------------------------------------------------------------------
# Rutas. Mismas que 26_consolidar_fuentes_carbono.py
# ---------------------------------------------------------------------------
RAIZ = Path(".")

FUENTES: dict[str, list[Path]] = {
    "verra": [RAIZ / "data/interim/verra_platts_colombia_con_municipio.csv"],
    "gold_standard": [
        RAIZ / "data/interim/goldstandard_projects_colombia_con_municipio.csv",
        RAIZ / "data/interim/goldstandard_coords_corregidas.csv",
    ],
    "renare": [
        RAIZ / "data/interim/renare_solicitudes_colombia.csv",
        RAIZ / "data/interim/diagnostics/renare_municipio_matching.csv",
    ],
    "cercarbono": [RAIZ / "data/interim/diagnostics/cercarbono_municipio_matching.csv"],
}

EVENTOS_FILE = RAIZ / "data/interim/eventos_carbono_consolidado.csv"
OUT_DIR = RAIZ / "data/interim/diagnostics"
OUT_FILE = OUT_DIR / "auditoria_sector_proyectos.csv"

# Palabras clave para detectar columnas que podrian contener el sector.
# Se busca en el nombre de la columna, en minusculas y sin acentos.
PATRONES_COLUMNA = [
    "sector", "scope", "sectoral", "methodolog", "metodolog", "protocol",
    "project_type", "projecttype", "tipo", "categor", "category", "activit",
    "actividad", "subtipo", "programa", "program",
]

# Clasificacion heuristica por nombre de proyecto. El orden importa: se
# evalua NO_AFOLU primero porque algunos nombres mezclan terminos
# ("biomass cookstove forest protection").
PATRONES_NO_AFOLU = [
    r"cook\s*stove", r"cookstove", r"estufa", r"cocina", r"fogon",
    r"hidroel", r"hydro", r"small\s*hydro", r"run[- ]of[- ]river",
    r"e[oó]lic", r"wind\s*(power|farm|energy)",
    r"solar", r"fotovolt", r"photovolt",
    r"biog[aá]s", r"biogas", r"landfill", r"relleno\s*sanitario",
    r"waste\s*(water|heat|管)", r"aguas\s*residuales",
    r"transport", r"movilidad", r"veh[ií]cul", r"bus\s*rapid",
    r"efficien(cy|t)\s*(lighting|appliance)", r"iluminaci[oó]n",
    r"cement", r"cemento", r"steel", r"acero", r"industrial\s*gas",
    r"refriger", r"HFC", r"N2O", r"fugitive",
    r"geoterm", r"geotherm",
    r"agua\s*segura", r"safe\s*water", r"water\s*purificat", r"filtro\s*de\s*agua",
]

PATRONES_AFOLU = [
    r"REDD", r"deforestat", r"deforestaci", r"forest", r"bosque", r"selva",
    r"reforest", r"aforest", r"afforest", r"restauraci", r"restorat",
    r"conservaci", r"conservat", r"manglar", r"mangrove",
    r"agroforest", r"silvopast", r"silvopastor",
    r"suelo", r"soil\s*carbon", r"grassland", r"pastizal", r"pradera",
    r"p[aá]ramo", r"humedal", r"wetland", r"peat",
    r"plantaci[oó]n\s*forestal", r"timber", r"madera",
    r"AFOLU", r"IFM", r"ARR", r"ALM", r"WRC",
]


def _norm(texto: str) -> str:
    """Minusculas sin acentos, para comparar nombres de columna."""
    reemplazos = str.maketrans("áéíóúñÁÉÍÓÚÑ", "aeiounAEIOUN")
    return str(texto).translate(reemplazos).lower()


def _leer_fuente(paths: list[Path]) -> pd.DataFrame:
    """Concatena las partes existentes de una fuente. Devuelve vacio si no hay."""
    partes = []
    for p in paths:
        if p.exists():
            try:
                partes.append(pd.read_csv(p, low_memory=False))
            except Exception as exc:  # CSV corrupto, encoding raro, etc.
                print(f"    [!] No pude leer {p}: {exc}")
        else:
            print(f"    [ ] No encuentro {p} (se omite)")
    if not partes:
        return pd.DataFrame()
    return pd.concat(partes, ignore_index=True)


def _columnas_candidatas(df: pd.DataFrame) -> list[str]:
    return [c for c in df.columns if any(p in _norm(c) for p in PATRONES_COLUMNA)]


def _columna_nombre(df: pd.DataFrame) -> str | None:
    for c in ["projectName", "name", "nombre_proyecto", "nombre", "titulo",
              "title", "proyecto", "descripcion", "actividades"]:
        if c in df.columns:
            return c
    return None


def _clasificar(texto: str) -> str:
    """AFOLU / NO_AFOLU / INDETERMINADO a partir del texto libre."""
    if not isinstance(texto, str) or not texto.strip():
        return "INDETERMINADO"
    t = _norm(texto)
    for pat in PATRONES_NO_AFOLU:
        if re.search(pat, t, flags=re.IGNORECASE):
            return "NO_AFOLU"
    for pat in PATRONES_AFOLU:
        if re.search(pat, t, flags=re.IGNORECASE):
            return "AFOLU"
    return "INDETERMINADO"


def _sep(titulo: str = "") -> None:
    print("\n" + "=" * 78)
    if titulo:
        print(titulo)
        print("=" * 78)


# ---------------------------------------------------------------------------
# 1-3. Inventario de columnas y clasificacion por fuente
# ---------------------------------------------------------------------------
def auditar_fuentes() -> pd.DataFrame:
    filas = []

    for fuente, paths in FUENTES.items():
        _sep(f"FUENTE: {fuente.upper()}")
        df = _leer_fuente(paths)
        if df.empty:
            print("    Sin datos. Nada que auditar.")
            continue

        print(f"    Registros: {len(df)}  |  Columnas: {len(df.columns)}")

        # -- columnas candidatas a contener el sector -----------------------
        candidatas = _columnas_candidatas(df)
        if candidatas:
            print(f"\n    Columnas candidatas a SECTOR ({len(candidatas)}):")
            for c in candidatas:
                n_no_nulos = df[c].notna().sum()
                print(f"      - {c}  ({n_no_nulos}/{len(df)} no nulos)")
                vc = df[c].dropna().astype(str).str.strip()
                vc = vc[vc != ""].value_counts()
                if 0 < len(vc) <= 30:
                    for valor, n in vc.items():
                        print(f"            {n:>5}  {valor[:70]}")
                elif len(vc) > 30:
                    print(f"            ({len(vc)} valores distintos; top 15)")
                    for valor, n in vc.head(15).items():
                        print(f"            {n:>5}  {valor[:70]}")
        else:
            print("\n    [!] Ninguna columna con nombre sugerente de sector.")
            print("        Se recurre solo a la clasificacion por nombre de proyecto.")
            print(f"        Columnas disponibles: {list(df.columns)}")

        # -- clasificacion heuristica por nombre ----------------------------
        col_nombre = _columna_nombre(df)
        if col_nombre is None:
            print("\n    [!] No hay columna de nombre/descripcion. No se puede clasificar.")
            continue

        df["_clasificacion"] = df[col_nombre].apply(_clasificar)
        conteo = df["_clasificacion"].value_counts()
        print(f"\n    Clasificacion heuristica sobre '{col_nombre}':")
        for etiqueta in ["AFOLU", "NO_AFOLU", "INDETERMINADO"]:
            print(f"      {etiqueta:<15} {conteo.get(etiqueta, 0)}")

        sospechosos = df[df["_clasificacion"] == "NO_AFOLU"]
        if len(sospechosos) > 0:
            print("\n    Proyectos marcados NO_AFOLU (revisar uno por uno):")
            cols_ver = [col_nombre]
            if "COD_DANE" in df.columns:
                cols_ver.append("COD_DANE")
            if "NOMBRE_MPI" in df.columns:
                cols_ver.append("NOMBRE_MPI")
            for _, r in sospechosos[cols_ver].head(40).iterrows():
                mpio = r.get("NOMBRE_MPI", r.get("COD_DANE", ""))
                print(f"      [{mpio}] {str(r[col_nombre])[:80]}")
            if len(sospechosos) > 40:
                print(f"      ... y {len(sospechosos) - 40} mas (ver el CSV de salida)")

        # -- acumular para el CSV -------------------------------------------
        salida = pd.DataFrame({
            "fuente": fuente,
            "nombre_proyecto": df[col_nombre].astype(str),
            "clasificacion_heuristica": df["_clasificacion"],
        })
        salida["COD_DANE"] = (
            df["COD_DANE"].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5)
            if "COD_DANE" in df.columns else ""
        )
        salida["NOMBRE_MPI"] = df["NOMBRE_MPI"] if "NOMBRE_MPI" in df.columns else ""
        for c in candidatas:
            salida[f"sector__{c}"] = df[c]
        filas.append(salida)

    return pd.concat(filas, ignore_index=True) if filas else pd.DataFrame()


# ---------------------------------------------------------------------------
# 4. Impacto sobre el conteo de municipios tratados
# ---------------------------------------------------------------------------
def estimar_impacto(auditoria: pd.DataFrame) -> None:
    _sep("IMPACTO ESTIMADO SOBRE EL PANEL")

    if not EVENTOS_FILE.exists():
        print(f"    No encuentro {EVENTOS_FILE}.")
        print("    Corre 26_consolidar_fuentes_carbono.py primero para este bloque.")
        return

    eventos = pd.read_csv(EVENTOS_FILE, low_memory=False)
    eventos["COD_DANE"] = (
        eventos["COD_DANE"].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5)
    )

    # Municipios cuya UNICA justificacion de tratamiento es un proyecto NO_AFOLU.
    # Un municipio con al menos un proyecto AFOLU sigue tratado; solo cae si
    # todos sus proyectos son no forestales.
    clave = ["COD_DANE", "nombre_proyecto"]
    if not set(clave).issubset(auditoria.columns):
        print("    La auditoria no trae las columnas necesarias para el cruce.")
        return

    mapa = auditoria.drop_duplicates(subset=clave).set_index(clave)["clasificacion_heuristica"]
    eventos["_clasif"] = [
        mapa.get((cd, np), "INDETERMINADO")
        for cd, np in zip(eventos["COD_DANE"], eventos.get("nombre_proyecto", ""))
    ]

    con_fecha = eventos[eventos["anio_inicio"].notna()].copy()

    for etiqueta, confianzas in [
        ("Confianza alta (Verra + Gold Standard)", ["alta"]),
        ("Todas las fuentes (+ RENARE + Cercarbono)", ["alta", "media"]),
    ]:
        sub = con_fecha[con_fecha["confianza"].isin(confianzas)]
        actuales = set(sub["COD_DANE"].unique())
        con_afolu = set(sub[sub["_clasif"] == "AFOLU"]["COD_DANE"].unique())
        indet = set(sub[sub["_clasif"] == "INDETERMINADO"]["COD_DANE"].unique())

        caen = actuales - con_afolu - indet
        dudosos = (indet - con_afolu)

        print(f"\n    {etiqueta}")
        print(f"      Municipios tratados hoy ................ {len(actuales)}")
        print(f"      Con al menos un proyecto AFOLU ......... {len(con_afolu)}")
        print(f"      Caerian (todos sus proyectos NO_AFOLU) . {len(caen)}")
        print(f"      Requieren revision manual .............. {len(dudosos)}")
        if caen:
            nombres = (
                sub[sub["COD_DANE"].isin(caen)][["COD_DANE", "nombre_proyecto"]]
                .drop_duplicates().head(20)
            )
            print("      Detalle de los que caerian:")
            for _, r in nombres.iterrows():
                print(f"        {r['COD_DANE']}  {str(r['nombre_proyecto'])[:65]}")


# ---------------------------------------------------------------------------
def main() -> int:
    print("Auditoria sectorial de los registros de carbono")
    print(f"Directorio de trabajo: {Path.cwd()}")

    if not (RAIZ / "data/interim").exists():
        print("\n[X] No encuentro data/interim/. Corre este script desde tesis_gfc/.")
        return 1

    auditoria = auditar_fuentes()

    if auditoria.empty:
        print("\n[X] No se pudo auditar ninguna fuente.")
        return 1

    estimar_impacto(auditoria)

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    auditoria.to_csv(OUT_FILE, index=False, encoding="utf-8-sig")

    _sep("SIGUIENTE PASO")
    print(f"    Escrito: {OUT_FILE}")
    print("""
    1. Abre el CSV y revisa a mano la columna 'clasificacion_heuristica',
       empezando por los INDETERMINADO. La heuristica por nombre es un
       primer filtro, no una decision: un nombre como "Proyecto Vichada"
       no dice nada del sector.

    2. Si alguna fuente SI expone un campo de sector estructurado (columnas
       'sector__*' del CSV), usa ese campo y no la heuristica.

    3. Decide la regla de inclusion y dejala escrita en la Seccion 4 de la
       tesis. La defendible es: se conservan los proyectos cuyo mecanismo
       declarado opera sobre cobertura boscosa o uso del suelo (AFOLU),
       porque son los unicos para los que la deforestacion municipal es un
       resultado plausible.

    4. Traslada la lista de exclusion a 26_consolidar_fuentes_carbono.py,
       al lado de SOSPECHOSOS_VERRA, y vuelve a correr el pipeline.
    """)
    return 0


if __name__ == "__main__":
    sys.exit(main())
