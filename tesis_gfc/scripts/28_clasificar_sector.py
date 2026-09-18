#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
28_clasificar_sector.py
=======================

Construye (o actualiza) la tabla de decision sectorial que consume
26_consolidar_fuentes_carbono.py a traves de filtro_sectorial.py.

    python 28_clasificar_sector.py

Salida: <raiz>/data/interim/clasificacion_sectorial.csv

Las rutas se anclan a la ubicacion de este archivo (scripts/), NO al
directorio de trabajo. Esto es deliberado: existe una carpeta data/interim
duplicada bajo scripts/ y la ambiguedad ya provoco que el pipeline de Python
y la cadena de R usaran variables de tratamiento distintas.

QUE HACE
--------
Recorre las cuatro fuentes de registros, arma una fila por proyecto, y
propone una clasificacion usando el campo sectorial estructurado cuando
existe (methodology en Gold Standard, tipo en RENARE, scope en Verra) y
palabras clave cuando no (Cercarbono).

La columna que MANDA es 'clasificacion_final', que se edita a mano. Las
columnas 'clasificacion_sugerida' y 'motivo_sugerencia' son solo el trabajo
adelantado.

IDEMPOTENTE
-----------
Si la tabla ya existe, se preservan todas las decisiones manuales previas.
Solo se agregan los proyectos nuevos. Puedes correrlo tantas veces como
quieras sin perder trabajo de revision.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pandas as pd

from filtro_sectorial import (
    CLASIFICACION_FILE,
    DIR_INTERIM,
    ETIQUETA_PENDIENTE,
    RAIZ_PROYECTO,
    avisar_carpeta_sombra,
    clasificar_automatico,
    clave_proyecto,
)

# ---------------------------------------------------------------------------
# Fuentes. Mismas rutas que 26_consolidar_fuentes_carbono.py, pero absolutas.
# Cada entrada trae ademas un patron de busqueda: si el archivo esperado no
# esta, se listan los candidatos que si existen, en vez de fallar mudo.
# ---------------------------------------------------------------------------
FUENTES: dict[str, dict] = {
    "verra": {
        "paths": [DIR_INTERIM / "verra_platts_colombia_con_municipio.csv"],
        "glob": "*verra*",
    },
    "gold_standard": {
        "paths": [
            DIR_INTERIM / "goldstandard_projects_colombia_con_municipio.csv",
            DIR_INTERIM / "goldstandard_coords_corregidas.csv",
        ],
        "glob": "*goldstandard*",
    },
    "renare": {
        "paths": [
            DIR_INTERIM / "renare_solicitudes_colombia.csv",
            DIR_INTERIM / "diagnostics" / "renare_municipio_matching.csv",
        ],
        "glob": "*renare*",
    },
    "cercarbono": {
        "paths": [DIR_INTERIM / "diagnostics" / "cercarbono_municipio_matching.csv"],
        "glob": "*cercarbono*",
    },
}

# Columnas candidatas a nombre de proyecto, en orden de preferencia.
# IMPORTANTE: estas deben coincidir con las que 26_consolidar_fuentes_carbono.py
# usa para llenar 'nombre_proyecto' en los eventos, o la clave de union falla:
#   Verra         -> projectName
#   Gold Standard -> name  (o projectName)
#   RENARE        -> nombre_iniciativa
#   Cercarbono    -> Project Name
COLS_NOMBRE = [
    "projectName", "name", "nombre_iniciativa", "Project Name",
    "nombre_proyecto", "nombre", "titulo", "title", "proyecto",
]

# Campos sectoriales. Se copian al CSV con prefijo 'sector__' para que la
# evidencia quede a la vista de quien revisa.
COLS_SECTOR = [
    "methodology", "programme_of_activities", "tipo", "actividades",
    "sector", "scope", "sectoralScope", "project_type", "projectType",
    "categoria", "category", "protocol",
    # Cercarbono (Projects Report)
    "Sector", "Methodology", "Mitigation type", "Protocol",
]

# Identificacion del municipio, con los nombres que realmente usa cada fuente
COLS_DANE = ["COD_DANE", "cod_dane"]
COLS_MPIO = ["NOMBRE_MPI", "municipio", "nombre_mpi"]

COLUMNAS_SALIDA = [
    "clave", "fuente", "nombre_proyecto", "COD_DANE", "NOMBRE_MPI",
    "clasificacion_sugerida", "motivo_sugerencia",
    "clasificacion_final", "justificacion",
]


def _primera(df: pd.DataFrame, candidatas: list[str]) -> str | None:
    return next((c for c in candidatas if c in df.columns), None)


def _leer(fuente: str, config: dict) -> pd.DataFrame:
    partes = []
    encontrado = False
    for p in config["paths"]:
        if p.exists():
            encontrado = True
            try:
                partes.append(pd.read_csv(p, low_memory=False))
                print(f"    [ok] {p.name}")
            except Exception as exc:
                print(f"    [!] No pude leer {p.name}: {exc}")
        else:
            print(f"    [ ] Falta {p.name}")

    if not encontrado:
        # Busqueda de candidatos, para no dejar al usuario adivinando
        candidatos = sorted(
            list(DIR_INTERIM.glob(config["glob"]))
            + list((DIR_INTERIM / "diagnostics").glob(config["glob"]))
        )
        if candidatos:
            print(f"    Archivos que SI existen y podrian servir para {fuente}:")
            for c in candidatos:
                print(f"       {c.relative_to(RAIZ_PROYECTO)}")
            print("    Ajusta FUENTES en este script con el nombre correcto.")
        else:
            print(f"    No hay ningun archivo que coincida con '{config['glob']}'.")

    return pd.concat(partes, ignore_index=True) if partes else pd.DataFrame()


def construir() -> pd.DataFrame:
    filas = []

    for fuente, config in FUENTES.items():
        print(f"\n--- {fuente} ---")
        df = _leer(fuente, config)
        if df.empty:
            continue

        col_nombre = _primera(df, COLS_NOMBRE)
        if col_nombre is None:
            print(f"    [!] Sin columna de nombre reconocible.")
            print(f"        Columnas disponibles: {list(df.columns)}")
            print(f"        Anade la correcta a COLS_NOMBRE en este script.")
            continue

        trabajo = pd.DataFrame({"nombre_proyecto": df[col_nombre].astype(str)})
        for c in COLS_SECTOR:
            if c in df.columns:
                trabajo[f"sector__{c}"] = df[c]

        col_dane = _primera(df, COLS_DANE)
        trabajo["COD_DANE"] = (
            df[col_dane].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5)
            if col_dane else ""
        )
        col_mpio = _primera(df, COLS_MPIO)
        trabajo["NOMBRE_MPI"] = df[col_mpio] if col_mpio else ""
        trabajo["fuente"] = fuente

        # Una fila por proyecto, no por evento municipio-proyecto
        trabajo = trabajo.drop_duplicates(subset=["fuente", "nombre_proyecto"])

        sugerencias = trabajo.apply(
            lambda r: clasificar_automatico(fuente, r), axis=1, result_type="expand"
        )
        trabajo["clasificacion_sugerida"] = sugerencias[0]
        trabajo["motivo_sugerencia"] = sugerencias[1]
        trabajo["clave"] = [
            clave_proyecto(fuente, n) for n in trabajo["nombre_proyecto"]
        ]

        conteo = trabajo["clasificacion_sugerida"].value_counts()
        print(f"    campo nombre: '{col_nombre}'  |  {len(trabajo)} proyectos unicos")
        print("    " + "  ".join(f"{k}={v}" for k, v in conteo.items()))

        filas.append(trabajo)

    if not filas:
        return pd.DataFrame(columns=COLUMNAS_SALIDA)

    nuevo = pd.concat(filas, ignore_index=True)
    nuevo["clasificacion_final"] = ""
    nuevo["justificacion"] = ""

    cols_sector = sorted(c for c in nuevo.columns if c.startswith("sector__"))
    return nuevo[COLUMNAS_SALIDA + cols_sector]


def fusionar_con_existente(nuevo: pd.DataFrame, ruta: Path) -> pd.DataFrame:
    """Preserva las decisiones manuales ya tomadas."""
    nuevo = nuevo.copy()

    if ruta.exists():
        viejo = pd.read_csv(ruta, dtype=str, encoding="utf-8-sig").fillna("")
        decisiones = viejo[viejo["clasificacion_final"].str.strip() != ""]
        mapa_clas = dict(zip(decisiones["clave"], decisiones["clasificacion_final"]))
        mapa_just = dict(zip(decisiones["clave"], decisiones.get("justificacion", "")))
        nuevo["clasificacion_final"] = nuevo["clave"].map(mapa_clas).fillna("")
        nuevo["justificacion"] = nuevo["clave"].map(mapa_just).fillna("")
        print(f"\nDecisiones manuales preservadas: "
              f"{len(set(nuevo['clave']) & set(mapa_clas))}"
              f"  |  proyectos nuevos: {len(set(nuevo['clave']) - set(viejo['clave']))}")

    # Prellenar SOLO donde la sugerencia es inequivoca. Lo pendiente queda
    # vacio a proposito: el filtro debe fallar si nadie lo reviso.
    vacio = nuevo["clasificacion_final"].str.strip() == ""
    inequivoca = nuevo["clasificacion_sugerida"] != ETIQUETA_PENDIENTE
    nuevo.loc[vacio & inequivoca, "clasificacion_final"] = nuevo.loc[
        vacio & inequivoca, "clasificacion_sugerida"
    ]
    return nuevo


def main() -> int:
    print("Construyendo tabla de decision sectorial")
    print(f"Raiz del proyecto: {RAIZ_PROYECTO}")
    print(f"Datos:             {DIR_INTERIM}")
    avisar_carpeta_sombra()

    if not DIR_INTERIM.exists():
        print(f"\n[X] No existe {DIR_INTERIM}.")
        print("    Este script asume que vive en tesis_gfc/scripts/.")
        return 1

    nuevo = construir()
    if nuevo.empty:
        print("\n[X] No se pudo leer ninguna fuente.")
        return 1

    tabla = fusionar_con_existente(nuevo, CLASIFICACION_FILE)

    CLASIFICACION_FILE.parent.mkdir(parents=True, exist_ok=True)
    tabla.to_csv(CLASIFICACION_FILE, index=False, encoding="utf-8-sig")

    pendientes = (tabla["clasificacion_final"].str.strip() == "").sum()
    por_fuente = tabla.groupby("fuente")["clasificacion_final"].apply(
        lambda s: (s.str.strip() == "").sum()
    )

    print("\n" + "=" * 70)
    print(f"Escrito: {CLASIFICACION_FILE}")
    print(f"Proyectos en la tabla: {len(tabla)}")
    print(f"Pendientes de revision manual: {pendientes}")
    for f, n in por_fuente.items():
        print(f"    {f:<16} {n} pendientes de {(tabla['fuente'] == f).sum()}")
    print("=" * 70)
    print("""
Abre el CSV y llena 'clasificacion_final' con AFOLU o NO_AFOLU en las filas
vacias. Escribe una linea en 'justificacion': esa columna es la que vas a
citar si te preguntan por un municipio especifico en la sustentacion.

Revisa tambien las filas ya prellenadas. La sugerencia se aplico solo donde
el campo estructurado era inequivoco, pero conviene pasar la vista, sobre
todo por Gold Standard (son 12 proyectos, cinco minutos).

Cuando no quede ninguna vacia, corre 26_consolidar_fuentes_carbono.py.
""")
    return 0


if __name__ == "__main__":
    sys.exit(main())
