#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
28_clasificar_sector.py
=======================

Construye (o actualiza) la tabla de decision que consume
26_consolidar_fuentes_carbono.py a traves de filtro_sectorial.py.

    python 28_clasificar_sector.py

Salida: data/interim/clasificacion_sectorial.csv

QUE HACE
--------
Recorre las fuentes de registros, arma una fila por proyecto, y propone DOS
clasificaciones independientes usando campos estructurados cuando existen:

    clasificacion_final   AFOLU | NO_AFOLU   -- eje sectorial, define D3
    clase_redd_final      REDD  | NO_REDD    -- eje de mecanismo, define D1

Las columnas que MANDAN son las dos '_final', que se editan a mano. Las
'_sugerida' y los 'motivo_' son el trabajo adelantado.

NOVEDAD: verra_con_metodologia.csv
----------------------------------
Se agrega como primera fuente de Verra, por delante del interino de Platts.
Trae la columna clase_metodologia que produce 34_enriquecer_verra_metodologia.py
a partir del campo Methodology del export publico de la VCS Project
Database. Con eso los 101 proyectos de Verra quedan clasificados en los dos
ejes por dato del registrador, sin heuristica de nombre.

El orden importa: _leer() concatena y luego se deduplica por
(fuente, nombre_proyecto) conservando la PRIMERA aparicion, asi que el
archivo con metodologia gana sobre el que no la tiene.

IDEMPOTENTE
-----------
Si la tabla ya existe, se preservan todas las decisiones manuales previas
en AMBOS ejes. Solo se agregan los proyectos nuevos. Puedes correrlo
tantas veces como quieras sin perder trabajo de revision.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pandas as pd

from filtro_sectorial import (
    CLASIFICACION_FILE,
    ETIQUETA_PENDIENTE,
    clasificar_automatico,
    clasificar_redd_automatico,
    clave_proyecto,
)

RAIZ = Path(".")

# Mismas rutas que 26_consolidar_fuentes_carbono.py
FUENTES: dict[str, list[Path]] = {
    "verra": [
        # PRIMERO el enriquecido: trae clase_metodologia y status_registro.
        RAIZ / "data/interim/verra_con_metodologia.csv",
        RAIZ / "data/interim/verra_platts_colombia_con_municipio.csv",
    ],
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

# Columnas candidatas a nombre de proyecto, por orden de preferencia
COLS_NOMBRE = [
    "projectName", "name", "nombre_proyecto", "nombre_iniciativa",
    "Project Name", "nombre", "titulo", "title", "proyecto",
]

# Columnas candidatas a campo sectorial. Se copian al CSV con prefijo
# 'sector__' para que queden a la vista de quien revisa.
COLS_SECTOR = [
    "methodology", "programme_of_activities", "tipo", "actividades",
    "sector", "scope", "sectoralScope", "project_type", "projectType",
    "categoria", "category", "protocol",
    # Nuevas, de 34_enriquecer_verra_metodologia.py
    "clase_metodologia", "afolu_activities", "status_registro",
    "estado_registro", "es_wrc",
]

COLUMNAS_SALIDA = [
    "clave", "fuente", "nombre_proyecto", "COD_DANE", "NOMBRE_MPI",
    "clasificacion_sugerida", "motivo_sugerencia",
    "clasificacion_final", "justificacion",
    "clase_redd_sugerida", "motivo_redd",
    "clase_redd_final", "justificacion_redd",
]

# Pares (columna_final, columna_sugerida, columna_justificacion) de los dos
# ejes. Todo lo que sigue los trata de forma simetrica.
EJES = [
    ("clasificacion_final", "clasificacion_sugerida", "justificacion"),
    ("clase_redd_final", "clase_redd_sugerida", "justificacion_redd"),
]


def _leer(paths: list[Path]) -> pd.DataFrame:
    partes = []
    for p in paths:
        if p.exists():
            try:
                partes.append(pd.read_csv(p, low_memory=False))
            except Exception as exc:
                print(f"    [!] No pude leer {p}: {exc}")
        else:
            print(f"    [ ] Falta {p}")
    return pd.concat(partes, ignore_index=True) if partes else pd.DataFrame()


def _primera(df: pd.DataFrame, candidatas: list[str]) -> str | None:
    return next((c for c in candidatas if c in df.columns), None)


def construir() -> pd.DataFrame:
    filas = []

    for fuente, paths in FUENTES.items():
        print(f"\n--- {fuente} ---")
        df = _leer(paths)
        if df.empty:
            print("    sin datos")
            continue

        col_nombre = _primera(df, COLS_NOMBRE)
        if col_nombre is None:
            print(f"    [!] sin columna de nombre; columnas: {list(df.columns)[:15]}")
            continue

        # Renombrar los campos sectoriales al prefijo que esperan
        # clasificar_automatico() y clasificar_redd_automatico()
        trabajo = pd.DataFrame({"nombre_proyecto": df[col_nombre].astype(str)})
        for c in COLS_SECTOR:
            if c in df.columns:
                trabajo[f"sector__{c}"] = df[c]

        trabajo["COD_DANE"] = (
            df["COD_DANE"].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5)
            if "COD_DANE" in df.columns else ""
        )
        trabajo["NOMBRE_MPI"] = df["NOMBRE_MPI"] if "NOMBRE_MPI" in df.columns else ""
        trabajo["fuente"] = fuente

        # Una fila por proyecto, no por evento municipio-proyecto.
        # keep='first' hace que gane el archivo listado primero en FUENTES,
        # que es el que trae los campos estructurados mas ricos.
        trabajo = trabajo.drop_duplicates(subset=["fuente", "nombre_proyecto"], keep="first")

        sug = trabajo.apply(
            lambda r: clasificar_automatico(fuente, r), axis=1, result_type="expand"
        )
        trabajo["clasificacion_sugerida"] = sug[0]
        trabajo["motivo_sugerencia"] = sug[1]

        sug_redd = trabajo.apply(
            lambda r: clasificar_redd_automatico(fuente, r), axis=1, result_type="expand"
        )
        trabajo["clase_redd_sugerida"] = sug_redd[0]
        trabajo["motivo_redd"] = sug_redd[1]

        trabajo["clave"] = [
            clave_proyecto(fuente, n) for n in trabajo["nombre_proyecto"]
        ]

        c1 = trabajo["clasificacion_sugerida"].value_counts()
        c2 = trabajo["clase_redd_sugerida"].value_counts()
        print(f"    {len(trabajo)} proyectos unicos")
        print("      sector: " + "  ".join(f"{k}={v}" for k, v in c1.items()))
        print("      redd:   " + "  ".join(f"{k}={v}" for k, v in c2.items()))

        filas.append(trabajo)

    if not filas:
        return pd.DataFrame(columns=COLUMNAS_SALIDA)

    nuevo = pd.concat(filas, ignore_index=True)
    for final, _, just in EJES:
        nuevo[final] = ""
        nuevo[just] = ""

    # Conservar columnas sector__* al final, para que quien revisa vea la
    # evidencia sin abrir los archivos crudos
    cols_sector = sorted(c for c in nuevo.columns if c.startswith("sector__"))
    return nuevo[COLUMNAS_SALIDA + cols_sector]


def fusionar_con_existente(nuevo: pd.DataFrame, ruta: Path) -> pd.DataFrame:
    """Preserva las decisiones manuales ya tomadas, en los dos ejes."""
    nuevo = nuevo.copy()

    if not ruta.exists():
        # Primera corrida: la sugerencia arranca como valor por defecto
        # SOLO donde es inequivoca. Lo pendiente queda vacio a proposito,
        # para que el filtro falle si nadie lo revisa.
        for final, sugerida, _ in EJES:
            ineq = nuevo[sugerida] != ETIQUETA_PENDIENTE
            nuevo.loc[ineq, final] = nuevo.loc[ineq, sugerida]
        return nuevo

    viejo = pd.read_csv(ruta, dtype=str, encoding="utf-8-sig").fillna("")

    for final, sugerida, just in EJES:
        if final in viejo.columns:
            # Decisiones humanas previas: mandan sobre cualquier sugerencia.
            dec = viejo[viejo[final].str.strip() != ""]
            mapa_val = dict(zip(dec["clave"], dec[final]))
            mapa_just = dict(zip(dec["clave"], dec[just])) if just in dec.columns else {}
            nuevo[final] = nuevo["clave"].map(mapa_val).fillna("")
            nuevo[just] = nuevo["clave"].map(mapa_just).fillna("")
            n_pres = len(set(nuevo["clave"]) & set(mapa_val))
        else:
            # Eje nuevo: la tabla vieja no lo tenia. Nada que preservar.
            print(f"    [i] '{final}' no existia en la tabla previa; se crea desde cero.")
            n_pres = 0

        # Rellenar solo lo que sigue vacio y tiene sugerencia inequivoca
        vacio = nuevo[final].str.strip() == ""
        ineq = nuevo[sugerida] != ETIQUETA_PENDIENTE
        nuevo.loc[vacio & ineq, final] = nuevo.loc[vacio & ineq, sugerida]
        print(f"    {final}: {n_pres} decisiones manuales preservadas, "
              f"{int((vacio & ineq).sum())} prellenadas")

    n_nuevas = len(set(nuevo["clave"]) - set(viejo["clave"]))
    print(f"    proyectos nuevos: {n_nuevas}")
    return nuevo


def main() -> int:
    print("Construyendo tabla de decision (eje sectorial + eje REDD)")
    print(f"Directorio de trabajo: {Path.cwd()}")

    if not (RAIZ / "data/interim").exists():
        print("\n[X] No encuentro data/interim/. Corre esto desde tesis_gfc/.")
        return 1

    if not (RAIZ / "data/interim/verra_con_metodologia.csv").exists():
        print(
            "\n[!] No esta verra_con_metodologia.csv. Sin el, los 101 proyectos\n"
            "    de Verra quedan REVISAR en el eje REDD y D1 no se puede armar.\n"
            "    Corre primero: python scripts/34_enriquecer_verra_metodologia.py\n"
        )

    nuevo = construir()
    if nuevo.empty:
        print("\n[X] No se pudo leer ninguna fuente.")
        return 1

    print("\n--- Fusion con decisiones previas ---")
    tabla = fusionar_con_existente(nuevo, CLASIFICACION_FILE)

    CLASIFICACION_FILE.parent.mkdir(parents=True, exist_ok=True)
    tabla.to_csv(CLASIFICACION_FILE, index=False, encoding="utf-8-sig")

    pend_sec = (tabla["clasificacion_final"].str.strip() == "").sum()
    pend_redd = (tabla["clase_redd_final"].str.strip() == "").sum()

    print("\n" + "=" * 70)
    print(f"Escrito: {CLASIFICACION_FILE}  ({len(tabla)} proyectos)")
    print(f"  Pendientes eje sectorial: {pend_sec}")
    print(f"  Pendientes eje REDD:      {pend_redd}")
    print("=" * 70)

    # Quien queda pendiente, por fuente: orienta donde vale la pena invertir
    # el trabajo manual.
    for final, etq in [("clasificacion_final", "sector"), ("clase_redd_final", "redd")]:
        p = tabla[tabla[final].str.strip() == ""]
        if len(p):
            print(f"\nPendientes de {etq}, por fuente:")
            print(p["fuente"].value_counts().to_string())

    print("""
COMO REVISAR
  Los dos ejes son independientes. Un proyecto ARR es AFOLU y NO_REDD; una
  hidroelectrica es NO_AFOLU y NO_REDD. D1 es la interseccion AFOLU + REDD.

  Para D1 solo bloquean los pendientes del eje REDD. Los pendientes del eje
  sectorial (tipicamente PDBC de RENARE) afectan D3, no D1.

  Escribe una linea en 'justificacion' / 'justificacion_redd': esa columna
  es la que vas a citar si te preguntan por un municipio en la sustentacion.

  Cuando el eje REDD no tenga vacias, corre 26_consolidar_fuentes_carbono.py.
""")
    return 0


if __name__ == "__main__":
    sys.exit(main())
