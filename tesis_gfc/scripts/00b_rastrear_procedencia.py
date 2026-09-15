"""
00b_rastrear_procedencia.py

Establece de cual de los arboles data/ del proyecto salio realmente el panel
usado en el analisis econometrico.

CONTEXTO
--------
El diagnostico 00 descarto las hipotesis de geometrias invalidas y de perdida
de ceros a la izquierda, y dejo tres hechos incompatibles entre si:

  - data/interim/baseline_forest.csv esta VACIO (0 filas), escrito por el
    fallback _write_empty_outputs() de 04_merge_covariates.py.
  - panel_analisis_did.rds SI tiene baseline_forest con valores reales en 974
    de 1.122 municipios.
  - data/interim/loss_by_municipio_year.csv SI contiene Antioquia (7.205,1 ha)
    y Atlantico (536,1 ha), pero el panel los tiene en 0,00 los 24 anios.

Un intermedio vacio no puede producir un panel poblado. Luego el panel se
construyo desde otro arbol data/. El proyecto tiene cuatro, incluido uno
dentro de .claude/worktrees/.

Este script inventaria los cuatro, compara fechas de modificacion y contrasta
los valores de Antioquia y Atlantico en cada uno, para determinar cual es la
fuente real y si alguno tiene datos correctos.

Solo LEE. No escribe ni modifica nada.

USO
---
    python 00b_rastrear_procedencia.py
"""

from datetime import datetime
from pathlib import Path

import pandas as pd

DPTOS = {"05": "ANTIOQUIA", "08": "ATLANTICO"}

# Archivos clave del pipeline, en orden de dependencia.
ARCHIVOS_CLAVE = [
    ("interim", "municipios_clean.gpkg"),
    ("interim", "loss_by_municipio_year.csv"),
    ("interim", "loss_by_municipio.csv"),
    ("interim", "baseline_forest.csv"),
    ("final", "panel_municipal.csv"),
    ("final", "panel_con_psm_covariables.csv"),
    ("final", "panel_con_tratamiento_actualizado.csv"),
]


def encontrar_raiz() -> Path:
    origenes = [Path(__file__).resolve().parent, Path.cwd().resolve()]
    for origen in origenes:
        for c in [origen, *origen.parents]:
            if (c / "data").is_dir() and (c / "scripts").is_dir():
                return c
    for origen in origenes:
        for c in [origen, *origen.parents]:
            if (c / "data").is_dir():
                return c
    return Path.cwd().resolve()


def listar_arboles(raiz: Path) -> list[Path]:
    """Todo directorio que contenga un subdirectorio data/."""
    arboles = set()
    if (raiz / "data").is_dir():
        arboles.add(raiz)
    for p in raiz.rglob("data"):
        if p.is_dir() and p.parent != p:
            arboles.add(p.parent)
    return sorted(arboles, key=lambda p: len(str(p)))


def resumir_csv(ruta: Path) -> dict:
    """Filas, cobertura de Antioquia/Atlantico y suma de la variable relevante."""
    info = {"filas": None, "cols": None}
    try:
        df = pd.read_csv(ruta, dtype={"COD_DANE": str}, low_memory=False)
    except Exception as exc:
        info["error"] = str(exc)[:60]
        return info

    info["filas"] = len(df)
    info["cols"] = len(df.columns)
    if len(df) == 0 or "COD_DANE" not in df.columns:
        return info

    dptos = df["COD_DANE"].astype(str).str.zfill(5).str[:2]
    info["n_municipios"] = df["COD_DANE"].nunique()

    col_valor = next(
        (c for c in ["loss_area_ha", "baseline_forest"] if c in df.columns), None
    )
    for cod, nombre in DPTOS.items():
        sel = dptos == cod
        info[f"{nombre}_filas"] = int(sel.sum())
        info[f"{nombre}_mun"] = int(df.loc[sel, "COD_DANE"].nunique())
        if col_valor:
            info[f"{nombre}_{col_valor}"] = float(df.loc[sel, col_valor].sum())
    if col_valor:
        info["variable"] = col_valor
        info["total_nacional"] = float(df[col_valor].sum())
    return info


def inventariar(arboles: list[Path], raiz: Path) -> None:
    print("=" * 78)
    print("INVENTARIO DE LOS ARBOLES data/")
    print("=" * 78)

    for arbol in arboles:
        etiqueta = "." if arbol == raiz else str(arbol.relative_to(raiz))
        print(f"\n{'-' * 78}\nARBOL: {etiqueta}\n{'-' * 78}")

        for subdir, nombre in ARCHIVOS_CLAVE:
            ruta = arbol / "data" / subdir / nombre
            if not ruta.exists():
                continue

            st = ruta.stat()
            fecha = datetime.fromtimestamp(st.st_mtime).strftime("%Y-%m-%d %H:%M")
            print(f"\n  {subdir}/{nombre}")
            print(f"    {st.st_size:>15,} bytes   modificado {fecha}")

            if nombre.endswith(".csv"):
                info = resumir_csv(ruta)
                if "error" in info:
                    print(f"    error al leer: {info['error']}")
                    continue
                if info["filas"] == 0:
                    print("    >>> VACIO (fallback _write_empty_outputs)")
                    continue
                print(f"    filas: {info['filas']:,}  columnas: {info['cols']}"
                      f"  municipios: {info.get('n_municipios', '?')}")
                var = info.get("variable")
                for cod, dnom in DPTOS.items():
                    mun = info.get(f"{dnom}_mun", 0)
                    linea = f"    {dnom}: {mun} municipios"
                    if var:
                        val = info.get(f"{dnom}_{var}", 0.0)
                        linea += f", {var} = {val:,.1f} ha"
                    print(linea)
                    if mun == 0:
                        print(f"      >>> {dnom} AUSENTE en este archivo")
                if var:
                    print(f"    total nacional {var}: {info['total_nacional']:,.1f} ha")


def contrastar_con_panel(arboles: list[Path], raiz: Path) -> None:
    """El script 01 de R lee data/final/panel_con_psm_covariables.csv. Se
    identifica cual copia coincide con lo que quedo en el panel analizado:
    Antioquia y Atlantico en cero, y baseline_forest poblado en 974
    municipios."""
    print("\n" + "=" * 78)
    print("CONTRASTE CON LA HUELLA DEL PANEL ANALIZADO")
    print("=" * 78)
    print("  Huella buscada: Antioquia y Atlantico con loss_area_ha = 0,00 y")
    print("  baseline_forest = 0, con el resto del pais poblado.\n")

    candidatos = []
    for arbol in arboles:
        ruta = arbol / "data" / "final" / "panel_con_psm_covariables.csv"
        if not ruta.exists():
            continue

        etiqueta = "." if arbol == raiz else str(arbol.relative_to(raiz))
        try:
            df = pd.read_csv(ruta, dtype={"COD_DANE": str}, low_memory=False)
        except Exception as exc:
            print(f"  {etiqueta}: error al leer ({str(exc)[:50]})")
            continue

        dptos = df["COD_DANE"].astype(str).str.zfill(5).str[:2]
        sosp = dptos.isin(DPTOS)

        loss_sosp = df.loc[sosp, "loss_area_ha"].sum() if "loss_area_ha" in df else None
        loss_resto = df.loc[~sosp, "loss_area_ha"].sum() if "loss_area_ha" in df else None
        bf_sosp = df.loc[sosp, "baseline_forest"].sum() if "baseline_forest" in df else None
        n_bf_pos = int((df.drop_duplicates("COD_DANE")["baseline_forest"] > 0).sum()) \
            if "baseline_forest" in df else None

        coincide = (loss_sosp is not None and loss_sosp == 0
                    and loss_resto is not None and loss_resto > 0)

        print(f"  {etiqueta}")
        print(f"    municipios: {df['COD_DANE'].nunique()}   filas: {len(df):,}")
        if loss_sosp is not None:
            print(f"    loss_area_ha Antioquia+Atlantico : {loss_sosp:,.1f} ha")
            print(f"    loss_area_ha resto del pais      : {loss_resto:,.1f} ha")
        if bf_sosp is not None:
            print(f"    baseline_forest Ant+Atl          : {bf_sosp:,.1f}")
            print(f"    municipios con baseline_forest>0 : {n_bf_pos}")
        print(f"    >>> {'COINCIDE con el panel analizado' if coincide else 'no coincide'}")
        print()

        candidatos.append((etiqueta, coincide, loss_sosp, n_bf_pos))

    if not candidatos:
        print("  No se encontro panel_con_psm_covariables.csv en ningun arbol.")
        print("  Buscar manualmente:")
        for p in sorted(Path(raiz).rglob("panel_con_psm_covariables.csv")):
            print(f"    {p.relative_to(raiz)}")
        return

    sanos = [c for c in candidatos if not c[1] and c[2] is not None and c[2] > 0]
    if sanos:
        print("  >>> Existe al menos una copia CON datos de Antioquia/Atlantico:")
        for etiqueta, _, loss, _ in sanos:
            print(f"      {etiqueta}  ({loss:,.1f} ha)")
        print("      Verificar su fecha y, si es posterior y consistente, re-correr")
        print("      01_preparar_datos_did.R apuntando a esa copia.")
    else:
        print("  >>> Ninguna copia tiene datos de Antioquia/Atlantico.")
        print("      La extraccion espacial debe re-ejecutarse desde cero, con")
        print("      el raster de treecover presente y sin fallbacks silenciosos.")


def verificar_rasters(arboles: list[Path], raiz: Path) -> None:
    print("\n" + "=" * 78)
    print("RASTERS DE ENTRADA")
    print("=" * 78)
    print("  baseline_forest.csv vacio implica que 04_merge_covariates.py no")
    print("  encontro el raster de treecover. Se verifica su presencia.\n")

    encontrado = False
    for arbol in arboles:
        for patron in ["*treecover*", "*lossyear*", "*datamask*"]:
            for ruta in (arbol / "data" / "raw").rglob(patron):
                if ruta.is_file():
                    encontrado = True
                    st = ruta.stat()
                    fecha = datetime.fromtimestamp(st.st_mtime).strftime("%Y-%m-%d %H:%M")
                    marca = "  <-- VACIO" if st.st_size == 0 else ""
                    print(f"  {ruta.relative_to(raiz)}")
                    print(f"    {st.st_size:>15,} bytes   {fecha}{marca}")

    if not encontrado:
        print("  >>> NO se encontro ningun raster de Hansen en el proyecto.")
        print("      Sin treecover no hay baseline_forest, y sin lossyear no hay")
        print("      perdida anual. Hay que re-descargarlos antes de nada.")


if __name__ == "__main__":
    raiz = encontrar_raiz()
    print(f"Raiz del proyecto: {raiz}\n")
    arboles = listar_arboles(raiz)
    print(f"Arboles data/ detectados: {len(arboles)}")
    for a in arboles:
        print(f"  {'.' if a == raiz else a.relative_to(raiz)}")

    inventariar(arboles, raiz)
    contrastar_con_panel(arboles, raiz)
    verificar_rasters(arboles, raiz)

    print("\n" + "=" * 78)
    print("Diagnostico terminado. Ninguna escritura realizada.")
    print("=" * 78)
