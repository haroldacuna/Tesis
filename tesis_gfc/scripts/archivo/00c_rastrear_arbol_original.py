"""
00c_rastrear_arbol_original.py

Localiza, dentro del arbol de proyecto que realmente produjo el panel, el
primer artefacto de la cadena en que Antioquia y Atlantico quedan en cero.

CONTEXTO
--------
Consolidacion_DF.ipynb apunta a un BASE_DIR distinto del arbol que se venia
revisando:

    C:\\Users\\USUARIO\\Documents\\Tesis\\tesis_gfc\\data      <- fuente real
    C:\\Users\\USUARIO\\Documents\\Maestria\\Tesis\\tesis_gfc  <- arbol revisado

El arbol revisado esta a medio construir: su loss_by_municipio_year.csv tiene
792 municipios y 108.431 ha nacionales, mientras el panel analizado tiene 1.122
municipios y 5.331.218 ha. El panel no desciende de ahi.

Este script recorre el arbol correcto, ordena por fecha todos los CSV que
contengan COD_DANE, y para cada uno reporta cobertura nacional y de los dos
departamentos. El primero de la cadena cronologica en que Antioquia pasa a cero
es el punto de falla.

Solo LEE. No escribe ni modifica nada.

USO
---
    python 00c_rastrear_arbol_original.py
    python 00c_rastrear_arbol_original.py "D:\\otra\\ruta\\tesis_gfc"
"""

import sys
from datetime import datetime
from pathlib import Path

import pandas as pd

RAIZ_POR_DEFECTO = Path(r"C:\Users\USUARIO\Documents\Tesis\tesis_gfc")
DPTOS = {"05": "ANTIOQUIA", "08": "ATLANTICO"}
COLS_VALOR = ["loss_area_ha", "baseline_forest", "loss_pixels"]


def localizar_raiz(argv: list[str]) -> Path | None:
    if len(argv) > 1:
        ruta = Path(argv[1]).expanduser()
        return ruta if ruta.is_dir() else None

    if RAIZ_POR_DEFECTO.is_dir():
        return RAIZ_POR_DEFECTO

    # Busqueda de respaldo: cualquier carpeta tesis_gfc bajo Documents.
    documentos = Path.home() / "Documents"
    if documentos.is_dir():
        candidatas = [p for p in documentos.rglob("tesis_gfc") if p.is_dir()]
        if candidatas:
            print("  Ruta por defecto no encontrada. Candidatas halladas:")
            for c in candidatas:
                print(f"    {c}")
            return candidatas[0]
    return None


def perfilar(ruta: Path) -> dict | None:
    """Cobertura nacional y departamental de un CSV del pipeline."""
    try:
        df = pd.read_csv(ruta, dtype={"COD_DANE": str}, low_memory=False)
    except Exception:
        return None

    if "COD_DANE" not in df.columns or len(df) == 0:
        return None

    cods = df["COD_DANE"].astype(str).str.zfill(5)
    dptos = cods.str[:2]
    col = next((c for c in COLS_VALOR if c in df.columns), None)

    perfil = {
        "filas": len(df),
        "municipios": cods.nunique(),
        "dptos": dptos.nunique(),
        "variable": col,
        "mtime": datetime.fromtimestamp(ruta.stat().st_mtime),
    }

    if col:
        perfil["total"] = float(df[col].sum())
        for cod, nombre in DPTOS.items():
            sel = dptos == cod
            perfil[f"{nombre}_mun"] = int(cods[sel].nunique())
            perfil[f"{nombre}_val"] = float(df.loc[sel, col].sum())

    return perfil


def main() -> None:
    raiz = localizar_raiz(sys.argv)
    if raiz is None:
        print("No se encontro el arbol de proyecto original.")
        print(f"Se busco en: {RAIZ_POR_DEFECTO}")
        print("Pasa la ruta como argumento:")
        print('  python 00c_rastrear_arbol_original.py "C:\\ruta\\a\\tesis_gfc"')
        return

    print("=" * 78)
    print(f"ARBOL ORIGINAL: {raiz}")
    print("=" * 78)

    csvs = sorted(
        (p for p in (raiz / "data").rglob("*.csv") if p.is_file()),
        key=lambda p: p.stat().st_mtime,
    )
    print(f"  CSV encontrados bajo data/: {len(csvs)}\n")

    perfiles = []
    for ruta in csvs:
        perfil = perfilar(ruta)
        if perfil is None:
            continue
        perfil["ruta"] = ruta
        perfiles.append(perfil)

    if not perfiles:
        print("  Ningun CSV con columna COD_DANE.")
        return

    print("-" * 78)
    print("CADENA CRONOLOGICA (ordenada por fecha de modificacion)")
    print("-" * 78)

    primer_cero = None
    ultimo_sano = None

    for p in perfiles:
        rel = p["ruta"].relative_to(raiz)
        print(f"\n  {rel}")
        print(f"    {p['mtime']:%Y-%m-%d %H:%M}   filas {p['filas']:,}"
              f"   municipios {p['municipios']}   dptos {p['dptos']}")

        if not p["variable"]:
            print("    (sin variable de deforestacion)")
            continue

        print(f"    {p['variable']} nacional: {p['total']:,.1f}")
        ant = p.get("ANTIOQUIA_val", 0.0)
        atl = p.get("ATLANTICO_val", 0.0)
        print(f"    ANTIOQUIA: {p.get('ANTIOQUIA_mun', 0)} municipios, {ant:,.1f}")
        print(f"    ATLANTICO: {p.get('ATLANTICO_mun', 0)} municipios, {atl:,.1f}")

        en_cero = (ant == 0.0 and atl == 0.0 and p["total"] > 0)
        if en_cero:
            print("    >>> ANTIOQUIA Y ATLANTICO EN CERO")
            if primer_cero is None:
                primer_cero = p
        elif ant > 0:
            ultimo_sano = p
            print("    OK: ambos departamentos con datos")

    print("\n" + "=" * 78)
    print("CONCLUSION")
    print("=" * 78)

    if ultimo_sano:
        print(f"\n  Ultimo archivo SANO (Antioquia con datos):")
        print(f"    {ultimo_sano['ruta'].relative_to(raiz)}")
        print(f"    {ultimo_sano['mtime']:%Y-%m-%d %H:%M}"
              f"   Antioquia = {ultimo_sano.get('ANTIOQUIA_val', 0):,.1f}")
    else:
        print("\n  Ningun archivo del arbol tiene datos de Antioquia.")
        print("  El defecto es anterior a todo CSV conservado: esta en la")
        print("  extraccion espacial misma. Hay que re-extraer.")

    if primer_cero:
        print(f"\n  Primer archivo CORRUPTO (ambos departamentos en cero):")
        print(f"    {primer_cero['ruta'].relative_to(raiz)}")
        print(f"    {primer_cero['mtime']:%Y-%m-%d %H:%M}")
        print("\n  >>> El script que produjo este archivo a partir del anterior")
        print("      es donde se introduce el cero. Revisar ese paso: tipicamente")
        print("      un merge que no cruza y un fillna(0) que lo enmascara.")

    if ultimo_sano and primer_cero:
        print(f"\n  Ventana temporal a investigar:")
        print(f"    {ultimo_sano['mtime']:%Y-%m-%d %H:%M}"
              f"  ->  {primer_cero['mtime']:%Y-%m-%d %H:%M}")

    # Rastro del ancestro citado por el notebook.
    enriched = raiz / "data" / "final" / "panel_municipio_year_enriched.csv"
    print(f"\n  panel_municipio_year_enriched.csv (base de Consolidacion_DF.ipynb):")
    print(f"    {'EXISTE' if enriched.exists() else 'NO EXISTE'} en {enriched}")

    print("\n" + "=" * 78)
    print("Diagnostico terminado. Ninguna escritura realizada.")
    print("=" * 78)


if __name__ == "__main__":
    main()
