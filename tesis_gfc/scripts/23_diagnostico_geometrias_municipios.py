"""
23_diagnostico_geometrias_municipios.py

.distance() está devolviendo NaN contra los 1122 municipios, incluso sin
ninguna reproyección de por medio — eso ya no encaja con un problema de
PROJ_DATA. Este script aísla el problema al mínimo: revisa el shapefile en
sí, y hace una prueba de distancia con shapely puro (sin pandas/geopandas
de por medio) para saber si el problema es de las geometrías, de la
instalación de shapely/GEOS, o de otra cosa.

USO
---
    python 23_diagnostico_geometrias_municipios.py
"""

from __future__ import annotations

from pathlib import Path

import geopandas as gpd
import shapely
from shapely.geometry import Point

MUNICIPIOS_GPKG = Path("data/interim/municipios_clean.gpkg")
MUNICIPIOS_LAYER = "municipios_clean"


def main() -> None:
    print(f"shapely version: {shapely.__version__}")
    print(f"geopandas version: {gpd.__version__}")
    try:
        import pyproj
        print(f"pyproj version: {pyproj.__version__}")
    except Exception as exc:
        print(f"No se pudo importar pyproj: {exc}")

    if not MUNICIPIOS_GPKG.exists():
        print(f"\nERROR: no encuentro {MUNICIPIOS_GPKG}")
        return

    print(f"\nCargando {MUNICIPIOS_GPKG} ...")
    municipios = gpd.read_file(MUNICIPIOS_GPKG, layer=MUNICIPIOS_LAYER)
    print(f"Filas: {len(municipios)}")
    print(f"CRS: {municipios.crs}")
    print(f"Columnas: {list(municipios.columns)}")

    geom = municipios.geometry
    print(f"\nGeometrías None: {geom.isna().sum()}")
    print(f"Tipos de geometría presentes: {geom.geom_type.value_counts().to_dict()}")
    print(f"Geometrías válidas (is_valid): {geom.is_valid.sum()} / {len(geom)}")
    print(f"Geometrías vacías (is_empty): {geom.is_empty.sum()} / {len(geom)}")

    # Tomar la primera geometría no nula y probar TODO en shapely puro,
    # sin pasar por Series de geopandas/pandas en ningún momento.
    primera_valida = None
    for i, g in enumerate(geom):
        if g is not None and not g.is_empty:
            primera_valida = (i, g)
            break

    if primera_valida is None:
        print("\n*** NINGUNA geometría del shapefile es utilizable (todas None o vacías). Ese es el problema de raíz. ***")
        return

    i, g = primera_valida
    print(f"\nUsando geometría de la fila {i} ({municipios.iloc[i].get('NOMBRE_MPI', '?')}) para pruebas aisladas:")
    print(f"  tipo: {g.geom_type}")
    print(f"  bounds: {g.bounds}")
    print(f"  is_valid: {g.is_valid}")
    print(f"  area (en grados^2, sin reproyectar): {g.area}")

    punto_test = Point(g.centroid.x, g.centroid.y)  # el centroide de su propia geometría: distancia debería ser 0 o casi 0
    print(f"\nPunto de prueba (centroide de la misma geometría): {punto_test}")

    try:
        d = g.distance(punto_test)
        print(f"g.distance(punto_test) con shapely puro = {d}")
        if d != d:  # NaN check
            print("*** shapely puro TAMBIÉN da NaN — el problema es la geometría en sí, no geopandas/pandas. ***")
        else:
            print("shapely puro funciona bien. El problema está en cómo geopandas arma la Series, no en las geometrías.")
    except Exception as exc:
        print(f"g.distance(punto_test) lanzó una excepción: {exc}")

    # Probar también contra un punto arbitrario lejano, para descartar que
    # el problema sea específico de distancia ~0.
    punto_lejano = Point(-74.0, 4.0)
    try:
        d2 = g.distance(punto_lejano)
        print(f"\ng.distance(punto_lejano) = {d2}")
    except Exception as exc:
        print(f"\ng.distance(punto_lejano) lanzó una excepción: {exc}")

    print(
        "\nPégame TODA esta salida — con esto ya no debería hacer falta otra ronda de "
        "prueba y error, vamos a saber exactamente qué está pasando."
    )


if __name__ == "__main__":
    main()
