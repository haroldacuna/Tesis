"""
14_asignar_municipio_goldstandard_espacial.py

12 de los 28 proyectos de Gold Standard Colombia (goldstandard_projects_colombia.csv)
traen latitude/longitude reales. En vez de matchear por texto (propenso a error),
hacemos un cruce espacial punto-en-polígono contra el shapefile de municipios
que ya generaste en 01_prepare_boundaries.py (data/interim/municipios_clean.gpkg).
Esto da el municipio correcto con certeza geométrica, no por coincidencia de texto.

Antes de cruzar, valida el rango de coordenadas (Colombia continental: lat entre
-4.3 y 13.5, lon entre -79.1 y -66.8 aprox.) porque el diagnóstico ya mostró que
al menos 2 filas del CSV tienen latitude/longitude invertidos o con signo
equivocado — esas se separan para revisión manual en vez de asignarlas mal.

USO
---
    python 14_asignar_municipio_goldstandard_espacial.py

Salidas:
    data/interim/goldstandard_projects_colombia_con_municipio.csv
        (proyectos con coordenadas válidas + COD_DANE asignado)
    data/interim/diagnostics/goldstandard_coords_invalidas.csv
        (proyectos con lat/long fuera de rango Colombia, para revisar a mano)
"""

from __future__ import annotations

from pathlib import Path

import geopandas as gpd
import pandas as pd
from shapely.geometry import Point

GS_INPUT = Path("data/interim/goldstandard_projects_colombia.csv")
MUNICIPIOS_GPKG = Path("data/interim/municipios_clean.gpkg")
MUNICIPIOS_LAYER = "municipios_clean"

OUT_OK = Path("data/interim/goldstandard_projects_colombia_con_municipio.csv")
OUT_INVALID = Path("data/interim/diagnostics/goldstandard_coords_invalidas.csv")
OUT_INVALID.parent.mkdir(parents=True, exist_ok=True)

# Rango aproximado de Colombia continental + insular (generoso a propósito).
LAT_MIN, LAT_MAX = -4.5, 13.6
LON_MIN, LON_MAX = -82.0, -66.5


def main() -> None:
    if not GS_INPUT.exists():
        print(f"ERROR: no encuentro {GS_INPUT}. Corre primero 13_extraer_goldstandard.py.")
        return
    if not MUNICIPIOS_GPKG.exists():
        print(f"ERROR: no encuentro {MUNICIPIOS_GPKG}. Corre primero 01_prepare_boundaries.py.")
        return

    gs = pd.read_csv(GS_INPUT)
    print(f"Total proyectos Gold Standard: {len(gs)}")

    con_coords = gs.dropna(subset=["latitude", "longitude"]).copy()
    print(f"Con latitude/longitude no nulos: {len(con_coords)}")

    # Separar coordenadas fuera de rango (posible lat/long invertido o con signo malo).
    en_rango = con_coords[
        con_coords["latitude"].between(LAT_MIN, LAT_MAX)
        & con_coords["longitude"].between(LON_MIN, LON_MAX)
    ].copy()
    fuera_de_rango = con_coords[~con_coords.index.isin(en_rango.index)].copy()

    if not fuera_de_rango.empty:
        print(f"\n*** {len(fuera_de_rango)} filas con coordenadas fuera de rango Colombia ***")
        print(fuera_de_rango[["id", "name", "latitude", "longitude"]].to_string())
        fuera_de_rango.to_csv(OUT_INVALID, index=False, encoding="utf-8-sig")
        print(f"Guardadas en {OUT_INVALID} para revisar/corregir a mano (probablemente lat/long invertidos).")

    if en_rango.empty:
        print("\nNo quedaron filas con coordenadas válidas en rango. Nada que cruzar.")
        return

    print(f"\nCruzando {len(en_rango)} proyectos con el shapefile de municipios...")
    gdf_puntos = gpd.GeoDataFrame(
        en_rango,
        geometry=[Point(lon, lat) for lon, lat in zip(en_rango["longitude"], en_rango["latitude"])],
        crs="EPSG:4326",
    )

    municipios = gpd.read_file(MUNICIPIOS_GPKG, layer=MUNICIPIOS_LAYER)
    if municipios.crs is None:
        print("Aviso: el shapefile de municipios no tiene CRS definido, asumiendo EPSG:4326.")
        municipios = municipios.set_crs("EPSG:4326")
    elif municipios.crs.to_epsg() != 4326:
        municipios = municipios.to_crs("EPSG:4326")

    cols_municipio = [c for c in ["COD_DANE", "NOMBRE_MPI", "geometry"] if c in municipios.columns]
    join = gpd.sjoin(gdf_puntos, municipios[cols_municipio], how="left", predicate="within")

    n_asignados = join["COD_DANE"].notna().sum()
    print(f"Proyectos con municipio asignado por punto-en-polígono: {n_asignados}/{len(join)}")

    if n_asignados < len(join):
        sin_poligono = join[join["COD_DANE"].isna()][["id", "name", "latitude", "longitude"]]
        print(f"\nProyectos cuyo punto no cayó dentro de ningún polígono (revisar, puede ser error de límites/costa):")
        print(sin_poligono.to_string())

    join = join.drop(columns=["geometry", "index_right"], errors="ignore")
    join.to_csv(OUT_OK, index=False, encoding="utf-8-sig")
    print(f"\nGuardado: {OUT_OK}")
    print(
        join[["id", "name", "latitude", "longitude", "COD_DANE", "NOMBRE_MPI"]]
        .to_string()
    )


if __name__ == "__main__":
    main()
