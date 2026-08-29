"""
20_corregir_coords_goldstandard.py

2 de los 12 proyectos de Gold Standard con coordenadas quedaron fuera de
rango de Colombia (detectado por 14_asignar_municipio_goldstandard_espacial.py).
Inspección manual confirmó que son errores de captura de datos típicos:
lat/long intercambiados, o el signo de la longitud sin el negativo
(Colombia siempre tiene longitud negativa).

Este script prueba, para cada fila inválida, las 4 combinaciones plausibles:
  1. tal cual (ya sabemos que falla, pero se prueba por completitud)
  2. intercambiar lat <-> lon
  3. negar el signo de la longitud
  4. intercambiar Y negar

Solo acepta una corrección si EXACTAMENTE UNA de las combinaciones cae
dentro del rango de Colombia — si más de una combinación es válida, no
adivina: lo deja para revisión manual (evita "corregir" con confianza falsa).

Tras corregir, vuelve a intentar el cruce punto-en-polígono contra
data/interim/municipios_clean.gpkg, igual que el script 14.

USO
---
    python 20_corregir_coords_goldstandard.py
"""

from __future__ import annotations

from pathlib import Path

import geopandas as gpd
import pandas as pd
from shapely.geometry import Point

IN_INVALIDAS = Path("data/interim/diagnostics/goldstandard_coords_invalidas.csv")
MUNICIPIOS_GPKG = Path("data/interim/municipios_clean.gpkg")
MUNICIPIOS_LAYER = "municipios_clean"
OUT_CORREGIDAS = Path("data/interim/goldstandard_coords_corregidas.csv")
OUT_SIN_RESOLVER = Path("data/interim/diagnostics/goldstandard_coords_sin_resolver.csv")

LAT_MIN, LAT_MAX = -4.5, 13.6
LON_MIN, LON_MAX = -82.0, -66.5


def _en_rango(lat: float, lon: float) -> bool:
    return LAT_MIN <= lat <= LAT_MAX and LON_MIN <= lon <= LON_MAX


def _candidatos(lat: float, lon: float) -> dict[str, tuple[float, float]]:
    return {
        "original": (lat, lon),
        "intercambiado": (lon, lat),
        "signo_lon_negado": (lat, -abs(lon)),
        "intercambiado_y_negado": (lon, -abs(lat)),
    }


def main() -> None:
    if not IN_INVALIDAS.exists():
        print(f"ERROR: no encuentro {IN_INVALIDAS}. Corre primero 14_asignar_municipio_goldstandard_espacial.py.")
        return

    df = pd.read_csv(IN_INVALIDAS)
    print(f"Filas a intentar corregir: {len(df)}")

    resueltas = []
    sin_resolver = []

    for _, row in df.iterrows():
        candidatos = _candidatos(row["latitude"], row["longitude"])
        validos = {k: v for k, v in candidatos.items() if _en_rango(*v)}

        if len(validos) == 1:
            metodo, (lat_fix, lon_fix) = next(iter(validos.items()))
            print(f"  id={row['id']} ({row['name'][:50]}): corregido via '{metodo}' -> lat={lat_fix:.4f}, lon={lon_fix:.4f}")
            fila = row.to_dict()
            fila["latitude"] = lat_fix
            fila["longitude"] = lon_fix
            fila["metodo_correccion"] = metodo
            resueltas.append(fila)
        else:
            print(f"  id={row['id']} ({row['name'][:50]}): {len(validos)} combinaciones válidas, no se puede decidir automáticamente -> revisión manual")
            sin_resolver.append(row.to_dict())

    if sin_resolver:
        pd.DataFrame(sin_resolver).to_csv(OUT_SIN_RESOLVER, index=False, encoding="utf-8-sig")
        print(f"\nSin resolver automáticamente: {OUT_SIN_RESOLVER}")

    if not resueltas:
        print("\nNada que cruzar espacialmente.")
        return

    corregidas_df = pd.DataFrame(resueltas)
    corregidas_df.to_csv(OUT_CORREGIDAS, index=False, encoding="utf-8-sig")

    if not MUNICIPIOS_GPKG.exists():
        print(f"\nAviso: no encuentro {MUNICIPIOS_GPKG}, no se puede hacer el cruce espacial ahora. Coordenadas corregidas guardadas de todas formas.")
        return

    gdf_puntos = gpd.GeoDataFrame(
        corregidas_df,
        geometry=[Point(lon, lat) for lon, lat in zip(corregidas_df["longitude"], corregidas_df["latitude"])],
        crs="EPSG:4326",
    )
    municipios = gpd.read_file(MUNICIPIOS_GPKG, layer=MUNICIPIOS_LAYER)
    if municipios.crs is None:
        municipios = municipios.set_crs("EPSG:4326")
    elif municipios.crs.to_epsg() != 4326:
        municipios = municipios.to_crs("EPSG:4326")

    cols_municipio = [c for c in ["COD_DANE", "NOMBRE_MPI", "geometry"] if c in municipios.columns]

    # "within" es estricto: si el punto cae justo sobre o muy cerca de un borde
    # (por imprecisión de las coordenadas originales, o huecos/solapes en la
    # topología del shapefile), no encuentra nada. "intersects" es más laxo.
    join = gpd.sjoin(gdf_puntos, municipios[cols_municipio], how="left", predicate="intersects")

    # Para lo que siga sin match ni con "intersects": asignar el municipio más
    # cercano (vecino más próximo), útil cuando el punto cae justo afuera del
    # polígono por un margen pequeño. Se marca explícitamente como aproximado.
    #
    # OJO: usamos la columna 'id' (estable) para reconectar resultados en vez
    # del índice de pandas — sjoin_nearest no garantiza preservar el índice
    # original entre versiones de geopandas, y confiar en él produce falsos
    # "no encontrado" aunque el cruce sí haya funcionado internamente.
    sin_match = join["COD_DANE"].isna()
    if sin_match.any():
        print(f"\n{sin_match.sum()} punto(s) sin match ni con 'intersects' — buscando municipio más cercano...")

        n_invalidas = (~municipios.geometry.is_valid).sum()
        if n_invalidas > 0:
            print(f"  Aviso: {n_invalidas} geometrías inválidas en el shapefile de municipios, reparando con buffer(0)...")
            municipios = municipios.copy()
            municipios["geometry"] = municipios.geometry.buffer(0)

        # NOTA: se abandonó la reproyección a EPSG:3116 (CRS de Colombia) —
        # en este entorno .distance() devolvía NaN contra TODOS los
        # municipios sin excepción, lo cual apunta a un problema de pyproj
        # en Windows (archivos de rejilla/PROJ_DATA faltantes para esa
        # transformación específica), no a un problema de los datos.
        # En su lugar, calculamos distancia directo en EPSG:4326 (grados,
        # sin reproyectar) y convertimos a km de forma aproximada
        # (1 grado ~ 111 km). No es precisión de agrimensor, pero sobra
        # para decidir cuál es el municipio más cercano y para detectar si
        # el resultado es sospechosamente lejano.
        join["id"] = join["id"].astype(str).str.replace(r"\.0$", "", regex=True)
        gdf_puntos["id"] = gdf_puntos["id"].astype(str).str.replace(r"\.0$", "", regex=True)

        ids_sin_match = join.loc[sin_match, "id"].tolist()
        print(f"  IDs a buscar: {ids_sin_match}")

        for pid in ids_sin_match:
            idx_candidatos = join.index[join["id"] == pid]
            if len(idx_candidatos) == 0:
                continue
            idx = idx_candidatos[0]

            fila_punto = gdf_puntos.loc[gdf_puntos["id"] == pid]
            if fila_punto.empty:
                print(f"  id={pid}: no encontré el punto en gdf_puntos (revisar manualmente).")
                continue
            punto = fila_punto.geometry.iloc[0]  # ya en EPSG:4326, sin reproyectar

            distancias_grados = municipios.geometry.distance(punto)
            distancias_grados = distancias_grados.dropna()
            if distancias_grados.empty:
                print(f"  id={pid}: .distance() devolvió NaN incluso sin reproyectar — revisar geometrías del shapefile directamente.")
                continue

            pos_min = distancias_grados.idxmin()
            dist_km_aprox = distancias_grados.loc[pos_min] * 111.0
            join.loc[idx, "COD_DANE"] = municipios.loc[pos_min, "COD_DANE"]
            join.loc[idx, "NOMBRE_MPI"] = municipios.loc[pos_min, "NOMBRE_MPI"]
            join.loc[idx, "metodo_correccion"] = (
                str(join.loc[idx, "metodo_correccion"]) + f" + mas_cercano_sin_reproyectar (~{dist_km_aprox:.1f} km aprox.)"
            )
            print(f"  id={pid}: asignado a {municipios.loc[pos_min, 'NOMBRE_MPI']} (~{dist_km_aprox:.1f} km aprox.)")

    join = join.drop(columns=["geometry", "index_right"], errors="ignore")
    join.to_csv(OUT_CORREGIDAS, index=False, encoding="utf-8-sig")

    print(f"\nMunicipio asignado tras corrección:")
    print(join[["id", "name", "latitude", "longitude", "metodo_correccion", "COD_DANE", "NOMBRE_MPI"]].to_string())
    print(f"\nGuardado: {OUT_CORREGIDAS}")


if __name__ == "__main__":
    main()