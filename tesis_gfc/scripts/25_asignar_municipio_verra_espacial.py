"""
25_asignar_municipio_verra_espacial.py

Igual que 14_asignar_municipio_goldstandard_espacial.py, pero para los
proyectos de Verra extraídos por 24_extraer_verra_platts.py, que también
trae latitude/longitude directas — no hace falta matching de texto.

Aprendizajes aplicados de la ronda de Gold Standard (evitar repetir esos
mismos problemas):
  - NO reproyectar a un CRS proyectado (EPSG:3116 falló silenciosamente
    por un problema de pyproj/PROJ_DATA en este entorno) — todo se hace en
    EPSG:4326 directo.
  - NO usar gpd.sjoin_nearest (devolvía NaN en todo sin error) — el
    fallback de "más cercano" es un bucle manual con .distance().
  - 'id'/'projectId' se fuerza a string en todos lados para evitar
    problemas de tipo (float vs int) al reconectar resultados.

USO
---
    python 25_asignar_municipio_verra_espacial.py
"""

from __future__ import annotations

from pathlib import Path

import geopandas as gpd
import pandas as pd
from shapely.geometry import Point

VERRA_INPUT = Path("data/interim/verra_platts_colombia.csv")
MUNICIPIOS_GPKG = Path("data/interim/municipios_clean.gpkg")
MUNICIPIOS_LAYER = "municipios_clean"

OUT_OK = Path("data/interim/verra_platts_colombia_con_municipio.csv")
OUT_INVALID = Path("data/interim/diagnostics/verra_platts_coords_invalidas.csv")
OUT_INVALID.parent.mkdir(parents=True, exist_ok=True)

LAT_MIN, LAT_MAX = -4.5, 13.6
LON_MIN, LON_MAX = -82.0, -66.5


def _en_rango(lat: float, lon: float) -> bool:
    return LAT_MIN <= lat <= LAT_MAX and LON_MIN <= lon <= LON_MAX


def _corregir_coords(lat: float, lon: float) -> tuple[float, float, str] | None:
    """Prueba correcciones comunes (signo de longitud, lat/long invertidos)
    y devuelve la corrección SOLO si exactamente una es válida — igual que
    ya hicimos con Gold Standard. Si el original ya es válido, se devuelve
    tal cual sin marcar corrección."""
    if _en_rango(lat, lon):
        return (lat, lon, "original")

    candidatos = {
        "signo_lon_negado": (lat, -abs(lon)),
        "intercambiado": (lon, lat),
        "intercambiado_y_negado": (lon, -abs(lat)),
    }
    validos = {k: v for k, v in candidatos.items() if _en_rango(*v)}
    if not validos:
        return None

    # Deduplicar por VALOR: si dos métodos distintos (ej. "intercambiado" e
    # "intercambiado_y_negado") producen el mismo par (lat, lon) porque el
    # original ya tenía el signo que la otra transformación habría forzado,
    # eso es UNA sola respuesta válida, no dos en conflicto.
    valores_unicos = {v for v in validos.values()}
    if len(valores_unicos) == 1:
        metodo = next(iter(validos.keys()))
        lat_fix, lon_fix = next(iter(validos.values()))
        return (lat_fix, lon_fix, metodo)
    return None


def main() -> None:
    if not VERRA_INPUT.exists():
        print(f"ERROR: no encuentro {VERRA_INPUT}. Corre primero 24_extraer_verra_platts.py.")
        return
    if not MUNICIPIOS_GPKG.exists():
        print(f"ERROR: no encuentro {MUNICIPIOS_GPKG}. Corre primero 01_prepare_boundaries.py.")
        return

    df = pd.read_csv(VERRA_INPUT, low_memory=False)
    print(f"Total proyectos Verra: {len(df)}")

    if "projectId" in df.columns:
        df["_id_estable"] = df["projectId"].astype(str).str.replace(r"\.0$", "", regex=True)
    else:
        df["_id_estable"] = df.index.astype(str)

    campos_lat = [c for c in ["latitude", "lat"] if c in df.columns]
    campos_lon = [c for c in ["longitude", "lon", "lng"] if c in df.columns]
    if not campos_lat or not campos_lon:
        print(f"ERROR: no encuentro columnas de latitud/longitud. Columnas disponibles: {list(df.columns)}")
        return
    col_lat, col_lon = campos_lat[0], campos_lon[0]

    con_coords = df.dropna(subset=[col_lat, col_lon]).copy()
    print(f"Con {col_lat}/{col_lon} no nulos: {len(con_coords)} / {len(df)}")

    # Corregir signo/orden de coordenadas antes de filtrar por rango — la
    # base de Verra/Platts tiene un patrón sistemático de longitud con
    # signo positivo (debería ser negativo, Colombia siempre está al oeste).
    metodos = []
    lats_fix, lons_fix = [], []
    for lat, lon in zip(con_coords[col_lat], con_coords[col_lon]):
        resultado = _corregir_coords(lat, lon)
        if resultado is None:
            lats_fix.append(lat)
            lons_fix.append(lon)
            metodos.append("sin_corregir")
        else:
            lat_fix, lon_fix, metodo = resultado
            lats_fix.append(lat_fix)
            lons_fix.append(lon_fix)
            metodos.append(metodo)
    con_coords[col_lat] = lats_fix
    con_coords[col_lon] = lons_fix
    con_coords["metodo_correccion_coords"] = metodos

    n_corregidos = (con_coords["metodo_correccion_coords"] != "original").sum() - (con_coords["metodo_correccion_coords"] == "sin_corregir").sum()
    print(f"Coordenadas corregidas automáticamente (signo/orden): {n_corregidos}")
    print(f"  Detalle: {con_coords['metodo_correccion_coords'].value_counts().to_dict()}")

    en_rango = con_coords[
        con_coords[col_lat].between(LAT_MIN, LAT_MAX) & con_coords[col_lon].between(LON_MIN, LON_MAX)
    ].copy()
    fuera_de_rango = con_coords[~con_coords.index.isin(en_rango.index)].copy()

    if not fuera_de_rango.empty:
        print(f"\n*** {len(fuera_de_rango)} filas con coordenadas fuera de rango Colombia ***")
        cols_show = [c for c in ["_id_estable", "projectName", col_lat, col_lon] if c in fuera_de_rango.columns]
        print(fuera_de_rango[cols_show].to_string())
        fuera_de_rango.to_csv(OUT_INVALID, index=False, encoding="utf-8-sig")
        print(f"Guardadas en {OUT_INVALID} para revisar a mano.")

    if en_rango.empty:
        print("\nNo quedaron filas con coordenadas válidas. Nada que cruzar.")
        return

    print(f"\nCargando shapefile de municipios...")
    municipios = gpd.read_file(MUNICIPIOS_GPKG, layer=MUNICIPIOS_LAYER)
    print(f"Municipios cargados: {len(municipios)}")
    if len(municipios) == 0:
        print("*** ERROR: el shapefile de municipios está vacío. Revisa data/raw/boundaries y vuelve a correr 01_prepare_boundaries.py. ***")
        return

    if municipios.crs is None:
        municipios = municipios.set_crs("EPSG:4326")
    elif municipios.crs.to_epsg() != 4326:
        municipios = municipios.to_crs("EPSG:4326")

    n_invalidas = (~municipios.geometry.is_valid).sum()
    if n_invalidas > 0:
        print(f"Reparando {n_invalidas} geometrías inválidas con buffer(0)...")
        municipios = municipios.copy()
        municipios["geometry"] = municipios.geometry.buffer(0)

    print(f"\nCruzando {len(en_rango)} proyectos con el shapefile (punto-en-polígono)...")
    gdf_puntos = gpd.GeoDataFrame(
        en_rango,
        geometry=[Point(lon, lat) for lon, lat in zip(en_rango[col_lon], en_rango[col_lat])],
        crs="EPSG:4326",
    )

    cols_municipio = [c for c in ["COD_DANE", "NOMBRE_MPI", "geometry"] if c in municipios.columns]
    join = gpd.sjoin(gdf_puntos, municipios[cols_municipio], how="left", predicate="intersects")

    n_asignados = join["COD_DANE"].notna().sum()
    print(f"Asignados por intersección directa: {n_asignados}/{len(join)}")

    sin_match = join["COD_DANE"].isna()
    if sin_match.any():
        print(f"\n{sin_match.sum()} punto(s) sin match — buscando municipio más cercano (distancia en grados, sin reproyectar)...")
        ids_sin_match = join.loc[sin_match, "_id_estable"].tolist()
        for pid in ids_sin_match:
            fila_punto = gdf_puntos.loc[gdf_puntos["_id_estable"] == pid]
            if fila_punto.empty:
                continue
            punto = fila_punto.geometry.iloc[0]
            distancias = municipios.geometry.distance(punto).dropna()
            if distancias.empty:
                print(f"  {pid}: no se pudo calcular distancia contra ningún municipio.")
                continue
            pos_min = distancias.idxmin()
            dist_km_aprox = distancias.loc[pos_min] * 111.0
            idx = join.index[join["_id_estable"] == pid][0]
            join.loc[idx, "COD_DANE"] = municipios.loc[pos_min, "COD_DANE"]
            join.loc[idx, "NOMBRE_MPI"] = municipios.loc[pos_min, "NOMBRE_MPI"]
            print(f"  {pid}: asignado a {municipios.loc[pos_min, 'NOMBRE_MPI']} (~{dist_km_aprox:.1f} km aprox., revisar si es razonable)")

    join = join.drop(columns=["geometry", "index_right"], errors="ignore")
    join.to_csv(OUT_OK, index=False, encoding="utf-8-sig")

    print(f"\nGuardado: {OUT_OK}")
    cols_final = [c for c in ["_id_estable", "projectName", "city", "stateProvince", col_lat, col_lon, "COD_DANE", "NOMBRE_MPI"] if c in join.columns]
    print(join[cols_final].to_string())

    n_final = join["COD_DANE"].notna().sum()
    print(f"\nResumen: {n_final}/{len(df)} proyectos Verra con municipio asignado por geometría.")


if __name__ == "__main__":
    main()
