#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
61_clima_linea_base.py  (tarea 1.1)
===================================

Recalcula el clima de LINEA BASE de los 1.122 municipios desde la geometria
oficial del DANE, con una sola fuente por variable y sin cruces por nombre.

Por que (JM p.21 y revision del 30/06):
  * Los 996 municipios originales se unieron por NOMBRE; el propio 30 documenta
    colisiones ("Ciudad Bolivar" vs "Bolivar"). Una colision no deja faltante:
    deja el clima de otro municipio, sin aviso.
  * obtener_clima_gee (06) usa ERA5 diario (~28 km) y, si no hay valor, pasa a
    ERA5-Land (~11 km): la fuente de temperatura depende del tamanio del
    municipio y del anio (ERA5 diario no se actualiza despues de ~2020).

Que calcula (una pasada por variable sobre los 1.122 poligonos):
  temp_2001_c        ERA5-Land mensual, temperature_2m, media 2001 (C)
  prec_2001_mm       CHIRPS diario, suma 2001 (mm)
  temp_2001_2009_c   media de las medias anuales 2001-2009 (ventana de linea base, D4)
  prec_2001_2009_mm  media de las sumas anuales 2001-2009
El modelo usa hoy el anio 2001 (temp_media_c_base, prec_anual_mm_base); la
version 2001-2009 queda como sensibilidad.

Logica espacial: reduceRegions con Reducer.mean(), que pondera cada pixel por
la fraccion que cae dentro del poligono: los municipios pequenios reciben valor
aunque ningun centro de pixel caiga dentro. Cada variable se reduce en su escala
nativa (CHIRPS 5.566 m, ERA5-Land 11.132 m). Las geometrias se simplifican a
200 m antes de subirlas: frente a pixeles de 5-11 km el efecto es despreciable y
evita el limite de tamanio de las solicitudes.

Salidas (NO toca el panel):
  data/interim/clima_linea_base_municipio.csv
  data/interim/diagnostics/clima_comparacion_vs_panel.csv
  y un reporte en pantalla: correlacion, diferencia media y municipios con
  diferencias grandes (candidatos a colision de nombre).

USO (PowerShell, desde tesis_gfc/):
  python scripts\\61_clima_linea_base.py --ee-project <ID>
  python scripts\\61_clima_linea_base.py --solo-comparar      # reusa el CSV ya calculado
"""
from __future__ import annotations

import argparse
import sys
import time
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd

ANIO_BASE = 2001
VENTANA = list(range(2001, 2010))
ESC_CHIRPS = 5566
ESC_ERA5L = 11132
LOTE = 120
TOL_SIMPLIFICAR_M = 200
REINTENTOS = 5

OUT = Path("data/interim/clima_linea_base_municipio.csv")
OUT_CMP = Path("data/interim/diagnostics/clima_comparacion_vs_panel.csv")
GPKG = Path("data/interim/municipios_clean.gpkg")
PANELES = [Path("data/final/panel_con_psm_covariables_reparado.csv"),
           Path("data/final/dataset_consolidado_completo.csv")]
FALTANTES_30 = Path("data/interim/clima_municipios_faltantes_gee.csv")


def verificar_raiz() -> None:
    if not Path("data/.raiz_canonica").exists():
        sys.exit("No estoy en la raiz canonica: corre desde tesis_gfc/.")


def cargar_municipios() -> gpd.GeoDataFrame:
    g = gpd.read_file(GPKG)
    col = next((c for c in g.columns if c.upper() in ("COD_DANE", "MPIO_CDPMP", "CODIGO")), None)
    if col is None:
        sys.exit(f"{GPKG} sin columna de codigo DANE. Columnas: {list(g.columns)}")
    g["COD_DANE"] = g[col].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5)
    g = g[["COD_DANE", "geometry"]].dissolve(by="COD_DANE", as_index=False)
    g = g.to_crs(9377)
    g["geometry"] = g.geometry.simplify(TOL_SIMPLIFICAR_M, preserve_topology=True).buffer(0)
    g = g.to_crs(4326)
    print(f"Municipios: {len(g)} (geometrias simplificadas a {TOL_SIMPLIFICAR_M} m)")
    if len(g) != 1122:
        print(f"[!] Se esperaban 1.122 municipios y hay {len(g)}.")
    return g


def imagenes(ee):
    def chirps_anual(a):
        return (ee.ImageCollection("UCSB-CHG/CHIRPS/DAILY")
                .filterDate(f"{a}-01-01", f"{a + 1}-01-01").select("precipitation").sum())

    def era5l_anual(a):
        return (ee.ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")
                .filterDate(f"{a}-01-01", f"{a + 1}-01-01").select("temperature_2m").mean()
                .subtract(273.15))

    prec = ee.Image.cat([
        chirps_anual(ANIO_BASE).rename("prec_2001_mm"),
        ee.ImageCollection([chirps_anual(a) for a in VENTANA]).mean().rename("prec_2001_2009_mm"),
    ])
    temp = ee.Image.cat([
        era5l_anual(ANIO_BASE).rename("temp_2001_c"),
        ee.ImageCollection([era5l_anual(a) for a in VENTANA]).mean().rename("temp_2001_2009_c"),
    ])
    return prec, temp


def reducir_lote(ee, img, escala, gdf_lote) -> list[dict]:
    fc = ee.FeatureCollection([
        ee.Feature(ee.Geometry(geom.__geo_interface__), {"COD_DANE": cod})
        for cod, geom in zip(gdf_lote["COD_DANE"], gdf_lote.geometry)])
    for intento in range(1, REINTENTOS + 1):
        try:
            res = img.reduceRegions(collection=fc, reducer=ee.Reducer.mean(),
                                    scale=escala, tileScale=4).getInfo()
            return [f["properties"] for f in res["features"]]
        except Exception as e:  # noqa: BLE001
            espera = 15 * intento
            print(f"    intento {intento}/{REINTENTOS} fallo ({str(e)[:90]}); espero {espera} s", flush=True)
            time.sleep(espera)
    raise RuntimeError("Lote sin resultado tras varios intentos")


def respaldo_era5(ee, g_na) -> list[dict]:
    """Municipios sin valor en las grillas terrestres (San Andres y Providencia):
    ERA5 mensual (~28 km, cubre mar y tierra). Solo 2001-2009: la coleccion
    ECMWF/ERA5/MONTHLY llega hasta 2020."""
    col = ee.ImageCollection("ECMWF/ERA5/MONTHLY")

    def anual(a):
        c = col.filterDate(f"{a}-01-01", f"{a + 1}-01-01")
        return ee.Image.cat([c.select("mean_2m_air_temperature").mean().subtract(273.15).rename("t"),
                             c.select("total_precipitation").sum().multiply(1000).rename("p")])

    img = ee.Image.cat([
        anual(ANIO_BASE).select("t").rename("temp_2001_c"),
        anual(ANIO_BASE).select("p").rename("prec_2001_mm"),
        ee.ImageCollection([anual(a) for a in VENTANA]).mean().select("t").rename("temp_2001_2009_c"),
        ee.ImageCollection([anual(a) for a in VENTANA]).mean().select("p").rename("prec_2001_2009_mm"),
    ])
    return reducir_lote(ee, img, 27830, g_na)


def calcular(proyecto: str | None) -> pd.DataFrame:
    import ee
    try:
        ee.Initialize(project=proyecto) if proyecto else ee.Initialize()
    except Exception:  # noqa: BLE001
        ee.Authenticate()
        ee.Initialize(project=proyecto) if proyecto else ee.Initialize()
    g = cargar_municipios()
    prec, temp = imagenes(ee)
    filas = {}
    lotes = [g.iloc[i:i + LOTE] for i in range(0, len(g), LOTE)]
    for k, lote in enumerate(lotes, 1):
        print(f"  lote {k}/{len(lotes)} ({len(lote)} municipios)", flush=True)
        for img, esc in ((prec, ESC_CHIRPS), (temp, ESC_ERA5L)):
            for p in reducir_lote(ee, img, esc, lote):
                filas.setdefault(p["COD_DANE"], {"COD_DANE": p["COD_DANE"]}).update(
                    {kk: v for kk, v in p.items() if kk != "COD_DANE"})
    df = pd.DataFrame(filas.values()).sort_values("COD_DANE")
    cols = ["COD_DANE", "temp_2001_c", "prec_2001_mm", "temp_2001_2009_c", "prec_2001_2009_mm"]
    df = df.reindex(columns=cols)
    df["fuente"] = "ERA5-Land mensual (temp) + CHIRPS diario (prec); geometria DANE; reduceRegions ponderado"
    ## Respaldo para municipios fuera de las grillas terrestres (islas)
    sin = df.loc[df[cols[1:]].isna().any(axis=1), "COD_DANE"].tolist()
    if sin:
        print(f"Respaldo ERA5 mensual (mar y tierra) para: {sin}")
        for p in respaldo_era5(ee, g[g["COD_DANE"].isin(sin)]):
            i = df.index[df["COD_DANE"] == p["COD_DANE"]]
            for c in cols[1:]:
                if p.get(c) is not None:
                    df.loc[i, c] = p[c]
            df.loc[i, "fuente"] = "ERA5 mensual (temp y prec; respaldo para islas); geometria DANE"
        print(df[df["COD_DANE"].isin(sin)][cols].round(2).to_string(index=False))
    nas = df[cols[1:]].isna().sum()
    print("NA por variable:", nas.to_dict())
    if nas.any():
        print("[!] Municipios con NA:", df.loc[df[cols[1:]].isna().any(axis=1), "COD_DANE"].tolist())
    OUT.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(OUT, index=False, encoding="utf-8-sig")
    print(f"Guardado: {OUT} ({len(df)} filas)")
    return df


def valores_panel() -> pd.DataFrame:
    ruta = next((p for p in PANELES if p.exists()), None)
    if ruta is None:
        sys.exit("No encuentro el panel para comparar: " + ", ".join(map(str, PANELES)))
    cab = pd.read_csv(ruta, nrows=0).columns
    if {"temp_media_c_base", "prec_anual_mm_base"} <= set(cab):
        p = pd.read_csv(ruta, usecols=["COD_DANE", "temp_media_c_base", "prec_anual_mm_base"],
                        dtype={"COD_DANE": str}, low_memory=False)
        p = p.rename(columns={"temp_media_c_base": "temp_viejo", "prec_anual_mm_base": "prec_viejo"})
    else:
        col_anio = "year" if "year" in cab else "anio"
        p = pd.read_csv(ruta, usecols=["COD_DANE", col_anio, "temp_media_c", "prec_anual_mm"],
                        dtype={"COD_DANE": str}, low_memory=False)
        p = p[p[col_anio] == ANIO_BASE].rename(columns={"temp_media_c": "temp_viejo",
                                                        "prec_anual_mm": "prec_viejo"})
    p["COD_DANE"] = p["COD_DANE"].str.replace(r"\.0$", "", regex=True).str.zfill(5)
    print(f"Valores actuales tomados de {ruta}")
    return p.groupby("COD_DANE", as_index=False)[["temp_viejo", "prec_viejo"]].first()


def comparar(nuevo: pd.DataFrame) -> None:
    viejo = valores_panel()
    c = nuevo.merge(viejo, on="COD_DANE", how="left")
    if FALTANTES_30.exists():
        f30 = pd.read_csv(FALTANTES_30, dtype={"COD_DANE": str})["COD_DANE"].str.zfill(5).unique()
        c["recalculado_en_30"] = c["COD_DANE"].isin(f30)
    else:
        c["recalculado_en_30"] = np.nan
    c["dif_temp"] = c["temp_2001_c"] - c["temp_viejo"]
    c["dif_prec_pct"] = 100 * (c["prec_2001_mm"] - c["prec_viejo"]) / c["prec_viejo"]
    ## Diferencias "grandes": fuera de 4 desviaciones robustas (MAD) de la diferencia tipica
    for col in ("dif_temp", "dif_prec_pct"):
        med = c[col].median(); mad = 1.4826 * (c[col] - med).abs().median()
        c[f"{col}_atipica"] = (c[col] - med).abs() > 4 * max(mad, 1e-9)
    c["revisar"] = c["dif_temp_atipica"] | c["dif_prec_pct_atipica"]
    OUT_CMP.parent.mkdir(parents=True, exist_ok=True)
    c.to_csv(OUT_CMP, index=False, encoding="utf-8-sig")

    print("\n" + "=" * 72 + "\nCOMPARACION CON LOS VALORES ACTUALES DEL PANEL (anio 2001)\n" + "=" * 72)
    ok = c.dropna(subset=["temp_viejo", "prec_viejo", "temp_2001_c", "prec_2001_mm"])
    print(f"Municipios comparables: {len(ok)} de {len(c)}")
    for var, nuevo_c, viejo_c, uni in (("Temperatura", "temp_2001_c", "temp_viejo", "C"),
                                       ("Precipitacion", "prec_2001_mm", "prec_viejo", "mm")):
        r = np.corrcoef(ok[nuevo_c], ok[viejo_c])[0, 1]
        d = ok[nuevo_c] - ok[viejo_c]
        print(f"  {var:13s} correlacion {r:.4f} | dif. media {d.mean():+.2f} {uni} | "
              f"dif. mediana {d.median():+.2f} | dif. abs. media {d.abs().mean():.2f}")
    if c["recalculado_en_30"].notna().any():
        for grupo, sub in ok.groupby("recalculado_en_30"):
            et = "126 recalculados en el 30" if grupo else "996 unidos por nombre"
            print(f"  [{et}] dif. abs. media temp {sub['dif_temp'].abs().mean():.2f} C | "
                  f"prec {sub['dif_prec_pct'].abs().mean():.1f} %")
    rev = c[c["revisar"]].sort_values("dif_temp", key=lambda s: s.abs(), ascending=False)
    print(f"\nMunicipios con diferencia atipica (candidatos a colision de nombre): {len(rev)}")
    if len(rev):
        print(rev[["COD_DANE", "temp_viejo", "temp_2001_c", "dif_temp", "prec_viejo", "prec_2001_mm",
                   "dif_prec_pct", "recalculado_en_30"]].head(30).round(2).to_string(index=False))
    print(f"\nGuardado: {OUT_CMP}")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--ee-project", default=None)
    ap.add_argument("--solo-comparar", action="store_true")
    a = ap.parse_args()
    verificar_raiz()
    if a.solo_comparar:
        if not OUT.exists():
            sys.exit(f"No existe {OUT}: corre primero sin --solo-comparar.")
        nuevo = pd.read_csv(OUT, dtype={"COD_DANE": str})
    else:
        nuevo = calcular(a.ee_project)
    comparar(nuevo)
    return 0


if __name__ == "__main__":
    sys.exit(main())
