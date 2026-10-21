"""
72_distancia_borde.py

RDD espacial en el borde de los poligonos REDD+ (tarea 0.2 del plan,
comentario de Jorge: "aqui tienen los elementos para correr un RDD").

Este script solo construye la VARIABLE DE ASIGNACION; la estimacion va en
73_rdd_espacial.R (rdrobust).

    d_i = + distancia del centroide de la celda al borde del poligono, si esta dentro
          - distancia al borde, si esta fuera

El borde es el de los proyectos de deforestacion evitada (AD) de las cohortes
del analisis de celdas del 56 (2013, 2014, 2015, 2017, 2018), para que el RDD
y el DiD de celdas evaluen los mismos proyectos.

Reglas de muestra (fijadas antes de estimar, 20/10/2026):
  - celdas elegibles del 55: >= 50 % de bosque en 2000 y con municipio
  - |d| <= BANDA_MAX_KM (la ventana optima la elige rdrobust dentro de ella)
  - fuera: celdas con cobertura de proyectos ARR/IFM o de un AD de otra cohorte
    (no son contrafactual "sin proyecto" ni tratamiento del borde evaluado)
  - el lado se decide por el centroide; las celdas parcialmente cubiertas
    quedan en la muestra y la dona del 73 las saca en la sensibilidad.

Entradas:  data/interim/celdas/celdas_atributos.csv, proyectos_gee.geojson,
           manifiesto_54.json  (salidas del 54 y 55)
Salida:    data/interim/celdas/rdd_celdas.csv

USO (desde tesis_gfc/):  python scripts/72_distancia_borde.py
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

try:
    import geopandas as gpd
except ImportError:
    sys.exit("Falta geopandas: pip install geopandas")

COHORTES = [2013, 2014, 2015, 2017, 2018]   # iguales al 56
BANDA_MAX_KM = 20.0
UMBRAL_BOSQUE = 0.50


def encontrar_raiz() -> Path:
    d = Path.cwd().resolve()
    for p in [d, *d.parents]:
        if (p / "data" / ".raiz_canonica").exists():
            return p
    sys.exit("No encuentro data/.raiz_canonica subiendo desde " + str(d))


def main() -> None:
    raiz = encontrar_raiz()
    d_cel = raiz / "data" / "interim" / "celdas"
    man = json.loads((d_cel / "manifiesto_54.json").read_text(encoding="utf-8"))
    crs, lado = man.get("crs_grilla", "EPSG:3116"), man.get("celda_m", 2000)

    cel = pd.read_csv(d_cel / "celdas_atributos.csv", dtype={"COD_DANE": str})
    proy = gpd.read_file(d_cel / "proyectos_gee.geojson").to_crs(crs)
    proy["anio"] = proy["anio"].astype(int)

    ad_win = proy[(proy.tipo == "AD") & proy.anio.isin(COHORTES)]
    ad_win = ad_win.dissolve(by="ProjectID", aggfunc={"anio": "min"}).reset_index()
    ad_win["geometry"] = ad_win.geometry.buffer(0)            # repara geometrias invalidas
    ids_win = set(ad_win.ProjectID)
    print(f"[BORDES] {len(ad_win)} proyectos AD de las cohortes {COHORTES}")
    print(ad_win.groupby("anio").size().rename("proyectos").to_string())

    # --- Muestra de celdas -----------------------------------------------------
    n0 = len(cel)
    cel = cel[(cel.pct_bosque2000 >= UMBRAL_BOSQUE) & cel.COD_DANE.notna()].copy()
    otra_cob = (cel.ha_otros_proyectos > 0) | (cel.proyecto_ad.notna() & ~cel.proyecto_ad.isin(ids_win))
    print(f"[MUESTRA] elegibles: {len(cel)} de {n0} | fuera por otra cobertura (ARR/IFM o AD de otra cohorte): "
          f"{int(otra_cob.sum())}")
    cel = cel[~otra_cob]

    ix, iy = np.divmod(cel["celda"].to_numpy(), 10_000)
    pts = gpd.GeoDataFrame(cel[["celda"]].reset_index(drop=True),
                           geometry=gpd.points_from_xy((ix + 0.5) * lado, (iy + 0.5) * lado), crs=crs)

    # --- Distancia al borde mas cercano ------------------------------------------
    bordes = gpd.GeoDataFrame(ad_win[["ProjectID", "anio"]], geometry=ad_win.geometry.boundary, crs=crs)
    cerca = gpd.sjoin_nearest(pts, bordes, how="left", distance_col="dist_m",
                              max_distance=BANDA_MAX_KM * 1000)
    cerca = cerca.dropna(subset=["ProjectID"]).sort_values("dist_m").drop_duplicates("celda")

    dentro = gpd.sjoin(pts, ad_win[["ProjectID", "geometry"]], how="inner", predicate="within")
    dentro_de = dentro.groupby("celda")["ProjectID"].agg(lambda s: ";".join(sorted(set(s))))
    cerca["dentro"] = cerca.celda.isin(dentro_de.index).astype(int)
    cerca["proyecto_contiene"] = cerca.celda.map(dentro_de)

    # Celda dentro de un proyecto pero cuyo borde mas cercano es de OTRO proyecto
    # (proyectos contiguos): ese borde separa dos tratados, no tratado/control.
    conflicto = (cerca.dentro == 1) & ~cerca.apply(
        lambda r: r.ProjectID in str(r.proyecto_contiene).split(";"), axis=1)
    print(f"[MUESTRA] celdas dentro cuyo borde mas cercano es de un proyecto vecino (fuera): {int(conflicto.sum())}")
    cerca = cerca[~conflicto]

    cerca["dist_borde_km"] = np.where(cerca.dentro == 1, 1, -1) * cerca.dist_m / 1000
    out = (cerca[["celda", "dist_borde_km", "dentro", "ProjectID", "anio"]]
           .rename(columns={"ProjectID": "proyecto", "anio": "cohorte"})
           .merge(cel[["celda", "COD_DANE", "area_ha", "bosque2000_ha", "pct_bosque2000", "pct_ad", "elev",
                       "pend", "lon", "lat", "grupo"]], on="celda", how="left"))
    out.to_csv(d_cel / "rdd_celdas.csv", index=False)

    print(f"\n[SALIDA] {len(out)} celdas a <= {BANDA_MAX_KM:.0f} km de un borde | dentro {int(out.dentro.sum())} | "
          f"fuera {int((out.dentro == 0).sum())} | proyectos {out.proyecto.nunique()}")
    print(out.groupby(["cohorte", "dentro"]).size().unstack(fill_value=0).to_string())
    print("\nCeldas por banda de distancia (km):")
    print(pd.cut(out.dist_borde_km, [-20, -10, -5, -2, -1, 0, 1, 2, 5, 10, 20]).value_counts().sort_index().to_string())
    print(f"\nGuardado: {d_cel / 'rdd_celdas.csv'}")


if __name__ == "__main__":
    main()
