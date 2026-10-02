#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
62_areas_protegidas.py  (tarea 1.2, JM p.29)
============================================

Proporcion del area de cada municipio dentro de areas protegidas del RUNAP,
separando:
  * SINAP publico: todas las categorias excepto las Reservas Naturales de la
    Sociedad Civil (PNN, santuarios, reservas forestales protectoras, parques
    y distritos regionales, etc.).
  * RNSC: Reservas Naturales de la Sociedad Civil (privadas).

FECHA DE CORTE. El RUNAP solo trae la fecha de INSCRIPCION en el registro
(fecha_insc), no la de declaratoria: el registro se creo en 2010-2011 y las
areas existentes se inscribieron en bloque entonces. Por eso no se puede medir
a 2009 como fija D4. Se usa CORTE = 2012: incluye todo lo inscrito hasta el
anio anterior a la primera cohorte reportable (2013), de modo que la covariable
sigue siendo anterior al tratamiento para todas las cohortes de la ventana. El
script imprime la distribucion de fechas para documentar esta decision.

Solo areas con condicion = REGISTRADA (se excluyen las que estan en
construccion). Las areas de cada grupo se disuelven antes de intersectar con
los municipios, para no contar dos veces los traslapes.

Salida: data/interim/areas_protegidas_municipio.csv
  COD_DANE, pct_sinap_pub_2012, pct_rnsc_2012, pct_sinap_pub_actual, pct_rnsc_actual

USO (desde tesis_gfc/):  python scripts\\62_areas_protegidas.py
"""
from __future__ import annotations

import sys
from pathlib import Path

import geopandas as gpd
import pandas as pd

CORTE = 2012
RUNAP = Path("data/raw/runap/runap.shp")
GPKG = Path("data/interim/municipios_clean.gpkg")
OUT = Path("data/interim/areas_protegidas_municipio.csv")
RNSC = "reserva natural de la sociedad civil"


def main() -> int:
    if not Path("data/.raiz_canonica").exists():
        sys.exit("Corre desde tesis_gfc/ (raiz canonica).")
    ap = gpd.read_file(RUNAP)
    print(f"RUNAP: {len(ap)} areas | CRS {ap.crs}")
    print("Condicion:", ap["condicion"].value_counts(dropna=False).to_dict())
    ## REGISTRADA e INSCRITA son areas declaradas (las inscritas son todas publicas);
    ## "REGISTRADO" es un error de tipeo del RUNAP. Solo quedan fuera las de CONSTRUCCION.
    ap = ap[ap["condicion"].astype(str).str.upper().str.strip().isin(
        ["REGISTRADA", "REGISTRADO", "INSCRITA"])].copy()
    ap["grupo"] = ap["ap_categor"].astype(str).str.lower().str.contains(RNSC).map(
        {True: "rnsc", False: "sinap_pub"})
    ap["anio_insc"] = pd.to_datetime(ap["fecha_insc"], errors="coerce").dt.year
    print(f"Registradas: {len(ap)} | sin fecha de inscripcion: {ap['anio_insc'].isna().sum()}")

    ## Distribucion de fechas: documenta por que el corte es 2012 y no 2009
    bins = pd.cut(ap["anio_insc"], [0, 2009, 2010, 2011, 2012, 2018, 2100],
                  labels=["<=2009", "2010", "2011", "2012", "2013-2018", ">=2019"])
    print("\nAreas por anio de inscripcion y grupo:")
    print(pd.crosstab(bins, ap["grupo"], dropna=False).to_string())
    print("\nCategorias:")
    print(ap.groupby("grupo")["ap_categor"].value_counts().to_string())

    mun = gpd.read_file(GPKG)
    col = next(c for c in mun.columns if c.upper() in ("COD_DANE", "MPIO_CDPMP", "CODIGO"))
    mun["COD_DANE"] = mun[col].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5)
    mun = mun[["COD_DANE", "geometry"]].dissolve(by="COD_DANE", as_index=False).to_crs(9377)
    mun["geometry"] = mun.geometry.make_valid()
    mun["a_mun"] = mun.area

    ap = ap.to_crs(9377)
    ap["geometry"] = ap.geometry.make_valid()
    out = mun[["COD_DANE"]].copy()
    for grupo in ("sinap_pub", "rnsc"):
        for etiqueta, sub in ((str(CORTE), ap[(ap["grupo"] == grupo) & (ap["anio_insc"] <= CORTE)]),
                              ("actual", ap[ap["grupo"] == grupo])):
            nombre = f"pct_{'sinap_pub' if grupo == 'sinap_pub' else 'rnsc'}_{etiqueta}"
            if sub.empty:
                out[nombre] = 0.0; print(f"{nombre}: sin areas"); continue
            union = gpd.GeoDataFrame(geometry=[sub.geometry.union_all()], crs=9377)
            inter = gpd.overlay(mun[["COD_DANE", "a_mun", "geometry"]], union, how="intersection",
                                keep_geom_type=True)
            inter["a"] = inter.area
            pct = (100 * inter.groupby("COD_DANE")["a"].sum() / mun.set_index("COD_DANE")["a_mun"]).clip(upper=100)
            out[nombre] = out["COD_DANE"].map(pct).fillna(0.0)
            print(f"{nombre}: {len(sub)} areas | municipios con > 0 %: {(out[nombre] > 0).sum()} | "
                  f"media {out[nombre].mean():.2f} % | max {out[nombre].max():.1f} %")

    OUT.parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(OUT, index=False, encoding="utf-8-sig")
    print(f"\nGuardado: {OUT} ({len(out)} municipios)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
