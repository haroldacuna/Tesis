#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
63_fiscal_linea_base.py  (tarea 1.3, solo heterogeneidad exploratoria)
======================================================================

Desde el Panel de Buen Gobierno del CEDE (2024):
  idf_2001           DF_desemp_fisc en 2001 (continuidad con la version anterior)
  idf_2001_2009      promedio de DF_desemp_fisc 2001-2009 (ventana de linea base)
  inv_amb_pct        inversion ambiental como % de la inversion total, promedio
                     2002-2009 (inv_ambiental no tiene datos en 2001)
DF2_nuevoidf no se usa: es la metodologia vigente desde 2020, no hay linea base.
La inversion ambiental se expresa como proporcion porque en niveles mide el
tamanio del municipio, no su prioridad ambiental.

Salida: data/interim/fiscal_linea_base_municipio.csv
USO (desde tesis_gfc/):  python scripts\\63_fiscal_linea_base.py
"""
from __future__ import annotations

import sys
from pathlib import Path

import pandas as pd

DTA = Path("data/raw/auxiliary/PANEL_BUEN_GOBIERNO(2024).dta")
OUT = Path("data/interim/fiscal_linea_base_municipio.csv")


def main() -> int:
    if not Path("data/.raiz_canonica").exists():
        sys.exit("Corre desde tesis_gfc/ (raiz canonica).")
    d = pd.read_stata(DTA, columns=["codmpio", "ano", "DF_desemp_fisc", "inv_ambiental", "inv_total"])
    d["COD_DANE"] = d["codmpio"].astype("Int64").astype(str).str.zfill(5)
    d["ano"] = d["ano"].astype(int)

    idf01 = d[d["ano"] == 2001].groupby("COD_DANE")["DF_desemp_fisc"].mean().rename("idf_2001")
    idf_v = d[d["ano"].between(2001, 2009)].groupby("COD_DANE")["DF_desemp_fisc"].mean().rename("idf_2001_2009")
    v = d[d["ano"].between(2002, 2009) & (d["inv_total"] > 0)].copy()
    v["pct"] = 100 * v["inv_ambiental"] / v["inv_total"]
    raros = (v["pct"] < 0) | (v["pct"] > 100)
    if raros.any():
        print(f"[!] {raros.sum()} municipio-anios con % de inversion ambiental fuera de [0, 100]: se descartan")
    inv = v[~raros].groupby("COD_DANE")["pct"].mean().rename("inv_amb_pct_2002_2009")

    out = pd.concat([idf01, idf_v, inv], axis=1).reset_index()
    print(out.describe().round(2).to_string())
    print("\nCobertura:", out.drop(columns="COD_DANE").notna().sum().to_dict(), f"de {out['COD_DANE'].nunique()} municipios")
    r = out[["idf_2001", "idf_2001_2009"]].corr().iloc[0, 1]
    print(f"Correlacion idf_2001 vs idf_2001_2009: {r:.3f}")
    OUT.parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(OUT, index=False, encoding="utf-8-sig")
    print(f"Guardado: {OUT}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
