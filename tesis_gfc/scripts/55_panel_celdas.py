#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
55_panel_celdas.py
==================

Arma el panel de celdas de 2 x 2 km a partir de las exportaciones del 54
(data/raw/celdas_gee/*.csv), para la evaluacion a nivel de poligono.

Pasos
  1. Lee las teselas, verifica que esten las 131 del manifiesto y decodifica
     las claves (aritmetica vectorizada, espejo exacto de 54.decodificar).
  2. Suma las partes de las celdas partidas entre teselas (areas aditivas).
  3. Atributos por celda: area, bosque natural 2000 por tipo y antiguedad,
     cobertura por proyecto, elevacion, pendiente, distancia al proyecto mas
     cercano, municipio del centroide.
  4. Grupos (FIJADOS ANTES DE VER EL PANEL):
       elegible     bosque natural 2000 >= 50 % del area de la celda
       tratada      elegible y >= 50 % dentro de un proyecto AD; cohorte = anio
                    de inicio del proyecto AD con mas area en la celda
       parcial      0-50 % AD, o cualquier cobertura ARR/IFM -> se excluye
       amortiguacion  sin cobertura y <= 10 km del proyecto mas cercano (fuga)
       control      sin cobertura y > 10 km de cualquier proyecto
  5. Resultados por celda y anio (ha):
       def_p1         PRINCIPAL: bosque estable 5a -> antropico, persistente t+1
                      (mismo concepto que def_est5_p1 municipal de 39b; <= 2023)
       def_est5       bosque estable 5a -> antropico (sin persistencia; <= 2024)
       def_p1_o{1..4} por origen: tierra firme, manglar, inundable, arena
       def_p1_{destino} por destino: agropecuario (21, 14), palma, silvicultura,
                      platano, mineria, infraestructura, otros_antropicos
       def_p1_rastrojo  rastrojo (bosque no estable) -> antropico persistente
       a_natural      bosque -> formacion natural no boscosa (diagnostico)
  6. VERIFICACION OBLIGATORIA: la suma nacional de def_est5 y def_p1 por anio
     debe coincidir con la suma municipal de 39b con diferencia < 1 % (mismas
     clases). Si no, el script termina con codigo 1.

Salidas (data/interim/celdas/)
  celdas_atributos.csv          una fila por celda
  panel_celdas_nocero.csv.gz    solo celda-anio con algun resultado > 0 (el 56
                                completa los ceros para las celdas que use)
  qc_55.json

Uso:  & $py scripts\\55_panel_celdas.py
"""
from __future__ import annotations

import glob
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

try:
    import geopandas as gpd
except ImportError as exc:  # pragma: no cover
    sys.exit(f"[ERROR] Falta geopandas ({exc}).")

BASE, F_BOSQUE, F_PROY, F_AREA = 100_000, 90_000, 80_000, 99_999
UMBRAL_BOSQUE = 0.50
UMBRAL_TRATADA = 0.50
BUFFER_M = 10_000
ANIO_FIN_P1 = 2023
TOL_VERIF_PCT = 1.0
ANTROPICOS = list(range(1, 11))                     # d = 1..10 == CLASES_ANTROPICAS
DESTINOS = {"agropecuario": [1, 2], "palma": [3], "silvicultura": [4], "platano": [5],
            "mineria": [6], "infraestructura": [7], "otros_antropicos": [8, 9, 10]}
ORIGENES = {1: "tierra_firme", 2: "manglar", 3: "inundable", 4: "arena"}


def encontrar_raiz() -> Path:
    for base in (Path(__file__).resolve().parent, Path.cwd().resolve()):
        for p in (base, *base.parents):
            if (p / "data" / ".raiz_canonica").is_file():
                return p
    sys.exit("[ERROR] No se encontro data/.raiz_canonica.")


def decodificar_vec(k: np.ndarray) -> pd.DataFrame:
    """Version vectorizada de 54.decodificar (misma aritmetica)."""
    k = np.rint(k).astype(np.int64)
    celda, resto = np.divmod(k, BASE)
    tipo = np.where(resto == F_AREA, "area",
                    np.where(resto >= F_BOSQUE, "bosque2000",
                             np.where(resto >= F_PROY, "proyecto", "transicion")))
    rb = resto - F_BOSQUE
    q, r = np.divmod(resto, 10_000)
    s, r = np.divmod(r, 1_000)
    o, d = np.divmod(r, 100)
    return pd.DataFrame({
        "celda": celda, "tipo": tipo,
        "q": np.where(tipo == "transicion", q, -1),
        "s": np.where(tipo == "transicion", s, np.where(tipo == "bosque2000", rb // 1000, -1)),
        "o": np.where(tipo == "transicion", o, np.where(tipo == "bosque2000", (rb % 1000) // 100, -1)),
        "d": np.where(tipo == "transicion", d, -1),
        "pid": np.where(tipo == "proyecto", resto - F_PROY, -1)})


def leer_teselas(dir_csv: Path, man: dict) -> tuple[pd.DataFrame, pd.DataFrame, list[str]]:
    archivos = sorted(glob.glob(str(dir_csv / "*.csv")))
    if not archivos:
        sys.exit(f"[ERROR] No hay CSV en {dir_csv}")
    esperadas = set(man.get("tareas", {}))
    presentes = {Path(a).stem.replace("celdas_", "", 1) for a in archivos}
    faltan = sorted(esperadas - presentes)
    partes, covs = [], []
    for a in archivos:
        x = pd.read_csv(a, dtype={"k": "float64", "ha": "float64", "anio": "float64"})
        covs.append(x.loc[x["anio"] == -1, ["k", "elev", "pend"]])
        partes.append(x.loc[x["anio"] >= 0, ["k", "ha", "anio"]])
    reg = pd.concat(partes, ignore_index=True)
    cov = pd.concat(covs, ignore_index=True)
    print(f"[LEER] {len(archivos)} archivos | {len(reg):,} registros | teselas faltantes: {len(faltan)}")
    return reg, cov, faltan


def main() -> int:
    raiz = encontrar_raiz()
    d_int = raiz / "data" / "interim" / "celdas"
    man = json.loads((d_int / "manifiesto_54.json").read_text(encoding="utf-8"))
    crs = man.get("crs_grilla", "EPSG:3116")
    lado = man.get("celda_m", 2000)
    qc: dict = {"crs_grilla": crs, "celda_m": lado}

    reg, cov, faltan = leer_teselas(raiz / "data" / "raw" / "celdas_gee", man)
    qc["teselas_faltantes"] = faltan
    reg = pd.concat([reg.reset_index(drop=True), decodificar_vec(reg["k"].to_numpy())], axis=1)
    reg["anio"] = reg["anio"].astype(int)

    # --- Atributos fijos por celda (sumando partes entre teselas) ------------
    area = reg[reg.tipo == "area"].groupby("celda")["ha"].sum().rename("area_ha")
    b = reg[reg.tipo == "bosque2000"]
    bos = b.groupby("celda")["ha"].sum().rename("bosque2000_ha")
    bos_est = b[b.s == 1].groupby("celda")["ha"].sum().rename("bosque2000_estable_ha")
    bos_o = (b.groupby(["celda", "o"])["ha"].sum().unstack(fill_value=0)
             .rename(columns=lambda o: f"bosque2000_{ORIGENES.get(int(o), f'o{o}')}_ha"))
    pid = pd.read_csv(d_int / "proyectos_pid.csv")
    pr = reg[reg.tipo == "proyecto"].groupby(["celda", "pid"])["ha"].sum().reset_index()
    pr = pr.merge(pid[["pid", "ProjectID", "tipo", "anio"]], on="pid", how="left")
    ad = pr[pr["tipo"] == "AD"]
    ha_ad = ad.groupby("celda")["ha"].sum().rename("ha_ad")
    ha_otros = pr[pr["tipo"] != "AD"].groupby("celda")["ha"].sum().rename("ha_otros_proyectos")
    princ = (ad.sort_values("ha", ascending=False).drop_duplicates("celda")
             .set_index("celda")[["ProjectID", "anio"]]
             .rename(columns={"ProjectID": "proyecto_ad", "anio": "cohorte"}))

    cel = pd.concat([area, bos, bos_est, bos_o, ha_ad, ha_otros, princ], axis=1)
    cel = cel[cel["area_ha"].notna()].copy()           # descarta celdas en el mar
    for c in ["bosque2000_ha", "bosque2000_estable_ha", "ha_ad", "ha_otros_proyectos"] + list(bos_o.columns):
        cel[c] = cel[c].fillna(0.0)
    cel.index.name = "celda"
    cov = cov.assign(celda=np.rint(cov["k"]).astype(np.int64)).groupby("celda")[["elev", "pend"]].mean()
    cel = cel.join(cov, how="left")

    # --- Geometria: centroide, distancia al proyecto, municipio --------------
    ix, iy = np.divmod(cel.index.to_numpy(), 10_000)
    cen = gpd.GeoDataFrame(cel[[]].copy(), geometry=gpd.points_from_xy((ix + 0.5) * lado, (iy + 0.5) * lado),
                           crs=crs)
    proy = gpd.read_file(d_int / "proyectos_gee.geojson").to_crs(crs)
    dist = gpd.sjoin_nearest(cen.reset_index(), proy[["geometry"]], how="left", distance_col="dist_m")
    cel["dist_proyecto_km"] = dist.groupby("celda")["dist_m"].min().reindex(cel.index) / 1000
    mun = gpd.read_file(raiz / "data" / "interim" / "municipios_clean.gpkg", layer="municipios_clean")
    mun["COD_DANE"] = pd.to_numeric(mun["COD_DANE"], errors="coerce").astype("Int64").astype(str).str.zfill(5)
    en_mun = gpd.sjoin(cen.reset_index(), mun[["COD_DANE", "geometry"]].to_crs(crs), how="left",
                       predicate="within").drop_duplicates("celda").set_index("celda")
    cel["COD_DANE"] = en_mun["COD_DANE"].reindex(cel.index)
    lon_lat = cen.to_crs(4326).geometry
    cel["lon"], cel["lat"] = lon_lat.x.values, lon_lat.y.values

    # --- Grupos --------------------------------------------------------------
    cel["pct_bosque2000"] = cel["bosque2000_ha"] / cel["area_ha"]
    cel["pct_ad"] = cel["ha_ad"] / cel["area_ha"]
    cel["pct_otros"] = cel["ha_otros_proyectos"] / cel["area_ha"]
    eleg = cel["pct_bosque2000"] >= UMBRAL_BOSQUE
    tratada = eleg & (cel["pct_ad"] >= UMBRAL_TRATADA) & cel["cohorte"].notna()
    cubierta = (cel["ha_ad"] > 0) | (cel["ha_otros_proyectos"] > 0)
    cel["grupo"] = np.select(
        [~eleg, tratada, cubierta, cel["dist_proyecto_km"] <= BUFFER_M / 1000],
        ["no_elegible", "tratada", "parcial", "amortiguacion"], default="control")
    cel.loc[cel.grupo != "tratada", "cohorte"] = np.nan
    qc["celdas_por_grupo"] = cel["grupo"].value_counts().to_dict()
    qc["tratadas_por_cohorte"] = cel[cel.grupo == "tratada"]["cohorte"].value_counts().sort_index() \
        .rename(lambda x: str(int(x))).to_dict()
    print(f"[GRUPOS] {qc['celdas_por_grupo']}")
    print(f"[COHORTES] tratadas por anio de inicio: {qc['tratadas_por_cohorte']}")
    print("[BOSQUE 2000 por grupo, % de tipo]")
    comp = cel[cel.grupo != "no_elegible"].groupby("grupo")[[c for c in cel.columns if c.startswith("bosque2000_")
                                                             and c not in ("bosque2000_ha",)]].sum()
    print((100 * comp.div(cel[cel.grupo != "no_elegible"].groupby("grupo")["bosque2000_ha"].sum(), axis=0))
          .round(1).to_string())
    cel.to_csv(d_int / "celdas_atributos.csv")

    # --- Resultados por celda y anio -------------------------------------------
    t = reg[reg.tipo == "transicion"]
    antr = t.d.isin(ANTROPICOS)
    base_p1 = t[antr & (t.s == 1) & (t.q == 1)]
    cols = {
        "def_est5": t[antr & (t.s == 1)],
        "def_p1": base_p1,
        "def_p1_rastrojo": t[antr & (t.s == 0) & (t.q == 1)],
        "a_natural": t[t.d == 20],
    }
    for o, nom in ORIGENES.items():
        cols[f"def_p1_{nom}"] = base_p1[base_p1.o == o]
    for nom, ds in DESTINOS.items():
        cols[f"def_p1_{nom}"] = base_p1[base_p1.d.isin(ds)]
    panel = pd.concat({k: v.groupby(["celda", "anio"])["ha"].sum() for k, v in cols.items()}, axis=1).fillna(0.0)
    panel.loc[panel.index.get_level_values("anio") > ANIO_FIN_P1,
              [c for c in panel.columns if "p1" in c]] = np.nan
    panel = panel.reset_index()
    panel.to_csv(d_int / "panel_celdas_nocero.csv.gz", index=False, compression="gzip")
    qc["panel_filas"] = int(len(panel))                       # el 56 verifica que lee todo
    qc["panel_suma_def_est5"] = round(float(panel["def_est5"].sum()), 2)
    print(f"[PANEL] {len(panel):,} filas celda-anio con algun resultado > 0 -> panel_celdas_nocero.csv.gz")

    # --- Verificacion obligatoria contra 39b ---------------------------------
    ruta_mun = raiz / "data" / "interim" / "mapbiomas" / "deforestacion_estable_mapbiomas_municipio_anio.csv"
    ok = True
    if faltan:
        print(f"[VERIFICACION] omitida: faltan {len(faltan)} teselas (la comparacion nacional no aplica).")
        ok = False
    elif not ruta_mun.is_file():
        print(f"[VERIFICACION] omitida: no esta {ruta_mun.name}")
        ok = False
    else:
        m = pd.read_csv(ruta_mun)
        nac_c = panel.groupby("anio")[["def_est5", "def_p1"]].sum()
        nac_m = m.groupby("anio")[["def_est5_ha", "def_est5_p1_ha"]].sum(min_count=1)
        comp = nac_c.join(nac_m, how="inner")
        comp["dif_est5_pct"] = 100 * (comp.def_est5 / comp.def_est5_ha - 1)
        comp["dif_p1_pct"] = 100 * (comp.def_p1 / comp.def_est5_p1_ha - 1)
        print("\n[VERIFICACION] celdas vs municipal (39b), diferencia % por anio:")
        print(comp[["def_est5", "def_est5_ha", "dif_est5_pct", "dif_p1_pct"]].round(2).to_string())
        peor = float(np.nanmax(np.abs(comp[["dif_est5_pct", "dif_p1_pct"]].to_numpy())))
        qc["verificacion_max_abs_dif_pct"] = round(peor, 3)
        ok = peor < TOL_VERIF_PCT
        print(f">>> Diferencia maxima {peor:.3f} % (tolerancia {TOL_VERIF_PCT} %) -> "
              f"{'OK' if ok else 'NO COINCIDE: revisar decodificacion antes de estimar'}")
    qc["verificacion_ok"] = bool(ok)
    (d_int / "qc_55.json").write_text(json.dumps(qc, indent=2, ensure_ascii=False, default=str), encoding="utf-8")
    print(f"[OK] salidas en {d_int}")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
