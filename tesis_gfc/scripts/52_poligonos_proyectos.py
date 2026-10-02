#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
52_poligonos_proyectos.py
=========================

Cobertura municipal de los proyectos de carbono a partir de sus POLIGONOS,
usando la base abierta de Karnik et al. (2025, Scientific Data 12:581;
Zenodo 10.5281/zenodo.11459391): south_america.gpkg + indice CSV.

Resuelve dos limitaciones de 38_construir_dosis_tratamiento.py (area declarada
asignada integra al municipio anfitrion): el proyecto se reparte entre los
municipios que realmente cubre, y los traslapes se cuentan una vez (UNION).

Salidas (data/interim/proyectos_poligonos/):
  proyectos_municipio.csv      una fila por (proyecto, municipio): ha y % del
                               proyecto y del municipio
  cobertura_municipal.csv      una fila por municipio: ha cubiertas por la
                               UNION de proyectos de deforestacion evitada (AD)
                               y de cualquier tipo, dosis por area, anio del
                               primer proyecto AD, conteos
  diagnostics/...              validacion contra la D1 depurada

Dosis por area (la de bosque va en un segundo paso con GEE):
    d_area_i = | (U_k P_k) interseccion M_i | / | M_i |      (EPSG:9377)

REGLAS fijadas ANTES de correr (27/09/2026):
  * Cobertura minima para considerar un municipio "cubierto": >= 1 % de su
    area Y >= 100 ha (descarta rebabas de borde). Se reportan tambien 5 % y 10 %.
  * Controles parcialmente tratados: si los controles de la muestra principal
    (D1 depurada, ventana 2013-2019, controles limpios) con cobertura AD >= 5 %
    concentran >= 10 % del peso ATT del 48 (w = p/(1-p)), la especificacion
    principal debe reestimarse sin ellos (script 53).

Uso (PowerShell):
    & $py scripts\\52_poligonos_proyectos.py
"""
from __future__ import annotations

import json
import re
import sys
from pathlib import Path

import numpy as np
import pandas as pd

try:
    import geopandas as gpd
    import shapely
except ImportError as exc:  # pragma: no cover
    sys.exit(f"[ERROR] Falta geopandas/shapely ({exc}).")

CRS_AREA = 9377                 # MAGNA-SIRGAS Origen Nacional (metros)
MIN_PCT, MIN_HA = 1.0, 100.0    # umbral de cobertura (regla fijada)
UMBRALES_REPORTE = (1.0, 5.0, 10.0)
UMBRAL_PCT_CONTROL = 5.0        # regla de controles parcialmente tratados
UMBRAL_PESO = 10.0              # % del peso ATT
VENTANA = range(2013, 2020)
REBABA_HA = 1.0                 # intersecciones menores se descartan
COL_DEP = "anio_inicio_tratamiento_todas_fuentes_redd_depurada"
COL_MUE = "muestra_redd_depurada"


# ---------------------------------------------------------------------------
def encontrar_raiz() -> Path:
    for base in (Path(__file__).resolve().parent, Path.cwd().resolve()):
        for p in (base, *base.parents):
            if (p / "data" / ".raiz_canonica").is_file():
                return p
    sys.exit("[ERROR] No se encontro data/.raiz_canonica.")


def pad5(s: pd.Series) -> pd.Series:
    return pd.to_numeric(s, errors="coerce").astype("Int64").astype(str).str.zfill(5)


def anio(fecha) -> float:
    m = re.search(r"(19|20)\d{2}", str(fecha))
    return float(m.group(0)) if m else np.nan


def poligonal(g):
    """Repara y deja solo la parte poligonal (make_valid puede devolver colecciones)."""
    if g is None or g.is_empty:
        return None
    g = shapely.make_valid(g)
    if g.geom_type in ("Polygon", "MultiPolygon"):
        return g
    partes = [x for x in getattr(g, "geoms", []) if x.geom_type in ("Polygon", "MultiPolygon")]
    return shapely.unary_union(partes) if partes else None


# ---------------------------------------------------------------------------
def cargar_proyectos(ruta_gpkg: Path, pais: str = "Colombia") -> tuple[gpd.GeoDataFrame, pd.DataFrame]:
    if not ruta_gpkg.is_file():
        sys.exit(f"[ERROR] Falta {ruta_gpkg}")
    g = gpd.read_file(ruta_gpkg)
    g = g[g["Country"].astype(str).str.contains(pais, case=False, na=False)].copy()
    if g.empty:
        sys.exit(f"[ERROR] No hay proyectos de {pais} en {ruta_gpkg.name}")
    g["anio_inicio"] = g["Project Start Date"].map(anio)
    g["tipo"] = g["Project Type"].astype(str).str.upper().str.strip()
    es_punto = g.geom_type.isin(["Point", "MultiPoint"])
    puntos = g.loc[es_punto, ["ProjectID", "Project Name", "Registry Name", "tipo", "anio_inicio"]].copy()
    g = g[~es_punto].copy()
    n_inval = int((~g.is_valid).sum())
    g["geometry"] = [poligonal(x) for x in g.geometry]
    g = g[g.geometry.notna()].to_crs(CRS_AREA)
    g["ha_proyecto"] = g.area / 1e4
    print(f"[PROYECTOS] {len(g)} poligonos ({n_inval} reparados) + {len(puntos)} solo punto | "
          f"tipos: {g['tipo'].value_counts().to_dict()}")
    return g, puntos


def cargar_municipios(ruta: Path) -> gpd.GeoDataFrame:
    if not ruta.is_file():
        sys.exit(f"[ERROR] Falta {ruta}")
    m = gpd.read_file(ruta, layer="municipios_clean")
    m["COD_DANE"] = pad5(m["COD_DANE"])
    m["geometry"] = [poligonal(x) for x in m.geometry]
    m = m[m.geometry.notna()].to_crs(CRS_AREA)
    m["ha_municipio"] = m.area / 1e4
    keep = [c for c in ("COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR", "ha_municipio", "geometry") if c in m.columns]
    return m[keep]


def proyecto_por_municipio(p: gpd.GeoDataFrame, m: gpd.GeoDataFrame) -> pd.DataFrame:
    cols = ["ProjectID", "Project Name", "Registry Name", "tipo", "anio_inicio", "ha_proyecto", "geometry"]
    x = gpd.overlay(p[cols], m[["COD_DANE", "ha_municipio", "geometry"]], how="intersection",
                    keep_geom_type=True)
    x["ha_interseccion"] = x.area / 1e4
    x = x[x["ha_interseccion"] >= REBABA_HA].copy()
    x["pct_del_proyecto"] = 100 * x["ha_interseccion"] / x["ha_proyecto"]
    x["pct_del_municipio"] = 100 * x["ha_interseccion"] / x["ha_municipio"]
    return pd.DataFrame(x.drop(columns="geometry"))


def cobertura_union(p: gpd.GeoDataFrame, m: gpd.GeoDataFrame, etiqueta: str) -> pd.DataFrame:
    """ha de cada municipio cubiertas por la UNION de los poligonos dados."""
    if p.empty:
        return pd.DataFrame(columns=["COD_DANE", f"ha_{etiqueta}"])
    u = gpd.GeoDataFrame(geometry=[shapely.unary_union(p.geometry.values)], crs=p.crs)
    x = gpd.overlay(m[["COD_DANE", "geometry"]], u, how="intersection", keep_geom_type=True)
    x[f"ha_{etiqueta}"] = x.area / 1e4
    return x.groupby("COD_DANE", as_index=False)[f"ha_{etiqueta}"].sum()


def controles_contaminados(trat: pd.DataFrame) -> set[str]:
    """Misma regla que comun_controles_limpios.R."""
    cols = [c for c in trat.columns if c.startswith("anio_inicio_tratamiento_") or c.startswith("proyectos_")]
    pos = trat[cols].apply(pd.to_numeric, errors="coerce").gt(0).any(axis=1)
    return set(trat.loc[pos, "COD_DANE"])


# ---------------------------------------------------------------------------
def main() -> int:
    raiz = encontrar_raiz()
    din = raiz / "data" / "raw" / "proyectos_poligonos"
    out = raiz / "data" / "interim" / "proyectos_poligonos"
    diag = out / "diagnostics"
    diag.mkdir(parents=True, exist_ok=True)

    p, puntos = cargar_proyectos(din / "south_america.gpkg")
    m = cargar_municipios(raiz / "data" / "interim" / "municipios_clean.gpkg")
    nombres = m[[c for c in ("COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR") if c in m.columns]]

    # 1) Proyecto x municipio
    pm = proyecto_por_municipio(p, m).merge(nombres, on="COD_DANE", how="left")
    pm.to_csv(out / "proyectos_municipio.csv", index=False)
    resumen_p = (pm.sort_values("ha_interseccion", ascending=False)
                 .groupby("ProjectID")
                 .agg(nombre=("Project Name", "first"), registro=("Registry Name", "first"),
                      tipo=("tipo", "first"), anio_inicio=("anio_inicio", "first"),
                      ha_proyecto=("ha_proyecto", "first"), n_municipios=("COD_DANE", "nunique"),
                      municipio_principal=("COD_DANE", "first"),
                      pct_en_principal=("pct_del_proyecto", "first"))
                 .reset_index())
    resumen_p.to_csv(diag / "proyectos_resumen.csv", index=False)
    ad = resumen_p[resumen_p["tipo"] == "AD"]
    print(f"[REPARTO] proyectos AD: {len(ad)} | abarcan >1 municipio: {(ad.n_municipios > 1).sum()} | "
          f"mediana de % en el municipio principal: {ad.pct_en_principal.median():.1f} %")

    # 2) Cobertura municipal (UNION), AD y todos los tipos
    cob = (m.drop(columns="geometry")
           .merge(cobertura_union(p[p["tipo"] == "AD"], m, "ad"), on="COD_DANE", how="left")
           .merge(cobertura_union(p, m, "todos"), on="COD_DANE", how="left"))
    cob[["ha_ad", "ha_todos"]] = cob[["ha_ad", "ha_todos"]].fillna(0.0)
    cob["dosis_area_ad"] = cob["ha_ad"] / cob["ha_municipio"]
    cob["pct_ad"] = 100 * cob["dosis_area_ad"]
    cob["pct_todos"] = 100 * cob["ha_todos"] / cob["ha_municipio"]
    cob["cubierto_ad"] = (cob["pct_ad"] >= MIN_PCT) & (cob["ha_ad"] >= MIN_HA)
    # primer anio AD entre proyectos que por si solos cumplen el umbral en el municipio
    pm_ad = pm[(pm["tipo"] == "AD") & (pm["pct_del_municipio"] >= MIN_PCT) & (pm["ha_interseccion"] >= MIN_HA)]
    primer = pm_ad.groupby("COD_DANE").agg(cohorte_poligono=("anio_inicio", "min"),
                                           n_proyectos_ad=("ProjectID", "nunique"),
                                           proyectos_ad=("ProjectID", lambda s: ";".join(sorted(set(s)))))
    cob = cob.merge(primer, on="COD_DANE", how="left")
    cob.to_csv(out / "cobertura_municipal.csv", index=False)
    print(f"[COBERTURA] municipios con cobertura AD >= {MIN_PCT:g} % y >= {MIN_HA:g} ha: {int(cob.cubierto_ad.sum())}")

    # 3) Cruce con la D1 depurada y la muestra principal
    ruta_trat = raiz / "data" / "final" / "panel_con_tratamiento_actualizado.csv"
    trat = pd.read_csv(ruta_trat, dtype={"COD_DANE": str}, low_memory=False)
    trat["COD_DANE"] = pad5(trat["COD_DANE"])
    contam = controles_contaminados(trat)
    t1 = (trat.groupby("COD_DANE")
          .agg(G_dep=(COL_DEP, lambda s: pd.to_numeric(s, errors="coerce").min()),
               muestra_dep=(COL_MUE, "min")).reset_index())
    t1["G_dep"] = t1["G_dep"].fillna(0)
    x = cob.merge(t1, on="COD_DANE", how="left")
    x["tratado_d1"] = x["G_dep"] > 0
    x["en_ventana"] = x["G_dep"].isin(VENTANA)
    x["control_principal"] = (x["G_dep"] == 0) & (x["muestra_dep"] == 1) & ~x["COD_DANE"].isin(contam)

    qc = {"proyectos_poligono": int(len(p)), "proyectos_solo_punto": puntos["ProjectID"].tolist(),
          "proyectos_ad": int(len(ad)), "ad_en_mas_de_un_municipio": int((ad.n_municipios > 1).sum())}

    # 3a) Validacion: tratados D1 en ventana con / sin cobertura de poligono
    tv = x[x["en_ventana"] & (x["muestra_dep"] == 1)]
    sin_pol = tv[~tv["cubierto_ad"]]
    qc["tratados_ventana"] = int(len(tv))
    qc["tratados_ventana_con_poligono"] = int(tv["cubierto_ad"].sum())
    print(f"\n[VALIDACION] tratados D1 depurada 2013-2019: {len(tv)} | con poligono AD que los cubre: "
          f"{int(tv.cubierto_ad.sum())}")
    tv[["COD_DANE", "NOMBRE_MPI", "G_dep", "cohorte_poligono", "pct_ad", "proyectos_ad"]].to_csv(
        diag / "tratados_vs_poligonos.csv", index=False)
    if len(sin_pol):
        print("  tratados SIN poligono en la base (proyecto no incluido o solo punto):")
        print(sin_pol[["COD_DANE", "NOMBRE_MPI", "G_dep"]].to_string(index=False))
    dif_coh = tv[tv.cubierto_ad & (tv.cohorte_poligono.notna()) & (tv.cohorte_poligono != tv.G_dep)]
    qc["tratados_con_cohorte_distinta"] = int(len(dif_coh))
    if len(dif_coh):
        print("  cohorte del registro distinta de la del poligono (primer proyecto que lo cubre):")
        print(dif_coh[["COD_DANE", "NOMBRE_MPI", "G_dep", "cohorte_poligono", "proyectos_ad"]].to_string(index=False))

    # 3b) Controles de la muestra principal cubiertos por proyectos AD
    ctrl = x[x["control_principal"]].copy()
    print(f"\n[CONTROLES] controles de la muestra principal: {len(ctrl)}")
    for u in UMBRALES_REPORTE:
        n = int(((ctrl.pct_ad >= u) & (ctrl.ha_ad >= MIN_HA)).sum())
        qc[f"controles_con_cobertura_ad_ge_{u:g}pct"] = n
        print(f"  con cobertura AD >= {u:g} %: {n}")
    cub = ctrl[(ctrl.pct_ad >= MIN_PCT) & (ctrl.ha_ad >= MIN_HA)].sort_values("pct_ad", ascending=False)
    cub[["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR", "ha_ad", "pct_ad", "cohorte_poligono", "proyectos_ad"]].to_csv(
        diag / "controles_cubiertos_por_poligonos.csv", index=False)
    if len(cub):
        print(cub[["COD_DANE", "NOMBRE_MPI", "pct_ad", "cohorte_poligono", "proyectos_ad"]]
              .head(25).round(1).to_string(index=False))

    # 3c) Regla: peso ATT (48) en controles cubiertos >= 5 %
    ruta_ps = raiz / "outputs" / "tablas" / "48_propension_municipal.csv"
    if ruta_ps.is_file():
        ps = pd.read_csv(ruta_ps, dtype={"COD_DANE": str})
        ps["COD_DANE"] = pad5(ps["COD_DANE"])
        pc = ps[ps["tratado"] == 0].copy()
        pc["w"] = pc["p"] / (1 - pc["p"].clip(upper=1 - 1e-12))
        pc = pc.merge(ctrl[["COD_DANE", "pct_ad", "ha_ad"]], on="COD_DANE", how="left").fillna({"pct_ad": 0, "ha_ad": 0})
        marca = (pc.pct_ad >= UMBRAL_PCT_CONTROL) & (pc.ha_ad >= MIN_HA)
        peso = 100 * pc.loc[marca, "w"].sum() / pc["w"].sum()
        qc["peso_att_controles_cubiertos_ge_5pct"] = round(float(peso), 2)
        reestimar = peso >= UMBRAL_PESO
        qc["reestimar_sin_controles_cubiertos"] = bool(reestimar)
        print(f"\n[REGLA] controles con cobertura AD >= {UMBRAL_PCT_CONTROL:g} % concentran {peso:.1f} % del "
              f"peso ATT -> {'REESTIMAR la principal sin ellos (53)' if reestimar else 'no requiere reestimar'}")
        pc.loc[marca].sort_values("w", ascending=False).assign(
            peso_pct=lambda d: 100 * d.w / pc["w"].sum()).to_csv(diag / "peso_att_controles_cubiertos.csv", index=False)
    else:
        print(f"\n[AVISO] No esta {ruta_ps.name}: corre 48_soporte_comun.R para evaluar la regla de peso.")

    # 3d) Proyectos AD cuyo poligono cae sobre todo en municipios NO tratados en la D1
    principal_pm = pm[pm["tipo"] == "AD"].merge(t1, on="COD_DANE", how="left")
    fuera = (principal_pm.assign(no_trat=lambda d: d.G_dep.fillna(0) == 0)
             .groupby("ProjectID")
             .apply(lambda d: 100 * d.loc[d.no_trat, "ha_interseccion"].sum() / d["ha_interseccion"].sum(),
                    include_groups=False)
             .rename("pct_en_no_tratados").reset_index())
    fuera = fuera.merge(ad[["ProjectID", "nombre", "registro", "anio_inicio", "ha_proyecto"]], on="ProjectID")
    fuera = fuera.sort_values("pct_en_no_tratados", ascending=False)
    fuera.to_csv(diag / "proyectos_ad_en_no_tratados.csv", index=False)
    qc["proyectos_ad_mayoria_en_no_tratados"] = int((fuera.pct_en_no_tratados >= 50).sum())
    print(f"\n[COBERTURA DEL TRATAMIENTO] proyectos AD con >= 50 % de su area en municipios no tratados "
          f"en la D1: {qc['proyectos_ad_mayoria_en_no_tratados']}")
    print(fuera[fuera.pct_en_no_tratados >= 50][["ProjectID", "registro", "anio_inicio", "pct_en_no_tratados",
                                                 "nombre"]].round(1).to_string(index=False)[:2500])

    (diag / "qc_poligonos.json").write_text(json.dumps(qc, indent=2, ensure_ascii=False), encoding="utf-8")
    print(f"\n[OK] salidas en {out}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
