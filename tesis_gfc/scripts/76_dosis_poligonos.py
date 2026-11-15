#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
76_dosis_poligonos.py
=====================

Dosis de tratamiento con la GEOMETRIA de los proyectos (lista de chequeo,
puntos 1, 3, 4 y 6): corrige la asignacion espacial ANTES de excluir.

Problema que resuelve
---------------------
La dosis del 38/39 usa el area DECLARADA de cada proyecto de Verra y la
asigna integra al municipio donde cae su punto. Un proyecto que abarca varios
municipios infla la dosis del anfitrion (9 de los 42 tratados con dosis > 1) y
deja en cero a los vecinos. El 52 ya intersecto los poligonos de Karnik et al.
(2025) con los limites municipales; aqui esa interseccion se convierte en la
dosis del analisis de intensidad.

Definiciones (EPSG:9377, poligonos AD = deforestacion evitada)
---------------------------------------------------------------
    dosis_area_i   = | (U_k P_k) ∩ M_i | / | M_i |                       (52)

    dosis_bosque_i = sum_c B_c * a_c^AD / a_c  /  sum_c B_c ,  c ∈ celdas de i

donde B_c es el bosque natural de 2000 de la celda c de 2 x 2 km (MapBiomas,
55), a_c su area y a_c^AD el area de la celda dentro de la UNION de los
poligonos AD. La dosis de bosque supone el bosque distribuido uniformemente
dentro de cada celda de 400 ha; numerador y denominador vienen de la misma
fuente, de modo que la dosis no puede superar 1.

Clasificacion geografica de cada municipio tratado (D1 depurada)
----------------------------------------------------------------
    poligono     : la union de poligonos AD cubre >= 1 % del municipio y
                   >= 100 ha (regla del 52, fijada el 27/09/2026). Tiene dosis.
    reubicado    : su proyecto REDD+ de Verra SI tiene poligono en Karnik, pero
                   ese poligono no lo cubre (cae sobre todo en otros
                   municipios): el punto del registro es la sede o un punto de
                   referencia. Sin dosis; el 60 (A10) lo saca del tratamiento.
    sin_poligono : ningun proyecto suyo tiene poligono en la base. Se mantiene
                   la asignacion por punto en el analisis binario; sin dosis
                   en la principal, con la dosis declarada (truncada) en la
                   version hibrida, que es solo de sensibilidad.

Terciles: por dosis_bosque (tercil_dosis, nombre que lee el 40) y por
dosis_area (tercil_area), entre TODOS los tratados de la D1 depurada con
dosis, igual que el 39.

Insumos
-------
    data/interim/proyectos_poligonos/cobertura_municipal.csv     (52)
    data/interim/proyectos_poligonos/proyectos_municipio.csv     (52)
    data/interim/celdas/celdas_atributos.csv                     (55)
    data/interim/dosis_tratamiento_final.csv                     (39, dosis declarada)
    data/interim/verra_platts_colombia_con_municipio.csv         (24/25)
    data/final/panel_con_tratamiento_actualizado.csv             (26)

Salidas
-------
    data/interim/dosis_tratamiento_poligonos.csv         (la lee el 40 via RUTA_DOSIS)
    data/interim/diagnostics/76_dosis_declarada_vs_poligono.csv
    outputs/tablas/76_resumen_dosis.csv

Uso (PowerShell, desde tesis_gfc):
    & $py scripts\\76_dosis_poligonos.py
"""
from __future__ import annotations

import sys
from pathlib import Path

import numpy as np
import pandas as pd

MIN_PCT, MIN_HA = 1.0, 100.0          # regla del 52: municipio "cubierto"
VENTANA = range(2013, 2020)
COL_G = "anio_inicio_tratamiento_todas_fuentes_redd_depurada"
COL_MUE = "muestra_redd_depurada"
METODOLOGIAS_REDD = ("VM0006", "VM0007", "VM0009", "VM0015")
EXCLUIDOS_D2_RESPALDO = {"1657", "5680", "5335"}   # si no se puede importar filtro_sectorial


def raiz() -> Path:
    for base in (Path(__file__).resolve().parent, Path.cwd().resolve()):
        for p in (base, *base.parents):
            if (p / "data" / ".raiz_canonica").is_file():
                return p
    sys.exit("[ERROR] No se encontro data/.raiz_canonica: corre desde tesis_gfc/.")


def pad5(s: pd.Series) -> pd.Series:
    return pd.to_numeric(s, errors="coerce").astype("Int64").astype(str).str.zfill(5)


def leer(ruta: Path, **kw) -> pd.DataFrame:
    if not ruta.is_file():
        sys.exit(f"[ERROR] Falta {ruta}. Corre antes el script que lo genera (ver docstring).")
    return pd.read_csv(ruta, **kw)


def excluidos_d2() -> set[str]:
    try:
        sys.path.insert(0, str(Path(__file__).resolve().parent))
        from filtro_sectorial import EXCLUSIONES  # noqa: E402
        return {str(i) for e in EXCLUSIONES if e["fuente"].lower() == "verra" for i in e["ids"]}
    except Exception as exc:  # pragma: no cover
        print(f"[!] No se pudo importar filtro_sectorial ({exc}); se usan los IDs de respaldo.")
        return EXCLUIDOS_D2_RESPALDO


def terciles(s: pd.Series) -> pd.Series:
    out = pd.Series(np.nan, index=s.index, dtype=object)
    ok = s.notna()
    if ok.sum() >= 9:
        out[ok] = pd.qcut(s[ok].rank(method="first"), 3, labels=["bajo", "medio", "alto"]).astype(str)
    else:
        print(f"[!] Solo {int(ok.sum())} municipios con dosis: no alcanza para terciles.")
    return out


def main() -> int:
    R = raiz()
    I = R / "data" / "interim"
    diag = I / "diagnostics"; diag.mkdir(parents=True, exist_ok=True)
    tab = R / "outputs" / "tablas"; tab.mkdir(parents=True, exist_ok=True)

    # --- 1. Cobertura (52) y bosque dentro de los poligonos (celdas) --------
    cob = leer(I / "proyectos_poligonos" / "cobertura_municipal.csv", dtype={"COD_DANE": str})
    cob["COD_DANE"] = pad5(cob["COD_DANE"])
    pm = leer(I / "proyectos_poligonos" / "proyectos_municipio.csv", dtype={"COD_DANE": str})
    pm["COD_DANE"] = pad5(pm["COD_DANE"])

    cel = leer(I / "celdas" / "celdas_atributos.csv", dtype={"COD_DANE": str},
               usecols=["area_ha", "bosque2000_ha", "ha_ad", "COD_DANE"])
    cel = cel[cel["COD_DANE"].notna()].copy()
    cel["COD_DANE"] = pad5(cel["COD_DANE"])
    cel["bosque_ad"] = cel["bosque2000_ha"] * (cel["ha_ad"] / cel["area_ha"]).clip(0, 1)
    bos = cel.groupby("COD_DANE", as_index=False).agg(bosque2000_mb_ha=("bosque2000_ha", "sum"),
                                                      bosque_ad_ha=("bosque_ad", "sum"),
                                                      ha_ad_celdas=("ha_ad", "sum"))
    # Control de calidad: el area AD de las celdas debe reproducir la del 52
    q = cob.merge(bos, on="COD_DANE")
    q = q[q["ha_ad"] > 1000]
    r = (q["ha_ad_celdas"] / q["ha_ad"]).median()
    print(f"[QC] area AD celdas / area AD 52 (mediana, municipios con > 1.000 ha): {r:.3f}")
    if not 0.95 <= r <= 1.05:
        sys.exit("[ERROR] Las celdas no reproducen la cobertura del 52: revisa que 52 y 55 usen la misma base.")

    # --- 2. Tratados D1 depurada --------------------------------------------
    p = leer(R / "data" / "final" / "panel_con_tratamiento_actualizado.csv", dtype={"COD_DANE": str},
             usecols=["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR", COL_G, COL_MUE], low_memory=False)
    p["COD_DANE"] = pad5(p["COD_DANE"])
    d1 = (p.groupby("COD_DANE")
          .agg(NOMBRE_MPI=("NOMBRE_MPI", "first"), DPTO_CNMBR=("DPTO_CNMBR", "first"),
               cohorte=(COL_G, "min"), muestra_dep=(COL_MUE, "min")).reset_index())
    d1 = d1[d1["cohorte"].fillna(0) > 0].copy()
    d1["principal"] = (d1["muestra_dep"] == 1) & d1["cohorte"].isin(VENTANA)
    print(f"[D1] tratados D1 depurada: {len(d1)} | en la muestra principal: {int(d1.principal.sum())}")

    # --- 3. Proyectos REDD+ de Verra de cada tratado y si tienen poligono ---
    v = leer(I / "verra_platts_colombia_con_municipio.csv", dtype={"COD_DANE": str}, encoding="utf-8-sig")
    v["COD_DANE"] = v["COD_DANE"].astype(str).str.extract(r"(\d+)")[0].str.zfill(5)
    v["vid"] = pd.to_numeric(v["projectId"], errors="coerce").astype("Int64").astype(str)
    txt = (v.get("afoluNames", "").astype(str) + " " + v.get("methodologies", "").astype(str)).str.upper()
    v = v[txt.str.contains("REDD|" + "|".join(METODOLOGIAS_REDD), na=False) & ~v["vid"].isin(excluidos_d2())]
    v["ProjectID"] = "VCS" + v["vid"]
    con_pol = set(pm["ProjectID"])
    cubre = pm[(pm["pct_del_municipio"] >= MIN_PCT) & (pm["ha_interseccion"] >= MIN_HA)]
    cubre = set(zip(cubre["ProjectID"], cubre["COD_DANE"]))
    vt = v[v["COD_DANE"].isin(d1["COD_DANE"])][["COD_DANE", "ProjectID"]].drop_duplicates()
    vt["tiene_poligono"] = vt["ProjectID"].isin(con_pol)
    vt["cubre_anfitrion"] = [(a, b) in cubre for a, b in zip(vt["ProjectID"], vt["COD_DANE"])]
    vres = vt.groupby("COD_DANE").agg(proyectos_verra=("ProjectID", lambda s: ";".join(sorted(s))),
                                      verra_con_poligono=("tiene_poligono", "any"),
                                      verra_cubre=("cubre_anfitrion", "any")).reset_index()

    # --- 4. Dosis y clase geografica -----------------------------------------
    dec = leer(I / "dosis_tratamiento_final.csv", dtype={"COD_DANE": str})
    dec["COD_DANE"] = pad5(dec["COD_DANE"])
    dec = dec[["COD_DANE", "area_redd_ha", "dosis_area_cruda", "dosis_bosque_cruda", "dosis_area",
               "dosis_bosque", "tercil_dosis"]].rename(columns={
        "area_redd_ha": "area_declarada_ha", "dosis_area_cruda": "dosis_area_declarada_cruda",
        "dosis_bosque_cruda": "dosis_bosque_declarada_cruda", "dosis_area": "dosis_area_declarada",
        "dosis_bosque": "dosis_bosque_declarada", "tercil_dosis": "tercil_declarado"})

    x = (d1.merge(cob[["COD_DANE", "ha_municipio", "ha_ad", "pct_ad", "cubierto_ad", "cohorte_poligono",
                       "proyectos_ad"]], on="COD_DANE", how="left")
           .merge(bos, on="COD_DANE", how="left").merge(vres, on="COD_DANE", how="left")
           .merge(dec, on="COD_DANE", how="left"))
    x["cubierto_ad"] = x["cubierto_ad"].astype(str).str.lower().eq("true")
    for c in ("verra_con_poligono", "verra_cubre"):
        x[c] = x[c].astype("boolean").fillna(False).astype(bool)

    x["clase_geo"] = np.select(
        [x["cubierto_ad"], x["verra_con_poligono"] & ~x["verra_cubre"]],
        ["poligono", "reubicado"], default="sin_poligono")
    pol = x["clase_geo"].eq("poligono")
    x["dosis_area"] = np.where(pol, x["ha_ad"] / x["ha_municipio"], np.nan)
    x["dosis_bosque"] = np.where(pol & (x["bosque2000_mb_ha"] > 0),
                                 x["bosque_ad_ha"] / x["bosque2000_mb_ha"], np.nan)
    x["tiene_dosis"] = pol.astype(int)
    # Hibrida (solo sensibilidad): poligono si existe; si no, la declarada truncada,
    # salvo los reubicados (su area declarada esta en otro municipio).
    usa_dec = x["clase_geo"].eq("sin_poligono")
    x["dosis_area_hibrida"] = np.where(pol, x["dosis_area"], np.where(usa_dec, x["dosis_area_declarada"], np.nan))
    x["dosis_bosque_hibrida"] = np.where(pol, x["dosis_bosque"], np.where(usa_dec, x["dosis_bosque_declarada"], np.nan))
    x["tiene_dosis_hibrida"] = x["dosis_bosque_hibrida"].notna().astype(int)
    x["tercil_dosis"] = terciles(x["dosis_bosque"])
    x["tercil_area"] = terciles(x["dosis_area"])
    x["tercil_hibrido"] = terciles(x["dosis_bosque_hibrida"])
    # Compatibilidad con el 71 y el 39: por construccion ninguna dosis supera 1
    x["desborda_area"] = 0; x["desborda_bosque"] = 0
    if (x[["dosis_area", "dosis_bosque"]] > 1 + 1e-9).any().any():
        sys.exit("[ERROR] Una dosis por poligono supera 1: revisa la union del 52.")

    # --- 5. Reporte ---------------------------------------------------------
    print("\n[CLASE GEOGRAFICA] todos los tratados D1 | muestra principal")
    print(pd.crosstab(x["clase_geo"], x["principal"], margins=True).to_string())
    print("\nReubicados (su poligono esta en otros municipios):")
    print(x[x.clase_geo == "reubicado"][["COD_DANE", "NOMBRE_MPI", "cohorte", "proyectos_verra", "pct_ad"]]
          .round(2).to_string(index=False))
    print("\nSin poligono en la base (se mantiene la asignacion por punto):")
    print(x[x.clase_geo == "sin_poligono"][["COD_DANE", "NOMBRE_MPI", "cohorte", "principal", "proyectos_verra",
                                            "dosis_bosque_declarada"]].round(3).to_string(index=False))

    ambos = x[pol & x["dosis_bosque_declarada"].notna()]
    rho_b = ambos["dosis_bosque"].corr(ambos["dosis_bosque_declarada"], method="spearman")
    rho_a = ambos["dosis_area"].corr(ambos["dosis_area_declarada"], method="spearman")
    mismo = (ambos["tercil_dosis"] == ambos["tercil_declarado"]).mean()
    print(f"\n[DECLARADA vs POLIGONO] {len(ambos)} tratados con ambas | Spearman bosque {rho_b:.2f}, "
          f"area {rho_a:.2f} | mismo tercil: {100 * mismo:.0f} %")
    ex = x[x["dosis_bosque_declarada_cruda"] > 1]
    print(f"Los {len(ex)} tratados con dosis declarada > 1, con el poligono:")
    print(ex[["NOMBRE_MPI", "clase_geo", "dosis_bosque_declarada_cruda", "dosis_bosque"]].round(3).to_string(index=False))

    print("\n[TERCILES] dosis de bosque y de area (todos los tratados con dosis)")
    for col, val in (("tercil_dosis", "dosis_bosque"), ("tercil_area", "dosis_area")):
        g = x[pol].groupby(col).agg(n=("COD_DANE", "size"), en_principal=("principal", "sum"),
                                    min=(val, "min"), max=(val, "max"), cohorte_media=("cohorte", "mean"))
        print(f"  {col}:\n{g.reindex(['bajo', 'medio', 'alto']).round(3).to_string()}")
    print(f"  mismo tercil por bosque y por area: {100 * (x.loc[pol, 'tercil_dosis'] == x.loc[pol, 'tercil_area']).mean():.0f} %")

    pp = x[x["principal"]]
    print("\n[CONTDID] tratados de la principal con dosis por cohorte (se exigen >= 3 dosis distintas):")
    print(pp[pp.tiene_dosis == 1].groupby("cohorte").agg(n=("COD_DANE", "size"),
          distintas=("dosis_bosque", "nunique")).to_string())

    # --- 6. Escritura -------------------------------------------------------
    cols = ["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR", "cohorte", "principal", "clase_geo", "tiene_dosis",
            "dosis_area", "dosis_bosque", "tercil_dosis", "tercil_area",
            "dosis_area_hibrida", "dosis_bosque_hibrida", "tiene_dosis_hibrida", "tercil_hibrido",
            "dosis_area_declarada", "dosis_bosque_declarada", "dosis_area_declarada_cruda",
            "dosis_bosque_declarada_cruda", "tercil_declarado", "area_declarada_ha",
            "ha_municipio", "ha_ad", "pct_ad", "bosque2000_mb_ha", "bosque_ad_ha", "cohorte_poligono",
            "proyectos_ad", "proyectos_verra", "desborda_area", "desborda_bosque"]
    x = x[cols].sort_values(["cohorte", "COD_DANE"])
    x.to_csv(I / "dosis_tratamiento_poligonos.csv", index=False)
    x.to_csv(diag / "76_dosis_declarada_vs_poligono.csv", index=False)

    res = []
    for nombre, d in (("todos D1", x), ("principal", x[x["principal"]])):
        for c in ("poligono", "reubicado", "sin_poligono"):
            res.append({"muestra": nombre, "indicador": f"n_{c}", "valor": int((d.clase_geo == c).sum())})
        dd = d[d.tiene_dosis == 1]
        for c in ("dosis_area", "dosis_bosque"):
            res.append({"muestra": nombre, "indicador": f"{c}_mediana", "valor": dd[c].median()})
            res.append({"muestra": nombre, "indicador": f"{c}_max", "valor": dd[c].max()})
    res += [{"muestra": "todos D1", "indicador": "spearman_bosque_declarada_poligono", "valor": rho_b},
            {"muestra": "todos D1", "indicador": "spearman_area_declarada_poligono", "valor": rho_a},
            {"muestra": "todos D1", "indicador": "pct_mismo_tercil_declarado", "valor": 100 * mismo},
            {"muestra": "todos D1", "indicador": "n_declarada_mayor_1", "valor": len(ex)}]
    pd.DataFrame(res).to_csv(tab / "76_resumen_dosis.csv", index=False)
    print(f"\n[OK] {I / 'dosis_tratamiento_poligonos.csv'}\n[OK] {tab / '76_resumen_dosis.csv'}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
