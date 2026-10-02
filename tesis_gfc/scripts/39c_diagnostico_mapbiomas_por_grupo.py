#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
39c_diagnostico_mapbiomas_por_grupo.py
======================================

Paso 3 del WBS de MapBiomas: decide, con una regla fijada ANTES de estimar
cualquier ATT, cuál es el outcome principal de MapBiomas:

    DEF^(1)  (persistencia de un año; ya calculado por 39)
    DEF^est  (conversión de bosque estable m años; requiere la pasada 39b)

Grupos (especificación principal):
    T = anio_inicio_tratamiento_todas_fuentes_redd_depurada en [2013, 2019]
    C = nunca tratados
    (cohortes fuera de la ventana se excluyen; filtro muestra_redd_depurada == 1)

Todas las métricas de la regla usan 2001-2012, antes de la primera cohorte:
lo que se compara es la MEDICIÓN, no el efecto del tratamiento.

Regla (constantes UMBRAL_*; cambiarlas solo antes de correr):
    R1  |regen/bruta_T - regen/bruta_C| > UMBRAL_R1         (sumas, 2001-2012)
    R2  DiD del ruido: [rev%_(2016-22) - rev%_(2001-12)]_T - [...]_C
        con IC bootstrap 95 % por municipio que excluye 0
    R3  tendencia pre-tratamiento diferencial de la brecha
            Delta_it = 100 * (Hansen_it - DEF1_it) / F2000_i
        y_it = a_i + d_t + b * T_i * (t - 2001) + e_it,  t <= 2012
        con EE agrupados por municipio; p < UMBRAL_R3
    Si alguna se cumple -> DEF^est principal; si ninguna -> DEF^(1).

Modo --outcome est5 (27/09/2026, tras la pasada 39b):
    Repite el diagnóstico con DEF^est5 como outcome. La decisión principal ya
    se tomó (DEF_est); este modo aplica la regla de LECTURA fijada antes de
    correrlo:
      R2_est5 significativa -> principal = DEF^est5 con persistencia (p1),
                               panel 2001-2023; si no -> DEF^est5 hasta 2024
      R3_est5 significativa -> la divergencia Hansen-MB no la explica la
                               rotación; decide el event study por fuente en R
    R1 no aplica (la pila estable no tiene regeneración); se reporta en su
    lugar la fracción de la bruta que ocurre sobre bosque estable, por grupo.

Uso (PowerShell):
    & $py scripts\\39c_diagnostico_mapbiomas_por_grupo.py
    & $py scripts\\39c_diagnostico_mapbiomas_por_grupo.py --sin-filtro-muestra
    & $py scripts\\39c_diagnostico_mapbiomas_por_grupo.py --outcome est5
"""
from __future__ import annotations

import argparse
import json
import sys
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd

# ---------------------------------------------------------------------------
# Regla de decisión (FIJAR ANTES DE CORRER)
# ---------------------------------------------------------------------------
UMBRAL_R1 = 0.10          # diferencia absoluta en regen/bruta
ALFA_R2 = 0.05            # IC bootstrap al 95 %
UMBRAL_R3 = 0.05          # p-valor de la tendencia diferencial
N_BOOT = 999
SEMILLA = 20260926

COL_COHORTE = "anio_inicio_tratamiento_todas_fuentes_redd_depurada"
COL_MUESTRA = "muestra_redd_depurada"
VENTANA_COHORTES = (2013, 2019)
PRE = (2001, 2012)         # antes de la primera cohorte
TARDE = (2016, 2022)       # post cambio de sensor (Landsat 8), p1 disponible
ANIO_MAX_P1 = 2023         # DEF^(1) no existe para 2024

# Regiones APROXIMADAS por departamento (solo para lectura; la regla usa T vs C)
REGION_DPTO = {
    "91": "Amazonía", "94": "Amazonía", "97": "Amazonía", "18": "Amazonía",
    "86": "Amazonía", "95": "Amazonía", "27": "Pacífico",
    "52": "Pacífico/Andes", "76": "Pacífico/Andes", "19": "Pacífico/Andes",
    "50": "Orinoquía", "81": "Orinoquía", "85": "Orinoquía", "99": "Orinoquía",
    "08": "Caribe", "13": "Caribe", "20": "Caribe", "23": "Caribe",
    "44": "Caribe", "47": "Caribe", "70": "Caribe", "88": "Caribe",
}


# ---------------------------------------------------------------------------
# Rutas
# ---------------------------------------------------------------------------
def encontrar_raiz() -> Path:
    for base in (Path(__file__).resolve().parent, Path.cwd().resolve()):
        for p in (base, *base.parents):
            if (p / "data" / ".raiz_canonica").is_file():
                return p
    sys.exit("[ERROR] No se encontró data/.raiz_canonica (¿carpeta sombra?).")


def leer(ruta: Path, **kw) -> pd.DataFrame:
    if not ruta.is_file():
        sys.exit(f"[ERROR] No existe {ruta}")
    try:
        return pd.read_csv(ruta, **kw)
    except PermissionError:
        sys.exit(f"[ERROR] {ruta} está bloqueado (¿abierto en Excel?). Ciérralo.")


# ---------------------------------------------------------------------------
# Datos
# ---------------------------------------------------------------------------
def construir_base(raiz: Path, filtrar_muestra: bool, outcome: str = "p1") -> pd.DataFrame:
    mb_dir = raiz / "data" / "interim" / "mapbiomas"
    mb = leer(mb_dir / "deforestacion_mapbiomas_municipio_anio.csv",
              dtype={"COD_DANE": str})
    mb["COD_DANE"] = mb["COD_DANE"].str.zfill(5)
    if outcome == "est5":
        # Las funciones de métricas leen def_mb_ha / def_mb_p1_ha / reversion_ha /
        # bosque_mb_2000_ha: en este modo esas columnas pasan a contener las de
        # bosque estable, y la bruta original se guarda en def_bruta_ha.
        est = leer(mb_dir / "deforestacion_estable_mapbiomas_municipio_anio.csv",
                   dtype={"COD_DANE": str})
        est["COD_DANE"] = est["COD_DANE"].str.zfill(5)
        req = {"def_est5_ha", "def_est5_p1_ha", "rev_est5_ha", "bosque_est5_2000_ha"}
        if req - set(est.columns):
            sys.exit(f"[ERROR] Faltan columnas en el panel estable: {req - set(est.columns)}")
        mb = mb.merge(est[["COD_DANE", "anio", *sorted(req)]], on=["COD_DANE", "anio"],
                      how="inner", validate="one_to_one")
        mb["def_bruta_ha"] = mb["def_mb_ha"]
        mb["def_mb_ha"] = mb["def_est5_ha"]
        mb["def_mb_p1_ha"] = mb["def_est5_p1_ha"]
        mb["reversion_ha"] = mb["rev_est5_ha"]
        mb["bosque_mb_2000_ha"] = mb["bosque_est5_2000_ha"]
        mb["regen_mb_ha"] = np.nan          # no existe en la pila estable

    panel = leer(raiz / "data" / "final" / "panel_con_tratamiento_actualizado.csv",
                 dtype={"COD_DANE": str})
    panel["COD_DANE"] = panel["COD_DANE"].str.zfill(5)
    faltan = {COL_COHORTE, "year", "loss_area_ha"} - set(panel.columns)
    if faltan:
        sys.exit(f"[ERROR] Faltan columnas en el panel: {faltan}")

    # Asignación de tratamiento a nivel municipio (invariante en el tiempo)
    cols = ["COD_DANE", COL_COHORTE] + ([COL_MUESTRA] if COL_MUESTRA in panel else [])
    asig = panel[cols].drop_duplicates("COD_DANE").copy()
    if asig["COD_DANE"].duplicated().any():
        sys.exit("[ERROR] La cohorte varía dentro de un municipio.")
    coh = pd.to_numeric(asig[COL_COHORTE], errors="coerce")
    nunca = coh.isna() | (coh <= 0) | (coh > 2100)
    en_ventana = coh.between(*VENTANA_COHORTES)
    asig["grupo"] = np.select([en_ventana, nunca], ["T", "C"], default="excluido")
    asig["cohorte"] = coh.where(en_ventana)

    print(f"[GRUPOS] antes del filtro: {asig['grupo'].value_counts().to_dict()}")
    if filtrar_muestra:
        if COL_MUESTRA not in asig:
            print(f"[AVISO] No existe {COL_MUESTRA}; no se filtra.")
        else:
            m = pd.to_numeric(asig[COL_MUESTRA], errors="coerce").fillna(0) == 1
            asig = asig[m]
            print(f"[GRUPOS] con {COL_MUESTRA}==1: {asig['grupo'].value_counts().to_dict()}")
    asig = asig[asig["grupo"] != "excluido"]

    # Hansen al panel de MapBiomas
    hansen = panel[["COD_DANE", "year", "loss_area_ha"]].rename(columns={"year": "anio"})
    df = (mb.merge(asig[["COD_DANE", "grupo", "cohorte"]], on="COD_DANE", how="inner")
            .merge(hansen, on=["COD_DANE", "anio"], how="left"))
    sin_h = df["loss_area_ha"].isna().sum()
    if sin_h:
        print(f"[AVISO] {sin_h} municipio-año sin Hansen (se excluyen de R3)")
    df["region"] = df["COD_DANE"].str[:2].map(REGION_DPTO).fillna("Andes")
    return df


# ---------------------------------------------------------------------------
# Métricas
# ---------------------------------------------------------------------------
def metricas(d: pd.DataFrame) -> dict:
    """Razones de sumas (ponderadas por área) y medianas municipales."""
    v = d[d["anio"] <= ANIO_MAX_P1]
    s = v[["def_mb_ha", "def_mb_p1_ha", "reversion_ha", "regen_mb_ha"]].sum()
    por_mun = v.groupby("COD_DANE")[["def_mb_ha", "regen_mb_ha", "reversion_ha"]].sum()
    por_mun = por_mun[por_mun["def_mb_ha"] > 10]           # evita razones de 0/0
    extra = {}
    if "def_bruta_ha" in v.columns:
        extra["estable_pct_de_bruta"] = 100 * s["def_mb_ha"] / v["def_bruta_ha"].sum()
    return {
        **extra,
        "municipios": d["COD_DANE"].nunique(),
        "bruta_kha_anio": s["def_mb_ha"] / v["anio"].nunique() / 1e3,
        "regen_bruta": s["regen_mb_ha"] / s["def_mb_ha"] if s["regen_mb_ha"] else np.nan,
        "regen_bruta_mediana_mun": (por_mun["regen_mb_ha"] / por_mun["def_mb_ha"]).median(),
        "reversion_pct": 100 * s["reversion_ha"] / s["def_mb_ha"],
        "p1_pct": 100 * s["def_mb_p1_ha"] / s["def_mb_ha"],
    }


def en(d, rango):
    return d[d["anio"].between(*rango)]


def tabla_grupos(df: pd.DataFrame, rango) -> pd.DataFrame:
    filas = []
    sub = en(df, rango)
    for g, d in sub.groupby("grupo"):
        filas.append({"grupo": g, "region": "Todas", **metricas(d)})
    for (g, r), d in sub.groupby(["grupo", "region"]):
        filas.append({"grupo": g, "region": r, **metricas(d)})
    # Sensibilidad: tratados sin Cartagena del Chairá (domina las sumas)
    t = sub[(sub["grupo"] == "T") & (sub["COD_DANE"] != "18150")]
    if len(t):
        filas.append({"grupo": "T sin 18150", "region": "Todas", **metricas(t)})
    # Controles comparables: solo departamentos con algún tratado
    dptos_t = set(sub.loc[sub["grupo"] == "T", "COD_DANE"].str[:2])
    c2 = sub[(sub["grupo"] == "C") & sub["COD_DANE"].str[:2].isin(dptos_t)]
    if len(c2):
        filas.append({"grupo": "C mismos dptos", "region": "Todas", **metricas(c2)})
    return pd.DataFrame(filas)


def rev_pct(d: pd.DataFrame) -> float:
    s = d[["reversion_ha", "def_mb_ha"]].sum()
    return 100 * s["reversion_ha"] / s["def_mb_ha"] if s["def_mb_ha"] else np.nan


def did_ruido(df: pd.DataFrame) -> float:
    t, c = df[df["grupo"] == "T"], df[df["grupo"] == "C"]
    return ((rev_pct(en(t, TARDE)) - rev_pct(en(t, PRE)))
            - (rev_pct(en(c, TARDE)) - rev_pct(en(c, PRE))))


def bootstrap_did_ruido(df: pd.DataFrame, rng) -> tuple[float, float, float]:
    """Bootstrap por conglomerado (municipio), estratificado por grupo.

    Vectorizado: la razón de sumas solo necesita, por municipio, reversión y
    bruta en cada período. Cada réplica remuestrea municipios con reemplazo
    y suma esos cuatro agregados.
    """
    est = did_ruido(df)
    agg = {}
    for nombre, rango in (("pre", PRE), ("tar", TARDE)):
        a = en(df, rango).groupby("COD_DANE")[["reversion_ha", "def_mb_ha"]].sum()
        agg[nombre] = a
    base = (agg["pre"].join(agg["tar"], lsuffix="_pre", rsuffix="_tar", how="outer")
            .fillna(0.0))
    grupo = df.drop_duplicates("COD_DANE").set_index("COD_DANE")["grupo"]
    base = base.join(grupo)

    def muestra(g):
        m = base[base["grupo"] == g][["reversion_ha_pre", "def_mb_ha_pre",
                                        "reversion_ha_tar", "def_mb_ha_tar"]].to_numpy()
        idx = rng.integers(0, len(m), size=(N_BOOT, len(m)))
        s = m[idx].sum(axis=1)                               # (N_BOOT, 4)
        with np.errstate(divide="ignore", invalid="ignore"):
            return 100 * (s[:, 2] / s[:, 3] - s[:, 0] / s[:, 1])

    reps = muestra("T") - muestra("C")
    lo, hi = np.nanpercentile(reps, [100 * ALFA_R2 / 2, 100 * (1 - ALFA_R2 / 2)])
    return est, lo, hi


def tendencia_brecha(df: pd.DataFrame, col: str = "def_mb_p1_ha") -> dict:
    """y_it = a_i + d_t + b*T_i*(t-2001), t<=2012; EE agrupados por municipio.

    Con panel balanceado el doble demeaning (y - ybar_i - ybar_t + ybar)
    absorbe exactamente ambos efectos fijos (Frisch-Waugh-Lovell).
    """
    d = en(df, PRE).dropna(subset=["loss_area_ha", col]).copy()
    d = d[d["bosque_mb_2000_ha"] > 0]
    n_anios = d.groupby("COD_DANE")["anio"].transform("nunique")
    d = d[n_anios == d["anio"].nunique()]                    # balanceado
    d["y"] = 100 * (d["loss_area_ha"] - d[col]) / d["bosque_mb_2000_ha"]
    d["x"] = (d["grupo"] == "T").astype(float) * (d["anio"] - PRE[0])

    def dm(col):
        return (d[col] - d.groupby("COD_DANE")[col].transform("mean")
                - d.groupby("anio")[col].transform("mean") + d[col].mean())

    y, x = dm("y").to_numpy(), dm("x").to_numpy()
    b = (x @ y) / (x @ x)
    u = y - b * x
    g = pd.Series(x * u).groupby(d["COD_DANE"].to_numpy()).sum().to_numpy()
    G = len(g)
    se = np.sqrt(G / (G - 1) * (g @ g)) / (x @ x)
    from math import erf, sqrt
    z = b / se
    p = 2 * (1 - 0.5 * (1 + erf(abs(z) / sqrt(2))))
    return {"b_pp_por_anio": b, "ee": se, "z": z, "p": p, "municipios": G,
            "anios": int(d["anio"].nunique())}


def correlaciones(df: pd.DataFrame) -> pd.DataFrame:
    """Hansen vs DEF^(1): correlación intra-municipio (serie anual demeaneada)."""
    v = df[(df["anio"] <= ANIO_MAX_P1)].dropna(subset=["loss_area_ha", "def_mb_p1_ha"])
    filas = []
    for (g, r), d in v.groupby(["grupo", "region"]):
        cors = []
        for _, m in d.groupby("COD_DANE"):
            if m["loss_area_ha"].std() > 0 and m["def_mb_p1_ha"].std() > 0:
                cors.append(np.corrcoef(m["loss_area_ha"], m["def_mb_p1_ha"])[0, 1])
        tot = d.groupby("COD_DANE")[["loss_area_ha", "def_mb_p1_ha"]].sum()
        filas.append({
            "grupo": g, "region": r, "municipios": d["COD_DANE"].nunique(),
            "corr_intra_mediana": np.median(cors) if cors else np.nan,
            "razon_hansen_mb": tot["loss_area_ha"].sum() / tot["def_mb_p1_ha"].sum(),
        })
    return pd.DataFrame(filas)


# ---------------------------------------------------------------------------
def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--sin-filtro-muestra", action="store_true")
    ap.add_argument("--outcome", choices=["p1", "est5"], default="p1",
                    help="p1 = decisión original; est5 = relectura con bosque estable")
    cfg = ap.parse_args()
    est5 = cfg.outcome == "est5"

    raiz = encontrar_raiz()
    out = (raiz / "data" / "interim" / "mapbiomas" / "diagnostics"
           / ("diag_grupos_est5" if est5 else "diag_grupos"))
    out.mkdir(parents=True, exist_ok=True)
    df = construir_base(raiz, filtrar_muestra=not cfg.sin_filtro_muestra, outcome=cfg.outcome)
    nt = df.loc[df["grupo"] == "T", "COD_DANE"].nunique()
    nc = df.loc[df["grupo"] == "C", "COD_DANE"].nunique()
    print(f"[BASE] T={nt} municipios, C={nc} municipios")
    if nt < 5:
        sys.exit("[ERROR] Muy pocos tratados: revisa la columna de cohorte o el filtro.")

    # --- Tablas descriptivas -------------------------------------------------
    t_pre = tabla_grupos(df, PRE)
    t_tar = tabla_grupos(df, TARDE)
    t_pre.to_csv(out / "metricas_por_grupo_2001_2012.csv", index=False)
    t_tar.to_csv(out / "metricas_por_grupo_2016_2022.csv", index=False)
    serie = (df[df["anio"] <= ANIO_MAX_P1]
             .groupby(["grupo", "anio"])[["reversion_ha", "regen_mb_ha", "def_mb_ha"]]
             .sum().reset_index())
    serie["rev_pct"] = 100 * serie["reversion_ha"] / serie["def_mb_ha"]
    serie["regen_bruta"] = serie["regen_mb_ha"] / serie["def_mb_ha"]
    serie.to_csv(out / "ruido_por_grupo_y_anio.csv", index=False)
    cor = correlaciones(df) if not est5 else correlaciones(df.assign(def_mb_p1_ha=df["def_mb_ha"]))
    cor.to_csv(out / "hansen_vs_mapbiomas.csv", index=False)

    # --- Regla ----------------------------------------------------------------
    tp = t_pre[t_pre["region"] == "Todas"].set_index("grupo")
    if est5:
        r1_dif, r1 = np.nan, False            # no aplica: sin regeneración
    else:
        r1_dif = tp.loc["T", "regen_bruta"] - tp.loc["C", "regen_bruta"]
        r1 = abs(r1_dif) > UMBRAL_R1

    rng = np.random.default_rng(SEMILLA)
    print(f"[R2] bootstrap ({N_BOOT} réplicas)...", flush=True)
    r2_est, r2_lo, r2_hi = bootstrap_did_ruido(df[df["anio"] <= ANIO_MAX_P1], rng)
    r2 = not (r2_lo <= 0 <= r2_hi)

    r3_res = tendencia_brecha(df, col="def_mb_ha" if est5 else "def_mb_p1_ha")
    r3 = r3_res["p"] < UMBRAL_R3

    if est5:
        decision = "DEF_est5_p1 (2001-2023)" if r2 else "DEF_est5 (2001-2024)"
        lectura_r3 = ("divergencia Hansen-MB NO explicada por rotación: decide event study por fuente"
                      if r3 else "divergencia Hansen-MB explicada por rotación de rastrojos")
    else:
        decision = "DEF_est" if (r1 or r2 or r3) else "DEF_p1"
        lectura_r3 = None
    resumen = {
        "fecha_utc": datetime.now(timezone.utc).isoformat(),
        "outcome": cfg.outcome,
        "grupos": {"T": nt, "C": nc, "filtro_muestra": not cfg.sin_filtro_muestra},
        "R1": ({"no_aplica": "pila estable sin regeneración",
                "estable_pct_bruta_T": tp.loc["T", "estable_pct_de_bruta"],
                "estable_pct_bruta_C": tp.loc["C", "estable_pct_de_bruta"]} if est5 else
               {"regen_bruta_T": tp.loc["T", "regen_bruta"],
                "regen_bruta_C": tp.loc["C", "regen_bruta"],
                "diferencia": r1_dif, "umbral": UMBRAL_R1, "cumple": bool(r1)}),
        "R2": {"did_reversion_pp": r2_est, "ic95": [r2_lo, r2_hi], "cumple": bool(r2)},
        "R3": {**r3_res, "umbral_p": UMBRAL_R3, "cumple": bool(r3), "lectura": lectura_r3},
        ("outcome_principal_final" if est5 else "decision_outcome_principal"): decision,
    }
    nombre = "diagnostico_est5.json" if est5 else "decision_outcome_mapbiomas.json"
    (out / nombre).write_text(
        json.dumps(resumen, indent=2, ensure_ascii=False, default=float), encoding="utf-8")

    pd.set_option("display.width", 160)
    col = (["grupo", "region", "municipios", "bruta_kha_anio", "estable_pct_de_bruta",
            "reversion_pct", "p1_pct"] if est5 else
           ["grupo", "region", "municipios", "bruta_kha_anio", "regen_bruta",
            "regen_bruta_mediana_mun", "reversion_pct", "p1_pct"])
    print(f"\n=== Métricas 2001-2012 (pre-tratamiento) | outcome: {cfg.outcome} ===")
    print(t_pre[col].round(3).to_string(index=False))
    print("\n=== Hansen vs " + ("DEF^est5" if est5 else "DEF^(1)") + " ===")
    print(cor.round(3).to_string(index=False))
    print("\n=== REGLA ===")
    if est5:
        print(f"R1 no aplica | estable/bruta T={tp.loc['T','estable_pct_de_bruta']:.1f}% "
              f"C={tp.loc['C','estable_pct_de_bruta']:.1f}%")
    else:
        print(f"R1 regen/bruta T={tp.loc['T','regen_bruta']:.3f} C={tp.loc['C','regen_bruta']:.3f} "
              f"dif={r1_dif:+.3f} (umbral {UMBRAL_R1}) -> {'CUMPLE' if r1 else 'no'}")
    print(f"R2 DiD ruido = {r2_est:+.2f} pp, IC95 [{r2_lo:+.2f}, {r2_hi:+.2f}] "
          f"-> {'CUMPLE' if r2 else 'no'}")
    print(f"R3 tendencia brecha b = {r3_res['b_pp_por_anio']:+.4f} pp/año "
          f"(EE {r3_res['ee']:.4f}, p = {r3_res['p']:.3f}, G = {r3_res['municipios']}) "
          f"-> {'CUMPLE' if r3 else 'no'}")
    if est5:
        print(f"    lectura R3: {lectura_r3}")
    print(f"\n>>> OUTCOME PRINCIPAL MAPBIOMAS: {decision}")
    print(f"[OK] salidas en {out}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
