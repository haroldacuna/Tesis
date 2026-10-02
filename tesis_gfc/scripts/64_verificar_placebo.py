#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
64_verificar_placebo.py  (tarea 1.9)
====================================

Verifica el grupo placebo AFOLU no-REDD+ que usa el 60 (bloque A.6):
municipios con proyecto forestal (anio_inicio_tratamiento_todas_fuentes > 0)
y sin ningun REDD+ (anio_inicio_tratamiento_todas_fuentes_redd = 0).

Un placebo solo sirve si el municipio NO tiene ninguna exposicion a REDD+. La
definicion por columnas puede dejar pasar cuatro casos:
  f_ambiguo   municipio fuera de la muestra depurada (REDD+ retirado/rechazado)
  f_redd_sin  algun evento con nombre REDD+ / deforestacion evitada que no entro
              al tratamiento (p. ej., sin fecha de inicio)
  f_d2        municipio de un proyecto excluido por D2 (vinculo con carbono)
  f_poligono  >= 5 % de su area cubierta por un poligono de deforestacion
              evitada (Karnik et al.), aunque el proyecto este asignado a otro
Ademas lista los proyectos de cada municipio para revisarlos a ojo.

Salida: data/interim/diagnostics/placebo_afolu_verificacion.csv
        (columna placebo_limpio = en ventana y sin ninguna bandera)
USO (desde tesis_gfc/):  python scripts\\64_verificar_placebo.py
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

import pandas as pd

VENTANA = range(2013, 2020)
UMBRAL_PCT = 5
PANEL = Path("data/final/panel_con_tratamiento_actualizado.csv")
EVENTOS = Path("data/interim/eventos_carbono_consolidado.csv")
COBERTURA = Path("data/interim/proyectos_poligonos/cobertura_municipal.csv")
OUT = Path("data/interim/diagnostics/placebo_afolu_verificacion.csv")
PAT_REDD = re.compile(r"redd|deforest|avoided|evitad", re.I)


def pad5(s: pd.Series) -> pd.Series:
    return s.astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5)


def main() -> int:
    if not Path("data/.raiz_canonica").exists():
        sys.exit("Corre desde tesis_gfc/ (raiz canonica).")
    cols = ["COD_DANE", "anio_inicio_tratamiento_todas_fuentes", "anio_inicio_tratamiento_todas_fuentes_redd",
            "muestra_redd_depurada", "proyectos_excluidos_d2"]
    p = pd.read_csv(PANEL, usecols=lambda c: c in cols, dtype={"COD_DANE": str}, low_memory=False)
    p["COD_DANE"] = pad5(p["COD_DANE"])
    num = lambda c: pd.to_numeric(p[c], errors="coerce").fillna(0) if c in p else 0
    m = pd.DataFrame({"COD_DANE": p["COD_DANE"],
                      "G_afolu": num("anio_inicio_tratamiento_todas_fuentes"),
                      "G_redd": num("anio_inicio_tratamiento_todas_fuentes_redd"),
                      "muestra_dep": num("muestra_redd_depurada"),
                      "d2": num("proyectos_excluidos_d2")})
    m = m.groupby("COD_DANE").agg(G_afolu=("G_afolu", lambda s: s[s > 0].min() if (s > 0).any() else 0),
                                  G_redd=("G_redd", "max"), muestra_dep=("muestra_dep", "min"),
                                  d2=("d2", "max")).reset_index()
    plac = m[(m["G_afolu"] > 0) & (m["G_redd"] == 0)].copy()
    print(f"Municipios AFOLU no-REDD+ (definicion del 60): {len(plac)}")

    ev = pd.read_csv(EVENTOS, dtype=str)
    ev["COD_DANE"] = pad5(ev["COD_DANE"])
    ev["excl"] = ev.get("excluido_d2", "False").astype(str).str.lower().eq("true")
    nombres = (ev[ev["COD_DANE"].isin(plac["COD_DANE"])]
               .assign(etq=lambda d: d["fuente"] + ": " + d["nombre_proyecto"].fillna("").str[:55]
                       + d["anio_inicio"].fillna("s/f").map(lambda a: f" ({str(a)[:4]})")
                       + d["excl"].map({True: " [D2]", False: ""}))
               .groupby("COD_DANE")["etq"].apply(lambda s: " | ".join(sorted(set(s)))))
    redd_sin = (ev[ev["nombre_proyecto"].fillna("").str.contains(PAT_REDD)]
                .groupby("COD_DANE").size())

    plac["en_ventana"] = plac["G_afolu"].isin(VENTANA)
    plac["f_ambiguo"] = plac["muestra_dep"] == 0
    plac["f_redd_sin"] = plac["COD_DANE"].isin(redd_sin.index)
    plac["f_d2"] = plac["d2"] > 0
    if COBERTURA.exists():
        cob = pd.read_csv(COBERTURA, dtype={"COD_DANE": str})
        cob["COD_DANE"] = pad5(cob["COD_DANE"])
        cub = cob.loc[pd.to_numeric(cob["pct_ad"], errors="coerce") >= UMBRAL_PCT, "COD_DANE"]
        plac["f_poligono"] = plac["COD_DANE"].isin(cub)
    else:
        print(f"[!] No encuentro {COBERTURA}: se omite la bandera de poligonos")
        plac["f_poligono"] = False
    banderas = ["f_ambiguo", "f_redd_sin", "f_d2", "f_poligono"]
    plac["placebo_limpio"] = plac["en_ventana"] & ~plac[banderas].any(axis=1)
    plac["proyectos"] = plac["COD_DANE"].map(nombres).fillna("")

    pd.set_option("display.width", 250); pd.set_option("display.max_colwidth", 120)
    print("\nBanderas (cuantos municipios las tienen):", plac[banderas].sum().to_dict())
    print(f"En la ventana 2013-2019: {plac['en_ventana'].sum()} | placebo limpio: {plac['placebo_limpio'].sum()}")
    print("\nCohortes del placebo limpio:", plac.loc[plac["placebo_limpio"], "G_afolu"].astype(int)
          .value_counts().sort_index().to_dict())
    print("\nDetalle (en la ventana o con alguna bandera):")
    ver = plac[plac["en_ventana"] | plac[banderas].any(axis=1)].sort_values(["placebo_limpio", "G_afolu"])
    print(ver[["COD_DANE", "G_afolu", "placebo_limpio"] + banderas + ["proyectos"]].to_string(index=False))
    OUT.parent.mkdir(parents=True, exist_ok=True)
    plac.to_csv(OUT, index=False, encoding="utf-8-sig")
    print(f"\nGuardado: {OUT}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
