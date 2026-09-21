#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
39_reconciliar_dosis_panel.py
=============================
Reconcilia la dosis de tratamiento (salida de 38) con el universo de
tratamiento D1 del panel, y deja la dosis lista para Callaway-Sant'Anna por
terciles.

Regla de gobierno: el universo de tratamiento lo MANDA EL PANEL
(`tratado_todas_fuentes_redd`), porque es el que se estimo y el que esta
validado y triangulado. La dosis se adjunta a ese universo; no lo redefine.
Donde un municipio tratado no tiene dosis, se marca y NO se imputa.

Asimetria esperada: D1 es "todas_fuentes" (Verra + Cercarbono + otros
registros), mientras que la dosis solo puede construirse con Verra, que es el
unico registro que publica area declarada. Los municipios tratados solo por
fuentes distintas de Verra quedaran sin dosis. El script los lista.

Insumos:
    data/interim/dosis_tratamiento_municipio.csv
    data/final/panel_con_tratamiento_actualizado.csv

Salidas:
    data/interim/dosis_tratamiento_final.csv
    data/interim/diagnostics/reconciliacion_dosis_panel.csv
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

import numpy as np
import pandas as pd

CENTINELA = Path("data/.raiz_canonica")
DOSIS = Path("data/interim/dosis_tratamiento_municipio.csv")
PANEL = Path("data/final/panel_con_tratamiento_actualizado.csv")
OUT_FINAL = Path("data/interim/dosis_tratamiento_final.csv")
OUT_RECON = Path("data/interim/diagnostics/reconciliacion_dosis_panel.csv")

# Especificacion principal: D1 depurada por estado de registro (ver la
# seccion "D1 DEPURADA" en 26_consolidar_fuentes_carbono.py). Con --original
# se usa la D1 sin depurar y la muestra completa, para la sensibilidad.
DEFS = {
    "depurada": ("tratado_todas_fuentes_redd_depurada",
                 "anio_inicio_tratamiento_todas_fuentes_redd_depurada",
                 "muestra_redd_depurada"),
    "original": ("tratado_todas_fuentes_redd",
                 "anio_inicio_tratamiento_todas_fuentes_redd",
                 None),
}

# Exclusiones por error de asignacion geografica. Se dejan en el codigo, con
# su motivo, para que la decision quede auditada en el control de versiones.
EXCLUSIONES = {
    # Barranquilla ya se excluye aguas arriba (SOSPECHOSOS_VERRA en el 26);
    # se deja aqui como red de seguridad por si se corre con un panel viejo.
    "08001": ("BARRANQUILLA", "Proyecto CO2ROZO, 400.000 ha geocodificadas sobre un "
              "municipio urbano de 15.413 ha. El punto corresponde a la sede del "
              "proponente. Excluido tambien del tratamiento binario."),
    "68464": ("MOGOTES", "Proyecto agrupado del corredor de robles Guantiva-La Rusia-"
              "Iguaque. La ubicacion es plausible y el municipio se mantiene tratado en el "
              "analisis binario, pero el area declarada (1.075.348 ha) es la frontera del "
              "proyecto agrupado completo y no mide la superficie bajo conservacion en "
              "Mogotes (dosis bosque 32). Se excluye solo del analisis de intensidad."),
}


def anclar_raiz_canonica() -> Path:
    actual = Path.cwd().resolve()
    for c in [actual, *actual.parents]:
        if (c / CENTINELA).exists():
            return c
    raise SystemExit(f"ERROR: no se encontro '{CENTINELA}'. Corre desde tesis_gfc/.")


def cargar_dosis(raiz: Path) -> pd.DataFrame:
    ruta = raiz / DOSIS
    if not ruta.exists():
        raise SystemExit(f"ERROR: falta {ruta}. Corre primero 38_construir_dosis_tratamiento.py")
    d = pd.read_csv(ruta, dtype={"COD_DANE": str})
    d["COD_DANE"] = d["COD_DANE"].str.zfill(5)
    return d


def cargar_d1(raiz: Path, definicion: str) -> pd.DataFrame:
    """Universo D1 del panel: un renglon por municipio tratado, con su cohorte."""
    COL_TRAT, COL_COHORTE, COL_MUESTRA = DEFS[definicion]
    ruta = raiz / PANEL
    if not ruta.exists():
        raise SystemExit(f"ERROR: falta {ruta}")
    cols = ["COD_DANE", COL_TRAT, COL_COHORTE] + ([COL_MUESTRA] if COL_MUESTRA else [])
    p = pd.read_csv(ruta, dtype={"COD_DANE": str}, usecols=lambda c: c in cols)
    faltan = set(cols) - set(p.columns)
    if faltan:
        raise SystemExit(
            f"ERROR: el panel no tiene {sorted(faltan)}.\n"
            "       Corre primero la version actual de 26_consolidar_fuentes_carbono.py."
        )
    p["COD_DANE"] = p["COD_DANE"].str.zfill(5)

    if COL_MUESTRA:
        fuera = sorted(p.loc[p[COL_MUESTRA] == 0, "COD_DANE"].unique())
        print(f"[ok] muestra depurada: se excluyen {len(fuera)} municipios de "
              f"tratamiento ambiguo {fuera}")
        p = p[p[COL_MUESTRA] == 1]

    # El panel es largo (municipio x anio). La marca de tratamiento puede ser
    # estatica o activarse en el anio de adopcion: se toma el maximo por
    # municipio para capturar "alguna vez tratado".
    p[COL_TRAT] = pd.to_numeric(p[COL_TRAT], errors="coerce").fillna(0)
    p[COL_COHORTE] = pd.to_numeric(p[COL_COHORTE], errors="coerce")
    g = p.groupby("COD_DANE").agg(
        tratado_panel=(COL_TRAT, "max"),
        cohorte_panel=(COL_COHORTE, "min"),
    ).reset_index()
    return g[g["tratado_panel"] > 0].copy()


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--n-terciles", type=int, default=3,
                    help="Numero de grupos de intensidad (3 = terciles)")
    ap.add_argument("--original", action="store_true",
                    help="Usar la D1 sin depurar y la muestra completa (sensibilidad)")
    args = ap.parse_args()
    definicion = "original" if args.original else "depurada"

    raiz = anclar_raiz_canonica()
    print(f"[ok] definicion de tratamiento: D1 {definicion}\n")
    dosis = cargar_dosis(raiz)
    d1 = cargar_d1(raiz, definicion)

    # -- 1. Exclusiones ------------------------------------------------------
    print("=" * 70)
    print("1. EXCLUSIONES POR ERROR DE ASIGNACION")
    print("=" * 70)
    for cod, (nom, motivo) in EXCLUSIONES.items():
        presente = cod in set(dosis.loc[dosis["tratado"] == 1, "COD_DANE"])
        en_d1 = cod in set(d1["COD_DANE"])
        print(f"  {cod} {nom:14s} en dosis: {'si' if presente else 'no'}   "
              f"en panel D1: {'SI  <-- revisar' if en_d1 else 'no'}")
    print()

    trat_dosis = dosis[(dosis["tratado"] == 1) & (~dosis["COD_DANE"].isin(EXCLUSIONES))].copy()

    # -- 2. Comparacion de conjuntos -----------------------------------------
    A = set(trat_dosis["COD_DANE"])      # con dosis (Verra, sin errores)
    B = set(d1["COD_DANE"])              # tratados D1 del panel

    ambos = A & B
    solo_dosis = A - B
    solo_panel = B - A

    print("=" * 70)
    print("2. RECONCILIACION DE CONJUNTOS")
    print("=" * 70)
    print(f"  Con dosis (Verra, tras exclusiones): {len(A)}")
    print(f"  Tratados en el panel (D1):           {len(B)}")
    print(f"  En ambos:                            {len(ambos)}")
    print(f"  Solo en la dosis:                    {len(solo_dosis)}")
    print(f"  Solo en el panel (sin dosis):        {len(solo_panel)}")
    print()

    nombres = dosis.set_index("COD_DANE")[["NOMBRE_MPI", "DPTO_CNMBR"]]

    if solo_dosis:
        print("  -- Tienen proyecto REDD+ en Verra pero NO estan en D1 --")
        print("     La regla automatica (metodologia/AFOLU) los marca y el panel los")
        print("     excluyo. Averiguar por que antes de decidir:")
        for c in sorted(solo_dosis):
            n = nombres.loc[c] if c in nombres.index else ("?", "?")
            print(f"       {c}  {n.iloc[0]:25s} {n.iloc[1]}")
        print()

    if solo_panel:
        print("  -- Tratados en D1 pero SIN dosis --")
        print("     Probablemente tratados por una fuente distinta de Verra (Cercarbono,")
        print("     RENARE), que no publica area. No se imputa: quedan fuera del analisis")
        print("     de intensidad pero dentro del analisis binario.")
        for c in sorted(solo_panel):
            n = nombres.loc[c] if c in nombres.index else ("?", "?")
            print(f"       {c}  {n.iloc[0]:25s} {n.iloc[1]}")
        print()

    # -- 3. Coherencia de cohortes -------------------------------------------
    comp = trat_dosis.merge(d1, on="COD_DANE", how="inner")
    comp["dif_cohorte"] = comp["anio_primer_proyecto"] - comp["cohorte_panel"]
    discrepa = comp[comp["dif_cohorte"].fillna(0) != 0]

    print("=" * 70)
    print("3. COHERENCIA DE COHORTES (municipios en ambos)")
    print("=" * 70)
    print(f"  Cohorte coincide: {len(comp) - len(discrepa)} de {len(comp)}")
    if len(discrepa):
        print("  Discrepancias (la cohorte que manda es la del panel):")
        print(discrepa[["COD_DANE", "NOMBRE_MPI", "anio_primer_proyecto",
                        "cohorte_panel", "dif_cohorte"]]
              .sort_values("dif_cohorte").to_string(index=False))
        print("\n  Una cohorte distinta no altera la dosis, pero indica que el panel")
        print("  fecho la adopcion con otra fuente o con otro evento (registro vs.")
        print("  inicio del periodo de acreditacion). Conviene saber cual.")
    print()

    # -- 4. Archivo final: universo del panel + dosis adjunta ----------------
    final = d1.merge(
        trat_dosis[["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR", "n_proyectos",
                    "area_redd_ha", "area_municipio_ha", "bosque_base_ha",
                    "dosis_area_cruda", "dosis_bosque_cruda",
                    "dosis_area", "dosis_bosque",
                    "desborda_area", "desborda_bosque"]],
        on="COD_DANE", how="left",
    )
    # Completa nombres de los que no tienen dosis.
    faltan_nom = final["NOMBRE_MPI"].isna()
    if faltan_nom.any():
        final.loc[faltan_nom, "NOMBRE_MPI"] = final.loc[faltan_nom, "COD_DANE"].map(
            nombres["NOMBRE_MPI"])
        final.loc[faltan_nom, "DPTO_CNMBR"] = final.loc[faltan_nom, "COD_DANE"].map(
            nombres["DPTO_CNMBR"])

    final["tiene_dosis"] = final["dosis_bosque"].notna().astype(int)

    con = final["tiene_dosis"] == 1
    if con.sum() >= args.n_terciles * 3:
        etiquetas = {3: ["bajo", "medio", "alto"]}.get(
            args.n_terciles, [f"g{i+1}" for i in range(args.n_terciles)])
        final.loc[con, "tercil_dosis"] = pd.qcut(
            final.loc[con, "dosis_bosque"].rank(method="first"),
            args.n_terciles, labels=etiquetas,
        ).astype(str)
    else:
        print(f"[!!] Solo {int(con.sum())} municipios con dosis: no alcanza para "
              f"{args.n_terciles} grupos con al menos 3 unidades cada uno.")

    final["cohorte"] = final["cohorte_panel"]

    print("=" * 70)
    print("4. GRUPOS DE INTENSIDAD (sobre dosis_bosque truncada)")
    print("=" * 70)
    if "tercil_dosis" in final.columns:
        resumen = final[con].groupby("tercil_dosis").agg(
            n=("COD_DANE", "size"),
            dosis_min=("dosis_bosque", "min"),
            dosis_max=("dosis_bosque", "max"),
            en_tope=("desborda_bosque", "sum"),
            cohorte_min=("cohorte", "min"),
            cohorte_max=("cohorte", "max"),
        ).reindex(["bajo", "medio", "alto"]).round(3)
        print(resumen.to_string())
        print("\n  'en_tope' = municipios con dosis cruda > 1, truncados en 1. Si se")
        print("  concentran en el tercil alto, el orden sigue siendo valido pero el")
        print("  valor de su dosis no: por eso se usan terciles y no la dosis continua.")

        # Cohortes por tercil: si un tercil queda con cohortes que casi no
        # tienen pre-periodo, CS no puede estimarlo.
        cruz = pd.crosstab(final.loc[con, "cohorte"], final.loc[con, "tercil_dosis"])
        cruz = cruz.reindex(columns=["bajo", "medio", "alto"], fill_value=0)
        print("\n  Municipios por cohorte y tercil:")
        print(cruz.to_string())
    print()

    # -- Escritura -----------------------------------------------------------
    out_final = OUT_FINAL if definicion == "depurada" else \
        OUT_FINAL.with_name(OUT_FINAL.stem + "_original.csv")
    out_recon = OUT_RECON if definicion == "depurada" else \
        OUT_RECON.with_name(OUT_RECON.stem + "_original.csv")
    final["definicion_tratamiento"] = definicion

    (raiz / out_final).parent.mkdir(parents=True, exist_ok=True)
    final.sort_values(["tercil_dosis", "dosis_bosque"], na_position="last").to_csv(
        raiz / out_final, index=False, encoding="utf-8")

    recon = pd.DataFrame(
        [(c, "ambos") for c in sorted(ambos)]
        + [(c, "solo_dosis") for c in sorted(solo_dosis)]
        + [(c, "solo_panel") for c in sorted(solo_panel)]
        + [(c, "excluido_error_asignacion") for c in EXCLUSIONES],
        columns=["COD_DANE", "estado"],
    )
    recon["NOMBRE_MPI"] = recon["COD_DANE"].map(nombres["NOMBRE_MPI"])
    recon["motivo"] = recon["COD_DANE"].map({c: m for c, (_, m) in EXCLUSIONES.items()})
    (raiz / out_recon).parent.mkdir(parents=True, exist_ok=True)
    recon.to_csv(raiz / out_recon, index=False, encoding="utf-8")

    print(f"[ok] {raiz / out_final}  ({len(final)} municipios tratados, "
          f"{int(final['tiene_dosis'].sum())} con dosis)")
    print(f"[ok] {raiz / out_recon}")


if __name__ == "__main__":
    sys.exit(main())
