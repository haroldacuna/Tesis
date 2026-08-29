"""
32_integrar_accesibilidad_conflicto_cede.py

Integra al panel las variables de accesibilidad geográfica y conflicto
armado del Panel Municipal del CEDE, para el emparejamiento (PSM) que
plantea la propuesta ("deforestación histórica, variables climáticas,
accesibilidad geográfica y conflicto armado").

HALLAZGO IMPORTANTE sobre las variables de conflicto: no existe NINGÚN
cero literal en columnas como homicidios o H_coca (el valor mínimo no-nulo
es 1 y 0.07 respectivamente) — son datos a nivel de incidente/evento: si
un municipio-año no tuvo ningún caso, simplemente no genera una fila en la
fuente original, y al fusionar contra un panel balanceado queda como NaN.
Es decir, NaN significa "cero eventos", NO "sin dato" — pero SOLO dentro
del rango de años en que la fuente realmente existe (fuera de ese rango,
NaN sí es genuino "no se midió"). Por eso este script rellena con 0 según
el rango de disponibilidad documentado de CADA variable, no de forma
global.

Variables incorporadas:

  Accesibilidad (de PANEL_CARACTERISTICAS_GENERALES, prácticamente
  invariantes en el tiempo — geografía):
    - discapital: distancia a la capital del departamento
    - disbogota: distancia a Bogotá
    - altura: altura sobre el nivel del mar
    - distancia_mercado: distancia al mercado de alimentos más cercano
      (disponible 1993-2018 solamente, se deja NaN fuera de ese rango)

  Conflicto armado (de PANEL_CONFLICTO_Y_VIOLENCIA):
    - H_coca (hectáreas de coca, 1999-2023) — rellenar NaN con 0
    - coca (dummy presencia de coca, 1999-2020) — rellenar NaN con 0
    - homicidios (2003-2023) — rellenar NaN con 0
    - secuestros (2003-2023) — rellenar NaN con 0
    - o_desplaza (desplazamiento forzado, ocurrencia RUV, 1993-2021) — rellenar NaN con 0
    - acc_subversivas (acciones subversivas, 2003-2021) — rellenar NaN con 0

USO
---
    python 32_integrar_accesibilidad_conflicto_cede.py
"""

from __future__ import annotations

from pathlib import Path

import pandas as pd

PANEL_CANDIDATOS = [
    Path("data/final/panel_con_clima_completo.csv"),
    Path("data/final/dataset_consolidado_completo.csv"),
    Path("data/final/panel_municipio_year.csv"),
]
CEDE_CARACT = Path("data/raw/auxiliary/PANEL_CARACTERISTICAS_GENERALES(2024).dta")
CEDE_CONFLICTO = Path("data/raw/auxiliary/PANEL_CONFLICTO_Y_VIOLENCIA(2024).dta")

OUT_PANEL = Path("data/final/panel_con_psm_covariables.csv")

# Rango de años documentado en el diccionario de datos del CEDE para cada
# variable de conflicto. Fuera de este rango, NaN es genuino "no medido" y
# se deja como NaN; dentro del rango, NaN se interpreta como 0 eventos.
RANGOS_CONFLICTO = {
    "H_coca": (1999, 2023),
    "coca": (1999, 2020),
    "homicidios": (2003, 2023),
    "secuestros": (2003, 2023),
    "o_desplaza": (1993, 2021),
    "acc_subversivas": (2003, 2021),
}

COLS_ACCESIBILIDAD = ["discapital", "disbogota", "altura", "distancia_mercado"]


def _cod_dane(serie: pd.Series) -> pd.Series:
    return serie.dropna().astype(int).astype(str).str.zfill(5).reindex(serie.index)


def _elegir_panel() -> Path | None:
    return next((p for p in PANEL_CANDIDATOS if p.exists()), None)


def cargar_accesibilidad() -> pd.DataFrame:
    df = pd.read_stata(CEDE_CARACT, convert_categoricals=False).copy()
    df["COD_DANE"] = _cod_dane(df["codmpio"])
    df["year"] = df["ano"].astype(int)
    cols = ["COD_DANE", "year"] + [c for c in COLS_ACCESIBILIDAD if c in df.columns]
    out = df[cols].drop_duplicates(subset=["COD_DANE", "year"])
    print(f"Accesibilidad (CEDE características generales): {len(out)} filas, {out['COD_DANE'].nunique()} municipios")
    for c in COLS_ACCESIBILIDAD:
        if c in out.columns:
            print(f"  {c}: {out[c].notna().mean():.1%} de cobertura")
    return out


def cargar_conflicto() -> pd.DataFrame:
    df = pd.read_stata(CEDE_CONFLICTO, convert_categoricals=False).copy()
    df["COD_DANE"] = _cod_dane(df["codmpio"])
    df["year"] = df["ano"].astype(int)

    cols_disponibles = [c for c in RANGOS_CONFLICTO if c in df.columns]
    faltantes = [c for c in RANGOS_CONFLICTO if c not in df.columns]
    if faltantes:
        print(f"Aviso: no encontré estas columnas en el archivo de conflicto: {faltantes}")

    out = df[["COD_DANE", "year"] + cols_disponibles].drop_duplicates(subset=["COD_DANE", "year"])

    print(f"\nConflicto (CEDE panel conflicto y violencia): {len(out)} filas, {out['COD_DANE'].nunique()} municipios")
    for col in cols_disponibles:
        anio_ini, anio_fin = RANGOS_CONFLICTO[col]
        en_rango = (out["year"] >= anio_ini) & (out["year"] <= anio_fin)
        antes = out.loc[en_rango, col].notna().mean()
        # Rellenar NaN -> 0 SOLO dentro del rango documentado de la variable.
        out.loc[en_rango, col] = out.loc[en_rango, col].fillna(0)
        despues = out.loc[en_rango, col].notna().mean()
        print(f"  {col} ({anio_ini}-{anio_fin}): cobertura {antes:.1%} -> {despues:.1%} tras interpretar NaN como 0")

    return out


def main() -> None:
    panel_path = _elegir_panel()
    if panel_path is None:
        print(f"ERROR: no encuentro ninguno de {PANEL_CANDIDATOS}")
        return
    if not CEDE_CARACT.exists():
        print(f"ERROR: no encuentro {CEDE_CARACT}")
        return
    if not CEDE_CONFLICTO.exists():
        print(f"ERROR: no encuentro {CEDE_CONFLICTO}")
        return

    print(f"Usando panel base: {panel_path}\n")
    panel = pd.read_csv(panel_path, low_memory=False, dtype={"COD_DANE": str})
    panel["COD_DANE"] = panel["COD_DANE"].str.zfill(5)
    filas_antes = len(panel)

    accesibilidad = cargar_accesibilidad()
    conflicto = cargar_conflicto()

    panel = panel.merge(accesibilidad, on=["COD_DANE", "year"], how="left")
    panel = panel.merge(conflicto, on=["COD_DANE", "year"], how="left")

    if len(panel) != filas_antes:
        print(f"\n*** AVISO: el panel cambió de {filas_antes} a {len(panel)} filas tras el merge — "
              f"revisar si hay codmpio/year duplicados en las fuentes CEDE. ***")
    else:
        print(f"\nEl panel mantiene {len(panel)} filas (el merge no duplicó nada).")

    panel.to_csv(OUT_PANEL, index=False, encoding="utf-8-sig")
    print(f"\nGuardado: {OUT_PANEL}")
    print(f"(NO se sobreescribió {panel_path} — compara antes de reemplazarlo)")

    print("\n=== Cobertura final en el panel completo ===")
    for c in COLS_ACCESIBILIDAD + list(RANGOS_CONFLICTO.keys()):
        if c in panel.columns:
            print(f"  {c}: {panel[c].notna().mean():.1%}")


if __name__ == "__main__":
    main()
