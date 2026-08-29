"""
31_integrar_clima_gee_al_panel.py

Toma el resultado de 30_recalcular_clima_gee_directo.py (clima calculado
por geometría real para los 126 municipios que no cruzaban por nombre) y
lo integra al panel final — rellenando SOLO las celdas que hoy están en
NaN. No toca temp_media_c/prec_anual_mm de los ~996 municipios que ya
tenían clima correcto por el cruce de nombre original, ni ninguna otra
columna del panel (deforestación, población, proyectos de carbono, etc.).

USO
---
    python 31_integrar_clima_gee_al_panel.py
"""

from __future__ import annotations

from pathlib import Path

import pandas as pd

PANEL_CANDIDATOS = [
    Path("data/final/dataset_consolidado_completo.csv"),
    Path("data/final/panel_municipio_year.csv"),
]
CLIMA_GEE_FILE = Path("data/interim/clima_municipios_faltantes_gee.csv")
OUT_PANEL = Path("data/final/panel_con_clima_completo.csv")


def main() -> None:
    panel_path = next((p for p in PANEL_CANDIDATOS if p.exists()), None)
    if panel_path is None:
        print("ERROR: no encuentro el panel.")
        return
    if not CLIMA_GEE_FILE.exists():
        print(f"ERROR: no encuentro {CLIMA_GEE_FILE}. Corre primero 30_recalcular_clima_gee_directo.py.")
        return

    panel = pd.read_csv(panel_path, low_memory=False, dtype={"COD_DANE": str})
    clima_gee = pd.read_csv(CLIMA_GEE_FILE, dtype={"COD_DANE": str})

    antes_temp = panel["temp_media_c"].isna().sum()
    antes_prec = panel["prec_anual_mm"].isna().sum()
    print(f"Antes de integrar: {antes_temp} NaN en temp_media_c, {antes_prec} en prec_anual_mm")

    clima_gee_idx = clima_gee.set_index(["COD_DANE", "year"])[["temp_media_c", "prec_anual_mm"]]

    panel = panel.set_index(["COD_DANE", "year"])
    # combine_first: usa el valor existente si no es NaN; si es NaN, usa el
    # de clima_gee. Nunca sobrescribe un valor que ya estaba bien.
    for col in ["temp_media_c", "prec_anual_mm"]:
        panel[col] = panel[col].combine_first(clima_gee_idx[col])
    panel = panel.reset_index()

    despues_temp = panel["temp_media_c"].isna().sum()
    despues_prec = panel["prec_anual_mm"].isna().sum()
    print(f"Después de integrar: {despues_temp} NaN en temp_media_c, {despues_prec} en prec_anual_mm")
    print(f"Rellenados: {antes_temp - despues_temp} filas de temperatura, {antes_prec - despues_prec} de precipitación")

    if despues_temp > 0:
        pendientes = panel[panel["temp_media_c"].isna()][["COD_DANE", "NOMBRE_MPI"]].drop_duplicates()
        print(f"\nAún quedan {len(pendientes)} municipios sin clima (probablemente años fuera del rango calculado, o municipios que no procesó el script 30):")
        print(pendientes.to_string())

    panel.to_csv(OUT_PANEL, index=False, encoding="utf-8-sig")
    print(f"\nGuardado: {OUT_PANEL}")
    print(f"(NO se sobreescribió {panel_path} — compara antes de reemplazarlo)")


if __name__ == "__main__":
    main()
