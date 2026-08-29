"""
28_verificar_adm2_code.py

clima_historico_gee.csv tiene una columna ADM2_CODE que no se está usando
en el cruce actual (solo se usa ADM2_NAME por texto). Si ADM2_CODE
corresponde al código DANE (o es fácilmente convertible), podríamos cruzar
por código en vez de por nombre normalizado — sin ambigüedad, sin
colisiones de nombre, resolviendo los 126 municipios de una sola vez.

Este script compara ADM2_CODE contra COD_DANE para los municipios que HOY
SÍ cruzan bien por nombre (ej. Medellín, Bogotá si su nombre limpio ya
coincidiera, Cali, etc.) — si los códigos coinciden o son claramente
convertibles (mismo número, distinto padding de ceros, etc.), tenemos
camino libre para un cruce mucho mejor.

USO
---
    python 28_verificar_adm2_code.py
"""

from __future__ import annotations

import unicodedata
from pathlib import Path
from typing import Any

import pandas as pd

PANEL_CANDIDATOS = [
    Path("data/final/dataset_consolidado_completo.csv"),
    Path("data/final/panel_municipio_year.csv"),
]
CLIMATE_CSV = Path("data/raw/auxiliary/clima_historico_gee.csv")


def limpiar_texto(texto: Any) -> str:
    if pd.isna(texto):
        return ""
    texto = str(texto).strip().lower()
    return "".join(c for c in unicodedata.normalize("NFD", texto) if unicodedata.category(c) != "Mn")


def main() -> None:
    panel_path = next((p for p in PANEL_CANDIDATOS if p.exists()), None)
    if panel_path is None:
        print("ERROR: no encuentro el panel.")
        return

    panel = pd.read_csv(panel_path, low_memory=False, dtype={"COD_DANE": str})
    clima = pd.read_csv(CLIMATE_CSV, low_memory=False)

    panel["mun_limpio"] = panel["NOMBRE_MPI"].apply(limpiar_texto)
    clima["mun_limpio"] = clima["ADM2_NAME"].apply(limpiar_texto)

    # Municipios de ejemplo que hoy SÍ cruzan bien por nombre (para comparar
    # su COD_DANE real contra el ADM2_CODE que les tocó en el cruce actual).
    ejemplos = panel[panel["temp_media_c"].notna()][["COD_DANE", "NOMBRE_MPI", "mun_limpio"]].drop_duplicates().head(15)

    print("Comparando COD_DANE (panel) vs ADM2_CODE (clima) para municipios que ya cruzan bien por nombre:\n")
    print(f"{'NOMBRE_MPI':<30} {'COD_DANE':<12} {'ADM2_CODE':<15} {'ADM2_NAME (clima)'}")
    print("-" * 90)

    coinciden_exacto = 0
    total_comparados = 0

    for _, row in ejemplos.iterrows():
        match_clima = clima[clima["mun_limpio"] == row["mun_limpio"]]
        if match_clima.empty:
            continue
        adm2_code = str(match_clima.iloc[0]["ADM2_CODE"])
        adm2_name = match_clima.iloc[0]["ADM2_NAME"]
        cod_dane = row["COD_DANE"]

        total_comparados += 1
        marca = ""
        if adm2_code == cod_dane:
            marca = "<-- IDÉNTICO"
            coinciden_exacto += 1
        elif adm2_code.zfill(5) == cod_dane.zfill(5):
            marca = "<-- IGUAL (diferente padding de ceros)"
            coinciden_exacto += 1
        elif adm2_code.lstrip("0") == cod_dane.lstrip("0"):
            marca = "<-- IGUAL (sin ceros a la izquierda)"
            coinciden_exacto += 1

        print(f"{row['NOMBRE_MPI']:<30} {cod_dane:<12} {adm2_code:<15} {adm2_name}  {marca}")

    print(f"\n{coinciden_exacto}/{total_comparados} coinciden (exacto o por padding de ceros).")

    if coinciden_exacto == total_comparados and total_comparados > 0:
        print(
            "\n*** ADM2_CODE SÍ corresponde a COD_DANE. Podemos re-hacer el cruce completo "
            "por código en vez de por nombre — esto debería resolver los 126 municipios "
            "de una sola vez, sin ambigüedad. Avísame y preparo el script. ***"
        )
    elif coinciden_exacto > 0:
        print(
            "\n*** Coincide PARCIALMENTE. Puede que el formato no sea 100% consistente, o que "
            "haya algún desfase. Pégame esta tabla completa para revisar el patrón exacto. ***"
        )
    else:
        print(
            "\n*** ADM2_CODE NO corresponde a COD_DANE (son sistemas de codificación distintos, "
            "probablemente el ID interno de GADM). No sirve como atajo — seguimos con el "
            "crosswalk por nombre revisado a mano. ***"
        )


if __name__ == "__main__":
    main()
