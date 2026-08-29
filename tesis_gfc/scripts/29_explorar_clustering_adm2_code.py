"""
29_explorar_clustering_adm2_code.py

ADM2_CODE no corresponde a COD_DANE, pero los valores vistos hasta ahora
(todos en el rango 13341-13408 para municipios de Antioquia) sugieren que
podría estar agrupado por departamento internamente en GADM, aunque el
número en sí sea arbitrario. Si eso se confirma, podemos usarlo como señal
adicional para descartar candidatos de departamento equivocado en los 126
municipios problemáticos — sin necesitar una columna de departamento
explícita en el CSV de clima.

Este script construye, a partir de los ~996 municipios que YA cruzan bien
por nombre, un mapa de rango de ADM2_CODE por departamento (usando
DPTO_CNMBR del panel, que sí conocemos con certeza), y reporta si los
rangos están limpiamente separados o se mezclan entre departamentos.

USO
---
    python 29_explorar_clustering_adm2_code.py
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

    # Un registro por municipio (no por municipio-año) con su ADM2_CODE,
    # para los que sí cruzan bien hoy.
    panel_mun = panel[["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR", "mun_limpio"]].drop_duplicates()
    clima_mun = clima[["mun_limpio", "ADM2_CODE"]].drop_duplicates(subset=["mun_limpio"])

    cruzados = panel_mun.merge(clima_mun, on="mun_limpio", how="inner")
    cruzados["ADM2_CODE"] = pd.to_numeric(cruzados["ADM2_CODE"], errors="coerce")
    cruzados = cruzados.dropna(subset=["ADM2_CODE"])
    print(f"Municipios cruzados hoy con ADM2_CODE numérico válido: {len(cruzados)}")

    rangos = cruzados.groupby("DPTO_CNMBR")["ADM2_CODE"].agg(["min", "max", "count"]).sort_values("min")
    print(f"\nRangos de ADM2_CODE por departamento ({len(rangos)} departamentos):")
    print(rangos.to_string())

    # Verificar si los rangos se solapan entre departamentos (si no se
    # solapan, el clustering es limpio y sirve como filtro de
    # desambiguación).
    print("\nVerificando solapamientos entre departamentos consecutivos (ordenados por mínimo)...")
    solapamientos = 0
    filas = rangos.reset_index().to_dict("records")
    for i in range(len(filas) - 1):
        actual, siguiente = filas[i], filas[i + 1]
        if actual["max"] >= siguiente["min"]:
            solapamientos += 1
            print(f"  SOLAPAN: {actual['DPTO_CNMBR']} (hasta {actual['max']:.0f}) y {siguiente['DPTO_CNMBR']} (desde {siguiente['min']:.0f})")

    if solapamientos == 0:
        print(
            "\n*** Los rangos NO se solapan entre departamentos — el clustering es limpio. "
            "Podemos usar el rango de ADM2_CODE del departamento correcto para filtrar "
            "candidatos y eliminar automáticamente las colisiones de nombre peligrosas "
            "(ej. descartar 'bolivar' como candidato para Ciudad Bolívar si su ADM2_CODE "
            "cae en el rango de Bolívar-departamento en vez de Antioquia). ***"
        )
    else:
        print(
            f"\n*** {solapamientos} pares de departamentos con rangos solapados. El clustering "
            "no es perfectamente limpio, pero puede seguir siendo útil como señal parcial "
            "(reduce candidatos, no los elimina con 100% de certeza). ***"
        )


if __name__ == "__main__":
    main()
