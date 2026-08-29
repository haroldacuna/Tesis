"""
27_diagnosticar_clima_faltante.py

126 de 1122 municipios (11.2%, constante en los 24 años — señal de que es
un problema de cruce, no de cobertura satelital) no tienen temp_media_c /
prec_anual_mm en el panel. El cruce en 06_enrich_panel_external.py
(consolidar_dataset_local) se hace por nombre normalizado (limpiar_texto:
solo minúsculas + sin tildes) entre NOMBRE_MPI del panel (nomenclatura
DANE) y ADM2_NAME del CSV de clima (probablemente GADM, vía GEE) — y
limpiar_texto NO quita sufijos/calificadores como ", D.C." o "DE INDIAS",
que sí aparecen en los nombres oficiales DANE pero no necesariamente en
GADM. Por eso BOGOTÁ, D.C. o CARTAGENA DE INDIAS —ciudades grandes, no
casos raros— quedan sin clima.

Este script:
  1. Carga el CSV de clima local y el panel.
  2. Para cada uno de los municipios sin clima, busca por coincidencia
     difusa (no exacta) el nombre más parecido dentro de ADM2_NAME.
  3. Exporta las sugerencias para que las verifiques a mano antes de
     aplicarlas — no reemplaza nada automáticamente.

USO
---
    python 27_diagnosticar_clima_faltante.py

Salida: data/interim/diagnostics/clima_crosswalk_sugerido.csv
"""

from __future__ import annotations

import difflib
import unicodedata
from pathlib import Path
from typing import Any

import pandas as pd

PANEL_CANDIDATOS = [
    Path("data/final/dataset_consolidado_completo.csv"),
    Path("data/final/panel_municipio_year.csv"),
]
CLIMATE_CSV = Path("data/raw/auxiliary/clima_historico_gee.csv")
OUT_CROSSWALK = Path("data/interim/diagnostics/clima_crosswalk_sugerido.csv")
OUT_CROSSWALK.parent.mkdir(parents=True, exist_ok=True)


def limpiar_texto(texto: Any) -> str:
    """Misma función que 06_enrich_panel_external.py, reproducida aquí para
    no depender de importar ese módulo completo (que requiere geopandas)."""
    if pd.isna(texto):
        return ""
    texto = str(texto).strip().lower()
    return "".join(c for c in unicodedata.normalize("NFD", texto) if unicodedata.category(c) != "Mn")


def _elegir_panel() -> Path | None:
    for p in PANEL_CANDIDATOS:
        if p.exists():
            return p
    return None


def main() -> None:
    panel_path = _elegir_panel()
    if panel_path is None:
        print(f"ERROR: no encuentro ninguno de {PANEL_CANDIDATOS}")
        return
    if not CLIMATE_CSV.exists():
        print(f"ERROR: no encuentro {CLIMATE_CSV}")
        return

    print(f"Usando panel: {panel_path}")
    panel = pd.read_csv(panel_path, low_memory=False, dtype={"COD_DANE": str})
    clima = pd.read_csv(CLIMATE_CSV, low_memory=False)

    if "ADM2_NAME" not in clima.columns:
        print(f"ERROR: {CLIMATE_CSV} no tiene columna 'ADM2_NAME'. Columnas disponibles: {list(clima.columns)}")
        return

    clima["mun_limpio"] = clima["ADM2_NAME"].apply(limpiar_texto)
    nombres_clima_unicos = sorted(clima["mun_limpio"].dropna().unique())
    nombres_clima_unicos = [n for n in nombres_clima_unicos if n]
    print(f"Nombres únicos disponibles en el CSV de clima: {len(nombres_clima_unicos)}")

    panel["mun_limpio"] = panel["NOMBRE_MPI"].apply(limpiar_texto)

    # Municipios sin clima en NINGÚN año (el problema estructural, no un
    # hueco temporal puntual).
    if "temp_media_c" not in panel.columns:
        print("ERROR: el panel no tiene columna 'temp_media_c'.")
        return

    mun_stats = panel.groupby(["COD_DANE", "NOMBRE_MPI", "mun_limpio"])["temp_media_c"].apply(
        lambda s: s.isna().mean()
    ).reset_index(name="pct_faltante")
    sin_clima = mun_stats[mun_stats["pct_faltante"] >= 0.99].copy()  # ~100% faltante en todos los años
    print(f"\nMunicipios con clima faltante en (casi) todos los años: {len(sin_clima)}")

    sugerencias = []
    for _, row in sin_clima.iterrows():
        nombre_panel = row["mun_limpio"]

        # Coincidencias por CONTENCIÓN (una cadena literalmente dentro de la
        # otra) son mucho más confiables que las de difflib por similitud de
        # caracteres — "buga" está contenido en "guadalajara de buga" (real),
        # mientras que "yondo" se parece a "arroyohondo" solo por coincidencia
        # de letras sueltas (Yondo es un municipio real y distinto, en otro
        # departamento). Se reportan por separado a propósito.
        candidatos_substring = [
            n for n in nombres_clima_unicos
            if n and len(n) >= 4 and (n in nombre_panel or nombre_panel in n)
        ]
        candidatos_difflib = [
            n for n in difflib.get_close_matches(nombre_panel, nombres_clima_unicos, n=5, cutoff=0.6)
            if n not in candidatos_substring
        ]

        if candidatos_substring:
            confianza = "alta (coincidencia de subcadena)"
        elif candidatos_difflib:
            confianza = "baja (solo similitud de letras, verificar con cuidado)"
        else:
            confianza = "sin sugerencia"

        sugerencias.append({
            "COD_DANE": row["COD_DANE"],
            "NOMBRE_MPI": row["NOMBRE_MPI"],
            "mun_limpio_panel": nombre_panel,
            "confianza_sugerencia": confianza,
            "candidatos_subcadena": "; ".join(candidatos_substring[:5]),
            "candidatos_difflib_solamente": "; ".join(candidatos_difflib[:5]),
        })

    sug_df = pd.DataFrame(sugerencias)
    sug_df.to_csv(OUT_CROSSWALK, index=False, encoding="utf-8-sig")

    print(f"\nDesglose por nivel de confianza:")
    print(sug_df["confianza_sugerencia"].value_counts().to_string())
    print(f"\nGuardado en: {OUT_CROSSWALK}")
    print("\n=== Casos de ALTA confianza (subcadena) — revisar rápido y aplicar ===")
    alta = sug_df[sug_df["confianza_sugerencia"].str.startswith("alta")]
    print(alta[["NOMBRE_MPI", "candidatos_subcadena"]].to_string())
    print("\n=== Casos de BAJA confianza (revisar con cuidado, riesgo de asignar mal) ===")
    baja = sug_df[sug_df["confianza_sugerencia"].str.startswith("baja")]
    print(baja[["NOMBRE_MPI", "candidatos_difflib_solamente"]].to_string())
    print(
        "\n*** Los de 'alta confianza' probablemente son correctos, pero igual revísalos "
        "(ej. verifica que el candidato no sea un municipio real distinto en otro "
        "departamento). Los de 'baja confianza' NO los apliques sin buscar el nombre real "
        "del municipio manualmente — aplicar mal el cruce es peor que dejarlo en blanco, "
        "porque le asignarías el clima de un lugar equivocado sin que se note. ***"
    )


if __name__ == "__main__":
    main()
