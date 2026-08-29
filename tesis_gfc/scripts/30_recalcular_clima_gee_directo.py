"""
30_recalcular_clima_gee_directo.py

En vez de seguir intentando arreglar el cruce por nombre (que tiene
colisiones peligrosas: "Ciudad Bolívar" vs "Bolívar", "Santiago de Tolú"
vs "Santiago", etc.), este script usa la función que YA EXISTE en tu
pipeline (obtener_clima_gee) para calcular clima directamente desde la
geometría real de cada uno de los 126 municipios problemáticos —sin
ningún nombre de por medio—, usando exactamente el mismo método (CHIRPS
para precipitación, ERA5 para temperatura) que se usó para los otros 996.

IMPORTANTE — TIEMPO DE EJECUCIÓN: esto hace hasta 126 × 24 años = 3,024
llamadas a Earth Engine, cada una síncrona (getInfo()). Puede tardar horas,
no minutos. El script guarda progreso incremental cada 20 llamadas, así
que si se corta a mitad de camino, puedes retomar donde quedó sin perder
lo ya calculado (no vuelve a pedir lo que ya está en el checkpoint).

REQUISITOS: paquete 'earthengine-api' instalado y autenticado
(`earthengine authenticate` en la terminal, una sola vez, si no lo has
hecho ya) — es la misma cuenta de Earth Engine que usa el resto del
pipeline, así que si obtener_clima_gee ya te ha funcionado antes, esto
debería funcionar igual.

USO
---
    python 30_recalcular_clima_gee_directo.py
    python 30_recalcular_clima_gee_directo.py --anio-desde 2015 --anio-hasta 2024   # subset más rápido para probar primero
"""

from __future__ import annotations

import argparse
import importlib.util
import sys
import time
from pathlib import Path

import geopandas as gpd
import pandas as pd

ENRICH_SCRIPT = Path(__file__).resolve().parent / "06_enrich_panel_external.py"
if not ENRICH_SCRIPT.exists():
    print(f"ERROR: no encuentro {ENRICH_SCRIPT}.")
    sys.exit(1)

_spec = importlib.util.spec_from_file_location("enrich", ENRICH_SCRIPT)
enrich = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(enrich)  # type: ignore[union-attr]

PANEL_CANDIDATOS = [
    Path("data/final/dataset_consolidado_completo.csv"),
    Path("data/final/panel_municipio_year.csv"),
]
MUNICIPIOS_GPKG = Path("data/interim/municipios_clean.gpkg")
MUNICIPIOS_LAYER = "municipios_clean"

DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)
OUT_CHECKPOINT = DIAG_DIR / "clima_gee_directo_checkpoint.csv"
OUT_FINAL = Path("data/interim/clima_municipios_faltantes_gee.csv")


def _identificar_municipios_sin_clima(panel: pd.DataFrame) -> pd.DataFrame:
    stats = panel.groupby(["COD_DANE", "NOMBRE_MPI"])["temp_media_c"].apply(lambda s: s.isna().mean()).reset_index(
        name="pct_faltante"
    )
    return stats[stats["pct_faltante"] >= 0.99][["COD_DANE", "NOMBRE_MPI"]]


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--anio-desde", type=int, default=2001)
    parser.add_argument("--anio-hasta", type=int, default=2024)
    parser.add_argument("--ee-project", type=str, default=None, help="ID de proyecto de Earth Engine, si tu cuenta lo requiere.")
    args = parser.parse_args()

    panel_path = next((p for p in PANEL_CANDIDATOS if p.exists()), None)
    if panel_path is None:
        print("ERROR: no encuentro el panel.")
        return
    if not MUNICIPIOS_GPKG.exists():
        print(f"ERROR: no encuentro {MUNICIPIOS_GPKG}.")
        return

    panel = pd.read_csv(panel_path, low_memory=False, dtype={"COD_DANE": str})
    if "temp_media_c" not in panel.columns:
        print("ERROR: el panel no tiene columna 'temp_media_c'.")
        return

    faltantes = _identificar_municipios_sin_clima(panel)
    print(f"Municipios sin clima a recalcular: {len(faltantes)}")

    municipios = gpd.read_file(MUNICIPIOS_GPKG, layer=MUNICIPIOS_LAYER)
    municipios["COD_DANE"] = municipios["COD_DANE"].astype(str).str.zfill(5)
    if municipios.crs is None:
        municipios = municipios.set_crs("EPSG:4326")
    elif municipios.crs.to_epsg() != 4326:
        municipios = municipios.to_crs("EPSG:4326")

    faltantes = faltantes.merge(municipios[["COD_DANE", "geometry"]], on="COD_DANE", how="left")
    sin_geometria = faltantes["geometry"].isna().sum()
    if sin_geometria > 0:
        print(f"Aviso: {sin_geometria} municipios sin geometría en el shapefile (no se pueden procesar, quedan fuera).")
        faltantes = faltantes.dropna(subset=["geometry"])

    print("\nInicializando Google Earth Engine...")
    gee_ok = enrich._init_earth_engine(True, args.ee_project)
    if not gee_ok:
        print("\nNo se pudo inicializar Earth Engine. Corre 'earthengine authenticate' en la terminal y vuelve a intentar.")
        return

    # Retomar desde checkpoint si existe (para no perder progreso si se cortó antes).
    resultados: list[dict] = []
    ya_calculados: set[tuple[str, int]] = set()
    if OUT_CHECKPOINT.exists():
        prev = pd.read_csv(OUT_CHECKPOINT, dtype={"COD_DANE": str})
        resultados = prev.to_dict("records")
        ya_calculados = {(r["COD_DANE"], int(r["year"])) for r in resultados}
        print(f"Retomando desde checkpoint: {len(ya_calculados)} combinaciones municipio-año ya calculadas.")

    anios = list(range(args.anio_desde, args.anio_hasta + 1))
    total_llamadas = len(faltantes) * len(anios)
    print(f"\nTotal a calcular: {len(faltantes)} municipios × {len(anios)} años = {total_llamadas} llamadas")
    print("Esto puede tardar bastante — se guarda progreso cada 20 llamadas.\n")

    contador = 0
    t_inicio = time.time()

    for _, row in faltantes.iterrows():
        try:
            geom_ee = enrich.ee.Geometry(row["geometry"].__geo_interface__)
        except Exception as exc:
            print(f"  {row['NOMBRE_MPI']}: no se pudo convertir la geometría a Earth Engine ({exc}), se omite.")
            continue

        for anio in anios:
            if (row["COD_DANE"], anio) in ya_calculados:
                continue

            clima = enrich.obtener_clima_gee(geom_ee, anio)
            resultados.append({
                "COD_DANE": row["COD_DANE"],
                "NOMBRE_MPI": row["NOMBRE_MPI"],
                "year": anio,
                "temp_media_c": clima["temp_media_c"],
                "prec_anual_mm": clima["prec_anual_mm"],
            })
            contador += 1

            if contador % 20 == 0:
                pd.DataFrame(resultados).to_csv(OUT_CHECKPOINT, index=False, encoding="utf-8-sig")
                transcurrido = time.time() - t_inicio
                ritmo = contador / transcurrido if transcurrido > 0 else 0
                restantes = total_llamadas - len(ya_calculados) - contador
                eta_min = (restantes / ritmo / 60) if ritmo > 0 else float("nan")
                print(f"  {contador} calculadas en esta corrida ({len(resultados)} en total) — ritmo: {ritmo:.2f}/s, ETA: {eta_min:.0f} min")

    pd.DataFrame(resultados).to_csv(OUT_CHECKPOINT, index=False, encoding="utf-8-sig")

    final_df = pd.DataFrame(resultados)
    final_df.to_csv(OUT_FINAL, index=False, encoding="utf-8-sig")
    print(f"\nGuardado final: {OUT_FINAL}")

    con_datos = final_df["temp_media_c"].notna().sum()
    print(f"Combinaciones municipio-año con clima calculado exitosamente: {con_datos}/{len(final_df)}")
    print(
        "\nEste archivo NO está fusionado al panel todavía — es intencional, para que "
        "puedas revisarlo primero. Cuando confirmes que se ve bien, te preparo el script "
        "que lo integra al panel final por COD_DANE + year (reemplazando solo los NaN "
        "existentes, sin tocar los 996 municipios que ya tenían clima)."
    )


if __name__ == "__main__":
    main()
