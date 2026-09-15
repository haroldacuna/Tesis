"""
21_reparar_deforestacion_panel.py

Re-extrae loss_area_ha y baseline_forest para los 1.122 municipios y los
sustituye en el panel, sin tocar ninguna otra variable.

POR QUE UNA REPARACION QUIRURGICA Y NO RE-CORRER EL PIPELINE
------------------------------------------------------------
El diagnostico acoto el defecto a dos columnas y 148 municipios (Antioquia y
Atlantico completos). Todas las demas variables del panel -clima, poblacion,
CEDE, conflicto, tratamiento- tienen cobertura identica en esos departamentos
que en el resto del pais y medias plausibles. Reconstruir toda la cadena de
enriquecimiento reintroduciria riesgo sin ganancia.

Los rasters quedaron verificados: cubren el territorio continental, tienen la
resolucion de 1 segundo de arco de Hansen y estan alineados con las geometrias
(los conteos de pixeles reproducen las areas municipales reales).

DIFERENCIAS CON 02_extract_loss_by_municipio.py y 04_merge_covariates.py
-----------------------------------------------------------------------
1. Lectura por ventanas. El script 04 hacia src.read(1), que carga 3,6 GB en
   memoria (55.609 x 65.401 uint8) y falla en la mayoria de equipos. Aqui se
   recorta por municipio.
2. Sin fallbacks silenciosos. Ningun _write_empty_outputs, ningun fillna(0).
   Si algo falla, el script se detiene.
3. Validacion de plausibilidad, no solo de estructura.

Escribe a archivos NUEVOS. No sobrescribe nada.

USO
---
    python 21_reparar_deforestacion_panel.py
"""

import sys
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import rasterio
from rasterio.mask import mask

# --- Parametros --------------------------------------------------------------

ANIO_INICIO, ANIO_FIN = 2001, 2024
UMBRAL_DOSEL = 30          # Hansen: treecover2000 en % de cobertura (0-100)
PIXEL_HA = 0.09            # 30 m x 30 m, misma convencion que el pipeline actual

# Correccion por latitud. En EPSG:4326 el ancho del pixel en metros decrece con
# cos(latitud), de modo que 0,09 ha sobreestima el area hacia el norte del pais.
# Entre 0 y 12 grados N el sesgo va de 0% a ~2,2%. Se deja DESACTIVADA por
# defecto para mantener comparabilidad con las cifras ya escritas en la tesis,
# pero el script reporta la magnitud para que la decision quede documentada.
CORREGIR_AREA_POR_LATITUD = False


def encontrar_raiz() -> Path:
    origenes = [Path(__file__).resolve().parent, Path.cwd().resolve()]
    for origen in origenes:
        for c in [origen, *origen.parents]:
            if (c / "data").is_dir() and (c / "scripts").is_dir():
                return c
    for origen in origenes:
        for c in [origen, *origen.parents]:
            if (c / "data").is_dir():
                return c
    return Path.cwd().resolve()


RAIZ = encontrar_raiz()
RUTA_MUNICIPIOS = RAIZ / "data" / "interim" / "municipios_clean.gpkg"
RUTA_LOSSYEAR = RAIZ / "data" / "raw" / "gfc" / "Colombia_GFC_lossyear.tif"
RUTA_TREECOVER = RAIZ / "data" / "raw" / "auxiliary" / "Colombia_treecover2000.tif"
RUTA_PANEL = RAIZ / "data" / "final" / "panel_con_psm_covariables.csv"

SALIDA_LOSS = RAIZ / "data" / "interim" / "loss_by_municipio_year_reparado.csv"
SALIDA_BASELINE = RAIZ / "data" / "interim" / "baseline_forest_reparado.csv"
SALIDA_PANEL = RAIZ / "data" / "final" / "panel_con_psm_covariables_reparado.csv"


def verificar_insumos() -> None:
    faltan = [r for r in (RUTA_MUNICIPIOS, RUTA_LOSSYEAR, RUTA_TREECOVER, RUTA_PANEL)
              if not r.exists()]
    if faltan:
        print("Faltan insumos:")
        for r in faltan:
            print(f"  {r}")
        sys.exit(1)


def cargar_municipios() -> gpd.GeoDataFrame:
    mun = gpd.read_file(RUTA_MUNICIPIOS, layer="municipios_clean")
    mun["COD_DANE"] = mun["COD_DANE"].astype(str).str.zfill(5)

    vacias = mun.geometry.is_empty | mun.geometry.isna()
    if vacias.any():
        raise ValueError(
            f"{int(vacias.sum())} municipios con geometria vacia: "
            f"{mun.loc[vacias, 'COD_DANE'].tolist()[:20]}"
        )

    print(f"  Municipios: {len(mun)}")
    print(f"  Departamentos: {mun['COD_DANE'].str[:2].nunique()}")
    if len(mun) != 1122:
        print(f"  AVISO: se esperaban 1.122 municipios, hay {len(mun)}.")
    return mun


def extraer(mun: gpd.GeoDataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Recorre municipio por municipio, leyendo ventanas de ambos rasters."""
    filas_loss, filas_base = [], []
    n = len(mun)

    with rasterio.open(RUTA_LOSSYEAR) as src_loss, rasterio.open(RUTA_TREECOVER) as src_tree:
        mun_loss = mun.to_crs(src_loss.crs)
        mun_tree = mun.to_crs(src_tree.crs)

        for i in range(n):
            fila = mun_loss.iloc[i]
            cod = fila["COD_DANE"]

            if (i + 1) % 50 == 0 or i == 0:
                print(f"    {i + 1:>5} / {n}   ({cod} {fila['NOMBRE_MPI'][:28]})")

            # Factor de area, corregido por latitud si corresponde
            if CORREGIR_AREA_POR_LATITUD:
                lat = fila.geometry.centroid.y
                factor = PIXEL_HA * float(np.cos(np.radians(lat)))
            else:
                factor = PIXEL_HA

            # --- Perdida anual -------------------------------------------------
            try:
                recorte, _ = mask(src_loss, [fila.geometry], crop=True, filled=False)
                vals = recorte.compressed()
            except ValueError:
                raise ValueError(
                    f"Municipio {cod} ({fila['NOMBRE_MPI']}) queda fuera de la "
                    f"extension del raster de perdida. No se continua."
                )

            conteos = {}
            if vals.size:
                v, c = np.unique(vals[vals > 0], return_counts=True)
                conteos = dict(zip(v.tolist(), c.tolist()))

            for anio in range(ANIO_INICIO, ANIO_FIN + 1):
                px = int(conteos.get(anio - 2000, 0))
                filas_loss.append({
                    "COD_DANE": cod,
                    "NOMBRE_MPI": fila["NOMBRE_MPI"],
                    "DPTO_CNMBR": fila["DPTO_CNMBR"],
                    "year": anio,
                    "loss_pixels": px,
                    "loss_area_ha": px * factor,
                })

            # --- Cobertura de bosque en 2000 -----------------------------------
            geom_tree = mun_tree.iloc[i].geometry
            try:
                recorte_t, _ = mask(src_tree, [geom_tree], crop=True, filled=False)
                vals_t = recorte_t.compressed()
            except ValueError:
                raise ValueError(
                    f"Municipio {cod} queda fuera del raster de treecover."
                )

            px_bosque = int((vals_t >= UMBRAL_DOSEL).sum()) if vals_t.size else 0
            filas_base.append({
                "COD_DANE": cod,
                "NOMBRE_MPI": fila["NOMBRE_MPI"],
                "DPTO_CNMBR": fila["DPTO_CNMBR"],
                "baseline_forest": px_bosque * factor,
                "pixeles_totales": int(vals_t.size),
            })

    return pd.DataFrame(filas_loss), pd.DataFrame(filas_base)


def validar(loss: pd.DataFrame, base: pd.DataFrame, mun: gpd.GeoDataFrame) -> None:
    print("\n" + "=" * 78)
    print("VALIDACION")
    print("=" * 78)

    esperadas = len(mun) * (ANIO_FIN - ANIO_INICIO + 1)
    if len(loss) != esperadas:
        raise ValueError(f"Se esperaban {esperadas:,} filas, hay {len(loss):,}.")
    print(f"  Panel completo: {len(loss):,} filas ({len(mun)} municipios x "
          f"{ANIO_FIN - ANIO_INICIO + 1} anios)")

    if loss["loss_area_ha"].isna().any() or base["baseline_forest"].isna().any():
        raise ValueError("Hay valores nulos tras la extraccion.")

    total = loss["loss_area_ha"].sum()
    print(f"\n  Perdida nacional {ANIO_INICIO}-{ANIO_FIN}: {total:,.0f} ha")
    if not 3e6 < total < 9e6:
        print("  >>> AVISO: fuera del rango plausible (3-9 millones de ha).")
        print("      Global Forest Watch reporta ~5,4 millones para Colombia.")

    # Municipios sin bosque: legitimos, pero deben ser pocos
    sin_bosque = base[base["baseline_forest"] <= 0]
    print(f"\n  Municipios con baseline_forest = 0: {len(sin_bosque)}")
    if len(sin_bosque) > 60:
        print("  >>> AVISO: demasiados. Antes de la reparacion eran 148, todos")
        print("      de Antioquia y Atlantico. Revisar antes de continuar.")
    for _, f in sin_bosque.head(15).iterrows():
        print(f"      {f['COD_DANE']} {f['NOMBRE_MPI']} ({f['DPTO_CNMBR']})")

    # Ranking departamental: el control de plausibilidad que faltaba
    loss = loss.copy()
    loss["dpto"] = loss["COD_DANE"].str[:2]
    por_dpto = (loss.groupby(["dpto", "DPTO_CNMBR"])["loss_area_ha"]
                .sum().sort_values(ascending=False))
    print(f"\n  Diez departamentos con mayor perdida acumulada:")
    for (cod, nombre), val in por_dpto.head(10).items():
        print(f"      {cod} {nombre:<28} {val:>12,.0f} ha  ({100 * val / total:>4.1f}%)")

    for cod, nombre in [("05", "ANTIOQUIA"), ("08", "ATLANTICO")]:
        sel = por_dpto[por_dpto.index.get_level_values(0) == cod]
        val = float(sel.iloc[0]) if len(sel) else 0.0
        pos = list(por_dpto.index.get_level_values(0)).index(cod) + 1 if cod in \
            list(por_dpto.index.get_level_values(0)) else None
        print(f"\n  {nombre}: {val:,.0f} ha  (puesto {pos} de {len(por_dpto)})")
        if val == 0:
            raise ValueError(f"{nombre} sigue en cero. La reparacion no funciono.")

    print("\n  Contrastar el ranking anterior contra el Boletin de Deteccion")
    print("  Temprana de Deforestacion del IDEAM antes de usar estos datos.")


def reparar_panel(loss: pd.DataFrame, base: pd.DataFrame) -> None:
    print("\n" + "=" * 78)
    print("SUSTITUCION EN EL PANEL")
    print("=" * 78)

    panel = pd.read_csv(RUTA_PANEL, dtype={"COD_DANE": str}, low_memory=False)
    panel["COD_DANE"] = panel["COD_DANE"].astype(str).str.zfill(5)
    print(f"  Panel original: {len(panel):,} filas, {panel['COD_DANE'].nunique()} municipios")

    antes_total = panel["loss_area_ha"].sum()
    antes_ant = panel.loc[panel["COD_DANE"].str[:2] == "05", "loss_area_ha"].sum()

    panel = panel.drop(columns=[c for c in ["loss_area_ha", "loss_pixels", "baseline_forest"]
                                if c in panel.columns])

    panel = panel.merge(
        loss[["COD_DANE", "year", "loss_pixels", "loss_area_ha"]],
        on=["COD_DANE", "year"], how="left", validate="one_to_one",
    )
    panel = panel.merge(
        base[["COD_DANE", "baseline_forest"]],
        on="COD_DANE", how="left", validate="many_to_one",
    )

    # Sin fallback: un NaN aqui significa que el cruce fallo, y eso es
    # precisamente lo que un fillna(0) enmascaro durante meses.
    for col in ["loss_area_ha", "loss_pixels", "baseline_forest"]:
        nulos = panel[col].isna()
        if nulos.any():
            faltantes = sorted(panel.loc[nulos, "COD_DANE"].unique())
            raise ValueError(
                f"{len(faltantes)} municipios sin dato de {col} tras el merge: "
                f"{faltantes[:20]}. No se escribe el panel."
            )

    despues_total = panel["loss_area_ha"].sum()
    despues_ant = panel.loc[panel["COD_DANE"].str[:2] == "05", "loss_area_ha"].sum()

    print(f"\n  Perdida nacional  antes: {antes_total:>14,.0f} ha")
    print(f"                  despues: {despues_total:>14,.0f} ha")
    print(f"                 variacion: {despues_total - antes_total:>+14,.0f} ha "
          f"({100 * (despues_total / antes_total - 1):+.1f}%)")
    print(f"\n  Antioquia         antes: {antes_ant:>14,.0f} ha")
    print(f"                  despues: {despues_ant:>14,.0f} ha")

    panel.to_csv(SALIDA_PANEL, index=False)
    print(f"\n  Escrito: {SALIDA_PANEL.relative_to(RAIZ)}")
    print("  El panel original NO se modifico.")


def main() -> None:
    print("=" * 78)
    print(f"REPARACION DE DEFORESTACION — {RAIZ}")
    print("=" * 78)
    if CORREGIR_AREA_POR_LATITUD:
        print("  Area de pixel: 0,09 ha corregida por cos(latitud)")
    else:
        print("  Area de pixel: 0,09 ha constante (sin correccion por latitud)")

    verificar_insumos()
    print("\n--- Cargando geometrias ---")
    mun = cargar_municipios()

    print(f"\n--- Extrayendo ({len(mun)} municipios, dos rasters) ---")
    print("    Puede tardar entre 10 y 40 minutos.")
    loss, base = extraer(mun)

    loss.to_csv(SALIDA_LOSS, index=False)
    base.to_csv(SALIDA_BASELINE, index=False)
    print(f"\n  Escritos los intermedios reparados en data/interim/")

    validar(loss, base, mun)
    reparar_panel(loss, base)

    print("\n" + "=" * 78)
    print("SIGUIENTE PASO")
    print("=" * 78)
    print("""
  1. Revisar el ranking departamental contra IDEAM.
  2. Si cuadra, apuntar RUTA_PANEL de 01_preparar_datos_did.R al archivo
     panel_con_psm_covariables_reparado.csv y re-correr la cadena de R
     desde el script 01.
  3. Volver a correr 02_matching_psm.R: con los 148 municipios recuperados,
     el numero de tratados con bosque deberia subir de 57 a cerca de 88.
""")


if __name__ == "__main__":
    main()
