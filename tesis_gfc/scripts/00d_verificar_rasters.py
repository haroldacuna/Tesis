"""
00d_verificar_rasters.py

Determina si los rasters locales de Hansen sirven para re-extraer la perdida
de bosque, o si hay que volver a descargarlos.

CONTEXTO
--------
La aritmetica ya identifico el mecanismo del defecto: en la cadena

    loss_by_municipio_year.csv     792 municipios   108.431,1 ha   Antioquia 7.205,1
    panel_municipio_year_base.csv  1.122 municipios 100.689,8 ha   Antioquia 0,0

la diferencia 108.431,1 - 100.689,8 = 7.741,3 ha equivale exactamente a
Antioquia (7.205,1) mas Atlantico (536,1). El merge de 03_expand_panel_years.py
pierde esos dos departamentos y el fillna(0) lo enmascara.

Queda una duda previa a re-extraer: el total nacional de esa corrida fue de
108.431 ha, cuando la perdida real de Colombia en 2001-2024 ronda los 5,3
millones. Dos explicaciones posibles:

  (a) el raster de lossyear esta recortado a una fraccion del territorio, o
  (b) la extraccion corrio contra la capa incompleta de 792 municipios.

Este script las distingue. Si es (b), re-extraer con la capa completa resuelve
todo. Si es (a), hay que re-descargar los tiles de Hansen primero.

Solo LEE. No escribe ni modifica nada.

USO
---
    python 00d_verificar_rasters.py
"""

from pathlib import Path

try:
    import geopandas as gpd
    import rasterio
    import numpy as np
    from rasterio.mask import mask
except ImportError as exc:
    raise SystemExit(f"Falta una dependencia: {exc}. Instalar con conda-forge.")

# Bounding box continental de Colombia en EPSG:4326.
COLOMBIA_BBOX = {"oeste": -79.1, "este": -66.8, "sur": -4.3, "norte": 13.5}

# Municipios de prueba: todos con perdida de bosque documentada por IDEAM.
# Se eligen de departamentos distintos para detectar recortes parciales.
MUNICIPIOS_PRUEBA = {
    "05040": "ANORI (Antioquia, Bajo Cauca)",
    "05837": "TURBO (Antioquia, Uraba)",
    "18753": "SAN VICENTE DEL CAGUAN (Caqueta)",
    "18001": "FLORENCIA (Caqueta)",
    "27001": "QUIBDO (Choco)",
    "08001": "BARRANQUILLA (Atlantico)",
    "50370": "LA MACARENA (Meta)",
}

PIXEL_HA = 0.09


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


def describir_raster(ruta: Path, etiqueta: str) -> dict | None:
    print(f"\n{'-' * 78}\n{etiqueta}\n{'-' * 78}")
    if not ruta.exists():
        print(f"  NO EXISTE: {ruta}")
        return None

    with rasterio.open(ruta) as src:
        b = src.bounds
        print(f"  Archivo    : {ruta.name}  ({ruta.stat().st_size:,} bytes)")
        print(f"  CRS        : {src.crs}")
        print(f"  Dimensiones: {src.width:,} x {src.height:,} pixeles")
        print(f"  Resolucion : {src.res[0]:.8f} x {src.res[1]:.8f}")
        print(f"  dtype      : {src.dtypes[0]}   nodata: {src.nodata}")
        print(f"  Extension  : oeste {b.left:.3f}  este {b.right:.3f}")
        print(f"               sur   {b.bottom:.3f}  norte {b.top:.3f}")

        # Cobertura del territorio continental
        cubre = (
            b.left <= COLOMBIA_BBOX["oeste"] + 0.5
            and b.right >= COLOMBIA_BBOX["este"] - 0.5
            and b.bottom <= COLOMBIA_BBOX["sur"] + 0.5
            and b.top >= COLOMBIA_BBOX["norte"] - 0.5
        )
        print(f"\n  Bbox de Colombia: oeste {COLOMBIA_BBOX['oeste']}  este {COLOMBIA_BBOX['este']}"
              f"  sur {COLOMBIA_BBOX['sur']}  norte {COLOMBIA_BBOX['norte']}")
        if cubre:
            print("  >>> El raster CUBRE el territorio continental.")
        else:
            print("  >>> EL RASTER NO CUBRE TODO EL TERRITORIO.")
            for lado, val, ref, cmp in [
                ("oeste", b.left, COLOMBIA_BBOX["oeste"], "mayor que"),
                ("este", b.right, COLOMBIA_BBOX["este"], "menor que"),
                ("sur", b.bottom, COLOMBIA_BBOX["sur"], "mayor que"),
                ("norte", b.top, COLOMBIA_BBOX["norte"], "menor que"),
            ]:
                falta = (val > ref + 0.5) if lado in ("oeste", "sur") else (val < ref - 0.5)
                if falta:
                    print(f"      borde {lado}: {val:.3f} es {cmp} {ref} — falta territorio")

        # Resolucion esperada de Hansen: 1 arcsec ~ 0.00025 grados
        if abs(src.res[0] - 0.00025) > 0.00005:
            print(f"  >>> AVISO: resolucion inusual. Hansen GFC usa ~0.00025 grados.")
            print(f"      Si el raster fue remuestreado, el conteo de pixeles y por")
            print(f"      tanto las hectareas quedan mal escalados.")

        return {"bounds": b, "crs": src.crs, "res": src.res}


def probar_municipios(raiz: Path, ruta_raster: Path) -> None:
    print(f"\n{'=' * 78}")
    print("LECTURA POR MUNICIPIO (lossyear)")
    print("=" * 78)

    capa = raiz / "data" / "interim" / "municipios_clean.gpkg"
    if not capa.exists():
        print(f"  No existe {capa}")
        return

    mun = gpd.read_file(capa, layer="municipios_clean")
    mun["COD_DANE"] = mun["COD_DANE"].astype(str).str.zfill(5)
    print(f"  Capa: {len(mun)} municipios, {mun['COD_DANE'].str[:2].nunique()} departamentos")

    with rasterio.open(ruta_raster) as src:
        mun_r = mun.to_crs(src.crs)
        print(f"\n  {'MUNICIPIO':<38} {'PIXELES':>10} {'CON PERDIDA':>12} {'HECTAREAS':>12}")
        print("  " + "-" * 74)

        for cod, nombre in MUNICIPIOS_PRUEBA.items():
            fila = mun_r[mun_r["COD_DANE"] == cod]
            if len(fila) == 0:
                print(f"  {nombre:<38} {'NO ESTA EN LA CAPA':>36}")
                continue

            geom = fila.geometry.iloc[0]
            if geom.is_empty:
                print(f"  {nombre:<38} {'GEOMETRIA VACIA':>36}")
                continue

            try:
                recorte, _ = mask(src, [geom], crop=True, filled=False)
                vals = recorte.compressed()
                con_perdida = int((vals > 0).sum())
                ha = con_perdida * PIXEL_HA
                marca = "  <-- CERO" if con_perdida == 0 else ""
                print(f"  {nombre:<38} {vals.size:>10,} {con_perdida:>12,} {ha:>12,.1f}{marca}")
            except ValueError:
                print(f"  {nombre:<38} {'FUERA DE LA EXTENSION DEL RASTER':>36}")
            except Exception as exc:
                print(f"  {nombre:<38} error: {str(exc)[:30]}")


def diagnostico_final(raiz: Path) -> None:
    print(f"\n{'=' * 78}")
    print("COMO LEER ESTE RESULTADO")
    print("=" * 78)
    print("""
  Si los municipios de prueba devuelven pixeles con perdida en cantidades
  razonables (Anori y San Vicente del Caguan deberian dar decenas de miles de
  hectareas cada uno), entonces los rasters SIRVEN. La corrida que produjo
  108.431 ha nacionales fallo por usar la capa incompleta de 792 municipios,
  no por los rasters. Basta re-correr:

      python 02_extract_loss_by_municipio.py
      python 03_expand_panel_years.py     <- CON EL PARCHE, ver abajo

  Si en cambio devuelven cero pixeles, o 'fuera de la extension', el raster
  esta recortado y hay que re-descargar los tiles de Hansen que cubren
  Colombia (10N_080W, 10N_070W, 00N_080W, 00N_070W, 20N_080W) desde
  storage.googleapis.com/earthenginepartners-hansen/

  PARCHE OBLIGATORIO en 03_expand_panel_years.py, cualquiera sea el caso.
  Sustituir:

      panel["loss_area_ha"] = panel["loss_area_ha"].fillna(0.0)

  Por:

      sin_dato = panel["loss_area_ha"].isna()
      if sin_dato.any():
          faltantes = sorted(panel.loc[sin_dato, "COD_DANE"].unique())
          raise ValueError(
              f"{len(faltantes)} municipios sin dato de perdida tras el merge: "
              f"{faltantes[:20]}. Un fillna(0) aqui convertiria una extraccion "
              f"fallida en una afirmacion de deforestacion nula."
          )

  Un municipio sin bosque produce 0 de forma legitima porque SI aparece en el
  archivo de perdida con valor cero. Un municipio que no cruza produce NaN.
  El fillna(0) borra esa distincion, y es lo que dejo a Antioquia y Atlantico
  con deforestacion nula durante 24 anios sin que nada lo advirtiera.

  VALIDACION POSTERIOR. Antes de volver al analisis, contrastar los totales
  departamentales contra las cifras publicadas por IDEAM en su Boletin de
  Deteccion Temprana de Deforestacion. Antioquia debe aparecer entre los
  departamentos con mayor perdida del pais. Documentar ese contraste en el
  capitulo de datos: es exactamente el control que habria detectado esto.
""")


if __name__ == "__main__":
    raiz = encontrar_raiz()
    print("=" * 78)
    print(f"VERIFICACION DE RASTERS — {raiz}")
    print("=" * 78)

    ruta_loss = raiz / "data" / "raw" / "gfc" / "Colombia_GFC_lossyear.tif"
    ruta_tree = raiz / "data" / "raw" / "auxiliary" / "Colombia_treecover2000.tif"

    describir_raster(ruta_loss, "RASTER DE PERDIDA ANUAL (lossyear)")
    describir_raster(ruta_tree, "RASTER DE COBERTURA 2000 (treecover)")

    if ruta_loss.exists():
        probar_municipios(raiz, ruta_loss)

    diagnostico_final(raiz)
