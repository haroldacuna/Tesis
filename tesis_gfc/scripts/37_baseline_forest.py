#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
37_baseline_forest.py
=====================
Regenera `data/interim/baseline_forest.csv`: bosque base por municipio en el
anio 2000 segun Hansen Global Forest Change (treecover2000 + datamask).

Reemplaza la rutina `_baseline_forest_by_municipio` de `04_merge_covariates.py`,
que (a) carga el raster completo en memoria y (b) escribe salidas vacias en
silencio cuando faltan insumos.

Diferencias de diseno respecto de 04:
  1. Anclado al centinela `data/.raiz_canonica`: aborta si se ejecuta desde el
     arbol sombra `scripts/data/`.
  2. NUNCA escribe un CSV vacio. Si falta un insumo, levanta excepcion.
  3. Backend `local`: lectura por ventanas (una por municipio), no del raster
     entero. Uso de RAM proporcional al municipio mas grande, no al pais.
  4. Area de pixel calculada por latitud, no con la constante 0,09 ha.
  5. Calcula varios umbrales de dosel (30/50/75 %) para analisis de
     sensibilidad de la dosis de intensidad.

Uso:
    python scripts/37_baseline_forest.py --backend gee
    python scripts/37_baseline_forest.py --backend local
    python scripts/37_baseline_forest.py --backend local --umbral-principal 50

Salida: data/interim/baseline_forest.csv
    COD_DANE, NOMBRE_MPI, DPTO_CNMBR,
    baseline_forest,          <- ha con el umbral principal (compat. con 01_preparar_datos_did.R)
    baseline_forest_tc30/50/75,
    area_municipio_ha,        <- denominador para la dosis (i)
    px_bosque_tc30, fuente, version_gfc, fecha_generacion
"""

from __future__ import annotations

import argparse
import datetime as _dt
import sys
from pathlib import Path

import socket
import numpy as np
import pandas as pd

# geopandas/rasterio se importan tarde para que --help funcione sin el entorno
# espacial completo (en Windows es comun que GDAL falle al importar).

# ---------------------------------------------------------------------------
# Configuracion
# ---------------------------------------------------------------------------

CENTINELA = Path("data/.raiz_canonica")
MUNICIPIOS_CANDIDATOS = [
    Path("data/interim/municipios_clean.gpkg"),
    Path("data/interim/municipios_clean.geojson"),
]
MUNICIPIOS_LAYER = "municipios_clean"
OUT_BASELINE = Path("data/interim/baseline_forest.csv")

# Carpetas donde puede estar el raster local. Se incluyen las dos convenciones
# del repo: 02 usa data/raw/gfc/ y 04 usa data/raw/auxiliary/.
DIRS_RASTER = [Path("data/raw/gfc"), Path("data/raw/auxiliary")]

UMBRALES = (30, 50, 75)          # % de dosel
UMBRAL_PRINCIPAL_DEFECTO = 30    # convencion Hansen / FAO
TILE_SCALE_TOPE = 16             # limite duro de Image.reduceRegions

# IMPORTANTE: esta version debe coincidir con la del lossyear usado en
# 02_extract_loss_by_municipio.py. Si el panel llega a 2024, el lossyear tiene
# que ser la coleccion 2024 (valores 1..24). Verificalo antes de correr.
GEE_COLECCION_DEFECTO = "UMD/hansen/global_forest_change_2024_v1_12"

CRS_METRICO = "EPSG:9377"        # Origen Nacional Colombia, para areas planas
RADIO_TIERRA_M = 6_378_137.0


# ---------------------------------------------------------------------------
# Raiz canonica
# ---------------------------------------------------------------------------

def anclar_raiz_canonica(inicio: Path | None = None) -> Path:
    """Sube por el arbol de directorios hasta encontrar data/.raiz_canonica.

    Evita el problema documentado de las cuatro copias del arbol de datos: si
    el script se corre desde `tesis_gfc/scripts/`, las rutas relativas apuntan
    a `scripts/data/` (la sombra) y se escriben salidas que R nunca lee.
    """
    actual = (inicio or Path.cwd()).resolve()
    for candidata in [actual, *actual.parents]:
        if (candidata / CENTINELA).exists():
            return candidata
    raise SystemExit(
        f"ERROR: no se encontro el centinela '{CENTINELA}' subiendo desde {actual}.\n"
        "       Corre el script desde tesis_gfc/ o crea el centinela con:\n"
        "       New-Item -ItemType File data\\.raiz_canonica"
    )


# ---------------------------------------------------------------------------
# Insumos
# ---------------------------------------------------------------------------

def cargar_municipios(raiz: Path):
    import geopandas as gpd

    for rel in MUNICIPIOS_CANDIDATOS:
        ruta = raiz / rel
        if not ruta.exists():
            continue
        if ruta.suffix.lower() == ".gpkg":
            mun = gpd.read_file(ruta, layer=MUNICIPIOS_LAYER)
        else:
            mun = gpd.read_file(ruta)

        # Fallo silencioso conocido: 01_prepare_boundaries.py escribe un GPKG
        # valido pero con 0 features cuando no encuentra los limites crudos.
        if len(mun) == 0:
            raise SystemExit(
                f"ERROR: {ruta} existe pero tiene 0 features.\n"
                "       Es la capa vacia que escribe 01_prepare_boundaries.py cuando\n"
                "       data/raw/boundaries/ esta vacio. Vuelve a correr 01 con los\n"
                "       shapefiles DIVIPOLA en su sitio antes de este script."
            )

        faltantes = {"COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR"} - set(mun.columns)
        if faltantes:
            raise SystemExit(f"ERROR: {ruta} sin columnas {sorted(faltantes)}")

        mun["COD_DANE"] = mun["COD_DANE"].astype(str).str.zfill(5)
        print(f"[ok] municipios: {len(mun)} features desde {ruta} (CRS {mun.crs})")
        return mun

    raise SystemExit(
        f"ERROR: no hay capa de municipios. Buscado en: "
        f"{[str(raiz / r) for r in MUNICIPIOS_CANDIDATOS]}"
    )


def buscar_raster(raiz: Path, patron: str) -> Path | None:
    """Busca un .tif cuyo nombre contenga `patron` en las carpetas conocidas."""
    candidatos: list[Path] = []
    for d in DIRS_RASTER:
        carpeta = raiz / d
        if not carpeta.exists():
            continue
        for tif in carpeta.glob("*.tif"):
            if patron in tif.stem.lower() and tif.stat().st_size > 0:
                # Prioriza el que mencione 2000 y el nombre mas corto.
                rango = 0 if "2000" in tif.stem else 1
                candidatos.append((rango, len(tif.name), tif))
    if not candidatos:
        return None
    candidatos.sort(key=lambda t: (t[0], t[1], t[2].name.lower()))
    return candidatos[0][2]


def area_municipal_ha(mun) -> pd.DataFrame:
    """Area planimetrica por municipio en EPSG:9377 (denominador de la dosis i)."""
    m = mun.to_crs(CRS_METRICO)
    return pd.DataFrame(
        {
            "COD_DANE": mun["COD_DANE"].values,
            "area_municipio_ha": (m.geometry.area / 10_000.0).values,
        }
    )


# ---------------------------------------------------------------------------
# Backend LOCAL: raster de Hansen en disco, lectura por ventanas
# ---------------------------------------------------------------------------

def _areas_por_fila(transform, filas: int, columnas: int) -> np.ndarray:
    """Area en ha de cada pixel, por fila de la ventana.

    Hansen se publica en EPSG:4326 con pixel de 0,00025 grados. El alto en
    metros es aprox. constante; el ancho encoge con cos(latitud). Devuelve un
    vector columna (filas, 1) que se difunde sobre el ancho de la ventana.

        dy_m = |dy_deg| * (pi/180) * R
        dx_m = |dx_deg| * (pi/180) * R * cos(lat)
    """
    dx_deg = abs(transform.a)
    dy_deg = abs(transform.e)
    # Latitud del centro de cada fila de la ventana.
    lat = transform.f - (np.arange(filas) + 0.5) * dy_deg
    rad = np.pi / 180.0
    dy_m = dy_deg * rad * RADIO_TIERRA_M
    dx_m = dx_deg * rad * RADIO_TIERRA_M * np.cos(lat * rad)
    return ((dx_m * dy_m) / 10_000.0).reshape(filas, 1)


def baseline_local(raiz: Path, mun, umbrales=UMBRALES) -> pd.DataFrame:
    import rasterio
    from rasterio.features import geometry_mask
    from rasterio.windows import from_bounds

    tc_path = buscar_raster(raiz, "treecover")
    if tc_path is None:
        raise SystemExit(
            "ERROR: no hay raster treecover2000 en "
            f"{[str(raiz / d) for d in DIRS_RASTER]}.\n"
            "       Opciones:\n"
            "         a) usa --backend gee (no requiere descarga), o\n"
            "         b) baja los 6 tiles de Hansen que cubren Colombia\n"
            "            ({00N,10N,20N} x {070W,080W}), mosaicalos y recortalos\n"
            "            a Colombia_GFC_treecover2000.tif en data/raw/gfc/.\n"
            "       La version DEBE coincidir con la del lossyear usado en 02."
        )
    dm_path = buscar_raster(raiz, "datamask")
    print(f"[ok] treecover: {tc_path}")
    print(f"[{'ok' if dm_path else '!!'}] datamask : {dm_path or 'AUSENTE (no se excluira agua)'}")

    src_tc = rasterio.open(tc_path)
    src_dm = rasterio.open(dm_path) if dm_path else None

    # El raster de Hansen viene en EPSG:4326; reproyectamos los poligonos, no
    # el raster (mucho mas barato).
    mun_r = mun.to_crs(src_tc.crs) if mun.crs != src_tc.crs else mun

    filas = []
    total = len(mun_r)
    for i, fila in enumerate(mun_r.itertuples(index=False), start=1):
        geom = fila.geometry
        if geom is None or geom.is_empty:
            continue

        # Ventana acotada al bbox del municipio: la RAM depende del municipio
        # mas grande, no del pais.
        try:
            win = from_bounds(*geom.bounds, transform=src_tc.transform).round_offsets().round_lengths()
            tc = src_tc.read(1, window=win, boundless=True, fill_value=0)
        except Exception as exc:  # municipio fuera del raster (islas, p.ej.)
            print(f"  [!] {fila.COD_DANE} {fila.NOMBRE_MPI}: ventana invalida ({exc})")
            continue

        if tc.size == 0:
            continue

        win_transform = src_tc.window_transform(win)

        # Mascara del poligono: True DENTRO (invert=True en rasterio).
        dentro = geometry_mask(
            [geom], out_shape=tc.shape, transform=win_transform, invert=True, all_touched=False
        )

        valido = dentro
        if src_dm is not None:
            dm = src_dm.read(1, window=win, boundless=True, fill_value=0)
            # datamask: 1 = tierra firme, 2 = agua permanente, 0 = sin datos
            valido = dentro & (dm == 1)

        areas = _areas_por_fila(win_transform, tc.shape[0], tc.shape[1])

        registro = {
            "COD_DANE": fila.COD_DANE,
            "NOMBRE_MPI": fila.NOMBRE_MPI,
            "DPTO_CNMBR": fila.DPTO_CNMBR,
        }
        for u in umbrales:
            bosque = valido & (tc >= u)
            registro[f"baseline_forest_tc{u}"] = float((bosque * areas).sum())
            if u == umbrales[0]:
                registro[f"px_bosque_tc{u}"] = int(bosque.sum())
        filas.append(registro)

        if i % 100 == 0 or i == total:
            print(f"  ... {i}/{total} municipios")

    src_tc.close()
    if src_dm is not None:
        src_dm.close()

    df = pd.DataFrame(filas)
    df["fuente"] = f"local:{tc_path.name}"
    df["version_gfc"] = tc_path.stem
    return df


# ---------------------------------------------------------------------------
# Backend GEE: sin descargas, calculo del lado del servidor
# ---------------------------------------------------------------------------

def _coste_geom(geom) -> tuple[int, float]:
    """Costo local de un municipio: (vertices, area en grados cuadrados).

    Los vertices predicen el peso de la SUBIDA (bytes del GeoJSON) y el area
    predice el costo del COMPUTO (pixeles a 30 m). Estimarlos antes de enviar
    evita el aprendizaje a golpes: cada peticion fallida cuesta un timeout
    completo del servidor, del orden de minutos.
    """
    try:
        n = len(geom.__geo_interface__["coordinates"].__str__()) // 22  # ~22 chars/vertice
    except Exception:
        n = 1000
    return max(n, 1), float(geom.area)


def _fragmentar(geom, n: int):
    """Parte un poligono en una rejilla de n x n celdas (interseccion).

    La suma de areas de bosque sobre una particion del poligono es igual al
    area sobre el poligono completo, asi que fragmentar no altera el
    resultado: solo reparte el computo en peticiones que si caben.
    """
    from shapely.geometry import box

    minx, miny, maxx, maxy = geom.bounds
    dx, dy = (maxx - minx) / n, (maxy - miny) / n
    piezas = []
    for i in range(n):
        for j in range(n):
            celda = box(minx + i * dx, miny + j * dy,
                        minx + (i + 1) * dx, miny + (j + 1) * dy)
            inter = geom.intersection(celda)
            if not inter.is_empty and inter.area > 0:
                piezas.append(inter)
    return piezas


def _reducir(ee, imagen, feats, tile_scale: int):
    """Envia una lista de ee.Feature y devuelve el resultado. Puede lanzar."""
    reducido = imagen.reduceRegions(
        collection=ee.FeatureCollection(feats),
        reducer=ee.Reducer.sum(),
        scale=30,
        tileScale=tile_scale,
    )
    return reducido.getInfo()


def _feature(ee, geom, props, simplificar: float):
    g = geom.simplify(simplificar, preserve_topology=True) if simplificar > 0 else geom
    return ee.Feature(ee.Geometry(g.__geo_interface__), props)


def _clasificar_error(exc: Exception) -> str:
    """Tres regimenes de fallo, cada uno con su remedio.

      'subida'  -> payload > 10 MB. Menos municipios o menos vertices.
      'computo' -> timeout o memoria. Mas tileScale, o menos area por peticion.
      'otro'    -> asset, permisos, red. Abortar.
    """
    t = str(exc).lower()
    if "payload size exceeds" in t or "request size" in t:
        return "subida"
    if ("timed out" in t or "timeout" in t or "user memory limit" in t
            or "too many" in t or "computed value is too large" in t):
        return "computo"
    return "otro"


def _resolver_municipio(ee, imagen, fila, umbrales, simplificar, tile_scale,
                        max_fragmentos: int = 8) -> dict | None:
    """Resuelve UN municipio, fragmentandolo en el espacio si hace falta.

    Duplica la rejilla (1 -> 2x2 -> 4x4 -> 8x8) hasta que cada celda cabe en
    el presupuesto de computo del servidor. Devuelve None si ni 8x8 alcanza.
    """
    props = {"COD_DANE": fila["COD_DANE"]}
    n = 1
    while n <= max_fragmentos:
        try:
            if n == 1:
                feats = [_feature(ee, fila.geometry, props, simplificar)]
            else:
                feats = [_feature(ee, p, props, simplificar)
                         for p in _fragmentar(fila.geometry, n)]
            salida = _reducir(ee, imagen, feats, tile_scale)
        except Exception as exc:
            if _clasificar_error(exc) == "otro":
                raise
            n *= 2
            print(f"      [~] {fila['COD_DANE']} {fila['NOMBRE_MPI']}: "
                  f"se fragmenta en rejilla {n}x{n}")
            continue

        acum = {f"baseline_forest_tc{u}": 0.0 for u in umbrales}
        for f in salida["features"]:
            p = f["properties"]
            for u in umbrales:
                acum[f"baseline_forest_tc{u}"] += float(
                    p.get(f"baseline_forest_tc{u}", 0.0) or 0.0)
        return {
            "COD_DANE": fila["COD_DANE"],
            "NOMBRE_MPI": fila["NOMBRE_MPI"],
            "DPTO_CNMBR": fila["DPTO_CNMBR"],
            **acum,
            "n_fragmentos": len(feats),
        }
    return None


def _empaquetar(df, max_vert: int, max_area: float, max_n: int) -> list[list[int]]:
    """Agrupa municipios en lotes que respeten los dos presupuestos.

    Un municipio que por si solo excede cualquiera de los dos presupuestos va
    en un lote propio, donde lo resolvera la fragmentacion espacial.
    """
    lotes, actual, v_acum, a_acum = [], [], 0, 0.0
    for idx, fila in df.iterrows():
        v, a = fila["_n_vert"], fila["_area_deg"]
        solo = (v > max_vert) or (a > max_area)
        if solo:
            if actual:
                lotes.append(actual)
                actual, v_acum, a_acum = [], 0, 0.0
            lotes.append([idx])
            continue
        if actual and (v_acum + v > max_vert or a_acum + a > max_area
                       or len(actual) >= max_n):
            lotes.append(actual)
            actual, v_acum, a_acum = [], 0, 0.0
        actual.append(idx)
        v_acum += v
        a_acum += a
    if actual:
        lotes.append(actual)
    return lotes


def baseline_gee(mun, coleccion: str, lote: int = 25, umbrales=UMBRALES,
                 proyecto: str | None = None, simplificar: float = 0.0,
                 checkpoint: Path | None = None, tile_scale_max: int = TILE_SCALE_TOPE,
                 max_vert: int = 45_000, max_area: float = 0.6) -> pd.DataFrame:
    """Suma ee.Image.pixelArea() sobre los pixeles de bosque, por municipio.

    Estrategia (el orden importa, porque cada fallo cuesta un timeout de
    varios minutos):

      1. Estimar el costo de cada municipio LOCALMENTE (vertices y area) y
         empaquetar lotes que respeten ambos presupuestos. Asi casi ningun
         lote falla, en lugar de descubrirlo a golpes.
      2. Si un lote falla igual, partirlo por la mitad.
      3. Si el que falla es UN municipio, fragmentarlo en el espacio: la suma
         sobre una particion del poligono es igual al total, asi que Mitu o
         Taraira se resuelven en una rejilla de celdas que si caben.
      4. Guardar checkpoint despues de cada lote para poder reanudar.
    """
    import ee
    
    socket.setdefaulttimeout(420)

    tile_scale_max = max(1, min(int(tile_scale_max), TILE_SCALE_TOPE))
    kwargs = {"project": proyecto} if proyecto else {}
    try:
        ee.Initialize(**kwargs)
    except Exception:
        print("[..] GEE no inicializado; lanzando autenticacion interactiva")
        ee.Authenticate()
        ee.Initialize(**kwargs)

    gfc = ee.Image(coleccion)
    tc = gfc.select("treecover2000")
    dm = gfc.select("datamask")
    area_ha = ee.Image.pixelArea().divide(10_000)
    imagen = ee.Image.cat([
        tc.gte(u).And(dm.eq(1)).multiply(area_ha).rename(f"baseline_forest_tc{u}")
        for u in umbrales
    ])

    mun_wgs = mun.to_crs("EPSG:4326").reset_index(drop=True)

    registros: list[dict] = []
    ya: set[str] = set()
    if checkpoint is not None and checkpoint.exists():
        prev = pd.read_csv(checkpoint, dtype={"COD_DANE": str})
        if len(prev):
            registros = prev.to_dict("records")
            ya = set(prev["COD_DANE"])
            print(f"[ok] checkpoint: {len(ya)} municipios resueltos, se reanuda")

    pend = mun_wgs[~mun_wgs["COD_DANE"].isin(ya)].reset_index(drop=True)
    if len(pend) == 0:
        print("[ok] nada pendiente")
    else:
        costos = [_coste_geom(g) for g in pend.geometry]
        pend["_n_vert"] = [c[0] for c in costos]
        pend["_area_deg"] = [c[1] for c in costos]

        lotes = _empaquetar(pend, max_vert, max_area, lote)
        solos = sum(1 for L in lotes if len(L) == 1)
        print(f"[ok] {len(pend)} pendientes empaquetados en {len(lotes)} lotes "
              f"(mediana {int(np.median([len(L) for L in lotes]))} municipios; "
              f"{solos} pesados van solos)")

        fallidos: list[str] = []
        hechos = 0

        def _guardar():
            if checkpoint is None or not registros:
                return
            checkpoint.parent.mkdir(parents=True, exist_ok=True)
            pd.DataFrame(registros).to_csv(checkpoint, index=False, encoding="utf-8")

        pila = [(L, 8) for L in lotes][::-1]
        while pila:
            idxs, ts = pila.pop()
            trozo = pend.loc[idxs]
            try:
                feats = [
                    _feature(ee, r.geometry,
                             {"COD_DANE": r.COD_DANE, "NOMBRE_MPI": r.NOMBRE_MPI,
                              "DPTO_CNMBR": r.DPTO_CNMBR},
                             simplificar)
                    for r in trozo.itertuples()
                ]
                salida = _reducir(ee, imagen, feats, ts)
            except Exception as exc:
                clase = _clasificar_error(exc)
                if clase == "otro":
                    print(f"  [X] lote de {len(idxs)} fallo por causa no recuperable:")
                    _guardar()
                    raise
                if clase == "computo" and ts < tile_scale_max:
                    pila.append((idxs, min(ts * 2, tile_scale_max)))
                    continue
                if len(idxs) > 1:
                    m = len(idxs) // 2
                    print(f"  [~] lote de {len(idxs)} no cabe; se parte en {m} y {len(idxs) - m}")
                    pila.extend([(idxs[m:], tile_scale_max), (idxs[:m], tile_scale_max)][::-1])
                    continue
                # Un solo municipio: partir en el espacio.
                fila = pend.loc[idxs[0]]
                reg = _resolver_municipio(ee, imagen, fila, umbrales,
                                          simplificar, tile_scale_max)
                if reg is None:
                    print(f"  [!] {fila['COD_DANE']} {fila['NOMBRE_MPI']}: "
                          f"irresoluble en GEE; usa el valor del backend local")
                    fallidos.append(f"{fila['COD_DANE']} {fila['NOMBRE_MPI']}")
                else:
                    registros.append(reg)
                    hechos += 1
                    _guardar()
                    print(f"  ... {hechos}/{len(pend)} (fragmentado en {reg['n_fragmentos']})")
                continue

            for f in salida["features"]:
                p = f["properties"]
                registros.append({
                    "COD_DANE": p["COD_DANE"],
                    "NOMBRE_MPI": p["NOMBRE_MPI"],
                    "DPTO_CNMBR": p["DPTO_CNMBR"],
                    **{f"baseline_forest_tc{u}": float(p.get(f"baseline_forest_tc{u}", 0.0) or 0.0)
                       for u in umbrales},
                    "n_fragmentos": 1,
                })
            hechos += len(idxs)
            _guardar()
            print(f"  ... {hechos}/{len(pend)} municipios pendientes resueltos")

        if fallidos:
            print(f"\n[!!] {len(fallidos)} sin resolver: {', '.join(fallidos[:10])}")

    df = pd.DataFrame(registros)
    df["fuente"] = f"gee:{coleccion}" + (f"+simplify{simplificar}" if simplificar else "")
    df["version_gfc"] = coleccion.rsplit("/", 1)[-1]
    return df


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--backend", choices=["gee", "local"], default="gee")
    ap.add_argument("--coleccion", default=GEE_COLECCION_DEFECTO,
                    help="Asset de GEE de Hansen GFC (debe coincidir con la version del lossyear)")
    ap.add_argument("--lote", type=int, default=25,
                    help="Tope de municipios por peticion (los lotes se arman por costo, no por conteo)")
    ap.add_argument("--max-vert", type=int, default=45_000,
                    help="Presupuesto de vertices por peticion (controla el peso de la subida)")
    ap.add_argument("--max-area", type=float, default=0.6,
                    help="Presupuesto de area por peticion en grados cuadrados (controla el computo)")
    ap.add_argument("--simplificar", type=float, default=0.0,
                    help="Tolerancia Douglas-Peucker en grados para aligerar la subida (0 = exacto)")
    ap.add_argument("--tile-scale-max", type=int, default=TILE_SCALE_TOPE,
                    help=f"tileScale maximo antes de partir el lote (tope del servidor: {TILE_SCALE_TOPE})")
    ap.add_argument("--checkpoint", default=None,
                    help="CSV donde guardar el avance para poder reanudar (recomendado en --backend gee)")
    ap.add_argument("--ee-project", default=None,
                    help="ID de proyecto de Earth Engine (ej. my-project-1251-491400)")
    ap.add_argument("--umbral-principal", type=int, default=UMBRAL_PRINCIPAL_DEFECTO,
                    choices=list(UMBRALES), help="Umbral de dosel que alimenta la columna baseline_forest")
    ap.add_argument("--codigos", default=None,
                    help="CSV con una columna COD_DANE: restringe la corrida a esos municipios")
    ap.add_argument("--muestra", type=int, default=0,
                    help="Ademas de --codigos, agrega N municipios al azar (semilla fija)")
    ap.add_argument("--salida", default=str(OUT_BASELINE))
    args = ap.parse_args()

    raiz = anclar_raiz_canonica()
    print(f"[ok] raiz canonica: {raiz}")

    mun = cargar_municipios(raiz)
    
    if args.codigos or args.muestra:
        sel = set()
    if args.codigos:
        sel |= set(pd.read_csv(raiz / args.codigos, dtype=str)["COD_DANE"].str.zfill(5))
    if args.muestra:
        resto = mun[~mun["COD_DANE"].isin(sel)]
        sel |= set(resto.sample(min(args.muestra, len(resto)), random_state=42)["COD_DANE"])
    mun = mun[mun["COD_DANE"].isin(sel)].reset_index(drop=True)
    print(f"[ok] corrida restringida a {len(mun)} municipios")

    if args.backend == "local":
        df = baseline_local(raiz, mun)
    else:
        ck = Path(args.checkpoint) if args.checkpoint else None
        if ck is not None and not ck.is_absolute():
            ck = raiz / ck
        df = baseline_gee(mun, args.coleccion, lote=args.lote, proyecto=args.ee_project,
                          simplificar=args.simplificar, checkpoint=ck,
                          tile_scale_max=args.tile_scale_max,
                          max_vert=args.max_vert, max_area=args.max_area)
    # Columna de compatibilidad: 01_preparar_datos_did.R lee `baseline_forest`.
    df["baseline_forest"] = df[f"baseline_forest_tc{args.umbral_principal}"]
    df = df.merge(area_municipal_ha(mun), on="COD_DANE", how="left")
    df["fecha_generacion"] = _dt.datetime.now().isoformat(timespec="seconds")

    # -- Controles de sanidad: no escribir basura sin avisar -----------------
    n_cero = int((df["baseline_forest"] <= 0).sum())
    n_sobre = int((df["baseline_forest"] > df["area_municipio_ha"] * 1.05).sum())
    print(f"\n[qc] municipios: {len(df)}")
    print(f"[qc] bosque base total: {df['baseline_forest'].sum():,.0f} ha")
    print(f"[qc] con bosque = 0: {n_cero}")
    print(f"[qc] con bosque > area municipal (+5%): {n_sobre}")
    if len(df) == 0:
        raise SystemExit("ERROR: 0 filas. No se escribe el CSV (este es el bug que trajo aqui).")
    if n_cero > len(df) * 0.5:
        print("[!!] Mas de la mitad sin bosque: revisa datamask y CRS antes de usar esto.")
    if n_sobre > 0:
        print("[!!] Hay municipios con mas bosque que area: desalineacion CRS o poligonos solapados.")

    ruta = raiz / args.salida
    ruta.parent.mkdir(parents=True, exist_ok=True)
    df.sort_values("COD_DANE").to_csv(ruta, index=False, encoding="utf-8")
    print(f"\n[ok] escrito: {ruta}  ({len(df)} filas)")


if __name__ == "__main__":
    sys.exit(main())
