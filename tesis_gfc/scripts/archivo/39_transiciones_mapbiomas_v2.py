#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
39_transiciones_mapbiomas.py
============================

Construye, para los 1.122 municipios DIVIPOLA, la serie anual 2001-2024 de
conversión de bosque natural según MapBiomas Colombia (Colección 3.0), como
outcome alternativo a la pérdida de dosel de Hansen (GFC v1.12).

Definición (una transición por píxel p y año t, comparando mapas t-1 y t):

    DEF_it = sum_p a_p * 1{c_p,t-1 in F} * 1{c_p,t in A}

    F = formación boscosa natural  {1, 3, 5, 6, 49}
    A = usos antrópicos            {14, 9, 35, 74, 21, 24, 30, 25, 75, 31}

v2 (26/09/2026), tras la primera corrida nacional:
  * Persistencia. La deforestación bruta resultó ~4 veces el cambio neto del
    stock, con regeneración ~75 % de la bruta: parpadeo bosque<->mosaico. Se
    añaden DEF^(k) = F(t-1) & A(t) & ... & A(t+k):
        def_mb_p1 (k=1, PRINCIPAL; no existe para 2024)
        def_mb_p2 (k=2, sensibilidad; no existe para 2023-2024)
        reversion = F(t-1) & A(t) & F(t+1)   (mide la oscilación)
  * Robustez: HTTP 500 = transitorio (v1 lo confundía con cómputo porque todo
    mensaje de EE contiene "(computation: ID)"); caída de red = esperar y
    retomar; ningún lote tumba la corrida; con cómputo se AÍSLA primero el
    municipio pesado y solo después se sube tileScale.
  * Caché de municipios difíciles (diagnostics/municipios_dificiles.json): se
    prepartén en los bloques siguientes y en corridas futuras.
  * --simplificar-m: reduce vértices si el costo resulta ser de rasterización.
  * Checkpoint versionado (_v2): no se mezcla con la pila de v1.

Se calculan además, en la misma pasada, las bandas que permiten auditar el
resultado y descomponer la salida de bosque:

    salida_bosque   = F -> no-F        (todo lo que sale del stock)
    def_mb          = F -> A           (deforestación: outcome principal)
    def_mb_silv     = F -> 9           (subconjunto: bosque -> plantación)
    bosque_a_natural= F -> N           (confusión de clase o cambio natural)
    bosque_a_noobs  = F -> 27          (sin observación en t)
    entrada_bosque  = no-F -> F
    regen_mb        = A -> F           (regeneración sobre uso antrópico)
    bosque_mb       = stock F en t
    noobs           = área 27 en t

y dos bandas base (año 2000): stock de bosque natural (denominador fijo de la
tasa y de la dosis ii) y área mapeada.

Identidades contables que se verifican al final (detectan errores de código,
no de datos):

    (1) bosque_t - bosque_{t-1} = entrada_t - salida_t
    (2) salida_t = def_t + bosque_a_natural_t + bosque_a_noobs_t + residuo_t
        residuo > 0 solo si aparecen clases fuera de A, N o 27 (p. ej. 22).

Lógica espacial:
  * La reducción se hace en la grilla NATIVA del mapa (crs + crsTransform del
    asset), no con `scale=30`, para no remuestrear datos categóricos.
  * El área de píxel es ee.Image.pixelArea() (elipsoide), igual que en la
    validación GEE de `37_baseline_forest.py`.
  * Las sumas son aditivas sobre una partición del polígono, así que un
    municipio que agota el cómputo se parte en el ESPACIO (2x2 recursivo) y
    se suman las piezas.

Lecciones de `37` incorporadas: tres regímenes de fallo con remedios distintos
(payload -> menos geometría por solicitud; cómputo -> tileScale hasta 16 y
luego partición espacial; errores reales -> abortar), lotes por presupuesto de
vértices y área en vez de conteo fijo, socket timeout, checkpoint reanudable y
procesamiento de los municipios grandes primero.

Uso (PowerShell, desde cualquier carpeta del repo):
    python scripts\\39_transiciones_mapbiomas.py --ee-project <ID>
    python scripts\\39_transiciones_mapbiomas.py --ee-project <ID> --muestra 30
    python scripts\\39_transiciones_mapbiomas.py --solo-ensamblar
"""
from __future__ import annotations

import argparse
import json
import os
import random
import re
import socket
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd

try:
    import geopandas as gpd
    from shapely.geometry import box, mapping, MultiPolygon, Polygon
except ImportError as exc:  # pragma: no cover
    sys.exit(f"[ERROR] Falta geopandas/shapely ({exc}). "
             "Instala con: python -m pip install geopandas shapely")

try:
    from shapely import make_valid as _make_valid  # shapely >= 2.0
except ImportError:  # pragma: no cover
    _make_valid = None

# `ee` se importa de forma perezosa: --solo-ensamblar no necesita Earth Engine.
ee = None

# ---------------------------------------------------------------------------
# Configuración fija (decidida antes de ver resultados)
# ---------------------------------------------------------------------------
ASSET_MAPBIOMAS = ("projects/mapbiomas-colombia/assets/LULC/COLECCION3/"
                   "INTEGRACION/COLOMBIA-1")
COLECCION = "MapBiomas Colombia Colección 3.0"

# Leyenda oficial de la Colección 3 (Codigo-de-la-Leyenda-coleccion-3.pdf)
LEYENDA = {
    1: "Formación boscosa", 3: "Bosque", 5: "Manglar", 6: "Bosque inundable",
    49: "Vegetación leñosa sobre arena",
    10: "Formación natural no boscosa", 11: "Formación natural no forestal inundable",
    12: "Formación herbácea", 32: "Planicie de marea hipersalina",
    29: "Afloramiento rocoso", 50: "Vegetación herbácea sobre arena",
    13: "Otra formación natural no forestal", 81: "Herbazales o arbustales andinos",
    82: "Herbazales o arbustales andinos inundables",
    14: "Área agropecuaria", 9: "Silvicultura", 35: "Palma aceitera",
    74: "Plátano y banano (beta)", 21: "Mosaico de agricultura o pasto",
    22: "Área sin vegetación", 23: "Playas, dunas y bancos de arena",
    24: "Infraestructura urbana", 30: "Minería",
    68: "Otra área natural sin vegetación", 25: "Otra área sin vegetación",
    75: "Parques solares", 26: "Cuerpo de agua", 33: "Río, lago u océano",
    31: "Acuicultura", 34: "Glaciar y nival", 27: "No observado",
}

CLASES_BOSQUE = [1, 3, 5, 6, 49]
CLASES_ANTROPICAS = [14, 9, 35, 74, 21, 24, 30, 25, 75, 31]
CLASES_SILVICULTURA = [9]
CLASES_NATURALES_NO_BOSQUE = [10, 11, 12, 13, 29, 32, 50, 81, 82,
                              23, 68, 26, 33, 34]
CLASES_NO_OBSERVADO = [27]
# 22 (Área sin vegetación, nivel 1) mezcla natural y antrópico: se deja fuera
# de A y de N a propósito y aparece en el residuo de la identidad (2).
CLASES_AMBIGUAS = [22]

# Bounding box aproximado de Colombia continental (solo para el chequeo de leyenda)
BBOX_COLOMBIA = [-79.1, -4.3, -66.8, 12.5]

TILE_SCALE_MAX = 16          # tope duro de Earth Engine
SOCKET_TIMEOUT_S = 420
MAX_REINTENTOS_RATE = 5
MAX_REINTENTOS_TRANSITORIO = 3   # HTTP 500 antes de tratarlo como cómputo
MAX_ESPERA_RED_MIN = 60          # tiempo máximo esperando que vuelva la red

# v2: bandas de persistencia (k=1, k=2) y reversión. El checkpoint lleva la
# versión en el nombre para no mezclar pilas distintas.
VERSION_PILA = "v2"

# Pila "estable" (39b, 27/09/2026): decidida por la regla de 39c (R1, R2, R3).
# DEF^est_t = [F en t-m ... t-1] & A_t. Excluye la tala de rastrojos jóvenes
# (ciclos de rotación) que dominan la bruta en Andes y Pacífico.
VERSION_ESTABLE = "v1"
M_ESTABLE = 5          # PRINCIPAL, fijado antes del diagnóstico
M_ESTABLE_LARGO = 10   # sensibilidad


class RedCaida(Exception):
    """La red no volvió dentro de MAX_ESPERA_RED_MIN: se guarda y se sale."""


# ---------------------------------------------------------------------------
# Rutas ancladas al centinela
# ---------------------------------------------------------------------------
def encontrar_raiz() -> Path:
    """Sube desde el script y desde el cwd hasta encontrar data/.raiz_canonica.

    La carpeta sombra `scripts/data/` no tiene centinela, así que nunca se
    confunde con el árbol real.
    """
    for base in (Path(__file__).resolve().parent, Path.cwd().resolve()):
        for p in (base, *base.parents):
            if (p / "data" / ".raiz_canonica").is_file():
                return p
    sys.exit("[ERROR] No se encontró data/.raiz_canonica. "
             "Ejecuta desde el repo canónico (no desde una copia sombra).")


def rutas(raiz: Path, pila: str = "transiciones") -> dict:
    out = raiz / "data" / "interim" / "mapbiomas"
    comun = {
        "municipios": raiz / "data" / "interim" / "municipios_clean.gpkg",
        "out": out,
        "diag": out / "diagnostics",
        "dificiles": out / "diagnostics" / "municipios_dificiles.json",
        "panel_transiciones": out / "deforestacion_mapbiomas_municipio_anio.csv",
    }
    if pila == "estable":
        return {**comun,
                "checkpoint": out / f"_checkpoint_estable_mapbiomas_{VERSION_ESTABLE}.csv",
                "panel": out / "deforestacion_estable_mapbiomas_municipio_anio.csv",
                "base": out / "bosque_estable_2000_mapbiomas.csv",
                "procedencia": out / "procedencia_mapbiomas_estable.json",
                "fallidos": out / "diagnostics" / "municipios_fallidos_estable.csv"}
    return {**comun,
            "checkpoint": out / f"_checkpoint_transiciones_mapbiomas_{VERSION_PILA}.csv",
            "panel": out / "deforestacion_mapbiomas_municipio_anio.csv",
            "base": out / "bosque_natural_2000_mapbiomas.csv",
            "procedencia": out / "procedencia_mapbiomas.json",
            "fallidos": out / "diagnostics" / "municipios_fallidos.csv"}


# ---------------------------------------------------------------------------
# Earth Engine
# ---------------------------------------------------------------------------
def iniciar_ee(proyecto: str | None) -> None:
    global ee
    try:
        import ee as _ee
    except ImportError:
        sys.exit("[ERROR] Falta earthengine-api: python -m pip install earthengine-api")
    ee = _ee
    proyecto = proyecto or os.environ.get("EE_PROJECT")
    if not proyecto:
        sys.exit("[ERROR] Pasa --ee-project <ID> o define $env:EE_PROJECT.")
    try:
        ee.Initialize(project=proyecto)
    except Exception as exc:  # noqa: BLE001
        msg = str(exc).lower()
        if "authenticate" in msg or "credential" in msg:
            sys.exit("[ERROR] Sin credenciales de Earth Engine. Corre:\n"
                     "  python -m ee.cli.eecli authenticate")
        raise
    socket.setdefaulttimeout(SOCKET_TIMEOUT_S)
    print(f"[EE] Inicializado con proyecto '{proyecto}' "
          f"(earthengine-api {getattr(ee, '__version__', '?')})", flush=True)


class MapaMapBiomas:
    """Acceso por año al mapa integrado, sea Image multibanda o ImageCollection."""

    _RE_ANIO = re.compile(r"(19|20)\d{2}")

    def __init__(self, asset: str):
        try:
            info = ee.data.getAsset(asset)
        except Exception as exc:  # noqa: BLE001
            sys.exit(f"[ERROR] No se pudo leer el asset {asset}: {exc}\n"
                     "Verifica el ID en colombia.mapbiomas.org/herramientas.")
        self.asset = asset
        self.tipo = info.get("type", "?")
        self._por_anio: dict[int, "ee.Image"] = {}
        self.fuentes: dict[int, str] = {}

        if self.tipo == "IMAGE":
            img = ee.Image(asset)
            for b in img.bandNames().getInfo():
                m = self._RE_ANIO.search(b)
                if m:
                    anio = int(m.group(0))
                    if anio in self._por_anio:
                        print(f"[AVISO] Año {anio} repetido en bandas; se usa {b}")
                    self._por_anio[anio] = img.select(b)
                    self.fuentes[anio] = b
            proy_img = img.select(0)
        elif self.tipo == "IMAGE_COLLECTION":
            col = ee.ImageCollection(asset)
            ids = col.aggregate_array("system:index").getInfo()
            anios = col.aggregate_array("year").getInfo()
            if len(anios) == len(ids) and all(a is not None for a in anios):
                pares = list(zip(ids, anios))
            else:
                pares = []
                for i in ids:
                    m = self._RE_ANIO.search(i)
                    pares.append((i, m.group(0) if m else None))
            for idx, a in pares:
                if a is None:
                    continue
                self._por_anio[int(a)] = ee.Image(f"{asset}/{idx}").select(0)
                self.fuentes[int(a)] = idx
            if not self._por_anio:
                sys.exit("[ERROR] No se identificaron años en la colección.")
            proy_img = self._por_anio[min(self._por_anio)]
        else:
            sys.exit(f"[ERROR] Tipo de asset no soportado: {self.tipo}")

        proy = proy_img.projection().getInfo()
        self.crs = proy["crs"]
        self.transform = proy.get("transform")
        print(f"[MB] {self.tipo} con años {min(self._por_anio)}-{max(self._por_anio)}; "
              f"crs={self.crs}, transform={self.transform}", flush=True)

    @property
    def anios(self) -> list[int]:
        return sorted(self._por_anio)

    def anio(self, a: int):
        if a not in self._por_anio:
            raise KeyError(f"Año {a} no disponible. Disponibles: {self.anios}")
        return self._por_anio[a]


def verificar_leyenda(mapa: MapaMapBiomas, anios: list[int]) -> dict:
    """Chequeo grueso de que los códigos del ráster están en la leyenda.

    Escala gruesa (5 km) por costo: clases raras pueden no aparecer. El
    residuo de la identidad (2) es el control fino.
    """
    bbox = ee.Geometry.Rectangle(BBOX_COLOMBIA, None, False)
    reporte = {}
    for a in anios:
        h = mapa.anio(a).reduceRegion(
            reducer=ee.Reducer.frequencyHistogram(), geometry=bbox,
            scale=5000, maxPixels=1e10, bestEffort=True).getInfo()
        hist = next(iter(h.values()), None) or {}
        codigos = sorted({int(float(k)) for k in hist})
        desconocidos = [c for c in codigos if c not in LEYENDA]
        ambiguos = [c for c in codigos if c in CLASES_AMBIGUAS]
        reporte[a] = {"codigos": codigos, "desconocidos": desconocidos,
                      "ambiguos": ambiguos}
        print(f"[LEYENDA] {a}: {len(codigos)} códigos; "
              f"desconocidos={desconocidos or 'ninguno'}; "
              f"ambiguos presentes={ambiguos or 'ninguno'}", flush=True)
        if desconocidos:
            print("[AVISO] Hay códigos fuera de la leyenda: irán al residuo. "
                  "Revisa la leyenda antes de interpretar.", flush=True)
    return reporte


def construir_pila(mapa: MapaMapBiomas, anios_bloque: list[int],
                   incluir_base: bool, anio_base: int):
    """Imagen multibanda con todas las transiciones de un bloque de años."""
    area = ee.Image.pixelArea().divide(1e4)   # ha, elipsoide

    def es(img, clases):
        # 1 si la clase está en `clases`, 0 si no; conserva la máscara del mapa
        return img.remap(clases, [1] * len(clases), 0)

    bandas = []
    if incluir_base:
        c0 = mapa.anio(anio_base)
        bandas.append(es(c0, CLASES_BOSQUE).multiply(area)
                      .rename(f"base_bosque_{anio_base}"))
        bandas.append(c0.gte(0).multiply(area)
                      .rename(f"base_area_mapeada_{anio_base}"))

    ultimo = max(mapa.anios)
    for t in anios_bloque:
        prev, cur = mapa.anio(t - 1), mapa.anio(t)
        f0, f1 = es(prev, CLASES_BOSQUE), es(cur, CLASES_BOSQUE)
        a1 = es(cur, CLASES_ANTROPICAS)
        defo = f0.And(a1)
        componentes = {}
        # Persistencia: el uso antrópico se mantiene k años más. Filtra el
        # parpadeo bosque<->mosaico. No existe cuando t+k supera el último mapa.
        if t + 1 <= ultimo:
            siguiente = mapa.anio(t + 1)
            a2 = es(siguiente, CLASES_ANTROPICAS)
            componentes["def_mb_p1"] = defo.And(a2)
            componentes["reversion"] = defo.And(es(siguiente, CLASES_BOSQUE))
            if t + 2 <= ultimo:
                componentes["def_mb_p2"] = defo.And(a2).And(
                    es(mapa.anio(t + 2), CLASES_ANTROPICAS))
        componentes.update({
            "def_mb": defo,
            "def_mb_silv": f0.And(es(cur, CLASES_SILVICULTURA)),
            "bosque_a_natural": f0.And(es(cur, CLASES_NATURALES_NO_BOSQUE)),
            "bosque_a_noobs": f0.And(es(cur, CLASES_NO_OBSERVADO)),
            "salida_bosque": f0.And(f1.Not()),
            "entrada_bosque": f0.Not().And(f1),
            "regen_mb": es(prev, CLASES_ANTROPICAS).And(f1),
            "bosque_mb": f1,
            "noobs": es(cur, CLASES_NO_OBSERVADO),
        })
        for nombre, mascara in componentes.items():
            bandas.append(mascara.multiply(area).rename(f"{nombre}_{t}"))
    return ee.Image.cat(bandas)


def construir_pila_estable(mapa: MapaMapBiomas, anios_bloque: list[int],
                           incluir_base: bool, anio_base: int):
    """Conversión de bosque ESTABLE.

        est_m(t)        = prod_{y=t-m+1}^{t} 1{c_y in F}   (bosque los últimos m años)
        def_est5_t      = est_5(t-1) & A_t                  PRINCIPAL
        def_est5_p1_t   = est_5(t-1) & A_t & A_{t+1}        + persistencia
        rev_est5_t      = est_5(t-1) & A_t & F_{t+1}        oscilación residual
        def_est10_t     = est_10(t-1) & A_t                 sensibilidad
        bosque_est5_t   = est_5(t)                          stock estable
    Base 2000: est_5(2000) y est_10(2000) como denominadores fijos.
    Por construcción def_est10 <= def_est5 <= def_mb (el QC lo verifica).
    """
    area = ee.Image.pixelArea().divide(1e4)
    ultimo = max(mapa.anios)
    cache: dict[int, object] = {}

    def es(img, clases):
        return img.remap(clases, [1] * len(clases), 0)

    def F(y):
        if y not in cache:
            cache[y] = es(mapa.anio(y), CLASES_BOSQUE)
        return cache[y]

    def estable(t_fin, m):
        img = F(t_fin)
        for y in range(t_fin - m + 1, t_fin):
            img = img.And(F(y))
        return img

    bandas = []
    if incluir_base:
        bandas += [
            estable(anio_base, M_ESTABLE).multiply(area)
            .rename(f"base_bosque_est{M_ESTABLE}_{anio_base}"),
            estable(anio_base, M_ESTABLE_LARGO).multiply(area)
            .rename(f"base_bosque_est{M_ESTABLE_LARGO}_{anio_base}"),
            mapa.anio(anio_base).gte(0).multiply(area)
            .rename(f"base_area_mapeada_{anio_base}"),
        ]
    for t in anios_bloque:
        a_t = es(mapa.anio(t), CLASES_ANTROPICAS)
        d5 = estable(t - 1, M_ESTABLE).And(a_t)
        comp = {
            f"def_est{M_ESTABLE}": d5,
            f"def_est{M_ESTABLE_LARGO}": estable(t - 1, M_ESTABLE_LARGO).And(a_t),
            f"bosque_est{M_ESTABLE}": estable(t, M_ESTABLE),
        }
        if t + 1 <= ultimo:
            sig = mapa.anio(t + 1)
            comp[f"def_est{M_ESTABLE}_p1"] = d5.And(es(sig, CLASES_ANTROPICAS))
            comp[f"rev_est{M_ESTABLE}"] = d5.And(es(sig, CLASES_BOSQUE))
        for nombre, mascara in comp.items():
            bandas.append(mascara.multiply(area).rename(f"{nombre}_{t}"))
    return ee.Image.cat(bandas)


# ---------------------------------------------------------------------------
# Geometrías
# ---------------------------------------------------------------------------
def _redondear(obj, nd=6):
    if isinstance(obj, (list, tuple)):
        return [_redondear(x, nd) for x in obj]
    if isinstance(obj, float):
        return round(obj, nd)
    if isinstance(obj, dict):
        return {k: _redondear(v, nd) for k, v in obj.items()}
    return obj


def _poligonal(g):
    """Extrae solo la parte poligonal (intersecciones pueden dar colecciones)."""
    if g.is_empty:
        return None
    if isinstance(g, (Polygon, MultiPolygon)):
        return g
    polys = [x for x in getattr(g, "geoms", []) if isinstance(x, (Polygon, MultiPolygon))]
    if not polys:
        return None
    partes = []
    for p in polys:
        partes.extend(p.geoms if isinstance(p, MultiPolygon) else [p])
    return MultiPolygon(partes)


def n_vertices(g) -> int:
    if isinstance(g, Polygon):
        return len(g.exterior.coords) + sum(len(r.coords) for r in g.interiors)
    if hasattr(g, "geoms"):
        return sum(n_vertices(x) for x in g.geoms)
    return 0


def geom_a_ee(g):
    # geodesic=False: aristas rectas en lat/lon, igual que el GPKG
    return ee.Geometry(_redondear(mapping(g)), None, False)


def cargar_municipios(ruta: Path) -> gpd.GeoDataFrame:
    if not ruta.is_file():
        sys.exit(f"[ERROR] No existe {ruta}. Corre 01_prepare_boundaries.py.")
    gdf = gpd.read_file(ruta, layer="municipios_clean")
    if gdf.empty:
        sys.exit(f"[ERROR] {ruta} tiene 0 features (¿boundaries vacío?).")
    if "COD_DANE" not in gdf.columns:
        sys.exit("[ERROR] El GPKG no tiene COD_DANE.")
    gdf["COD_DANE"] = gdf["COD_DANE"].astype(str).str.zfill(5)
    if gdf.crs is None:
        sys.exit("[ERROR] El GPKG no tiene CRS definido.")

    # Área plana de referencia en MAGNA-SIRGAS Origen Nacional (EPSG:9377)
    gdf["area_municipio_ha"] = gdf.to_crs(9377).geometry.area / 1e4

    gdf = gdf.to_crs(4326)
    if _make_valid is not None:
        gdf["geometry"] = gdf.geometry.apply(_make_valid)
    else:
        gdf["geometry"] = gdf.geometry.buffer(0)
    gdf["geometry"] = gdf.geometry.apply(_poligonal)
    vacias = gdf.geometry.isna()
    if vacias.any():
        print(f"[AVISO] {vacias.sum()} geometrías vacías tras validar: "
              f"{gdf.loc[vacias, 'COD_DANE'].tolist()}")
        gdf = gdf[~vacias]
    return gdf


def dividir_espacial(item: dict, n: int = 2) -> list[dict]:
    """Parte el polígono en una rejilla n x n. La suma de las piezas = total."""
    g = item["geom"]
    minx, miny, maxx, maxy = g.bounds
    dx, dy = (maxx - minx) / n, (maxy - miny) / n
    piezas = []
    for i in range(n):
        for j in range(n):
            celda = box(minx + i * dx, miny + j * dy,
                        minx + (i + 1) * dx, miny + (j + 1) * dy)
            parte = _poligonal(g.intersection(celda))
            if parte is not None and parte.area > 0:
                piezas.append({**item, "geom": parte,
                               "pieza": f"{item['pieza']}.{i}{j}",
                               "vertices": n_vertices(parte),
                               "area_ha": item["area_ha"] * parte.area / g.area})
    return piezas


def empaquetar(items: list[dict], max_vertices: int, max_area_ha: float,
               max_n: int) -> list[list[dict]]:
    """Lotes por presupuesto (vértices ~ payload, área ~ cómputo)."""
    lotes, actual, v, a = [], [], 0, 0.0
    for it in items:
        excede = (actual and (v + it["vertices"] > max_vertices
                              or a + it["area_ha"] > max_area_ha
                              or len(actual) >= max_n))
        if excede:
            lotes.append(actual)
            actual, v, a = [], 0, 0.0
        actual.append(it)
        v += it["vertices"]
        a += it["area_ha"]
    if actual:
        lotes.append(actual)
    return lotes


# ---------------------------------------------------------------------------
# Reducción con manejo de fallos
# ---------------------------------------------------------------------------
def clasificar_error(exc: BaseException) -> str:
    """Régimen de fallo. OJO: todo mensaje de EE trae '(computation: "ID")',
    así que 'computation' a secas NO sirve para reconocer timeouts (bug de v1)."""
    msg = str(exc).lower()
    # 1) Red caída: esperar y repetir, nunca partir
    if isinstance(exc, ConnectionError) or any(s in msg for s in (
            "getaddrinfo", "failed to resolve", "name resolution",
            "max retries exceeded", "connection aborted", "connection reset",
            "remote end closed", "temporarily unavailable", "network is unreachable")):
        return "red"
    # 2) Cómputo: timeouts y memoria (antes que 'transitorio')
    if isinstance(exc, (socket.timeout, TimeoutError)):
        return "computo"
    if any(s in msg for s in ("computation timed out", "timed out", "memory limit",
                              "user memory", "deadline exceeded", "aggregation")):
        if "too many concurrent" not in msg:
            return "computo"
    # 3) Payload
    if any(s in msg for s in ("payload", "request size", "413", "too large")):
        return "payload"
    # 4) Cuota
    if any(s in msg for s in ("too many requests", "429", "quota exceeded",
                              "rate limit", "too many concurrent")):
        return "rate"
    # 5) Error interno del servidor (HTTP 500/502/503/504): reintentar igual
    if any(s in msg for s in ("internal error", "backend error", " 500",
                              " 502", " 503", " 504", "service unavailable")):
        return "transitorio"
    return "fatal"


def esperar_red(max_min: float = MAX_ESPERA_RED_MIN) -> None:
    """Bloquea hasta que resuelva el DNS de Earth Engine o se agote el plazo."""
    limite = time.time() + 60 * max_min
    while time.time() < limite:
        try:
            socket.getaddrinfo("earthengine.googleapis.com", 443)
            print("    [red] conexión restablecida", flush=True)
            return
        except OSError:
            time.sleep(30)
    raise RedCaida(f"Sin red durante {max_min} min")


def reducir(pila, items: list[dict], mapa: MapaMapBiomas, tile_scale: int):
    feats = [ee.Feature(geom_a_ee(it["geom"]),
                        {"COD_DANE": it["cod"], "pieza": it["pieza"]})
             for it in items]
    kwargs = dict(collection=ee.FeatureCollection(feats),
                  reducer=ee.Reducer.sum(), tileScale=tile_scale, crs=mapa.crs)
    if mapa.transform:
        kwargs["crsTransform"] = mapa.transform
    else:
        kwargs["scale"] = 30
    res = pila.reduceRegions(**kwargs)
    # Sin geometría de vuelta: solo propiedades (reduce el payload de bajada)
    res = res.select(propertySelectors=[".*"], retainGeometry=False)
    return [f["properties"] for f in res.getInfo()["features"]]


def procesar(pila, items, mapa, ts, cfg, profundidad=0) -> list[dict]:
    """Reintenta según el régimen de fallo. Devuelve propiedades por pieza.

    Orden de remedios para cómputo: primero AISLAR (partir el lote), después
    subir tileScale sobre el ítem aislado y por último partirlo en el espacio.
    En v1 se subía tileScale sobre el lote entero: tres timeouts pagados por
    un solo municipio pesado.
    """
    intentos_rate = intentos_trans = 0
    while True:
        try:
            return reducir(pila, items, mapa, ts)
        except (KeyboardInterrupt, RedCaida):
            raise
        except Exception as exc:  # noqa: BLE001
            tipo = clasificar_error(exc)
            etiqueta = f"{len(items)} geom, ts={ts}, prof={profundidad}"

            if tipo == "red":
                print(f"    [red] {etiqueta}: sin conexión, espero...", flush=True)
                esperar_red(cfg.max_espera_red)
                continue
            if tipo == "rate" and intentos_rate < MAX_REINTENTOS_RATE:
                espera = 30 * 2 ** intentos_rate
                print(f"    [rate] {etiqueta}: espero {espera}s", flush=True)
                time.sleep(espera)
                intentos_rate += 1
                continue
            if tipo == "transitorio":
                if intentos_trans < MAX_REINTENTOS_TRANSITORIO:
                    espera = 20 * 2 ** intentos_trans
                    print(f"    [500] {etiqueta}: reintento en {espera}s", flush=True)
                    time.sleep(espera)
                    intentos_trans += 1
                    continue
                tipo = "computo"   # 500 persistente: suele ser carga excesiva

            if tipo in ("payload", "computo"):
                if len(items) > 1:
                    k = len(items) // 2
                    print(f"    [{tipo}] {etiqueta}: parto el lote en 2", flush=True)
                    return (procesar(pila, items[:k], mapa, ts, cfg, profundidad)
                            + procesar(pila, items[k:], mapa, ts, cfg, profundidad))
                if tipo == "computo" and ts < TILE_SCALE_MAX:
                    nuevo = min(TILE_SCALE_MAX, ts * 2)
                    print(f"    [cómputo] {etiqueta}: tileScale -> {nuevo}", flush=True)
                    return procesar(pila, items, mapa, nuevo, cfg, profundidad)
                if profundidad >= cfg.max_profundidad:
                    raise RuntimeError(
                        f"{items[0]['cod']} pieza {items[0]['pieza']}: "
                        f"falla aún partido a profundidad {profundidad}: {exc}") from exc
                piezas = dividir_espacial(items[0])
                cod = items[0]["cod"]
                cfg.dificiles[cod] = max(cfg.dificiles.get(cod, 0), profundidad + 1)
                print(f"    [{tipo}] {cod}: partición espacial en {len(piezas)} piezas "
                      f"(prof {profundidad + 1})", flush=True)
                # Las piezas heredan el tileScale alto: ya sabemos que es pesado
                out = []
                for p in piezas:
                    out += procesar(pila, [p], mapa, ts, cfg, profundidad + 1)
                return out
            raise


def prepartir(item: dict, niveles: int) -> list[dict]:
    """Aplica `niveles` particiones 2x2 de antemano (municipios ya conocidos)."""
    piezas = [item]
    for _ in range(niveles):
        piezas = [q for p in piezas for q in dividir_espacial(p)]
    return piezas


def simplificar(g, tol_m: float):
    """Simplificación con tolerancia en metros, muy por debajo del píxel.
    Reduce vértices (costo de rasterización en EE) sin mover el área de forma
    apreciable; el QC de área mapeada lo verifica."""
    if tol_m <= 0:
        return g
    s = g.simplify(tol_m / 111_320.0, preserve_topology=True)
    return _poligonal(s) or g


def a_largo(props: list[dict], bloque: int) -> pd.DataFrame:
    df = pd.DataFrame(props)
    if df.empty:
        return pd.DataFrame(columns=["COD_DANE", "bloque", "banda", "valor_ha"])
    df["COD_DANE"] = df["COD_DANE"].astype(str).str.zfill(5)
    bandas = [c for c in df.columns if c not in ("COD_DANE", "pieza")]
    nulos = int(df[bandas].isna().sum().sum())
    if nulos:
        print(f"    [AVISO] {nulos} valores nulos (región sin píxeles válidos) -> 0")
    df[bandas] = df[bandas].apply(pd.to_numeric, errors="coerce").fillna(0.0)
    agg = df.groupby("COD_DANE", as_index=False)[bandas].sum()   # suma de piezas
    largo = agg.melt(id_vars="COD_DANE", var_name="banda", value_name="valor_ha")
    largo.insert(1, "bloque", bloque)
    return largo


# ---------------------------------------------------------------------------
# Ensamblaje y QC (no requiere Earth Engine)
# ---------------------------------------------------------------------------
def ensamblar(r: dict, municipios: pd.DataFrame | None, anio_base: int,
              anio_fin: int) -> int:
    if not r["checkpoint"].is_file():
        sys.exit(f"[ERROR] No hay checkpoint en {r['checkpoint']}.")
    ck = pd.read_csv(r["checkpoint"], dtype={"COD_DANE": str})
    ck["COD_DANE"] = ck["COD_DANE"].str.zfill(5)
    ck = ck.drop_duplicates(["COD_DANE", "bloque", "banda"], keep="last")
    partes = ck["banda"].str.rsplit("_", n=1, expand=True)
    ck["variable"], ck["anio"] = partes[0], partes[1].astype(int)

    es_base = ck["variable"].str.startswith("base_")
    base = (ck[es_base].pivot_table(index="COD_DANE", columns="variable",
                                    values="valor_ha", aggfunc="sum")
            .rename(columns={"base_bosque": "bosque_mb_2000_ha",
                             "base_area_mapeada": "area_mapeada_mb_ha"})
            .reset_index())
    panel = (ck[~es_base].pivot_table(index=["COD_DANE", "anio"], columns="variable",
                                      values="valor_ha", aggfunc="sum")
             .reset_index())
    panel.columns.name = None
    panel = panel.rename(columns={c: f"{c}_ha" for c in panel.columns
                                  if c not in ("COD_DANE", "anio")})

    panel = panel.merge(base[["COD_DANE", "bosque_mb_2000_ha"]], on="COD_DANE", how="left")
    panel["residuo_salida_ha"] = (panel["salida_bosque_ha"] - panel["def_mb_ha"]
                                  - panel["bosque_a_natural_ha"] - panel["bosque_a_noobs_ha"])
    for col in ("def_mb_p1_ha", "def_mb_p2_ha", "reversion_ha"):
        if col not in panel.columns:          # checkpoint v1 o bloque sin t+1
            panel[col] = np.nan
    den = panel["bosque_mb_2000_ha"].where(panel["bosque_mb_2000_ha"] > 0)
    panel["tasa_def_mb"] = panel["def_mb_ha"] / den          # bruta (k=0)
    panel["tasa_def_mb_p1"] = panel["def_mb_p1_ha"] / den    # PRINCIPAL (k=1)
    panel["tasa_def_mb_p2"] = panel["def_mb_p2_ha"] / den    # sensibilidad (k=2)
    panel = panel.sort_values(["COD_DANE", "anio"]).reset_index(drop=True)

    qc = {}
    # QC 1: completitud
    anios = list(range(anio_base + 1, anio_fin + 1))
    cods = (sorted(municipios["COD_DANE"]) if municipios is not None
            else sorted(panel["COD_DANE"].unique()))
    esperado = pd.MultiIndex.from_product([cods, anios], names=["COD_DANE", "anio"])
    faltan = esperado.difference(pd.MultiIndex.from_frame(panel[["COD_DANE", "anio"]]))
    qc["municipio_anio_faltantes"] = len(faltan)
    if len(faltan):
        pd.DataFrame(list(faltan), columns=["COD_DANE", "anio"]).to_csv(
            r["diag"] / "qc_faltantes.csv", index=False)

    # QC 2: identidad de stock (detecta errores de código)
    # Stock del año anterior SOLO si ese año existe (v1 comparaba contra la
    # fila previa aunque hubiera huecos de años por un checkpoint parcial)
    previo = panel[["COD_DANE", "anio", "bosque_mb_ha"]].assign(anio=panel["anio"] + 1)
    panel_id = panel.merge(previo.rename(columns={"bosque_mb_ha": "bosque_prev"}),
                           on=["COD_DANE", "anio"], how="left")
    es_primero = panel_id["anio"] == anio_base + 1
    panel_id.loc[es_primero, "bosque_prev"] = panel_id.loc[es_primero, "bosque_mb_2000_ha"]
    panel_id = panel_id[panel_id["bosque_prev"].notna()].copy()
    panel_id["brecha_ha"] = (panel_id["bosque_mb_ha"] - panel_id["bosque_prev"]
                             - (panel_id["entrada_bosque_ha"] - panel_id["salida_bosque_ha"]))
    tol = np.maximum(1.0, 1e-4 * panel_id["bosque_prev"].abs())
    malos = panel_id.loc[panel_id["brecha_ha"].abs() > tol]
    qc["identidad_stock_violaciones"] = len(malos)
    if len(malos):
        malos.to_csv(r["diag"] / "qc_identidad_stock.csv", index=False)

    # QC 3: residuo de la descomposición (clases ambiguas o desconocidas)
    sal = panel["salida_bosque_ha"].sum()
    qc["residuo_pct_de_salida_nacional"] = (
        round(100 * panel["residuo_salida_ha"].sum() / sal, 4) if sal else None)

    # QC 4: no observado (error de medición potencialmente ligado al tratamiento)
    panel["noobs_pct_bosque2000"] = np.where(
        panel["bosque_mb_2000_ha"] > 0,
        100 * panel["noobs_ha"] / panel["bosque_mb_2000_ha"], np.nan)
    altos = panel[panel["noobs_pct_bosque2000"] > 5]
    qc["municipio_anio_noobs_mayor_5pct"] = len(altos)
    if len(altos):
        altos[["COD_DANE", "anio", "noobs_ha", "noobs_pct_bosque2000"]].to_csv(
            r["diag"] / "qc_no_observado.csv", index=False)

    # QC 5: área mapeada vs área del polígono (fronteras, San Andrés)
    if municipios is not None:
        base = base.merge(municipios[["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR",
                                      "area_municipio_ha"]], on="COD_DANE", how="left")
        base["ratio_mapeada"] = base["area_mapeada_mb_ha"] / base["area_municipio_ha"]
        fuera = base[(base["ratio_mapeada"] < 0.97) | (base["ratio_mapeada"] > 1.03)]
        qc["municipios_area_fuera_3pct"] = len(fuera)
        fuera.to_csv(r["diag"] / "qc_area_mapeada.csv", index=False)

    # QC 6: oscilación bosque <-> antrópico (parpadeo de clasificación)
    valido = panel["reversion_ha"].notna()
    d0 = panel.loc[valido, "def_mb_ha"].sum()
    if d0:
        qc["reversion_pct_de_def_bruta"] = round(
            100 * panel.loc[valido, "reversion_ha"].sum() / d0, 2)
        qc["p1_pct_de_def_bruta"] = round(
            100 * panel.loc[valido, "def_mb_p1_ha"].sum() / d0, 2)
    v2 = panel["def_mb_p2_ha"].notna()
    d2 = panel.loc[v2, "def_mb_ha"].sum()
    if d2:
        qc["p2_pct_de_def_bruta"] = round(100 * panel.loc[v2, "def_mb_p2_ha"].sum() / d2, 2)

    # Totales nacionales (para contrastar con las estadísticas publicadas)
    nac = panel.groupby("anio")[["def_mb_ha", "def_mb_p1_ha", "def_mb_p2_ha",
                                 "reversion_ha", "regen_mb_ha", "def_mb_silv_ha",
                                 "bosque_mb_ha", "noobs_ha"]].sum(min_count=1)
    nac.to_csv(r["diag"] / "totales_nacionales_por_anio.csv")
    qc["bosque_mb_2000_nacional_Mha"] = round(base["bosque_mb_2000_ha"].sum() / 1e6, 3)

    panel.to_csv(r["panel"], index=False)
    base.to_csv(r["base"], index=False)
    print("\n[QC]", json.dumps(qc, indent=2, ensure_ascii=False))
    print(nac.round(0).to_string())
    print(f"\n[OK] {r['panel']} ({len(panel)} filas)\n[OK] {r['base']} ({len(base)} filas)")
    (r["diag"] / "qc_resumen.json").write_text(json.dumps(qc, indent=2, ensure_ascii=False),
                                               encoding="utf-8")
    graves = (qc["identidad_stock_violaciones"] > 0 or qc["municipio_anio_faltantes"] > 0)
    return 1 if graves else 0


def ensamblar_estable(r: dict, municipios: pd.DataFrame | None, anio_base: int,
                      anio_fin: int) -> int:
    if not r["checkpoint"].is_file():
        sys.exit(f"[ERROR] No hay checkpoint en {r['checkpoint']}.")
    ck = pd.read_csv(r["checkpoint"], dtype={"COD_DANE": str})
    ck["COD_DANE"] = ck["COD_DANE"].str.zfill(5)
    ck = ck.drop_duplicates(["COD_DANE", "bloque", "banda"], keep="last")
    partes = ck["banda"].str.rsplit("_", n=1, expand=True)
    ck["variable"], ck["anio"] = partes[0], partes[1].astype(int)
    es_base = ck["variable"].str.startswith("base_")

    base = (ck[es_base].pivot_table(index="COD_DANE", columns="variable",
                                    values="valor_ha", aggfunc="sum").reset_index())
    base.columns.name = None
    base = base.rename(columns={c: c.replace("base_", "") + f"_{anio_base}_ha"
                                for c in base.columns if c.startswith("base_")})
    panel = (ck[~es_base].pivot_table(index=["COD_DANE", "anio"], columns="variable",
                                      values="valor_ha", aggfunc="sum").reset_index())
    panel.columns.name = None
    panel = panel.rename(columns={c: f"{c}_ha" for c in panel.columns
                                  if c not in ("COD_DANE", "anio")})
    m, ml = M_ESTABLE, M_ESTABLE_LARGO
    for c in (f"def_est{m}_p1_ha", f"rev_est{m}_ha"):
        if c not in panel.columns:
            panel[c] = np.nan
    den5 = f"bosque_est{m}_{anio_base}_ha"
    den10 = f"bosque_est{ml}_{anio_base}_ha"
    panel = panel.merge(base[["COD_DANE", den5, den10]], on="COD_DANE", how="left")
    d5 = panel[den5].where(panel[den5] > 0)
    d10 = panel[den10].where(panel[den10] > 0)
    panel[f"tasa_def_est{m}"] = panel[f"def_est{m}_ha"] / d5          # PRINCIPAL
    panel[f"tasa_def_est{m}_p1"] = panel[f"def_est{m}_p1_ha"] / d5
    panel[f"tasa_def_est{ml}"] = panel[f"def_est{ml}_ha"] / d10
    panel = panel.sort_values(["COD_DANE", "anio"]).reset_index(drop=True)

    qc = {}
    anios = list(range(anio_base + 1, anio_fin + 1))
    cods = (sorted(municipios["COD_DANE"]) if municipios is not None
            else sorted(panel["COD_DANE"].unique()))
    esperado = pd.MultiIndex.from_product([cods, anios], names=["COD_DANE", "anio"])
    faltan = esperado.difference(pd.MultiIndex.from_frame(panel[["COD_DANE", "anio"]]))
    qc["municipio_anio_faltantes"] = len(faltan)

    # Inclusiones que deben cumplirse por construcción (detectan errores de código)
    tol = 1.0
    viol = {
        f"est{ml}_mayor_que_est{m}": int((panel[f"def_est{ml}_ha"]
                                          > panel[f"def_est{m}_ha"] + tol).sum()),
        f"p1_mayor_que_est{m}": int((panel[f"def_est{m}_p1_ha"]
                                     > panel[f"def_est{m}_ha"] + tol).sum()),
    }
    if r["panel_transiciones"].is_file():
        tr = pd.read_csv(r["panel_transiciones"], dtype={"COD_DANE": str},
                         usecols=["COD_DANE", "anio", "def_mb_ha", "bosque_mb_ha"])
        tr["COD_DANE"] = tr["COD_DANE"].str.zfill(5)
        x = panel.merge(tr, on=["COD_DANE", "anio"], how="inner")
        viol[f"est{m}_mayor_que_bruta"] = int((x[f"def_est{m}_ha"] > x["def_mb_ha"] + tol).sum())
        viol["stock_estable_mayor_que_stock"] = int(
            (x[f"bosque_est{m}_ha"] > x["bosque_mb_ha"] + tol).sum())
        v = x[x["anio"] <= 2023]
        qc[f"est{m}_pct_de_bruta"] = round(100 * v[f"def_est{m}_ha"].sum()
                                           / v["def_mb_ha"].sum(), 2)
    qc["violaciones_inclusion"] = viol
    v = panel[panel[f"rev_est{m}_ha"].notna()]
    if v[f"def_est{m}_ha"].sum():
        qc[f"reversion_pct_de_est{m}"] = round(100 * v[f"rev_est{m}_ha"].sum()
                                               / v[f"def_est{m}_ha"].sum(), 2)
    qc[f"bosque_est{m}_{anio_base}_nacional_Mha"] = round(base[den5].sum() / 1e6, 3)

    nac = panel.groupby("anio")[[f"def_est{m}_ha", f"def_est{m}_p1_ha", f"rev_est{m}_ha",
                                 f"def_est{ml}_ha", f"bosque_est{m}_ha"]].sum(min_count=1)
    nac.to_csv(r["diag"] / "totales_nacionales_estable.csv")
    panel.to_csv(r["panel"], index=False)
    base.to_csv(r["base"], index=False)
    (r["diag"] / "qc_resumen_estable.json").write_text(
        json.dumps(qc, indent=2, ensure_ascii=False), encoding="utf-8")
    print("\n[QC ESTABLE]", json.dumps(qc, indent=2, ensure_ascii=False))
    print(nac.round(0).to_string())
    print(f"\n[OK] {r['panel']} ({len(panel)} filas)\n[OK] {r['base']} ({len(base)} filas)")
    graves = qc["municipio_anio_faltantes"] > 0 or any(viol.values())
    return 1 if graves else 0


# ---------------------------------------------------------------------------
# Principal
# ---------------------------------------------------------------------------
def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[3])
    ap.add_argument("--ee-project", default=None)
    ap.add_argument("--asset", default=ASSET_MAPBIOMAS)
    ap.add_argument("--anio-base", type=int, default=2000,
                    help="Año del stock base; la primera transición es base->base+1")
    ap.add_argument("--anio-fin", type=int, default=2024)
    ap.add_argument("--anios-por-bloque", type=int, default=6)
    ap.add_argument("--max-vertices", type=int, default=120_000)
    ap.add_argument("--max-area-kha", type=float, default=4_000,
                    help="Presupuesto de área por lote en miles de ha")
    ap.add_argument("--max-municipios-lote", type=int, default=40)
    ap.add_argument("--tile-scale", type=int, default=4)
    ap.add_argument("--max-profundidad", type=int, default=4,
                    help="Niveles de partición 2x2 (4 -> hasta 256 piezas)")
    ap.add_argument("--codigos", default=None, help="Lista separada por comas")
    ap.add_argument("--muestra", type=int, default=None)
    ap.add_argument("--semilla", type=int, default=42)
    ap.add_argument("--solo-ensamblar", action="store_true")
    ap.add_argument("--reiniciar", action="store_true",
                    help="Borra el checkpoint y empieza de cero")
    ap.add_argument("--dificiles", dest="semilla_dificiles", default=None,
                    help="Semilla 'COD:niveles,...' de municipios a prepartir")
    ap.add_argument("--simplificar-m", type=float, default=0.0,
                    help="Tolerancia de simplificación en metros (0 = no)")
    ap.add_argument("--pila", choices=["transiciones", "estable"], default="transiciones",
                    help="transiciones = 39 v2; estable = 39b (bosque estable m=5/10)")
    ap.add_argument("--max-espera-red", type=float, default=MAX_ESPERA_RED_MIN,
                    help="Minutos esperando que vuelva la red antes de salir")
    cfg = ap.parse_args()

    raiz = encontrar_raiz()
    r = rutas(raiz, cfg.pila)
    fn_pila = construir_pila_estable if cfg.pila == "estable" else construir_pila
    fn_ensamblar = ensamblar_estable if cfg.pila == "estable" else ensamblar
    print(f"[PILA] {cfg.pila} -> checkpoint {r['checkpoint'].name}", flush=True)
    r["diag"].mkdir(parents=True, exist_ok=True)
    print(f"[RAÍZ] {raiz}", flush=True)

    mun = cargar_municipios(r["municipios"])
    print(f"[MUN] {len(mun)} municipios", flush=True)

    # Caché de municipios que necesitaron partición espacial (persiste entre
    # bloques y entre corridas: el costo de descubrirlos se paga una sola vez)
    cfg.dificiles = {}
    if r["dificiles"].is_file():
        cfg.dificiles = {k: int(v) for k, v in
                         json.loads(r["dificiles"].read_text(encoding="utf-8")).items()}
    if cfg.semilla_dificiles:
        for par in cfg.semilla_dificiles.split(","):
            cod, _, niv = par.strip().partition(":")
            if cod:
                cod = cod.zfill(5)
                cfg.dificiles[cod] = max(cfg.dificiles.get(cod, 0), int(niv or 1))
    if cfg.dificiles:
        print(f"[DIFÍCILES] {len(cfg.dificiles)} municipios se prepartirán: "
              f"{cfg.dificiles}", flush=True)

    if cfg.solo_ensamblar:
        return fn_ensamblar(r, mun, cfg.anio_base, cfg.anio_fin)

    if cfg.reiniciar and r["checkpoint"].exists():
        r["checkpoint"].unlink()

    # Selección de municipios
    sel = mun
    if cfg.codigos:
        pedidos = {c.strip().zfill(5) for c in cfg.codigos.split(",") if c.strip()}
        sel = mun[mun["COD_DANE"].isin(pedidos)]
        faltan = pedidos - set(sel["COD_DANE"])
        if faltan:
            print(f"[AVISO] Códigos no encontrados: {sorted(faltan)}")
    if cfg.muestra:
        # Aleatoria: el orden por código DANE dejaría fuera Amazonía/Orinoquía
        sel = sel.sample(n=min(cfg.muestra, len(sel)), random_state=cfg.semilla)

    iniciar_ee(cfg.ee_project)
    mapa = MapaMapBiomas(cfg.asset)
    desde = cfg.anio_base - (M_ESTABLE_LARGO - 1 if cfg.pila == "estable" else 0)
    necesarios = range(desde, cfg.anio_fin + 1)
    ausentes = [a for a in necesarios if a not in mapa.anios]
    if ausentes:
        sys.exit(f"[ERROR] El asset no tiene los años {ausentes}")
    leyenda = verificar_leyenda(mapa, [cfg.anio_base, cfg.anio_fin])

    # Procedencia (se escribe antes de extraer: queda aunque la corrida se corte)
    r["procedencia"].write_text(json.dumps({
        "coleccion": COLECCION, "asset": cfg.asset, "tipo_asset": mapa.tipo,
        "fuente_por_anio": {str(k): v for k, v in mapa.fuentes.items()},
        "crs": mapa.crs, "crs_transform": mapa.transform,
        "clases": {"bosque": CLASES_BOSQUE, "antropicas": CLASES_ANTROPICAS,
                   "silvicultura": CLASES_SILVICULTURA,
                   "naturales_no_bosque": CLASES_NATURALES_NO_BOSQUE,
                   "no_observado": CLASES_NO_OBSERVADO, "ambiguas": CLASES_AMBIGUAS},
        "chequeo_leyenda": leyenda, "pila": cfg.pila,
        "version_pila": VERSION_ESTABLE if cfg.pila == "estable" else VERSION_PILA,
        "m_estable": [M_ESTABLE, M_ESTABLE_LARGO] if cfg.pila == "estable" else None,
        "argumentos": {k: v for k, v in vars(cfg).items() if k != "dificiles"},
        "municipios_dificiles_al_inicio": dict(cfg.dificiles),
        "earthengine_api": getattr(ee, "__version__", None),
        "fecha_utc": datetime.now(timezone.utc).isoformat(),
    }, indent=2, ensure_ascii=False), encoding="utf-8")

    # Ítems: municipios grandes primero (los fallos difíciles salen temprano)
    items = []
    for row in sel.itertuples():
        g = simplificar(row.geometry, cfg.simplificar_m)
        items.append({"cod": row.COD_DANE, "pieza": "0", "geom": g,
                      "vertices": n_vertices(g),
                      "vertices_orig": n_vertices(row.geometry),
                      "area_ha": float(row.area_municipio_ha)})
    if cfg.simplificar_m > 0:
        vo = sum(i["vertices_orig"] for i in items)
        vs = sum(i["vertices"] for i in items)
        print(f"[GEOM] simplificación {cfg.simplificar_m} m: vértices {vo:,} -> {vs:,}",
              flush=True)
    items.sort(key=lambda d: d["area_ha"], reverse=True)

    transiciones = list(range(cfg.anio_base + 1, cfg.anio_fin + 1))
    bloques = [transiciones[i:i + cfg.anios_por_bloque]
               for i in range(0, len(transiciones), cfg.anios_por_bloque)]

    hechos = set()
    if r["checkpoint"].is_file():
        prev = pd.read_csv(r["checkpoint"], dtype={"COD_DANE": str},
                           usecols=["COD_DANE", "bloque"])
        hechos = set(zip(prev["COD_DANE"].str.zfill(5), prev["bloque"]))
        print(f"[CHECKPOINT] {len(hechos)} pares (municipio, bloque) ya hechos", flush=True)

    def guardar(props, b):
        largo = a_largo(props, b)
        largo.to_csv(r["checkpoint"], mode="a", index=False,
                     header=not r["checkpoint"].exists())
        r["dificiles"].write_text(json.dumps(cfg.dificiles, indent=1), encoding="utf-8")

    fallidos, red_caida = [], False
    for b, anios_b in enumerate(bloques):
        if red_caida:
            break
        pila = fn_pila(mapa, anios_b, incluir_base=(b == 0),
                              anio_base=cfg.anio_base)
        pendientes = [it for it in items if (it["cod"], b) not in hechos]
        dific = [it for it in pendientes if it["cod"] in cfg.dificiles]
        normales = [it for it in pendientes if it["cod"] not in cfg.dificiles]
        lotes = empaquetar(normales, cfg.max_vertices,
                           cfg.max_area_kha * 1e3, cfg.max_municipios_lote)
        print(f"\n[BLOQUE {b}] transiciones {anios_b[0]}-{anios_b[-1]}: "
              f"{len(normales)} municipios en {len(lotes)} lotes "
              f"+ {len(dific)} difíciles prepartidos", flush=True)

        # Tareas: (etiqueta, función que devuelve props). Difíciles primero,
        # prepartidos al nivel conocido y con tileScale máximo desde el inicio.
        tareas = []
        for it in dific:
            # Tope de 2 niveles (16 piezas): prepartir todo el municipio al nivel
            # que necesitó su peor pieza cuesta más que dejar recursar solo a esa
            niv = min(cfg.dificiles[it["cod"]], 2)
            tareas.append((f"{it['cod']} (prepartido x{niv})", [it], niv))
        for lote in lotes:
            tareas.append((f"{len(lote)} municipios", lote, 0))

        for i, (etq, lote, niv) in enumerate(tareas, 1):
            t0 = time.time()
            try:
                if niv:
                    props = []
                    for pz in prepartir(lote[0], niv):
                        props += procesar(pila, [pz], mapa, TILE_SCALE_MAX, cfg, niv)
                else:
                    props = procesar(pila, lote, mapa, cfg.tile_scale, cfg)
            except RedCaida as exc:
                print(f"\n[RED] {exc}. Lo hecho está en el checkpoint; "
                      "vuelve a correr el mismo comando para retomar.", flush=True)
                red_caida = True
                break
            except KeyboardInterrupt:
                raise
            except Exception as exc:  # noqa: BLE001  (un lote no tumba la corrida)
                print(f"  [FALLO] tarea {i}: {type(exc).__name__}: {str(exc)[:200]}",
                      flush=True)
                fallidos += [{"COD_DANE": it["cod"], "bloque": b,
                              "error": f"{type(exc).__name__}: {str(exc)[:300]}"}
                             for it in lote]
                continue
            guardar(props, b)
            print(f"  tarea {i}/{len(tareas)}: {etq}, {time.time() - t0:.0f}s", flush=True)

    if fallidos:
        pd.DataFrame(fallidos).to_csv(r["fallidos"], index=False)
        print(f"\n[AVISO] {len(fallidos)} (municipio, bloque) fallidos -> {r['fallidos']}. "
              "Vuelve a correr: el checkpoint retoma solo los pendientes.")

    if cfg.dificiles:
        vert = {it["cod"]: it["vertices"] for it in items}
        med = int(np.median([it["vertices"] for it in items])) if items else 0
        print(f"\n[DIFÍCILES] vértices (mediana de la selección = {med:,}):")
        for cod, niv in sorted(cfg.dificiles.items()):
            print(f"  {cod}: niveles={niv}, vértices={vert.get(cod, '?'):,}"
                  if isinstance(vert.get(cod), int) else f"  {cod}: niveles={niv}")

    codigo = fn_ensamblar(r, sel if (cfg.codigos or cfg.muestra) else mun,
                       cfg.anio_base, cfg.anio_fin)
    if red_caida:
        return 2
    return 1 if fallidos else codigo


if __name__ == "__main__":
    sys.exit(main())
