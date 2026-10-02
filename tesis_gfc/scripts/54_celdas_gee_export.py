#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
54_celdas_gee_export.py
=======================

Evaluacion a nivel de POLIGONO (comentario 3 del asesor): prepara en Google
Earth Engine un panel de celdas de 2 x 2 km con la conversion anual de bosque
natural desglosada por TIPO DE BOSQUE (origen) y TIPO DE USO (destino), segun
MapBiomas Colombia Coleccion 3, y la cobertura de cada celda por los
poligonos de proyectos de Karnik et al. (2025).

Que se exporta (una tabla por tesela de 1 grado, a Google Drive):
  Cada fila = (clave, ha, anio). La clave codifica todo:
    anual  (anio = t):  celda*1e5 + q*1e4 + s*1e3 + o*1e2 + d
        o  origen   1 bosque tierra firme (3), 2 manglar (5), 3 inundable (6),
                    4 lenosa sobre arena (49)
        s  1 = bosque estable los 5 anios previos (t-5..t-1), 0 = rastrojo
        d  destino  1 mosaico agric./pasto (21), 2 area agropecuaria (14),
                    3 palma (35), 4 silvicultura (9), 5 platano (74),
                    6 mineria (30), 7 infraestructura (24), 8 otra sin veg. (25),
                    9 acuicultura (31), 10 parques solares (75),
                    20 natural no boscosa, 30 no observado (27), 40 otras (22...)
        q  persistencia: 1 si el pixel sigue antropico (A) en t+1, 0 si no,
           2 si t = ultimo anio (no evaluable)
    fija   (anio = 0):
        celda*1e5 + 90000 + s*1e3 + o*1e2   bosque natural 2000 por tipo
                                            (s: estable 1996-2000)
        celda*1e5 + 80000 + pid             area de la celda dentro del
                                            proyecto pid
        celda*1e5 + 99999                   area mapeada de la celda
    covariables (anio = -1): una fila por celda con elevacion y pendiente medias
  celda = floor(x/2000)*1e4 + floor(y/2000), en EPSG:9377 (o 3116 si 9377 no
  esta disponible en GEE; queda registrado en el manifiesto).

  Las areas son aditivas: una celda partida entre dos teselas se reconstruye
  sumando sus filas en el paso 55.

Etapas (PowerShell):
  1. & $py scripts\\54_celdas_gee_export.py --ee-project ID --subir-poligonos
       prepara los poligonos (validos, simplificados a 15 m, medio pixel) y los sube como
       asset. Esperar a que la tarea termine (--estado).
  2. & $py scripts\\54_celdas_gee_export.py --ee-project ID --piloto
       lanza UNA tesela (Pacifico chocoano) para verificar el formato.
  3. & $py scripts\\54_celdas_gee_export.py --ee-project ID --lanzar
       lanza todas las teselas pendientes (reanudable: no repite las que ya
       tienen tarea COMPLETED o en curso).
  4. & $py scripts\\54_celdas_gee_export.py --ee-project ID --estado
       resumen de tareas; al terminar, descargar la carpeta de Drive a
       data/raw/celdas_gee/ para el paso 55.
"""
from __future__ import annotations

import argparse
import json
import math
import os
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd

try:
    import geopandas as gpd
    import shapely
except ImportError as exc:  # pragma: no cover
    sys.exit(f"[ERROR] Falta geopandas/shapely ({exc}).")

ee = None  # import perezoso

# ---------------------------------------------------------------------------
# Parametros (fijados antes de exportar)
# ---------------------------------------------------------------------------
ASSET_MB = "projects/mapbiomas-colombia/assets/LULC/COLECCION3/INTEGRACION/COLOMBIA-1"
CELDA_M = 2000
TESELA_GRADOS = 1.0
ANIO_INI, ANIO_FIN = 2001, 2024
M_ESTABLE = 5
SIMPLIFICAR_M = 15.0   # medio pixel de MapBiomas: cambio de area <= 1,8 % por proyecto
BBOX_COLOMBIA = (-79.1, -4.3, -66.8, 12.5)
CARPETA_DRIVE = "tesis_celdas_mapbiomas"
PROYECCIONES = ("EPSG:9377", "EPSG:3116")

ORIGEN = {3: 1, 5: 2, 6: 3, 49: 4, 1: 1}            # 1 = formacion boscosa (nivel 1)
DESTINO = {21: 1, 14: 2, 35: 3, 9: 4, 74: 5, 30: 6, 24: 7, 25: 8, 31: 9, 75: 10,
           10: 20, 11: 20, 12: 20, 13: 20, 29: 20, 32: 20, 50: 20, 81: 20, 82: 20,
           23: 20, 68: 20, 26: 20, 33: 20, 34: 20, 27: 30}
DESTINO_OTROS = 40
BOSQUE = [3, 5, 6, 49, 1]
ANTROPICAS = [14, 9, 35, 74, 21, 24, 30, 25, 75, 31]

# Codificacion (espejo en Python de la aritmetica del servidor; la usa el 55)
BASE_CELDA = 100_000
FIJA_BOSQUE, FIJA_PROYECTO, FIJA_AREA = 90_000, 80_000, 99_999


def codificar_celda(x_m: float, y_m: float) -> int:
    return int(math.floor(x_m / CELDA_M)) * 10_000 + int(math.floor(y_m / CELDA_M))


def decodificar(clave: float) -> dict:
    """Inverso exacto de las claves anuales y fijas (claves < 2^53)."""
    k = int(round(clave))
    celda, resto = divmod(k, BASE_CELDA)
    if resto == FIJA_AREA:
        return {"celda": celda, "tipo": "area"}
    if resto >= FIJA_BOSQUE:
        r = resto - FIJA_BOSQUE
        return {"celda": celda, "tipo": "bosque2000", "s": r // 1000, "o": (r % 1000) // 100}
    if resto >= FIJA_PROYECTO:
        return {"celda": celda, "tipo": "proyecto", "pid": resto - FIJA_PROYECTO}
    q, r = divmod(resto, 10_000)
    s, r = divmod(r, 1_000)
    o, d = divmod(r, 100)
    return {"celda": celda, "tipo": "transicion", "q": q, "s": s, "o": o, "d": d}


# ---------------------------------------------------------------------------
def encontrar_raiz() -> Path:
    for base in (Path(__file__).resolve().parent, Path.cwd().resolve()):
        for p in (base, *base.parents):
            if (p / "data" / ".raiz_canonica").is_file():
                return p
    sys.exit("[ERROR] No se encontro data/.raiz_canonica.")


def iniciar_ee(proyecto: str | None) -> str:
    global ee
    try:
        import ee as _ee
    except ImportError:
        sys.exit("[ERROR] Falta earthengine-api.")
    ee = _ee
    proyecto = proyecto or os.environ.get("EE_PROJECT")
    if not proyecto:
        sys.exit("[ERROR] Pasa --ee-project <ID>.")
    ee.Initialize(project=proyecto)
    return proyecto


def poligonal(g):
    if g is None or g.is_empty:
        return None
    g = shapely.make_valid(g)
    if g.geom_type in ("Polygon", "MultiPolygon"):
        return g
    partes = [x for x in getattr(g, "geoms", []) if x.geom_type in ("Polygon", "MultiPolygon")]
    return shapely.unary_union(partes) if partes else None


def preparar_poligonos(raiz: Path) -> gpd.GeoDataFrame:
    """Poligonos de TODOS los tipos (AD, ARR, IFM): los AD definen tratamiento
    y cualquier tipo define la zona de exclusion de controles en el 55."""
    ruta = raiz / "data" / "raw" / "proyectos_poligonos" / "south_america.gpkg"
    if not ruta.is_file():
        sys.exit(f"[ERROR] Falta {ruta}")
    g = gpd.read_file(ruta)
    g = g[g["Country"].astype(str).str.contains("Colombia", case=False, na=False)]
    g = g[~g.geom_type.isin(["Point", "MultiPoint"])].copy()
    g["geometry"] = [poligonal(x) for x in g.geometry]
    # La base trae vertices con Z (x, y, 0) mezclados con vertices 2D en el mismo
    # poligono; Earth Engine solo acepta pares (x, y) y rechaza la geometria.
    g["geometry"] = shapely.force_2d(g.geometry.values)
    g = g[g.geometry.notna()].to_crs(9377)
    g["geometry"] = g.geometry.simplify(SIMPLIFICAR_M, preserve_topology=True)
    g = g.dissolve(by="ProjectID", as_index=False, aggfunc="first")
    g["pid"] = np.arange(1, len(g) + 1)
    g["tipo"] = g["Project Type"].astype(str).str.upper().str.strip()
    g["anio"] = pd.to_numeric(g["Project Start Date"].astype(str).str.extract(r"((?:19|20)\d{2})")[0],
                              errors="coerce").astype("Int64")
    g["registro"] = g["Registry Name"].astype(str)
    g = g[["pid", "ProjectID", "tipo", "anio", "registro", "geometry"]].to_crs(4326)
    if len(g) > 999:
        sys.exit("[ERROR] Mas de 999 proyectos: la clave de proyecto no alcanza.")
    return g


def teselas(raiz: Path) -> list[dict]:
    """Teselas de 1 grado que tocan el territorio nacional (municipios_clean)."""
    m = gpd.read_file(raiz / "data" / "interim" / "municipios_clean.gpkg", layer="municipios_clean")
    pais = shapely.unary_union(m.to_crs(4326).geometry.values).simplify(0.01)
    out = []
    x0, y0, x1, y1 = BBOX_COLOMBIA
    for x in np.arange(math.floor(x0), math.ceil(x1), TESELA_GRADOS):
        for y in np.arange(math.floor(y0), math.ceil(y1), TESELA_GRADOS):
            caja = shapely.box(x, y, x + TESELA_GRADOS, y + TESELA_GRADOS)
            if caja.intersects(pais):
                ident = f"t_{x:+04.0f}_{y:+03.0f}".replace("+", "p").replace("-", "m")
                out.append({"id": ident,
                            "bbox": [float(x), float(y), float(x + TESELA_GRADOS), float(y + TESELA_GRADOS)]})
    return out


# ---------------------------------------------------------------------------
# Servidor (Earth Engine)
# ---------------------------------------------------------------------------
def elegir_proyeccion() -> str:
    for crs in PROYECCIONES:
        try:
            ee.Image.pixelCoordinates(ee.Projection(crs)).reduceRegion(
                ee.Reducer.first(), ee.Geometry.Point([-74, 4]), 30).getInfo()
            return crs
        except Exception as exc:  # noqa: BLE001
            print(f"[AVISO] {crs} no disponible en GEE ({str(exc)[:80]}); pruebo la siguiente.")
    sys.exit("[ERROR] Ninguna proyeccion metrica disponible para la grilla.")


def construir_expresiones(crs_grilla: str, assets_poligonos: list[str]):
    mb = ee.Image(ASSET_MB)
    bandas = mb.bandNames().getInfo()
    prefijo = bandas[0].rsplit("_", 1)[0] + "_"
    faltan = [a for a in range(ANIO_INI - M_ESTABLE, ANIO_FIN + 1) if f"{prefijo}{a}" not in bandas]
    if faltan:
        sys.exit(f"[ERROR] El asset no tiene los anios {faltan} (prefijo '{prefijo}').")
    proy_mb = mb.select(0).projection().getInfo()

    area = ee.Image.pixelArea().divide(1e4)
    xy = ee.Image.pixelCoordinates(ee.Projection(crs_grilla))
    celda = (xy.select("x").divide(CELDA_M).floor().multiply(10_000)
             .add(xy.select("y").divide(CELDA_M).floor())).toInt64()

    def clase(anio):
        return mb.select(ee.String(prefijo).cat(ee.Number(anio).format("%d")))

    def es(img, clases):
        return img.remap(clases, [1] * len(clases), 0)

    def bosque(anio):
        return es(clase(anio), BOSQUE)

    def estable(anio_fin):
        anios = ee.List.sequence(ee.Number(anio_fin).subtract(M_ESTABLE - 1), anio_fin)
        return ee.ImageCollection(anios.map(lambda a: bosque(a).unmask(0))).min()

    orig_de, orig_a = list(ORIGEN), [ORIGEN[k] for k in ORIGEN]
    dest_de, dest_a = list(DESTINO), [DESTINO[k] for k in DESTINO]

    def clave_anual(anio):
        anio = ee.Number(anio)
        prev, cur = clase(anio.subtract(1)), clase(anio)
        f0 = bosque(anio.subtract(1))
        f1 = bosque(anio)
        o = prev.remap(orig_de, orig_a, 0)
        d = cur.remap(dest_de, dest_a, DESTINO_OTROS)
        s = estable(anio.subtract(1))
        q = ee.Image(ee.Algorithms.If(
            anio.lt(ANIO_FIN),
            es(clase(anio.add(1).min(ANIO_FIN)), ANTROPICAS),
            ee.Image.constant(2)))
        salida = f0.eq(1).And(f1.eq(0))
        k = (celda.multiply(BASE_CELDA).add(q.multiply(10_000)).add(s.multiply(1_000))
             .add(o.multiply(100)).add(d)).toInt64()
        return area.addBands(k.rename("k")).updateMask(salida)

    def clave_bosque2000():
        f = bosque(2000)
        o = clase(2000).remap(orig_de, orig_a, 0)
        s = estable(2000)
        k = celda.multiply(BASE_CELDA).add(FIJA_BOSQUE).add(s.multiply(1_000)).add(o.multiply(100)).toInt64()
        return area.addBands(k.rename("k")).updateMask(f.eq(1))

    fc_proy = ee.FeatureCollection([ee.FeatureCollection(a) for a in assets_poligonos]).flatten()
    pid_img = ee.Image().int32().paint(fc_proy, "pid")

    def clave_proyecto():
        k = celda.multiply(BASE_CELDA).add(FIJA_PROYECTO).add(pid_img).toInt64()
        return area.addBands(k.rename("k")).updateMask(pid_img.gt(0))

    def clave_area():
        k = celda.multiply(BASE_CELDA).add(FIJA_AREA).toInt64()
        return area.addBands(k.rename("k")).updateMask(clase(2000).gte(0))

    dem = ee.Image("USGS/SRTMGL1_003")
    terreno = dem.rename("elev").addBands(ee.Terrain.slope(dem).rename("pend"))

    reductor = ee.Reducer.sum().group(groupField=1, groupName="k")
    kw = {"crs": proy_mb["crs"], "crsTransform": proy_mb["transform"], "maxPixels": 1e12, "tileScale": 4}

    def filas(img, geom, anio):
        grupos = ee.List(img.reduceRegion(reducer=reductor, geometry=geom, **kw).get("groups"))
        return ee.FeatureCollection(grupos.map(
            lambda g: ee.Feature(None, {"k": ee.Dictionary(g).get("k"),
                                        "ha": ee.Dictionary(g).get("sum"), "anio": anio})))

    def tabla_tesela(bbox):
        geom = ee.Geometry.Rectangle(bbox, None, False)
        anuales = ee.FeatureCollection(
            ee.List.sequence(ANIO_INI, ANIO_FIN).map(lambda a: filas(clave_anual(a), geom, a))).flatten()
        fijas = (filas(clave_bosque2000(), geom, 0)
                 .merge(filas(clave_proyecto(), geom, 0))
                 .merge(filas(clave_area(), geom, 0)))
        cov = terreno.addBands(celda.rename("c")).reduceRegion(
            reducer=ee.Reducer.mean().repeat(2).group(groupField=2, groupName="c"),
            geometry=geom, scale=90, crs=crs_grilla, maxPixels=1e12, tileScale=4)
        covs = ee.FeatureCollection(ee.List(cov.get("groups")).map(
            lambda g: ee.Feature(None, {"k": ee.Dictionary(g).get("c"),
                                        "elev": ee.List(ee.Dictionary(g).get("mean")).get(0),
                                        "pend": ee.List(ee.Dictionary(g).get("mean")).get(1),
                                        "anio": -1})))
        return anuales.merge(fijas).merge(covs)

    return tabla_tesela, proy_mb, prefijo


# ---------------------------------------------------------------------------
def cargar_manifiesto(ruta: Path) -> dict:
    return json.loads(ruta.read_text(encoding="utf-8")) if ruta.is_file() else {"tareas": {}}


def guardar_manifiesto(ruta: Path, man: dict) -> None:
    ruta.write_text(json.dumps(man, indent=2, ensure_ascii=False), encoding="utf-8")


def estados_tareas() -> dict:
    """{id_tarea: {'state', 'error_message'}} con la API vigente o la antigua."""
    try:
        ops = ee.data.listOperations()
        out = {}
        for op in ops:
            meta = op.get("metadata", {})
            tid = op.get("name", "").split("/")[-1]
            out[tid] = {"state": meta.get("state", "DESCONOCIDO").replace("SUCCEEDED", "COMPLETED")
                        .replace("PENDING", "READY"),
                        "error_message": op.get("error", {}).get("message", "")}
        return out
    except Exception:  # noqa: BLE001
        return {t["id"]: t for t in ee.data.getTaskList()}


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--ee-project", default=None)
    ap.add_argument("--subir-poligonos", action="store_true")
    ap.add_argument("--piloto", action="store_true")
    ap.add_argument("--lanzar", action="store_true")
    ap.add_argument("--estado", action="store_true")
    ap.add_argument("--solo-preparar", action="store_true",
                    help="Prepara poligonos y teselas localmente, sin Earth Engine")
    cfg = ap.parse_args()

    raiz = encontrar_raiz()
    out = raiz / "data" / "interim" / "celdas"
    out.mkdir(parents=True, exist_ok=True)
    ruta_man = out / "manifiesto_54.json"
    man = cargar_manifiesto(ruta_man)

    pol = preparar_poligonos(raiz)
    pol.drop(columns="geometry").to_csv(out / "proyectos_pid.csv", index=False)
    pol.to_file(out / "proyectos_gee.geojson", driver="GeoJSON",
                layer_options={"COORDINATE_PRECISION": 6})   # ~10 cm
    tes = teselas(raiz)
    tam_kb = (out / "proyectos_gee.geojson").stat().st_size / 1024
    print(f"[LOCAL] {len(pol)} proyectos ({pol.tipo.value_counts().to_dict()}), geojson {tam_kb:.0f} KB | "
          f"{len(tes)} teselas de {TESELA_GRADOS:g} grado")
    if cfg.solo_preparar:
        return 0

    proyecto = iniciar_ee(cfg.ee_project)
    asset = f"projects/{proyecto}/assets/tesis_proyectos_poligonos"

    if cfg.subir_poligonos:
        # Subida en trozos de ~4 MB: una sola solicitud cerca de 10 MB puede
        # rechazarse (limite de tamanio de solicitud de Earth Engine).
        gj = json.loads((out / "proyectos_gee.geojson").read_text(encoding="utf-8"))
        trozos, actual, peso = [], [], 0
        for f in gj["features"]:
            b = len(json.dumps(f))
            if b > 8_000_000:
                sys.exit(f"[ERROR] El proyecto {f['properties'].get('ProjectID')} solo pesa {b/1e6:.1f} MB: "
                         "sube SIMPLIFICAR_M.")
            if actual and peso + b > 4_000_000:
                trozos.append(actual); actual, peso = [], 0
            actual.append(f); peso += b
        if actual:
            trozos.append(actual)
        man["assets_poligonos"], man["tareas_poligonos"] = [], []
        for i, tro in enumerate(trozos, 1):
            feats = [ee.Feature(ee.Geometry(f["geometry"], None, False),
                                {k: v for k, v in f["properties"].items() if v is not None})
                     for f in tro]
            asset_i = f"{asset}_{i}"
            t = ee.batch.Export.table.toAsset(collection=ee.FeatureCollection(feats),
                                              description=f"tesis_proyectos_poligonos_{i}", assetId=asset_i)
            t.start()
            man["assets_poligonos"].append(asset_i)
            man["tareas_poligonos"].append(t.id)
            print(f"[SUBIDA] trozo {i}/{len(trozos)}: {len(tro)} proyectos -> {asset_i} (tarea {t.id})")
        guardar_manifiesto(ruta_man, man)
        print("Espera a que todas queden COMPLETED (--estado) antes de --piloto.")
        return 0

    if cfg.estado:
        est = estados_tareas()
        ids = man.get("tareas_poligonos", []) + [v["tarea"] for v in man["tareas"].values()]
        cuenta: dict[str, int] = {}
        for i in filter(None, ids):
            e = est.get(i, {}).get("state", "DESCONOCIDO")
            cuenta[e] = cuenta.get(e, 0) + 1
        print(f"[ESTADO] {cuenta}")
        fallidas = [k for k, v in man["tareas"].items() if est.get(v["tarea"], {}).get("state") == "FAILED"]
        for k in fallidas:
            print(f"  FAILED {k}: {est[man['tareas'][k]['tarea']].get('error_message', '')[:200]}")
        return 0

    if not (cfg.piloto or cfg.lanzar):
        sys.exit("[ERROR] Indica --subir-poligonos, --piloto, --lanzar o --estado.")
    assets = man.get("assets_poligonos", [])
    if not assets:
        sys.exit("[ERROR] No hay poligonos subidos: corre --subir-poligonos.")
    for a in assets:
        try:
            ee.data.getAsset(a)
        except Exception:  # noqa: BLE001
            sys.exit(f"[ERROR] {a} aun no existe: espera a que la subida quede COMPLETED (--estado).")
    n_subidos = ee.FeatureCollection([ee.FeatureCollection(a) for a in assets]).flatten().size().getInfo()
    if n_subidos != len(pol):
        sys.exit(f"[ERROR] En GEE hay {n_subidos} proyectos y localmente {len(pol)}: repite --subir-poligonos.")

    crs_grilla = elegir_proyeccion()
    tabla_tesela, proy_mb, prefijo = construir_expresiones(crs_grilla, assets)
    man.update({"crs_grilla": crs_grilla, "celda_m": CELDA_M, "asset_mapbiomas": ASSET_MB,
                "prefijo_bandas": prefijo, "crs_mapbiomas": proy_mb, "carpeta_drive": CARPETA_DRIVE,
                "anios": [ANIO_INI, ANIO_FIN], "m_estable": M_ESTABLE})
    print(f"[GRILLA] {crs_grilla}, celdas de {CELDA_M} m | MapBiomas {proy_mb['crs']}")

    if cfg.piloto:
        # Tesela del Pacifico chocoano (Bajo Baudo / Medio Baudo)
        lista = [t for t in tes if t["bbox"][0] == -78.0 and t["bbox"][1] == 4.0] or tes[:1]
    else:
        lista = tes
    est = estados_tareas()
    lanzadas = 0
    for t in lista:
        previa = man["tareas"].get(t["id"])
        if previa and est.get(previa["tarea"], {}).get("state") in ("COMPLETED", "RUNNING", "READY"):
            continue
        tarea = ee.batch.Export.table.toDrive(
            collection=tabla_tesela(t["bbox"]), description=f"celdas_{t['id']}",
            folder=CARPETA_DRIVE, fileNamePrefix=f"celdas_{t['id']}", fileFormat="CSV",
            selectors=["k", "ha", "anio", "elev", "pend"])
        tarea.start()
        man["tareas"][t["id"]] = {"tarea": tarea.id, "bbox": t["bbox"],
                                  "lanzada": datetime.now(timezone.utc).isoformat()}
        lanzadas += 1
        guardar_manifiesto(ruta_man, man)
        time.sleep(0.5)
    print(f"[LANZAR] {lanzadas} tareas nuevas | total en manifiesto: {len(man['tareas'])} de {len(tes)} teselas")
    print(f"Las tablas quedan en Google Drive, carpeta '{CARPETA_DRIVE}'. Seguimiento: --estado")
    return 0


if __name__ == "__main__":
    sys.exit(main())
