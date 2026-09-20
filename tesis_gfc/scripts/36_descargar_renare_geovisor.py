#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
36_descargar_renare_geovisor.py
===============================

Descarga la capa de POLIGONOS de iniciativas RENARE del geovisor del IDEAM
y les asigna municipio por interseccion espacial.

POR QUE IMPORTA
---------------
El raspado de la plataforma RENARE (16_extraer_renare.py) da 281 registros
pero sin ubicacion: el municipio se venia infiriendo por matching de texto
libre contra nombres de municipio, con 27 aciertos unicos de 281. El resto
quedaba fuera del tratamiento.

El geovisor del IDEAM SI publica la geometria:

    https://visualizador.ideam.gov.co/gisserver/rest/services/Capas_Ideam/FeatureServer/1
    "Iniciativas de Mitigacion de GEI - Fase de Implementacion en RENARE"

Con el poligono, el municipio deja de inferirse y se calcula. Y como es
poligono y no punto, tambien da el AREA por municipio, que es el insumo de
las medidas de intensidad:

    int_area_i   = area(union de poligonos ∩ municipio_i) / area(municipio_i)
    int_bosque_i = area(union ∩ municipio_i ∩ bosque_2000_i) / area(bosque_2000_i)

Se usa la UNION y no la suma para que proyectos superpuestos no den > 1.

LO QUE LA CAPA APORTA ADEMAS DE LA GEOMETRIA
--------------------------------------------
    tp_inic_mitg        'REDD+ Programa' / 'PDBC Proyecto' / 'MDL' / 'MDL-PoA'
                        -> eje REDD por dato del registrador
    tp_act              'Reduccion de las emisiones debidas a la deforestacion',
                        'Plantaciones forestales', ...  -> mecanismo
    fec_inio_act        inicio de la actividad (epoch en milisegundos)
    estad_inic          ACTIVO / ARCHIVADO -> escalera de estado
    reduc_verif_tco2e   reducciones verificadas -> activacion del mecanismo

LIMITACION CONOCIDA
-------------------
La capa es SOLO fase de implementacion. Las iniciativas en factibilidad o
formulacion no estan, asi que sigue habiendo cobertura parcial. Pero
implementacion es justamente la fase donde el proyecto opera, que es la
relevante para el tratamiento efectivo.

ENCODING
--------
El servicio devuelve los textos con U+FFFD en lugar de los acentos: la
perdida ocurre en el servidor, no al descargar, asi que no hay forma de
recuperarlos. Por eso el empate por nombre contra los registros raspados usa
similitud y no igualdad, y se prefiere empatar por 'cod' cuando se puede.

USO
---
    python 36_descargar_renare_geovisor.py --explorar   # cuantas hay y de que tipo
    python 36_descargar_renare_geovisor.py              # descarga + cruce
    python 36_descargar_renare_geovisor.py --sin-verificar-ssl   # proxy corporativo

SALIDAS
-------
    data/raw/renare_geovisor_iniciativas.geojson
    data/raw/renare_geovisor_iniciativas.fuente.json
    data/interim/renare_geovisor_municipios.csv
        Una fila por (iniciativa, municipio) con area intersectada e
        intensidad sobre el area municipal.
    data/interim/diagnostics/renare_geovisor_vs_raspado.csv
        Empate contra los 281 registros raspados, con metodo y score.
"""

from __future__ import annotations

import argparse
import difflib
import json
import re
import sys
import unicodedata
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

try:
    import geopandas as gpd
except ImportError:
    gpd = None

try:
    import requests
except ImportError:
    requests = None

SCRIPT_DIR = Path(__file__).resolve().parent
CENTINELA = "data/.raiz_canonica"

URL_CAPA = ("https://visualizador.ideam.gov.co/gisserver/rest/services/"
            "Capas_Ideam/FeatureServer/1/query")

# EPSG:4686 es MAGNA-SIRGAS geografico (grados). Para medir areas hay que
# proyectar: 9377 es el origen nacional unico CTM12, en metros.
CRS_ORIGEN = "EPSG:4686"
CRS_METRICO = "EPSG:9377"


def resolver_raiz(forzada: str | None = None) -> Path:
    if forzada:
        r = Path(forzada).resolve()
        if not (r / CENTINELA).exists():
            raise SystemExit(f"[X] {r} no tiene {CENTINELA}.")
        return r
    cands = []
    for base in (SCRIPT_DIR, Path.cwd().resolve()):
        d = base
        while True:
            if (d / CENTINELA).exists():
                cands.append(str(d))
                break
            if d.parent == d:
                break
            d = d.parent
    u = sorted(set(cands))
    if len(u) == 1:
        return Path(u[0])
    if not u:
        raise SystemExit(f"[X] No encuentro {CENTINELA} desde {SCRIPT_DIR} ni {Path.cwd()}.")
    raise SystemExit("[X] Dos raices canonicas:\n    " + "\n    ".join(u))


def normalizar(t) -> str:
    """Minusculas, sin acentos, sin puntuacion. Ademas quita el caracter de
    reemplazo U+FFFD que el servicio devuelve donde iban las tildes, para que
    'mitigaci<FFFD>n' y 'mitigacion' queden a distancia corta."""
    if pd.isna(t):
        return ""
    t = str(t).replace("\ufffd", "")
    t = unicodedata.normalize("NFKD", t)
    t = "".join(c for c in t if not unicodedata.combining(c)).lower()
    t = re.sub(r"[^a-z0-9\s]", " ", t)
    return re.sub(r"\s+", " ", t).strip()


def descargar(verificar_ssl: bool = True) -> dict:
    """Descarga todas las features paginando. maxRecordCount del servicio es
    5000, pero se pagina igual por si cambia."""
    if requests is None:
        raise SystemExit("[X] Falta requests: pip install requests")

    features, offset, pagina = [], 0, 2000
    while True:
        params = {
            "where": "1=1",
            "outFields": "*",
            "returnGeometry": "true",
            "outSR": "4686",
            "f": "geojson",
            "resultOffset": offset,
            "resultRecordCount": pagina,
        }
        print(f"  descargando desde offset {offset}...")
        r = requests.get(URL_CAPA, params=params, timeout=120, verify=verificar_ssl)
        r.raise_for_status()
        datos = r.json()
        lote = datos.get("features", [])
        features.extend(lote)
        if len(lote) < pagina:
            break
        offset += pagina
        if offset > 100_000:
            print("  [!] corte de seguridad a 100.000 features")
            break

    print(f"  total descargado: {len(features)} features")
    return {"type": "FeatureCollection", "features": features}


def explorar(fc: dict) -> None:
    filas = [f.get("properties", {}) for f in fc["features"]]
    d = pd.DataFrame(filas)
    print(f"\n{len(d)} iniciativas en la capa\n")
    for c in ["tp_inic_mitg", "tp_act", "estad_inic", "fas_reg"]:
        if c in d.columns:
            print(f"--- {c}")
            print(d[c].value_counts(dropna=False).head(15).to_string())
            print()
    if "reduc_verif_tco2e" in d.columns:
        v = pd.to_numeric(d["reduc_verif_tco2e"], errors="coerce").fillna(0)
        print(f"Con reducciones verificadas > 0: {(v > 0).sum()} de {len(v)}")
        print(f"Total verificado: {v.sum():,.0f} tCO2e")

    # Las de mecanismo REDD son las que interesan para D1
    if "tp_inic_mitg" in d.columns:
        redd = d[d["tp_inic_mitg"].map(normalizar).str.contains("redd", na=False)]
        print(f"\nIniciativas REDD+ en la capa: {len(redd)}")
        cols = [c for c in ["nom_inic_mitg", "nom_tit", "estad_inic", "reduc_verif_tco2e"]
                if c in redd.columns]
        if len(redd):
            print(redd[cols].to_string(index=False, max_colwidth=50))


def epoch_a_anio(v) -> int | None:
    """La capa entrega las fechas como milisegundos desde 1970."""
    if pd.isna(v):
        return None
    try:
        return datetime.fromtimestamp(float(v) / 1000, tz=timezone.utc).year
    except (ValueError, OSError, OverflowError):
        return None


def cruzar_municipios(fc: dict, raiz: Path) -> pd.DataFrame:
    """Interseccion espacial iniciativa x municipio.

    Devuelve una fila por par (iniciativa, municipio) con el area
    intersectada en hectareas y su proporcion sobre el area municipal. Un
    proyecto que abarca varios municipios genera varias filas, que es
    exactamente lo que el indicador binario no podia representar.
    """
    if gpd is None:
        raise SystemExit("[X] Falta geopandas.")

    mun_path = raiz / "data/interim/municipios_clean.gpkg"
    if not mun_path.exists():
        raise SystemExit(f"[X] Falta {mun_path}. Corre 01_prepare_boundaries.py")

    ini = gpd.GeoDataFrame.from_features(fc["features"], crs=CRS_ORIGEN)
    ini = ini[ini.geometry.notna() & ~ini.geometry.is_empty].copy()
    # Los poligonos de origen pueden venir con autointersecciones; buffer(0)
    # las repara y evita que overlay falle a mitad del cruce.
    ini["geometry"] = ini.geometry.buffer(0)
    ini = ini.to_crs(CRS_METRICO)

    mun = gpd.read_file(mun_path, layer="municipios_clean").to_crs(CRS_METRICO)
    col_dane = next((c for c in mun.columns
                     if re.search(r"(?i)cod.*dane|mpio.*cd|^cod", str(c))), None)
    if col_dane is None:
        raise SystemExit(f"[X] No encuentro columna DANE en {list(mun.columns)}")
    mun["COD_DANE"] = mun[col_dane].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5)
    mun["area_mun_ha"] = mun.geometry.area / 10_000
    col_nom_mun = next((c for c in mun.columns if re.search(r"(?i)nombre|mpio_cnmbr", str(c))), None)

    cols_mun = ["COD_DANE", "area_mun_ha", "geometry"] + ([col_nom_mun] if col_nom_mun else [])
    print(f"\n  intersectando {len(ini)} iniciativas con {len(mun)} municipios...")
    inter = gpd.overlay(ini, mun[cols_mun], how="intersection", keep_geom_type=False)

    inter["area_inter_ha"] = inter.geometry.area / 10_000
    inter["int_area_mun"] = inter["area_inter_ha"] / inter["area_mun_ha"]
    if "fec_inio_act" in inter.columns:
        inter["anio_inicio"] = inter["fec_inio_act"].map(epoch_a_anio)
    if "fec_fin_act" in inter.columns:
        inter["anio_fin"] = inter["fec_fin_act"].map(epoch_a_anio)

    # Descartar rebabas: intersecciones de menos de una hectarea suelen ser
    # artefactos del borde municipal, no presencia real del proyecto.
    n_antes = len(inter)
    inter = inter[inter["area_inter_ha"] >= 1.0].copy()
    if n_antes != len(inter):
        print(f"  descartadas {n_antes - len(inter)} intersecciones de menos de 1 ha (rebabas de borde)")

    print(f"  {len(inter)} pares (iniciativa, municipio)")
    print(f"  municipios tocados: {inter['COD_DANE'].nunique()}")
    return pd.DataFrame(inter.drop(columns="geometry"))


def empatar_con_raspado(inter: pd.DataFrame, raiz: Path) -> pd.DataFrame:
    """Empata la capa con los 281 registros raspados de la plataforma.

    Primero por 'cod' contra '_id' (si coinciden), luego por nombre
    normalizado exacto, luego por similitud. Todo se reporta: el objetivo es
    saber cuantas de las 281 gana ubicacion, no maquillar la tasa.
    """
    raspado_path = raiz / "data/interim/renare_solicitudes_colombia.csv"
    if not raspado_path.exists():
        print(f"\n[ ] Falta {raspado_path}; se omite el empate.")
        return pd.DataFrame()

    raspado = pd.read_csv(raspado_path, low_memory=False)
    raspado["_nom"] = raspado["nombre_iniciativa"].map(normalizar)

    capa = inter.drop_duplicates(subset=["cod"]) if "cod" in inter.columns else inter
    capa = capa.copy()
    capa["_nom"] = capa["nom_inic_mitg"].map(normalizar)

    # 1. por codigo
    por_cod = 0
    if "cod" in capa.columns and "_id" in raspado.columns:
        ids = set(pd.to_numeric(raspado["_id"], errors="coerce").dropna().astype(int))
        cods = set(pd.to_numeric(capa["cod"], errors="coerce").dropna().astype(int))
        por_cod = len(ids & cods)
        print(f"\n  empate por codigo (cod x _id): {por_cod} coincidencias")

    # 2. y 3. por nombre
    universo = [n for n in raspado["_nom"].tolist() if n]
    filas = []
    for _, r in capa.iterrows():
        n = r["_nom"]
        metodo, score, par = "sin_match", None, None
        if n and n in universo:
            metodo, score, par = "nombre_exacto", 1.0, n
        elif n:
            c = difflib.get_close_matches(n, universo, n=1, cutoff=0.85)
            if c:
                metodo = "nombre_similitud"
                score = round(difflib.SequenceMatcher(None, n, c[0]).ratio(), 3)
                par = c[0]
        filas.append({
            "cod": r.get("cod"),
            "nom_capa": r.get("nom_inic_mitg"),
            "tp_inic_mitg": r.get("tp_inic_mitg"),
            "tp_act": r.get("tp_act"),
            "estad_inic": r.get("estad_inic"),
            "reduc_verif_tco2e": r.get("reduc_verif_tco2e"),
            "metodo_match": metodo,
            "score_match": score,
            "nom_raspado_normalizado": par,
        })

    res = pd.DataFrame(filas)
    print(res["metodo_match"].value_counts().to_string())
    print(f"\n  iniciativas de la capa sin contraparte en el raspado: "
          f"{int((res.metodo_match == 'sin_match').sum())}")
    print("  (pueden ser iniciativas que el raspado no capturo: la brecha "
          "va en los dos sentidos)")
    return res


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--explorar", action="store_true")
    ap.add_argument("--raiz", default=None)
    ap.add_argument("--sin-verificar-ssl", action="store_true",
                    help="para proxies con inspeccion SSL; usa con cuidado")
    ap.add_argument("--usar-cache", action="store_true",
                    help="no descargar, leer el geojson ya guardado")
    args = ap.parse_args()

    raiz = resolver_raiz(args.raiz)
    print(f"Raiz canonica: {raiz}")

    geo_path = raiz / "data/raw/renare_geovisor_iniciativas.geojson"
    meta_path = raiz / "data/raw/renare_geovisor_iniciativas.fuente.json"
    out_inter = raiz / "data/interim/renare_geovisor_municipios.csv"
    diag = raiz / "data/interim/diagnostics"

    if args.usar_cache and geo_path.exists():
        print(f"Leyendo cache: {geo_path}")
        fc = json.loads(geo_path.read_text(encoding="utf-8"))
    else:
        print(f"Descargando {URL_CAPA}")
        fc = descargar(verificar_ssl=not args.sin_verificar_ssl)
        geo_path.parent.mkdir(parents=True, exist_ok=True)
        geo_path.write_text(json.dumps(fc, ensure_ascii=False), encoding="utf-8")
        meta_path.write_text(json.dumps({
            "url": URL_CAPA,
            "capa": "Iniciativas de Mitigacion de GEI - Fase de Implementacion en RENARE",
            "servicio": "visualizador.ideam.gov.co/gisserver Capas_Ideam (FeatureServer), capa 1",
            "fecha_descarga_utc": datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC"),
            "n_features": len(fc["features"]),
            "crs": CRS_ORIGEN,
            "nota": "Solo fase de implementacion; factibilidad y formulacion no estan en la capa",
        }, indent=2, ensure_ascii=True), encoding="utf-8")
        print(f"Guardado: {geo_path}")

    if args.explorar:
        explorar(fc)
        return 0

    explorar(fc)
    inter = cruzar_municipios(fc, raiz)
    out_inter.parent.mkdir(parents=True, exist_ok=True)
    inter.to_csv(out_inter, index=False, encoding="utf-8-sig")
    print(f"\nEscrito: {out_inter}")

    res = empatar_con_raspado(inter, raiz)
    if len(res):
        diag.mkdir(parents=True, exist_ok=True)
        res.to_csv(diag / "renare_geovisor_vs_raspado.csv", index=False, encoding="utf-8-sig")
        print(f"Escrito: {diag / 'renare_geovisor_vs_raspado.csv'}")

    print("""
SIGUIENTE PASO
  1. Revisa renare_geovisor_municipios.csv: una fila por (iniciativa,
     municipio) con area e intensidad. Compara los municipios que salen
     contra los 27 que daba el matching por texto.
  2. Decide como entra el Programa Vision Amazonia, que es REDD+
     jurisdiccional y cubre decenas de municipios: no es comparable con un
     proyecto y conviene tratarlo como especificacion aparte.
  3. Para la medida (ii) falta regenerar baseline_forest.csv desde
     treecover2000 de Hansen; con eso el mismo cruce da la intensidad
     sobre area de bosque.
""")
    return 0


if __name__ == "__main__":
    sys.exit(main())
