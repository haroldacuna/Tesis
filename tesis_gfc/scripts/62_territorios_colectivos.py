"""
62_territorios_colectivos.py

Covariable de linea base para responder al comentario 3.8 del jurado:
proporcion del area municipal en territorios colectivos titulados
(resguardos indigenas y consejos comunitarios) hasta el anio de corte.

    TC_i = Area( Resguardos_i U Consejos_i )_{titulados <= CORTE} / Area municipal_i

Logica espacial
---------------
* Todo se calcula en EPSG:9377 (MAGNA-SIRGAS Origen Nacional), proyeccion
  de areas iguales razonable para todo el pais, la misma de los demas scripts.
* Se usa la UNION de polygonos y no la suma: resguardos y consejos pueden
  traslaparse entre si (y una misma capa puede traer ampliaciones como
  polygonos separados que se solapan con el original). Sumar areas contaria
  dos veces el traslape y podria dar TC > 1.
* Corte en 2012, el mismo anio del RUNAP usado para las areas protegidas:
  la covariable debe ser de linea base (anterior a la primera cohorte, 2013)
  para no condicionar en algo que el tratamiento pudo afectar. La titulacion
  posterior a 2012 se reporta aparte (tc_total_hoy) solo como diagnostico.
* El anio se toma del acto administrativo de cada polygono. Si una capa no
  trae fecha, el script se detiene y lista las columnas: no se inventa un
  anio. Con --sin-corte (todas las capas) o --sin-corte-capa resguardos (una
  sola) se fuerza la version contemporanea, que queda marcada en la columna
  `corte` de la salida y debe declararse en el texto.
* Ojo con la capa de consejos de datos abiertos: su fecha es la del ULTIMO
  acto administrativo (ULTIMA_FECHA_ACTO_ADMIN), no la del titulo original.
  Un consejo titulado antes de 2012 y aclarado despues queda fuera del corte:
  el error va en direccion conservadora y su magnitud se acota con
  tc_total_hoy, que no aplica corte.

Insumos (descargar del portal de datos abiertos de la ANT y guardar en
data/raw/territorios_colectivos/; se aceptan .gpkg, .geojson, .json, .shp o
.zip con un shapefile adentro):
    resguardos*.*   capa "Resguardo Indigena Formalizado"
    consejos*.*     capa "Consejo Comunitario Titulado"
    data/interim/municipios_clean.gpkg

Opcional (--poligonos): cruce de los polygonos de Karnik et al. (2025) con
los territorios, para el comentario 3.2 (que representan los polygonos).
    data/raw/proyectos_poligonos/*  (se toma el primer archivo vectorial)

Salidas:
    data/interim/territorios_colectivos_municipio.csv
    data/interim/diagnostics/tc_qc.csv
    data/interim/diagnostics/tc_poligonos_karnik.csv     (con --poligonos)

USO (PowerShell, desde tesis_gfc/):
    python scripts/62_territorios_colectivos.py
    python scripts/62_territorios_colectivos.py --poligonos
    python scripts/62_territorios_colectivos.py --campo-fecha-resguardos FECHA_ACTO --corte 2012
"""

from __future__ import annotations

import argparse
import re
import sys
import zipfile
from pathlib import Path

import numpy as np
import pandas as pd

try:
    import geopandas as gpd
    import shapely
except ImportError as e:  # entorno sin pila geoespacial
    sys.exit(f"Falta la pila geoespacial ({e}). Instala con: pip install geopandas pyogrio")

CRS_AREA = 9377
M2_POR_HA = 10_000
EXT_VECTOR = (".gpkg", ".geojson", ".json", ".shp", ".zip")
# Columnas candidatas para el anio del acto administrativo, en orden de preferencia
PATRON_FECHA = re.compile(r"fec|acto|resol|acuerdo|anio|ano_|year|f_", re.IGNORECASE)  # "fec": shapefile recorta ULTIMA_FECHA... a ULTIMA_FEC
PATRON_COD = ("COD_DANE", "MPIO_CDPMP", "CODIGO", "MPIO_CCNCT")


# ---------------------------------------------------------------------------
# Raiz canonica (mismo centinela que el resto del pipeline: evita escribir en
# la carpeta sombra scripts/data/)
# ---------------------------------------------------------------------------
def raiz_canonica() -> Path:
    d = Path.cwd().resolve()
    for p in (d, *d.parents):
        if (p / "data" / ".raiz_canonica").exists():
            return p
    sys.exit("No encuentro data/.raiz_canonica subiendo desde " + str(d) + ". Corre desde tesis_gfc/.")


# ---------------------------------------------------------------------------
# Lectura
# ---------------------------------------------------------------------------
def buscar_archivo(carpeta: Path, prefijo: str) -> Path:
    cands = sorted(p for p in carpeta.glob(f"{prefijo}*") if p.suffix.lower() in EXT_VECTOR)
    if not cands:
        sys.exit(f"No encuentro {carpeta / (prefijo + '*')} ({', '.join(EXT_VECTOR)}). "
                 f"Descarga la capa de la ANT y guardala con ese prefijo.")
    if len(cands) > 1:
        # No se elige uno en silencio: dos versiones de la misma capa pueden traer campos distintos
        sys.exit(f"Hay varios archivos para '{prefijo}' en {carpeta}: {[c.name for c in cands]}. "
                 f"Deja solo la version que se va a usar.")
    return cands[0]


def leer_vector(ruta: Path) -> gpd.GeoDataFrame:
    if ruta.suffix.lower() == ".zip":
        with zipfile.ZipFile(ruta) as z:
            shp = [n for n in z.namelist() if n.lower().endswith(".shp")]
        if not shp:
            sys.exit(f"{ruta.name} no contiene un .shp")
        gdf = gpd.read_file(f"zip://{ruta}!{shp[0]}")
    else:
        gdf = gpd.read_file(ruta)
    if gdf.empty:
        sys.exit(f"{ruta.name} se leyo vacio (0 filas). No se escriben salidas vacias.")
    if gdf.crs is None:
        print(f"Aviso: {ruta.name} sin CRS; se asume EPSG:4686 (MAGNA-SIRGAS geograficas, el de la ANT).")
        gdf = gdf.set_crs(4686)
    return gdf


def limpiar_geometria(gdf: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    """Reproyecta, repara y deja solo polygonos. Las capas oficiales traen
    autointersecciones que hacen fallar overlay() con TopologyException."""
    gdf = gdf.to_crs(CRS_AREA)
    gdf = gdf[gdf.geometry.notna() & ~gdf.geometry.is_empty].copy()
    gdf["geometry"] = shapely.make_valid(gdf.geometry.values)
    # make_valid puede devolver GeometryCollection con lineas sueltas: se conservan los polygonos
    gdf["geometry"] = gdf.geometry.apply(
        lambda g: shapely.union_all([p for p in getattr(g, "geoms", [g]) if p.geom_type in ("Polygon", "MultiPolygon")])
        if g.geom_type == "GeometryCollection" else g)
    gdf = gdf[gdf.geometry.geom_type.isin(["Polygon", "MultiPolygon"]) & ~gdf.geometry.is_empty]
    return gdf


# ---------------------------------------------------------------------------
# Anio del acto administrativo
# ---------------------------------------------------------------------------
def a_anio(serie: pd.Series) -> pd.Series:
    """Convierte una columna de fecha en anio. Maneja datetime, epoch en
    milisegundos (formato de los servicios ArcGIS), enteros de anio y texto
    tipo 'Resolucion 012 del 14/03/1995' o '1995-03-14'."""
    s = serie
    if pd.api.types.is_datetime64_any_dtype(s):
        return s.dt.year.astype("Float64")
    if pd.api.types.is_numeric_dtype(s):
        v = pd.to_numeric(s, errors="coerce")
        if v.dropna().between(1800, 2100).all():
            return v.astype("Float64")
        if v.dropna().abs().gt(1e10).all():  # epoch en ms (negativo antes de 1970)
            return pd.to_datetime(v, unit="ms", errors="coerce").dt.year.astype("Float64")
        return pd.Series(pd.NA, index=s.index, dtype="Float64")
    # texto: se toma el ULTIMO anio de 4 cifras plausible (en '012 de 1995' el anio va al final)
    anios = s.astype(str).str.findall(r"(?<!\d)(1[89]\d{2}|20\d{2})(?!\d)")
    return anios.apply(lambda a: float(a[-1]) if a else np.nan).astype("Float64")


def detectar_anio(gdf: gpd.GeoDataFrame, campo: str | None, nombre: str) -> pd.Series:
    if campo:
        if campo not in gdf.columns:
            sys.exit(f"[{nombre}] no existe la columna '{campo}'. Columnas: {list(gdf.columns)}")
        anio = a_anio(gdf[campo])
        print(f"[{nombre}] anio desde '{campo}' (indicado): {anio.notna().mean():.0%} con dato")
        return anio
    mejor, mejor_cob = None, 0.0
    for c in gdf.columns:
        if c == "geometry" or not PATRON_FECHA.search(c):
            continue
        a = a_anio(gdf[c])
        cob = a.notna().mean()
        # se exige que el anio sea plausible para titulacion colectiva en Colombia
        ok = a.dropna().between(1900, 2026).mean() if a.notna().any() else 0
        if cob > mejor_cob and ok > 0.95:
            mejor, mejor_cob = c, cob
    if mejor is None:
        sys.exit(f"[{nombre}] no encontre una columna de fecha del acto administrativo.\n"
                 f"  Columnas: {list(gdf.columns)}\n"
                 f"  Indicala con --campo-fecha-{nombre}, o usa --sin-corte-capa {nombre} (version contemporanea, "
                 f"que NO es de linea base y debe declararse asi).")
    anio = a_anio(gdf[mejor])
    print(f"[{nombre}] anio detectado en '{mejor}': {mejor_cob:.0%} de los polygonos con dato | "
          f"rango {int(anio.min())}-{int(anio.max())}")
    return anio


# ---------------------------------------------------------------------------
# Area de la union por municipio
# ---------------------------------------------------------------------------
def area_union_por_municipio(capa: gpd.GeoDataFrame, mpios: gpd.GeoDataFrame) -> pd.Series:
    """Intersecta cada polygono con los municipios y une DENTRO de cada
    municipio antes de medir, para no contar dos veces los traslapes."""
    if capa.empty:
        return pd.Series(0.0, index=mpios["COD_DANE"].values)
    inter = gpd.overlay(capa[["geometry"]], mpios[["COD_DANE", "geometry"]],
                        how="intersection", keep_geom_type=True)
    if inter.empty:
        return pd.Series(0.0, index=mpios["COD_DANE"].values)
    area = inter.groupby("COD_DANE").geometry.apply(lambda g: shapely.union_all(g.values).area) / M2_POR_HA
    return area.reindex(mpios["COD_DANE"].values, fill_value=0.0)


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--corte", type=int, default=2012, help="ultimo anio de titulacion incluido (default 2012)")
    ap.add_argument("--campo-fecha-resguardos", default=None)
    ap.add_argument("--campo-fecha-consejos", default=None)
    ap.add_argument("--sin-corte", action="store_true", help="ignora la fecha en todas las capas")
    ap.add_argument("--sin-corte-capa", action="append", default=[], choices=["resguardos", "consejos"],
                    help="ignora la fecha solo en esa capa (repetible)")
    ap.add_argument("--poligonos", action="store_true", help="cruce con los polygonos de Karnik (comentario 3.2)")
    args = ap.parse_args()

    raiz = raiz_canonica()
    d_raw = raiz / "data" / "raw" / "territorios_colectivos"
    d_int = raiz / "data" / "interim"
    d_diag = d_int / "diagnostics"
    d_diag.mkdir(parents=True, exist_ok=True)

    # --- Municipios -------------------------------------------------------
    mpios = limpiar_geometria(leer_vector(d_int / "municipios_clean.gpkg"))
    col = next((c for c in PATRON_COD if c in mpios.columns), None)
    if col is None:
        sys.exit(f"municipios_clean.gpkg sin columna de codigo; columnas: {list(mpios.columns)}")
    mpios["COD_DANE"] = mpios[col].astype(str).str.zfill(5)
    mpios = mpios.dissolve("COD_DANE", as_index=False)  # por si un municipio viene en varias filas
    if len(mpios) != 1122:
        print(f"Aviso: {len(mpios)} municipios (se esperaban 1.122 del panel).")
    mpios["area_mun_ha"] = mpios.geometry.area / M2_POR_HA

    # --- Territorios ------------------------------------------------------
    capas, diag = {}, []
    for nombre, campo in (("resguardos", args.campo_fecha_resguardos), ("consejos", args.campo_fecha_consejos)):
        ruta = buscar_archivo(d_raw, nombre)
        g = limpiar_geometria(leer_vector(ruta))
        if args.sin_corte or nombre in args.sin_corte_capa:
            g["anio_titulo"] = pd.NA
            g["sin_corte"] = True
            print(f"[{nombre}] SIN corte de fecha: version contemporanea (declararlo en el texto)")
        else:
            g["anio_titulo"] = detectar_anio(g, campo, nombre).values
        n_sin = int(g["anio_titulo"].isna().sum())
        n_post = int((g["anio_titulo"] > args.corte).sum())
        print(f"[{nombre}] {len(g)} polygonos | {g.geometry.area.sum() / M2_POR_HA / 1e6:.1f} Mha "
              f"(suma, con traslapes) | sin fecha: {n_sin} | titulados despues de {args.corte}: {n_post}")
        diag.append({"capa": nombre, "archivo": ruta.name, "polygonos": len(g), "sin_fecha": n_sin,
                     f"post_{args.corte}": n_post})
        capas[nombre] = g

    def hasta_corte(g: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
        # Sin fecha => FUERA de la version de linea base (conservador: no se
        # imputa un titulo anterior al tratamiento sin evidencia)
        if args.sin_corte or "sin_corte" in g.columns:
            return g
        return g[g["anio_titulo"].notna() & (g["anio_titulo"] <= args.corte)]

    res = mpios[["COD_DANE", "area_mun_ha"]].set_index("COD_DANE").copy()
    res["ha_resguardos"] = area_union_por_municipio(hasta_corte(capas["resguardos"]), mpios).values
    res["ha_consejos"] = area_union_por_municipio(hasta_corte(capas["consejos"]), mpios).values
    ambos = pd.concat([hasta_corte(capas["resguardos"])[["geometry"]],
                       hasta_corte(capas["consejos"])[["geometry"]]], ignore_index=True)
    res["ha_total"] = area_union_por_municipio(gpd.GeoDataFrame(ambos, crs=CRS_AREA), mpios).values
    todos = pd.concat([capas["resguardos"][["geometry"]], capas["consejos"][["geometry"]]], ignore_index=True)
    res["ha_total_hoy"] = area_union_por_municipio(gpd.GeoDataFrame(todos, crs=CRS_AREA), mpios).values
    # Version sin corte por capa: con fechas de ULTIMO acto, el corte saca resguardos
    # constituidos hace decadas y ampliados o aclarados despues (p. ej. el Gran
    # Resguardo del Vaupes, ultimo acto 2024). Por eso se guardan ambas versiones.
    res["ha_resguardos_hoy"] = area_union_por_municipio(capas["resguardos"], mpios).values
    res["ha_consejos_hoy"] = area_union_por_municipio(capas["consejos"], mpios).values

    for c in ("resguardos", "consejos", "total", "total_hoy", "resguardos_hoy", "consejos_hoy"):
        res[f"tc_{c}"] = res[f"ha_{c}"] / res["area_mun_ha"]
    # Desborde por redondeo de bordes (capas ANT vs DANE no son topologicamente
    # consistentes): se reporta y se trunca solo si es menor a 1 %
    tc_cols = [c for c in res.columns if c.startswith("tc_")]
    desb = res[tc_cols].max(axis=1)
    if (desb > 1.01).any():
        print("Aviso: municipios con TC > 1,01 (revisar):\n", res.loc[desb > 1.01, tc_cols].round(3))
    res[tc_cols] = res[tc_cols].clip(upper=1.0)
    res["traslape_ha"] = res["ha_resguardos"] + res["ha_consejos"] - res["ha_total"]
    sin = ["resguardos", "consejos"] if args.sin_corte else sorted(set(args.sin_corte_capa))
    res["corte"] = (str(args.corte) + (" (sin corte: " + ", ".join(sin) + ")" if sin else "")) if len(sin) < 2 else "contemporaneo"

    # --- QC ---------------------------------------------------------------
    if res["ha_total"].sum() == 0:
        sys.exit("TC total = 0 en todos los municipios: revisa la ruta de las capas o el corte.")
    print(f"\nMunicipios con TC > 0: {(res.tc_total > 0).sum()} | con TC >= 0,10: {(res.tc_total >= 0.10).sum()}")
    print(f"Area titulada a {res.corte.iloc[0]}: {res.ha_total.sum() / 1e6:.1f} Mha | "
          f"hoy: {res.ha_total_hoy.sum() / 1e6:.1f} Mha | traslape resguardo-consejo: {res.traslape_ha.sum():,.0f} ha")
    print("Mayor TC:\n", res.sort_values("tc_total", ascending=False).head(8)[["tc_resguardos", "tc_consejos", "tc_total"]].round(3))

    salida = d_int / "territorios_colectivos_municipio.csv"
    res.reset_index().to_csv(salida, index=False, encoding="utf-8")
    pd.DataFrame(diag).to_csv(d_diag / "tc_qc.csv", index=False, encoding="utf-8")
    print(f"\nGuardado: {salida.relative_to(raiz)} ({len(res)} filas)")

    # --- Comentario 3.2: que representan los polygonos de Karnik -----------
    if args.poligonos:
        d_pol = raiz / "data" / "raw" / "proyectos_poligonos"
        arch = sorted(p for p in d_pol.glob("*") if p.suffix.lower() in EXT_VECTOR)
        if not arch:
            print(f"--poligonos: no hay archivos vectoriales en {d_pol}; se omite.")
            return
        pol = limpiar_geometria(leer_vector(arch[0])).reset_index(drop=True)
        # La base de Karnik cubre Suramerica: se dejan solo los polygonos con mas
        # de la mitad de su area dentro de Colombia (union de los municipios)
        col_union = shapely.union_all(mpios.geometry.values)
        dentro_col = pol.geometry.intersection(col_union).area / pol.geometry.area
        print(f"Polygonos de Karnik: {len(pol)} en el archivo | {(dentro_col > 0.5).sum()} en Colombia")
        pol = pol[dentro_col > 0.5].reset_index(drop=True)
        id_col = next((c for c in pol.columns if re.search(r"^(id|project_id|name|nombre)", c, re.I)), None)
        pol["id_pol"] = pol[id_col].astype(str) if id_col else pol.index.astype(str)
        terr = pd.concat([capas["resguardos"].assign(tipo="resguardo"),
                          capas["consejos"].assign(tipo="consejo")], ignore_index=True)[["tipo", "geometry"]]
        terr = gpd.GeoDataFrame(terr, crs=CRS_AREA).reset_index(names="id_terr")
        terr["area_terr"] = terr.geometry.area
        filas = []
        for _, p in pol.iterrows():
            a_pol = p.geometry.area
            cand = terr[terr.intersects(p.geometry)]
            if cand.empty:
                filas.append({"id_pol": p.id_pol, "area_pol_ha": a_pol / M2_POR_HA, "pct_pol_en_territorio": 0.0,
                              "tipo_principal": "ninguno", "pct_territorio_cubierto": np.nan})
                continue
            inter_area = cand.geometry.intersection(p.geometry).area
            dentro = shapely.union_all(cand.geometry.intersection(p.geometry).values).area
            k = inter_area.idxmax()   # territorio con mayor traslape
            filas.append({"id_pol": p.id_pol, "area_pol_ha": a_pol / M2_POR_HA,
                          "pct_pol_en_territorio": 100 * dentro / a_pol,
                          "tipo_principal": cand.loc[k, "tipo"],
                          "pct_territorio_cubierto": 100 * inter_area[k] / cand.loc[k, "area_terr"],
                          "n_territorios": len(cand)})
        out = pd.DataFrame(filas)
        out.to_csv(d_diag / "tc_poligonos_karnik.csv", index=False, encoding="utf-8")
        print(f"\n--- Polygonos de Karnik ({arch[0].name}, {len(out)}) ---")
        print(out[["pct_pol_en_territorio", "pct_territorio_cubierto"]].describe().round(1))
        n = len(out)
        print(f"Con >= 80 % de su area en territorios colectivos: {(out.pct_pol_en_territorio >= 80).sum()} de {n} | "
              f"fuera de territorios (< 20 %): {(out.pct_pol_en_territorio < 20).sum()}")
        en_t = out[out.pct_pol_en_territorio >= 20]
        print(f"De los {len(en_t)} que estan en territorios: cubren >= 80 % del territorio principal: "
              f"{(en_t.pct_territorio_cubierto >= 80).sum()} | < 50 %: {(en_t.pct_territorio_cubierto < 50).sum()}")
        print(f"Por tipo de territorio principal: {out.tipo_principal.value_counts().to_dict()}")
        print("Lectura: pct_territorio_cubierto cerca de 100 => el polygono es el territorio completo; "
              "valores bajos => area delimitada para conservacion dentro del territorio.")


if __name__ == "__main__":
    main()
