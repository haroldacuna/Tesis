#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
65_territorios_proponentes.py  (tarea 1.7, Tabla 4.6; JM oct-2026)
===================================================================

DESCRIPTIVO, sin estimacion (Jorge: "dejarlo en lo descriptivo y como posible
extension"). Para cada proyecto REDD+ / deforestacion evitada:
  * tipo de territorio: resguardo indigena, consejo comunitario (tierras de
    comunidades negras), mixto u otro;
  * tipo de proponente: comunitario, empresa, ONG, publico (o combinacion),
    clasificado por palabras clave y para REVISION MANUAL;
  * area, estado y creditos emitidos.
Y para cada municipio tratado de D1 depurada, el % de su area en resguardos y
en consejos comunitarios.

Proyectos incluidos:
  * Verra REDD+ (metodologias VM0006/7/9/15 o AFOLU REDD), sin exclusiones D2,
    con su punto (Platts) y, si existe, su poligono de Karnik et al.
  * Poligonos de deforestacion evitada de Karnik et al. en Colombia que no son
    de Verra (EcoRegistry, BioCarbon): proponente = "Project Developer Name".

Territorio de un proyecto:
  con poligono -> % del area del proyecto en cada tipo; tipo dominante si >= 50 %,
                  "mixto" si resguardo + consejo >= 50 % sin que ninguno llegue,
                  "otro" en el resto;
  sin poligono -> tipo del territorio que contiene el punto (aproximacion).

Insumos:
  data/raw/ant/*.shp|*.gpkg|*.geojson  (resguardos y tierras de comunidades negras)
  data/interim/verra_platts_colombia_con_municipio.csv
  data/interim/berkeley_vrod_projects_colombia.csv     (creditos emitidos)
  data/raw/proyectos_poligonos/south_america.gpkg       (Karnik et al.)
  data/interim/municipios_clean.gpkg, data/final/panel_con_tratamiento_actualizado.csv
Salidas (data/interim/diagnostics/):
  65_proyectos_territorio_proponente.csv   (una fila por proyecto; revisar 'tipo_proponente')
  65_municipios_tratados_territorio.csv
  65_tabla_4_6.csv
USO (desde tesis_gfc/):  python scripts\\65_territorios_proponentes.py
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

import geopandas as gpd
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent))
from filtro_sectorial import EXCLUSIONES  # noqa: E402

ANT = Path("data/raw/ant")
VERRA = Path("data/interim/verra_platts_colombia_con_municipio.csv")
BERKELEY = Path("data/interim/berkeley_vrod_projects_colombia.csv")
KARNIK = Path("data/raw/proyectos_poligonos/south_america.gpkg")
GPKG_MUN = Path("data/interim/municipios_clean.gpkg")
PANEL = Path("data/final/panel_con_tratamiento_actualizado.csv")
DIAG = Path("data/interim/diagnostics")
METOD_REDD = re.compile(r"VM0006|VM0007|VM0009|VM0015", re.I)
CRS_M = 9377

## Clasificacion del proponente por palabras clave (revision manual obligatoria)
REGLAS = [
    ("comunitario", r"consejo comunitario|community council|cabildo|resguardo|asociaci[oó]n|association|"
                    r"comunidad|community|ind[ií]gena|indigenous|traditional authorit|afro|cocomacia|acapa|"
                    r"pueblo|people|territorio colectivo|mayor|asocabildos"),
    ("publico", r"corporaci[oó]n aut[oó]noma|ministerio|ministry|gobernaci[oó]n|alcald[ií]a|"
                r"parques nacionales|\bcar\b|corpo(amazonia|chivor|nari[nñ]o|cauca|uraba)|instituto"),
    ## Masbosques: corporacion sin animo de lucro (no publica, no empresa)
    ("ong", r"fundaci[oó]n|foundation|\bwwf\b|conservation international|conservaci[oó]n|wildlife conservation|"
            r"natura\b|\bong\b|trust|society|mas.?bosque"),
    ("empresa", r"\bs\.?\s?a\.?\s?s\.?\b|\bs\.?\s?a\.?\b|ltda|\binc\b|\bllc\b|gmbh|\bltd\b|limited|"
                r"carbon|capital|ventures|partners|holding|\bcorp\b|company|\bag\b|\bbv\b|sociedad|"
                r"co2cero|karbon|greenlife|consultor|brokers|commodities|anthrotect|biofix|south pole|"
                r"wildlife works|medio ambiente sostenible|terra"),
]
SIN_NOMBRE = re.compile(r"^\s*multiple project proponents\s*$", re.I)


def clasificar_proponente(txt: str) -> str:
    if not isinstance(txt, str) or not txt.strip():
        return "sin dato"
    if SIN_NOMBRE.match(txt):
        ## Verra no publica los nombres; no se adivina el tipo
        return "multiples (sin detalle)"
    t = txt.lower()
    tipos = [k for k, pat in REGLAS if re.search(pat, t)]
    return "+".join(tipos) if tipos else "sin clasificar"


def pad5(s):
    return s.astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5)


def cargar_ant() -> tuple[gpd.GeoDataFrame, gpd.GeoDataFrame]:
    capas = {"resguardo": [], "consejo": []}
    for f in sorted(p for p in ANT.rglob("*") if p.suffix.lower() in {".shp", ".gpkg", ".geojson"}):
        g = gpd.read_file(f)
        texto = (f.stem + " " + " ".join(map(str, g.columns))).lower()
        muestra = " ".join(g.drop(columns="geometry").astype(str).head(20).to_numpy().ravel()).lower()
        if "resguard" in texto or "resguard" in muestra:
            tipo = "resguardo"
        elif any(k in texto + " " + muestra for k in ("negr", "consejo", "comunidades", "afro", "colectiv")):
            tipo = "consejo"
        else:
            print(f"[!] {f.name}: no reconozco si es resguardo o consejo comunitario; se omite. "
                  f"Columnas: {list(g.columns)[:12]}")
            continue
        print(f"ANT: {f.name} -> {tipo} ({len(g)} poligonos, CRS {g.crs})")
        capas[tipo].append(g.to_crs(CRS_M)[["geometry"]])
    if not capas["resguardo"] or not capas["consejo"]:
        sys.exit(f"Faltan capas en {ANT}: necesito resguardos y tierras de comunidades negras.")
    union = {}
    for tipo, lista in capas.items():
        g = pd.concat(lista, ignore_index=True)
        g["geometry"] = g.geometry.make_valid()
        union[tipo] = gpd.GeoDataFrame(geometry=[g.geometry.union_all()], crs=CRS_M)
    return union["resguardo"], union["consejo"]


def pct_en(geoms: gpd.GeoDataFrame, capa: gpd.GeoDataFrame, clave: str) -> pd.Series:
    """% del area de cada geometria que cae dentro de la capa (ya disuelta en una
    sola geometria). Interseccion directa en lugar de overlay: tolera las
    colecciones mixtas que deja make_valid()."""
    union = capa.geometry.iloc[0]
    a = geoms.geometry.area
    ai = geoms.geometry.intersection(union).area
    return (100 * ai / a).clip(upper=100).fillna(0.0).values


def emitidos_berkeley() -> pd.Series:
    if not BERKELEY.exists():
        print(f"[!] No encuentro {BERKELEY}: creditos emitidos en blanco"); return pd.Series(dtype=float)
    b = pd.read_csv(BERKELEY, dtype=str)
    ## Los encabezados de Berkeley traen saltos de linea ("Total Credits\nIssued"):
    ## re.S hace que el punto tambien los cruce.
    cid = next(c for c in b.columns if re.search(r"project.?id", c, re.I | re.S))
    ciss = next(c for c in b.columns if re.search(r"total.*issued", c, re.I | re.S))
    b = b[b[cid].str.upper().str.startswith("VCS", na=False)]
    v = pd.to_numeric(b[ciss].str.replace(r"[^0-9.]", "", regex=True), errors="coerce")
    return pd.Series(v.values, index=b[cid].str.replace(r"\D", "", regex=True)).groupby(level=0).max()


def main() -> int:
    if not Path("data/.raiz_canonica").exists():
        sys.exit("Corre desde tesis_gfc/ (raiz canonica).")
    resg, cons = cargar_ant()

    ## --- Verra REDD+ ------------------------------------------------------------
    v = pd.read_csv(VERRA, dtype=str)
    ids_d2 = {i for e in EXCLUSIONES for i in e["ids"]}
    v["id"] = v["id"].str.replace(r"\.0$", "", regex=True)
    es_redd = v["methodologies"].fillna("").str.contains(METOD_REDD) | \
        v["afoluNames"].fillna("").str.upper().str.contains("REDD")
    v = v[es_redd & ~v["id"].isin(ids_d2) & v["COD_DANE"].notna()].drop_duplicates("id").copy()
    v["vcs"] = v["projectId"].str.replace(r"\.0$", "", regex=True) if "projectId" in v else ""
    clat = next((c for c in v.columns if c.lower() in ("latitude", "lat")), None)
    clon = next((c for c in v.columns if c.lower() in ("longitude", "lon", "lng")), None)
    print(f"Verra REDD+ sin D2: {len(v)} | coordenadas: {clat}, {clon}")
    em = emitidos_berkeley()
    proy = pd.DataFrame({
        "registro": "Verra", "id": v["id"], "id_karnik": "VCS" + v["vcs"].astype(str),
        "nombre": v["projectName"], "proponente": v.get("proponents", ""),
        "COD_DANE": pad5(v["COD_DANE"]), "estado": v.get("status", ""),
        "area_ha": pd.to_numeric(v.get("area"), errors="coerce"),
        "anio_inicio": v.get("creditPeriodStartDate", "").astype(str).str[:4],
        "emitidos": v["vcs"].map(em).fillna(0.0),
        "lat": pd.to_numeric(v[clat], errors="coerce") if clat else None,
        "lon": pd.to_numeric(v[clon], errors="coerce") if clon else None})

    ## --- Poligonos de Karnik (Colombia, deforestacion evitada) ------------------
    k = gpd.read_file(KARNIK)
    k = k[k["Country"].astype(str).str.contains("Colombia", case=False, na=False)].copy()
    tipo_k = k["Project Type"].astype(str).str.lower() + " " + k["Methodology"].astype(str).str.lower()
    k = k[tipo_k.str.contains("redd|avoided|deforest|vm0006|vm0007|vm0009|vm0015|ad\\b")].to_crs(CRS_M)
    k["geometry"] = k.geometry.make_valid()
    k = k.dissolve(by="ProjectID", as_index=False)
    print(f"Karnik, Colombia, deforestacion evitada: {len(k)} proyectos con poligono")
    k["pct_resguardo"] = pct_en(k, resg, "ProjectID")
    k["pct_consejo"] = pct_en(k, cons, "ProjectID")
    kk = k.set_index("ProjectID")[["pct_resguardo", "pct_consejo"]]

    proy = proy.merge(kk, left_on="id_karnik", right_index=True, how="left")

    ## Verra publica "Multiple Project Proponents" sin nombres. Se completa con
    ## (1) el desarrollador de Karnik, si el proyecto tiene poligono, y (2) la
    ## columna de proponentes de cualquier export de la VCS Project Database que
    ## haya en data/ (busqueda automatica por encabezados).
    dev = k.set_index("ProjectID")["Project Developer Name"].dropna().astype(str)
    falta = proy["proponente"].fillna("").str.match(SIN_NOMBRE)
    proy.loc[falta, "proponente"] = proy.loc[falta, "id_karnik"].map(dev).fillna(proy.loc[falta, "proponente"])
    proy["fuente_proponente"] = "Verra (Platts)"
    proy.loc[falta & ~proy["proponente"].fillna("").str.match(SIN_NOMBRE), "fuente_proponente"] = "Karnik (desarrollador)"
    for f in sorted(Path("data").rglob("*.csv")):
        if f.resolve() == VERRA.resolve() or not re.search(r"vcs|verra", f.name, re.I):
            continue
        try:
            cab = pd.read_csv(f, nrows=0).columns
        except Exception:  # noqa: BLE001
            continue
        cpro = next((c for c in cab if re.search(r"proponent", c, re.I)), None)
        cidv = next((c for c in cab if re.fullmatch(r"\s*(id|project id|resource identifier)\s*", c, re.I)), None)
        if not cpro or not cidv:
            continue
        x = pd.read_csv(f, usecols=[cidv, cpro], dtype=str).dropna()
        x[cidv] = x[cidv].str.replace(r"\D", "", regex=True)
        mapa = x.drop_duplicates(cidv).set_index(cidv)[cpro]
        falta = proy["proponente"].fillna("").str.match(SIN_NOMBRE) & (proy["registro"] == "Verra")
        nuevos = proy.loc[falta, "id_karnik"].str.replace(r"\D", "", regex=True).map(mapa)
        proy.loc[falta & nuevos.reindex(proy.index).notna(), "fuente_proponente"] = f"export VCS ({f.name})"
        proy.loc[falta, "proponente"] = nuevos.fillna(proy.loc[falta, "proponente"])
        print(f"Proponentes completados desde {f} ({cpro}): {int(nuevos.notna().sum())}")
    print("Fuente del proponente:", proy["fuente_proponente"].value_counts().to_dict())
    proy["geometria"] = proy["pct_resguardo"].notna().map({True: "poligono", False: "punto"})
    ## Proyectos de Karnik que no son de Verra (EcoRegistry, BioCarbon)
    otros = k[~k["ProjectID"].isin(proy["id_karnik"])]
    if len(otros):
        mun = gpd.read_file(GPKG_MUN)
        cm = next(c for c in mun.columns if c.upper() in ("COD_DANE", "MPIO_CDPMP", "CODIGO"))
        mun = mun.assign(COD_DANE=pad5(mun[cm]))[["COD_DANE", "geometry"]].to_crs(CRS_M)
        cen = otros.copy(); cen["geometry"] = cen.geometry.representative_point()
        cod = gpd.sjoin(cen[["ProjectID", "geometry"]], mun, predicate="within", how="left") \
            .drop_duplicates("ProjectID").set_index("ProjectID")["COD_DANE"]
        proy = pd.concat([proy, pd.DataFrame({
            "registro": otros["Registry Name"].values, "id": otros["ProjectID"].values,
            "id_karnik": otros["ProjectID"].values, "nombre": otros["Project Name"].values,
            "proponente": otros["Project Developer Name"].values,
            "COD_DANE": otros["ProjectID"].map(cod).values, "estado": "",
            "area_ha": (otros.area / 1e4).values,
            "anio_inicio": otros["Project Start Date"].astype(str).str[:4].values, "emitidos": float("nan"),
            "pct_resguardo": otros["pct_resguardo"].values, "pct_consejo": otros["pct_consejo"].values,
            "geometria": "poligono"})], ignore_index=True)

    ## --- Territorio de los proyectos sin poligono: el punto ---------------------
    sinp = proy["geometria"] == "punto"
    if sinp.any() and clat and clon:
        pts = gpd.GeoDataFrame(proy.loc[sinp, ["id"]],
                               geometry=gpd.points_from_xy(proy.loc[sinp, "lon"], proy.loc[sinp, "lat"]),
                               crs=4326).to_crs(CRS_M)
        proy.loc[sinp, "pct_resguardo"] = pts.within(resg.geometry.iloc[0]).map({True: 100.0, False: 0.0}).values
        proy.loc[sinp, "pct_consejo"] = pts.within(cons.geometry.iloc[0]).map({True: 100.0, False: 0.0}).values

    def territorio(r):
        pr, pc = r["pct_resguardo"], r["pct_consejo"]
        if pd.isna(pr):
            return "sin geometria"
        if pr >= 50: return "resguardo indigena"
        if pc >= 50: return "consejo comunitario"
        if pr + pc >= 50: return "mixto"
        return "otro"
    proy["territorio"] = proy.apply(territorio, axis=1)
    proy["tipo_proponente"] = proy["proponente"].map(clasificar_proponente)
    ## Revision manual: data/reference/proponentes_manual.csv (id, tipo_proponente, nota)
    ## sobrescribe la clasificacion automatica, proyecto por proyecto.
    man = Path("data/reference/proponentes_manual.csv")
    if man.exists():
        mm = pd.read_csv(man, dtype=str).dropna(subset=["id", "tipo_proponente"])
        mm = mm[~mm["tipo_proponente"].str.contains("VERIFICAR", case=False)]
        mapa = mm.set_index("id")["tipo_proponente"]
        sel = proy["id"].isin(mapa.index)
        proy.loc[sel, "tipo_proponente"] = proy.loc[sel, "id"].map(mapa)
        proy.loc[sel, "fuente_proponente"] = "revision manual"
        print(f"Revision manual aplicada a {int(sel.sum())} proyectos ({man})")
    proy["con_creditos"] = proy["emitidos"] > 0

    DIAG.mkdir(parents=True, exist_ok=True)
    proy["fuente_proponente"] = proy["fuente_proponente"].fillna("Karnik (desarrollador)")
    cols = ["registro", "id", "nombre", "proponente", "fuente_proponente", "tipo_proponente", "territorio", "geometria",
            "pct_resguardo", "pct_consejo", "COD_DANE", "estado", "anio_inicio", "area_ha", "emitidos"]
    proy[cols].sort_values(["territorio", "registro", "nombre"]).to_csv(
        DIAG / "65_proyectos_territorio_proponente.csv", index=False, encoding="utf-8-sig")

    ## --- Tabla 4.6 ------------------------------------------------------------------
    t46 = proy.groupby(["territorio", "tipo_proponente"]).agg(
        proyectos=("id", "nunique"), municipios=("COD_DANE", "nunique"),
        area_mha=("area_ha", lambda s: round(s.sum() / 1e6, 2)),
        con_creditos=("con_creditos", "sum"),
        creditos_millones=("emitidos", lambda s: round(s.sum() / 1e6, 2))).reset_index()
    t46.to_csv(DIAG / "65_tabla_4_6.csv", index=False, encoding="utf-8-sig")
    pd.set_option("display.width", 200); pd.set_option("display.max_colwidth", 60)
    print("\nProyectos por territorio:", proy["territorio"].value_counts().to_dict())
    print("Proyectos por tipo de proponente:", proy["tipo_proponente"].value_counts().to_dict())
    print("\nTabla 4.6 (borrador):\n" + t46.to_string(index=False))
    rev = proy[proy["tipo_proponente"].isin(["sin dato", "sin clasificar"]) | proy["tipo_proponente"].str.contains(r"\+")]
    print(f"\nPara revision manual ({len(rev)} proyectos sin clasificar o con varias categorias):")
    print(rev[["registro", "id", "nombre", "proponente", "tipo_proponente"]].to_string(index=False))

    ## --- Municipios tratados de D1 depurada --------------------------------------
    p = pd.read_csv(PANEL, usecols=["COD_DANE", "anio_inicio_tratamiento_todas_fuentes_redd_depurada"],
                    dtype={"COD_DANE": str}, low_memory=False)
    p["COD_DANE"] = pad5(p["COD_DANE"])
    trat = p.loc[pd.to_numeric(p.iloc[:, 1], errors="coerce").fillna(0) > 0, "COD_DANE"].unique()
    mun = gpd.read_file(GPKG_MUN)
    cm = next(c for c in mun.columns if c.upper() in ("COD_DANE", "MPIO_CDPMP", "CODIGO"))
    mun = mun.assign(COD_DANE=pad5(mun[cm]))[["COD_DANE", "geometry"]]
    mun = mun[mun["COD_DANE"].isin(trat)].dissolve(by="COD_DANE", as_index=False).to_crs(CRS_M)
    mun["geometry"] = mun.geometry.make_valid()
    mun["pct_mun_resguardo"] = pct_en(mun, resg, "COD_DANE")
    mun["pct_mun_consejo"] = pct_en(mun, cons, "COD_DANE")
    mt = mun[["COD_DANE", "pct_mun_resguardo", "pct_mun_consejo"]].round(1)
    mt.to_csv(DIAG / "65_municipios_tratados_territorio.csv", index=False, encoding="utf-8-sig")
    print(f"\nMunicipios tratados (D1 depurada): {len(mt)} | con >= 50 % en resguardo: "
          f"{(mt['pct_mun_resguardo'] >= 50).sum()} | en consejo comunitario: {(mt['pct_mun_consejo'] >= 50).sum()}")
    print(f"\nGuardado en {DIAG}: 65_proyectos_territorio_proponente.csv, 65_tabla_4_6.csv, "
          "65_municipios_tratados_territorio.csv")
    return 0


if __name__ == "__main__":
    sys.exit(main())
