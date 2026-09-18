#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
29_listado_consolidado_proyectos.py
===================================

Listado maestro de TODOS los proyectos de las cuatro fuentes (Verra, Gold
Standard, RENARE, Cercarbono) con su tipo de proyecto, tengan o no
municipio asignado. Una fila por proyecto (no por evento municipio-año).

A diferencia de 28_clasificar_sector.py (binario AFOLU / NO_AFOLU y solo
proyectos ya emparejados), aqui se asigna un SUBTIPO:

    AFOLU    : REDD, ARR, IFM, ALM, WRC, AFOLU_SIN_SUBTIPO
    NO_AFOLU : HIDROELECTRICA, RENOVABLE_OTRA, ESTUFAS_EFICIENCIA,
               RESIDUOS_METANO, COMBUSTIBLES_TERMICA, TRANSPORTE, INDUSTRIA
    otro     : SIN_CLASIFICAR  (revision manual)

Jerarquia de evidencia (columna 'evidencia_tipo'):
  1. campo sectorial del registrador (methodology, sectoralScope, tipo,
     Sector, Mitigation type, ...)
  2. nombre del proyecto (respaldo)
La clasificacion manual de clasificacion_sectorial.csv se conserva y se
contrasta: si contradice al subtipo sugerido, se marca en 'alerta'.

Salidas
-------
  data/interim/listado_consolidado_proyectos.csv
  data/interim/listado_consolidado_proyectos.xlsx   (hojas: proyectos,
      resumen_fuente_tipo, posibles_duplicados, revisar)

Uso (PowerShell, desde tesis_gfc\\scripts):
    python .\\29_listado_consolidado_proyectos.py
    python .\\29_listado_consolidado_proyectos.py --raiz "C:\\ruta\\a\\tesis_gfc"
"""

from __future__ import annotations

import argparse
import ast
import json
import re
import sys
import unicodedata
import warnings
from pathlib import Path

import pandas as pd

# ---------------------------------------------------------------------------
# Utilidades compartidas con el pipeline (con respaldo si no se importan)
# ---------------------------------------------------------------------------
sys.path.insert(0, str(Path(__file__).resolve().parent))
try:
    from filtro_sectorial import clave_proyecto, normalizar
except ImportError:
    def normalizar(texto) -> str:
        if pd.isna(texto):
            return ""
        t = unicodedata.normalize("NFKD", str(texto))
        t = "".join(c for c in t if not unicodedata.combining(c))
        return re.sub(r"\s+", " ", t).strip().lower()

    def clave_proyecto(fuente, nombre) -> str:
        return f"{normalizar(fuente)}|{normalizar(nombre)}"


# ---------------------------------------------------------------------------
# Reglas de subtipo. El ORDEN importa: se devuelve la primera coincidencia.
# ALM va antes que ARR ("grassland restoration" no es reforestacion).
# ---------------------------------------------------------------------------
REGLAS_AFOLU = [
    ("REDD", r"REDD|VM0006|VM0007|VM0009|VM0015|VM0037|VM0048|"
             r"deforestaci[oó]n evitada|avoided (deforestation|conversion)|"
             r"conservaci[oó]n de (los )?bosques|forest conservation"),
    ("WRC",  r"manglar|mangrove|humedal|wetland|peatland|turber|VM0033|VM0024|"
             r"VM0036|blue carbon|carbono azul"),
    ("ALM",  r"grassland|pastizal|soil carbon|carbono (org[aá]nico )?del suelo|"
             r"agricultural land|VM0017|VM0026|VM0032|VM0042|silvopast"),
    ("IFM",  r"improved forest management|\bIFM\b|VM0003|VM0005|VM0010|VM0012|"
             r"manejo forestal mejorado"),
    ("ARR",  r"AR-AC?MS?\d|VM0047|afforest|reforest|revegetat|restauraci|"
             r"restorat|forestaci[oó]n|plantaci[oó]n forestal|agroforest"),
]
# Señales de AFOLU sin subtipo identificable
AFOLU_GENERICO = (r"Agriculture[,\s]*Forestry|Land\s*Use|\bAFOLU\b|scope\s*14|"
                  r"forest|bosque|selva")

REGLAS_NO_AFOLU = [
    ("HIDROELECTRICA",       r"hidroel|hydro(electric|power)?\b|run[- ]of[- ]river|"
                             r"\bPCH\b|peque[nñ]a central"),
    ("ESTUFAS_EFICIENCIA",   r"cook\s*stove|estufa|TPDDTEC|eficiencia energ|"
                             r"energy efficien|AMS-II\b"),
    ("RESIDUOS_METANO",      r"biog[aá]s|landfill|relleno sanitario|\bLFG\b|"
                             r"aguas residuales|\bPTAR\b|wastewater|compost|"
                             r"residuos|\bwaste\b|AMS-III|ACM0001"),
    ("COMBUSTIBLES_TERMICA", r"termo|ciclo combinado|combined cycle|fuel switch|"
                             r"sustituci[oó]n de combustible"),
    ("TRANSPORTE",           r"transport|movilidad|veh[ií]cul|\bBRT\b"),
    ("INDUSTRIA",            r"cement|acero|steel|\bHFC\b|N2O|refriger|industrial"),
    ("RENOVABLE_OTRA",       r"solar|fotovolt|e[oó]lic|\bwind\b|geoterm|geotherm|"
                             r"renewable energ|energ[ií]a renovable|AMS-I\b|AMS-I\.|"
                             r"ACM0002|energy industries"),
]


def _buscar(reglas, texto):
    for etiqueta, patron in reglas:
        if re.search(patron, texto, flags=re.IGNORECASE):
            return etiqueta
    return None


def clasificar(texto_campo: str, nombre: str) -> tuple[str, str, str]:
    """Devuelve (sector, subtipo, evidencia)."""
    for texto, evidencia in [(texto_campo, "campo_registrador"), (nombre, "nombre")]:
        if not texto.strip():
            continue
        sub = _buscar(REGLAS_AFOLU, texto)
        if sub:
            return "AFOLU", sub, evidencia
        sub = _buscar(REGLAS_NO_AFOLU, texto)
        if sub:
            return "NO_AFOLU", sub, evidencia
        if re.search(AFOLU_GENERICO, texto, re.I):
            if evidencia == "campo_registrador":
                # El registrador dice AFOLU pero no el subtipo: se intenta con el nombre
                sub = _buscar(REGLAS_AFOLU, nombre)
                if sub:
                    return "AFOLU", sub, "campo_registrador+nombre"
            return "AFOLU", "AFOLU_SIN_SUBTIPO", evidencia
    return "REVISAR", "SIN_CLASIFICAR", "ninguna"


# ---------------------------------------------------------------------------
# Lectura flexible de fuentes
# ---------------------------------------------------------------------------
# Columnas cuyo NOMBRE sugiere contenido sectorial (json_normalize produce
# nombres anidados tipo 'objeto.tipo'; se busca por fragmento).
PATRON_COL_SECTOR = re.compile(
    r"(scope|method|protocol|categor|sector|project_?type|mitigation|^tipo$|\.tipo$)",
    re.IGNORECASE,
)


def _col(df: pd.DataFrame, candidatas: list[str]) -> str | None:
    """Primera columna presente; admite coincidencia por sufijo anidado."""
    for c in candidatas:
        if c in df.columns:
            return c
    for c in candidatas:
        for col in df.columns:
            if col.lower().endswith("." + c.lower()):
                return col
    return None


def _leer_csv(rutas: list[Path]) -> pd.DataFrame:
    partes = []
    for r in rutas:
        if r.exists():
            try:
                partes.append(pd.read_csv(r, low_memory=False, encoding="utf-8-sig"))
            except Exception as exc:
                print(f"  [!] No se pudo leer {r.name}: {exc}")
        else:
            print(f"  [ ] Falta {r}")
    return pd.concat(partes, ignore_index=True) if partes else pd.DataFrame()


def _anio(valor):
    if pd.isna(valor):
        return pd.NA
    m = re.search(r"(19|20)\d{2}", str(valor))
    return int(m.group(0)) if m else pd.NA


def _fecha_renare(valor):
    """La fecha de RENARE viene anidada en 'actividades' (fecha_inicial_*)."""
    if pd.isna(valor):
        return pd.NA
    data = None
    for parser in (json.loads, ast.literal_eval):
        try:
            data = parser(str(valor))
            break
        except Exception:
            continue
    anios = []

    def recorrer(o):
        if isinstance(o, dict):
            for k, v in o.items():
                if str(k).startswith("fecha_inicial"):
                    a = _anio(v)
                    if not pd.isna(a):
                        anios.append(a)
                recorrer(v)
        elif isinstance(o, list):
            for x in o:
                recorrer(x)

    recorrer(data)
    return min(anios) if anios else pd.NA


def _cod(serie: pd.Series) -> pd.Series:
    return (serie.fillna("").astype(str).str.replace(r"\.0$", "", regex=True)
            .apply(lambda s: ";".join(x.strip().zfill(5) for x in s.split(";") if x.strip())))


def _armar(df, fuente, c_id, c_nombre, c_fecha, c_estado, cod_dane, n_matches=None,
           anio=None) -> pd.DataFrame:
    cols_sector = [c for c in df.columns if PATRON_COL_SECTOR.search(c)]
    texto = (df[cols_sector].fillna("").astype(str).agg(" | ".join, axis=1)
             if cols_sector else pd.Series("", index=df.index))
    out = pd.DataFrame({
        "fuente": fuente,
        "id_registro": df[c_id].fillna("").astype(str) if c_id else "",
        "nombre_proyecto": df[c_nombre].fillna("").astype(str),
        "estado": df[c_estado].fillna("").astype(str) if c_estado else "",
        "anio_inicio": anio if anio is not None else (df[c_fecha].apply(_anio) if c_fecha else pd.NA),
        "campos_sectoriales": texto.str.slice(0, 400),
        "COD_DANE": cod_dane if cod_dane is not None else "",
        "n_matches_texto": n_matches if n_matches is not None else pd.NA,
    })
    print(f"  {fuente}: {len(out)} filas | columnas sectoriales: {cols_sector or 'ninguna'}")
    return out


def cargar_verra(raiz: Path) -> pd.DataFrame:
    df = _leer_csv([raiz / "data/interim/verra_platts_colombia_con_municipio.csv"])
    if df.empty:
        return df
    return _armar(df, "verra",
                  _col(df, ["resourceIdentifier", "projectId", "id"]),
                  _col(df, ["projectName", "resourceName", "name"]),
                  _col(df, ["projectStartDate", "creditPeriodStartDate", "startDate"]),
                  _col(df, ["resourceStatus", "status", "projectStatus"]),
                  _cod(df["COD_DANE"]) if "COD_DANE" in df else None)


def cargar_gold_standard(raiz: Path) -> pd.DataFrame:
    df = _leer_csv([raiz / "data/interim/goldstandard_projects_colombia_con_municipio.csv",
                    raiz / "data/interim/goldstandard_coords_corregidas.csv"])
    if df.empty:
        return df
    if "id" in df.columns:        # los dos archivos se solapan
        df = df.sort_values("COD_DANE", na_position="last").drop_duplicates("id")
    return _armar(df, "gold_standard", _col(df, ["id"]), _col(df, ["name", "projectName"]),
                  _col(df, ["crediting_period_start_date", "start_date"]),
                  _col(df, ["status", "project_status"]),
                  _cod(df["COD_DANE"]) if "COD_DANE" in df else None)


def cargar_renare(raiz: Path) -> pd.DataFrame:
    # Universo completo (562), no solo los emparejados
    df = _leer_csv([raiz / "data/interim/renare_solicitudes_colombia.csv"])
    if df.empty:
        return df
    c_id = _col(df, ["_id"])
    match = _leer_csv([raiz / "data/interim/diagnostics/renare_municipio_matching.csv"])
    cod = n = None
    if not match.empty and c_id and "_id" in match:
        m = match.drop_duplicates("_id").set_index("_id")
        cod = _cod(df[c_id].map(m["cod_dane"]))
        n = pd.to_numeric(df[c_id].map(m["n_matches"]), errors="coerce")
    c_act = _col(df, ["actividades"])
    anio = df[c_act].apply(_fecha_renare) if c_act else None
    # 'actividades' es JSON largo: se usa para fecha, no como campo sectorial
    return _armar(df.drop(columns=[c_act]) if c_act else df, "renare", c_id,
                  _col(df, ["nombre_iniciativa", "nombre"]), None,
                  _col(df, ["fase", "estado"]), cod, n, anio)


def cargar_cercarbono(raiz: Path) -> pd.DataFrame:
    xlsx = raiz / "data/raw/auxiliary/cercarbono_projects_report.xlsx"
    if not xlsx.exists():
        print(f"  [ ] Falta {xlsx}")
        return pd.DataFrame()
    try:
        df = pd.read_excel(xlsx, sheet_name="Projects Report", header=17).dropna(how="all")
    except Exception as exc:
        print(f"  [!] No se pudo leer Cercarbono: {exc} (revisar HEADER_ROW=17)")
        return pd.DataFrame()
    if "Host Country" in df:
        df = df[df["Host Country"] == "Colombia"].copy()
    # Se descarta la descripcion larga: menciona tecnologias de contexto
    df = df.drop(columns=[c for c in df.columns if "description" in c.lower()])
    match = _leer_csv([raiz / "data/interim/diagnostics/cercarbono_municipio_matching.csv"])
    cod = n = None
    if not match.empty and "Project ID" in match and "Project ID" in df:
        m = match.drop_duplicates("Project ID").set_index("Project ID")
        cod = _cod(df["Project ID"].map(m["cod_dane"]))
        n = pd.to_numeric(df["Project ID"].map(m["n_matches"]), errors="coerce")
    return _armar(df, "cercarbono", _col(df, ["Project ID"]), _col(df, ["Project Name"]),
                  _col(df, ["Crediting periord start", "Crediting period start", "Duration start"]),
                  _col(df, ["Project Stage"]), cod, n)


# ---------------------------------------------------------------------------
def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--raiz", type=Path, default=Path(__file__).resolve().parents[1],
                    help="Carpeta tesis_gfc (por defecto, la madre de scripts/)")
    raiz = ap.parse_args().raiz.resolve()
    print(f"Raiz: {raiz}")
    if not (raiz / "data" / "interim").exists():
        sys.exit(f"No existe {raiz / 'data' / 'interim'}; use --raiz")
    if (raiz / "scripts" / "data").exists():
        warnings.warn("Existe la carpeta sombra scripts/data: este script la ignora.")

    partes = [f(raiz) for f in (cargar_verra, cargar_gold_standard, cargar_renare, cargar_cercarbono)]
    partes = [p for p in partes if not p.empty]
    if not partes:
        sys.exit("No se pudo leer ninguna fuente.")
    df = pd.concat(partes, ignore_index=True)

    # Una fila por proyecto: se agregan municipios si el archivo venia por evento
    df["COD_DANE"] = df["COD_DANE"].fillna("")
    df["anio_inicio"] = pd.to_numeric(df["anio_inicio"], errors="coerce")
    df["n_matches_texto"] = pd.to_numeric(df["n_matches_texto"], errors="coerce")
    df[["id_registro", "nombre_proyecto", "estado"]] = df[["id_registro", "nombre_proyecto", "estado"]].fillna("")
    df = (df.groupby(["fuente", "id_registro", "nombre_proyecto"], as_index=False, dropna=False)
            .agg(estado=("estado", "first"), anio_inicio=("anio_inicio", "min"),
                 campos_sectoriales=("campos_sectoriales", "first"),
                 COD_DANE=("COD_DANE", lambda s: ";".join(sorted({x for v in s for x in str(v).split(";") if x}))),
                 n_matches_texto=("n_matches_texto", "max")))
    df["n_municipios"] = df["COD_DANE"].apply(lambda s: len(s.split(";")) if s else 0)

    # Clasificacion
    res = [clasificar(t, n) for t, n in zip(df["campos_sectoriales"].fillna(""),
                                           df["nombre_proyecto"].fillna(""))]
    df[["sector_sugerido", "tipo_proyecto", "evidencia_tipo"]] = pd.DataFrame(res, index=df.index)

    # Decision manual previa (28_clasificar_sector.py) y contraste
    df["clave"] = [clave_proyecto(f, n) for f, n in zip(df["fuente"], df["nombre_proyecto"])]
    ruta_clas = raiz / "data/interim/clasificacion_sectorial.csv"
    if ruta_clas.exists():
        clas = pd.read_csv(ruta_clas, dtype=str, encoding="utf-8-sig").fillna("")
        mapa = clas.drop_duplicates("clave").set_index("clave")
        df["clasificacion_final_manual"] = df["clave"].map(mapa["clasificacion_final"]).fillna("")
        if "subtipo_afolu" in mapa:
            df["subtipo_manual"] = df["clave"].map(mapa["subtipo_afolu"]).fillna("")
    else:
        df["clasificacion_final_manual"] = ""
    manual = df["clasificacion_final_manual"].str.upper()
    df["alerta"] = ""
    df.loc[manual.isin(["AFOLU", "NO_AFOLU"]) & df["sector_sugerido"].isin(["AFOLU", "NO_AFOLU"])
           & (manual != df["sector_sugerido"]), "alerta"] = "manual contradice sugerido"
    df.loc[df["n_matches_texto"].fillna(0) > 1, "alerta"] += " | municipio ambiguo"
    df["alerta"] = df["alerta"].str.strip(" |")

    # Posibles duplicados entre registros (mismo nombre normalizado)
    df["nombre_norm"] = df["nombre_proyecto"].apply(normalizar)
    fuentes_por_nombre = df.groupby("nombre_norm")["fuente"].nunique()
    df["en_varias_fuentes"] = df["nombre_norm"].map(fuentes_por_nombre).gt(1)

    orden = ["fuente", "id_registro", "nombre_proyecto", "estado", "anio_inicio",
             "sector_sugerido", "tipo_proyecto", "evidencia_tipo",
             "clasificacion_final_manual"] + (["subtipo_manual"] if "subtipo_manual" in df else []) + \
            ["COD_DANE", "n_municipios", "n_matches_texto", "en_varias_fuentes",
             "alerta", "campos_sectoriales", "clave"]
    df = df.sort_values(["fuente", "sector_sugerido", "tipo_proyecto", "nombre_proyecto"])[orden]

    resumen = pd.crosstab(df["fuente"], df["tipo_proyecto"], margins=True, margins_name="Total")
    con_mpio = pd.crosstab(df.loc[df["n_municipios"] > 0, "fuente"],
                           df.loc[df["n_municipios"] > 0, "tipo_proyecto"],
                           margins=True, margins_name="Total")
    duplicados = df[df["en_varias_fuentes"]].sort_values("nombre_proyecto")
    revisar = df[df["tipo_proyecto"].isin(["SIN_CLASIFICAR", "AFOLU_SIN_SUBTIPO"])
                 | df["alerta"].ne("")]

    out_csv = raiz / "data/interim/listado_consolidado_proyectos.csv"
    df.to_csv(out_csv, index=False, encoding="utf-8-sig")   # utf-8-sig: Excel en Windows
    print(f"\nGuardado: {out_csv} ({len(df)} proyectos)")

    out_xlsx = out_csv.with_suffix(".xlsx")
    try:
        with pd.ExcelWriter(out_xlsx, engine="openpyxl") as xw:
            df.to_excel(xw, sheet_name="proyectos", index=False)
            resumen.to_excel(xw, sheet_name="resumen_fuente_tipo")
            con_mpio.to_excel(xw, sheet_name="resumen_con_municipio")
            duplicados.to_excel(xw, sheet_name="posibles_duplicados", index=False)
            revisar.to_excel(xw, sheet_name="revisar", index=False)
        print(f"Guardado: {out_xlsx}")
    except (ImportError, PermissionError) as exc:
        # PermissionError tipico en Windows si el archivo esta abierto en Excel
        print(f"[!] No se escribio el Excel ({exc}). El CSV si quedo guardado.")

    print("\n=== Todos los proyectos: fuente x tipo ===")
    print(resumen.to_string())
    print("\n=== Solo proyectos con municipio asignado ===")
    print(con_mpio.to_string())
    print(f"\nPor revisar a mano: {len(revisar)} | nombres en varias fuentes: "
          f"{duplicados['nombre_proyecto'].str.lower().nunique()}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
