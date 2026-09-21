#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
38_construir_dosis_tratamiento.py
=================================
Construye las dos medidas de intensidad (dosis) de tratamiento REDD+ pedidas
por el asesor:

    (i)  dosis_area   = area bajo proyecto REDD+ / area total del municipio
    (ii) dosis_bosque = area bajo proyecto REDD+ / bosque base 2000 del municipio

Insumos:
    data/interim/verra_platts_colombia_con_municipio.csv   (proyectos + area + COD_DANE)
    data/interim/baseline_forest.csv                       (salida de 37_baseline_forest.py)
    [opcional] export de la VCS Project Database con Methodology / AFOLU Activities

Salida:
    data/interim/dosis_tratamiento_municipio.csv
    data/interim/diagnostics/dosis_qc_desborde.csv

LIMITACIONES QUE EL SCRIPT NO PUEDE RESOLVER (documentarlas, no esconderlas)
---------------------------------------------------------------------------
1. SUMA vs UNION. Sin poligonos no se puede calcular |union(A_p)|, solo
   sum(a_p). Si dos proyectos del mismo municipio se traslapan, el numerador
   los cuenta dos veces. El script reporta cuantos municipios tienen mas de
   un proyecto para acotar el alcance del problema.
2. ATRIBUCION AL MUNICIPIO ANFITRION. El area declarada es la del proyecto
   completo; se asigna integra al municipio donde cae el punto. Sobreestima
   ahi y subestima en los municipios vecinos que el proyecto tambien cubre.
   El script marca los casos de desborde (dosis > 1) que son su sintoma
   visible; los que no desbordan tienen el mismo sesgo, solo que invisible.
3. DENOMINADOR FIJO EN 2000. Esto es deliberado y correcto: un denominador
   contemporaneo seria funcion del outcome.

Uso:
    python scripts/38_construir_dosis_tratamiento.py
    python scripts/38_construir_dosis_tratamiento.py --umbral-dosel 75
    python scripts/38_construir_dosis_tratamiento.py --sin-truncar
"""

from __future__ import annotations

import argparse
import datetime as _dt
import re
import sys
import unicodedata
from pathlib import Path

import numpy as np
import pandas as pd

CENTINELA = Path("data/.raiz_canonica")

VERRA_CANDIDATOS = [
    Path("data/interim/verra_platts_colombia_con_municipio.csv"),
    Path("data/interim/verra_colombia_con_municipio.csv"),
]
BASELINE = Path("data/interim/baseline_forest.csv")
CLASIFICACION = Path("data/interim/clasificacion_sectorial.csv")
OUT_DOSIS = Path("data/interim/dosis_tratamiento_municipio.csv")
OUT_QC = Path("data/interim/diagnostics/dosis_qc_desborde.csv")

# Metodologias REDD+ de Verra. Es la clasificacion por campo estructurado,
# no por nombre de proyecto: VM0006 (mosaico agricola), VM0007 (marco
# modular REDD+), VM0009 (deforestacion evitada planificada), VM0015
# (deforestacion no planificada).
METODOLOGIAS_REDD = ("VM0006", "VM0007", "VM0009", "VM0015")

# Nombres alternativos de columna. El archivo de Platts usa camelCase; el
# export de la VCS Project Database usa titulos con espacios.
CANDIDATOS = {
    "id":     ["_id_estable", "projectId", "project_id", "ID", "id"],
    "nombre": ["projectName", "Project Name", "nombre_proyecto", "name"],
    "area":   ["area", "projectArea", "Project Area", "area_ha", "estimatedArea"],
    # OJO: el export de Platts usa plural ("methodologies", "afoluNames");
    # el de la VCS Project Database usa titulos con espacios. Ambos aqui.
    "metodo": ["methodologies", "methodology", "Methodology", "metodologia",
               "protocol", "Methodology / Protocol"],
    "afolu":  ["afoluNames", "afoluActivities", "AFOLU Activities",
               "afolu_activities", "AFOLUActivities"],
    "inicio": ["creditPeriodStartDate", "crediting_period_start_date",
               "Crediting Period Start Date", "anio_inicio"],
    "estado": ["status", "Status", "estado"],
}


# ---------------------------------------------------------------------------
# Utilidades
# ---------------------------------------------------------------------------

def anclar_raiz_canonica(inicio: Path | None = None) -> Path:
    actual = (inicio or Path.cwd()).resolve()
    for c in [actual, *actual.parents]:
        if (c / CENTINELA).exists():
            return c
    raise SystemExit(
        f"ERROR: no se encontro '{CENTINELA}' subiendo desde {actual}.\n"
        "       Corre el script desde tesis_gfc/."
    )


def hallar(df: pd.DataFrame, clave: str) -> str | None:
    """Devuelve el primer nombre de columna presente para `clave`."""
    for cand in CANDIDATOS[clave]:
        if cand in df.columns:
            return cand
    # Busqueda laxa: ignora mayusculas, espacios y guiones bajos.
    norm = {re.sub(r"[\s_]+", "", c).lower(): c for c in df.columns}
    for cand in CANDIDATOS[clave]:
        k = re.sub(r"[\s_]+", "", cand).lower()
        if k in norm:
            return norm[k]
    return None


def normalizar_nombre(s: str) -> str:
    """Clave de deduplicacion: sin tildes, sin puntuacion, minusculas."""
    if not isinstance(s, str):
        return ""
    s = unicodedata.normalize("NFKD", s)
    s = "".join(ch for ch in s if not unicodedata.combining(ch))
    s = re.sub(r"[^a-z0-9]+", " ", s.lower())
    return re.sub(r"\s+", " ", s).strip()


def a_numero(serie: pd.Series) -> pd.Series:
    """Convierte area a float tolerando '113.492,00', '113,492', ' 1 200 ha'."""
    if pd.api.types.is_numeric_dtype(serie):
        return serie.astype(float)
    s = serie.astype(str).str.replace(r"[^\d,.\-]", "", regex=True)
    # Si hay punto y coma, el ultimo separador es el decimal.
    def _conv(x: str) -> float:
        if not x or x in {"-", ".", ","}:
            return np.nan
        if "," in x and "." in x:
            x = x.replace(".", "").replace(",", ".") if x.rfind(",") > x.rfind(".") \
                else x.replace(",", "")
        elif "," in x:
            # Coma sola: decimal si deja <=2 digitos a la derecha, si no miles.
            ent, _, dec = x.rpartition(",")
            x = f"{ent.replace(',', '')}.{dec}" if len(dec) <= 2 else x.replace(",", "")
        try:
            return float(x)
        except ValueError:
            return np.nan
    return serie.astype(str).map(lambda v: _conv(re.sub(r"[^\d,.\-]", "", str(v))))


def anio_de(v) -> float:
    """Extrae el anio de una fecha en cualquier formato razonable."""
    if pd.isna(v):
        return np.nan
    s = str(v)
    if re.fullmatch(r"\d{4}(\.0)?", s):
        return float(s[:4])
    m = re.search(r"(19|20)\d{2}", s)
    return float(m.group(0)) if m else np.nan


# ---------------------------------------------------------------------------
# Carga y clasificacion
# ---------------------------------------------------------------------------

def cargar_verra(raiz: Path) -> tuple[pd.DataFrame, dict]:
    for rel in VERRA_CANDIDATOS:
        ruta = raiz / rel
        if ruta.exists():
            df = pd.read_csv(ruta, dtype={"COD_DANE": str}, encoding="utf-8-sig")
            print(f"[ok] Verra: {len(df)} filas desde {ruta}")
            cols = {k: hallar(df, k) for k in CANDIDATOS}
            print("[ok] columnas detectadas:")
            for k, v in cols.items():
                marca = "ok" if v else "--"
                print(f"       [{marca}] {k:7s} -> {v}")
            if cols["area"] is None:
                raise SystemExit(
                    "ERROR: no hay columna de area en el archivo de Verra.\n"
                    "       Sin area no hay dosis. Revisa el export de la VCS\n"
                    "       Project Database o agrega el nombre real a\n"
                    "       CANDIDATOS['area'] arriba."
                )
            return df, cols
    raise SystemExit(
        f"ERROR: no se hallo el archivo de Verra. Buscado en: "
        f"{[str(raiz / r) for r in VERRA_CANDIDATOS]}"
    )


def marcar_redd_curada(raiz: Path, df: pd.DataFrame, cols: dict) -> pd.Series | None:
    """Marca REDD+ usando la clasificacion curada, si existe.

    `clasificacion_sectorial.csv` trae `clase_redd_final`, que es el resultado
    de la revision manual sobre la sugerencia automatica. Esa mano ya pasada
    manda sobre cualquier regla de regex: se prefiere siempre que este
    disponible y se pueda unir por identificador.
    """
    ruta = raiz / CLASIFICACION
    if not ruta.exists():
        return None
    cla = pd.read_csv(ruta, dtype=str, encoding="utf-8-sig")
    col_clase = next((c for c in ["clase_redd_final", "clase_redd_sugerida"]
                      if c in cla.columns), None)
    if col_clase is None:
        return None

    id_cla = hallar(cla, "id")
    id_df = cols["id"]
    if id_cla is None or id_df is None:
        print("[!!] clasificacion_sectorial.csv existe pero no hay id comun para unir")
        return None

    llave = cla.set_index(cla[id_cla].astype(str).str.replace(r"\.0$", "", regex=True))[col_clase]
    serie = df[id_df].astype(str).str.replace(r"\.0$", "", regex=True).map(llave)
    marca = serie.astype(str).str.upper().str.contains("REDD", na=False)
    cubiertos = int(serie.notna().sum())
    print(f"[ok] clasificacion curada ({col_clase}): {cubiertos}/{len(df)} proyectos "
          f"con clase asignada, {int(marca.sum())} marcados REDD+")
    if cubiertos < len(df) * 0.5:
        print("[!!] la clasificacion curada cubre menos de la mitad: se usa la regla automatica")
        return None
    return marca


def marcar_redd(df: pd.DataFrame, cols: dict) -> pd.Series:
    """REDD+ por campo estructurado: metodologia VM00xx y/o actividad AFOLU.

    Se evita clasificar por nombre de proyecto, que fue el metodo fragil que
    produjo el universo 88/99 anterior.
    """
    marca = pd.Series(False, index=df.index)
    if cols["metodo"]:
        txt = df[cols["metodo"]].astype(str).str.upper()
        marca |= txt.str.contains("|".join(METODOLOGIAS_REDD), na=False)
    if cols["afolu"]:
        txt = df[cols["afolu"]].astype(str).str.upper()
        marca |= txt.str.contains("REDD", na=False)
    if not marca.any():
        print("[!!] Ninguna fila clasifico como REDD+ por campo estructurado.")
        print("     Revisa que el archivo traiga Methodology o AFOLU Activities;")
        print("     si no las trae, usa el export de la VCS Project Database.")
    return marca


def deduplicar(df: pd.DataFrame, cols: dict) -> pd.DataFrame:
    """Un renglon por proyecto. Duplicados conocidos: MATANI, Caruquia.

    Prioridad de clave: id estable > (nombre normalizado + COD_DANE).
    """
    antes = len(df)
    if cols["id"] and df[cols["id"]].notna().all():
        df = df.drop_duplicates(subset=[cols["id"]], keep="first")
    else:
        clave = df[cols["nombre"]].map(normalizar_nombre) + "|" + df["COD_DANE"].astype(str)
        df = df.loc[~clave.duplicated(keep="first")]
    if antes != len(df):
        print(f"[ok] deduplicacion: {antes} -> {len(df)} proyectos "
              f"({antes - len(df)} duplicados eliminados)")
    return df


# ---------------------------------------------------------------------------
# Construccion de la dosis
# ---------------------------------------------------------------------------

def construir_dosis(raiz: Path, umbral: int, truncar: bool) -> tuple[pd.DataFrame, pd.DataFrame]:
    verra, cols = cargar_verra(raiz)

    verra["COD_DANE"] = verra["COD_DANE"].astype(str).str.extract(r"(\d+)")[0].str.zfill(5)
    verra = verra[verra["COD_DANE"].notna() & (verra["COD_DANE"] != "0000n")]

    marca = marcar_redd_curada(raiz, verra, cols)
    if marca is None:
        marca = marcar_redd(verra, cols)
        print("[ok] clasificacion REDD+ por campo estructurado (metodologia / AFOLU)")
    redd = verra[marca].copy()
    print(f"[ok] proyectos REDD+ por campo estructurado: {len(redd)}")
    redd = deduplicar(redd, cols)

    redd["area_ha"] = a_numero(redd[cols["area"]])
    sin_area = int(redd["area_ha"].isna().sum())
    if sin_area:
        print(f"[!!] {sin_area} proyectos REDD+ sin area utilizable: quedan fuera del numerador")
        redd = redd[redd["area_ha"].notna()]

    redd["anio_inicio"] = redd[cols["inicio"]].map(anio_de) if cols["inicio"] else np.nan

    # -- Agregacion por municipio anfitrion ---------------------------------
    agg = redd.groupby("COD_DANE").agg(
        area_redd_ha=("area_ha", "sum"),
        n_proyectos=("area_ha", "size"),
        anio_primer_proyecto=("anio_inicio", "min"),
        area_proyecto_max_ha=("area_ha", "max"),
    ).reset_index()

    multi = int((agg["n_proyectos"] > 1).sum())
    print(f"[ok] municipios con proyecto REDD+: {len(agg)} "
          f"({multi} con mas de uno, donde suma != union)")

    # -- Denominadores ------------------------------------------------------
    base = pd.read_csv(raiz / BASELINE, dtype={"COD_DANE": str})
    base["COD_DANE"] = base["COD_DANE"].str.zfill(5)
    col_bosque = f"baseline_forest_tc{umbral}"
    if col_bosque not in base.columns:
        raise SystemExit(
            f"ERROR: {BASELINE} no tiene la columna {col_bosque}.\n"
            f"       Columnas disponibles: {[c for c in base.columns if 'baseline' in c]}"
        )

    panel = base[["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR",
                  "area_municipio_ha", col_bosque]].copy()
    panel = panel.rename(columns={col_bosque: "bosque_base_ha"})
    panel = panel.merge(agg, on="COD_DANE", how="left")

    huerfanos = set(agg["COD_DANE"]) - set(base["COD_DANE"])
    if huerfanos:
        print(f"[!!] {len(huerfanos)} municipios con proyecto no estan en baseline_forest: "
              f"{sorted(huerfanos)[:5]}")

    panel["area_redd_ha"] = panel["area_redd_ha"].fillna(0.0)
    panel["n_proyectos"] = panel["n_proyectos"].fillna(0).astype(int)
    panel["tratado"] = (panel["area_redd_ha"] > 0).astype(int)

    # -- Las dos dosis ------------------------------------------------------
    panel["dosis_area_cruda"] = panel["area_redd_ha"] / panel["area_municipio_ha"]
    panel["dosis_bosque_cruda"] = np.where(
        panel["bosque_base_ha"] > 0,
        panel["area_redd_ha"] / panel["bosque_base_ha"],
        np.nan,
    )

    panel["desborda_area"] = (panel["dosis_area_cruda"] > 1).astype(int)
    panel["desborda_bosque"] = (panel["dosis_bosque_cruda"] > 1).astype(int)

    if truncar:
        panel["dosis_area"] = panel["dosis_area_cruda"].clip(upper=1.0)
        panel["dosis_bosque"] = panel["dosis_bosque_cruda"].clip(upper=1.0)
    else:
        panel["dosis_area"] = panel["dosis_area_cruda"]
        panel["dosis_bosque"] = panel["dosis_bosque_cruda"]

    panel["umbral_dosel"] = umbral
    panel["truncada"] = int(truncar)
    panel["fecha_generacion"] = _dt.datetime.now().isoformat(timespec="seconds")

    qc = panel[(panel["desborda_area"] == 1) | (panel["desborda_bosque"] == 1)].copy()
    return panel, qc


def reportar(panel: pd.DataFrame, qc: pd.DataFrame, umbral: int) -> None:
    trat = panel[panel["tratado"] == 1]
    print(f"\n{'='*66}\nRESUMEN DE LA DOSIS (umbral de dosel {umbral} %)\n{'='*66}")
    print(f"Municipios tratados: {len(trat)}")
    if len(trat) == 0:
        return
    desc = trat[["dosis_area_cruda", "dosis_bosque_cruda"]].describe(
        percentiles=[0.25, 0.5, 0.75, 0.9]
    ).round(3)
    print(desc.to_string())

    print(f"\nDesbordes (dosis > 1): area {int(trat['desborda_area'].sum())}, "
          f"bosque {int(trat['desborda_bosque'].sum())}")

    # Variacion util: si casi toda la masa esta en un punto, la curva
    # dosis-respuesta no se puede identificar.
    d = trat["dosis_bosque"].dropna()
    if len(d) >= 4:
        q = d.quantile([0.25, 0.5, 0.75]).round(3)
        print(f"\nSoporte de dosis_bosque -- Q1={q.iloc[0]}  mediana={q.iloc[1]}  Q3={q.iloc[2]}")
        print(f"Rango intercuartilico: {round(q.iloc[2] - q.iloc[0], 3)}")
        if (q.iloc[2] - q.iloc[0]) < 0.10:
            print("[!!] IQR muy estrecho: con tan poca variacion en la dosis, contdid")
            print("     no va a identificar una curva dosis-respuesta. Reportar")
            print("     el binario como principal y la dosis como exploratoria.")

    if len(qc):
        print(f"\nMunicipios con desborde (revisar atribucion o poligono):")
        print(qc[["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR", "n_proyectos",
                  "dosis_area_cruda", "dosis_bosque_cruda"]]
              .sort_values("dosis_area_cruda", ascending=False)
              .head(15).round(3).to_string(index=False))


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--umbral-dosel", type=int, default=30, choices=[30, 50, 75],
                    help="Umbral de dosel del denominador de la dosis (ii)")
    ap.add_argument("--sin-truncar", action="store_true",
                    help="No truncar la dosis en 1; deja el valor crudo (para diagnostico)")
    ap.add_argument("--salida", default=str(OUT_DOSIS))
    args = ap.parse_args()

    raiz = anclar_raiz_canonica()
    print(f"[ok] raiz canonica: {raiz}\n")

    panel, qc = construir_dosis(raiz, args.umbral_dosel, truncar=not args.sin_truncar)
    reportar(panel, qc, args.umbral_dosel)

    if len(panel) == 0:
        raise SystemExit("ERROR: 0 filas, no se escribe nada.")

    ruta = raiz / args.salida
    ruta.parent.mkdir(parents=True, exist_ok=True)
    panel.sort_values("COD_DANE").to_csv(ruta, index=False, encoding="utf-8")
    print(f"\n[ok] escrito: {ruta}  ({len(panel)} filas)")

    if len(qc):
        (raiz / OUT_QC).parent.mkdir(parents=True, exist_ok=True)
        qc.sort_values("dosis_area_cruda", ascending=False).to_csv(
            raiz / OUT_QC, index=False, encoding="utf-8")
        print(f"[ok] desbordes: {raiz / OUT_QC}  ({len(qc)} filas)")


if __name__ == "__main__":
    sys.exit(main())
