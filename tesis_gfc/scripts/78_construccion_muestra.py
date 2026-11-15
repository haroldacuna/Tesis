#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
78_construccion_muestra.py
==========================

Tabla de construccion de la muestra (Tabla A.5): reconcilia todos los conteos
de proyectos, municipios y observaciones que aparecen en el texto, las tablas y
los anexos. Ninguna cifra se escribe a mano: todas salen de las bases y de las
salidas finales de los scripts.

Paneles (la unidad de cada uno va entre parentesis)
  A. Proyectos REDD+ de Verra (proyecto unico por identificador): todos los
     sectores, con la actividad AFOLU REDD+, excluidos, validos, con fecha, con
     poligono, sin tratamiento y el de metodologia REDD+ sin la actividad.
  B. Universo de la Tabla A.3 (registros): Verra mas poligonos de deforestacion
     evitada de otros registros, y cuantos proyectos unicos son.
  C. Base de poligonos (Karnik et al., 2025) y municipios que cubren.
  D. Municipios (municipio): del panel a la muestra principal.
  E. Muestra de cada especificacion (municipio y municipio-anio; proyectos en
     la evaluacion por poligonos y en la regresion discontinua).

Insumos
  data/interim/verra_platts_colombia_con_municipio.csv           (24/25)
  scripts/filtro_sectorial.py (EXCLUSIONES D2)
  data/interim/proyectos_poligonos/diagnostics/proyectos_resumen.csv  (52)
  data/interim/proyectos_poligonos/cobertura_municipal.csv            (52)
  data/interim/diagnostics/65_proyectos_territorio_proponente.csv     (65)
  data/final/panel_con_tratamiento_actualizado.csv               (26)
  data/interim/dosis_tratamiento_poligonos.csv                   (76)
  outputs/tablas/71_grupo_comparacion.csv, 60_robusteces_A10.csv,
  60_robusteces_A11.csv, 40_contdid_resumen.csv, 56_resultados_celdas.csv,
  73_rdd_espacial.csv

Salida
  outputs/tablas/78_construccion_muestra.csv

Uso (PowerShell, desde tesis_gfc):
  $py = (Get-Command python).Source
  & $py scripts\\78_construccion_muestra.py
"""
from __future__ import annotations

import sys
import unicodedata
from pathlib import Path

import pandas as pd

VENTANA = range(2013, 2020)
ANIOS = 24                                   # panel balanceado 2001-2024
COLS_VINCULO = ("anio_inicio_tratamiento_", "proyectos_")   # regla de controles limpios
COL_G = "anio_inicio_tratamiento_todas_fuentes_redd_depurada"
COL_GA = "anio_inicio_tratamiento_alta_confianza_redd"
COL_M = "muestra_redd_depurada"
# Metodologias REDD+ de Verra (REDD_ESTRICTO en 34_enriquecer_verra_metodologia.py)
METOD_REDD = r"VM0004|VM0006|VM0007|VM0009|VM0011|VM0015|VM0029|VM0037|VM0048|VMD0055"


def raiz() -> Path:
    for base in (Path(__file__).resolve().parent, Path.cwd().resolve()):
        for p in (base, *base.parents):
            if (p / "data" / ".raiz_canonica").is_file():
                return p
    sys.exit("[ERROR] No se encontro data/.raiz_canonica: corre desde tesis_gfc/.")


def leer(r: Path, **kw) -> pd.DataFrame:
    if not r.is_file():
        sys.exit(f"[ERROR] Falta {r}")
    return pd.read_csv(r, **kw)


def pad5(s):
    return pd.to_numeric(s, errors="coerce").astype("Int64").astype(str).str.zfill(5)


def normalizar(s: pd.Series) -> pd.Series:
    """Nombre comparable entre registros: sin acentos, signos ni las palabras
    REDD/PROJECT/PROYECTO (Chonta Corozo REDD+ Project = CHONTA COROZO REDD+)."""
    return (s.fillna("").map(lambda x: unicodedata.normalize("NFKD", x).encode("ascii", "ignore").decode())
            .str.upper().str.replace(r"[^A-Z]", "", regex=True).str.replace(r"REDD|PROJECT|PROYECTO", "", regex=True))


def main() -> int:
    R = raiz(); I = R / "data" / "interim"; T = R / "outputs" / "tablas"
    filas = []
    def f(panel, etapa, n, controles=None, obs=None, nota=""):
        filas.append({"panel": panel, "etapa": etapa, "n": n, "controles": controles, "obs": obs, "nota": nota})

    # --- A. Proyectos de Verra --------------------------------------------------
    # Unidad: proyecto unico por identificador (projectId). Dos criterios del
    # registro definen REDD+ y coinciden en todos los proyectos menos dos:
    #   etiqueta AFOLU REDD+ (universo descriptivo: Tabla 1, Figura 3, escalera)
    #   metodologia REDD+ (la que usa el tratamiento, via 34 y filtro_sectorial)
    # El de corredores de jaguar tiene la etiqueta y una metodologia de
    # restauracion; Yaguara Llanos del Yari, la metodologia sin la etiqueta.
    v = leer(I / "verra_platts_colombia_con_municipio.csv", dtype=str, encoding="utf-8-sig")
    redd = v[v["afoluNames"].fillna("").str.upper().str.contains("REDD")].drop_duplicates("projectId")
    metod = v["methodologies"].fillna("").str.upper().str.contains(METOD_REDD)
    solo_metod = v[metod & ~v["projectId"].isin(redd["projectId"])].drop_duplicates("projectId")
    sys.path.insert(0, str(Path(__file__).resolve().parent))
    from filtro_sectorial import EXCLUSIONES  # noqa: E402
    ids_d2 = {str(i) for e in EXCLUSIONES if e["fuente"].lower() == "verra" for i in e["ids"]}
    # D2 por ubicacion: el punto no corresponde al proyecto (sede del proponente)
    ubic = redd[redd["projectId"].isin(ids_d2) & (redd["COD_DANE"].astype(str).str.zfill(5) == "08001")]
    validos = redd[~redd["projectId"].isin(ubic["projectId"])]
    con_fecha = validos["creditPeriodStartDate"].notna().sum()
    res = leer(I / "proyectos_poligonos" / "diagnostics" / "proyectos_resumen.csv")
    con_pol = ("VCS" + validos["projectId"]).isin(set(res["ProjectID"])).sum()
    # Validos que no tratan a ningun municipio: metodologia que no es REDD+ o
    # exclusion D2 por area (el punto no puede asignar un area 22 veces mayor)
    met_v = validos["methodologies"].fillna("").str.upper().str.contains(METOD_REDD)
    no_tratan = validos[~met_v | validos["projectId"].isin(ids_d2)]
    corto = lambda s: s.str.replace(r"^(REDD\+\s*|Grouped Project (for )?)", "", regex=True).str.slice(0, 45)
    f("A", "Proyectos de Verra en Colombia, todos los sectores (Tabla 1)", v["projectId"].nunique())
    f("A", "Con actividad AFOLU REDD+ declarada", len(redd))
    f("A", "Menos: punto en la sede del proponente", len(ubic), nota="; ".join(ubic["projectName"]))
    f("A", "Con ubicación válida (Figura 3 y escalera de estado)", len(validos))
    f("A", "De ellos, con año de inicio publicado", int(con_fecha))
    f("A", "De ellos, con polígono en Karnik et al. (2025)", int(con_pol))
    f("A", "De ellos, sin tratamiento (metodología o área)", len(no_tratan),
      nota="; ".join(corto(no_tratan["projectName"])))
    f("A", f"Fuera de los {len(redd)}: metodología REDD+ sin la actividad", len(solo_metod),
      nota="; ".join(corto(solo_metod["projectName"])))

    # --- B. Universo de la Tabla A.3 (65): registros, no proyectos --------------
    # Verra (actividad o metodologia REDD+, sin CO2ROZO) mas los poligonos de
    # deforestacion evitada de Karnik que no son de Verra. Un mismo proyecto
    # puede figurar en dos registros: se identifica por municipio y nombre
    # normalizado (sin acentos, signos ni las palabras REDD/PROJECT/PROYECTO).
    r65 = I / "diagnostics" / "65_proyectos_territorio_proponente.csv"
    if r65.is_file():
        t65 = pd.read_csv(r65, dtype=str, encoding="utf-8-sig")
        t65 = t65.assign(clave=t65["COD_DANE"].fillna("") + "|" + normalizar(t65["nombre"]))
        dup = t65[t65.duplicated("clave", keep=False)].drop_duplicates("clave")
        es_v = t65["registro"].eq("Verra")
        # Los registros de Verra deben ser exactamente los validos mas el de
        # metodologia sin la actividad; si no, la etiqueta no lo afirma.
        iguales = set(normalizar(t65.loc[es_v, "nombre"])) == set(normalizar(pd.concat([validos, solo_metod])["projectName"]))
        f("B", f"De Verra: los {len(validos)} válidos y el de metodología sin la actividad" if iguales
          else "De Verra (actividad o metodología REDD+)", int(es_v.sum()))
        f("B", "Polígonos de deforestación evitada de otros registros", int((~es_v).sum()),
          nota="; ".join(f"{k}: {n}" for k, n in t65.loc[~es_v, "registro"].value_counts().items()))
        f("B", "Total de registros", len(t65))
        f("B", "Proyectos únicos", int(t65["clave"].nunique()),
          nota="; ".join(t65.loc[t65["clave"].isin(dup["clave"]) & es_v, "nombre"]
                         .str.replace(r"\s*REDD\+*.*$", "", regex=True, case=False).str.title()))

    # --- C. Base de poligonos ---------------------------------------------------
    cob = leer(I / "proyectos_poligonos" / "cobertura_municipal.csv", dtype={"COD_DANE": str})
    cub = cob["cubierto_ad"].astype(str).str.lower().eq("true")
    ad = res["tipo"] == "AD"
    f("C", "Polígonos en Colombia (todos los registros y tipos)", len(res))
    f("C", "De deforestación evitada", int(ad.sum()))
    f("C", "De ellos, de proyectos de Verra", int((ad & res["registro"].eq("Verra")).sum()))
    f("C", "Municipios que cubren (≥ 1 % y ≥ 100 ha)", int(cub.sum()))

    # --- D. Municipios ------------------------------------------------------------
    p = leer(R / "data" / "final" / "panel_con_tratamiento_actualizado.csv", dtype={"COD_DANE": str},
             low_memory=False, encoding="utf-8-sig")
    p["COD_DANE"] = pad5(p["COD_DANE"])
    vinc = [c for c in p.columns if c.startswith(COLS_VINCULO)]
    m = p.groupby("COD_DANE").agg(G=(COL_G, "min"), Ga=(COL_GA, "min"), mu=(COL_M, "min"))
    m["vinculo"] = p[vinc].apply(pd.to_numeric, errors="coerce").gt(0).groupby(p["COD_DANE"]).any().any(axis=1)
    m["G"] = m["G"].fillna(0)
    trat = m[m["G"] > 0]; ppal = trat[(trat["mu"] == 1) & trat["G"].isin(VENTANA)]
    excl = m[(m["G"] == 0) & (m["mu"] != 1)]
    nunca = m[(m["G"] == 0) & (m["mu"] == 1)]
    limpios = nunca[~nunca["vinculo"]]
    geo = leer(I / "dosis_tratamiento_poligonos.csv", dtype={"COD_DANE": str})
    geo = geo[geo["principal"].astype(str).str.lower().eq("true")]
    f("D", "Municipios del panel", len(m), obs=len(m) * ANIOS)
    f("D", "Tratados (definición principal)", len(trat))
    f("D", "De ellos, con adopción fuera de 2013–2019", int((~trat["G"].isin(VENTANA)).sum()))
    f("D", "Excluidos: único proyecto retirado o rechazado", len(excl))
    f("D", "Nunca tratados", len(nunca))
    f("D", "Menos: con vínculo con proyectos de carbono", int(nunca["vinculo"].sum()))
    f("D", "Controles limpios", len(limpios))
    f("D", "Tratados de la muestra principal", len(ppal))
    # Con proyecto REDD+ de Verra (sin exclusiones D2): mismo universo que la escalera de estado del 58
    vm = v.copy(); vm["COD_DANE"] = vm["COD_DANE"].astype(str).str.extract(r"(\d+)")[0].str.zfill(5)
    con_verra = set(vm.loc[vm["projectId"].isin(redd["projectId"]) & ~vm["projectId"].isin(ids_d2), "COD_DANE"])
    n_v = len(set(ppal.index) & con_verra)
    f("D", "De ellos, con proyecto REDD+ de Verra", n_v)
    f("D", "De ellos, solo con proyectos de RENARE o Cercarbono", len(ppal) - n_v)
    for clase, etq in (("poligono", "De ellos, con ubicación verificada"),
                       ("reubicado", "De ellos, con su polígono en otros municipios"),
                       ("sin_poligono", "De ellos, sin polígono en la base")):
        f("D", etq, int((geo["clase_geo"] == clase).sum()))

    # --- E. Muestra por especificacion -------------------------------------------
    def esp(etq, nt, nc):
        f("E", etq, int(nt), int(nc), int((nt + nc) * ANIOS))
    n_lim = len(limpios)
    esp("Binaria principal (Tabla 4)", len(ppal), n_lim)
    g71 = leer(T / "71_grupo_comparacion.csv")
    q = g71[g71["especificacion"] == "Ubicacion verificada por poligono"].iloc[0]
    esp("Ubicación verificada (Tabla 4, columna 6)", q["n_tratados"], q["n_nunca"])
    cd = leer(T / "40_contdid_resumen.csv")
    nt = cd.loc[cd["corrida"].str.startswith("relativo dosis_bosque todos_controles"), "n_tratados"].iloc[0]
    esp("Tratamiento continuo y gradiente (Tabla 5)", nt, n_lim)
    nh = cd.loc[cd["corrida"].str.contains("hibrida"), "n_tratados"].iloc[0]
    esp("Dosis híbrida (Tabla 5)", nh, n_lim)
    for arch, patron, etq in (("60_robusteces_A10.csv", "^Reasignacion", "Reasignación geográfica (Tabla 10)"),
                              ("60_robusteces_A11.csv", "^Adopcion", "Adopción según el polígono (Tabla 10)")):
        r = T / arch
        if r.is_file():
            a = pd.read_csv(r); a = a[a["especificacion"].str.contains(patron)].iloc[0]
            esp(etq, a["n_tratados"], a["n_controles"])
    cel = T / "56_resultados_celdas.csv"
    if cel.is_file():
        c = pd.read_csv(cel); c = c[c["analisis"].astype(str).str.startswith("principal")].iloc[0]
        f("E", "Evaluación por polígonos (Tabla 9), proyectos", int(c["conglomerados_tratados"]),
          nota="celdas de 2 x 2 km dentro de los polígonos y controles emparejados")
    rdd = T / "73_rdd_espacial.csv"
    if rdd.is_file():
        z = pd.read_csv(rdd).iloc[0]
        f("E", "Regresión discontinua (Tabla A.4), proyectos", int(z.get("proyectos", 0)),
          nota="celdas de 2 x 2 km a menos de 20 km del borde")

    out = pd.DataFrame(filas)
    out.to_csv(T / "78_construccion_muestra.csv", index=False)
    print(out.to_string(index=False))
    # Verificaciones cruzadas: deben coincidir con 00_config (REF_N_TRATADOS/CONTROLES)
    assert len(ppal) == 36 and n_lim == 986, "La muestra principal no reproduce 36 tratados y 986 controles."
    print(f"\n[OK] {T / '78_construccion_muestra.csv'}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
