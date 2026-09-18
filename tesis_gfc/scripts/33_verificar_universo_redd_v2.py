#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
33_verificar_universo_redd_v2.py
================================

VERIFICACION V1 (corregida) -- ¿cuantos municipios quedan tratados si el
universo se restringe a proyectos REDD+?

    python 33_verificar_universo_redd_v2.py

SOLO LECTURA sobre el pipeline. Escribe unicamente su CSV de diagnostico.

QUE CAMBIO FRENTE A LA v1
-------------------------
1) PRECEDENCIA POR CLASE, NO POR CAMPO.
   La v1 recorria campo por campo y retornaba al primer match dentro del
   campo. El 'sectoralScope' de Verra ("Agriculture, Forestry and Other
   Land Use") golpeaba el patron 'agricultur' y devolvia AFOLU_NO_REDD
   sin llegar a mirar el nombre del proyecto. Resultado: Verra reportaba
   0 REDD+ con 72 AFOLU, lo cual es imposible.

   Ahora se prueba REDD+ contra TODOS los campos primero; solo si nada
   coincide se pasa a AFOLU_NO_REDD, y despues a NO_AFOLU.

2) EL ALCANCE SECTORIAL NO DECIDE.
   "Agriculture, Forestry and Other Land Use" y "AFOLU" dicen que el
   proyecto es de uso del suelo, no que mecanismo usa. Un proyecto cuya
   unica evidencia sea el alcance sectorial queda en AFOLU_GENERICO y va
   a revision manual, no a una clase sustantiva.

3) RENARE CON FECHAS.
   Las fechas estan anidadas en el JSON de 'actividades'. Se replica la
   extraccion de 26_consolidar_fuentes_carbono.py. Sin esto, los 176
   REDD+ de RENARE se caian enteros por falta de anio.

4) FILTRO n_matches == 1 en RENARE y Cercarbono.
   Igual que el consolidador: los matches ambiguos de texto no entran.
   Se fuerza a numerico, porque si el CSV los guardo como texto la
   comparacion falla en silencio para todas las filas.
"""

from __future__ import annotations

import ast
import re
import sys
import unicodedata
from pathlib import Path

import pandas as pd

RAIZ = Path(".")

ANIO_PANEL_MIN = 2001
ANIO_PANEL_MAX = 2023
COHORTE_MIN_ESTIMABLE = ANIO_PANEL_MIN + 1
COHORTE_MAX_ESTIMABLE = ANIO_PANEL_MAX - 1

VERRA_FILE = RAIZ / "data/interim/verra_platts_colombia_con_municipio.csv"
GS_FILES = [
    RAIZ / "data/interim/goldstandard_projects_colombia_con_municipio.csv",
    RAIZ / "data/interim/goldstandard_coords_corregidas.csv",
]
RENARE_RAW = RAIZ / "data/interim/renare_solicitudes_colombia.csv"
RENARE_MATCH = RAIZ / "data/interim/diagnostics/renare_municipio_matching.csv"
CERCARBONO_MATCH = RAIZ / "data/interim/diagnostics/cercarbono_municipio_matching.csv"

OUT_DIAG = RAIZ / "data/interim/diagnostics/verificacion_universo_redd_v2.csv"

# ---------------------------------------------------------------------------
# Reglas
# ---------------------------------------------------------------------------

# Mecanismo = evitar perdida de cobertura. Esto es REDD+.
PAT_REDD = [
    r"^VM000?6", r"^VM0007", r"^VM0009", r"^VM0015", r"^VM0037", r"^VM0048",
    r"\bREDD\b", r"REDD\+",
    r"avoided\s+(unplanned\s+|planned\s+)?deforestation",
    r"avoided\s+ecosystem\s+conversion",
    r"reducing\s+emissions\s+from\s+deforestation",
    r"deforestaci[oó]n\s+evitada", r"degradaci[oó]n\s+evitada",
    r"conservaci[oó]n\s+de\s+bosque",
]

# Mecanismo = agregar cobertura o cambiar manejo. AFOLU pero NO REDD+.
# Es el grupo placebo.
PAT_AFOLU_NO_REDD = [
    r"^AR-", r"^VM0047", r"^VM0042",
    r"afforest", r"reforest", r"forestaci[oó]n",
    r"restauraci", r"restorat", r"\bARR\b", r"\bIFM\b",
    r"improved\s+forest\s+management", r"plantaci[oó]n",
    r"agroforest", r"silvopast", r"soil\s+carbon", r"suelos\s+degradados",
    r"grassland", r"wetland", r"humedal", r"manglar", r"mangrove",
    r"blue\s+carbon", r"\bALM\b", r"\bWRC\b",
    r"agricultural\s+land\s+management",
]

# Fuera de AFOLU.
PAT_NO_AFOLU = [
    r"^AMS-I", r"TPDDTEC", r"fuel\s*switch",
    r"cook\s*stove", r"cookstove", r"estufa", r"cocina",
    r"hidroel", r"hydro", r"run[- ]of[- ]river", r"\bPCH\b", r"\bSHP\b",
    r"e[oó]lic", r"wind\s*(power|farm|energy)", r"solar", r"fotovolt",
    r"biog[aá]s", r"landfill", r"relleno\s*sanitario", r"\bLFG\b",
    r"aguas\s*residuales", r"\bPTAR\b", r"water\s*treatment",
    r"termo", r"ciclo\s*combinado", r"combined\s*cycle",
    r"transport", r"movilidad", r"veh[ií]cul",
    r"cement", r"acero", r"industrial\s*gas", r"refriger", r"\bHFC\b",
    r"geoterm", r"geotherm", r"renewable\s*energ", r"energ[ií]a\s*renovable",
    r"energy\s*efficiency", r"eficiencia\s*energetica",
]

# Solo dice "es de uso del suelo". NO decide clase: manda a revision.
PAT_AFOLU_GENERICO = [
    r"agriculture[,\s]*forestry", r"\bAFOLU\b", r"land\s*use",
    r"forestry\s+and\s+other\s+land", r"scope\s*14", r"agricultur",
]


def normalizar(texto) -> str:
    if pd.isna(texto):
        return ""
    t = unicodedata.normalize("NFKD", str(texto))
    t = "".join(c for c in t if not unicodedata.combining(c))
    return re.sub(r"\s+", " ", t).strip()


def _match(patrones: list[str], texto: str) -> str | None:
    for p in patrones:
        if re.search(p, texto, flags=re.IGNORECASE):
            return p
    return None


def clasificar(campos: dict[str, str]) -> tuple[str, str]:
    """Precedencia POR CLASE. Se prueba cada clase contra todos los campos
    antes de pasar a la siguiente. REDD+ va primero porque todo REDD+ es
    AFOLU pero no al reves: probar AFOLU primero absorbe los REDD+."""
    for clase, patrones in [
        ("REDD_PLUS", PAT_REDD),
        ("AFOLU_NO_REDD", PAT_AFOLU_NO_REDD),
        ("NO_AFOLU", PAT_NO_AFOLU),
    ]:
        for campo, texto in campos.items():
            if not texto.strip():
                continue
            p = _match(patrones, texto)
            if p:
                return clase, f"{campo}~{p}"

    # Ultimo recurso: ¿al menos sabemos que es de uso del suelo?
    for campo, texto in campos.items():
        if texto.strip() and _match(PAT_AFOLU_GENERICO, texto):
            return "AFOLU_GENERICO", f"{campo}~solo alcance sectorial"

    return "REVISAR", "sin coincidencia"


def _anio(valor) -> int | None:
    if pd.isna(valor):
        return None
    m = re.search(r"(19|20)\d{2}", str(valor))
    return int(m.group(0)) if m else None


def _fecha_actividades(valor) -> int | None:
    """RENARE anida las fechas dentro del JSON de 'actividades'."""
    if pd.isna(valor):
        return None
    try:
        data = ast.literal_eval(str(valor))
    except (ValueError, SyntaxError):
        return None
    if isinstance(data, list) and data:
        data = data[0]
    if not isinstance(data, dict):
        return None
    return _anio(data.get("fecha_inicial_1"))


def _fila(fuente, nombre, campos, dane, anio) -> dict:
    clase, motivo = clasificar(campos)
    return {
        "fuente": fuente,
        "nombre_proyecto": nombre,
        "campos_sectoriales": " | ".join(v for k, v in campos.items() if k != "nombre")[:200],
        "COD_DANE": str(dane).split(".")[0].zfill(5) if pd.notna(dane) else None,
        "anio_inicio": anio,
        "clase": clase,
        "motivo": motivo,
    }


# ---------------------------------------------------------------------------
# Carga por fuente
# ---------------------------------------------------------------------------

def cargar_verra() -> list[dict]:
    if not VERRA_FILE.exists():
        print(f"    [ ] Falta {VERRA_FILE}")
        return []
    df = pd.read_csv(VERRA_FILE, low_memory=False)
    cols_sec = [c for c in ["methodology", "Methodology", "sectoralScope",
                            "projectType", "protocol"] if c in df.columns]
    print(f"    filas={len(df)}  campos sectoriales={cols_sec}")
    if not any(c.lower() == "methodology" for c in cols_sec):
        print("    [!] NO hay campo 'methodology'. Sin el, un proyecto REDD+ de")
        print("        Verra solo se detecta si su NOMBRE lo dice. Ver seccion 5.")
    return [
        _fila("verra", normalizar(r.get("projectName")),
              {"nombre": normalizar(r.get("projectName")),
               **{c: normalizar(r.get(c)) for c in cols_sec}},
              r.get("COD_DANE"), _anio(r.get("projectStartDate")))
        for _, r in df.iterrows()
    ]


def cargar_gs() -> list[dict]:
    partes = [pd.read_csv(p, low_memory=False) for p in GS_FILES if p.exists()]
    if not partes:
        return []
    df = pd.concat(partes, ignore_index=True)
    if "id" in df.columns:
        df = df.drop_duplicates(subset=["id"])
    cols_sec = [c for c in ["methodology"] if c in df.columns]
    print(f"    filas={len(df)}  campos sectoriales={cols_sec}")
    return [
        _fila("gold_standard", normalizar(r.get("name")),
              {"nombre": normalizar(r.get("name")),
               **{c: normalizar(r.get(c)) for c in cols_sec}},
              r.get("COD_DANE"), _anio(r.get("crediting_period_start_date")))
        for _, r in df.iterrows()
    ]


def cargar_renare() -> list[dict]:
    if not RENARE_MATCH.exists():
        print(f"    [ ] Falta {RENARE_MATCH}")
        return []
    m = pd.read_csv(RENARE_MATCH, low_memory=False)
    m["n_matches"] = pd.to_numeric(m["n_matches"], errors="coerce")
    print(f"    n_matches: {m['n_matches'].value_counts(dropna=False).to_dict()}")
    u = m[m["n_matches"] == 1].copy()
    print(f"    filas={len(m)}  con match unico={len(u)}")

    if RENARE_RAW.exists():
        raw = pd.read_csv(RENARE_RAW, low_memory=False)
        keep = [c for c in ["_id", "actividades", "tipo", "tipo_iniciativa"] if c in raw.columns]
        if "_id" in keep and "_id" in u.columns:
            u = u.merge(raw[keep], on="_id", how="left", suffixes=("", "_raw"))
            print(f"    columnas traidas del crudo: {keep}")
        else:
            print("    [!] No pude unir con el crudo: falta '_id'. Sin fechas.")
    else:
        print(f"    [!] Falta {RENARE_RAW}. RENARE quedara sin fechas.")

    cols_sec = [c for c in ["tipo_iniciativa", "tipo", "tipo_raw", "actividades"]
                if c in u.columns]
    filas = []
    for _, r in u.iterrows():
        anio = _fecha_actividades(r.get("actividades")) if "actividades" in u.columns else None
        filas.append(_fila(
            "renare", normalizar(r.get("nombre_iniciativa")),
            {"nombre": normalizar(r.get("nombre_iniciativa")),
             **{c: normalizar(r.get(c))[:300] for c in cols_sec}},
            r.get("cod_dane"), anio))
    con_anio = sum(1 for f in filas if f["anio_inicio"])
    print(f"    con anio de inicio extraido: {con_anio}/{len(filas)}")
    return filas


def cargar_cercarbono() -> list[dict]:
    if not CERCARBONO_MATCH.exists():
        print(f"    [ ] Falta {CERCARBONO_MATCH}")
        return []
    m = pd.read_csv(CERCARBONO_MATCH, low_memory=False)
    m["n_matches"] = pd.to_numeric(m["n_matches"], errors="coerce")
    u = m[m["n_matches"] == 1].copy()
    print(f"    filas={len(m)}  con match unico={len(u)}")
    cols_sec = [c for c in ["Methodology", "Sector", "Protocol", "Mitigation type"]
                if c in u.columns]
    col_fecha = next((c for c in ["Duration start", "Crediting periord start",
                                  "Crediting period start"] if c in u.columns), None)
    print(f"    campos sectoriales={cols_sec}  fecha={col_fecha}")
    return [
        _fila("cercarbono", normalizar(r.get("Project Name")),
              {"nombre": normalizar(r.get("Project Name")),
               **{c: normalizar(r.get(c)) for c in cols_sec}},
              r.get("cod_dane"), _anio(r.get(col_fecha)) if col_fecha else None)
        for _, r in u.iterrows()
    ]


# ---------------------------------------------------------------------------
# Reporte
# ---------------------------------------------------------------------------

def _linea(t: str = "") -> None:
    print("=" * 72 if not t else f"\n{'=' * 72}\n{t}\n{'=' * 72}")


def reportar(ev: pd.DataFrame) -> None:
    _linea("1. PROYECTOS POR CLASE Y FUENTE")
    print(pd.crosstab(ev["fuente"], ev["clase"], margins=True).to_string())

    _linea("2. POR QUE SE PIERDEN PROYECTOS")
    redd = ev[ev["clase"] == "REDD_PLUS"]
    print(f"\n  REDD+ identificados:            {len(redd)}")
    print(f"    sin COD_DANE:                 {redd['COD_DANE'].isna().sum()}")
    print(f"    sin anio de inicio:           {redd['anio_inicio'].isna().sum()}")
    print(f"    utilizables (ambos):          "
          f"{(redd['COD_DANE'].notna() & redd['anio_inicio'].notna()).sum()}")
    print("\n  Perdida por fuente:")
    print(redd.assign(
        utilizable=redd["COD_DANE"].notna() & redd["anio_inicio"].notna()
    ).groupby("fuente")["utilizable"].agg(["sum", "count"]).to_string())

    _linea("3. MUNICIPIOS TRATADOS POR DEFINICION")
    util = ev[ev["COD_DANE"].notna() & ev["anio_inicio"].notna()].copy()
    util["anio_inicio"] = util["anio_inicio"].astype(int)

    filas = []
    for etiqueta, clases in {
        "D1  REDD+ estricto": ["REDD_PLUS"],
        "D2  REDD+ + AFOLU forestal": ["REDD_PLUS", "AFOLU_NO_REDD"],
        "D3  esquema vigente (todo)": ["REDD_PLUS", "AFOLU_NO_REDD",
                                       "NO_AFOLU", "AFOLU_GENERICO"],
    }.items():
        sub = util[util["clase"].isin(clases)]
        coh = sub.groupby("COD_DANE")["anio_inicio"].min()
        est = coh[(coh >= COHORTE_MIN_ESTIMABLE) & (coh <= COHORTE_MAX_ESTIMABLE)]
        filas.append({"definicion": etiqueta, "proyectos": len(sub),
                      "municipios": coh.size, "estimables": est.size,
                      "cohortes": est.nunique()})
    print(pd.DataFrame(filas).to_string(index=False))

    _linea("4. DISTRIBUCION DE COHORTES -- REDD+ ESTRICTO")
    r = util[util["clase"] == "REDD_PLUS"]
    if r.empty:
        print("\n  Ninguno utilizable. Mira la seccion 2 para saber si falta")
        print("  municipio, fecha, o ambos -- el arreglo es distinto en cada caso.")
    else:
        print(r.groupby("COD_DANE")["anio_inicio"].min()
              .value_counts().sort_index().to_string())

    _linea("5. REVISION MANUAL PENDIENTE")
    for clase in ["AFOLU_GENERICO", "REVISAR"]:
        sub = ev[ev["clase"] == clase]
        if sub.empty:
            continue
        print(f"\n  {clase}: {len(sub)} proyecto(s)")
        print(sub.groupby("fuente").size().to_string())
        print("  Ejemplos:")
        for n in sub["nombre_proyecto"].head(8):
            print(f"    - {n[:70]}")

    _linea("6. PLACEBO -- AFOLU NO REDD+")
    pl = util[util["clase"] == "AFOLU_NO_REDD"]
    solo = set(pl["COD_DANE"]) - set(r["COD_DANE"] if not r.empty else [])
    print(f"\n  Proyectos: {len(pl)}   Municipios sin REDD+: {len(solo)}")


def main() -> None:
    if (RAIZ / "scripts/data/interim").exists():
        print("\n[!] Existe la carpeta sombra scripts/data/interim/.")
        print("    Este script leyo de ./data/interim/. Verifica cual es la buena.\n")

    todo = []
    for nombre, fn in [("verra", cargar_verra), ("gold_standard", cargar_gs),
                       ("renare", cargar_renare), ("cercarbono", cargar_cercarbono)]:
        print(f"\n--- {nombre} ---")
        todo.extend(fn())

    if not todo:
        print("\nNo se cargo nada. ¿Estas en la raiz del proyecto?")
        sys.exit(1)

    ev = pd.DataFrame(todo)
    reportar(ev)

    OUT_DIAG.parent.mkdir(parents=True, exist_ok=True)
    ev.sort_values(["clase", "fuente", "nombre_proyecto"]).to_csv(
        OUT_DIAG, index=False, encoding="utf-8-sig")
    _linea()
    print(f"Detalle: {OUT_DIAG}")


if __name__ == "__main__":
    main()
