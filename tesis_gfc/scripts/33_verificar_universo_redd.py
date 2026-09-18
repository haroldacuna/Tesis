#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
33_verificar_universo_redd.py
=============================

VERIFICACION V1 -- ¿cuantos municipios quedan tratados si el universo se
restringe a proyectos REDD+?

    python 33_verificar_universo_redd.py

SOLO LECTURA. No escribe sobre el panel, ni sobre
data/interim/clasificacion_sectorial.csv, ni sobre ningun archivo del
pipeline. Su unica salida es un CSV de diagnostico en
data/interim/diagnostics/ y un reporte por consola.

POR QUE EXISTE
--------------
El filtro sectorial vigente separa AFOLU de NO_AFOLU. REDD+ es un
subconjunto ESTRICTO de AFOLU: excluye forestacion/reforestacion (ARR),
restauracion y agricultura, cuyo mecanismo es adicion de cobertura y no
evitacion de perdida. Como el outcome del panel es perdida de cobertura
(Hansen GFC), esos proyectos no deberian mover el indicador.

Antes de reescribir capitulos hay que saber si queda N suficiente para
estimar. Este script responde esa pregunta y nada mas.

QUE MIRA EN CADA FUENTE
-----------------------
  RENARE      tipo_iniciativa      -> campo estructurado, sin ambiguedad
  Verra       methodology / scope  -> metodologias VM00xx de deforestacion
  Cercarbono  protocolo / nombre   -> respaldo por palabra clave
  Gold Standard                    -> ya verificado: 0 REDD+ (ver V2)

DONDE PUEDE FALLAR
------------------
Si una fuente no expone su campo sectorial, el script NO adivina: marca
los proyectos como REVISAR y los reporta aparte. Un proyecto sin
clasificar nunca se cuenta como tratado.
"""

from __future__ import annotations

import re
import sys
import unicodedata
from pathlib import Path

import pandas as pd

# ---------------------------------------------------------------------------
# Configuracion
# ---------------------------------------------------------------------------

RAIZ = Path(".")

# Ventana del panel. Una cohorte fuera de este rango no es estimable:
# no hay periodos pre o no hay periodos post.
ANIO_PANEL_MIN = 2001
ANIO_PANEL_MAX = 2023

# Callaway-Sant'Anna necesita al menos un periodo pre y uno post por
# cohorte. Ese es el rango realmente utilizable.
COHORTE_MIN_ESTIMABLE = ANIO_PANEL_MIN + 1
COHORTE_MAX_ESTIMABLE = ANIO_PANEL_MAX - 1

FUENTES: dict[str, list[Path]] = {
    "verra": [RAIZ / "data/interim/verra_platts_colombia_con_municipio.csv"],
    "gold_standard": [
        RAIZ / "data/interim/goldstandard_projects_colombia_con_municipio.csv",
        RAIZ / "data/interim/goldstandard_coords_corregidas.csv",
    ],
    "renare": [
        RAIZ / "data/interim/renare_solicitudes_colombia.csv",
        RAIZ / "data/interim/diagnostics/renare_municipio_matching.csv",
    ],
    "cercarbono": [RAIZ / "data/interim/diagnostics/cercarbono_municipio_matching.csv"],
}

OUT_DIAG = RAIZ / "data/interim/diagnostics/verificacion_universo_redd.csv"

# Columnas candidatas, por orden de preferencia
COLS_NOMBRE = [
    "projectName", "Project Name", "name", "nombre_iniciativa",
    "nombre_proyecto", "nombre", "titulo", "title", "proyecto",
]

COLS_SECTOR = [
    "tipo_iniciativa", "methodology", "Methodology", "metodologia",
    "scope", "sectoralScope", "Sector", "sector", "protocol", "Protocol",
    "protocolo", "project_type", "projectType", "tipo", "actividades",
    "categoria", "category", "Mitigation type",
]

COLS_FECHA_INICIO = [
    "projectStartDate", "creditPeriodStartDate", "startDate",
    "crediting_period_start_date", "start_date", "registration_date",
    "Duration start", "fecha_inicio", "anio_inicio",
]

COLS_DANE = ["COD_DANE", "cod_dane"]

# ---------------------------------------------------------------------------
# Reglas de deteccion
#
# El orden de evaluacion importa: REDD+ se prueba PRIMERO, porque todo
# REDD+ es AFOLU pero no al reves. Si se probara AFOLU primero, los
# proyectos REDD+ caerian en la categoria generica.
# ---------------------------------------------------------------------------

# Verra: metodologias de deforestacion/degradacion evitada.
# VM0047 y las AR-ACM/AR-AM son ARR -> AFOLU pero NO REDD+.
REDD_ESTRUCTURADO = [
    r"^VM000?6", r"^VM0007", r"^VM0009", r"^VM0015", r"^VM0037", r"^VM0048",
    r"\bREDD\b", r"REDD\+",
    r"avoided\s+(unplanned\s+|planned\s+)?deforestation",
    r"avoided\s+ecosystem\s+conversion",
    r"deforestaci[oó]n\s+evitada",
    r"degradaci[oó]n\s+evitada",
]

# AFOLU que NO es REDD+. Estos son el grupo placebo.
AFOLU_NO_REDD = [
    r"^AR-", r"^VM0047", r"afforest", r"reforest", r"forestaci[oó]n",
    r"restauraci", r"restorat", r"\bARR\b", r"\bIFM\b",
    r"improved\s+forest\s+management", r"plantaci[oó]n",
    r"agricultur", r"agroforest", r"silvopast", r"soil\s+carbon",
    r"suelos\s+degradados", r"grassland", r"wetland", r"manglar",
    r"mangrove", r"blue\s+carbon", r"\bALM\b", r"\bWRC\b",
]

# Fuera de AFOLU por completo. Ya clasificados en el filtro vigente;
# se repiten aqui para que el script corra de forma autonoma.
NO_AFOLU = [
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
]


def normalizar(texto) -> str:
    if pd.isna(texto):
        return ""
    t = unicodedata.normalize("NFKD", str(texto))
    t = "".join(c for c in t if not unicodedata.combining(c))
    return re.sub(r"\s+", " ", t).strip()


def _match(patrones: list[str], texto: str) -> str | None:
    """Devuelve el patron que hizo match, para poder auditar la decision."""
    for p in patrones:
        if re.search(p, texto, flags=re.IGNORECASE):
            return p
    return None


def clasificar(texto_sector: str, texto_nombre: str) -> tuple[str, str]:
    """Devuelve (clase, motivo). El campo estructurado manda sobre el nombre."""
    for etiqueta, ambito in [("sector", texto_sector), ("nombre", texto_nombre)]:
        if not ambito.strip():
            continue
        p = _match(REDD_ESTRUCTURADO, ambito)
        if p:
            return "REDD_PLUS", f"{etiqueta}~{p}"
        p = _match(AFOLU_NO_REDD, ambito)
        if p:
            return "AFOLU_NO_REDD", f"{etiqueta}~{p}"
        p = _match(NO_AFOLU, ambito)
        if p:
            return "NO_AFOLU", f"{etiqueta}~{p}"
    return "REVISAR", "sin coincidencia"


# ---------------------------------------------------------------------------
# Carga
# ---------------------------------------------------------------------------

def _primera(df: pd.DataFrame, candidatas: list[str]) -> str | None:
    return next((c for c in candidatas if c in df.columns), None)


def _anio(valor) -> int | None:
    if pd.isna(valor):
        return None
    m = re.search(r"(19|20)\d{2}", str(valor))
    return int(m.group(0)) if m else None


def _leer(paths: list[Path]) -> pd.DataFrame:
    partes = []
    for p in paths:
        if not p.exists():
            print(f"    [ ] Falta {p}")
            continue
        try:
            partes.append(pd.read_csv(p, low_memory=False))
        except Exception as exc:
            print(f"    [!] No pude leer {p}: {exc}")
    return pd.concat(partes, ignore_index=True) if partes else pd.DataFrame()


def cargar_todo() -> pd.DataFrame:
    filas = []

    for fuente, paths in FUENTES.items():
        print(f"\n--- {fuente} ---")
        df = _leer(paths)
        if df.empty:
            print("    sin datos")
            continue

        col_nombre = _primera(df, COLS_NOMBRE)
        col_dane = _primera(df, COLS_DANE)
        col_fecha = _primera(df, COLS_FECHA_INICIO)
        cols_sector = [c for c in COLS_SECTOR if c in df.columns]

        print(f"    filas={len(df)}  nombre={col_nombre}  dane={col_dane}  fecha={col_fecha}")
        print(f"    campos sectoriales disponibles: {cols_sector or 'NINGUNO'}")

        if not cols_sector:
            print("    [!] Sin campo sectorial: todo quedara como REVISAR.")
            print(f"        Columnas presentes: {list(df.columns)[:20]}")
        if col_dane is None:
            print("    [!] Sin COD_DANE: esta fuente no puede aportar municipios tratados.")

        for _, r in df.iterrows():
            nombre = normalizar(r.get(col_nombre)) if col_nombre else ""
            sector = " | ".join(normalizar(r.get(c)) for c in cols_sector)
            clase, motivo = clasificar(sector, nombre)

            dane = r.get(col_dane) if col_dane else None
            dane = str(dane).split(".")[0].zfill(5) if pd.notna(dane) else None

            filas.append({
                "fuente": fuente,
                "nombre_proyecto": nombre,
                "campo_sectorial": sector[:200],
                "COD_DANE": dane,
                "anio_inicio": _anio(r.get(col_fecha)) if col_fecha else None,
                "clase": clase,
                "motivo": motivo,
            })

    return pd.DataFrame(filas)


# ---------------------------------------------------------------------------
# Reporte
# ---------------------------------------------------------------------------

def _linea(t: str = "") -> None:
    print("=" * 72 if not t else f"\n{'=' * 72}\n{t}\n{'=' * 72}")


def reportar(ev: pd.DataFrame) -> None:
    _linea("1. PROYECTOS POR CLASE Y FUENTE")
    print(pd.crosstab(ev["fuente"], ev["clase"], margins=True).to_string())

    pendientes = ev[ev["clase"] == "REVISAR"]
    if len(pendientes):
        print(f"\n[!] {len(pendientes)} proyecto(s) sin clasificar. NO se cuentan como")
        print("    tratados. Revisalos antes de fijar el N definitivo:")
        print(pendientes.groupby("fuente").size().to_string())

    _linea("2. MUNICIPIOS TRATADOS POR DEFINICION")

    utilizables = ev[ev["COD_DANE"].notna() & ev["anio_inicio"].notna()].copy()
    utilizables["anio_inicio"] = utilizables["anio_inicio"].astype(int)

    definiciones = {
        "D1  REDD+ estricto": ["REDD_PLUS"],
        "D2  REDD+ + AFOLU forestal": ["REDD_PLUS", "AFOLU_NO_REDD"],
        "D3  todo AFOLU+no-AFOLU (esquema vigente)": ["REDD_PLUS", "AFOLU_NO_REDD", "NO_AFOLU"],
    }

    resumen = []
    for etiqueta, clases in definiciones.items():
        sub = utilizables[utilizables["clase"].isin(clases)]
        cohortes = sub.groupby("COD_DANE")["anio_inicio"].min()
        en_panel = cohortes[(cohortes >= ANIO_PANEL_MIN) & (cohortes <= ANIO_PANEL_MAX)]
        estimables = cohortes[
            (cohortes >= COHORTE_MIN_ESTIMABLE) & (cohortes <= COHORTE_MAX_ESTIMABLE)
        ]
        resumen.append({
            "definicion": etiqueta,
            "proyectos": len(sub),
            "municipios": cohortes.size,
            "en_panel": en_panel.size,
            "estimables": estimables.size,
            "cohortes_distintas": estimables.nunique(),
        })

    print(pd.DataFrame(resumen).to_string(index=False))
    print(f"\n  'estimables' = cohorte en [{COHORTE_MIN_ESTIMABLE}, {COHORTE_MAX_ESTIMABLE}],")
    print("  es decir con al menos un periodo pre y uno post. Ese es el N que")
    print("  realmente entra a Callaway-Sant'Anna.")

    _linea("3. DISTRIBUCION DE COHORTES -- REDD+ ESTRICTO")

    redd = utilizables[utilizables["clase"] == "REDD_PLUS"]
    if redd.empty:
        print("\n  Ningun proyecto REDD+ con municipio y fecha. Revisa la seccion 1:")
        print("  lo mas probable es que falte el campo sectorial en alguna fuente.")
        return

    cohortes = redd.groupby("COD_DANE")["anio_inicio"].min()
    tabla = cohortes.value_counts().sort_index()
    print(tabla.to_string())

    chicas = tabla[(tabla.index >= COHORTE_MIN_ESTIMABLE)
                   & (tabla.index <= COHORTE_MAX_ESTIMABLE) & (tabla < 3)]
    if len(chicas):
        print(f"\n[!] {len(chicas)} cohorte(s) con menos de 3 municipios:")
        print(f"    {list(chicas.index)}")
        print("    Sus ATT(g,t) van a ser muy imprecisos. Considera agregarlas")
        print("    en bloques o justificar su exclusion.")

    _linea("4. GRUPO PLACEBO -- AFOLU NO REDD+")
    placebo = utilizables[utilizables["clase"] == "AFOLU_NO_REDD"]
    n_solo_placebo = set(placebo["COD_DANE"]) - set(redd["COD_DANE"])
    print(f"\n  Proyectos AFOLU no-REDD+: {len(placebo)}")
    print(f"  Municipios con AFOLU no-REDD+ y SIN REDD+: {len(n_solo_placebo)}")
    print("\n  Estos son el test de falsacion: su ATT deberia ser cero.")
    if len(n_solo_placebo) < 15:
        print("  [!] Menos de 15 municipios: el placebo tendra poca potencia.")
        print("      Reportalo como indicativo, no como evidencia concluyente.")


def main() -> None:
    sombra = RAIZ / "scripts/data/interim"
    if sombra.exists():
        print("\n[!] AVISO: existe la carpeta sombra scripts/data/interim/.")
        print("    Este script leyo de ./data/interim/. Si los interim buenos")
        print("    estan en la sombra, los conteos de abajo NO sirven.")
        print("    Unifica las rutas antes de tomar decisiones con esto.\n")

    ev = cargar_todo()
    if ev.empty:
        print("\nNo se cargo ninguna fuente. Revisa que estas corriendo el script")
        print("desde la raiz del proyecto (donde vive data/).")
        sys.exit(1)

    reportar(ev)

    OUT_DIAG.parent.mkdir(parents=True, exist_ok=True)
    ev.sort_values(["clase", "fuente", "nombre_proyecto"]).to_csv(
        OUT_DIAG, index=False, encoding="utf-8-sig"
    )
    _linea()
    print(f"Detalle por proyecto guardado en: {OUT_DIAG}")
    print("Revisa ahi las filas con clase=REVISAR antes de fijar el N definitivo.")


if __name__ == "__main__":
    main()
