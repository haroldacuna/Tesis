#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
34_enriquecer_verra_metodologia.py
==================================

Recupera el campo METHODOLOGY (y AFOLU ACTIVITIES) del export publico de la
VCS Project Database de Verra, lo pega al interino de Platts y contrasta el
resultado contra la clasificacion REDD+ hecha por TITULO.

PROBLEMA QUE RESUELVE
---------------------
El endpoint de Platts que usa 24_extraer_verra_platts.py NO expone
'methodology': solo 'sectoralScope' y 'projectType'. Por eso los 40 proyectos
REDD+ de Verra estan detectados por el NOMBRE del proyecto, sin campo
estructurado que los respalde, y quedan 16 AFOLU genericos + 9 por revisar
con sospecha de falsos negativos. Ese es el eslabon debil de la nueva
definicion de tratamiento (D1 REDD+ estricto).

El export publico del registro si trae 'Methodology' y 'AFOLU Activities'.
Con eso la clasificacion pasa a ser dato del registrador y no heuristica de
palabras clave -- el mismo estandar que ya se aplico a Gold Standard
(methodology) y RENARE (tipo).

USO
---
    # 0. Anotar la procedencia del export recien descargado (una sola vez)
    python 34_enriquecer_verra_metodologia.py --anotar-descarga

    # 1. Inspeccionar el export sin escribir nada (recomendado la 1a vez)
    python 34_enriquecer_verra_metodologia.py --explorar

    # 2. Corrida completa
    python 34_enriquecer_verra_metodologia.py

    # 3. Con fallback por nombre desactivado (solo union por ID)
    python 34_enriquecer_verra_metodologia.py --sin-fallback-nombre

ENTRADAS
--------
    data/raw/verra_registry_export.csv            (descarga manual)
    data/raw/verra_registry_export.fuente.json    (procedencia; lo crea --anotar-descarga)
    data/interim/verra_platts_colombia_con_municipio.csv

SALIDAS
-------
    data/interim/verra_con_metodologia.csv
        El interino de Platts + methodology, afolu_activities, clase_metodologia,
        metodo_match y score_match.
    data/interim/diagnostics/verra_metodologia_auditoria.csv
        UNA fila por proyecto con como se resolvio (o por que no). Este es el
        archivo que se cita en la seccion de limitaciones.
    data/interim/diagnostics/verra_contraste_titulo_vs_metodologia.csv
        Matriz de confusion titulo x metodologia, larga, para pegar en el
        Capitulo 5.
    data/interim/diagnostics/verra_fechas_discrepantes.csv
        projectStartDate (Platts) vs Crediting Period Start (export), para la
        regla de prelacion Verra > Cercarbono > RENARE.

NOTA DE CODIFICACION
--------------------
El script es ASCII puro a proposito (incluidos los prints). PowerShell en
Windows usa cp1252 por defecto y un print con tildes revienta con
UnicodeEncodeError a mitad de una corrida larga. Los CSV si se escriben en
utf-8-sig para que Excel los abra bien.
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

# ---------------------------------------------------------------------------
# 0. Resolucion de rutas -- guarda contra la carpeta sombra
#
# Sintoma conocido del repo: los scripts de Python con rutas relativas
# escriben en tesis_gfc/scripts/data/interim/ mientras R lee
# tesis_gfc/data/interim/. Aqui la raiz se ancla al ARCHIVO, no al cwd, y si
# se detecta la sombra el script aborta en vez de escribir en el lugar
# equivocado.
# ---------------------------------------------------------------------------
SCRIPT_DIR = Path(__file__).resolve().parent

# Nombre del archivo centinela. Marca CUAL de los arboles de datos es el
# bueno. Se crea una sola vez, a mano:
#     New-Item -ItemType File ...\tesis_gfc\data\.raiz_canonica -Force
# Un arbol sombra (scripts/data, un git worktree, una copia de respaldo) NO
# lo tiene, asi que el script se niega a escribir ahi.
CENTINELA = "data/.raiz_canonica"


def resolver_raiz(raiz_forzada: str | None = None) -> Path:
    """Sube directorios desde el script buscando el centinela.

    Por que no basta con buscar data/interim: en este repo llegaron a
    coexistir TRES arboles con esa estructura (tesis_gfc/data, el real;
    tesis_gfc/scripts/data, la sombra que crean las rutas relativas; y
    .claude/worktrees/.../tesis_gfc/data, del worktree de git). Cualquier
    heuristica basada en "el primero que tenga data/" elige mal la mitad de
    las veces y lo hace en silencio. El centinela es explicito y unico.
    """
    if raiz_forzada:
        r = Path(raiz_forzada).resolve()
        if not (r / CENTINELA).exists():
            raise SystemExit(f"[X] {r} no tiene {CENTINELA}. No es la raiz canonica.")
        return r

    candidatas = []
    for base in (SCRIPT_DIR, Path.cwd().resolve()):
        d = base
        while True:
            if (d / CENTINELA).exists():
                candidatas.append(d)
                break
            if d.parent == d:
                break
            d = d.parent

    unicas = sorted({str(c) for c in candidatas})
    if len(unicas) == 1:
        return Path(unicas[0])

    if not unicas:
        raise SystemExit(
            f"[X] No encuentro el centinela {CENTINELA} subiendo desde:\n"
            f"      script: {SCRIPT_DIR}\n"
            f"      cwd:    {Path.cwd().resolve()}\n"
            "    Creamelo en el arbol bueno y vuelve a correr:\n"
            "      New-Item -ItemType File <...>\\tesis_gfc\\data\\.raiz_canonica -Force\n"
            "    Si no sabes cual es el bueno, es el que lee 01_preparar_datos_did.R."
        )

    raise SystemExit(
        "[X] Dos arboles canonicos distintos segun desde donde se mire:\n    "
        + "\n    ".join(unicas)
        + "\n    Hay un centinela de mas. Borra el que no corresponda, o pasa --raiz."
    )


def inventariar_arboles(raiz: Path) -> list[Path]:
    """Busca otros arboles data/interim bajo el mismo repo. Solo informa:
    la decision de borrar es del usuario, nunca del script."""
    tope = raiz.parent
    otros = []
    try:
        for d in tope.rglob("data/interim"):
            if d.is_dir() and d.resolve() != (raiz / "data/interim").resolve():
                otros.append(d)
    except OSError:
        pass
    return otros


EXPORT = PROCEDENCIA = PLATTS = DIAG = SALIDA = AUDITORIA = CONTRASTE = FECHAS = Path(".")


def configurar_rutas(raiz: Path) -> None:
    """Fija todas las rutas de entrada/salida a partir de la raiz canonica.
    Se llama una sola vez, desde main(), despues de resolver la raiz."""
    global RAIZ, EXPORT, PROCEDENCIA, PLATTS, DIAG, SALIDA, AUDITORIA, CONTRASTE, FECHAS
    RAIZ = raiz
    EXPORT = raiz / "data/raw/verra_registry_export.csv"
    PROCEDENCIA = raiz / "data/raw/verra_registry_export.fuente.json"
    PLATTS = raiz / "data/interim/verra_platts_colombia_con_municipio.csv"
    DIAG = raiz / "data/interim/diagnostics"
    SALIDA = raiz / "data/interim/verra_con_metodologia.csv"
    AUDITORIA = DIAG / "verra_metodologia_auditoria.csv"
    CONTRASTE = DIAG / "verra_contraste_titulo_vs_metodologia.csv"
    FECHAS = DIAG / "verra_fechas_discrepantes.csv"


# ---------------------------------------------------------------------------
# 1. Normalizacion
# ---------------------------------------------------------------------------
def normalizar(texto) -> str:
    """Minusculas, sin acentos, sin puntuacion, sin espacios repetidos.
    Misma logica que filtro_sectorial.normalizar, ampliada para tolerar las
    diferencias de tipeo entre el export y Platts (guiones, 'REDD+' vs
    'REDD', comillas tipograficas)."""
    if pd.isna(texto):
        return ""
    t = unicodedata.normalize("NFKD", str(texto))
    t = "".join(c for c in t if not unicodedata.combining(c))
    t = t.lower().replace("+", " ").replace("&", " y ")
    t = re.sub(r"[^a-z0-9\s]", " ", t)
    return re.sub(r"\s+", " ", t).strip()


def normalizar_id(valor) -> str:
    """Los IDs de Verra viajan de tres formas distintas segun la fuente:
        1234        (entero)
        1234.0      (pandas lo leyo como float por los NaN de la columna)
        VCS1234     (prefijo del ecosistema OffsetsDB / Toucan)
    Se reducen todas a los digitos. Cadena vacia = no hay ID utilizable."""
    if pd.isna(valor):
        return ""
    s = str(valor).strip()
    s = re.sub(r"\.0+$", "", s)          # 1234.0 -> 1234
    s = re.sub(r"(?i)^vcs[\s_-]*", "", s)  # VCS1234 -> 1234
    digitos = re.sub(r"\D", "", s)
    return digitos.lstrip("0")           # 0123 == 123


# ---------------------------------------------------------------------------
# 2. Deteccion automatica de columnas
#
# No se asume el nombre de ninguna columna: el export de Verra ha cambiado de
# esquema al menos dos veces (registro clasico -> Platts) y el interino de
# Platts viene de json_normalize, asi que puede traer prefijos anidados
# (ej. 'project.resourceIdentifier'). Se busca por patron y se VALIDA que la
# columna elegida efectivamente solape con la otra fuente.
# ---------------------------------------------------------------------------
PATRONES_ID = [
    r"^resourceidentifier$", r"^projectid$", r"^project_id$",
    r"^id$", r"\bproject\s*id\b", r"resourceidentifier", r"\bvcs\s*id\b",
]
PATRONES_NOMBRE = [
    r"^projectname$", r"^resourcename$", r"^name$",
    r"\bproject\s*name\b", r"^nombre_proyecto$", r"projectname",
]
PATRONES_METODOLOGIA = [r"methodolog", r"\bprotocol\b"]
PATRONES_AFOLU = [r"afolu", r"activit"]
# 'Crediting Period Duration Start Date' lleva 'Duration' en medio, asi que
# el patron tiene que ser laxo entre 'period' y 'start'.
PATRONES_FECHA_EXPORT = [
    r"crediting\s*period.*start", r"creditingperiod.*start",
    r"estimated\s*project\s*start",
]


def elegir_columna(df: pd.DataFrame, patrones: list[str]) -> str | None:
    """Primera columna cuyo nombre normalizado casa con alguno de los patrones,
    en el orden en que se pasan (el orden ES la prioridad)."""
    cols = {c: re.sub(r"[^a-z0-9\s]", "", str(c).lower()) for c in df.columns}
    for p in patrones:
        for original, limpio in cols.items():
            if re.search(p, limpio):
                return original
    return None


def elegir_id_validado(
    df_a: pd.DataFrame, df_b: pd.DataFrame, etiqueta_a: str, etiqueta_b: str
) -> tuple[str | None, str | None, int]:
    """Elige la pareja de columnas de ID que MAXIMIZA el solape real.

    Preferir el primer nombre que casa con el patron es fragil: 'id' puede
    ser un correlativo interno de Platts que no tiene nada que ver con el ID
    publico de Verra. Aqui se prueban todas las columnas candidatas de ambos
    lados y se elige la pareja con mas coincidencias efectivas. Si el mejor
    solape es 0, se devuelve None y el script cae al fallback por nombre
    reportandolo -- nunca en silencio.
    """
    def candidatas(df: pd.DataFrame) -> list[str]:
        out = []
        for c in df.columns:
            limpio = re.sub(r"[^a-z0-9\s]", "", str(c).lower())
            if any(re.search(p, limpio) for p in PATRONES_ID):
                out.append(c)
        return out

    cand_a, cand_b = candidatas(df_a), candidatas(df_b)
    print(f"  Candidatas de ID en {etiqueta_a}: {cand_a}")
    print(f"  Candidatas de ID en {etiqueta_b}: {cand_b}")

    mejor = (None, None, 0)
    for ca in cand_a:
        sa = set(df_a[ca].map(normalizar_id)) - {""}
        for cb in cand_b:
            sb = set(df_b[cb].map(normalizar_id)) - {""}
            solape = len(sa & sb)
            if solape > mejor[2]:
                mejor = (ca, cb, solape)
    return mejor


# ---------------------------------------------------------------------------
# 3. Clasificacion de metodologias VCS
#
# El criterio es el MECANISMO, no el sector. El outcome del panel es PERDIDA
# de cobertura (Hansen/GFC), asi que solo las metodologias de deforestacion o
# degradacion EVITADA operan sobre el mismo margen que el outcome. ARR
# (restauracion) e IFM (manejo forestal) son AFOLU pero suman cobertura o
# cambian rotaciones: su efecto esperado sobre la perdida bruta no es el
# mismo y no deben entrar a D1.
#
# VERIFICAR esta tabla contra el catalogo oficial antes de la sustentacion:
# https://verra.org/methodologies-main/  (catalogo de metodologias VCS)
# Si una metodologia no esta aqui, el proyecto cae en PENDIENTE y hay que
# resolverlo a mano; el script no adivina.
# ---------------------------------------------------------------------------
REDD_ESTRICTO = {
    "VM0004": "Conversion evitada en turberas (APD)",
    "VM0006": "REDD mosaico / escala de paisaje",
    "VM0007": "REDD+ Methodology Framework (REDD-MF)",
    "VM0009": "Conversion evitada de ecosistemas (AUDD)",
    "VM0011": "Degradacion planificada evitada",
    "VM0015": "Deforestacion no planificada evitada (AUD)",
    "VM0029": "Degradacion evitada - lena",
    "VM0037": "REDD+ en paisajes de deforestacion en mosaico",
    "VM0048": "REDD consolidada (marco 2023)",
    "VMD0055": "Modulo de linea base de deforestacion (VM0048)",
}

AFOLU_NO_REDD = {
    "VM0003": "IFM - extension de turno",
    "VM0005": "IFM - conversion de bosque de baja productividad",
    "VM0010": "IFM - de bosque maderable a bosque protegido",
    "VM0012": "IFM - bosques templados y boreales",
    "VM0017": "ALM - manejo de suelos agricolas",
    "VM0021": "ALM - carbono en suelo",
    "VM0022": "ALM - fertilizacion",
    "VM0026": "ALM - pastizales sostenibles",
    "VM0032": "ARR - revegetacion de arbustales",
    "VM0033": "WRC - restauracion de humedales costeros",
    "VM0035": "IFM - bosques templados",
    "VM0042": "ALM - manejo agricola mejorado",
    "VM0045": "IFM - emparejamiento dinamico",
    "VM0047": "ARR - forestacion, reforestacion y revegetacion",
    "AR-ACM0003": "CDM A/R - forestacion y reforestacion",
    "AR-AMS0007": "CDM A/R de pequena escala",
    "AR-ACM0001": "CDM A/R - tierras degradadas",
}

# Valores del campo 'AFOLU Activities' del export. Es el campo mas limpio del
# archivo y se usa como segunda evidencia, cruzada contra el codigo.
AFOLU_ACT_REDD = {"redd", "redd+", "acogs", "avoided conversion", "wrc redd"}
AFOLU_ACT_NO_REDD = {"arr", "ifm", "alm", "wrc", "arr ifm", "ifm arr"}

# ---------------------------------------------------------------------------
# Escalera de estado del registro
#
# El export trae 'Status', que Platts no exponia. De los proyectos REDD+ de
# Colombia una parte esta retirada, rechazada, inactiva o apenas en pipeline
# listing: tratarlos como tratados asigna tratamiento a municipios donde el
# proyecto no llego a operar o fue revertido, lo que atenua el ATT hacia
# cero.
#
# No se excluyen en bloque -- un proyecto que acredito desde 2013 y se
# retiro en 2022 si intervino el territorio. Se construyen tres anillos
# anidados para correr la misma especificacion en los tres y reportar la
# sensibilidad:
#
#     S1  cualquier aparicion en el registro   (definicion vigente)
#     S2  registro efectivo                    (paso de validacion)
#     S3  solo 'Registered'                    (nucleo duro)
#
# Contraste recomendado: cruzar S2/S3 con unitsIssued > 0 del interino de
# Platts, que es evidencia de operacion real y no de tramite.
# ---------------------------------------------------------------------------
ESTADO_S3 = {"registered"}
ESTADO_S2 = ESTADO_S3 | {
    "late to verify",
    "verification approval requested",
    "registration and verification approval requested",
    "units transferred from approved ghg program",
}


def tier_estado(status) -> str:
    s = normalizar(status)
    if s in {normalizar(x) for x in ESTADO_S3}:
        return "S3"
    if s in {normalizar(x) for x in ESTADO_S2}:
        return "S2"
    return "S1"


ETIQUETAS = ["REDD_ESTRICTO", "AFOLU_NO_REDD", "NO_AFOLU", "PENDIENTE"]


def extraer_codigos(texto) -> list[str]:
    """Saca los codigos de metodologia de un campo de texto libre.

    El export escribe la metodologia como 'VM0007 (Version NA)' y puede
    listar varias separadas por coma ('ACM0002 (Version NA), AMS-I.D.
    (Version NA)'), asi que devuelve lista.

    Tres formas conviven en el archivo real:
        VM0007, VM0015, VM0042   metodologias VCS
        AR-ACM0003               CDM de forestacion/reforestacion
        ACM0002, AMS-I.D.,       CDM sectorial (energia, residuos):
        AMS-II.D., AMS-III.G.    ojo con la notacion con puntos, que un
                                 regex de solo digitos deja pasar.
    """
    if pd.isna(texto):
        return []
    s = str(texto).upper()
    codigos: list[str] = []
    codigos += re.findall(r"\bVMD?\d{4}\b", s)
    codigos += [m.replace(" ", "").replace("ARACM", "AR-ACM")
                for m in re.findall(r"\bAR[\s-]?ACM\d{4}\b", s)]
    codigos += re.findall(r"\bACM\d{4}\b", s)
    codigos += re.findall(r"\bAMS[\s-]?[IVX]+\.[A-Z]\.?", s)   # AMS-I.D.
    codigos += re.findall(r"\bAM\d{4}\b", s)
    codigos = sorted(set(codigos))
    # 'AR-ACM0003' contiene 'ACM0003' y el segundo regex lo captura aparte.
    # Se descarta el hijo para no clasificar un proyecto A/R como CDM
    # sectorial.
    if any(c.startswith("AR-ACM") for c in codigos):
        hijos = {c[3:] for c in codigos if c.startswith("AR-ACM")}
        codigos = [c for c in codigos if c not in hijos]
    return codigos


def clasificar_metodologia(metodologia, afolu_activities) -> tuple[str, str]:
    """Devuelve (etiqueta, motivo). La jerarquia de evidencia es:
        1. codigo de metodologia reconocido      -> decide
        2. campo AFOLU Activities                -> decide
        3. cualquier otro codigo VM/CDM          -> NO_AFOLU (energia, residuos)
        4. nada                                  -> PENDIENTE (falla ruidoso)
    """
    codigos = extraer_codigos(metodologia)

    hit_redd = [c for c in codigos if c in REDD_ESTRICTO]
    if hit_redd:
        return "REDD_ESTRICTO", f"metodologia {'/'.join(hit_redd)}"

    hit_otros = [c for c in codigos if c in AFOLU_NO_REDD]
    if hit_otros:
        return "AFOLU_NO_REDD", f"metodologia {'/'.join(hit_otros)}"

    act = normalizar(afolu_activities)
    if act:
        if any(a in act.split() or a == act for a in AFOLU_ACT_REDD):
            return "REDD_ESTRICTO", f"AFOLU Activities = {act}"
        if any(a in act.split() or a == act for a in AFOLU_ACT_NO_REDD):
            return "AFOLU_NO_REDD", f"AFOLU Activities = {act}"

    if codigos:
        # Codigo valido pero fuera de las dos tablas AFOLU: es sectorial de
        # energia, residuos o transporte.
        return "NO_AFOLU", f"metodologia {'/'.join(codigos)} fuera de AFOLU"

    return "PENDIENTE", "sin metodologia legible en el export"


# ---------------------------------------------------------------------------
# 4. Heuristica por TITULO -- se reconstruye aqui a proposito
#
# Es la clasificacion que se quiere AUDITAR, asi que tiene que ser explicita
# y reproducible dentro de este mismo script. Si ya existe un CSV con la
# clasificacion por titulo, se puede pasar con --clasificacion y el script lo
# usa en vez de recalcular.
# ---------------------------------------------------------------------------
PALABRAS_REDD = [
    "redd", "deforestacion evitada", "avoided deforestation",
    "conservacion de bosque", "forest conservation", "conservation",
]


def redd_por_titulo(nombre) -> bool:
    n = normalizar(nombre)
    return any(p in n for p in PALABRAS_REDD)


# ---------------------------------------------------------------------------
# 5. Procedencia de la descarga
# ---------------------------------------------------------------------------
def anotar_descarga(nota: str | None = None) -> int:
    if not EXPORT.exists():
        print(f"[X] No existe {EXPORT}. Descarga el export primero.")
        return 1
    meta = {
        "archivo": EXPORT.name,
        "fuente": "https://registry.verra.org/app/search/VCS (Project Database, filtro Country/Area = Colombia)",
        "fecha_descarga_utc": datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC"),
        "bytes": EXPORT.stat().st_size,
        "filas": int(sum(1 for _ in EXPORT.open(encoding="utf-8-sig", errors="replace")) - 1),
        "descargado_por": "manual (navegador)",
        "nota": nota or "",
    }
    PROCEDENCIA.parent.mkdir(parents=True, exist_ok=True)
    PROCEDENCIA.write_text(json.dumps(meta, indent=2, ensure_ascii=True), encoding="utf-8")
    print(f"Procedencia anotada en {PROCEDENCIA}:")
    print(json.dumps(meta, indent=2, ensure_ascii=True))
    return 0


def leer_procedencia() -> dict:
    if not PROCEDENCIA.exists():
        raise SystemExit(
            f"[X] Falta {PROCEDENCIA}.\n"
            "    El export no tiene fecha de descarga anotada y este dato entra\n"
            "    a la tesis. Corre primero:\n"
            "        python 34_enriquecer_verra_metodologia.py --anotar-descarga"
        )
    return json.loads(PROCEDENCIA.read_text(encoding="utf-8"))


# ---------------------------------------------------------------------------
# 6. Exploracion
# ---------------------------------------------------------------------------
def explorar() -> int:
    for ruta in (EXPORT, PLATTS):
        if not ruta.exists():
            print(f"[X] Falta {ruta}")
            return 1
    exp = pd.read_csv(EXPORT, low_memory=False)
    plt = pd.read_csv(PLATTS, low_memory=False)

    print(f"\nEXPORT  {EXPORT}  ->  {len(exp)} filas")
    print(f"  columnas: {list(exp.columns)}")
    print(f"\nPLATTS  {PLATTS}  ->  {len(plt)} filas")
    print(f"  columnas: {list(plt.columns)}")

    print("\n--- Deteccion de llave de union ---")
    ca, cb, solape = elegir_id_validado(exp, plt, "export", "platts")
    print(f"  Mejor pareja: export['{ca}'] x platts['{cb}']  ->  {solape} IDs en comun")

    col_met = elegir_columna(exp, PATRONES_METODOLOGIA)
    col_afo = elegir_columna(exp, PATRONES_AFOLU)
    print(f"\n  Columna de metodologia en el export: {col_met}")
    print(f"  Columna de AFOLU Activities:         {col_afo}")
    if col_met:
        print("\n  Valores mas frecuentes de metodologia:")
        print(exp[col_met].value_counts().head(20).to_string())
    if col_afo:
        print("\n  Valores de AFOLU Activities:")
        print(exp[col_afo].value_counts().head(20).to_string())
    return 0


# ---------------------------------------------------------------------------
# 7. Union
# ---------------------------------------------------------------------------
def unir(
    plt: pd.DataFrame, exp: pd.DataFrame, usar_fallback: bool, umbral: float
) -> pd.DataFrame:
    """Devuelve plt con columnas _met, _afo, metodo_match, score_match.

    Estrategia en cascada, cada escalon reportado:
        A. ID exacto (normalizado)
        B. nombre normalizado exacto
        C. nombre por similitud difflib >= umbral   [marcado para revision]
    """
    col_id_exp, col_id_plt, solape = elegir_id_validado(exp, plt, "export", "platts")
    col_nom_exp = elegir_columna(exp, PATRONES_NOMBRE)
    col_nom_plt = elegir_columna(plt, PATRONES_NOMBRE)
    col_met = elegir_columna(exp, PATRONES_METODOLOGIA)
    col_afo = elegir_columna(exp, PATRONES_AFOLU)
    col_fec_exp = elegir_columna(exp, PATRONES_FECHA_EXPORT)
    col_status = elegir_columna(exp, [r"^status$", r"\bstatus\b", r"estado"])

    if col_met is None:
        raise SystemExit(
            "[X] El export no tiene ninguna columna que parezca 'Methodology'.\n"
            f"    Columnas vistas: {list(exp.columns)}\n"
            "    Es posible que hayas bajado el reporte de CREDITOS y no el de\n"
            "    PROYECTOS. Vuelve a la pestana 'Projects' antes de exportar."
        )
    if col_nom_exp is None or col_nom_plt is None:
        raise SystemExit("[X] No encuentro columna de nombre de proyecto en alguna de las dos fuentes.")

    print(f"\n  Llave por ID:        export['{col_id_exp}'] x platts['{col_id_plt}']")
    print(f"  Llave por nombre:    export['{col_nom_exp}'] x platts['{col_nom_plt}']")
    print(f"  Metodologia:         export['{col_met}']")
    print(f"  AFOLU Activities:    export['{col_afo}']")
    print(f"  Fecha de acreditacion: export['{col_fec_exp}']")
    print(f"  Estado del registro:   export['{col_status}']")

    exp = exp.copy()
    plt = plt.copy()
    exp["_key_id"] = exp[col_id_exp].map(normalizar_id) if col_id_exp else ""
    plt["_key_id"] = plt[col_id_plt].map(normalizar_id) if col_id_plt else ""

    # Si una fila del export no tiene ID utilizable (o no hay columna de ID),
    # se le pone una clave sintetica unica. Sin esto, todas las filas sin ID
    # colapsarian en UNA sola en el groupby de abajo y el fallback por nombre
    # se quedaria sin universo contra que comparar -- justo cuando mas se
    # necesita. Las claves sinteticas nunca casan con Platts, asi que no
    # producen falsos matches por ID.
    sin_id = exp["_key_id"] == ""
    if sin_id.any():
        motivo = ("no hay columna de ID pareable entre las dos fuentes"
                  if not (col_id_exp and col_id_plt) else "ID vacio o ilegible")
        print(f"  [i] {int(sin_id.sum())} filas del export con clave sintetica ({motivo}).")
        exp.loc[sin_id, "_key_id"] = [f"__sin_id_{i}" for i in range(int(sin_id.sum()))]
    exp["_key_nom"] = exp[col_nom_exp].map(normalizar)
    plt["_key_nom"] = plt[col_nom_plt].map(normalizar)

    # El export puede traer varias filas por proyecto (una por periodo de
    # acreditacion). Se colapsa a una fila por ID concatenando metodologias
    # distintas, para no inflar el panel en el merge.
    agr = {col_met: lambda s: "; ".join(sorted({str(x) for x in s.dropna()}))}
    if col_afo:
        agr[col_afo] = lambda s: "; ".join(sorted({str(x) for x in s.dropna()}))
    if col_fec_exp:
        agr[col_fec_exp] = "min"
    if col_status:
        agr[col_status] = "first"
    agr[col_nom_exp] = "first"
    agr["_key_nom"] = "first"

    dups = exp["_key_id"].duplicated().sum()
    if dups:
        print(f"  [i] El export trae {dups} filas duplicadas por ID; se colapsan a una por proyecto.")
    exp_u = (
        exp[exp["_key_id"] != ""]
        .groupby("_key_id", as_index=False)
        .agg(agr)
    )

    plt["_met"] = pd.NA
    plt["_afo"] = pd.NA
    plt["_fec_exp"] = pd.NA
    plt["_nom_exp"] = pd.NA
    plt["_status"] = pd.NA
    plt["metodo_match"] = "sin_match"
    plt["score_match"] = pd.NA

    # --- A. ID exacto ------------------------------------------------------
    mapa_met = dict(zip(exp_u["_key_id"], exp_u[col_met]))
    mapa_afo = dict(zip(exp_u["_key_id"], exp_u[col_afo])) if col_afo else {}
    mapa_fec = dict(zip(exp_u["_key_id"], exp_u[col_fec_exp])) if col_fec_exp else {}
    mapa_nom = dict(zip(exp_u["_key_id"], exp_u[col_nom_exp]))
    mapa_sta = dict(zip(exp_u["_key_id"], exp_u[col_status])) if col_status else {}

    hit_id = plt["_key_id"].isin(mapa_met.keys()) & (plt["_key_id"] != "")
    plt.loc[hit_id, "_met"] = plt.loc[hit_id, "_key_id"].map(mapa_met)
    plt.loc[hit_id, "_afo"] = plt.loc[hit_id, "_key_id"].map(mapa_afo)
    plt.loc[hit_id, "_fec_exp"] = plt.loc[hit_id, "_key_id"].map(mapa_fec)
    plt.loc[hit_id, "_nom_exp"] = plt.loc[hit_id, "_key_id"].map(mapa_nom)
    plt.loc[hit_id, "_status"] = plt.loc[hit_id, "_key_id"].map(mapa_sta)
    plt.loc[hit_id, "metodo_match"] = "id"
    plt.loc[hit_id, "score_match"] = 1.0
    print(f"\n  [A] Match por ID:            {int(hit_id.sum())} / {len(plt)}")

    if usar_fallback:
        # --- B. nombre normalizado exacto ---------------------------------
        mapa_nom_met = dict(zip(exp_u["_key_nom"], exp_u[col_met]))
        mapa_nom_afo = dict(zip(exp_u["_key_nom"], exp_u[col_afo])) if col_afo else {}
        mapa_nom_fec = dict(zip(exp_u["_key_nom"], exp_u[col_fec_exp])) if col_fec_exp else {}
        mapa_nom_nom = dict(zip(exp_u["_key_nom"], exp_u[col_nom_exp]))
        mapa_nom_sta = dict(zip(exp_u["_key_nom"], exp_u[col_status])) if col_status else {}

        pend = plt["metodo_match"] == "sin_match"
        hit_nom = pend & plt["_key_nom"].isin(mapa_nom_met.keys()) & (plt["_key_nom"] != "")
        plt.loc[hit_nom, "_met"] = plt.loc[hit_nom, "_key_nom"].map(mapa_nom_met)
        plt.loc[hit_nom, "_afo"] = plt.loc[hit_nom, "_key_nom"].map(mapa_nom_afo)
        plt.loc[hit_nom, "_fec_exp"] = plt.loc[hit_nom, "_key_nom"].map(mapa_nom_fec)
        plt.loc[hit_nom, "_nom_exp"] = plt.loc[hit_nom, "_key_nom"].map(mapa_nom_nom)
        plt.loc[hit_nom, "_status"] = plt.loc[hit_nom, "_key_nom"].map(mapa_nom_sta)
        plt.loc[hit_nom, "metodo_match"] = "nombre_exacto"
        plt.loc[hit_nom, "score_match"] = 1.0
        print(f"  [B] Match por nombre exacto: {int(hit_nom.sum())}")

        # --- C. similitud -------------------------------------------------
        universo = [k for k in exp_u["_key_nom"].tolist() if k]
        n_fuzzy = 0
        for i in plt.index[plt["metodo_match"] == "sin_match"]:
            clave = plt.at[i, "_key_nom"]
            if not clave:
                continue
            cercanos = difflib.get_close_matches(clave, universo, n=1, cutoff=umbral)
            if not cercanos:
                continue
            c = cercanos[0]
            plt.at[i, "_met"] = mapa_nom_met.get(c)
            plt.at[i, "_afo"] = mapa_nom_afo.get(c)
            plt.at[i, "_fec_exp"] = mapa_nom_fec.get(c)
            plt.at[i, "_nom_exp"] = mapa_nom_nom.get(c)
            plt.at[i, "_status"] = mapa_nom_sta.get(c)
            plt.at[i, "metodo_match"] = "nombre_similitud"
            plt.at[i, "score_match"] = round(
                difflib.SequenceMatcher(None, clave, c).ratio(), 3
            )
            n_fuzzy += 1
        print(f"  [C] Match por similitud (>= {umbral}): {n_fuzzy}  <- REQUIEREN REVISION MANUAL")
    else:
        print("  [B/C] Fallback por nombre DESACTIVADO por --sin-fallback-nombre")

    sin = int((plt["metodo_match"] == "sin_match").sum())
    tasa = 100 * (len(plt) - sin) / len(plt) if len(plt) else 0
    print(f"\n  TASA DE MATCH TOTAL: {len(plt) - sin}/{len(plt)} = {tasa:.1f}%   (sin match: {sin})")

    # Proyectos del export que NO aparecen en Platts: son la otra cara del
    # error de cobertura y hay que reportarlos igual.
    if col_id_exp and col_id_plt:
        ids_plt = set(plt["_key_id"]) - {""}
        huerfanos = exp_u[~exp_u["_key_id"].isin(ids_plt)]
    else:
        # Sin llave de ID el conteo de huerfanos solo puede hacerse por nombre.
        noms_plt = set(plt["_key_nom"]) - {""}
        huerfanos = exp_u[~exp_u["_key_nom"].isin(noms_plt)]
    print(f"  Proyectos en el export SIN contraparte en Platts: {len(huerfanos)}")
    if len(huerfanos):
        print("    (revisalos: pueden ser proyectos colombianos que la API de Platts no devolvio)")

    plt.attrs["col_nom_plt"] = col_nom_plt
    plt.attrs["huerfanos"] = huerfanos
    return plt


# ---------------------------------------------------------------------------
# 8. Contraste titulo vs metodologia
# ---------------------------------------------------------------------------
def contrastar(plt: pd.DataFrame, col_nom: str, clasificacion: Path | None) -> pd.DataFrame:
    if clasificacion and clasificacion.exists():
        tabla = pd.read_csv(clasificacion, dtype=str, encoding="utf-8-sig").fillna("")
        col_c = next((c for c in tabla.columns if "redd" in c.lower()), None)
        if col_c:
            mapa = {
                normalizar(n): str(v).strip().upper().startswith("REDD")
                for n, v in zip(tabla.get("nombre_proyecto", []), tabla[col_c])
            }
            plt["redd_titulo"] = plt[col_nom].map(lambda n: mapa.get(normalizar(n), redd_por_titulo(n)))
            print(f"\n  Clasificacion por titulo leida de {clasificacion} (columna '{col_c}')")
        else:
            plt["redd_titulo"] = plt[col_nom].map(redd_por_titulo)
    else:
        plt["redd_titulo"] = plt[col_nom].map(redd_por_titulo)
        print("\n  Clasificacion por titulo recalculada con la heuristica interna del script.")

    plt["clase_titulo"] = plt["redd_titulo"].map({True: "REDD_por_titulo", False: "no_REDD_por_titulo"})

    tab = (
        plt.groupby(["clase_titulo", "clase_metodologia"], dropna=False)
        .size()
        .reset_index(name="n")
        .sort_values(["clase_titulo", "n"], ascending=[True, False])
    )

    print("\n" + "=" * 72)
    print("CONTRASTE: clasificacion por TITULO  x  clasificacion por METODOLOGIA")
    print("=" * 72)
    print(pd.crosstab(plt["clase_titulo"], plt["clase_metodologia"], margins=True).to_string())

    confirmados = int(((plt["clase_titulo"] == "REDD_por_titulo") & (plt["clase_metodologia"] == "REDD_ESTRICTO")).sum())
    desmentidos = int(((plt["clase_titulo"] == "REDD_por_titulo") & (plt["clase_metodologia"].isin(["AFOLU_NO_REDD", "NO_AFOLU"]))).sum())
    rescatados = int(((plt["clase_titulo"] == "no_REDD_por_titulo") & (plt["clase_metodologia"] == "REDD_ESTRICTO")).sum())
    pendientes = int((plt["clase_metodologia"] == "PENDIENTE").sum())

    print("\n  FALSOS POSITIVOS del titulo (decia REDD+, la metodologia dice que no): "
          f"{desmentidos}")
    print(f"  FALSOS NEGATIVOS del titulo (no decia REDD+, la metodologia dice que si): {rescatados}")
    print(f"  CONFIRMADOS:                                                             {confirmados}")
    print(f"  SIN RESOLVER (entran a revision manual, no al tratamiento):              {pendientes}")
    print(
        "\n  Lectura: cada falso negativo es un municipio que hoy esta en el grupo de\n"
        "  CONTROL estando tratado -- atenua el ATT hacia cero, la misma direccion\n"
        "  del nulo reportado. Cada falso positivo fija una cohorte espuria."
    )
    return tab


# ---------------------------------------------------------------------------
# 9. Main
# ---------------------------------------------------------------------------
def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--explorar", action="store_true", help="solo inspeccionar esquemas y llave de union")
    ap.add_argument("--anotar-descarga", action="store_true", help="escribir el sidecar de procedencia")
    ap.add_argument("--nota", default=None, help="nota libre para el sidecar de procedencia")
    ap.add_argument("--sin-fallback-nombre", action="store_true", help="unir solo por ID")
    ap.add_argument("--umbral", type=float, default=0.92, help="cutoff de similitud para el fallback (0-1)")
    ap.add_argument("--clasificacion", type=Path, default=None, help="CSV con la clasificacion REDD+ por titulo")
    ap.add_argument("--raiz", default=None, help="forzar la raiz canonica (debe contener data/.raiz_canonica)")
    args = ap.parse_args()

    print("34_enriquecer_verra_metodologia.py")
    raiz = resolver_raiz(args.raiz)
    configurar_rutas(raiz)
    print(f"Raiz canonica: {raiz}   (centinela {CENTINELA} verificado)")

    otros = inventariar_arboles(raiz)
    if otros:
        print("\n[!] Existen otros arboles de datos bajo el mismo repo:")
        for d in otros:
            n = sum(1 for _ in d.rglob("*") if _.is_file())
            print(f"      {d}   ({n} archivos)")
        print("    Este script NO escribe en ellos. Consolida antes de propagar"
              " la reclasificacion al panel.\n")

    if args.anotar_descarga:
        return anotar_descarga(args.nota)
    if args.explorar:
        return explorar()

    for ruta in (EXPORT, PLATTS):
        if not ruta.exists():
            print(f"[X] Falta {ruta}")
            return 1

    meta = leer_procedencia()
    print(f"Export descargado el {meta['fecha_descarga_utc']} ({meta['filas']} filas)")

    exp = pd.read_csv(EXPORT, low_memory=False)
    plt = pd.read_csv(PLATTS, low_memory=False)
    print(f"Export: {len(exp)} filas  |  Platts interino: {len(plt)} filas")

    print("\n--- Union ---")
    plt = unir(plt, exp, usar_fallback=not args.sin_fallback_nombre, umbral=args.umbral)
    col_nom = plt.attrs["col_nom_plt"]
    huerfanos = plt.attrs["huerfanos"]

    clases = plt.apply(lambda r: clasificar_metodologia(r["_met"], r["_afo"]), axis=1, result_type="expand")
    plt["clase_metodologia"] = clases[0]
    plt["motivo_metodologia"] = clases[1]
    plt = plt.rename(columns={"_met": "methodology", "_afo": "afolu_activities",
                              "_status": "status_registro"})

    # Escalera de estado y bandera de blue carbon.
    plt["estado_registro"] = plt["status_registro"].map(tier_estado)
    plt.loc[plt["status_registro"].isna(), "estado_registro"] = "sin_estado"
    plt["es_wrc"] = plt["afolu_activities"].fillna("").str.upper().str.contains("WRC")

    print("\n--- Clasificacion por metodologia ---")
    print(plt["clase_metodologia"].value_counts().to_string())

    redd = plt[plt["clase_metodologia"] == "REDD_ESTRICTO"]
    if len(redd):
        print("\n--- Escalera de estado sobre los REDD+ ---")
        print(redd["status_registro"].value_counts(dropna=False).to_string())
        n1, n2, n3 = len(redd), int((redd["estado_registro"] != "S1").sum()), int((redd["estado_registro"] == "S3").sum())
        print(f"\n  S1 cualquier aparicion : {n1}")
        print(f"  S2 registro efectivo   : {n2}")
        print(f"  S3 solo 'Registered'   : {n3}")
        print(f"  Blue carbon (WRC)      : {int(redd['es_wrc'].sum())}  <- sensibilidad aparte: Hansen captura manglar de forma parcial")
        print("  Corre la misma especificacion en los tres anillos y reporta la sensibilidad.")

    tab = contrastar(plt, col_nom, args.clasificacion)

    # --- Fechas: Platts vs export -----------------------------------------
    col_fec_plt = next((c for c in plt.columns if "projectstartdate" in str(c).lower()), None)
    if col_fec_plt and plt["_fec_exp"].notna().any():
        cmp_fechas = plt[[col_nom, col_fec_plt, "_fec_exp", "metodo_match"]].copy()
        cmp_fechas["anio_platts"] = pd.to_datetime(cmp_fechas[col_fec_plt], errors="coerce").dt.year
        cmp_fechas["anio_export"] = pd.to_datetime(cmp_fechas["_fec_exp"], errors="coerce").dt.year
        discrep = cmp_fechas[
            cmp_fechas["anio_platts"].notna()
            & cmp_fechas["anio_export"].notna()
            & (cmp_fechas["anio_platts"] != cmp_fechas["anio_export"])
        ]
        DIAG.mkdir(parents=True, exist_ok=True)
        discrep.to_csv(FECHAS, index=False, encoding="utf-8-sig")
        print(f"\n  Discrepancias de anio Platts vs export: {len(discrep)}  ->  {FECHAS}")
        print("  (relevante para la regla de prelacion Verra > Cercarbono > RENARE)")

    # --- Salidas -----------------------------------------------------------
    DIAG.mkdir(parents=True, exist_ok=True)
    SALIDA.parent.mkdir(parents=True, exist_ok=True)

    plt.drop(columns=[c for c in ["_key_id", "_key_nom"] if c in plt.columns]).to_csv(
        SALIDA, index=False, encoding="utf-8-sig"
    )

    cols_aud = [c for c in [col_nom, "_nom_exp", "methodology", "afolu_activities",
                            "clase_metodologia", "motivo_metodologia", "clase_titulo",
                            "status_registro", "estado_registro", "es_wrc",
                            "metodo_match", "score_match", "COD_DANE", "NOMBRE_MPI"]
                if c in plt.columns]
    auditoria = plt[cols_aud].drop_duplicates()
    auditoria["requiere_revision"] = (
        auditoria["metodo_match"].isin(["nombre_similitud", "sin_match"])
        | (auditoria["clase_metodologia"] == "PENDIENTE")
        | (auditoria["clase_titulo"].eq("REDD_por_titulo") & auditoria["clase_metodologia"].ne("REDD_ESTRICTO"))
        | (auditoria["clase_titulo"].eq("no_REDD_por_titulo") & auditoria["clase_metodologia"].eq("REDD_ESTRICTO"))
    )
    auditoria.sort_values(["requiere_revision", "clase_metodologia"], ascending=[False, True]).to_csv(
        AUDITORIA, index=False, encoding="utf-8-sig"
    )
    tab.to_csv(CONTRASTE, index=False, encoding="utf-8-sig")
    if len(huerfanos):
        huerfanos.to_csv(DIAG / "verra_export_sin_contraparte_platts.csv", index=False, encoding="utf-8-sig")

    print("\n" + "=" * 72)
    print(f"Escrito: {SALIDA}")
    print(f"Escrito: {AUDITORIA}   ({int(auditoria['requiere_revision'].sum())} filas marcadas para revision)")
    print(f"Escrito: {CONTRASTE}")
    print("=" * 72)
    print("""
SIGUIENTE PASO
  1. Abre la auditoria y resuelve las filas con requiere_revision = True.
  2. Lleva las decisiones a data/interim/clasificacion_sectorial.csv
     (columna clasificacion_final) -- este script NO la sobreescribe a
     proposito: las decisiones manuales previas mandan.
  3. Vuelve a correr 28_clasificar_sector.py y luego
     26_consolidar_fuentes_carbono.py DESDE tesis_gfc/, verificando que el
     conteo de tratados coincida con lo que despues lee 01_preparar_datos_did.R.
""")
    return 0


if __name__ == "__main__":
    sys.exit(main())
