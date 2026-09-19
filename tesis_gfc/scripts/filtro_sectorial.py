# -*- coding: utf-8 -*-
"""
filtro_sectorial.py
===================

Clasificacion de los proyectos de carbono y filtros. Este modulo NO se
ejecuta solo: lo importan 28_clasificar_sector.py (que construye la tabla de
decision) y 26_consolidar_fuentes_carbono.py (que la aplica).

DOS EJES INDEPENDIENTES
-----------------------
Hasta ahora la tabla tenia un solo eje, AFOLU / NO_AFOLU, que responde
"opera este proyecto sobre cobertura o uso del suelo?". Eso basta para D3
pero no para D1, porque dentro de AFOLU conviven mecanismos distintos:

    REDD      deforestacion o degradacion EVITADA. Opera sobre el mismo
              margen que el outcome del panel (perdida de cobertura).
    ARR/IFM   restauracion y manejo forestal. Suman cobertura o cambian
              rotaciones; su efecto esperado sobre la perdida bruta no es
              el mismo.

Por eso se agrega un segundo eje, ortogonal al primero:

    clasificacion_final  ->  AFOLU | NO_AFOLU        (eje sectorial, D3)
    clase_redd_final     ->  REDD  | NO_REDD         (eje de mecanismo, D1)

Son independientes a proposito. Un proyecto ARR es AFOLU y NO_REDD; una
hidroelectrica es NO_AFOLU y NO_REDD. La combinacion AFOLU + REDD define D1.

CRITERIO DE INCLUSION SECTORIAL (sin cambios)
---------------------------------------------
Se conserva un proyecto si su mecanismo de mitigacion declarado opera sobre
cobertura boscosa o uso del suelo. Los demas se excluyen porque la
deforestacion municipal no es un resultado plausible de su mecanismo: una
hidroelectrica, una PTAR o un programa de estufas eficientes reducen
emisiones por vias que no pasan por la cobertura forestal del municipio.

Incluirlos no es solo ruido -- es sesgo. Sus municipios anfitriones
(Medellin, Barranquilla, Sogamoso) tienen dinamicas de perdida de cobertura
radicalmente distintas a las de un municipio de frontera agricola, y entran
al estimador fijando cohortes de tratamiento espurias.

FALLA EN RUIDOSO
----------------
Si un proyecto no esta en la tabla de decision, o esta marcado REVISAR, el
filtro lanza una excepcion en vez de asumir un valor. Un proyecto sin
clasificar nunca entra al tratamiento por omision. Esto vale para los dos
ejes.
"""

from __future__ import annotations

import re
import unicodedata
from pathlib import Path

import pandas as pd

# Ruta de la tabla de decision. Se construye con 28_clasificar_sector.py
# y se edita a mano.
CLASIFICACION_FILE = Path("data/interim/clasificacion_sectorial.csv")

ETIQUETAS_VALIDAS = {"AFOLU", "NO_AFOLU"}
ETIQUETAS_REDD_VALIDAS = {"REDD", "NO_REDD"}
ETIQUETA_PENDIENTE = "REVISAR"


# ---------------------------------------------------------------------------
# Clave de union entre la tabla de decision y los eventos
# ---------------------------------------------------------------------------
def normalizar(texto) -> str:
    """Minusculas, sin acentos, sin espacios repetidos. Estable frente a
    diferencias de tipeo entre archivos."""
    if pd.isna(texto):
        return ""
    t = unicodedata.normalize("NFKD", str(texto))
    t = "".join(c for c in t if not unicodedata.combining(c))
    return re.sub(r"\s+", " ", t).strip().lower()


def clave_proyecto(fuente, nombre) -> str:
    """Identificador estable de un proyecto. Se usa en los dos sentidos:
    al construir la tabla y al aplicarla."""
    return f"{normalizar(fuente)}|{normalizar(nombre)}"


# ---------------------------------------------------------------------------
# Pre-clasificacion automatica -- EJE SECTORIAL
#
# Solo es una SUGERENCIA para ahorrar trabajo manual. La columna que manda
# es clasificacion_final, que se edita a mano. Donde la fuente expone un
# campo estructurado se usa ese campo; las palabras clave son el ultimo
# recurso.
# ---------------------------------------------------------------------------

# Gold Standard: el campo 'methodology' es diagnostico.
#   AMS-I.*  -> metodologias CDM de generacion electrica renovable
#   TPDDTEC  -> Technologies and Practices to Displace Decentralized
#               Thermal Energy Consumption (estufas eficientes)
#   Fuel Switch -> sustitucion de combustible
GS_METODOLOGIA_NO_AFOLU = [r"^AMS-I", r"TPDDTEC", r"Fuel\s*Switch"]
GS_METODOLOGIA_AFOLU = [r"Afforestation", r"Reforestation", r"AR-AM", r"AR-ACM"]

# RENARE: el campo 'tipo' clasifica los 562 registros sin ambiguedad.
#   'Proyecto REDD+' y 'Programa REDD+' son forestales por definicion.
#   PDBC, MDL, NAMA agrupan todo lo demas -- pero PDBC puede contener
#   foresteria legitima, asi que va a REVISAR, no a NO_AFOLU.
RENARE_TIPO_AFOLU = [r"REDD\+"]

# Verra: el alcance sectorial 14 es AFOLU. Las metodologias VM00xx y
# AR-ACM/AR-AM cubren forestal; VMR es revision de metodologia.
VERRA_AFOLU = [
    r"Agriculture[,\s]*Forestry", r"Land\s*Use", r"\bAFOLU\b",
    r"^VM00", r"^AR-", r"scope\s*14",
]

# Respaldo por nombre, cuando no hay campo estructurado (caso Cercarbono).
# Se evalua NO_AFOLU primero: hay nombres que mezclan terminos.
NOMBRE_NO_AFOLU = [
    r"cook\s*stove", r"cookstove", r"estufa", r"cocina",
    r"hidroel", r"hydro", r"run[- ]of[- ]river", r"\bPCH\b", r"central hidro",
    r"e[oó]lic", r"wind\s*(power|farm|energy)", r"solar", r"fotovolt",
    r"biog[aá]s", r"landfill", r"relleno\s*sanitario", r"\bLFG\b",
    r"aguas\s*residuales", r"\bPTAR\b", r"water\s*treatment",
    r"termo", r"ciclo\s*combinado", r"combined\s*cycle",
    r"transport", r"movilidad", r"veh[ií]cul",
    r"cement", r"acero", r"industrial\s*gas", r"refriger", r"\bHFC\b",
    r"geoterm", r"geotherm", r"renewable\s*energ", r"energ[ií]a\s*renovable",
]

NOMBRE_AFOLU = [
    r"REDD", r"deforestaci", r"deforestat", r"forest", r"bosque", r"selva",
    r"reforest", r"afforest", r"restauraci", r"restorat",
    r"conservaci", r"conservat", r"manglar", r"mangrove",
    r"agroforest", r"silvopast", r"uso\s*del\s*suelo", r"land\s*use",
    r"soil\s*carbon", r"suelos\s*degradados", r"p[aá]ramo", r"humedal",
    r"plantaci[oó]n\s*forestal", r"\bAFOLU\b", r"\bIFM\b", r"\bARR\b",
]


def _match(patrones: list[str], texto: str) -> bool:
    return any(re.search(p, texto, flags=re.IGNORECASE) for p in patrones)


def clasificar_automatico(fuente: str, fila: pd.Series) -> tuple[str, str]:
    """Devuelve (etiqueta_sugerida, motivo). La etiqueta puede ser AFOLU,
    NO_AFOLU o REVISAR."""
    f = normalizar(fuente)
    nombre = str(fila.get("nombre_proyecto", "") or "")

    # -- campos estructurados, por fuente ---------------------------------
    if f == "gold_standard":
        met = str(fila.get("sector__methodology", "") or "")
        if met:
            if _match(GS_METODOLOGIA_AFOLU, met):
                return "AFOLU", f"methodology: {met[:60]}"
            if _match(GS_METODOLOGIA_NO_AFOLU, met):
                return "NO_AFOLU", f"methodology: {met[:60]}"

    if f == "renare":
        tipo = str(fila.get("sector__tipo", "") or "")
        if tipo:
            if _match(RENARE_TIPO_AFOLU, tipo):
                return "AFOLU", f"tipo: {tipo[:60]}"
            # PDBC / MDL / NAMA: puede haber foresteria adentro.
            # Se mira 'actividades' antes de rendirse.
            act = str(fila.get("sector__actividades", "") or "")
            if act and _match(NOMBRE_AFOLU, act):
                return ETIQUETA_PENDIENTE, f"tipo={tipo[:30]} pero actividades sugiere AFOLU"
            if act and _match(NOMBRE_NO_AFOLU, act):
                return "NO_AFOLU", f"tipo={tipo[:30]}; actividades: {act[:40]}"
            return ETIQUETA_PENDIENTE, f"tipo: {tipo[:60]} (no concluyente)"

    if f == "verra":
        # Nuevo: si 34_enriquecer_verra_metodologia.py ya corrio, la clase
        # por metodologia decide tambien el eje sectorial y es mejor
        # evidencia que sectoralScope (VM0042 es ALM y cae en AFOLU aunque
        # el scope sea generico).
        clase = str(fila.get("sector__clase_metodologia", "") or "").strip().upper()
        if clase in {"REDD_ESTRICTO", "AFOLU_NO_REDD"}:
            return "AFOLU", f"clase_metodologia: {clase}"
        if clase == "NO_AFOLU":
            return "NO_AFOLU", "clase_metodologia: NO_AFOLU"

        for col in ["sector__sector", "sector__scope", "sector__methodology",
                    "sector__project_type", "sector__categor"]:
            val = str(fila.get(col, "") or "")
            if val and _match(VERRA_AFOLU, val):
                return "AFOLU", f"{col}: {val[:60]}"

    # -- respaldo por nombre ----------------------------------------------
    if _match(NOMBRE_NO_AFOLU, nombre):
        return "NO_AFOLU", "nombre del proyecto"
    if _match(NOMBRE_AFOLU, nombre):
        return "AFOLU", "nombre del proyecto"

    return ETIQUETA_PENDIENTE, "sin campo sectorial ni coincidencia por nombre"


# ---------------------------------------------------------------------------
# Pre-clasificacion automatica -- EJE REDD
#
# Jerarquia de evidencia, de mas fuerte a mas debil:
#   1. Verra   clase_metodologia (codigo VM00xx del export publico)
#   2. RENARE  tipo (taxonomia cerrada del registro nacional: 'Proyecto
#              REDD+' es un valor propio, distinto de PDBC/MDL/NAMA)
#   3. Gold Standard  methodology (verificado: ninguno de sus 12 proyectos
#              colombianos es REDD; los 3 forestales son ARR)
#   4. Nombre  ultimo recurso, solo para Cercarbono mientras no se
#              recuperen sus campos estructurados del Excel
#
# Ojo con la asimetria: 'REDD' en el nombre es evidencia DEBIL de que si lo
# es (el falso positivo Yagual decia 'Conservation and Restoration' y era
# AR-ACM0003), mientras que un nombre claramente energetico es evidencia
# fuerte de que NO lo es. Por eso los dos casos se motivan distinto y el
# primero queda marcado para revision en la tabla.
# ---------------------------------------------------------------------------
RENARE_TIPO_REDD = [r"REDD\+"]
NOMBRE_REDD = [
    r"\bREDD\b", r"REDD\+", r"deforestaci[oó]n\s*evitada",
    r"avoided\s*deforestation", r"deforestaci[oó]n\s*no\s*planificada",
]


def clasificar_redd_automatico(fuente: str, fila: pd.Series) -> tuple[str, str]:
    """Devuelve (etiqueta_sugerida, motivo) sobre el eje REDD.
    Etiquetas: REDD, NO_REDD o REVISAR."""
    f = normalizar(fuente)
    nombre = str(fila.get("nombre_proyecto", "") or "")

    # 1. Verra: dato del registrador via 34_enriquecer_verra_metodologia.py
    if f == "verra":
        clase = str(fila.get("sector__clase_metodologia", "") or "").strip().upper()
        if clase == "REDD_ESTRICTO":
            met = str(fila.get("sector__methodology", "") or "")[:40]
            return "REDD", f"metodologia VCS: {met}" if met else "clase_metodologia: REDD_ESTRICTO"
        if clase in {"AFOLU_NO_REDD", "NO_AFOLU"}:
            return "NO_REDD", f"clase_metodologia: {clase}"
        # Sin la columna nueva: no se adivina. 34 todavia no corrio.
        return ETIQUETA_PENDIENTE, "falta clase_metodologia (corre 34_enriquecer_verra_metodologia.py)"

    # 2. RENARE: taxonomia cerrada
    if f == "renare":
        tipo = str(fila.get("sector__tipo", "") or "")
        if tipo:
            if _match(RENARE_TIPO_REDD, tipo):
                return "REDD", f"tipo: {tipo[:60]}"
            # PDBC, MDL, NAMA son categorias propias del registro: si el
            # proyecto fuera REDD+, estaria marcado como tal. Esto NO dice
            # que sean AFOLU o no -- solo que no son REDD.
            return "NO_REDD", f"tipo: {tipo[:40]} (categoria distinta de REDD+ en RENARE)"

    # 3. Gold Standard
    if f == "gold_standard":
        met = str(fila.get("sector__methodology", "") or "")
        if met:
            return "NO_REDD", f"methodology: {met[:60]} (GS no opera REDD en Colombia)"

    # 4. Nombre -- ultimo recurso
    if _match(NOMBRE_NO_AFOLU, nombre):
        return "NO_REDD", "nombre del proyecto (mecanismo no forestal)"
    if _match(NOMBRE_REDD, nombre):
        return ETIQUETA_PENDIENTE, "REDD en el nombre, sin campo estructurado que lo respalde"

    return ETIQUETA_PENDIENTE, "sin campo estructurado ni coincidencia por nombre"


# ---------------------------------------------------------------------------
# Aplicacion de los filtros
# ---------------------------------------------------------------------------
def cargar_tabla(ruta: Path = CLASIFICACION_FILE) -> pd.DataFrame:
    if not ruta.exists():
        raise FileNotFoundError(
            f"No encuentro la tabla de decision en {ruta}.\n"
            "Corre primero:  python 28_clasificar_sector.py"
        )
    tabla = pd.read_csv(ruta, dtype=str, encoding="utf-8-sig").fillna("")
    faltan = {"clave", "clasificacion_final"} - set(tabla.columns)
    if faltan:
        raise ValueError(f"La tabla de decision no tiene las columnas {faltan}.")
    return tabla


def _anotar(
    eventos: pd.DataFrame,
    ruta: Path,
    col_tabla: str,
    col_salida: str,
    etiquetas_validas: set[str],
    estricto: bool,
) -> pd.DataFrame:
    """Motor comun de los dos filtros. Mapea la decision de la tabla a los
    eventos por la clave fuente|nombre y verifica que no quede ninguno sin
    resolver."""
    tabla = cargar_tabla(ruta)
    if col_tabla not in tabla.columns:
        raise ValueError(
            f"La tabla de decision no tiene la columna '{col_tabla}'.\n"
            "Vuelve a correr 28_clasificar_sector.py con la version que\n"
            "incluye el eje REDD."
        )
    mapa = dict(zip(tabla["clave"], tabla[col_tabla].str.strip().str.upper()))

    ev = eventos.copy()
    ev["_clave"] = [
        clave_proyecto(f, n) for f, n in zip(ev["fuente"], ev["nombre_proyecto"])
    ]
    ev[col_salida] = ev["_clave"].map(mapa).fillna("SIN_CLASIFICAR")

    pendientes = ev[~ev[col_salida].isin(etiquetas_validas)]
    if len(pendientes) > 0:
        detalle = (
            pendientes[["fuente", "nombre_proyecto", col_salida]]
            .drop_duplicates()
            .to_string(index=False)
        )
        mensaje = (
            f"\n{len(pendientes)} evento(s) sin '{col_tabla}' valida:\n"
            f"{detalle}\n\n"
            f"Edita '{col_tabla}' en {ruta} (valores permitidos: "
            f"{sorted(etiquetas_validas)}) y vuelve a correr."
        )
        if estricto:
            raise RuntimeError(mensaje)
        print("AVISO (modo no estricto):" + mensaje)

    return ev.drop(columns=["_clave"])


def anotar_sector(
    eventos: pd.DataFrame,
    ruta: Path = CLASIFICACION_FILE,
    estricto: bool = True,
) -> pd.DataFrame:
    """Anade la columna 'sector' (AFOLU / NO_AFOLU) a los eventos.

    Con estricto=True (el default y lo que debe usar la tesis), cualquier
    evento sin clasificacion final valida interrumpe la ejecucion. Usa
    estricto=False solo para explorar.
    """
    ev = _anotar(eventos, ruta, "clasificacion_final", "sector",
                 ETIQUETAS_VALIDAS, estricto)

    n_fuera = (ev["sector"] == "NO_AFOLU").sum()
    print(f"\nFiltro sectorial: {len(ev) - n_fuera} eventos AFOLU conservados, "
          f"{n_fuera} excluidos por sector.")
    if n_fuera:
        excluidos = (
            ev[ev["sector"] == "NO_AFOLU"][["COD_DANE", "fuente", "nombre_proyecto"]]
            .drop_duplicates()
        )
        print("  Excluidos:")
        for _, r in excluidos.iterrows():
            print(f"    {r['COD_DANE']}  [{r['fuente']}]  {str(r['nombre_proyecto'])[:60]}")

    return ev


def anotar_redd(
    eventos: pd.DataFrame,
    ruta: Path = CLASIFICACION_FILE,
    estricto: bool = True,
) -> pd.DataFrame:
    """Anade la columna 'clase_redd' (REDD / NO_REDD) a los eventos.

    Ortogonal a anotar_sector: se pueden aplicar los dos, en cualquier
    orden. D1 es la interseccion (sector == AFOLU y clase_redd == REDD).

    Sobre estricto: mientras no se recuperen los campos estructurados de
    Cercarbono, una parte de sus proyectos queda en REVISAR. Correr con
    estricto=False los trata como NO_REDD e imprime la lista completa --
    aceptable para explorar, NO para la especificacion final. La version
    que sustenta la tesis debe correr en estricto.
    """
    ev = _anotar(eventos, ruta, "clase_redd_final", "clase_redd",
                 ETIQUETAS_REDD_VALIDAS, estricto)

    if not estricto:
        # En modo exploratorio lo no resuelto NO entra al tratamiento.
        ev.loc[~ev["clase_redd"].isin(ETIQUETAS_REDD_VALIDAS), "clase_redd"] = "NO_REDD"

    n_redd = (ev["clase_redd"] == "REDD").sum()
    n_mun = ev.loc[ev["clase_redd"] == "REDD", "COD_DANE"].nunique()
    print(f"\nFiltro REDD: {n_redd} eventos REDD en {n_mun} municipios "
          f"({len(ev) - n_redd} eventos excluidos por mecanismo).")
    return ev


def filtrar_d1(
    eventos: pd.DataFrame,
    ruta: Path = CLASIFICACION_FILE,
    estricto: bool = True,
) -> pd.DataFrame:
    """Atajo para D1: aplica los dos ejes y devuelve solo AFOLU + REDD.

    La interseccion deberia ser redundante (todo REDD es AFOLU), pero se
    aplica igual: si algun proyecto sale REDD y NO_AFOLU, hay una
    inconsistencia entre los dos ejes que conviene ver y no esconder.
    """
    ev = anotar_sector(eventos, ruta, estricto)
    ev = anotar_redd(ev, ruta, estricto)

    incoherentes = ev[(ev["clase_redd"] == "REDD") & (ev["sector"] == "NO_AFOLU")]
    if len(incoherentes):
        print(f"\n[!] {len(incoherentes)} evento(s) marcados REDD pero NO_AFOLU. "
              "Los dos ejes se contradicen; revisalos:")
        print(incoherentes[["fuente", "nombre_proyecto"]].drop_duplicates()
              .to_string(index=False))

    d1 = ev[(ev["sector"] == "AFOLU") & (ev["clase_redd"] == "REDD")].copy()
    print(f"\nD1 (AFOLU + REDD): {len(d1)} eventos en "
          f"{d1['COD_DANE'].nunique()} municipios.")
    return d1
