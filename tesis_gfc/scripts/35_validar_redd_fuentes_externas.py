#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
35_validar_redd_fuentes_externas.py
===================================

Triangula la clasificacion REDD+ de Verra contra DOS fuentes independientes
y mide cuantos proyectos activaron efectivamente el mecanismo de credito.

POR QUE
-------
La clasificacion de 34_enriquecer_verra_metodologia.py sale del campo
'Methodology' del export publico de Verra: es dato del registrador, pero de
UN solo registrador, procesado por una sola cadena de custodia (la nuestra).

Berkeley VROD y OffsetsDB clasifican los MISMOS proyectos por caminos
distintos:

    Berkeley (Voluntary Registry Offsets Database, UC Berkeley)
        columna 'Type', normalizada a mano por su equipo a partir de los
        registros. Trae ademas 'Total Credits Issued' y una columna por
        anio con las emisiones anuales.

    OffsetsDB (CarbonPlan)
        columna 'protocol', normalizada programaticamente; 'issued' y
        'status'.

Coincidencia entre tres cadenas independientes = validez externa. Los casos
en desacuerdo son mas informativos que los de acuerdo y se listan uno por
uno, nunca agregados.

QUE NO HACE
-----------
No corrige nada ni reescribe la clasificacion. Es un instrumento de medicion:
produce evidencia para el Capitulo 5 y una lista de casos a resolver a mano.

USO
---
    python 35_validar_redd_fuentes_externas.py
    python 35_validar_redd_fuentes_externas.py --raiz C:\\...\\tesis_gfc

SALIDAS
-------
    data/interim/diagnostics/redd_triangulacion_fuentes.csv
        Una fila por proyecto de Verra con la etiqueta de las tres fuentes.
    data/interim/diagnostics/redd_desacuerdos.csv
        Solo los casos donde al menos una fuente disiente.
    data/interim/diagnostics/redd_emisiones_por_proyecto.csv
        Creditos emitidos por proyecto REDD+, con la serie anual de Berkeley
        cuando esta disponible -- insumo de la especificacion de dosis.
"""

from __future__ import annotations

import argparse
import re
import sys
import unicodedata
from pathlib import Path

import pandas as pd

SCRIPT_DIR = Path(__file__).resolve().parent
CENTINELA = "data/.raiz_canonica"


def resolver_raiz(raiz_forzada: str | None = None) -> Path:
    """Igual que en el script 34: el centinela manda. Ver la nota alli sobre
    por que buscar 'data/interim' no basta en este repo."""
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
                candidatas.append(str(d))
                break
            if d.parent == d:
                break
            d = d.parent
    unicas = sorted(set(candidatas))
    if len(unicas) == 1:
        return Path(unicas[0])
    if not unicas:
        raise SystemExit(f"[X] No encuentro {CENTINELA} subiendo desde {SCRIPT_DIR} ni {Path.cwd()}.")
    raise SystemExit("[X] Dos raices canonicas distintas:\n    " + "\n    ".join(unicas))


# ---------------------------------------------------------------------------
# Normalizacion de IDs y texto
# ---------------------------------------------------------------------------
def norm_id(v) -> str:
    """Los IDs de Verra viajan como 1234, 1234.0, VCS1234 o VCS-1234 segun la
    fuente. Se reducen a digitos sin ceros a la izquierda."""
    if pd.isna(v):
        return ""
    s = re.sub(r"\.0+$", "", str(v).strip())
    s = re.sub(r"(?i)^vcs[\s_:-]*", "", s)
    return re.sub(r"\D", "", s).lstrip("0")


def norm(t) -> str:
    if pd.isna(t):
        return ""
    t = unicodedata.normalize("NFKD", str(t))
    t = "".join(c for c in t if not unicodedata.combining(c))
    t = t.lower().replace("+", " ")
    t = re.sub(r"[^a-z0-9\s]", " ", t)
    return re.sub(r"\s+", " ", t).strip()


def etiqueta_berkeley(tipo) -> str:
    """Berkeley usa 'REDD+' y 'Jurisdictional REDD+' como valores tipados."""
    t = norm(tipo)
    if not t:
        return "sin_dato"
    if "redd" in t:
        return "REDD"
    if any(k in t for k in ["afforestation", "reforestation", "grassland",
                            "improved forest", "biochar", "agricultur"]):
        return "AFOLU_no_REDD"
    return "no_AFOLU"


PROTOCOLOS_REDD = {"vm0004", "vm0006", "vm0007", "vm0009", "vm0011",
                   "vm0015", "vm0029", "vm0037", "vm0048", "ccb-redd"}
PROTOCOLOS_AFOLU_NO_REDD = {"ar-acm0003", "ar-acm0001", "vm0047", "vm0042",
                            "vm0032", "vm0033", "vm0035", "vm0045", "ccb-refor",
                            "vm0003", "vm0010", "vm0012", "vm0017", "vm0021",
                            "vm0026"}


def etiqueta_offsetsdb(protocol, project_type) -> str:
    """OffsetsDB guarda protocol como el texto de una lista de Python:
    "['vm0015']". Se parsea con regex en vez de eval, que es inseguro sobre
    datos de terceros."""
    codigos = set()
    if pd.notna(protocol):
        codigos = {c.strip().lower() for c in re.findall(r"'([^']+)'", str(protocol))}
    if codigos & PROTOCOLOS_REDD:
        return "REDD"
    if codigos & PROTOCOLOS_AFOLU_NO_REDD:
        return "AFOLU_no_REDD"
    t = norm(project_type)
    if "redd" in t:
        return "REDD"
    if t and t != "unknown":
        return "AFOLU_no_REDD" if any(
            k in t for k in ["afforestation", "reforestation", "grassland", "agricultur"]
        ) else "no_AFOLU"
    return "sin_dato"


def col(df: pd.DataFrame, *patrones: str) -> str | None:
    """Primera columna que casa con alguno de los patrones. Berkeley trae
    saltos de linea DENTRO de los nombres de columna ('Total Credits \\nIssued'),
    asi que se normaliza el nombre antes de comparar."""
    limpio = {c: re.sub(r"\s+", " ", str(c)).strip().lower() for c in df.columns}
    for p in patrones:
        for original, l in limpio.items():
            if re.search(p, l):
                return original
    return None


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--raiz", default=None)
    args = ap.parse_args()

    raiz = resolver_raiz(args.raiz)
    print(f"Raiz canonica: {raiz}")
    interim = raiz / "data/interim"
    diag = interim / "diagnostics"
    diag.mkdir(parents=True, exist_ok=True)

    p_verra = interim / "verra_con_metodologia.csv"
    p_berk = interim / "berkeley_vrod_projects_colombia.csv"
    p_odb = interim / "offsetsdb_projects_colombia.csv"

    if not p_verra.exists():
        print(f"[X] Falta {p_verra}. Corre primero 34_enriquecer_verra_metodologia.py")
        return 1

    v = pd.read_csv(p_verra, low_memory=False)
    c_id_v = col(v, r"^projectid$", r"^id$")
    c_nom_v = col(v, r"^projectname$", r"^name$")
    v["_id"] = v[c_id_v].map(norm_id)
    v["_nom"] = v[c_nom_v].map(norm)
    print(f"Verra clasificado: {len(v)} proyectos  ({int((v['clase_metodologia']=='REDD_ESTRICTO').sum())} REDD+)")

    base = v[["_id", "_nom", c_nom_v, "clase_metodologia", "status_registro",
              "estado_registro", "es_wrc", "COD_DANE", "NOMBRE_MPI"]].copy()
    base = base.rename(columns={c_nom_v: "nombre_verra", "clase_metodologia": "verra"})
    base["verra"] = base["verra"].map(
        {"REDD_ESTRICTO": "REDD", "AFOLU_NO_REDD": "AFOLU_no_REDD", "NO_AFOLU": "no_AFOLU"}
    ).fillna("sin_dato")

    # --- Berkeley ---------------------------------------------------------
    if p_berk.exists():
        b = pd.read_csv(p_berk, low_memory=False)
        c_id_b = col(b, r"^project id$")
        c_tipo_b = col(b, r"^type$")
        c_reg_b = col(b, r"voluntary registry")
        c_emit_b = col(b, r"total credits issued")
        # Solo la porcion Verra: Berkeley cubre tambien Gold Standard, ACR, CAR
        if c_reg_b:
            antes = len(b)
            b = b[b[c_reg_b].map(norm).str.contains("verra|vcs", na=False)]
            print(f"Berkeley: {antes} proyectos colombianos, {len(b)} de Verra")
        b["_id"] = b[c_id_b].map(norm_id)
        b["berkeley"] = b[c_tipo_b].map(etiqueta_berkeley)
        b["creditos_berkeley"] = pd.to_numeric(
            b[c_emit_b].astype(str).str.replace(r"[^\d.-]", "", regex=True), errors="coerce"
        ) if c_emit_b else pd.NA
        base = base.merge(
            b[["_id", "berkeley", "creditos_berkeley", c_tipo_b]].rename(columns={c_tipo_b: "tipo_berkeley"}),
            on="_id", how="left",
        )
        # Serie anual: las columnas se llaman como los anios
        anios = [c for c in b.columns if re.fullmatch(r"(19|20)\d{2}", str(c).strip())]
        if anios:
            serie = b[["_id"] + anios].copy()
            serie.to_csv(diag / "redd_emisiones_por_proyecto.csv", index=False, encoding="utf-8-sig")
            print(f"Serie anual de emisiones de Berkeley: {len(anios)} anios -> redd_emisiones_por_proyecto.csv")
    else:
        base["berkeley"] = "sin_dato"
        base["creditos_berkeley"] = pd.NA
        print(f"[ ] Falta {p_berk}")

    # --- OffsetsDB --------------------------------------------------------
    if p_odb.exists():
        o = pd.read_csv(p_odb, low_memory=False)
        c_id_o = col(o, r"^project_id_x$", r"^project_id")
        o = o[o["registry"].map(norm).str.contains("verra|vcs", na=False)] if "registry" in o.columns else o
        o["_id"] = o[c_id_o].map(norm_id)
        o["offsetsdb"] = o.apply(lambda r: etiqueta_offsetsdb(r.get("protocol"), r.get("project_type")), axis=1)
        o["creditos_odb"] = pd.to_numeric(o.get("issued"), errors="coerce")
        base = base.merge(
            o[["_id", "offsetsdb", "creditos_odb", "protocol", "status"]]
            .rename(columns={"status": "status_odb"})
            .drop_duplicates("_id"),
            on="_id", how="left",
        )
    else:
        base["offsetsdb"] = "sin_dato"
        base["creditos_odb"] = pd.NA
        print(f"[ ] Falta {p_odb}")

    base["berkeley"] = base["berkeley"].fillna("sin_dato")
    base["offsetsdb"] = base["offsetsdb"].fillna("sin_dato")

    # --- Acuerdo ----------------------------------------------------------
    def veredicto(r) -> str:
        votos = [r["verra"], r["berkeley"], r["offsetsdb"]]
        con_dato = [x for x in votos if x != "sin_dato"]
        if len(con_dato) <= 1:
            return "sin_contraste"
        return "acuerdo" if len(set(con_dato)) == 1 else "DESACUERDO"

    base["veredicto"] = base.apply(veredicto, axis=1)

    print("\n" + "=" * 72)
    print("TRIANGULACION  Verra x Berkeley x OffsetsDB")
    print("=" * 72)
    print(base["veredicto"].value_counts().to_string())
    print("\nEtiqueta por fuente (solo proyectos con dato en las tres):")
    tres = base[(base[["verra", "berkeley", "offsetsdb"]] != "sin_dato").all(axis=1)]
    print(f"  proyectos con las tres fuentes: {len(tres)}")
    for f in ["verra", "berkeley", "offsetsdb"]:
        print(f"  {f:10s} REDD = {int((base[f] == 'REDD').sum()):3d}")

    print("\n--- Casos en desacuerdo (se resuelven a mano, uno por uno) ---")
    des = base[base["veredicto"] == "DESACUERDO"]
    if len(des):
        print(des[["nombre_verra", "verra", "berkeley", "offsetsdb", "tipo_berkeley", "protocol"]]
              .to_string(index=False, max_colwidth=45))
    else:
        print("  Ninguno. Las tres cadenas de custodia coinciden proyecto por proyecto.")

    # --- Activacion del mecanismo ----------------------------------------
    redd = base[base["verra"] == "REDD"].copy()
    redd["creditos"] = redd["creditos_berkeley"].fillna(redd["creditos_odb"])
    con_emision = redd[redd["creditos"].fillna(0) > 0]
    print("\n" + "=" * 72)
    print("ACTIVACION DEL MECANISMO sobre los REDD+ de Verra")
    print("=" * 72)
    print(f"  REDD+ totales (S1):                 {len(redd)}")
    print(f"  Con creditos emitidos > 0:          {len(con_emision)}")
    print(f"  Con 0 creditos o sin dato:          {len(redd) - len(con_emision)}")
    if len(con_emision):
        print(f"  Creditos emitidos, mediana:         {con_emision['creditos'].median():,.0f}")
        print(f"  Creditos emitidos, total:           {con_emision['creditos'].sum():,.0f}")
    print(
        "\n  Un proyecto sin creditos emitidos no recibio pago por resultados:\n"
        "  el mecanismo de incentivo no se activo. Tratarlo como tratado es\n"
        "  error de medicion con atenuacion hacia cero."
    )

    base.drop(columns=["_nom"]).to_csv(diag / "redd_triangulacion_fuentes.csv", index=False, encoding="utf-8-sig")
    des.drop(columns=["_nom"]).to_csv(diag / "redd_desacuerdos.csv", index=False, encoding="utf-8-sig")
    print(f"\nEscrito: {diag / 'redd_triangulacion_fuentes.csv'}")
    print(f"Escrito: {diag / 'redd_desacuerdos.csv'}   ({len(des)} casos)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
