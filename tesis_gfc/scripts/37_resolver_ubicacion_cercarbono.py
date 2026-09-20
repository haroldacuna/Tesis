#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
37_resolver_ubicacion_cercarbono.py
===================================

Convierte la plantilla de recuperacion manual en una tabla de eventos lista
para 26_consolidar_fuentes_carbono.py.

QUE RESUELVE
------------
Los 22 proyectos REDD+ de Cercarbono con creditos emitidos (47.407.872 tCO2e)
no tienen municipio en ninguna fuente automatica: OffsetsDB trae cod_dane
vacio, el matching por texto contra nombres de municipio falla porque los
nombres son toponimos en lenguas indigenas, y el empate contra la capa de
resguardos de la ANT por similitud produce falsos positivos (se probo: 8 de
21 "empates" eran todos falsos).

La ubicacion se recupero a mano de las fichas publicas del registro de
Cercarbono. Este script solo hace la ultima milla: pasar de nombre de
municipio a codigo DANE, contra la MISMA capa que usa el panel.

POR QUE CONTRA municipios_clean.gpkg Y NO CONTRA UNA LISTA
-----------------------------------------------------------
Si el codigo se escribe a mano y no coincide con el que trae la capa del
panel, el merge de 26 no falla: simplemente no encuentra el municipio y el
proyecto desaparece en silencio. Resolver contra la capa garantiza que todo
codigo emitido existe en el panel.

DESAMBIGUACION POR DEPARTAMENTO
-------------------------------
Hay municipios homonimos en departamentos distintos. El caso vivo aqui es
Riosucio, que existe en Choco (Pedeguita y Mancilla) y en Caldas. Por eso la
plantilla escribe 'Riosucio (CHOCO)' y este script exige el departamento
cuando el nombre no es unico. Sin esa pista, aborta en vez de elegir.

FUENTES DE CODIGO, EN ORDEN
---------------------------
1. cod_dane que la plantilla calcula por VLOOKUP contra la capa de
   resguardos de la ANT (cuando se identifico el resguardo)
2. nombres de municipio escritos en municipios_nombre, resueltos aqui

Las dos pueden coexistir: un proyecto puede tener el municipio del resguardo
segun la ANT mas otros municipios que la ficha nombra.

CONFIANZA
---------
Solo se emiten filas con confianza alta o media. Las de confianza baja se
listan y se excluyen: son proyectos cuya ficha no permite determinar la
ubicacion, y meterlos al tratamiento fabricaria cohortes espurias.

USO
---
    python 37_resolver_ubicacion_cercarbono.py
    python 37_resolver_ubicacion_cercarbono.py --plantilla ruta\\al\\archivo.xlsx

SALIDAS
-------
    data/interim/cercarbono_ubicacion_manual.csv
        Una fila por (proyecto, municipio), con el esquema de eventos que
        consume 26_consolidar_fuentes_carbono.py.
    data/interim/diagnostics/cercarbono_ubicacion_sin_resolver.csv
        Lo que no resolvio y por que. Se revisa antes de correr 26.
"""

from __future__ import annotations

import argparse
import re
import sys
import unicodedata
from pathlib import Path

import pandas as pd

try:
    import geopandas as gpd
except ImportError:
    gpd = None

SCRIPT_DIR = Path(__file__).resolve().parent
CENTINELA = "data/.raiz_canonica"

# Solo estas entran al panel. 26 filtra ademas por confianza, asi que 'baja'
# no llegaria al tratamiento igual; se excluye aqui para que ni siquiera
# aparezca en la tabla de eventos.
CONFIANZAS_VALIDAS = {"alta", "media"}


def resolver_raiz(forzada: str | None = None) -> Path:
    if forzada:
        r = Path(forzada).resolve()
        if not (r / CENTINELA).exists():
            raise SystemExit(f"[X] {r} no tiene {CENTINELA}.")
        return r
    cands = []
    for base in (SCRIPT_DIR, Path.cwd().resolve()):
        d = base
        while True:
            if (d / CENTINELA).exists():
                cands.append(str(d)); break
            if d.parent == d:
                break
            d = d.parent
    u = sorted(set(cands))
    if len(u) == 1:
        return Path(u[0])
    if not u:
        raise SystemExit(f"[X] No encuentro {CENTINELA} desde {SCRIPT_DIR} ni {Path.cwd()}.")
    raise SystemExit("[X] Dos raices canonicas:\n    " + "\n    ".join(u))


def norm(t) -> str:
    """Sin acentos, sin puntuacion, minusculas. Quita ademas los articulos
    que el DANE incluye y las fichas omiten ('El Canton del San Pablo' en la
    capa contra 'Canton de San Pablo' en la ficha)."""
    if pd.isna(t):
        return ""
    t = unicodedata.normalize("NFKD", str(t))
    t = "".join(c for c in t if not unicodedata.combining(c)).lower()
    t = re.sub(r"[^a-z0-9\s]", " ", t)
    t = re.sub(r"\b(el|la|los|las|de|del|san|santa)\b", " ", t)
    return re.sub(r"\s+", " ", t).strip()


def cargar_municipios(raiz: Path) -> pd.DataFrame:
    if gpd is None:
        raise SystemExit("[X] Falta geopandas.")
    p = raiz / "data/interim/municipios_clean.gpkg"
    if not p.exists():
        raise SystemExit(f"[X] Falta {p}. Corre 01_prepare_boundaries.py")
    m = gpd.read_file(p, layer="municipios_clean")

    col_d = next((c for c in m.columns if re.search(r"(?i)cod.*dane|mpio.*cdpmp|^cod", str(c))), None)
    col_n = next((c for c in m.columns if re.search(r"(?i)nombre.*mpi|mpio_cnmbr|^nombre", str(c))), None)
    col_dep = next((c for c in m.columns if re.search(r"(?i)dpto.*cnmbr|nombre.*dpto|departamento", str(c))), None)
    if col_d is None or col_n is None:
        raise SystemExit(f"[X] No identifico columnas DANE/nombre en {list(m.columns)}")
    print(f"  capa: codigo='{col_d}' nombre='{col_n}' departamento='{col_dep}'")

    out = pd.DataFrame({
        "COD_DANE": m[col_d].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5),
        "nombre": m[col_n].astype(str),
        "depto": m[col_dep].astype(str) if col_dep else "",
    })
    out["_n"] = out["nombre"].map(norm)
    out["_d"] = out["depto"].map(norm)
    return out


def parse_municipios(celda) -> list[tuple[str, str]]:
    """'Riosucio (CHOCO), Novita (CHOCO)' -> [('Riosucio','CHOCO'), ...]"""
    if pd.isna(celda) or not str(celda).strip():
        return []
    salida = []
    for parte in str(celda).split(","):
        parte = parte.strip()
        if not parte:
            continue
        m = re.match(r"^(.*?)\s*\(([^)]+)\)\s*$", parte)
        if m:
            salida.append((m.group(1).strip(), m.group(2).strip()))
        else:
            salida.append((parte, ""))
    return salida


def resolver(nombre: str, depto: str, mun: pd.DataFrame) -> tuple[str | None, str]:
    """Devuelve (COD_DANE, motivo). None si no resuelve: el motivo dice por que."""
    n = norm(nombre)
    if not n:
        return None, "nombre vacio"
    cand = mun[mun["_n"] == n]
    if len(cand) == 0:
        # segunda pasada: contencion, para 'Belen de Bajira' y similares
        cand = mun[mun["_n"].str.contains(re.escape(n), na=False)] if n else mun.iloc[0:0]
        if len(cand) == 0:
            return None, "no existe en municipios_clean.gpkg"
    if len(cand) > 1:
        if depto:
            d = norm(depto)
            filtrado = cand[cand["_d"].str.contains(d, na=False) | cand["_d"].eq(d)]
            if len(filtrado) == 1:
                return filtrado.iloc[0]["COD_DANE"], f"nombre + departamento {depto}"
            if len(filtrado) > 1:
                cand = filtrado
        opciones = ", ".join(f"{r.COD_DANE} {r.nombre} ({r.depto})" for r in cand.itertuples())
        return None, f"AMBIGUO ({len(cand)} coincidencias): {opciones}"
    return cand.iloc[0]["COD_DANE"], "nombre unico"


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--raiz", default=None)
    ap.add_argument("--plantilla", default=None,
                    help="ruta del xlsx; por defecto data/raw/plantilla_ubicacion_cercarbono_LLENA.xlsx")
    args = ap.parse_args()

    raiz = resolver_raiz(args.raiz)
    print(f"Raiz canonica: {raiz}")

    plantilla = Path(args.plantilla) if args.plantilla else raiz / "data/raw/plantilla_ubicacion_cercarbono_LLENA.xlsx"
    if not plantilla.exists():
        raise SystemExit(f"[X] Falta {plantilla}\n    Pasa la ruta con --plantilla o copiala a data/raw/")

    # data_only=True: se leen los valores calculados del VLOOKUP contra la ANT,
    # no las formulas. Si el archivo no se abrio nunca en Excel, esos valores
    # pueden venir vacios; por eso tambien se usa municipios_nombre.
    df = pd.read_excel(plantilla, sheet_name="Plantilla", engine="openpyxl")
    df = df[df["nombre_proyecto"].notna()].copy()
    df = df[~df["nombre_proyecto"].astype(str).str.startswith("EJEMPLO")]
    print(f"Plantilla: {len(df)} proyectos")

    print("\nCargando municipios del panel...")
    mun = cargar_municipios(raiz)
    print(f"  {len(mun)} municipios")

    filas, problemas = [], []
    for _, r in df.iterrows():
        nom = str(r["nombre_proyecto"]).strip()
        conf = str(r.get("confianza", "")).strip().lower()
        anio = r.get("anio_inicio")

        if conf not in CONFIANZAS_VALIDAS:
            problemas.append({"nombre_proyecto": nom, "motivo": f"confianza '{conf}' -> excluido",
                              "detalle": str(r.get("notas", ""))[:200]})
            continue
        if pd.isna(anio):
            problemas.append({"nombre_proyecto": nom, "motivo": "sin anio_inicio", "detalle": ""})
            continue

        codigos = {}

        # 1. el que calculo el VLOOKUP contra la capa de la ANT
        cd = r.get("cod_dane")
        if pd.notna(cd) and str(cd).strip():
            c = str(cd).strip().replace(".0", "").zfill(5)
            if c in set(mun["COD_DANE"]):
                codigos[c] = "ANT (resguardo)"
            else:
                problemas.append({"nombre_proyecto": nom,
                                  "motivo": f"cod_dane {c} de la ANT no existe en el panel",
                                  "detalle": "revisar id_ant"})

        # 2. los nombres escritos a mano
        for nm, dep in parse_municipios(r.get("municipios_nombre")):
            c, motivo = resolver(nm, dep, mun)
            if c:
                codigos.setdefault(c, f"ficha: {nm}")
            else:
                problemas.append({"nombre_proyecto": nom, "motivo": f"'{nm}' no resuelve",
                                  "detalle": motivo})

        if not codigos:
            problemas.append({"nombre_proyecto": nom, "motivo": "sin ningun municipio resuelto", "detalle": ""})
            continue

        for c, origen in codigos.items():
            filas.append({
                "COD_DANE": c,
                "fuente": "cercarbono",
                "nombre_proyecto": nom,
                "anio_inicio": int(anio),
                "anio_fin": None,
                "confianza": conf,
                "metodo": "ficha_registro_manual",
                "origen_codigo": origen,
                "url_ficha": r.get("url_ficha"),
                "territorio_declarado": r.get("territorio_declarado"),
                "evidencia": r.get("evidencia"),
            })

    ev = pd.DataFrame(filas)
    prob = pd.DataFrame(problemas)

    print("\n" + "=" * 70)
    print(f"Eventos resueltos: {len(ev)}  |  proyectos: {ev['nombre_proyecto'].nunique() if len(ev) else 0}"
          f"  |  municipios: {ev['COD_DANE'].nunique() if len(ev) else 0}")
    print("=" * 70)
    if len(ev):
        print(ev.groupby("confianza")["COD_DANE"].nunique().to_string())
        print("\nCohortes que aportaria esta fuente:")
        print(ev.groupby("COD_DANE")["anio_inicio"].min().value_counts().sort_index().to_string())
        print("\nMunicipios nuevos (no estaban en los eventos consolidados):")
        cons = raiz / "data/interim/eventos_carbono_consolidado.csv"
        if cons.exists():
            c = pd.read_csv(cons, low_memory=False)
            ya = set(c["COD_DANE"].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5))
            nuevos = sorted(set(ev["COD_DANE"]) - ya)
            print(f"  {len(nuevos)} de {ev['COD_DANE'].nunique()}: {nuevos}")

    if len(prob):
        print(f"\n[!] {len(prob)} problema(s) sin resolver:")
        print(prob.to_string(index=False, max_colwidth=70))

    out = raiz / "data/interim/cercarbono_ubicacion_manual.csv"
    diag = raiz / "data/interim/diagnostics"
    diag.mkdir(parents=True, exist_ok=True)
    out.parent.mkdir(parents=True, exist_ok=True)
    ev.to_csv(out, index=False, encoding="utf-8-sig")
    prob.to_csv(diag / "cercarbono_ubicacion_sin_resolver.csv", index=False, encoding="utf-8-sig")
    print(f"\nEscrito: {out}")
    print(f"Escrito: {diag / 'cercarbono_ubicacion_sin_resolver.csv'}")

    print("""
PARA INTEGRARLO A 26
  Estos proyectos NO estan en clasificacion_sectorial.csv con su eje REDD,
  asi que el preflight de 26 los va a listar como bloqueantes. Antes de
  correr 26 hay que agregarlos con clasificacion_final=AFOLU y
  clase_redd_final=REDD, justificados por el protocolo ccb-redd de OffsetsDB.

  Luego, en 26, agregar una fuente que lea este CSV. Conviene mantenerla
  separada de la de matching por texto para poder correr la especificacion
  CON y SIN las recuperadas a mano: esa comparacion es la prueba de
  robustez frente al error de cobertura documentado.
""")
    return 0


if __name__ == "__main__":
    sys.exit(main())
