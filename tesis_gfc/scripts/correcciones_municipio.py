# -*- coding: utf-8 -*-
"""
correcciones_municipio.py
=========================

Aplica las verificaciones manuales de UBICACION a los eventos consolidados.
Igual que filtro_sectorial.py, este modulo no se ejecuta solo: lo importa
26_consolidar_fuentes_carbono.py.

POR QUE EXISTE
--------------
RENARE y Cercarbono asignan municipio por coincidencia de texto. Eso produce
errores que la lista negra no puede corregir sin romper otros casos:
"BAJO ATRATO" contiene el token "ATRATO" (27050), y un proyecto de una CAR
puede quedar en un municipio que solo se menciona de pasada en la
descripcion. La correccion se hace proyecto a proyecto, con evidencia citable.

ESQUEMA DE data/interim/correcciones_municipio.csv
--------------------------------------------------
clave              fuente|nombre normalizado (misma funcion que filtro_sectorial)
cod_dane_original  municipio que asigno el matching de texto
accion             CONFIRMAR | ASIGNAR | EXCLUIR
cod_dane_nuevo     solo con ASIGNAR. Proyecto multi-municipio = una fila por
                   municipio (mismo clave y cod_dane_original)
nombre_mpi_nuevo   informativo
evidencia          URL + campo consultado + valor (obligatorio)
fecha_consulta     AAAA-MM-DD

Semantica:
  (vacia)    todavia no verificada: la fila se omite y se cuenta como
             pendiente. Permite conservar el esqueleto completo e ir
             llenandolo por tandas.
  CONFIRMAR  el municipio original es correcto (queda trazado como verificado)
  ASIGNAR    el evento se re-ubica en cod_dane_nuevo. Para CONSERVAR el
             original y AGREGAR otro, usa una fila CONFIRMAR + una ASIGNAR.
  EXCLUIR    el evento sale de la tabla (ubicacion falsa o no es Colombia).
             No se combina con otras acciones para el mismo evento.

FALLA EN RUIDOSO
----------------
Una correccion que no encuentra su evento (porque cambio el nombre o el
codigo aguas arriba) detiene la ejecucion: una correccion huerfana es una
correccion que dejo de aplicarse sin que nadie se entere.
"""

from __future__ import annotations

from pathlib import Path
from typing import Iterable

import pandas as pd

from filtro_sectorial import clave_proyecto

CORRECCIONES_FILE = Path("data/interim/correcciones_municipio.csv")
ACCIONES = {"CONFIRMAR", "ASIGNAR", "EXCLUIR"}
COLUMNAS_REQUERIDAS = ["clave", "cod_dane_original", "accion", "cod_dane_nuevo", "evidencia"]


def _dane(serie: pd.Series) -> pd.Series:
    """Codigo DANE de 5 digitos como texto. Protege contra el '5001.0' que
    deja pandas cuando la columna se leyo como float."""
    return (
        serie.astype(str).str.strip()
        .str.replace(r"\.0$", "", regex=True)
        .str.zfill(5)
    )


def cargar_correcciones(ruta: Path = CORRECCIONES_FILE) -> pd.DataFrame:
    if not ruta.exists():
        raise FileNotFoundError(
            f"No encuentro {ruta}. Genera la plantilla con "
            "29_hoja_revision_manual.py, completala y renombrala."
        )
    c = pd.read_csv(ruta, dtype=str, encoding="utf-8-sig").fillna("")

    faltan = set(COLUMNAS_REQUERIDAS) - set(c.columns)
    if faltan:
        raise ValueError(f"{ruta} no tiene las columnas {sorted(faltan)}")

    c["accion"] = c["accion"].str.strip().str.upper()
    pendientes = c[c["accion"] == ""]
    malas = c[(c["accion"] != "") & ~c["accion"].isin(ACCIONES)]
    if len(malas):
        raise ValueError(
            f"Acciones invalidas en {ruta} (permitidas: {sorted(ACCIONES)}):\n"
            + malas[["clave", "cod_dane_original", "accion"]].to_string(index=False)
        )
    # Filas sin decidir: se omiten, no detienen la corrida.
    c = c[c["accion"] != ""].copy()
    if len(pendientes):
        print(f"  {len(pendientes)} evento(s) aun sin verificar en {ruta.name}: "
              "conservan el municipio del matching de texto.")
    if c.empty:
        print("  Ninguna correccion aplicada todavia.")
        c["_k"] = pd.Series(dtype=str)
        return c

    sin_evidencia = c[c["evidencia"].str.strip() == ""]
    if len(sin_evidencia):
        raise ValueError(
            "Toda correccion necesita evidencia citable:\n"
            + sin_evidencia[["clave", "accion"]].to_string(index=False)
        )

    c["cod_dane_original"] = _dane(c["cod_dane_original"])
    asignar = c["accion"] == "ASIGNAR"
    if (c.loc[asignar, "cod_dane_nuevo"].str.strip() == "").any():
        raise ValueError("Hay filas ASIGNAR sin cod_dane_nuevo.")
    c.loc[asignar, "cod_dane_nuevo"] = _dane(c.loc[asignar, "cod_dane_nuevo"])

    c["_k"] = c["clave"].str.strip() + "#" + c["cod_dane_original"]

    mezcla = c.groupby("_k")["accion"].agg(
        lambda s: "EXCLUIR" in set(s) and len(set(s)) > 1
    )
    if mezcla.any():
        raise ValueError(
            "EXCLUIR no puede combinarse con otras acciones para el mismo evento: "
            f"{list(mezcla[mezcla].index)}"
        )
    return c


def aplicar_correcciones(
    eventos: pd.DataFrame,
    ruta: Path = CORRECCIONES_FILE,
    codigos_panel: Iterable[str] | None = None,
) -> pd.DataFrame:
    """Devuelve los eventos con la ubicacion verificada.

    codigos_panel: si se pasa, cada cod_dane_nuevo debe existir en el panel
    (evita re-ubicar un proyecto en un municipio que el estimador no ve,
    p. ej. Belen de Bajira si el panel usa la DIVIPOLA previa a 2022).
    """
    corr = cargar_correcciones(ruta)

    ev = eventos.copy()
    ev["COD_DANE"] = _dane(ev["COD_DANE"])
    ev["_k"] = [
        f"{clave_proyecto(f, n)}#{d}"
        for f, n, d in zip(ev["fuente"], ev["nombre_proyecto"], ev["COD_DANE"])
    ]

    huerfanas = sorted(set(corr["_k"]) - set(ev["_k"]))
    if huerfanas:
        raise RuntimeError(
            "Correcciones que no corresponden a ningun evento (cambio el nombre "
            "o el codigo aguas arriba, o el proyecto fue filtrado antes):\n  "
            + "\n  ".join(huerfanas)
        )

    if codigos_panel is not None:
        validos = set(_dane(pd.Series(list(codigos_panel))))
        nuevos = set(corr.loc[corr["accion"] == "ASIGNAR", "cod_dane_nuevo"])
        fuera = sorted(nuevos - validos)
        if fuera:
            raise ValueError(f"cod_dane_nuevo que no existen en el panel: {fuera}")

    tocados = ev[ev["_k"].isin(corr["_k"])]
    intactos = ev[~ev["_k"].isin(corr["_k"])]

    # Union uno-a-muchos: un proyecto multi-municipio genera varias filas.
    m = tocados.merge(
        corr[["_k", "accion", "cod_dane_nuevo"]], on="_k", how="left"
    )
    n_excluidos = m.loc[m["accion"] == "EXCLUIR", "_k"].nunique()
    m = m[m["accion"] != "EXCLUIR"].copy()
    es_asignar = m["accion"] == "ASIGNAR"
    m.loc[es_asignar, "COD_DANE"] = m.loc[es_asignar, "cod_dane_nuevo"]
    m["metodo"] = "verificacion_manual"
    m = m.drop(columns=["accion", "cod_dane_nuevo"])

    out = pd.concat([intactos, m], ignore_index=True)
    out = out.drop_duplicates(
        subset=["COD_DANE", "fuente", "nombre_proyecto", "anio_inicio"]
    ).drop(columns="_k")

    print(
        f"\nCorrecciones de municipio: {tocados['_k'].nunique()} eventos revisados | "
        f"{n_excluidos} excluidos | {len(out)} eventos resultantes "
        f"(antes {len(eventos)})."
    )
    return out
