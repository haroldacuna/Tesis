"""
26_consolidar_fuentes_carbono.py

Une las 4 fuentes de proyectos de carbono trabajadas en esta sesión en una
sola tabla de eventos (un renglón por proyecto con su municipio y año de
inicio), y de ahí arma las columnas de tratamiento a nivel municipio-año
para el panel final.

FUENTES Y NIVEL DE CONFIANZA
----------------------------
- Verra (Platts API): municipio por PUNTO-EN-POLÍGONO (geometría exacta).
  Confianza ALTA. Excluye por defecto 2 proyectos marcados como
  sospechosos en la revisión manual (ver SOSPECHOSOS_VERRA abajo) —
  ajusta esa lista si decides incluirlos o excluir más.
- Gold Standard: municipio por PUNTO-EN-POLÍGONO (geometría exacta,
  incluyendo los 2 corregidos por signo/orden de coordenadas).
  Confianza ALTA.
- RENARE: municipio por *matching* de texto libre (nombre_iniciativa +
  actividades) contra nombres de municipio, con lista negra de palabras
  problemáticas. Solo se usan los matches ÚNICOS (n_matches == 1).
  Confianza MEDIA — recomendado revisar a mano antes de publicar
  resultados basados en esta fuente.
- Cercarbono: mismo método de matching por texto que RENARE. Solo se usan
  los matches ÚNICOS. Confianza MEDIA, mismo caveat.

Los proyectos AMBIGUOS (más de un municipio mencionado) de RENARE y
Cercarbono NO se incluyen automáticamente — quedan en un archivo aparte
para que decidas caso por caso.

SALIDAS
-------
data/interim/eventos_carbono_consolidado.csv
    Un renglón por proyecto: COD_DANE, fuente, nombre, año_inicio,
    año_fin, confianza. Esta es la tabla para AUDITAR antes de confiar en
    el panel final — revísala.

data/interim/eventos_carbono_para_revisar_manualmente.csv
    Los ambiguos de RENARE/Cercarbono, para decidir caso por caso si
    entran o no.

data/final/panel_con_tratamiento_actualizado.csv
    El panel original + columnas nuevas de tratamiento. NO sobreescribe
    el panel original — es un archivo nuevo, para que compares antes de
    reemplazar.

USO
---
    python 26_consolidar_fuentes_carbono.py
"""

from __future__ import annotations

import ast
import re
from pathlib import Path
from filtro_sectorial import anotar_sector
import pandas as pd

# ---------------------------------------------------------------------------
# Rutas de entrada (ajusta si moviste algo)
# ---------------------------------------------------------------------------



VERRA_FILE = Path("data/interim/verra_platts_colombia_con_municipio.csv")
GOLDSTANDARD_FILE_1 = Path("data/interim/goldstandard_projects_colombia_con_municipio.csv")
GOLDSTANDARD_FILE_2 = Path("data/interim/goldstandard_coords_corregidas.csv")
RENARE_MATCHING_FILE = Path("data/interim/diagnostics/renare_municipio_matching.csv")
RENARE_RAW_FILE = Path("data/interim/renare_solicitudes_colombia.csv")
CERCARBONO_FILE = Path("data/interim/diagnostics/cercarbono_municipio_matching.csv")

PANEL_FILE = Path("data/final/panel_municipio_year.csv")
# Si tu panel final se llama distinto, ajusta esta ruta. También se intenta
# con dataset_consolidado_completo.csv como respaldo.
PANEL_FILE_ALT = Path("data/final/dataset_consolidado_completo.csv")

OUT_EVENTOS = Path("data/interim/eventos_carbono_consolidado.csv")
OUT_REVISAR = Path("data/interim/eventos_carbono_para_revisar_manualmente.csv")
OUT_PANEL = Path("data/final/panel_con_tratamiento_actualizado.csv")

# Proyectos de Verra identificados como sospechosos en la revisión manual
# de esta sesión (coordenada no coincide con el 'city' declarado). Ajusta
# esta lista si revisas y decides que sí son confiables, o si encuentras
# más casos raros.
SOSPECHOSOS_VERRA = [
    "Boomitra grassland Restoration in Colombia",
]


def _parse_fecha(valor) -> int | None:
    """Extrae el año de un valor de fecha en cualquiera de los formatos que
    aparecen en las distintas fuentes (ISO 'YYYY-MM-DD...', o 'DD/MM/YYYY')."""
    if pd.isna(valor):
        return None
    texto = str(valor).strip()
    m = re.match(r"^(\d{4})-\d{2}-\d{2}", texto)
    if m:
        return int(m.group(1))
    m = re.match(r"^\d{2}/\d{2}/(\d{4})$", texto)
    if m:
        return int(m.group(1))
    m = re.search(r"(19|20)\d{2}", texto)
    if m:
        return int(m.group(0))
    return None


def _cargar_verra() -> pd.DataFrame:
    if not VERRA_FILE.exists():
        print(f"Aviso: no encuentro {VERRA_FILE}, se omite Verra.")
        return pd.DataFrame()

    df = pd.read_csv(VERRA_FILE, low_memory=False)
    df = df[df["COD_DANE"].notna()].copy()

    n_antes = len(df)
    if "projectName" in df.columns:
        df = df[~df["projectName"].isin(SOSPECHOSOS_VERRA)].copy()
    n_excluidos = n_antes - len(df)
    if n_excluidos > 0:
        print(f"Verra: excluidos {n_excluidos} proyecto(s) marcados como sospechosos.")

    campo_fecha = next(
        (c for c in ["projectStartDate", "creditPeriodStartDate", "startDate"] if c in df.columns),
        None,
    )
    campo_fecha_fin = next((c for c in ["projectEndDate", "creditPeriodEndDate"] if c in df.columns), None)

    out = pd.DataFrame({
        "COD_DANE": df["COD_DANE"].astype(str).str.zfill(5),
        "fuente": "verra",
        "nombre_proyecto": df.get("projectName", ""),
        "anio_inicio": df[campo_fecha].apply(_parse_fecha) if campo_fecha else None,
        "anio_fin": df[campo_fecha_fin].apply(_parse_fecha) if campo_fecha_fin else None,
        "confianza": "alta",
        "metodo": "punto_en_poligono",
    })
    print(f"Verra: {len(out)} eventos cargados ({out['anio_inicio'].notna().sum()} con año de inicio válido).")
    return out


def _cargar_goldstandard() -> pd.DataFrame:
    partes = []
    for path in [GOLDSTANDARD_FILE_1, GOLDSTANDARD_FILE_2]:
        if path.exists():
            partes.append(pd.read_csv(path, low_memory=False))
        else:
            print(f"Aviso: no encuentro {path}, se omite esa parte de Gold Standard.")

    if not partes:
        return pd.DataFrame()

    df = pd.concat(partes, ignore_index=True)
    df = df[df["COD_DANE"].notna()].copy()
    if "id" in df.columns:
        df = df.drop_duplicates(subset=["id"])

    campo_fecha = next(
        (c for c in ["crediting_period_start_date", "start_date", "registration_date"] if c in df.columns),
        None,
    )
    campo_fecha_fin = next((c for c in ["crediting_period_end_date", "end_date"] if c in df.columns), None)
    campo_nombre = next((c for c in ["name", "projectName"] if c in df.columns), None)

    out = pd.DataFrame({
        "COD_DANE": df["COD_DANE"].astype(str).str.zfill(5),
        "fuente": "gold_standard",
        "nombre_proyecto": df[campo_nombre] if campo_nombre else "",
        "anio_inicio": df[campo_fecha].apply(_parse_fecha) if campo_fecha else None,
        "anio_fin": df[campo_fecha_fin].apply(_parse_fecha) if campo_fecha_fin else None,
        "confianza": "alta",
        "metodo": "punto_en_poligono",
    })
    print(f"Gold Standard: {len(out)} eventos cargados ({out['anio_inicio'].notna().sum()} con año de inicio válido).")
    return out


def _extraer_fecha_actividades(valor) -> tuple[int | None, int | None]:
    """RENARE no trae fecha estructurada a nivel de proyecto — está anidada
    dentro del JSON de 'actividades' (fecha_inicial_1 / fecha_final_1)."""
    if pd.isna(valor):
        return (None, None)
    try:
        data = ast.literal_eval(str(valor))
    except (ValueError, SyntaxError):
        return (None, None)
    if isinstance(data, list) and data:
        data = data[0]
    if not isinstance(data, dict):
        return (None, None)
    return (_parse_fecha(data.get("fecha_inicial_1")), _parse_fecha(data.get("fecha_final_1")))


def _cargar_renare() -> pd.DataFrame:
    if not RENARE_MATCHING_FILE.exists():
        print(f"Aviso: no encuentro {RENARE_MATCHING_FILE}, se omite RENARE.")
        return pd.DataFrame()

    matching = pd.read_csv(RENARE_MATCHING_FILE, low_memory=False)
    # 'n_matches' se fuerza a numérico explícitamente: si por cualquier motivo
    # quedó guardado como texto en el CSV (ej. "1" en vez de 1), la
    # comparación '== 1' de más abajo fallaría en silencio para TODAS las
    # filas sin dar ningún error — como pasó en una corrida real de esta
    # sesión (mostró 0 matches cuando debían ser ~27).
    matching["n_matches"] = pd.to_numeric(matching["n_matches"], errors="coerce")
    print(f"  Diagnóstico n_matches en RENARE: {matching['n_matches'].value_counts(dropna=False).to_dict()}")
    unicos = matching[matching["n_matches"] == 1].copy()
    print(f"RENARE: {len(unicos)} matches únicos de {len(matching)} totales (el resto: ambiguos o sin match, no se incluyen aquí).")

    if RENARE_RAW_FILE.exists() and "actividades" in pd.read_csv(RENARE_RAW_FILE, nrows=1).columns:
        raw = pd.read_csv(RENARE_RAW_FILE, low_memory=False)[["_id", "actividades"]]
        unicos = unicos.merge(raw, on="_id", how="left")
        fechas = unicos["actividades"].apply(_extraer_fecha_actividades)
        unicos["anio_inicio"] = [f[0] for f in fechas]
        unicos["anio_fin"] = [f[1] for f in fechas]
    else:
        print("Aviso: no se pudo extraer fecha de RENARE (falta el archivo crudo con 'actividades').")
        unicos["anio_inicio"] = None
        unicos["anio_fin"] = None

    out = pd.DataFrame({
        "COD_DANE": unicos["cod_dane"].astype(str).str.zfill(5),
        "fuente": "renare",
        "nombre_proyecto": unicos["nombre_iniciativa"],
        "anio_inicio": unicos["anio_inicio"],
        "anio_fin": unicos["anio_fin"],
        "confianza": "media",
        "metodo": "matching_texto",
    })
    print(f"RENARE: {out['anio_inicio'].notna().sum()} / {len(out)} con año de inicio extraído.")
    return out


COLS_FECHA_INICIO_CERC = ["Duration start", "Start date first crediting period",
                          "Crediting periord start"]
COLS_FECHA_FIN_CERC = ["Duration end", "Crediting period end"]


def _coalesce_anio(fila, columnas):
    for c in columnas:
        if c in fila.index:
            anio = _parse_fecha(fila[c])
            if anio is not None:
                return anio, c
    return None, "sin_fecha"


def _cargar_cercarbono() -> pd.DataFrame:
    if not CERCARBONO_FILE.exists():
        print(f"Aviso: no encuentro {CERCARBONO_FILE}, se omite Cercarbono.")
        return pd.DataFrame()

    # dtype str: evita que 05142 llegue como 5142.0
    matching = pd.read_csv(CERCARBONO_FILE, low_memory=False,
                           dtype={"cod_dane": str, "Project ID": str})
    matching["n_matches"] = pd.to_numeric(matching["n_matches"], errors="coerce")
    unicos = matching[matching["n_matches"] == 1].copy()

    if "Project ID" in unicos.columns:
        n0 = len(unicos)
        unicos = unicos.drop_duplicates(subset="Project ID")
        if len(unicos) < n0:
            print(f"  Aviso: {n0 - len(unicos)} filas repetidas por Project ID. "
                  "Re-corre 19 con la version que colapsa el reporte.")
    faltan = [c for c in COLS_FECHA_INICIO_CERC if c not in unicos.columns]
    if faltan:
        print(f"  Aviso: faltan columnas de fecha {faltan}. Amplia cols_base en 19 y re-corre.")
    print(f"Cercarbono: {len(unicos)} matches unicos de {len(matching)} totales.")

    inicio = [_coalesce_anio(r, COLS_FECHA_INICIO_CERC) for _, r in unicos.iterrows()]
    fin = [_coalesce_anio(r, COLS_FECHA_FIN_CERC)[0] for _, r in unicos.iterrows()]

    out = pd.DataFrame({
        "COD_DANE": unicos["cod_dane"].astype(str).str.replace(r"\.0$", "", regex=True)
                    .str.zfill(5).to_numpy(),
        "fuente": "cercarbono",
        "nombre_proyecto": unicos["Project Name"].to_numpy(),
        "id_registro": unicos["Project ID"].to_numpy() if "Project ID" in unicos else "",
        "anio_inicio": [a for a, _ in inicio],
        "anio_fin": fin,
        "fecha_origen": [o for _, o in inicio],
        "confianza": "media",
        "metodo": "matching_texto",
    })

    print("  Origen del año de inicio:")
    print("    " + out["fecha_origen"].value_counts().to_string().replace("\n", "\n    "))
    sin = out[out["anio_inicio"].isna()]
    if len(sin):
        print(f"  Aviso: {len(sin)} proyectos sin ninguna fecha; no recibiran tratamiento:")
        print("    " + sin[["id_registro", "nombre_proyecto"]].to_string(index=False)
              .replace("\n", "\n    "))
    return out


def _guardar_ambiguos() -> None:
    """Junta los ambiguos de RENARE y Cercarbono en un solo archivo para
    revisión manual — no entran al panel automáticamente."""
    partes = []
    if RENARE_MATCHING_FILE.exists():
        m = pd.read_csv(RENARE_MATCHING_FILE, low_memory=False)
        amb = m[m["n_matches"] > 1].copy()
        amb["fuente"] = "renare"
        partes.append(amb[["fuente", "nombre_iniciativa", "municipio", "cod_dane"]].rename(
            columns={"nombre_iniciativa": "nombre_proyecto", "municipio": "municipios_mencionados"}
        ))
    if CERCARBONO_FILE.exists():
        m = pd.read_csv(CERCARBONO_FILE, low_memory=False, dtype={"cod_dane": str, "Project ID": str})
        m["n_matches"] = pd.to_numeric(m["n_matches"], errors="coerce")
        amb = m[m["n_matches"] > 1].drop_duplicates("Project ID").copy()
        amb["fuente"] = "cercarbono"
        cols = [c for c in ["fuente", "Project ID", "Project Name", "Sector", "municipio", "cod_dane"]
                if c in amb.columns]
        partes.append(amb[cols].rename(columns={
            "Project Name": "nombre_proyecto", "municipio": "municipios_mencionados"}))
    if partes:
        pd.concat(partes, ignore_index=True).to_csv(OUT_REVISAR, index=False, encoding="utf-8-sig")
        print(f"\nAmbiguos guardados para revisión manual en: {OUT_REVISAR}")


def _elegir_panel_base() -> Path | None:
    """Elige el panel base más completo disponible, en vez de asumir por
    nombre de archivo cuál es el 'real'. Se detectó en esta sesión que
    data/final/panel_municipio_year.csv puede existir como un artefacto
    intermedio del pipeline (mismas columnas que el panel final, pero
    poblacion_dane/temp_media_c/prec_anual_mm sin poblar todavía) — si se
    usa ese por accidente, el resultado 'se ve' bien (mismas columnas) pero
    pierde en silencio los covariables ya calculados.
    """
    candidatos = [p for p in [PANEL_FILE, PANEL_FILE_ALT] if p.exists()]
    if not candidatos:
        return None
    if len(candidatos) == 1:
        return candidatos[0]

    # Si hay más de un candidato: preferir el que tenga MENOS nulos en
    # columnas de covariables clave (evidencia de ser el panel realmente
    # completo), no el primero que exista.
    print(f"\nAviso: encontré más de un panel candidato ({[str(c) for c in candidatos]}).")
    mejor, mejor_score = None, -1
    for c in candidatos:
        df_probe = pd.read_csv(c, low_memory=False, nrows=None)
        cols_clave = [col for col in ["poblacion_dane", "temp_media_c", "loss_area_ha"] if col in df_probe.columns]
        if not cols_clave:
            score = 0
        else:
            score = sum(df_probe[col].notna().mean() for col in cols_clave) / len(cols_clave)
        print(f"  {c}: {len(df_probe)} filas, completitud de covariables clave = {score:.1%}")
        if score > mejor_score:
            mejor, mejor_score = c, score

    print(f"  -> Usando {mejor} (el más completo).")
    if mejor_score < 0.5:
        print(
            "  *** AVISO: incluso el mejor candidato tiene menos del 50% de covariables "
            "clave pobladas. Revisa manualmente cuál es tu panel final real antes de "
            "confiar en este resultado. ***"
        )
    return mejor


def _construir_panel_tratamiento(eventos: pd.DataFrame) -> None:
    panel_path = _elegir_panel_base()
    if panel_path is None:
        print(f"\nAviso: no encuentro el panel final ({PANEL_FILE} ni {PANEL_FILE_ALT}). "
              f"No se puede fusionar — pero {OUT_EVENTOS} ya quedó listo si quieres hacerlo a mano.")
        return

    panel = pd.read_csv(panel_path, low_memory=False)
    panel["COD_DANE"] = panel["COD_DANE"].astype(str).str.zfill(5)

    eventos = anotar_sector(eventos, estricto=True)
    con_fecha = eventos[eventos["anio_inicio"].notna()].copy()
    con_fecha = con_fecha.drop_duplicates(subset=["COD_DANE", "nombre_proyecto"]) 
    con_fecha = eventos[eventos["anio_inicio"].notna()].copy()
    
    con_fecha["anio_inicio"] = con_fecha["anio_inicio"].astype(int)

    respaldo = con_fecha.get("fecha_origen", pd.Series(index=con_fecha.index, dtype=object)) \
                    .isin(["Start date first crediting period", "Crediting periord start"])
    especificaciones = {
        "alta_confianza": con_fecha["confianza"].eq("alta"),
        "todas_fuentes": con_fecha["confianza"].isin(["alta", "media"]),
        "todas_sin_fecha_respaldo": con_fecha["confianza"].isin(["alta", "media"]) & ~respaldo,
    }
    resultado = panel.copy()
    for sufijo, mascara in especificaciones.items():
        subset = con_fecha[mascara]
        primero = subset.groupby("COD_DANE")["anio_inicio"].min() \
                        .rename(f"anio_inicio_tratamiento_{sufijo}")
        resultado = resultado.merge(primero, on="COD_DANE", how="left")
        resultado[f"tratado_{sufijo}"] = (
            resultado["year"] >= resultado[f"anio_inicio_tratamiento_{sufijo}"]
        ).fillna(False).astype(int)

    resultado.to_csv(OUT_PANEL, index=False, encoding="utf-8-sig")
    print(f"\nPanel con tratamiento actualizado guardado en: {OUT_PANEL}")
    print(f"(NO se sobreescribió {panel_path} — compara antes de reemplazarlo)")

    n_mun_alta = con_fecha[con_fecha["confianza"] == "alta"]["COD_DANE"].nunique()
    n_mun_todas = con_fecha["COD_DANE"].nunique()
    print(f"\nMunicipios tratados (solo alta confianza — Verra + Gold Standard): {n_mun_alta}")
    print(f"Municipios tratados (alta + media confianza — + RENARE + Cercarbono): {n_mun_todas}")


from filtro_sectorial import anotar_sector
from correcciones_municipio import aplicar_correcciones


def main() -> None:
    print("=" * 70)
    print("Cargando y normalizando cada fuente...")
    print("=" * 70)

    eventos = pd.concat(
        [_cargar_verra(), _cargar_goldstandard(), _cargar_renare(), _cargar_cercarbono()],
        ignore_index=True,
    )
    eventos = anotar_sector(eventos)            # anota; falla si hay pendientes
    eventos = aplicar_correcciones(eventos)     # antes de filtrar, para no dejar huerfanas
    eventos = eventos[eventos["sector"] == "AFOLU"].copy()

    OUT_EVENTOS.parent.mkdir(parents=True, exist_ok=True)
    eventos.to_csv(OUT_EVENTOS, index=False, encoding="utf-8-sig")
    print(f"\n{'=' * 70}")
    print(f"Total eventos consolidados: {len(eventos)}")
    print(f"Municipios distintos (todas las fuentes, todas las confianzas): {eventos['COD_DANE'].nunique()}")
    print(f"Guardado en: {OUT_EVENTOS}")
    print("*** Revisa este archivo antes de confiar en el panel final. ***")

    _guardar_ambiguos()

    print(f"\n{'=' * 70}")
    print("Construyendo columnas de tratamiento en el panel...")
    print("=" * 70)
    _construir_panel_tratamiento(eventos)


if __name__ == "__main__":
    main()
