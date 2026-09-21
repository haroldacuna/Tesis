"""
26_consolidar_fuentes_carbono.py

Une las 4 fuentes de proyectos de carbono en una sola tabla de eventos (un
renglon por proyecto con su municipio y anio de inicio), y de ahi arma las
columnas de tratamiento a nivel municipio-anio para el panel final.

CAMBIOS DE ESTA VERSION (19/09/2026)
------------------------------------
1. EJE REDD. La tabla de decision ahora tiene dos ejes ortogonales:
       clasificacion_final  AFOLU / NO_AFOLU   -> define D3
       clase_redd_final     REDD  / NO_REDD    -> define D1
   Se anotan los dos y se generan columnas de tratamiento para ambos. Antes
   solo se llamaba anotar_sector, asi que el eje REDD no llegaba al panel.

2. DEDUPLICACION VIVA. El drop_duplicates estaba escrito pero muerto: la
   linea siguiente reasignaba con_fecha desde eventos y lo descartaba. Ahora
   se aplica y se reporta cuantas filas quita. La llave es
   (COD_DANE, fuente, nombre_proyecto): se deduplica DENTRO de cada fuente,
   nunca entre fuentes, porque dos registros del mismo municipio en
   plataformas distintas suelen ser proyectos distintos y no duplicados.

3. SIN PRELACION POR FUENTE. Se evaluo y se descarto con datos: de los 9
   municipios donde min(anio) y una regla de prelacion discreparian, 8 los
   descarta el filtro sectorial (hidroelectricas, cogeneracion, rellenos).
   El unico que sobrevive es 27025 Alto Baudo, y ahi ACABA REDD+ (Verra,
   2019) y PROYECTO REDD+ CUENCA ALTO BAUDO (RENARE, 2018) se verificaron
   como proyectos DISTINTOS, no como un doble registro. Con cero duplicados
   cruzados reales, min(anio) identifica la primera intervencion y la
   prelacion seria una complicacion sin caso de uso.

4. FECHA DE INICIO. Se mantiene projectStartDate para Verra. Se verifico
   que coincide con creditPeriodStartDate en los 44 proyectos REDD+
   comparables (cero discrepancias de anio), asi que la eleccion no afecta
   la asignacion de cohortes. El unico sin ninguna de las dos fechas es
   REDD+ Yaguara Llanos del Yari, retirado, que nunca tuvo periodo de
   acreditacion.

5. DIFF CONTRA EL PANEL PREVIO. Si existe el respaldo
   panel_con_tratamiento_PRE_REDD_88_99.csv, al final se reporta cuantos
   municipios entran, salen y cambian de cohorte. Ese diff es la forma
   reproducible de cuantificar el efecto de la reclasificacion, y sustituye
   al contraste contra la clasificacion por titulo, que nunca se persistio
   como artefacto.

FUENTES Y NIVEL DE CONFIANZA
----------------------------
- Verra (Platts API): municipio por PUNTO-EN-POLIGONO. Confianza ALTA.
- Gold Standard: punto-en-poligono, incluidos los 2 corregidos por signo.
  Confianza ALTA.
- RENARE: matching de texto libre, solo matches UNICOS. Confianza MEDIA.
- Cercarbono: mismo metodo de texto, solo unicos. Confianza MEDIA.

Los AMBIGUOS de RENARE y Cercarbono quedan en un archivo aparte.

USO
---
    python 26_consolidar_fuentes_carbono.py
    python 26_consolidar_fuentes_carbono.py --no-estricto   # solo explorar
"""

from __future__ import annotations

import argparse
import ast
import re
from pathlib import Path

import pandas as pd

from filtro_sectorial import anotar_sector, anotar_redd, cargar_tabla

# ---------------------------------------------------------------------------
# Rutas de entrada
# ---------------------------------------------------------------------------
VERRA_FILE = Path("data/interim/verra_platts_colombia_con_municipio.csv")
GOLDSTANDARD_FILE_1 = Path("data/interim/goldstandard_projects_colombia_con_municipio.csv")
GOLDSTANDARD_FILE_2 = Path("data/interim/goldstandard_coords_corregidas.csv")
RENARE_MATCHING_FILE = Path("data/interim/diagnostics/renare_municipio_matching.csv")
RENARE_RAW_FILE = Path("data/interim/renare_solicitudes_colombia.csv")
CERCARBONO_FILE = Path("data/interim/diagnostics/cercarbono_municipio_matching.csv")
# Ubicacion recuperada a mano de las fichas del registro, via
# 37_resolver_ubicacion_cercarbono.py. Fuente separada a proposito: permite
# correr la especificacion CON y SIN ella.
CERCARBONO_MANUAL_FILE = Path("data/interim/cercarbono_ubicacion_manual.csv")
SOSPECHOSOS_VERRA = [
    "Boomitra grassland Restoration in Colombia",
    # Punto sobre Barranquilla, sede del proponente: 400.000 ha declaradas en
    # un municipio urbano de 15.413 ha. Ubicacion real desconocida.
    "CO2ROZO",
]

PANEL_FILE = Path("data/final/panel_municipio_year.csv")
PANEL_FILE_ALT = Path("data/final/dataset_consolidado_completo.csv")

OUT_EVENTOS = Path("data/interim/eventos_carbono_consolidado.csv")
OUT_REVISAR = Path("data/interim/eventos_carbono_para_revisar_manualmente.csv")
OUT_PANEL = Path("data/final/panel_con_tratamiento_actualizado.csv")

# Respaldo del panel anterior a la reclasificacion REDD+, para el diff final.
PANEL_PREVIO = Path("data/final/panel_con_tratamiento_PRE_REDD_88_99.csv")

# Estados de registro de Verra que indican que el proyecto NO llego a ser
# proyecto. Ver la seccion "D1 depurada" en _construir_panel_tratamiento.
ESTADOS_FALLIDOS = {"withdrawn", "rejected by administrator"}


def _parse_fecha(valor) -> int | None:
    """Extrae el anio de un valor de fecha en cualquiera de los formatos que
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
        # Comparacion normalizada: un espacio final o una mayuscula distinta
        # no deben dejar pasar un proyecto que se decidio excluir.
        sospechosos = {x.strip().casefold() for x in SOSPECHOSOS_VERRA}
        nombres = df["projectName"].astype(str).str.strip().str.casefold()
        df = df[~nombres.isin(sospechosos)].copy()
    n_excluidos = n_antes - len(df)
    if n_excluidos > 0:
        print(f"Verra: excluidos {n_excluidos} proyecto(s) marcados como sospechosos.")

    # projectStartDate primero: se verifico que coincide con
    # creditPeriodStartDate en los 44 REDD+ comparables.
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
        # Solo Verra publica estado de registro. Se arrastra para poder
        # distinguir proyectos vivos de retirados o rechazados.
        "estado_registro": df["status"].values if "status" in df.columns else None,
    })
    print(f"Verra: {len(out)} eventos cargados ({out['anio_inicio'].notna().sum()} con anio de inicio valido).")
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
    print(f"Gold Standard: {len(out)} eventos cargados ({out['anio_inicio'].notna().sum()} con anio de inicio valido).")
    return out


def _extraer_fecha_actividades(valor) -> tuple[int | None, int | None]:
    """RENARE no trae fecha estructurada a nivel de proyecto: esta anidada
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
    # 'n_matches' se fuerza a numerico explicitamente: si quedo guardado como
    # texto, la comparacion '== 1' falla en silencio para TODAS las filas.
    matching["n_matches"] = pd.to_numeric(matching["n_matches"], errors="coerce")
    print(f"  Diagnostico n_matches en RENARE: {matching['n_matches'].value_counts(dropna=False).to_dict()}")
    unicos = matching[matching["n_matches"] == 1].copy()
    print(f"RENARE: {len(unicos)} matches unicos de {len(matching)} totales.")

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
    print(f"RENARE: {out['anio_inicio'].notna().sum()} / {len(out)} con anio de inicio extraido.")
    return out


def _cargar_cercarbono() -> pd.DataFrame:
    if not CERCARBONO_FILE.exists():
        print(f"Aviso: no encuentro {CERCARBONO_FILE}, se omite Cercarbono.")
        return pd.DataFrame()

    matching = pd.read_csv(CERCARBONO_FILE, low_memory=False)
    matching["n_matches"] = pd.to_numeric(matching["n_matches"], errors="coerce")
    unicos = matching[matching["n_matches"] == 1].copy()
    print(f"Cercarbono: {len(unicos)} matches unicos de {len(matching)} totales.")

    campo_fecha = next((c for c in ["Duration start"] if c in unicos.columns), None)
    campo_fecha_fin = next((c for c in ["Duration end"] if c in unicos.columns), None)

    out = pd.DataFrame({
        "COD_DANE": unicos["cod_dane"].astype(str).str.zfill(5),
        "fuente": "cercarbono",
        "nombre_proyecto": unicos.get("Project Name", ""),
        "anio_inicio": unicos[campo_fecha].apply(_parse_fecha) if campo_fecha else None,
        "anio_fin": unicos[campo_fecha_fin].apply(_parse_fecha) if campo_fecha_fin else None,
        "confianza": "media",
        "metodo": "matching_texto",
    })

    n_sin_fecha = out["anio_inicio"].isna().sum()
    if n_sin_fecha > 0 and campo_fecha:
        print(f"  Aviso: {n_sin_fecha} proyectos sin anio de inicio extraido.")
        crudos = unicos.loc[out["anio_inicio"].isna(), campo_fecha]
        print(f"    Valores crudos que fallaron: {crudos.value_counts(dropna=False).head(10).to_dict()}")
        print("    (si salen todos NaN es un hueco real de Cercarbono, no del parser)")
    print(f"Cercarbono: {out['anio_inicio'].notna().sum()} / {len(out)} con anio de inicio extraido.")
    return out


def _cargar_cercarbono_manual() -> pd.DataFrame:
    """Proyectos REDD+ de Cercarbono cuya ubicacion se recupero a mano.

    Estos proyectos no los alcanza ninguna via automatica: OffsetsDB trae
    cod_dane vacio y el matching por texto falla porque los nombres son
    toponimos en lenguas indigenas, no nombres de municipio. La ubicacion
    sale de la ficha publica del registro, con la frase que la sustenta
    guardada en la columna 'evidencia' del CSV.

    Se marca con metodo='ficha_registro_manual' para poder aislarla despues:
    la comparacion con y sin esta fuente es la prueba de robustez frente al
    error de cobertura documentado en el Capitulo 5.

    Un proyecto puede aportar VARIOS municipios (una fila por par), que es
    justamente lo que el matching por texto no podia representar.
    """
    if not CERCARBONO_MANUAL_FILE.exists():
        print(f"Aviso: no encuentro {CERCARBONO_MANUAL_FILE}; se omite la ubicacion manual de Cercarbono.")
        return pd.DataFrame()

    df = pd.read_csv(CERCARBONO_MANUAL_FILE, low_memory=False)
    if df.empty:
        print("Aviso: cercarbono_ubicacion_manual.csv esta vacio.")
        return pd.DataFrame()

    out = pd.DataFrame({
        "COD_DANE": df["COD_DANE"].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5),
        "fuente": "cercarbono",
        "nombre_proyecto": df["nombre_proyecto"],
        "anio_inicio": pd.to_numeric(df["anio_inicio"], errors="coerce"),
        "anio_fin": pd.to_numeric(df.get("anio_fin"), errors="coerce"),
        # La confianza viene calibrada por evidencia en la plantilla: alta si
        # la ficha nombra municipios explicitamente, media si hubo inferencia.
        "confianza": df["confianza"].astype(str).str.strip().str.lower(),
        "metodo": "ficha_registro_manual",
    })
    n_proy = out["nombre_proyecto"].nunique()
    print(f"Cercarbono (ficha manual): {len(out)} eventos, {n_proy} proyectos, "
          f"{out['COD_DANE'].nunique()} municipios.")
    print("  " + out.groupby("confianza")["COD_DANE"].nunique().to_string().replace("\n", "\n  "))
    return out


def _guardar_ambiguos() -> None:
    """Junta los ambiguos de RENARE y Cercarbono para revision manual."""
    partes = []
    if RENARE_MATCHING_FILE.exists():
        m = pd.read_csv(RENARE_MATCHING_FILE, low_memory=False)
        m["n_matches"] = pd.to_numeric(m["n_matches"], errors="coerce")
        amb = m[m["n_matches"] > 1].copy()
        amb["fuente"] = "renare"
        partes.append(amb[["fuente", "nombre_iniciativa", "municipio", "cod_dane"]].rename(
            columns={"nombre_iniciativa": "nombre_proyecto", "municipio": "municipios_mencionados"}
        ))
    if CERCARBONO_FILE.exists():
        m = pd.read_csv(CERCARBONO_FILE, low_memory=False)
        m["n_matches"] = pd.to_numeric(m["n_matches"], errors="coerce")
        amb = m[m["n_matches"] > 1].copy()
        amb["fuente"] = "cercarbono"
        partes.append(amb[["fuente", "Project Name", "municipio", "cod_dane"]].rename(
            columns={"Project Name": "nombre_proyecto", "municipio": "municipios_mencionados"}
        ))
    if partes:
        pd.concat(partes, ignore_index=True).to_csv(OUT_REVISAR, index=False, encoding="utf-8-sig")
        print(f"\nAmbiguos guardados para revision manual en: {OUT_REVISAR}")


def _preflight(eventos: pd.DataFrame) -> bool:
    """Lista SOLO los proyectos que llegan a los eventos consolidados y aun
    no tienen decision en alguno de los dos ejes.

    La tabla completa tiene cientos de pendientes, pero la mayoria no entra
    nunca al panel (sin municipio o sin fecha). Revisar los 236 seria
    trabajo perdido; estos son los que de verdad bloquean.
    """
    from filtro_sectorial import clave_proyecto

    try:
        tabla = cargar_tabla()
    except (FileNotFoundError, ValueError) as exc:
        print(f"\n[X] {exc}")
        return False

    ok = True
    for col, etiqueta in [("clasificacion_final", "sectorial"), ("clase_redd_final", "REDD")]:
        if col not in tabla.columns:
            print(f"\n[X] La tabla de decision no tiene '{col}'.")
            print("    Corre 28_clasificar_sector.py con la version que incluye el eje REDD.")
            ok = False
            continue
        pendientes = set(tabla.loc[tabla[col].str.strip() == "", "clave"])
        claves_ev = {clave_proyecto(f, n) for f, n in zip(eventos["fuente"], eventos["nombre_proyecto"])}
        bloquean = sorted(pendientes & claves_ev)
        if bloquean:
            ok = False
            print(f"\n[X] {len(bloquean)} proyecto(s) llegan al panel sin decision en el eje {etiqueta} "
                  f"('{col}'):")
            vista = tabla[tabla["clave"].isin(bloquean)][["fuente", "nombre_proyecto"]]
            for _, r in vista.iterrows():
                print(f"      [{r['fuente']}] {str(r['nombre_proyecto'])[:80]}")
            print(f"    Son los unicos que bloquean: el resto de pendientes de la tabla no entra al panel.")
    return ok


def _elegir_panel_base() -> Path | None:
    """Elige el panel base mas completo disponible.

    panel_municipio_year.csv puede existir como artefacto intermedio (mismas
    columnas que el final pero covariables sin poblar); si se usa por
    accidente el resultado 'se ve' bien y pierde covariables en silencio.
    """
    candidatos = [p for p in [PANEL_FILE, PANEL_FILE_ALT] if p.exists()]
    if not candidatos:
        return None
    if len(candidatos) == 1:
        return candidatos[0]

    print(f"\nAviso: hay mas de un panel candidato ({[str(c) for c in candidatos]}).")
    mejor, mejor_score = None, -1
    for c in candidatos:
        df_probe = pd.read_csv(c, low_memory=False)
        cols_clave = [col for col in ["poblacion_dane", "temp_media_c", "loss_area_ha"] if col in df_probe.columns]
        score = 0 if not cols_clave else sum(df_probe[col].notna().mean() for col in cols_clave) / len(cols_clave)
        print(f"  {c}: {len(df_probe)} filas, completitud de covariables clave = {score:.1%}")
        if score > mejor_score:
            mejor, mejor_score = c, score

    print(f"  -> Usando {mejor} (el mas completo).")
    if mejor_score < 0.5:
        print("  *** AVISO: el mejor candidato tiene menos del 50% de covariables clave "
              "pobladas. Revisa cual es tu panel final real. ***")
    return mejor


def _diff_contra_previo(resultado: pd.DataFrame) -> None:
    """Compara el tratamiento nuevo contra el respaldo previo a la
    reclasificacion REDD+. Cuantifica el efecto del cambio de definicion."""
    if not PANEL_PREVIO.exists():
        print(f"\n(no hay {PANEL_PREVIO}; se omite el diff contra el panel previo)")
        return

    previo = pd.read_csv(PANEL_PREVIO, low_memory=False)
    previo["COD_DANE"] = previo["COD_DANE"].astype(str).str.zfill(5)

    print("\n" + "=" * 70)
    print("DIFF CONTRA EL PANEL PREVIO A LA RECLASIFICACION")
    print("=" * 70)

    for col_prev, col_new in [
        ("anio_inicio_tratamiento_alta_confianza", "anio_inicio_tratamiento_alta_confianza"),
        ("anio_inicio_tratamiento_todas_fuentes", "anio_inicio_tratamiento_todas_fuentes"),
        ("anio_inicio_tratamiento_todas_fuentes", "anio_inicio_tratamiento_todas_fuentes_redd"),
        ("anio_inicio_tratamiento_todas_fuentes", "anio_inicio_tratamiento_todas_fuentes_redd_sinmanual"),
    ]:
        if col_prev not in previo.columns or col_new not in resultado.columns:
            continue
        a = previo.groupby("COD_DANE")[col_prev].first().dropna()
        b = resultado.groupby("COD_DANE")[col_new].first().dropna()
        entran = sorted(set(b.index) - set(a.index))
        salen = sorted(set(a.index) - set(b.index))
        comunes = set(a.index) & set(b.index)
        cambian = sorted(c for c in comunes if a[c] != b[c])

        print(f"\n  {col_prev}  ->  {col_new}")
        print(f"    antes: {len(a)} municipios | ahora: {len(b)}")
        print(f"    entran: {len(entran)} | salen: {len(salen)} | cambian de cohorte: {len(cambian)}")
        if entran[:10]:
            print(f"      entran (primeros 10): {entran[:10]}")
        if salen[:10]:
            print(f"      salen  (primeros 10): {salen[:10]}")
        for c in cambian[:10]:
            print(f"      {c}: {int(a[c])} -> {int(b[c])}")


def _construir_panel_tratamiento(eventos: pd.DataFrame, estricto: bool) -> None:
    panel_path = _elegir_panel_base()
    if panel_path is None:
        print(f"\nAviso: no encuentro el panel final. No se puede fusionar, "
              f"pero {OUT_EVENTOS} ya quedo listo.")
        return

    panel = pd.read_csv(panel_path, low_memory=False)
    panel["COD_DANE"] = panel["COD_DANE"].astype(str).str.zfill(5)

    # Los dos ejes, en el mismo dataframe.
    eventos = anotar_sector(eventos, estricto=estricto)
    eventos = anotar_redd(eventos, estricto=estricto)

    con_fecha = eventos[eventos["anio_inicio"].notna()].copy()

    # Deduplicacion DENTRO de cada fuente. Antes esta linea estaba escrita
    # pero la siguiente la pisaba, asi que nunca se aplico.
    n_antes = len(con_fecha)
    con_fecha = con_fecha.drop_duplicates(subset=["COD_DANE", "fuente", "nombre_proyecto"])
    if n_antes != len(con_fecha):
        print(f"\nDeduplicacion: {n_antes - len(con_fecha)} fila(s) repetidas eliminadas "
              f"(misma fuente, mismo municipio, mismo nombre).")

    con_fecha["anio_inicio"] = con_fecha["anio_inicio"].astype(int)

    # (confianzas, filtro, sufijo). filtro: None | "AFOLU" | "REDD"
    combinaciones = [
        (["alta"],          "AFOLU", "alta_confianza"),
        (["alta", "media"], "AFOLU", "todas_fuentes"),
        (["alta"],          None,    "alta_confianza_sinfiltro"),
        (["alta", "media"], None,    "todas_fuentes_sinfiltro"),
        # D1: mecanismo REDD+ estricto, que es la definicion nueva
        (["alta"],          "REDD",  "alta_confianza_redd"),
        (["alta", "media"], "REDD",  "todas_fuentes_redd"),
    ]

    # D1 excluyendo la ubicacion recuperada a mano. La diferencia entre esta
    # columna y todas_fuentes_redd mide cuanto del tratamiento depende de la
    # recuperacion manual: es la robustez frente al error de cobertura.
    sin_manual = con_fecha[con_fecha["metodo"] != "ficha_registro_manual"]

    resultado = panel.copy()
    print("\nMunicipios tratados por definicion:")
    for confianzas, filtro, sufijo in combinaciones:
        subset = con_fecha[con_fecha["confianza"].isin(confianzas)]
        if filtro == "AFOLU":
            subset = subset[subset["sector"] == "AFOLU"]
        elif filtro == "REDD":
            subset = subset[subset["clase_redd"] == "REDD"]

        primero = subset.groupby("COD_DANE")["anio_inicio"].min().rename(
            f"anio_inicio_tratamiento_{sufijo}"
        )
        resultado = resultado.merge(primero, on="COD_DANE", how="left")
        resultado[f"tratado_{sufijo}"] = (
            resultado["year"] >= resultado[f"anio_inicio_tratamiento_{sufijo}"]
        ).fillna(False).astype(int)

        n_coh = subset.groupby("COD_DANE")["anio_inicio"].min().nunique()
        print(f"  {sufijo:32s} {len(primero):3d} municipios en {n_coh} cohortes")

    # La columna de contraste, fuera del bucle porque filtra por metodo
    primero_sm = sin_manual[
        (sin_manual["confianza"].isin(["alta", "media"])) & (sin_manual["clase_redd"] == "REDD")
    ].groupby("COD_DANE")["anio_inicio"].min().rename("anio_inicio_tratamiento_todas_fuentes_redd_sinmanual")
    resultado = resultado.merge(primero_sm, on="COD_DANE", how="left")
    resultado["tratado_todas_fuentes_redd_sinmanual"] = (
        resultado["year"] >= resultado["anio_inicio_tratamiento_todas_fuentes_redd_sinmanual"]
    ).fillna(False).astype(int)
    print(f"  {'todas_fuentes_redd_sinmanual':32s} {len(primero_sm):3d} municipios "
          f"en {primero_sm.nunique()} cohortes")
    # Aporte de la recuperacion manual: municipios que SOLO existen gracias a
    # ella. Se cuenta sobre municipios unicos, no sobre filas del panel.
    # Se computa sobre los EVENTOS y no sobre el panel: un municipio tratado
    # que no exista en el panel base desapareceria del conteo sin avisar.
    d1_con = con_fecha[
        con_fecha["confianza"].isin(["alta", "media"]) & (con_fecha["clase_redd"] == "REDD")
    ].groupby("COD_DANE")["anio_inicio"].min()
    con_m, sin_m = set(d1_con.index), set(primero_sm.index)
    solo_manual = sorted(con_m - sin_m)
    print(f"\n  Aporte de la recuperacion manual: {len(solo_manual)} municipios "
          f"que no entrarian sin ella")
    if solo_manual:
        print(f"    {solo_manual}")
    adelantan = sorted(
        c for c in (con_m & sin_m)
        if d1_con[c] != primero_sm[c]
    )
    if adelantan:
        print(f"    Ademas adelanta la cohorte de {len(adelantan)} municipio(s): {adelantan}")

    # ------------------------------------------------------------------
    # D1 DEPURADA POR ESTADO DE REGISTRO
    # Un proyecto Verra retirado (Withdrawn) o rechazado (Rejected by
    # Administrator) no llego a ser proyecto. Pudo haber ejecutado algo en el
    # territorio antes de salir -acuerdos, guardabosques, linea base- o nada,
    # asi que el tratamiento de su municipio es AMBIGUO. Dos decisiones:
    #
    #   1. La cohorte depurada se calcula sin esos eventos. Si un municipio
    #      tiene un proyecto vivo y uno retirado, la fecha la pone el vivo.
    #   2. Los municipios cuyo UNICO vinculo REDD+ es un proyecto fallido se
    #      marcan muestra_redd_depurada = 0 para EXCLUIRLOS de la estimacion.
    #      No se pasan a control: un tratamiento ambiguo contamina igual en
    #      cualquiera de los dos grupos.
    #
    # El conjunto ambiguo se calcula sobre TODOS los eventos REDD+, con y sin
    # fecha. Un proyecto retirado sin fecha (Yaguara, en Calamar) no activa
    # tratamiento, pero su municipio tampoco es un control limpio.
    #
    # Asimetria a declarar: solo Verra publica estado. En Cercarbono, RENARE
    # y Gold Standard la regla no se puede aplicar.
    # ------------------------------------------------------------------
    def _es_fallido(df_):
        if "estado_registro" not in df_.columns:
            return pd.Series(False, index=df_.index)
        return df_["estado_registro"].astype(str).str.strip().str.casefold().isin(ESTADOS_FALLIDOS)

    redd_todos = eventos[
        eventos["confianza"].isin(["alta", "media"]) & (eventos["clase_redd"] == "REDD")
    ].copy()
    redd_todos["_fallido"] = _es_fallido(redd_todos)
    por_mpio = redd_todos.groupby("COD_DANE")["_fallido"].all()
    ambiguos = sorted(por_mpio[por_mpio].index)

    vivos = con_fecha[
        con_fecha["confianza"].isin(["alta", "media"])
        & (con_fecha["clase_redd"] == "REDD")
        & ~_es_fallido(con_fecha)
    ]
    primero_dep = vivos.groupby("COD_DANE")["anio_inicio"].min().rename(
        "anio_inicio_tratamiento_todas_fuentes_redd_depurada"
    )
    resultado = resultado.merge(primero_dep, on="COD_DANE", how="left")
    resultado["tratado_todas_fuentes_redd_depurada"] = (
        resultado["year"] >= resultado["anio_inicio_tratamiento_todas_fuentes_redd_depurada"]
    ).fillna(False).astype(int)
    resultado["muestra_redd_depurada"] = (~resultado["COD_DANE"].isin(ambiguos)).astype(int)

    print(f"\n  {'todas_fuentes_redd_depurada':32s} {len(primero_dep):3d} municipios "
          f"en {primero_dep.nunique()} cohortes")

    nombre_mpio = redd_todos.groupby("COD_DANE")["nombre_proyecto"].first()
    estado_mpio = redd_todos.groupby("COD_DANE")["estado_registro"].apply(
        lambda x: sorted(set(x.dropna().astype(str))))
    print(f"\n  Tratamiento ambiguo -- unico vinculo REDD+ retirado o rechazado: "
          f"{len(ambiguos)} municipios")
    for c in ambiguos:
        print(f"    {c}  {estado_mpio.get(c, [])}  {str(nombre_mpio.get(c, ''))[:60]}")
    print("    -> muestra_redd_depurada = 0: se EXCLUYEN de la estimacion, no pasan a control.")

    mueven = [
        (c, int(d1_con[c]), int(primero_dep[c]))
        for c in sorted(set(d1_con.index) & set(primero_dep.index))
        if d1_con[c] != primero_dep[c]
    ]
    if mueven:
        print(f"\n  Cambian de cohorte al quitar los proyectos fallidos: {len(mueven)}")
        for c, a, b in mueven:
            print(f"    {c}: {a} -> {b}")

    fuera_d1 = sorted(set(ambiguos) - set(d1_con.index))
    if fuera_d1:
        print(f"\n  Ambiguos que hoy estaban en el grupo de CONTROL: {fuera_d1}")

    resultado.to_csv(OUT_PANEL, index=False, encoding="utf-8-sig")
    print(f"\nPanel con tratamiento guardado en: {OUT_PANEL}")
    print(f"(NO se sobreescribio {panel_path})")

    _diff_contra_previo(resultado)


def main() -> None:
    import os
    from filtro_sectorial import RAIZ_PROYECTO, avisar_carpeta_sombra

    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--no-estricto", action="store_true",
                    help="no abortar ante proyectos sin clasificar (solo para explorar; "
                         "lo no resuelto NO entra al tratamiento)")
    args = ap.parse_args()
    estricto = not args.no_estricto

    os.chdir(RAIZ_PROYECTO)
    print(f"Directorio de trabajo fijado en: {RAIZ_PROYECTO}")
    avisar_carpeta_sombra()

    print("=" * 70)
    print("Cargando y normalizando cada fuente...")
    print("=" * 70)

    eventos = pd.concat(
        [_cargar_verra(), _cargar_goldstandard(), _cargar_renare(),
         _cargar_cercarbono_manual(), _cargar_cercarbono()],
        ignore_index=True,
    )

    if eventos.empty or "COD_DANE" not in eventos.columns:
        print("\n[X] Ninguna fuente aporto eventos. Revisa los avisos de arriba:\n"
              "    lo mas probable es que estes corriendo desde el directorio\n"
              "    equivocado, o que falten los interinos de data/interim/.")
        raise SystemExit(1)

    OUT_EVENTOS.parent.mkdir(parents=True, exist_ok=True)
    eventos.to_csv(OUT_EVENTOS, index=False, encoding="utf-8-sig")
    print(f"\n{'=' * 70}")
    print(f"Total eventos consolidados: {len(eventos)}")
    print(f"Municipios distintos: {eventos['COD_DANE'].nunique()}")
    print(f"Guardado en: {OUT_EVENTOS}")

    _guardar_ambiguos()

    if estricto and not _preflight(eventos):
        print("\n" + "=" * 70)
        print("ABORTADO. Resuelve los proyectos listados arriba en")
        print("data/interim/clasificacion_sectorial.csv y vuelve a correr.")
        print("Para explorar sin resolverlos: --no-estricto")
        print("=" * 70)
        raise SystemExit(1)

    print(f"\n{'=' * 70}")
    print("Construyendo columnas de tratamiento en el panel...")
    print("=" * 70)
    _construir_panel_tratamiento(eventos, estricto)


if __name__ == "__main__":
    main()