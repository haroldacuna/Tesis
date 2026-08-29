"""
08_diagnostico_fuentes_carbono.py

OBJETIVO
--------
No reemplaza el pipeline de extracción (06_enrich_panel_external.py). Su único
trabajo es generar EVIDENCIA para poder corregirlo:

  A) Sondear las 4 fuentes que hoy dan 0 en todo el panel (EcoRegistry, Gold
     Standard, ACR, Climate Action Data Trust) y guardar la respuesta cruda
     de cada URL candidata para inspección.
  B) Buscar RENARE (fuente oficial citada en la propuesta pero ausente del
     pipeline) en el catálogo de datos.gov.co (el mismo portal Socrata que ya
     usan para población DANE) y en URLs directas de MinAmbiente.
  C) Auditar la calidad del *matching* municipio<->proyecto para Verra,
     Berkeley y OffsetsDB usando los cachés locales que ya tengas en
     data/interim/, reutilizando las mismas funciones de
     06_enrich_panel_external.py (para que el diagnóstico sea fiel al
     comportamiento real del pipeline). Exporta a CSV los proyectos con fecha
     de inicio válida pero SIN municipio asignado, para revisión manual.

CÓMO CORRERLO
-------------
Ejecutar desde la raíz del proyecto (donde están las carpetas data/ y los
scripts 01_...06_...), con el mismo entorno/venv que usas para el pipeline:

    python 08_diagnostico_fuentes_carbono.py

Debe estar en la MISMA carpeta que 06_enrich_panel_external.py (lo importa
como módulo para reutilizar sus funciones de matching).

QUÉ HACER CON LA SALIDA
------------------------
Todo queda en data/interim/diagnostics/:
  - reporte_diagnostico.txt              -> resumen legible, léelo primero
  - <fuente>_raw_response.txt            -> cuerpo crudo de cada URL probada
  - verra_unmatched_projects.csv         -> proyectos Verra sin municipio
  - berkeley_unmatched_projects.csv      -> proyectos Berkeley sin municipio
  - offsetsdb_unmatched_projects.csv     -> proyectos OffsetsDB sin municipio

Pégame el contenido de reporte_diagnostico.txt (o al menos los status codes y
los primeros ~300 caracteres de cada fuente) y seguimos: con eso escribo el
extractor real de RENARE y arreglo el matching.
"""

from __future__ import annotations

import importlib.util
import json
import sys
from pathlib import Path
from typing import Any

import pandas as pd
import requests

# ---------------------------------------------------------------------------
# Cargar 06_enrich_panel_external.py como módulo para reutilizar sus
# funciones (normalización de texto, extracción de año, matching, rutas de
# caché) en vez de duplicar lógica que podría desincronizarse.
# ---------------------------------------------------------------------------
ENRICH_SCRIPT = Path(__file__).resolve().parent / "06_enrich_panel_external.py"
if not ENRICH_SCRIPT.exists():
    print(f"ERROR: no encuentro {ENRICH_SCRIPT}. Corre este script desde la misma carpeta que 06_enrich_panel_external.py")
    sys.exit(1)

_spec = importlib.util.spec_from_file_location("enrich", ENRICH_SCRIPT)
enrich = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(enrich)  # type: ignore[union-attr]

DIAG_DIR = Path("data/interim/diagnostics")
DIAG_DIR.mkdir(parents=True, exist_ok=True)

HEADERS_BROWSER = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
        "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"
    ),
    "Accept": "application/json, text/html, */*",
}
TIMEOUT = 20
SNIPPET_CHARS = 500  # cuánto se imprime en consola; el archivo guarda hasta 20k

report_lines: list[str] = []


def _log(line: str = "") -> None:
    print(line)
    report_lines.append(line)


def _save_raw(name: str, text: str) -> Path:
    path = DIAG_DIR / f"{name}_raw_response.txt"
    path.write_text(text[:20000], encoding="utf-8", errors="replace")
    return path


def _probe(name: str, url: str, method: str = "GET", **kwargs) -> dict[str, Any]:
    """GET/POST a una URL, sin lanzar excepciones. Guarda el cuerpo crudo."""
    result: dict[str, Any] = {
        "name": name, "url": url, "ok": False, "status": None,
        "content_type": None, "text": "", "error": None,
    }
    try:
        kwargs.setdefault("headers", HEADERS_BROWSER)
        kwargs.setdefault("timeout", TIMEOUT)
        resp = requests.request(method, url, **kwargs)
        result["status"] = resp.status_code
        result["content_type"] = resp.headers.get("Content-Type", "")
        result["ok"] = resp.status_code == 200
        result["text"] = resp.text or ""
        _save_raw(name, result["text"])
    except Exception as exc:
        result["error"] = str(exc)
    return result


def _report_probe(r: dict[str, Any]) -> None:
    _log(f"\n--- {r['name']} ---")
    _log(f"URL: {r['url']}")
    if r["error"]:
        _log(f"ERROR de red/DNS: {r['error']}")
        return
    _log(f"Status: {r['status']}  |  Content-Type: {r['content_type']}")
    preview = r["text"][:SNIPPET_CHARS].replace("\n", " ").strip()
    _log(f"Primeros {SNIPPET_CHARS} chars: {preview}")
    _log(f"(cuerpo completo guardado en {DIAG_DIR / (r['name'] + '_raw_response.txt')})")


# =============================================================================
# A) Sondeo de las 4 fuentes en cero
# =============================================================================

def diagnosticar_ecoregistry() -> None:
    _log("\n" + "=" * 70)
    _log("A1) ECOREGISTRY (registro voluntario colombiano, Bolsa Mercantil de Colombia)")
    _log("=" * 70)
    candidatos = [
        "https://ecoregistry.io",
        "https://ecoregistry.io/projects",
        "https://ecoregistry.io/api/projects",
        "https://ecoregistry.io/api/v1/projects",
        "https://ecoregistry.co",
        "https://registry.ecoregistry.io",
    ]
    for url in candidatos:
        _report_probe(_probe(f"ecoregistry_{_slug(url)}", url))


def diagnosticar_goldstandard() -> None:
    _log("\n" + "=" * 70)
    _log("A2) GOLD STANDARD (registry.goldstandard.org)")
    _log("=" * 70)
    candidatos = [
        "https://registry.goldstandard.org",
        "https://registry.goldstandard.org/projects?q=colombia",
        "https://registry.goldstandard.org/projects?country=CO",
        "https://api.goldstandard.org/projects?country=CO",
        "https://public.goldstandard.org/api/projects",
    ]
    for url in candidatos:
        _report_probe(_probe(f"goldstandard_{_slug(url)}", url))


def diagnosticar_acr() -> None:
    _log("\n" + "=" * 70)
    _log("A3) ACR - American Carbon Registry (plataforma APX, acr2.apx.com)")
    _log("=" * 70)
    _log("Nota: las plataformas APX (ACR, Climate Action Reserve) suelen tener")
    _log("reportes públicos exportables como CSV sin login, bajo /mymodule/reg/.")
    candidatos = [
        "https://acr2.apx.com",
        "https://acr2.apx.com/mymodule/reg/prjView.asp",
        "https://acr2.apx.com/mymodule/reg/tabs/ProjectTab.asp?r=111&at=1",
        "https://americancarbonregistry.org/carbon-accounting/project-database",
    ]
    for url in candidatos:
        _report_probe(_probe(f"acr_{_slug(url)}", url))


def diagnosticar_cad_trust() -> None:
    _log("\n" + "=" * 70)
    _log("A4) CLIMATE ACTION DATA TRUST (agregador multi-registro: Verra, GS, ACR, CDM...)")
    _log("=" * 70)
    _log("Si esta fuente responde con datos estructurados, podría cubrir Verra,")
    _log("Gold Standard, ACR y CDM en una sola llamada armonizada.")
    candidatos = [
        "https://climateactiondata.org",
        "https://api.climateactiondata.org",
        "https://api.climateactiondata.org/metadata/search?country=CO",
        "https://explorer.climateactiondata.org",
    ]
    for url in candidatos:
        _report_probe(_probe(f"cadtrust_{_slug(url)}", url))


# =============================================================================
# B) RENARE — buscarlo en el catálogo Socrata de datos.gov.co y en URLs de
#    MinAmbiente. Reutiliza el mismo patrón que ya usan para poblacion DANE.
# =============================================================================

def diagnosticar_renare() -> None:
    _log("\n" + "=" * 70)
    _log("B) RENARE (fuente oficial citada en la propuesta, ausente del pipeline)")
    _log("=" * 70)

    _log("\n-- B1) Búsqueda en catálogo Socrata de datos.gov.co --")
    catalog_url = "https://www.datos.gov.co/api/catalog/v1"
    for query in ["RENARE", "reduccion de emisiones", "bonos de carbono", "proyectos REDD", "carbono forestal"]:
        r = _probe(f"renare_catalog_{_slug(query)}", catalog_url, params={"q": query, "limit": 10})
        _report_probe(r)
        if r["ok"]:
            try:
                data = json.loads(r["text"])
                hits = data.get("results", [])
                _log(f"  -> {len(hits)} datasets encontrados para '{query}':")
                for h in hits[:6]:
                    res = h.get("resource", {})
                    _log(
                        f"     * {res.get('name')} | id={res.get('id')} | "
                        f"actualizado={res.get('updatedAt')}"
                    )
            except Exception as exc:
                _log(f"  (no se pudo parsear JSON del catálogo: {exc})")

    _log("\n-- B2) URLs directas plausibles de MinAmbiente --")
    candidatos = [
        "https://renare.minambiente.gov.co",
        "http://renare.minambiente.gov.co",
        "https://www.minambiente.gov.co/negocios-verdes-y-sostenibles/renare/",
        "https://www.minambiente.gov.co/cambio-climatico/renare/",
        "https://www.minambiente.gov.co/registro-nacional-de-reduccion-de-las-emisiones-de-gei-renare/",
    ]
    for url in candidatos:
        _report_probe(_probe(f"renare_direct_{_slug(url)}", url))

    _log(
        "\nSi ninguna de estas responde 200 con datos útiles: busca manualmente "
        "'RENARE' en https://www.datos.gov.co y pégame el resource id (el "
        "código tipo 'xxxx-xxxx' en la URL del dataset) — con eso escribo el "
        "extractor Socrata real, igual que ya hacen con población DANE."
    )


# =============================================================================
# C) Auditoría de matching municipio<->proyecto para Verra/Berkeley/OffsetsDB
#    usando los cachés locales y las funciones reales del pipeline.
# =============================================================================

def _slug(text: str) -> str:
    return (
        text.replace("https://", "").replace("http://", "")
        .replace("/", "_").replace("?", "_").replace("=", "_").replace("&", "_")
        .replace(" ", "_").strip("_")[:60]
    )


def _cargar_mun_panel() -> pd.DataFrame | None:
    """Reconstruye mun_panel (COD_DANE, NOMBRE_MPI) desde el panel final si existe."""
    if enrich.INPUT_PANEL.exists():
        panel = pd.read_csv(enrich.INPUT_PANEL)
        if {"COD_DANE", "NOMBRE_MPI"}.issubset(panel.columns):
            mun_panel = panel[["COD_DANE", "NOMBRE_MPI"]].drop_duplicates().copy()
            mun_panel["COD_DANE"] = mun_panel["COD_DANE"].astype(str)
            return mun_panel
    _log(f"Aviso: no encontré {enrich.INPUT_PANEL} para reconstruir el listado de municipios.")
    return None


def _encontrar_no_matcheados(
    df_projects: pd.DataFrame,
    mun_panel: pd.DataFrame,
    start_field: str,
    text_fields_hint: list[str],
) -> pd.DataFrame:
    """Reproduce la lógica de matching de enrich._compute_projects_active_counts_by_municipio
    pero devuelve, en vez de conteos, las filas que tenían fecha de inicio válida
    y NO obtuvieron ningún código de municipio."""
    if df_projects.empty:
        return pd.DataFrame()

    mun_by_name: dict[str, set[str]] = {}
    for _, mrow in mun_panel[["COD_DANE", "NOMBRE_MPI"]].drop_duplicates().iterrows():
        nm = enrich._normalize_text(mrow["NOMBRE_MPI"])
        mun_by_name.setdefault(nm, set()).add(str(mrow["COD_DANE"]))
    mun_name_tokens = [
        (enrich._normalize_text(name), str(code))
        for code, name in mun_panel[["COD_DANE", "NOMBRE_MPI"]].drop_duplicates().itertuples(index=False)
    ]

    unmatched_rows = []
    n_with_date = 0
    for _, row in df_projects.iterrows():
        start_year = None
        for field in [start_field, "start_date", "startDate", "projectStart", "project_start"]:
            if field in row and pd.notna(row[field]):
                start_year = enrich._to_year(row[field])
                if start_year is not None:
                    break
        if start_year is None:
            continue
        n_with_date += 1

        target_codes: set[str] = set()
        for field in ["cod_dane", "codigo_municipio", "municipio_code", "municipality_code"]:
            if field in row and pd.notna(row[field]):
                import re as _re
                for code in _re.findall(r"\d{4,6}", str(row[field])):
                    if code.zfill(5) in mun_panel["COD_DANE"].astype(str).unique():
                        target_codes.add(code.zfill(5))
        for field in ["municipio", "municipality", "location", "region"]:
            if field in row and pd.notna(row[field]):
                import re as _re
                for part in _re.split(r"[;,|/]", str(row[field])):
                    nm = enrich._normalize_text(part)
                    if nm in mun_by_name:
                        target_codes.update(mun_by_name[nm])
        if not target_codes:
            haystack = " ".join(str(row.get(f, "")) for f in row.index if pd.notna(row[f]))
            haystack_norm = enrich._normalize_text(haystack)
            for token, code in mun_name_tokens:
                if token and token in haystack_norm:
                    target_codes.add(code)

        if not target_codes:
            keep_cols = {f: row.get(f) for f in text_fields_hint if f in row}
            keep_cols["_start_year_detectado"] = start_year
            unmatched_rows.append(keep_cols)

    _log(f"  Proyectos con fecha de inicio válida: {n_with_date}")
    _log(f"  Proyectos SIN municipio asignado: {len(unmatched_rows)} "
         f"({(len(unmatched_rows) / n_with_date * 100) if n_with_date else 0:.1f}% del total con fecha)")
    return pd.DataFrame(unmatched_rows)


def auditar_matching() -> None:
    _log("\n" + "=" * 70)
    _log("C) AUDITORÍA DE MATCHING MUNICIPIO<->PROYECTO (Verra, Berkeley, OffsetsDB)")
    _log("=" * 70)

    mun_panel = _cargar_mun_panel()
    if mun_panel is None:
        _log("No se pudo auditar el matching: falta el panel final para listar municipios.")
        return

    fuentes = [
        ("Verra", enrich.VERA_CACHE, "creditingPeriodStartDate",
         ["name", "projectName", "description", "stateProvince", "countryArea", "location", "municipalities"]),
        ("Berkeley", enrich.BERKELEY_CACHE, "startDate",
         ["Project Name", "name", "location", "region", "municipio", "State/Province"]),
        ("OffsetsDB", enrich.OFFSETSDB_PROJECTS_CACHE, "startDate",
         ["name", "location", "region", "municipalities", "cod_dane"]),
    ]

    for label, cache_path, start_field, text_fields in fuentes:
        _log(f"\n-- {label} (caché: {cache_path}) --")
        if not cache_path.exists():
            _log(f"  No existe el caché local. Corre primero la extracción de {label} "
                 f"(o revisa por qué {cache_path} nunca se creó).")
            continue
        try:
            df = pd.read_csv(cache_path, low_memory=False)
        except Exception as exc:
            _log(f"  No se pudo leer el caché: {exc}")
            continue
        _log(f"  Filas en el caché: {len(df)}")
        if df.empty:
            _log("  El caché existe pero está vacío: el problema es la descarga/filtro, no el matching.")
            continue

        unmatched = _encontrar_no_matcheados(df, mun_panel, start_field, text_fields)
        if not unmatched.empty:
            out_path = DIAG_DIR / f"{label.lower()}_unmatched_projects.csv"
            unmatched.to_csv(out_path, index=False, encoding="utf-8-sig")
            _log(f"  Exportado a: {out_path} (revisar manualmente para completar el bridge)")


# =============================================================================
# main
# =============================================================================

def main() -> None:
    _log(f"Diagnóstico de fuentes de carbono — salidas en {DIAG_DIR}/\n")

    diagnosticar_ecoregistry()
    diagnosticar_goldstandard()
    diagnosticar_acr()
    diagnosticar_cad_trust()
    diagnosticar_renare()
    auditar_matching()

    report_path = DIAG_DIR / "reporte_diagnostico.txt"
    report_path.write_text("\n".join(report_lines), encoding="utf-8")
    _log(f"\n\nReporte completo guardado en: {report_path}")
    _log("Pégamelo (o al menos los status codes de cada fuente) para seguir con la corrección.")


if __name__ == "__main__":
    main()
