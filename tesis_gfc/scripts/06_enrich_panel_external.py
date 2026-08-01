from __future__ import annotations

import argparse
import io
import json
import re
import socket
import time
import zipfile
import unicodedata
from pathlib import Path
from typing import Any

import geopandas as gpd
import pandas as pd
import requests

try:
    import ee
except ImportError:  # pragma: no cover - optional dependency
    ee = None


INPUT_PANEL = Path("data/final/panel_municipio_year.csv")
OUTPUT_PANEL_ENRICHED = Path("data/final/panel_municipio_year_enriched.csv")
VERA_CACHE = Path("data/interim/verra_projects_colombia.csv")
VERA_LOCAL_FALLBACK = Path("data/raw/auxiliary/verra_projects_colombia_manual.csv")
VERA_MUNICIPIO_BRIDGE = Path("data/raw/auxiliary/verra_project_municipios.csv")
CHECKPOINT_FILE = Path("data/interim/panel_external_checkpoint.csv")
EXTERNAL_COLS = [
    "COD_DANE",
    "year",
    "poblacion_dane",
    "temp_media_c",
    "prec_anual_mm",
    "proyectos_carbono_co",
    "proyectos_berkeley_co",
    "proyectos_offsetsdb_co",
    "proyectos_cdm_co",
]
MUNICIPIOS_CANDIDATES = [
    Path("data/interim/municipios_clean.gpkg"),
    Path("data/interim/municipios_clean.geojson"),
]
MUNICIPIOS_LAYER = "municipios_clean"
DANE_URL = "https://www.datos.gov.co/resource/7p57-v9z7.json"
VERA_URL = "https://registry.verra.org/api/v1/projectSearch"
VERA_SEARCH_URL = "https://registry.verra.org/app/search/VCS"
VERA_PROJECT_DETAIL_URL = "https://registry.verra.org/app/projectDetail/VCS/{project_id}"
VERA_CACHE = Path("data/interim/verra_projects_colombia.csv")

# Berkeley VROD (Voluntary Registry Offset Database)
# https://vrod.berkeley.edu - Excel file with global voluntary offset projects
BERKELEY_XLSX_URL = "https://vrod.berkeley.edu/Download_VROD_Projects.xlsx"
BERKELEY_CACHE = Path("data/interim/berkeley_vrod_projects_colombia.csv")

# CDM (Clean Development Mechanism) - UN climate registry
# https://cdm.unfccc.int - Search interface for CDM projects
CDM_SEARCH_URL = "https://cdm.unfccc.int/Projects/search/search.html"
CDM_CACHE = Path("data/raw/auxiliary/cdm_projects_colombia.csv")
BERKELEY_COUNTS = Path("data/interim/berkeley_projects_municipio_year_counts.csv")

# CarbonPlan OffsetsDB - harmonized project and credit database
OFFSETSDB_URL = "https://carbonplan-offsets-db.s3.us-west-2.amazonaws.com/production/latest/offsets-db.csv.zip"
OFFSETSDB_PROJECTS_CACHE = Path("data/interim/offsetsdb_projects_colombia.csv")
OFFSETSDB_CREDITS_CACHE = Path("data/interim/offsetsdb_credits_colombia.csv")
OFFSETSDB_COUNTS = Path("data/interim/offsetsdb_projects_municipio_year_counts.csv")
OFFSETSDB_MUNICIPIO_BRIDGE = Path("data/raw/auxiliary/offsetsdb_project_municipios.csv")


def _empty_external_df() -> pd.DataFrame:
    return pd.DataFrame(columns=EXTERNAL_COLS)


def _load_checkpoint(checkpoint_path: Path) -> pd.DataFrame:
    if not checkpoint_path.exists():
        return _empty_external_df()

    try:
        df = pd.read_csv(checkpoint_path)
    except Exception:
        print(f"Warning: could not read checkpoint file at {checkpoint_path}. Starting from scratch.")
        return _empty_external_df()

    missing_required = {"COD_DANE", "year"} - set(df.columns)
    if missing_required:
        print(f"Warning: checkpoint missing required columns {sorted(missing_required)}. Starting from scratch.")
        return _empty_external_df()

    for col in EXTERNAL_COLS:
        if col not in df.columns:
            df[col] = pd.NA

    df = df[EXTERNAL_COLS].copy()
    df["COD_DANE"] = df["COD_DANE"].astype(str)
    df["year"] = pd.to_numeric(df["year"], errors="coerce").fillna(-1).astype(int)
    df = df[df["year"] >= 0].copy()
    df = df.drop_duplicates(subset=["COD_DANE", "year"], keep="last").reset_index(drop=True)
    return df


def _save_checkpoint(df: pd.DataFrame, checkpoint_path: Path) -> None:
    checkpoint_path.parent.mkdir(parents=True, exist_ok=True)
    out = df[EXTERNAL_COLS].drop_duplicates(subset=["COD_DANE", "year"], keep="last").copy()
    out = out.sort_values(["COD_DANE", "year"]).reset_index(drop=True)
    out.to_csv(checkpoint_path, index=False, encoding="utf-8-sig")


def _pick_municipios_file() -> Path:
    for path in MUNICIPIOS_CANDIDATES:
        if path.exists():
            return path
    raise FileNotFoundError(f"Missing municipalities file. Tried: {MUNICIPIOS_CANDIDATES}")


def _init_earth_engine(enabled: bool, project: str | None = None) -> bool:
    if not enabled:
        return False
    if ee is None:
        print("Warning: google-earth-engine package is not installed. Climate covariates will be skipped.")
        return False

    try:
        if project:
            ee.Initialize(project=project)
        else:
            ee.Initialize()
        print("Google Earth Engine initialized successfully.")
        return True
    except Exception as exc:  # pragma: no cover - external auth/network
        print("Warning: could not initialize Earth Engine. Run 'earthengine authenticate' and try again.")
        print(f"Details: {exc}")
        return False


def _to_year(value: Any) -> int | None:
    if value is None or (isinstance(value, float) and pd.isna(value)):
        return None
    text = str(value).strip()
    if not text:
        return None

    for chunk in (text, text[:10]):
        if len(chunk) >= 4 and chunk[:4].isdigit():
            return int(chunk[:4])

    # Support date formats like dd/mm/yyyy or yyyy-mm-dd by locating any 4-digit year.
    match = re.search(r"(19|20)\d{2}", text)
    if match:
        return int(match.group(0))
    return None


def _parse_population_from_row(row: dict[str, Any]) -> float | None:
    for field in ["poblacion_total", "poblacion", "total_poblacion", "total", "poblacion_nacional"]:
        if field in row and row[field] not in (None, ""):
            try:
                return float(row[field])
            except Exception:
                continue

    hombres = row.get("hombres")
    mujeres = row.get("mujeres")
    if hombres not in (None, "") and mujeres not in (None, ""):
        try:
            return float(hombres) + float(mujeres)
        except Exception:
            return None
    return None


def _normalize_text(value: str | None) -> str:
    if not value:
        return ""
    txt = unicodedata.normalize("NFKD", str(value))
    txt = "".join(ch for ch in txt if not unicodedata.combining(ch))
    return txt.upper().strip()


def _extract_year_from_row(row: dict[str, Any]) -> int | None:
    for key in ["a_o", "anio", "ano", "a\u00f1o"]:
        if key in row and row[key] not in (None, ""):
            try:
                return int(str(row[key])[:4])
            except Exception:
                continue
    return None


def obtener_poblacion_dane(
    cod_dane: str,
    anio: int,
    session: requests.Session,
    dane_url: str,
    municipio: str | None = None,
    departamento: str | None = None,
    dane_rows: list[dict[str, Any]] | None = None,
) -> float | None:
    """Fetch municipality population projection from datos.gov.co.

    The endpoint schema can change over time, so this function tries multiple
    common parameter/field names and fails gracefully.
    """
    if dane_rows:
        year_rows = [r for r in dane_rows if _extract_year_from_row(r) == int(anio)]

        # 1) Prefer exact code match when dataset has municipality code.
        coded = [
            r
            for r in year_rows
            if str(r.get("dpmp", "")).zfill(5) == str(cod_dane)
            or str(r.get("cod_muni", "")).zfill(5) == str(cod_dane)
            or str(r.get("cod_dane", "")).zfill(5) == str(cod_dane)
        ]
        if coded:
            total_first = [r for r in coded if str(r.get("rea_geogr_fica", "")).strip() == "Total"]
            pop = _parse_population_from_row(total_first[0] if total_first else coded[0])
            if pop is not None:
                return pop

        # 2) Try municipality name match.
        if municipio:
            nm = _normalize_text(municipio)
            name_rows = [
                r
                for r in year_rows
                if _normalize_text(r.get("municipio")) == nm or _normalize_text(r.get("mpio")) == nm
            ]
            if name_rows:
                total_first = [r for r in name_rows if str(r.get("rea_geogr_fica", "")).strip() == "Total"]
                pop = _parse_population_from_row(total_first[0] if total_first else name_rows[0])
                if pop is not None:
                    return pop

        # 3) Fallback: if dataset is national-by-year, return year aggregate.
        for r in year_rows:
            pop = _parse_population_from_row(r)
            if pop is not None and "poblacion_nacional" in r:
                return pop

    params_candidates = [
        {"a_o": anio, "$limit": 1},
        {"anio": anio, "$limit": 1},
        {"ano": anio, "$limit": 1},
        {"cod_muni": cod_dane, "a\u00f1o": anio, "$limit": 1},
        {"cod_muni": cod_dane, "anio": anio, "$limit": 1},
        {"cod_dane": cod_dane, "a\u00f1o": anio, "$limit": 1},
        {"cod_dane": cod_dane, "anio": anio, "$limit": 1},
        {"dpmp": cod_dane, "a_o": anio, "rea_geogr_fica": "Total", "$limit": 1},
        {"dpmp": cod_dane, "a_o": anio, "$limit": 1},
        {"mpio": cod_dane, "a_o": anio, "rea_geogr_fica": "Total", "$limit": 1},
    ]

    for params in params_candidates:
        try:
            response = session.get(dane_url, params=params, timeout=10)
            response.raise_for_status()
            data = response.json()
            if not data:
                continue

            pop = _parse_population_from_row(data[0])
            if pop is not None:
                return pop
        except Exception:
            continue

    if municipio:
        params_candidates_by_name: list[dict[str, Any]] = [
            {"municipio": municipio, "departamento": departamento, "a_o": anio, "$limit": 1},
            {"municipio": municipio, "departamento": departamento, "anio": anio, "$limit": 1},
            {"municipio": municipio, "departamento": departamento, "ano": anio, "$limit": 1},
            {"municipio": municipio, "a_o": anio, "$limit": 1},
            {"municipio": municipio, "anio": anio, "$limit": 1},
            {"dpnom": departamento, "a_o": anio, "rea_geogr_fica": "Total", "$limit": 1},
        ]

        for params in params_candidates_by_name:
            params = {k: v for k, v in params.items() if v not in (None, "")}
            try:
                response = session.get(dane_url, params=params, timeout=10)
                response.raise_for_status()
                data = response.json()
                if not data:
                    continue

                pop = _parse_population_from_row(data[0])
                if pop is not None:
                    return pop
            except Exception:
                continue

    return None


def obtener_clima_gee(geom_ee: Any, anio: int) -> dict[str, float | None]:
    """Compute annual precipitation (CHIRPS) and mean temperature (ERA5)."""
    fecha_inicio = f"{anio}-01-01"
    fecha_fin = f"{anio}-12-31"

    try:
        chirps = ee.ImageCollection("UCSB-CHG/CHIRPS/DAILY").filterDate(fecha_inicio, fecha_fin).sum()
        precip_stats = chirps.reduceRegion(
            reducer=ee.Reducer.mean(),
            geometry=geom_ee,
            scale=5566,
            maxPixels=1e9,
        ).getInfo()

        temp_k = None

        # Try ERA5 daily first, then ERA5-Land aggregated products as fallback.
        era5_candidates = [
            ("ECMWF/ERA5/DAILY", "mean_2m_air_temperature", 27830),
            ("ECMWF/ERA5_LAND/DAILY_AGGR", "temperature_2m", 11132),
            ("ECMWF/ERA5_LAND/DAILY_AGGR", "temperature_2m_mean", 11132),
        ]

        for collection_id, band_name, scale in era5_candidates:
            try:
                era5 = ee.ImageCollection(collection_id).filterDate(fecha_inicio, fecha_fin).select(band_name).mean()
                temp_stats = era5.reduceRegion(
                    reducer=ee.Reducer.mean(),
                    geometry=geom_ee,
                    scale=scale,
                    maxPixels=1e9,
                ).getInfo()
                if temp_stats and temp_stats.get(band_name) is not None:
                    temp_k = float(temp_stats.get(band_name))
                    break
            except Exception:
                continue

        return {
            "prec_anual_mm": precip_stats.get("precipitation") if precip_stats else None,
            "temp_media_c": (temp_k - 273.15) if temp_k is not None else None,
        }
    except Exception:
        return {"prec_anual_mm": None, "temp_media_c": None}


def _verra_headers() -> dict[str, str]:
    return {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
        "Accept": "application/json, text/plain, */*",
        "Content-Type": "application/json;charset=UTF-8",
        "Origin": "https://registry.verra.org",
        "Referer": VERA_SEARCH_URL,
    }


def _fetch_verra_from_api(session: requests.Session, country: str = "CO") -> pd.DataFrame:
    payload_candidates = [
        {"maxResults": 4000, "startIndex": 0, "country": country},
        {"maxResults": 4000, "startIndex": 0, "program": "VCS", "country": country},
    ]

    # Warm-up request so the registry can set any required cookies for API calls.
    try:
        session.get(VERA_SEARCH_URL, timeout=20)
    except Exception:
        pass

    errors: list[str] = []
    for payload in payload_candidates:
        try:
            response = session.post(
                VERA_URL,
                data=json.dumps(payload),
                headers=_verra_headers(),
                timeout=30,
            )
            response.raise_for_status()

            body = response.json()
            if isinstance(body, dict):
                items = body.get("items", [])
            elif isinstance(body, list):
                items = body
            else:
                items = []

            df = pd.DataFrame(items)
            if not df.empty:
                return df
        except Exception as exc:
            errors.append(str(exc))

    if errors:
        raise RuntimeError("; ".join(errors))
    return pd.DataFrame()


def _country_hints(country: str) -> list[str]:
    country_upper = (country or "").strip().upper()
    if country_upper == "CO":
        return ["colombia", "colombian", "colombiana", "colombiano"]
    return [country_upper.lower()] if country_upper else []


def obtener_proyectos_berkeley(
    cache_file: Path = BERKELEY_CACHE,
    country: str = "Colombia",
) -> pd.DataFrame:
    """Download Berkeley VROD projects for Colombia from Excel file.
    
    Berkeley Voluntary Registry Offset Database (VROD):
    https://vrod.berkeley.edu - Global database of voluntary offset projects
    """
    if cache_file.exists():
        try:
            df = pd.read_csv(cache_file)
            print(f"Using Berkeley VROD cache: {len(df)} projects")
            return df
        except Exception as exc:
            print(f"Warning: could not read Berkeley cache ({exc}).")
    
    try:
        print(f"Downloading Berkeley VROD from {BERKELEY_XLSX_URL}...")
        df = pd.read_excel(BERKELEY_XLSX_URL, sheet_name="PROJECTS", engine="openpyxl")
        df_co = df[df["Country/Area"] == country].copy()
        df_co["fuente"] = "berkeley_vrod"
        cache_file.parent.mkdir(parents=True, exist_ok=True)
        df_co.to_csv(cache_file, index=False)
        print(f"Berkeley VROD: {len(df_co)} proyectos Colombia → {cache_file}")
        return df_co
    except Exception as exc:
        print(f"Warning: could not download Berkeley VROD projects ({exc}).")
    
    return pd.DataFrame()


def obtener_proyectos_cdm(cache_file: Path = CDM_CACHE) -> pd.DataFrame:
    """Download CDM projects for Colombia via web interface.
    
    Clean Development Mechanism (CDM):
    https://cdm.unfccc.int - UN climate registry with ~7,800 registered projects
    """
    if cache_file.exists():
        try:
            df = pd.read_csv(cache_file)
            print(f"Using CDM cache: {len(df)} projects")
            return df
        except Exception as exc:
            print(f"Warning: could not read CDM cache ({exc}).")
    
    try:
        print(f"Accessing CDM projects from {CDM_SEARCH_URL}...")
        # CDM search interface - fetch projects with country=CO filter
        params = {"country": "CO", "limit": 500}
        resp = requests.get(CDM_SEARCH_URL, params=params, timeout=30)
        resp.raise_for_status()
        
        # Note: CDM interface returns HTML, not JSON. Manual cache or HTML parsing required.
        print("Note: CDM web scraping requires HTML parsing. Using cache if available.")
        
    except Exception as exc:
        print(f"Warning: could not access CDM projects ({exc}).")
    
    return pd.DataFrame()


def _load_csv_from_zip(zf: zipfile.ZipFile, name_contains: str) -> pd.DataFrame:
    matches = [name for name in zf.namelist() if name_contains in name.lower() and name.endswith(".csv")]
    if not matches:
        raise FileNotFoundError(f"No CSV matching '{name_contains}' found in archive: {zf.namelist()}")
    with zf.open(matches[0]) as handle:
        return pd.read_csv(handle, low_memory=False)


def _find_column(df: pd.DataFrame, candidates: list[str]) -> str | None:
    normalized = {str(col).strip().lower().replace(" ", "").replace("/", "_"): col for col in df.columns}
    for candidate in candidates:
        key = candidate.strip().lower().replace(" ", "").replace("/", "_")
        if key in normalized:
            return normalized[key]
    return None


def obtener_proyectos_offsetsdb(
    projects_cache: Path = OFFSETSDB_PROJECTS_CACHE,
    credits_cache: Path = OFFSETSDB_CREDITS_CACHE,
    bridge_file: Path = OFFSETSDB_MUNICIPIO_BRIDGE,
    country: str = "Colombia",
) -> pd.DataFrame:
    """Download CarbonPlan OffsetsDB projects and credits for Colombia."""
    if projects_cache.exists():
        try:
            df = pd.read_csv(projects_cache)
            print(f"Using OffsetsDB project cache: {len(df)} projects")
            if bridge_file.exists():
                df = _apply_offsetsdb_municipio_bridge(df, bridge_file)
                df.to_csv(projects_cache, index=False)
            return df
        except Exception as exc:
            print(f"Warning: could not read OffsetsDB project cache ({exc}).")

    try:
        print(f"Downloading OffsetsDB from {OFFSETSDB_URL}...")
        resp = requests.get(OFFSETSDB_URL, timeout=120)
        resp.raise_for_status()
        zf = zipfile.ZipFile(io.BytesIO(resp.content))

        projects = _load_csv_from_zip(zf, "project")
        credits = _load_csv_from_zip(zf, "credit")

        country_col = _find_column(projects, ["country", "country_area", "country/area", "countryarea"])
        if country_col:
            country_values = projects[country_col].astype(str)
            mask = country_values.str.contains(country, case=False, na=False)
            if country.upper() == "CO":
                mask = mask | country_values.str.contains("colombia", case=False, na=False)
            projects_co = projects[mask].copy()
        else:
            projects_co = projects.copy()

        project_id_col = _find_column(projects_co, ["project_id", "projectid", "id"])
        credit_project_id_col = _find_column(credits, ["project_id", "projectid", "id"])
        vintage_col = _find_column(credits, ["vintage", "vintage_year", "year"])

        if project_id_col and credit_project_id_col and not credits.empty:
            credits_co = credits[credits[credit_project_id_col].isin(projects_co[project_id_col])].copy()
        else:
            credits_co = credits.copy()

        if project_id_col and credit_project_id_col and vintage_col and not credits_co.empty:
            credits_co[vintage_col] = pd.to_numeric(credits_co[vintage_col], errors="coerce")
            credit_summary = (
                credits_co.dropna(subset=[vintage_col])
                .groupby(credit_project_id_col, as_index=False)
                .agg(
                    vintage_start=(vintage_col, "min"),
                    vintage_end=(vintage_col, "max"),
                    credit_rows=(credit_project_id_col, "size"),
                )
            )
            projects_co = projects_co.merge(
                credit_summary,
                left_on=project_id_col,
                right_on=credit_project_id_col,
                how="left",
            )
        else:
            projects_co["vintage_start"] = pd.NA
            projects_co["vintage_end"] = pd.NA
            projects_co["credit_rows"] = pd.NA

        start_col = _find_column(projects_co, ["startdate", "start_date", "projectstartdate", "project_start_date"])
        end_col = _find_column(projects_co, ["enddate", "end_date", "projectenddate", "project_end_date"])
        if start_col is None:
            projects_co["startDate"] = projects_co["vintage_start"].fillna(projects_co.get("vintage_end"))
        else:
            projects_co["startDate"] = projects_co[start_col]
        if end_col is None:
            projects_co["endDate"] = projects_co["vintage_end"].fillna(projects_co.get("vintage_start"))
        else:
            projects_co["endDate"] = projects_co[end_col]

            projects_co = _apply_offsetsdb_municipio_bridge(projects_co, bridge_file)
        projects_co["fuente"] = "carbonplan_offsetsdb"

        projects_cache.parent.mkdir(parents=True, exist_ok=True)
        credits_cache.parent.mkdir(parents=True, exist_ok=True)
        projects_co.to_csv(projects_cache, index=False)
        credits_co.to_csv(credits_cache, index=False)
        print(f"OffsetsDB: {len(projects_co)} Colombia projects → {projects_cache}")
        print(f"OffsetsDB: {len(credits_co)} Colombia credits → {credits_cache}")
        return projects_co
    except Exception as exc:
        print(f"Warning: could not download OffsetsDB data ({exc}).")

    return pd.DataFrame()


def _extract_label_value(text: str, label: str) -> str | None:
    pattern = rf"{re.escape(label)}\s*[:|]?\s*([A-Za-z0-9\-/,.'() ]{{2,80}})"
    m = re.search(pattern, text, flags=re.IGNORECASE)
    if not m:
        return None
    return m.group(1).strip()


def _extract_crediting_period_dates(text: str) -> tuple[str | None, str | None]:
    m = re.search(
        r"Crediting Period Term\s*[:|]?\s*(?:\d+(?:st|nd|rd|th),\s*)?"
        r"([0-3]?\d/[01]?\d/\d{4})\s*-\s*([0-3]?\d/[01]?\d/\d{4})",
        text,
        flags=re.IGNORECASE,
    )
    if not m:
        return None, None
    return m.group(1), m.group(2)


def _parse_verra_public_detail_page(html: str, project_id: int, country: str) -> dict[str, Any] | None:
    text = re.sub(r"<[^>]+>", " ", html)
    text = re.sub(r"\s+", " ", text).strip()
    text_lower = text.lower()

    if "project summary" not in text_lower or "vcs project status" not in text_lower:
        return None

    hints = _country_hints(country)
    if hints and not any(h in text_lower for h in hints):
        return None

    registration_date = _extract_label_value(text, "Project Registration Date")
    status = _extract_label_value(text, "VCS Project Status")
    start_date, end_date = _extract_crediting_period_dates(text)
    project_name = _extract_label_value(text, "BLUE CARBON PROJECT")

    if not registration_date and not start_date:
        return None

    return {
        "id": project_id,
        "name": project_name,
        "country": country,
        "status": status,
        "registrationDate": registration_date,
        "creditingPeriodStartDate": start_date,
        "creditingPeriodEndDate": end_date,
        "source": "public_project_detail",
        "projectDetailUrl": VERA_PROJECT_DETAIL_URL.format(project_id=project_id),
    }


def _fetch_verra_from_public_details(
    session: requests.Session,
    country: str = "CO",
    id_min: int = 1,
    id_max: int = 2800,
    timeout_sec: int = 15,
) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []
    consecutive_misses = 0

    for project_id in range(max(1, int(id_min)), max(1, int(id_max)) + 1):
        url = VERA_PROJECT_DETAIL_URL.format(project_id=project_id)
        try:
            response = session.get(url, timeout=timeout_sec)
            response.raise_for_status()
            parsed = _parse_verra_public_detail_page(response.text, project_id, country)
            if parsed is None:
                consecutive_misses += 1
            else:
                rows.append(parsed)
                consecutive_misses = 0
        except Exception:
            consecutive_misses += 1

        if project_id % 250 == 0:
            print(f"Verra public scan progress: checked up to VCS/{project_id}, matches={len(rows)}")

        # Exit early if long stretch has no matches after we've already scanned deep enough.
        if project_id > 1200 and consecutive_misses >= 700:
            break

    return pd.DataFrame(rows)


def _verra_host_resolves(hostname: str = "registry.verra.org") -> bool:
    try:
        socket.getaddrinfo(hostname, 443)
        return True
    except Exception:
        return False


def _apply_verra_municipio_bridge(df_verra: pd.DataFrame, bridge_file: Path) -> pd.DataFrame:
    if df_verra.empty:
        return df_verra
    if not bridge_file.exists():
        return df_verra

    try:
        bridge = pd.read_csv(bridge_file, dtype=str)
    except Exception as exc:
        print(f"Warning: could not read Verra municipio bridge file {bridge_file} ({exc}).")
        return df_verra

    required = {"project_id", "cod_dane"}
    if not required.issubset(set(bridge.columns)):
        print(
            "Warning: Verra municipio bridge missing required columns "
            f"{sorted(required)} at {bridge_file}."
        )
        return df_verra

    bridge = bridge.copy()
    bridge["project_id"] = bridge["project_id"].astype(str).str.strip()
    bridge["cod_dane"] = bridge["cod_dane"].astype(str).str.extract(r"(\d{4,6})", expand=False).fillna("")
    bridge["cod_dane"] = bridge["cod_dane"].str.zfill(5)
    bridge = bridge[bridge["project_id"] != ""]
    bridge = bridge[bridge["cod_dane"] != ""]
    if bridge.empty:
        return df_verra

    if "municipio" not in bridge.columns:
        bridge["municipio"] = ""

    agg = (
        bridge.groupby("project_id", as_index=False)
        .agg(
            bridge_cod_dane=("cod_dane", lambda s: ";".join(sorted(set(v for v in s if v)))),
            bridge_municipalities=("municipio", lambda s: ";".join(sorted(set(v for v in s if str(v).strip())))),
        )
    )

    out = df_verra.copy()
    out["project_id_str"] = out.get("id", "").astype(str).str.strip()
    out = out.merge(agg, how="left", left_on="project_id_str", right_on="project_id")

    if "cod_dane" not in out.columns:
        out["cod_dane"] = ""
    if "municipalities" not in out.columns:
        out["municipalities"] = ""

    out["cod_dane"] = out["cod_dane"].fillna("").astype(str)
    out["municipalities"] = out["municipalities"].fillna("").astype(str)
    out["bridge_cod_dane"] = out["bridge_cod_dane"].fillna("").astype(str)
    out["bridge_municipalities"] = out["bridge_municipalities"].fillna("").astype(str)

    out["cod_dane"] = out.apply(
        lambda r: ";".join(sorted(set(filter(None, re.split(r"[;,|/]", f"{r['cod_dane']};{r['bridge_cod_dane']}"))))),
        axis=1,
    )
    out["municipalities"] = out.apply(
        lambda r: ";".join(
            sorted(set(filter(None, [p.strip() for p in re.split(r"[;,|/]", f"{r['municipalities']};{r['bridge_municipalities']}")])) )
        ),
        axis=1,
    )

    matched = int((out["bridge_cod_dane"] != "").sum())
    if matched > 0:
        print(f"Applied Verra municipio bridge for {matched} project rows from: {bridge_file}")

    drop_cols = [c for c in ["project_id", "project_id_str", "bridge_cod_dane", "bridge_municipalities"] if c in out.columns]
    return out.drop(columns=drop_cols)


def _apply_offsetsdb_municipio_bridge(df_offsetsdb: pd.DataFrame, bridge_file: Path) -> pd.DataFrame:
    if df_offsetsdb.empty:
        return df_offsetsdb
    if not bridge_file.exists():
        return df_offsetsdb

    try:
        bridge = pd.read_csv(bridge_file, dtype=str)
    except Exception as exc:
        print(f"Warning: could not read OffsetsDB municipio bridge file {bridge_file} ({exc}).")
        return df_offsetsdb

    required = {"project_id", "cod_dane"}
    if not required.issubset(set(bridge.columns)):
        print(
            "Warning: OffsetsDB municipio bridge missing required columns "
            f"{sorted(required)} at {bridge_file}."
        )
        return df_offsetsdb

    bridge = bridge.copy()
    bridge["project_id"] = bridge["project_id"].astype(str).str.strip()
    bridge["cod_dane"] = bridge["cod_dane"].astype(str).str.extract(r"(\d{4,6})", expand=False).fillna("")
    bridge["cod_dane"] = bridge["cod_dane"].str.zfill(5)
    bridge = bridge[bridge["project_id"] != ""]
    bridge = bridge[bridge["cod_dane"] != ""]
    if bridge.empty:
        return df_offsetsdb

    if "municipio" not in bridge.columns:
        bridge["municipio"] = ""

    agg = (
        bridge.groupby("project_id", as_index=False)
        .agg(
            bridge_cod_dane=("cod_dane", lambda s: ";".join(sorted(set(v for v in s if v)))),
            bridge_municipalities=("municipio", lambda s: ";".join(sorted(set(v for v in s if str(v).strip())))),
        )
    )

    out = df_offsetsdb.copy()
    project_id_col = _find_column(out, ["project_id", "projectid", "id"])
    if project_id_col is None:
        return out

    out["project_id_str"] = out[project_id_col].astype(str).str.strip()
    out = out.merge(agg, how="left", left_on="project_id_str", right_on="project_id")

    if "cod_dane" not in out.columns:
        out["cod_dane"] = ""
    if "municipalities" not in out.columns:
        out["municipalities"] = ""

    out["cod_dane"] = out["cod_dane"].fillna("").astype(str)
    out["municipalities"] = out["municipalities"].fillna("").astype(str)
    out["bridge_cod_dane"] = out["bridge_cod_dane"].fillna("").astype(str)
    out["bridge_municipalities"] = out["bridge_municipalities"].fillna("").astype(str)

    out["cod_dane"] = out.apply(
        lambda r: ";".join(sorted(set(filter(None, re.split(r"[;,|/]", f"{r['cod_dane']};{r['bridge_cod_dane']}"))))),
        axis=1,
    )
    out["municipalities"] = out.apply(
        lambda r: ";".join(
            sorted(set(filter(None, [p.strip() for p in re.split(r"[;,|/]", f"{r['municipalities']};{r['bridge_municipalities']}" )])) )
        ),
        axis=1,
    )

    matched = int((out["bridge_cod_dane"] != "").sum())
    if matched > 0:
        print(f"Applied OffsetsDB municipio bridge for {matched} project rows from: {bridge_file}")

    drop_cols = [c for c in ["project_id", "project_id_str", "bridge_cod_dane", "bridge_municipalities"] if c in out.columns]
    return out.drop(columns=drop_cols)


def obtener_proyectos_verra(
    session: requests.Session | None = None,
    source: str = "auto",
    country: str = "CO",
    cache_file: Path = VERA_CACHE,
    local_file: Path = VERA_LOCAL_FALLBACK,
    public_id_min: int = 1,
    public_id_max: int = 2800,
    allow_empty: bool = False,
) -> pd.DataFrame:
    """Download Verra projects for a country, with cache fallback.

    source values:
    - auto: try registry web/api flow, then local files, then public projectDetail scan
    - api: only registry web/api flow
    - public: only public projectDetail scan
    - cache: only local files (checkpoint cache or manual csv)
    """
    src = (source or "auto").strip().lower()
    sess = session or requests.Session()

    def _load_local_verra_file(path: Path) -> pd.DataFrame:
        if not path.exists():
            return pd.DataFrame()
        try:
            df_local = pd.read_csv(path)
            if not df_local.empty:
                print(f"Using Verra local file: {path}")
            return df_local
        except Exception as exc:
            print(f"Warning: could not read Verra local file {path} ({exc}).")
            return pd.DataFrame()

    if src in {"cache"}:
        for path in [cache_file, local_file]:
            df_local = _load_local_verra_file(path)
            if not df_local.empty:
                return df_local
        if allow_empty:
            return pd.DataFrame()
        raise RuntimeError(
            "Verra source=cache but no usable local file was found. "
            f"Expected one of: {cache_file} or {local_file}"
        )

    host_available = _verra_host_resolves("registry.verra.org")
    if not host_available:
        print(
            "Warning: DNS for registry.verra.org is unavailable. "
            "Skipping online Verra extraction and trying local files."
        )

    if src in {"api", "public"} and not host_available:
        if allow_empty:
            return pd.DataFrame()
        raise RuntimeError(
            "Verra online source requested but registry.verra.org DNS is unavailable. "
            f"Use local CSV fallback: {local_file}"
        )

    if host_available and src in {"auto", "api"}:
        try:
            df = _fetch_verra_from_api(sess, country=country)
            if not df.empty:
                return df
        except Exception as exc:
            print(
                "Warning: could not download Verra projects from API "
                f"({exc}). This can happen when DNS/network to registry.verra.org is unavailable."
            )

    if src == "auto":
        for path in [cache_file, local_file]:
            df_local = _load_local_verra_file(path)
            if not df_local.empty:
                return df_local

    if host_available and src in {"auto", "public"}:
        try:
            df = _fetch_verra_from_public_details(
                sess,
                country=country,
                id_min=public_id_min,
                id_max=public_id_max,
            )
            if not df.empty:
                print(f"Loaded Verra projects from public detail pages: {len(df)}")
                return df
        except Exception as exc:
            print(f"Warning: could not load Verra projects from public detail pages ({exc}).")

    if src in {"api", "public"}:
        if allow_empty:
            return pd.DataFrame()
        raise RuntimeError(
            "Verra online extraction returned no data and empty output is not allowed. "
            f"Provide a local fallback CSV at {local_file} or run with --permitir-verra-vacio."
        )

    for path in [cache_file, local_file]:
        df_local = _load_local_verra_file(path)
        if not df_local.empty:
            return df_local

    if allow_empty:
        return pd.DataFrame()

    raise RuntimeError(
        "No Verra projects could be loaded from online sources or local files. "
        f"Add a manual CSV at {local_file} (or {cache_file}) or run with --permitir-verra-vacio."
    )


def _extract_verra_target_codes(
    project_row: pd.Series,
    mun_by_name: dict[str, set[str]],
    mun_name_tokens: list[tuple[str, str]],
) -> set[str]:
    target_codes: set[str] = set()

    # 1) Prefer explicit municipality code fields when available.
    explicit_code_fields = ["cod_dane", "codigo_municipio", "municipalityCode", "municipio_cod_dane"]
    for field in explicit_code_fields:
        if field in project_row and pd.notna(project_row[field]):
            text = str(project_row[field])
            for code in re.findall(r"\d{4,6}", text):
                target_codes.add(code.zfill(5))

    # 2) Exact municipality-name fields (can be one name or list-like text).
    explicit_name_fields = ["municipio", "municipality", "municipalities", "municipio_nombre"]
    for field in explicit_name_fields:
        if field in project_row and pd.notna(project_row[field]):
            raw = str(project_row[field])
            parts = re.split(r"[;,|/]", raw)
            for part in parts:
                nm = _normalize_text(part)
                if nm in mun_by_name:
                    target_codes.update(mun_by_name[nm])

    if target_codes:
        return target_codes

    # 3) Fallback: search municipality tokens in location text fields.
    text_fields = [
        "name",
        "projectName",
        "description",
        "projectDescription",
        "stateProvince",
        "countryArea",
        "location",
    ]
    haystack = " ".join(str(project_row.get(f, "")) for f in text_fields)
    haystack_norm = _normalize_text(haystack)

    for token, code in mun_name_tokens:
        if token and token in haystack_norm:
            target_codes.add(code)

    return target_codes


def _compute_verra_active_counts_by_municipio(
    df_verra: pd.DataFrame,
    mun_panel: pd.DataFrame,
    years: list[int],
) -> dict[tuple[str, int], int]:
    if df_verra.empty:
        return {(str(cod), int(year)): 0 for cod in mun_panel["COD_DANE"].astype(str).unique() for year in years}

    mun_panel = mun_panel.copy()
    mun_panel["COD_DANE"] = mun_panel["COD_DANE"].astype(str)
    mun_panel["NOMBRE_MPI"] = mun_panel["NOMBRE_MPI"].astype(str)

    # Build lookup from normalized municipality name to COD_DANE(s).
    mun_by_name: dict[str, set[str]] = {}
    for _, mrow in mun_panel[["COD_DANE", "NOMBRE_MPI"]].drop_duplicates().iterrows():
        nm = _normalize_text(mrow["NOMBRE_MPI"])
        mun_by_name.setdefault(nm, set()).add(str(mrow["COD_DANE"]))

    # Prebuild tokens used in fallback text matching.
    mun_name_tokens: list[tuple[str, str]] = [
        (_normalize_text(name), str(code))
        for code, name in mun_panel[["COD_DANE", "NOMBRE_MPI"]].drop_duplicates().itertuples(index=False)
    ]

    counts = {(str(cod), int(year)): 0 for cod in mun_panel["COD_DANE"].unique() for year in years}

    start_candidates = [
        "creditingPeriodStartDate",
        "projectStartDate",
        "registrationDate",
        "pipelineListingDate",
    ]
    end_candidates = [
        "creditingPeriodEndDate",
        "projectEndDate",
        "withdrawnDate",
    ]

    for _, row in df_verra.iterrows():
        start_year = None
        end_year = None

        for col in start_candidates:
            if col in row:
                start_year = _to_year(row[col])
            if start_year is not None:
                break

        for col in end_candidates:
            if col in row:
                end_year = _to_year(row[col])
            if end_year is not None:
                break

        if start_year is None:
            continue

        target_codes = _extract_verra_target_codes(row, mun_by_name, mun_name_tokens)
        if not target_codes:
            continue

        for year in years:
            if start_year <= year and (end_year is None or end_year >= year):
                for code in target_codes:
                    key = (str(code), int(year))
                    if key in counts:
                        counts[key] += 1

    return counts


def _compute_projects_active_counts_by_municipio(
    df_projects: pd.DataFrame,
    mun_panel: pd.DataFrame,
    years: list[int],
    start_date_field: str = "startDate",
    end_date_field: str = "endDate",
    location_field: str = "location",
) -> dict[tuple[str, int], int]:
    """Generic function to compute active project counts by municipality-year.
    
    Tries to extract dates from specified fields and match projects to municipalities
    based on location text matching and municipality name tokens.
    """
    if df_projects.empty:
        return {(str(cod), int(year)): 0 for cod in mun_panel["COD_DANE"].astype(str).unique() for year in years}

    mun_panel = mun_panel.copy()
    mun_panel["COD_DANE"] = mun_panel["COD_DANE"].astype(str)
    mun_panel["NOMBRE_MPI"] = mun_panel["NOMBRE_MPI"].astype(str)

    # Build lookup from normalized municipality name to COD_DANE(s).
    mun_by_name: dict[str, set[str]] = {}
    for _, mrow in mun_panel[["COD_DANE", "NOMBRE_MPI"]].drop_duplicates().iterrows():
        nm = _normalize_text(mrow["NOMBRE_MPI"])
        mun_by_name.setdefault(nm, set()).add(str(mrow["COD_DANE"]))

    # Prebuild tokens used in fallback text matching.
    mun_name_tokens: list[tuple[str, str]] = [
        (_normalize_text(name), str(code))
        for code, name in mun_panel[["COD_DANE", "NOMBRE_MPI"]].drop_duplicates().itertuples(index=False)
    ]

    counts = {(str(cod), int(year)): 0 for cod in mun_panel["COD_DANE"].unique() for year in years}

    for _, row in df_projects.iterrows():
        start_year = None
        end_year = None

        # Try to extract start date from multiple possible field names
        start_fields = [start_date_field, "start_date", "startDate", "projectStart", "project_start"]
        for field in start_fields:
            if field in row and pd.notna(row[field]):
                start_year = _to_year(row[field])
                if start_year is not None:
                    break

        # Try to extract end date
        end_fields = [end_date_field, "end_date", "endDate", "projectEnd", "project_end", "completionDate"]
        for field in end_fields:
            if field in row and pd.notna(row[field]):
                end_year = _to_year(row[field])
                if end_year is not None:
                    break

        if start_year is None:
            continue

        # Try to match project to municipalities
        target_codes: set[str] = set()

        # 1) Look for explicit municipality code fields
        code_fields = ["cod_dane", "codigo_municipio", "municipio_code", "municipality_code"]
        for field in code_fields:
            if field in row and pd.notna(row[field]):
                text = str(row[field])
                for code in re.findall(r"\d{4,6}", text):
                    if code.zfill(5) in mun_panel["COD_DANE"].astype(str).unique():
                        target_codes.add(code.zfill(5))

        # 2) Look for municipality name fields
        name_fields = ["municipio", "municipality", "location", "region"]
        for field in name_fields:
            if field in row and pd.notna(row[field]):
                raw = str(row[field])
                parts = re.split(r"[;,|/]", raw)
                for part in parts:
                    nm = _normalize_text(part)
                    if nm in mun_by_name:
                        target_codes.update(mun_by_name[nm])

        # 3) Fallback: search municipality tokens in all text fields
        if not target_codes:
            haystack = " ".join(str(row.get(f, "")) for f in row.index if pd.notna(row[f]))
            haystack_norm = _normalize_text(haystack)
            for token, code in mun_name_tokens:
                if token and token in haystack_norm:
                    target_codes.add(code)

        # Add project counts for active years
        for year in years:
            if start_year <= year and (end_year is None or end_year >= year):
                for code in target_codes:
                    key = (str(code), int(year))
                    if key in counts:
                        counts[key] += 1

    return counts


def construir_panel_enriquecido(
    anio_inicio: int = 2001,
    anio_fin: int = 2024,
    max_municipios: int | None = None,
    include_population: bool = True,
    include_climate: bool = True,
    include_verra: bool = True,
    include_berkeley: bool = True,
    include_offsetsdb: bool = True,
    include_cdm: bool = True,
    pause_seconds: float = 0.3,
    resume: bool = True,
    reset_checkpoint: bool = False,
    save_every_municipios: int = 1,
    checkpoint_file: Path = CHECKPOINT_FILE,
    retry_nulls: bool = False,
    ee_project: str | None = None,
    dane_url: str = DANE_URL,
    start_municipio_pos: int = 1,
    verra_source: str = "auto",
    verra_country: str = "CO",
    verra_public_id_min: int = 1,
    verra_public_id_max: int = 2800,
    verra_local_file: Path = VERA_LOCAL_FALLBACK,
    verra_municipio_bridge_file: Path = VERA_MUNICIPIO_BRIDGE,
    offsetsdb_municipio_bridge_file: Path = OFFSETSDB_MUNICIPIO_BRIDGE,
    allow_empty_verra: bool = False,
) -> pd.DataFrame:
    if not INPUT_PANEL.exists():
        raise FileNotFoundError(f"Missing final panel file: {INPUT_PANEL}")

    municipios_file = _pick_municipios_file()
    panel = pd.read_csv(INPUT_PANEL)
    if panel.empty:
        raise ValueError("Final panel is empty. Run scripts 01-05 first.")

    required_cols = {"COD_DANE", "NOMBRE_MPI", "year"}
    missing = required_cols - set(panel.columns)
    if missing:
        raise ValueError(f"Final panel missing required columns: {sorted(missing)}")

    if municipios_file.suffix.lower() == ".gpkg":
        municipios = gpd.read_file(municipios_file, layer=MUNICIPIOS_LAYER)
    else:
        municipios = gpd.read_file(municipios_file)

    municipios = municipios[["COD_DANE", "geometry"]].copy()
    municipios["COD_DANE"] = municipios["COD_DANE"].astype(str)
    panel["COD_DANE"] = panel["COD_DANE"].astype(str)
    panel["year"] = panel["year"].astype(int)

    years = sorted(y for y in panel["year"].unique() if anio_inicio <= int(y) <= anio_fin)
    panel = panel[(panel["year"] >= anio_inicio) & (panel["year"] <= anio_fin)].copy()

    mun_cols = ["COD_DANE", "NOMBRE_MPI"]
    if "DPTO_CNMBR" in panel.columns:
        mun_cols.append("DPTO_CNMBR")

    mun_panel = panel[mun_cols].drop_duplicates().merge(
        municipios,
        on=["COD_DANE"],
        how="left",
    )
    if max_municipios is not None:
        mun_panel = mun_panel.head(max_municipios)

    if reset_checkpoint and checkpoint_file.exists():
        checkpoint_file.unlink()
        print(f"Deleted checkpoint: {checkpoint_file}")

    checkpoint_df = _load_checkpoint(checkpoint_file) if resume else _empty_external_df()
    if not checkpoint_df.empty:
        print(f"Loaded checkpoint rows: {len(checkpoint_df)} from {checkpoint_file}")

    selected_codes = set(mun_panel["COD_DANE"].astype(str).unique())
    panel_selected = panel[panel["COD_DANE"].isin(selected_codes)].copy()
    expected_keys = set(zip(panel_selected["COD_DANE"].astype(str), panel_selected["year"].astype(int)))
    if expected_keys:
        checkpoint_df = checkpoint_df[
            checkpoint_df.apply(lambda r: (str(r["COD_DANE"]), int(r["year"])) in expected_keys, axis=1)
        ].copy()

    done_keys = set(zip(checkpoint_df["COD_DANE"].astype(str), checkpoint_df["year"].astype(int)))

    if retry_nulls and not checkpoint_df.empty:
        retry_cols: list[str] = []
        if include_population:
            retry_cols.append("poblacion_dane")
        if include_climate:
            retry_cols.extend(["temp_media_c", "prec_anual_mm"])

        if retry_cols:
            retry_mask = checkpoint_df[retry_cols].isna().any(axis=1)
            retry_df = checkpoint_df[retry_mask].copy()
            retry_keys = set(zip(retry_df["COD_DANE"].astype(str), retry_df["year"].astype(int)))
            done_keys = done_keys - retry_keys
            if retry_keys:
                print(f"Retrying {len(retry_keys)} municipality-year rows with null external values.")

    session = requests.Session()

    dane_rows: list[dict[str, Any]] | None = None
    if include_population:
        try:
            r = session.get(dane_url, timeout=20)
            if r.status_code == 200:
                payload = r.json()
                if isinstance(payload, list):
                    dane_rows = payload
                    if dane_rows:
                        print(f"Loaded DANE rows from endpoint: {len(dane_rows)}")
        except Exception:
            dane_rows = None

    gee_enabled = _init_earth_engine(include_climate, ee_project)

    df_verra = (
        obtener_proyectos_verra(
            session=session,
            source=verra_source,
            country=verra_country,
            cache_file=VERA_CACHE,
            local_file=verra_local_file,
            public_id_min=verra_public_id_min,
            public_id_max=verra_public_id_max,
            allow_empty=allow_empty_verra,
        )
        if include_verra
        else pd.DataFrame()
    )
    if include_verra and not df_verra.empty:
        df_verra = _apply_verra_municipio_bridge(df_verra, verra_municipio_bridge_file)

    if not df_verra.empty:
        VERA_CACHE.parent.mkdir(parents=True, exist_ok=True)
        df_verra.to_csv(VERA_CACHE, index=False)
        print(f"Saved Verra cache to: {VERA_CACHE}")
    verra_by_mun_year = _compute_verra_active_counts_by_municipio(df_verra, mun_panel, years)

    # Load Berkeley VROD projects
    # Load Berkeley counts from precomputed bridge if available (faster, deterministic)
    berkeley_by_mun_year: dict[tuple[str, int], int] = {}
    if include_berkeley:
        if BERKELEY_COUNTS.exists():
            try:
                df_bcounts = pd.read_csv(BERKELEY_COUNTS, dtype={"COD_DANE": str, "year": int})
                berkeley_by_mun_year = {
                    (str(r["COD_DANE"]).zfill(5), int(r["year"])): int(r["proyectos_berkeley_co"]) if "proyectos_berkeley_co" in r else 1
                    for _, r in df_bcounts.iterrows()
                }
                print(f"Loaded Berkeley municipality-year counts from: {BERKELEY_COUNTS} ({len(berkeley_by_mun_year)} entries)")
            except Exception as exc:
                print(f"Warning: could not read Berkeley counts ({exc}). Falling back to raw Berkeley parsing.")
                df_berkeley = obtener_proyectos_berkeley(cache_file=BERKELEY_CACHE)
                berkeley_by_mun_year = _compute_projects_active_counts_by_municipio(df_berkeley, mun_panel, years)
        else:
            df_berkeley = obtener_proyectos_berkeley(cache_file=BERKELEY_CACHE)
            berkeley_by_mun_year = _compute_projects_active_counts_by_municipio(df_berkeley, mun_panel, years)
    else:
        berkeley_by_mun_year = {}

    # Load OffsetsDB projects
    offsetsdb_by_mun_year: dict[tuple[str, int], int] = {}
    if include_offsetsdb:
        if OFFSETSDB_COUNTS.exists() and not offsetsdb_municipio_bridge_file.exists():
            try:
                df_offsetsdb_counts = pd.read_csv(OFFSETSDB_COUNTS, dtype={"COD_DANE": str, "year": int})
                offsetsdb_by_mun_year = {
                    (str(r["COD_DANE"]).zfill(5), int(r["year"])): int(r["proyectos_offsetsdb_co"]) if "proyectos_offsetsdb_co" in r else 1
                    for _, r in df_offsetsdb_counts.iterrows()
                }
                print(f"Loaded OffsetsDB municipality-year counts from: {OFFSETSDB_COUNTS} ({len(offsetsdb_by_mun_year)} entries)")
            except Exception as exc:
                print(f"Warning: could not read OffsetsDB counts ({exc}). Falling back to raw OffsetsDB parsing.")
                df_offsetsdb = obtener_proyectos_offsetsdb(bridge_file=offsetsdb_municipio_bridge_file)
                offsetsdb_by_mun_year = _compute_projects_active_counts_by_municipio(df_offsetsdb, mun_panel, years)
        else:
            if OFFSETSDB_COUNTS.exists() and offsetsdb_municipio_bridge_file.exists():
                print(
                    f"OffsetsDB bridge found at {offsetsdb_municipio_bridge_file}; recomputing municipality-year counts from raw data instead of using {OFFSETSDB_COUNTS}."
                )
            df_offsetsdb = obtener_proyectos_offsetsdb(bridge_file=offsetsdb_municipio_bridge_file)
            offsetsdb_by_mun_year = _compute_projects_active_counts_by_municipio(df_offsetsdb, mun_panel, years)
    else:
        offsetsdb_by_mun_year = {}

    # Load CDM projects
    df_cdm = obtener_proyectos_cdm(cache_file=CDM_CACHE) if include_cdm else pd.DataFrame()
    cdm_by_mun_year = _compute_projects_active_counts_by_municipio(df_cdm, mun_panel, years) if include_cdm else {}

    population_cache: dict[tuple[str, int], float | None] = {}
    climate_cache: dict[tuple[str, int], dict[str, float | None]] = {}

    rows_buffer: list[dict[str, Any]] = []
    municipios_since_save = 0
    for idx, row in mun_panel.iterrows():
        if idx + 1 < max(1, int(start_municipio_pos)):
            continue

        cod_dane = str(row["COD_DANE"])
        nombre = str(row["NOMBRE_MPI"])
        dpto = str(row["DPTO_CNMBR"]) if "DPTO_CNMBR" in row and pd.notna(row["DPTO_CNMBR"]) else None
        geom = row["geometry"]

        if all((cod_dane, int(anio)) in done_keys for anio in years):
            print(f"Skipping municipality {idx + 1}/{len(mun_panel)}: {nombre} ({cod_dane}) already in checkpoint")
            continue

        geom_ee = None
        if gee_enabled and geom is not None and not geom.is_empty:
            try:
                geom_ee = ee.Geometry(geom.__geo_interface__)
            except Exception:
                geom_ee = None

        print(f"Processing municipality {idx + 1}/{len(mun_panel)}: {nombre} ({cod_dane})")

        wrote_rows_this_municipio = 0

        for anio in years:
            key = (cod_dane, int(anio))
            if key in done_keys:
                continue

            poblacion = None
            if include_population:
                if key not in population_cache:
                    population_cache[key] = obtener_poblacion_dane(
                        cod_dane,
                        anio,
                        session,
                        dane_url,
                        municipio=nombre,
                        departamento=dpto,
                        dane_rows=dane_rows,
                    )
                poblacion = population_cache[key]

            clima = {"prec_anual_mm": None, "temp_media_c": None}
            if gee_enabled and geom_ee is not None:
                if key not in climate_cache:
                    climate_cache[key] = obtener_clima_gee(geom_ee, anio)
                clima = climate_cache[key]

            rows_buffer.append(
                {
                    "COD_DANE": cod_dane,
                    "year": int(anio),
                    "poblacion_dane": poblacion,
                    "temp_media_c": clima["temp_media_c"],
                    "prec_anual_mm": clima["prec_anual_mm"],
                    "proyectos_carbono_co": int(verra_by_mun_year.get((cod_dane, int(anio)), 0)),
                    "proyectos_berkeley_co": int(berkeley_by_mun_year.get((cod_dane, int(anio)), 0)) if include_berkeley else 0,
                    "proyectos_offsetsdb_co": int(offsetsdb_by_mun_year.get((cod_dane, int(anio)), 0)) if include_offsetsdb else 0,
                    "proyectos_cdm_co": int(cdm_by_mun_year.get((cod_dane, int(anio)), 0)) if include_cdm else 0,
                }
            )
            done_keys.add(key)
            wrote_rows_this_municipio += 1

            if pause_seconds > 0:
                time.sleep(pause_seconds)

        if wrote_rows_this_municipio > 0:
            municipios_since_save += 1

        if municipios_since_save >= max(1, int(save_every_municipios)) and rows_buffer:
            checkpoint_df = pd.concat([checkpoint_df, pd.DataFrame(rows_buffer)], ignore_index=True)
            _save_checkpoint(checkpoint_df, checkpoint_file)
            print(
                f"Checkpoint saved ({len(checkpoint_df)} rows) after municipality {idx + 1}/{len(mun_panel)}"
            )
            rows_buffer = []
            municipios_since_save = 0

    if rows_buffer:
        checkpoint_df = pd.concat([checkpoint_df, pd.DataFrame(rows_buffer)], ignore_index=True)
        _save_checkpoint(checkpoint_df, checkpoint_file)
        print(f"Final checkpoint saved ({len(checkpoint_df)} rows) to: {checkpoint_file}")

    external = checkpoint_df.copy()
    if expected_keys:
        external = external[
            external.apply(lambda r: (str(r["COD_DANE"]), int(r["year"])) in expected_keys, axis=1)
        ].copy()

    if external.empty:
        raise ValueError("No enrichment rows were generated. Check municipality geometry and year filters.")

    # Build merge_cols dynamically from columns that exist in external
    merge_cols = ["COD_DANE", "year"]
    replace_cols = []
    
    # Map of column names to their corresponding include flags
    column_flags = {
        "poblacion_dane": include_population,
        "temp_media_c": include_climate,
        "prec_anual_mm": include_climate,
        "proyectos_carbono_co": include_verra,
        "proyectos_berkeley_co": include_berkeley,
        "proyectos_offsetsdb_co": include_offsetsdb,
        "proyectos_cdm_co": include_cdm,
    }
    
    # Include columns that exist in external (preserve backward compatibility with old checkpoints)
    for col, flag in column_flags.items():
        if col in external.columns:
            merge_cols.append(col)
            replace_cols.append(col)

    panel_base = panel.drop(columns=replace_cols, errors="ignore")
    panel_enriched = panel_base.merge(external[merge_cols], on=["COD_DANE", "year"], how="left")
    
    # Fill nulls only for columns that were requested via flags
    if include_verra and "proyectos_carbono_co" in panel_enriched.columns:
        panel_enriched["proyectos_carbono_co"] = panel_enriched["proyectos_carbono_co"].fillna(0).astype(int)
    if include_berkeley and "proyectos_berkeley_co" in panel_enriched.columns:
        panel_enriched["proyectos_berkeley_co"] = panel_enriched["proyectos_berkeley_co"].fillna(0).astype(int)
    if include_offsetsdb and "proyectos_offsetsdb_co" in panel_enriched.columns:
        panel_enriched["proyectos_offsetsdb_co"] = panel_enriched["proyectos_offsetsdb_co"].fillna(0).astype(int)
    if include_cdm and "proyectos_cdm_co" in panel_enriched.columns:
        panel_enriched["proyectos_cdm_co"] = panel_enriched["proyectos_cdm_co"].fillna(0).astype(int)
    if include_population and "poblacion_dane" in panel_enriched.columns:
        # poblacion_dane can be null if DANE API failed, so don't force int
        pass
    if include_climate:
        if "temp_media_c" in panel_enriched.columns:
            pass  # Keep as float
        if "prec_anual_mm" in panel_enriched.columns:
            pass  # Keep as float
    
    # Create combined project column
    carbon_total_parts: list[pd.Series] = []
    if include_verra and "proyectos_carbono_co" in panel_enriched.columns:
        carbon_total_parts.append(panel_enriched["proyectos_carbono_co"].fillna(0).astype(int))
    if include_berkeley and "proyectos_berkeley_co" in panel_enriched.columns:
        carbon_total_parts.append(panel_enriched["proyectos_berkeley_co"].fillna(0).astype(int))
    if include_offsetsdb and "proyectos_offsetsdb_co" in panel_enriched.columns:
        carbon_total_parts.append(panel_enriched["proyectos_offsetsdb_co"].fillna(0).astype(int))

    if carbon_total_parts:
        total = carbon_total_parts[0].copy()
        for part in carbon_total_parts[1:]:
            total = total + part
        panel_enriched["proyectos_carbono_total"] = total
    
    panel_enriched = panel_enriched.sort_values(["COD_DANE", "year"]).reset_index(drop=True)

    OUTPUT_PANEL_ENRICHED.parent.mkdir(parents=True, exist_ok=True)
    panel_enriched.to_csv(OUTPUT_PANEL_ENRICHED, index=False, encoding="utf-8-sig")
    print(f"Written enriched panel to: {OUTPUT_PANEL_ENRICHED}")

    if max_municipios is None:
        panel_enriched.to_csv(INPUT_PANEL, index=False, encoding="utf-8-sig")
        print(f"Updated final panel with enriched columns: {INPUT_PANEL}")
    else:
        print("Skipped updating data/final/panel_municipio_year.csv because --max-municipios was used.")

    return panel_enriched


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Enrich municipal panel with DANE, GEE and Verra sources.")
    parser.add_argument("--anio-inicio", type=int, default=2001)
    parser.add_argument("--anio-fin", type=int, default=2024)
    parser.add_argument(
        "--max-municipios",
        type=int,
        default=None,
        help="Optional cap for quick test runs.",
    )
    parser.add_argument(
        "--sin-poblacion",
        action="store_true",
        help="Skip DANE API requests.",
    )
    parser.add_argument(
        "--sin-clima",
        action="store_true",
        help="Skip Earth Engine climate extraction.",
    )
    parser.add_argument(
        "--sin-verra",
        action="store_true",
        help="Skip Verra API requests.",
    )
    parser.add_argument(
        "--solo-verra",
        action="store_true",
        help="Run only the Verra enrichment and preserve existing population/climate columns.",
    )
    parser.add_argument(
        "--pausa",
        type=float,
        default=0.3,
        help="Sleep (seconds) between municipality-year requests.",
    )
    parser.add_argument(
        "--sin-reanudar",
        action="store_true",
        help="Ignore checkpoint file and start from scratch.",
    )
    parser.add_argument(
        "--reiniciar-checkpoint",
        action="store_true",
        help="Delete checkpoint file before running.",
    )
    parser.add_argument(
        "--guardar-cada-municipios",
        type=int,
        default=1,
        help="How many municipalities to process before checkpoint save.",
    )
    parser.add_argument(
        "--checkpoint-file",
        type=str,
        default=str(CHECKPOINT_FILE),
        help="Path to checkpoint csv for resume.",
    )
    parser.add_argument(
        "--rehacer-nulos",
        action="store_true",
        help="Recompute municipality-year rows that still have null external values in checkpoint.",
    )
    parser.add_argument(
        "--ee-project",
        type=str,
        default=None,
        help="Google Earth Engine Cloud project ID used for ee.Initialize(project=...).",
    )
    parser.add_argument(
        "--dane-url",
        type=str,
        default=DANE_URL,
        help="Socrata endpoint for municipal population data (e.g. https://www.datos.gov.co/resource/<id>.json).",
    )
    parser.add_argument(
        "--start-municipio-pos",
        type=int,
        default=1,
        help="1-based municipality position to start from (useful when resuming from advanced logs).",
    )
    parser.add_argument(
        "--verra-source",
        type=str,
        default="auto",
        choices=["auto", "api", "public", "cache"],
        help="Verra source strategy: auto (api+public+cache fallback), api, public, or cache.",
    )
    parser.add_argument(
        "--verra-country",
        type=str,
        default="CO",
        help="ISO country code filter for Verra project search.",
    )
    parser.add_argument(
        "--verra-public-id-min",
        type=int,
        default=1,
        help="Minimum VCS project ID to scan in public projectDetail extraction mode.",
    )
    parser.add_argument(
        "--verra-public-id-max",
        type=int,
        default=2800,
        help="Maximum VCS project ID to scan in public projectDetail extraction mode.",
    )
    parser.add_argument(
        "--verra-local-file",
        type=str,
        default=str(VERA_LOCAL_FALLBACK),
        help="Local CSV fallback for Verra projects when online sources are unavailable.",
    )
    parser.add_argument(
        "--verra-municipio-bridge-file",
        type=str,
        default=str(VERA_MUNICIPIO_BRIDGE),
        help="CSV mapping Verra project_id to COD_DANE (and optional municipio) for municipal aggregation.",
    )
    parser.add_argument(
        "--permitir-verra-vacio",
        action="store_true",
        help="Allow continuing with empty Verra data (otherwise the run fails explicitly).",
    )
    
    # New carbon registry sources
    parser.add_argument(
        "--sin-berkeley",
        action="store_true",
        help="Skip Berkeley VROD projects (https://vrod.berkeley.edu/).",
    )
    parser.add_argument(
        "--sin-offsetsdb",
        action="store_true",
        help="Skip CarbonPlan OffsetsDB projects (https://carbonplan.org/research/offsets-db).",
    )
    parser.add_argument(
        "--offsetsdb-municipio-bridge-file",
        type=str,
        default=str(OFFSETSDB_MUNICIPIO_BRIDGE),
        help="CSV mapping OffsetsDB project_id to COD_DANE for precise municipality aggregation.",
    )
    parser.add_argument(
        "--sin-cdm",
        action="store_true",
        help="Skip CDM (Clean Development Mechanism) projects (https://cdm.unfccc.int/).",
    )
    
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    include_population = not args.sin_poblacion and not args.solo_verra
    include_climate = not args.sin_clima and not args.solo_verra
    include_verra = (not args.sin_verra) if not args.solo_verra else True
    include_berkeley = not args.sin_berkeley
    include_offsetsdb = not args.sin_offsetsdb
    include_cdm = not args.sin_cdm
    construir_panel_enriquecido(
        anio_inicio=args.anio_inicio,
        anio_fin=args.anio_fin,
        max_municipios=args.max_municipios,
        include_population=include_population,
        include_climate=include_climate,
        include_verra=include_verra,
        include_berkeley=include_berkeley,
        include_offsetsdb=include_offsetsdb,
        include_cdm=include_cdm,
        pause_seconds=args.pausa,
        resume=not args.sin_reanudar,
        reset_checkpoint=args.reiniciar_checkpoint,
        save_every_municipios=args.guardar_cada_municipios,
        checkpoint_file=Path(args.checkpoint_file),
        retry_nulls=args.rehacer_nulos,
        ee_project=args.ee_project,
        dane_url=args.dane_url,
        start_municipio_pos=args.start_municipio_pos,
        verra_source=args.verra_source,
        verra_country=args.verra_country,
        verra_public_id_min=args.verra_public_id_min,
        verra_public_id_max=args.verra_public_id_max,
        verra_local_file=Path(args.verra_local_file),
        verra_municipio_bridge_file=Path(args.verra_municipio_bridge_file),
        offsetsdb_municipio_bridge_file=Path(args.offsetsdb_municipio_bridge_file),
        allow_empty_verra=args.permitir_verra_vacio,
    )


if __name__ == "__main__":
    main()
