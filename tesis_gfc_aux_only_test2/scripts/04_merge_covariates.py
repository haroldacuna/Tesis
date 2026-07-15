from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import rasterio
from rasterstats import zonal_stats


MUNICIPIOS_CANDIDATES = [
    Path("data/interim/municipios_clean.gpkg"),
    Path("data/interim/municipios_clean.geojson"),
]
MUNICIPIOS_LAYER = "municipios_clean"
PANEL_BASE_FILE = Path("data/interim/panel_municipio_year_base.csv")
AUXILIARY_DIR = Path("data/raw/auxiliary")
OUT_BASELINE = Path("data/interim/baseline_forest.csv")
OUT_FINAL = Path("data/final/panel_municipio_year.csv")
PIXEL_HA = 0.09


def _write_empty_outputs(reason: str) -> None:
    baseline = pd.DataFrame(columns=["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR", "baseline_forest"])
    final = pd.DataFrame(
        columns=[
            "COD_DANE",
            "NOMBRE_MPI",
            "DPTO_CNMBR",
            "year",
            "loss_pixels",
            "loss_area_ha",
            "baseline_forest",
        ]
    )
    OUT_BASELINE.parent.mkdir(parents=True, exist_ok=True)
    OUT_FINAL.parent.mkdir(parents=True, exist_ok=True)
    baseline.to_csv(OUT_BASELINE, index=False)
    final.to_csv(OUT_FINAL, index=False)
    print(f"Warning: {reason}")
    print(f"Written empty baseline covariate to: {OUT_BASELINE}")
    print(f"Written empty final panel to: {OUT_FINAL}")


def _pick_municipios_file() -> Path:
    for path in MUNICIPIOS_CANDIDATES:
        if path.exists():
            return path
    raise FileNotFoundError(f"Missing municipalities file. Tried: {MUNICIPIOS_CANDIDATES}")


def _validate_inputs(municipios_file: Path) -> None:
    required = [municipios_file, PANEL_BASE_FILE]
    missing = [path for path in required if not path.exists()]
    if missing:
        raise FileNotFoundError(f"Missing required inputs: {missing}")


def _pick_treecover_file() -> Path | None:
    if not AUXILIARY_DIR.exists():
        return None

    candidates = []
    for file_path in AUXILIARY_DIR.glob("*.tif"):
        name = file_path.stem.lower()
        if "treecover" in name:
            rank = 0 if "2000" in name else 1
            candidates.append((rank, len(file_path.name), file_path.name.lower(), file_path))

    if not candidates:
        return None

    candidates.sort(key=lambda item: (item[0], item[1], item[2]))
    return candidates[0][3]


def _pick_datamask_file() -> Path | None:
    if not AUXILIARY_DIR.exists():
        return None

    candidates = [p for p in AUXILIARY_DIR.glob("*.tif") if "datamask" in p.stem.lower()]
    if not candidates:
        return None

    candidates.sort(key=lambda p: (len(p.name), p.name.lower()))
    return candidates[0]


def _baseline_forest_by_municipio(mun: gpd.GeoDataFrame, treecover_file: Path | None, datamask_file: Path | None) -> pd.DataFrame:
    if treecover_file is None or not treecover_file.exists() or treecover_file.stat().st_size == 0:
        baseline = mun[["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR"]].copy()
        baseline["COD_DANE"] = baseline["COD_DANE"].astype(str)
        baseline["baseline_forest"] = 0.0
        print("Warning: treecover raster unavailable in data/raw/auxiliary. Using baseline_forest=0.")
        return baseline.drop_duplicates().sort_values("COD_DANE").reset_index(drop=True)

    try:
        with rasterio.open(treecover_file) as src:
            tree = src.read(1)
            affine = src.transform
            raster_crs = src.crs
    except rasterio.errors.RasterioIOError:
        baseline = mun[["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR"]].copy()
        baseline["COD_DANE"] = baseline["COD_DANE"].astype(str)
        baseline["baseline_forest"] = 0.0
        print(f"Warning: treecover raster is invalid ({treecover_file}). Using baseline_forest=0.")
        return baseline.drop_duplicates().sort_values("COD_DANE").reset_index(drop=True)

    if mun.crs != raster_crs:
        mun = mun.to_crs(raster_crs)

    valid_mask = np.ones(tree.shape, dtype=np.uint8)
    if datamask_file is not None and datamask_file.exists() and datamask_file.stat().st_size > 0:
        try:
            with rasterio.open(datamask_file) as src_mask:
                datamask = src_mask.read(1)
            valid_mask = (datamask > 0).astype(np.uint8)
        except rasterio.errors.RasterioIOError:
            print(f"Warning: datamask raster is invalid ({datamask_file}). Ignoring datamask.")

    # Hansen convention: treecover2000 in percent canopy cover (0-100).
    forest_pixels = ((tree >= 30) & (valid_mask == 1)).astype(np.uint8)

    stats = zonal_stats(
        mun,
        forest_pixels,
        affine=affine,
        stats=["sum"],
        nodata=0,
        geojson_out=False,
    )

    baseline_rows = []
    for i, stat in enumerate(stats):
        pixels = int(stat.get("sum", 0) if stat else 0)
        baseline_rows.append(
            {
                "COD_DANE": str(mun.iloc[i]["COD_DANE"]),
                "NOMBRE_MPI": str(mun.iloc[i]["NOMBRE_MPI"]),
                "DPTO_CNMBR": str(mun.iloc[i]["DPTO_CNMBR"]),
                "baseline_forest": pixels * PIXEL_HA,
            }
        )

    baseline = pd.DataFrame(baseline_rows)
    baseline = baseline.groupby(["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR"], as_index=False)["baseline_forest"].sum()
    baseline = baseline.sort_values("COD_DANE").reset_index(drop=True)
    return baseline


def main() -> None:
    try:
        municipios_file = _pick_municipios_file()
    except FileNotFoundError:
        _write_empty_outputs("municipalities file not found")
        return

    if not PANEL_BASE_FILE.exists():
        _write_empty_outputs("panel base file not found")
        return

    _validate_inputs(municipios_file)
    treecover_file = _pick_treecover_file()
    datamask_file = _pick_datamask_file()

    if municipios_file.suffix.lower() == ".gpkg":
        mun = gpd.read_file(municipios_file, layer=MUNICIPIOS_LAYER)
    else:
        mun = gpd.read_file(municipios_file)
    panel = pd.read_csv(PANEL_BASE_FILE)

    if mun.empty or panel.empty:
        _write_empty_outputs("municipalities layer or panel base is empty")
        return

    panel["COD_DANE"] = panel["COD_DANE"].astype(str)

    baseline = _baseline_forest_by_municipio(mun, treecover_file, datamask_file)

    OUT_BASELINE.parent.mkdir(parents=True, exist_ok=True)
    baseline.to_csv(OUT_BASELINE, index=False)

    final = panel.merge(
        baseline[["COD_DANE", "baseline_forest"]],
        on="COD_DANE",
        how="left",
    )
    final["baseline_forest"] = final["baseline_forest"].fillna(0.0)
    final = final.sort_values(["COD_DANE", "year"]).reset_index(drop=True)

    OUT_FINAL.parent.mkdir(parents=True, exist_ok=True)
    final.to_csv(OUT_FINAL, index=False)

    print(f"Written baseline covariate to: {OUT_BASELINE}")
    print(f"Written final panel to: {OUT_FINAL}")


if __name__ == "__main__":
    main()
