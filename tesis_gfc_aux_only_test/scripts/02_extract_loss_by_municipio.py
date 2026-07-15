from pathlib import Path

import geopandas as gpd
import pandas as pd
from rasterstats import zonal_stats


MUNICIPIOS_CANDIDATES = [
    Path("data/interim/municipios_clean.gpkg"),
    Path("data/interim/municipios_clean.geojson"),
]
MUNICIPIOS_LAYER = "municipios_clean"
LOSSYEAR_RASTER = Path("data/raw/gfc/Colombia_GFC_lossyear.tif")
OUT_BY_YEAR = Path("data/interim/loss_by_municipio_year.csv")
OUT_TOTAL = Path("data/interim/loss_by_municipio.csv")
PIXEL_HA = 0.09


def _pick_municipios_file() -> Path:
    for path in MUNICIPIOS_CANDIDATES:
        if path.exists():
            return path
    raise FileNotFoundError(f"Missing municipalities file. Tried: {MUNICIPIOS_CANDIDATES}")


def _validate_inputs(municipios_file: Path) -> None:
    if not municipios_file.exists():
        raise FileNotFoundError(f"Missing municipalities file: {municipios_file}")
    if not LOSSYEAR_RASTER.exists():
        raise FileNotFoundError(f"Missing GFC lossyear raster: {LOSSYEAR_RASTER}")


def main() -> None:
    municipios_file = _pick_municipios_file()
    _validate_inputs(municipios_file)

    if municipios_file.suffix.lower() == ".gpkg":
        mun = gpd.read_file(municipios_file, layer=MUNICIPIOS_LAYER)
    else:
        mun = gpd.read_file(municipios_file)
    required_cols = {"COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR", "geometry"}
    missing = required_cols - set(mun.columns)
    if missing:
        raise ValueError(f"Municipalities file missing columns: {sorted(missing)}")

    # Count categorical lossyear classes per municipality.
    stats = zonal_stats(
        mun,
        str(LOSSYEAR_RASTER),
        categorical=True,
        geojson_out=False,
        nodata=0,
    )

    rows: list[dict[str, object]] = []
    for i, stat in enumerate(stats):
        if not stat:
            continue

        cod = str(mun.iloc[i]["COD_DANE"])
        nombre = str(mun.iloc[i]["NOMBRE_MPI"])
        dpto = str(mun.iloc[i]["DPTO_CNMBR"])

        for lossyear_value, pixel_count in stat.items():
            if int(lossyear_value) == 0:
                continue
            year = 2000 + int(lossyear_value)
            rows.append(
                {
                    "COD_DANE": cod,
                    "NOMBRE_MPI": nombre,
                    "DPTO_CNMBR": dpto,
                    "year": year,
                    "loss_pixels": int(pixel_count),
                }
            )

    by_year = pd.DataFrame(rows)
    if by_year.empty:
        raise ValueError("No loss observations extracted. Check raster coverage and CRS alignment.")

    by_year["loss_area_ha"] = by_year["loss_pixels"] * PIXEL_HA
    by_year = by_year.sort_values(["COD_DANE", "year"]).reset_index(drop=True)

    by_total = (
        by_year.groupby(["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR"], as_index=False)[
            ["loss_pixels", "loss_area_ha"]
        ]
        .sum()
        .sort_values("COD_DANE")
        .reset_index(drop=True)
    )

    OUT_BY_YEAR.parent.mkdir(parents=True, exist_ok=True)
    by_year.to_csv(OUT_BY_YEAR, index=False)
    by_total.to_csv(OUT_TOTAL, index=False)

    print(f"Written yearly loss to: {OUT_BY_YEAR}")
    print(f"Written total loss to: {OUT_TOTAL}")


if __name__ == "__main__":
    main()