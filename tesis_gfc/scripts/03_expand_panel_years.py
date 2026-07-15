from pathlib import Path

import geopandas as gpd
import pandas as pd


MUNICIPIOS_CANDIDATES = [
    Path("data/interim/municipios_clean.gpkg"),
    Path("data/interim/municipios_clean.geojson"),
]
MUNICIPIOS_LAYER = "municipios_clean"
LOSS_BY_YEAR_FILE = Path("data/interim/loss_by_municipio_year.csv")
OUT_PANEL_BASE = Path("data/interim/panel_municipio_year_base.csv")
START_YEAR = 2001
END_YEAR = 2024


def _write_empty_panel(reason: str) -> None:
    panel = pd.DataFrame(
        columns=["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR", "year", "loss_pixels", "loss_area_ha"]
    )
    OUT_PANEL_BASE.parent.mkdir(parents=True, exist_ok=True)
    panel.to_csv(OUT_PANEL_BASE, index=False)
    print(f"Warning: {reason}")
    print(f"Written empty base panel to: {OUT_PANEL_BASE}")


def _pick_municipios_file() -> Path:
    for path in MUNICIPIOS_CANDIDATES:
        if path.exists():
            return path
    raise FileNotFoundError(f"Missing municipalities file. Tried: {MUNICIPIOS_CANDIDATES}")


def _validate_inputs(municipios_file: Path) -> None:
    if not municipios_file.exists():
        raise FileNotFoundError(f"Missing municipalities file: {municipios_file}")
    if not LOSS_BY_YEAR_FILE.exists():
        raise FileNotFoundError(f"Missing yearly loss file: {LOSS_BY_YEAR_FILE}")


def main() -> None:
    try:
        municipios_file = _pick_municipios_file()
    except FileNotFoundError:
        _write_empty_panel("municipalities file not found")
        return

    if not LOSS_BY_YEAR_FILE.exists():
        _write_empty_panel("yearly loss file not found")
        return

    _validate_inputs(municipios_file)

    if municipios_file.suffix.lower() == ".gpkg":
        mun = gpd.read_file(municipios_file, layer=MUNICIPIOS_LAYER)
    else:
        mun = gpd.read_file(municipios_file)
    loss = pd.read_csv(LOSS_BY_YEAR_FILE)

    if mun.empty:
        _write_empty_panel("municipalities layer is empty")
        return

    if loss.empty:
        _write_empty_panel("yearly loss file is empty")
        return

    mun_info = mun[["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR"]].drop_duplicates().copy()
    mun_info["COD_DANE"] = mun_info["COD_DANE"].astype(str)

    loss = loss.copy()
    loss["COD_DANE"] = loss["COD_DANE"].astype(str)
    loss["year"] = loss["year"].astype(int)

    years = list(range(START_YEAR, END_YEAR + 1))
    base = (
        pd.MultiIndex.from_product([mun_info["COD_DANE"].unique(), years], names=["COD_DANE", "year"])
        .to_frame(index=False)
    )

    panel = base.merge(
        loss[["COD_DANE", "year", "loss_pixels", "loss_area_ha"]],
        on=["COD_DANE", "year"],
        how="left",
    )
    panel["loss_pixels"] = panel["loss_pixels"].fillna(0).astype(int)
    panel["loss_area_ha"] = panel["loss_area_ha"].fillna(0.0)

    panel = panel.merge(mun_info, on="COD_DANE", how="left")
    panel = panel[["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR", "year", "loss_pixels", "loss_area_ha"]]
    panel = panel.sort_values(["COD_DANE", "year"]).reset_index(drop=True)

    OUT_PANEL_BASE.parent.mkdir(parents=True, exist_ok=True)
    panel.to_csv(OUT_PANEL_BASE, index=False)
    print(f"Written base panel to: {OUT_PANEL_BASE}")


if __name__ == "__main__":
    main()