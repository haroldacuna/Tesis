from pathlib import Path

import geopandas as gpd


BOUNDARIES_DIR = Path("data/raw/boundaries")
OUTPUT_FILE = Path("data/interim/municipios_clean.gpkg")
OUTPUT_LAYER = "municipios_clean"


def _pick_best_boundaries_file(boundaries_dir: Path) -> Path:
	"""Select the best available municipal boundaries file.

	Preference order: GeoPackage > Shapefile > GeoJSON.
	Within each format, file names containing "municip" are prioritized.
	"""
	if not boundaries_dir.exists():
		raise FileNotFoundError(f"Boundaries directory not found: {boundaries_dir}")

	candidates = []
	extension_priority = {".gpkg": 0, ".shp": 1, ".geojson": 2, ".json": 3}

	for ext, ext_rank in extension_priority.items():
		for file_path in boundaries_dir.glob(f"*{ext}"):
			name_rank = 0 if "municip" in file_path.stem.lower() else 1
			candidates.append((ext_rank, name_rank, file_path.name.lower(), file_path))

	if not candidates:
		raise FileNotFoundError(
			"No supported boundaries file found in data/raw/boundaries. "
			"Expected one of: .gpkg, .shp, .geojson, .json"
		)

	candidates.sort(key=lambda item: (item[0], item[1], item[2]))
	return candidates[0][3]


def _find_column(gdf: gpd.GeoDataFrame, options: list[str], label: str) -> str:
	"""Find the first matching column name from a list of common alternatives."""
	col_lookup = {col.upper(): col for col in gdf.columns}
	for option in options:
		if option.upper() in col_lookup:
			return col_lookup[option.upper()]
	raise ValueError(f"Missing required {label} column. Tried: {options}")


def _build_cod_dane(mun: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
	"""Build a standardized 5-digit municipality code in COD_DANE."""
	full_code_options = [
		"COD_DANE",
		"MPIO_CDPMP",
		"MUNICIPIO",
		"CODIGO_MPIO",
		"DANE",
	]

	for option in full_code_options:
		try:
			code_col = _find_column(mun, [option], "municipality code")
			codes = mun[code_col].astype(str).str.replace(".0", "", regex=False).str.zfill(5)
			if codes.str.len().eq(5).all() and codes.nunique() > 1000:
				mun = mun.copy()
				mun["COD_DANE"] = codes
				return mun
		except ValueError:
			continue

	dept_code_col = _find_column(mun, ["DPTO_CCDGO", "COD_DPTO", "DEPARTAMEN"], "department code")
	mpio_code_col = _find_column(mun, ["MPIO_CCDGO", "COD_MPIO", "MUNICIPIO"], "municipality code")

	mun = mun.copy()
	mun["COD_DANE"] = (
		mun[dept_code_col].astype(str).str.replace(".0", "", regex=False).str.zfill(2)
		+ mun[mpio_code_col].astype(str).str.replace(".0", "", regex=False).str.zfill(3)
	)
	return mun


def main() -> None:
	source_file = _pick_best_boundaries_file(BOUNDARIES_DIR)
	mun = gpd.read_file(source_file)
	mun = _build_cod_dane(mun)

	name_col = _find_column(mun, ["NOMBRE_MPI", "MPIO_CNMBR", "MUNICIPIO", "NOM_MPIO"], "municipality name")
	dept_col = _find_column(mun, ["DPTO_CNMBR", "NOMBRE_DPT", "DEPARTAMENTO", "NOM_DPTO"], "department name")

	mun = mun[["COD_DANE", name_col, dept_col, "geometry"]].copy()
	mun.columns = ["COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR", "geometry"]

	# Fix invalid geometries while preserving polygon topology.
	mun["geometry"] = mun.buffer(0)

	# GFC rasters are commonly processed in EPSG:4326.
	mun = mun.to_crs("EPSG:4326")

	OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
	mun.to_file(OUTPUT_FILE, layer=OUTPUT_LAYER, driver="GPKG")

	print(f"Source boundaries used: {source_file}")
	print(f"Output written to: {OUTPUT_FILE}")


if __name__ == "__main__":
	main()