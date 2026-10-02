"""Muestra ID, area declarada y area del municipio anfitrion de los
candidatos a exclusion D2 (CO2ROZO, Boomitra y el corredor de robles)."""
import importlib.util
import sys
from pathlib import Path

import geopandas as gpd
import pandas as pd

sys.path.insert(0, "scripts")  # para que el 26 encuentre filtro_sectorial
spec = importlib.util.spec_from_file_location("m26", "scripts/26_consolidar_fuentes_carbono.py")
m26 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m26)   # no corre main(): esta protegida por __name__

df = pd.read_csv(m26.VERRA_FILE, low_memory=False)
patron = "CO2ROZO|Boomitra|Oak ecological"
cand = df[df["projectName"].astype(str).str.contains(patron, case=False)].copy()
cols_area = [c for c in df.columns if "area" in c.lower()]
cand["COD_DANE"] = cand["COD_DANE"].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(5)

# Area del municipio anfitrion, en ha, desde los limites del DANE (EPSG:9377)
mun = gpd.read_file("data/interim/municipios_clean.gpkg")
col_cod = next(c for c in mun.columns if c.upper() in ("COD_DANE", "MPIO_CDPMP", "CODIGO"))
mun["COD_DANE"] = mun[col_cod].astype(str).str.zfill(5)
mun["area_mun_ha"] = mun.to_crs(9377).area / 1e4
cand = cand.merge(mun[["COD_DANE", "area_mun_ha"]], on="COD_DANE", how="left")

pd.set_option("display.width", 200)
print(cand[["id", "projectName", "COD_DANE", "area_mun_ha"] + cols_area].to_string(index=False))