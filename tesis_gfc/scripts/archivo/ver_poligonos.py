"""Lista los archivos de poligonos y busca Vision Amazonia por nombre."""
from pathlib import Path
import geopandas as gpd

CARPETA = Path("data/raw/runap")
EXT = {".gpkg", ".shp", ".geojson", ".json", ".kml"}
for f in sorted(p for p in CARPETA.rglob("*") if p.suffix.lower() in EXT):
    try:
        g = gpd.read_file(f)
    except Exception as e:
        print(f"\n== {f}  [no se pudo leer: {e}]"); continue
    print(f"\n== {f}  ({len(g)} filas, CRS {g.crs})")
    print("   columnas:", list(g.columns))
    txt = g.drop(columns="geometry").astype(str)
    hit = txt.apply(lambda c: c.str.contains("reserva natural de la sociedad", case=False)).any(axis=1)
    if hit.any():
        print("   filas que mencionan 'Amazon':")
        print(txt[hit].head(10).to_string())