"""Muestra las salidas de 56 (DiD en celdas) y 57 (efecto minimo detectable)."""
from pathlib import Path
import pandas as pd

pd.set_option("display.width", 250); pd.set_option("display.max_columns", 30); pd.set_option("display.max_colwidth", 60)
rutas = sorted(set(Path("outputs").rglob("5[67]_*.csv")) | set(Path("data").rglob("5[67]_*.csv")))
if not rutas:
    print("No encontre CSV 56_*/57_* en outputs/ ni data/")
for r in rutas:
    df = pd.read_csv(r)
    print(f"\n== {r}  ({len(df)} filas)")
    print("   columnas:", list(df.columns))
    print(df.head(12).to_string(index=False))
print("\nFiguras y rds del 56/57:")
for r in sorted(Path("outputs").rglob("5[67]_*")):
    if r.suffix != ".csv":
        print("  ", r)