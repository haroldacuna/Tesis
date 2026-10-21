"""Imprime los tres datos que faltan para cerrar los [VERIFICAR] de los capitulos 4 y 5."""
from pathlib import Path
import re
import pandas as pd

pd.set_option("display.width", 200); pd.set_option("display.max_columns", 20)

print("== 1. Tabla 4.1: clasificacion sectorial por registro")
for r in sorted(Path("data").rglob("clasificacion_sectorial*.csv")):
    d = pd.read_csv(r)
    print(f"-- {r} ({len(d)} filas) columnas: {list(d.columns)}")
    fuente = next((c for c in d.columns if re.search("fuente|registro|source", c, re.I)), None)
    for c in d.columns:
        if re.search("sector|clase|categ|redd|afolu", c, re.I) and d[c].nunique() < 12:
            print(pd.crosstab(d[fuente], d[c], margins=True) if fuente else d[c].value_counts(dropna=False))

print("\n== 2. Estado del proyecto de Boomitra (Verra)")
for r in sorted(Path("data").rglob("verra*.csv")):
    d = pd.read_csv(r, low_memory=False)
    m = d.apply(lambda f: f.astype(str).str.contains("Boomitra|5335|110200000672002", case=False)).any(axis=1)
    if m.any():
        cols = [c for c in d.columns if re.search("id|name|nombre|status|estado|issu|credit|emit", c, re.I)]
        print(f"-- {r}"); print(d.loc[m, cols].head().to_string(index=False))

print("\n== 3. Ancho de la zona de amortiguacion (54)")
for r in Path("scripts").glob("54_*.py"):
    for i, l in enumerate(r.read_text(encoding="utf-8", errors="replace").splitlines(), 1):
        if re.search("buffer|amortig|anillo", l, re.I):
            print(f"{r.name}:{i}: {l.strip()[:140]}")
