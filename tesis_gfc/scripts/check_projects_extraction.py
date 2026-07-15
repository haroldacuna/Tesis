"""Check extraction results for carbon projects in the enriched panel.
Prints summary stats and sample rows.
"""
import sys
from pathlib import Path
import pandas as pd

BASE = Path(__file__).resolve().parents[1]
FINAL = BASE / "data" / "final" / "panel_municipio_year_enriched.csv"
VERRA_CACHE = BASE / "data" / "interim" / "verra_projects_colombia.csv"
BERKELEY_CACHE = BASE / "data" / "interim" / "berkeley_vrod_projects_colombia.csv"
CDM_CACHE = BASE / "data" / "raw" / "auxiliary" / "cdm_projects_colombia.csv"

if not FINAL.exists():
    print(f"FINAL_MISSING: {FINAL}")
    sys.exit(1)

print(f"Final file: {FINAL}")
print(f"Size (bytes): {FINAL.stat().st_size}\n")

# Read final CSV (use low_memory=False for mixed types)
df = pd.read_csv(FINAL, low_memory=False)
print(f"Total rows in final: {len(df)}")
if 'COD_DANE' in df.columns:
    print(f"Unique municipalities (COD_DANE): {df['COD_DANE'].nunique()}")

proj_cols = [c for c in df.columns if c.startswith('proyectos_')]
print(f"Project columns found: {proj_cols}\n")

for c in proj_cols:
    s = df[c].fillna(0).astype(int).sum()
    nonzero = (df[c].fillna(0).astype(int) > 0).sum()
    print(f"{c}: total={s}, rows_with>0={nonzero}")

# Rows with any project
any_proj = df[[c for c in proj_cols]].fillna(0).astype(int).sum(axis=1) > 0
print(f"\nRows with any project >0: {any_proj.sum()}\n")

# Top municipalities by total projects
if 'COD_DANE' in df.columns:
    by_mun = df.groupby('COD_DANE')[[c for c in proj_cols]].sum()
    by_mun['total_projects'] = by_mun.sum(axis=1)
    top = by_mun.sort_values('total_projects', ascending=False).head(10)
    print("Top COD_DANE by total projects (top 10):")
    print(top.reset_index().to_string(index=False))

# Sample rows where any project >0
print("\nSample rows with any project >0:")
sample = df[any_proj].head(10)
if not sample.empty:
    print(sample.head(10).to_string(index=False))
else:
    print("  No rows with project counts > 0 found in final file.")

# Check caches
print("\nCache files:")
for p in [VERRA_CACHE, BERKELEY_CACHE, CDM_CACHE]:
    if p.exists():
        try:
            n = sum(1 for _ in open(p, 'r', encoding='utf-8'))
        except Exception:
            n = p.stat().st_size
        print(f"  {p} -> exists, approx lines/file-size: {n}")
    else:
        print(f"  {p} -> MISSING")

print("\nDone.")
