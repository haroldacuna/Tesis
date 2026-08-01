#!/usr/bin/env python
"""Quick smoke test for the current external data loaders."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT / "scripts"))

spec = importlib.util.spec_from_file_location("enrich", str(ROOT / "scripts" / "06_enrich_panel_external.py"))
module = importlib.util.module_from_spec(spec)
try:
    spec.loader.exec_module(module)
except ModuleNotFoundError as exc:
    print(f"Skipping smoke test: missing dependency {exc.name}")
    raise SystemExit(0)


def _assert_dataframe(name: str, df: pd.DataFrame) -> None:
    assert isinstance(df, pd.DataFrame), f"{name} did not return a DataFrame"
    print(f"✓ {name}: {len(df)} rows")


print("=" * 60)
print("Testing current external loaders...")
print("=" * 60)

_assert_dataframe("Berkeley VROD", module.obtener_proyectos_berkeley())
_assert_dataframe("OffsetsDB", module.obtener_proyectos_offsetsdb())
_assert_dataframe("CDM", module.obtener_proyectos_cdm())

assert "proyectos_offsetsdb_co" in module.EXTERNAL_COLS, "OffsetsDB column not wired into EXTERNAL_COLS"
print("✓ OffsetsDB column is present in EXTERNAL_COLS")

print("=" * 60)
print("✓ All endpoint smoke tests passed!")
print("=" * 60)
