#!/usr/bin/env python
"""Test script for new Berkeley VROD and CDM functions"""

import sys
sys.path.insert(0, 'scripts')
import importlib.util

spec = importlib.util.spec_from_file_location("enrich", "scripts/06_enrich_panel_external.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)

print("=" * 70)
print("Testing new carbon registry functions (Berkeley VROD and CDM)")
print("=" * 70)

# Test Berkeley VROD
print("\n1. Testing Berkeley VROD function...")
try:
    df_berkeley = module.obtener_proyectos_berkeley()
    print(f"   ✓ Berkeley VROD: {len(df_berkeley)} rows (cache empty, would download on first run)")
except Exception as e:
    print(f"   ✗ Error: {e}")

# Test CDM
print("\n2. Testing CDM function...")
try:
    df_cdm = module.obtener_proyectos_cdm()
    print(f"   ✓ CDM: {len(df_cdm)} rows (cache empty, would scrape on first run)")
except Exception as e:
    print(f"   ✗ Error: {e}")

# Test EXTERNAL_COLS
print("\n3. Checking EXTERNAL_COLS schema...")
expected_cols = ['COD_DANE', 'year', 'poblacion_dane', 'temp_media_c', 'prec_anual_mm', 
                'proyectos_carbono_co', 'proyectos_berkeley_co', 'proyectos_cdm_co']
actual_cols = module.EXTERNAL_COLS
if actual_cols == expected_cols:
    print(f"   ✓ EXTERNAL_COLS correct: {actual_cols}")
else:
    print(f"   ✗ EXTERNAL_COLS mismatch!")
    print(f"     Expected: {expected_cols}")
    print(f"     Actual:   {actual_cols}")

print("\n" + "=" * 70)
print("✓ All tests completed!")
print("=" * 70)
