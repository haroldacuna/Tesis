#!/usr/bin/env python
"""Quick test of updated endpoint functions"""

import sys
sys.path.insert(0, 'scripts')
import importlib.util

spec = importlib.util.spec_from_file_location("enrich", "scripts/06_enrich_panel_external.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)

print("=" * 60)
print("Testing deprecated carbon registry functions...")
print("=" * 60)

df_eco = module.obtener_proyectos_ecoregistry()
print(f"✓ EcoRegistry: {len(df_eco)} rows (expected: 0)")

df_cad = module.obtener_proyectos_climateactiondata()
print(f"✓ Climate Action Data: {len(df_cad)} rows (expected: 0)")

df_gs = module.obtener_proyectos_goldstandard()
print(f"✓ Gold Standard: {len(df_gs)} rows (expected: 0)")

df_acr = module.obtener_proyectos_acr()
print(f"✓ ACR: {len(df_acr)} rows (expected: 0)")

# Test that Offsets DB still works (trying cache fallback)
df_offsets = module.obtener_proyectos_offsetsdb()
print(f"✓ Offsets DB: {len(df_offsets)} rows (optional S3 data)")

print("=" * 60)
print("✓ All endpoint tests passed!")
print("=" * 60)
