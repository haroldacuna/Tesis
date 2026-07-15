# tesis_gfc

Pipeline for municipal forest-loss panel construction and enrichment.

## Current pipeline

1. `scripts/01_prepare_boundaries.py`
2. `scripts/02_extract_loss_by_municipio.py`
3. `scripts/03_expand_panel_years.py`
4. `scripts/04_merge_covariates.py`
5. `scripts/05_qc_checks.py`
6. `scripts/06_enrich_panel_external.py` (new)

## New enrichment step

`06_enrich_panel_external.py` adds external covariates to the final panel:

**Population & Climate:**
- DANE population projections via `datos.gov.co`
- Annual precipitation (CHIRPS) and temperature (ERA5) from Google Earth Engine

**Carbon Projects Registry Sources (national yearly counts):**
- **Verra** (VCS): https://registry.verra.org/api/v1/projectSearch ✅ **Working**
- **Berkeley VROD**: https://vrod.berkeley.edu - Voluntary Registry Offset Database (Excel file)
- **CDM**: https://cdm.unfccc.int - Clean Development Mechanism (UN climate registry)

Input file:

- `data/final/panel_municipio_year.csv`

Output file:

- `data/final/panel_municipio_year_enriched.csv`

Auxiliary caches:

- `data/interim/verra_projects_colombia.csv`
- `data/interim/berkeley_vrod_projects_colombia.csv`
- `data/raw/auxiliary/cdm_projects_colombia.csv`

## Quick start

Test run on a small subset:

```bash
python scripts/06_enrich_panel_external.py --max-municipios 3
```

Run without Earth Engine calls:

```bash
python scripts/06_enrich_panel_external.py --sin-clima
```

Skip specific carbon registries:

```bash
python scripts/06_enrich_panel_external.py --sin-berkeley --sin-cdm
```

Run full period with custom pacing:

```bash
python scripts/06_enrich_panel_external.py --anio-inicio 2001 --anio-fin 2024 --pausa 0.2
```

Resume-safe run (recommended for long jobs):

```bash
python scripts/06_enrich_panel_external.py --anio-inicio 2001 --anio-fin 2024 --pausa 0.2 --guardar-cada-municipios 1
```

The script now writes a checkpoint file at:

- `data/interim/panel_external_checkpoint.csv`

If the computer is shut down, run the same command again and it will resume automatically.

Start over from scratch (ignore previous progress):

```bash
python scripts/06_enrich_panel_external.py --reiniciar-checkpoint
```

Run without loading checkpoint (single fresh pass):

```bash
python scripts/06_enrich_panel_external.py --sin-reanudar
```

## Available command-line arguments

**Data sources:**
- `--sin-poblacion`: Skip DANE population API
- `--sin-clima`: Skip Google Earth Engine climate extraction
- `--sin-verra`: Skip Verra projects registry
- `--sin-ecoregistry`: Skip EcoRegistry projects
- `--sin-goldstandard`: Skip Gold Standard projects
- `--sin-offsetsdb`: Skip Offsets DB projects
- `--sin-acr`: Skip American Carbon Registry projects
- `--sin-climateactiondata`: Skip Climate Action Data projects
- `--solo-verra`: Run only Verra enrichment (preserve existing population/climate columns)

**Processing control:**
- `--anio-inicio`: Start year (default: 2001)
- `--anio-fin`: End year (default: 2024)
- `--max-municipios`: Cap on number of municipalities (default: all)
- `--pausa`: Sleep seconds between municipality requests (default: 0.3)
- `--guardar-cada-municipios`: Checkpoint save frequency (default: 1)
- `--start-municipio-pos`: 1-based position to resume from (default: 1)

**Checkpoint & recovery:**
- `--reiniciar-checkpoint`: Delete existing checkpoint before running
- `--sin-reanudar`: Ignore checkpoint and start fresh
- `--rehacer-nulos`: Recompute rows with null external values
- `--checkpoint-file`: Path to checkpoint CSV

**Advanced:**
- `--ee-project`: Google Earth Engine Cloud project ID
- `--dane-url`: Custom DANE population endpoint
- `--permitir-verra-vacio`: Allow empty Verra data
- Verra-specific: `--verra-source`, `--verra-country`, `--verra-public-id-min`, `--verra-public-id-max`, `--verra-local-file`, `--verra-municipio-bridge-file`

## Notes

- Earth Engine must be authenticated before climate extraction (`earthengine authenticate`).
- The DANE API schema may change over time; missing values are handled gracefully as nulls.
- Verra values are national yearly counts and can be replaced later with municipality-level spatial matching if project geometries are available.
