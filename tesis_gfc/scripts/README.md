# Scripts

Esta carpeta contiene únicamente los scripts necesarios para reconstruir la
data final del proyecto (deforestación + covariables + eventos de carbono) y
los scripts de análisis econométrico que consumen esa data final. Se
eliminaron los scripts de diagnóstico, exploración y pruebas puntuales que se
usaron durante el desarrollo pero que no son necesarios para reproducir los
resultados.

## Cómo correr el pipeline de datos

```
python run_all_pipeline.py
```

`run_all_pipeline.py` ejecuta, en orden, los scripts Python que construyen la
data final (ver lista abajo). Fija el directorio de trabajo en `tesis_gfc/`
automáticamente, así que puede lanzarse desde cualquier ubicación.

## Etapas del pipeline (Python)

1. **Base geoespacial y panel municipio-año**
   `01_prepare_boundaries.py` → `02_extract_loss_by_municipio.py` →
   `03_expand_panel_years.py` → `04_merge_covariates.py`
   Produce `data/final/panel_municipio_year.csv`.

2. **Extracción y geolocalización de proyectos de carbono** (alimentan la
   consolidación del paso 4): `12_construir_bridge_offsetsdb.py`,
   `13_extraer_goldstandard.py`, `14_asignar_municipio_goldstandard_espacial.py`,
   `20_corregir_coords_goldstandard.py` (corrige coordenadas inválidas
   detectadas en el paso anterior), `16_extraer_renare.py`,
   `17_matchear_renare_municipios.py`, `19_matchear_cercarbono_excel.py`,
   `24_extraer_verra_platts.py`, `25_asignar_municipio_verra_espacial.py`.

3. **Enriquecimiento externo y QC**: `06_enrich_panel_external.py` (población
   DANE, clima histórico, proyectos Verra/Berkeley/CDM/OffsetsDB) →
   `05_qc_checks.py` (valida el panel resultante, escribe
   `outputs/tables_figures/qc_report.txt`).
   Produce `data/final/panel_municipio_year_enriched.csv` y
   `data/final/dataset_consolidado_completo.csv`.

4. **Consolidación de eventos de tratamiento**: `26_consolidar_fuentes_carbono.py`
   cruza todas las fuentes de carbono geolocalizadas del paso 2 con el panel
   enriquecido. Produce `data/final/panel_con_tratamiento_actualizado.csv`.

5. **Cierre de huecos de clima vía Google Earth Engine**:
   `30_recalcular_clima_gee_directo.py` → `31_integrar_clima_gee_al_panel.py`.
   Produce `data/final/panel_con_clima_completo.csv`.

6. **Covariables de accesibilidad/conflicto (CEDE)**:
   `32_integrar_accesibilidad_conflicto_cede.py`. Produce
   `data/final/panel_con_psm_covariables.csv`, la data final usada por el
   análisis (junto con `panel_con_tratamiento_actualizado.csv`).

### Utilidad opcional

`07_monitor_progress.py` no forma parte del flujo secuencial: es un monitor
que se corre en paralelo, en otra terminal, mientras `06_enrich_panel_external.py`
está corriendo, para ver su progreso en vivo leyendo el log. No produce data.

## Análisis econométrico (R)

Los scripts `01_preparar_datos_did.R` a `11_heterogeneidad_territorial.R` son
una etapa posterior e independiente: toman la data final producida arriba
(`panel_con_psm_covariables.csv` y `panel_con_tratamiento_actualizado.csv`) y
corren el matching, el DiD de Callaway–Sant'Anna y los análisis de robustez.
No forman parte de `run_all_pipeline.py` porque corren en R, no en Python, y
se ejecutan manualmente en orden numérico según el análisis que se quiera
reproducir.
