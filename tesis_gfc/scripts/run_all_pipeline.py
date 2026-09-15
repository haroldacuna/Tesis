import subprocess
import sys
import time
from pathlib import Path

# Carpeta del proyecto (tesis_gfc/). Todos los scripts asumen que se ejecutan
# con este directorio como cwd (usan rutas relativas tipo "data/...",
# "outputs/..."), así que fijamos el cwd explícitamente en vez de depender
# de desde dónde se lance este archivo.
PROJECT_ROOT = Path(__file__).resolve().parent.parent
SCRIPTS_DIR = Path(__file__).resolve().parent

# Rscript se instala fuera del PATH en este equipo. Se usa una ruta explícita
# para que el lanzador sea reproducible desde VS Code y PowerShell.
RSCRIPT = Path(r"C:\Program Files\R\R-4.4.1\bin\Rscript.exe")

# Orden de producción: primero se construyen los datos, después se consolidan
# las fuentes y finalmente se ejecutan los modelos y tablas en R.
scripts_to_run = [
    "00_diagnostico_geometrias.py",
    "00b_rastrear_procedencia.py",
    "00c_rastrear_arbol_original.py",
    "00d_verificar_rasters.py",
    "01_prepare_boundaries.py",
    "02_extract_loss_by_municipio.py",
    "03_expand_panel_years.py",
    "04_merge_covariates.py",
    "12_construir_bridge_offsetsdb.py",
    "13_extraer_goldstandard.py",
    "14_asignar_municipio_goldstandard_espacial.py",
    "20_corregir_coords_goldstandard.py",
    "16_extraer_renare.py",
    "17_matchear_renare_municipios.py",
    "19_matchear_cercarbono_excel.py",
    "24_extraer_verra_platts.py",
    "25_asignar_municipio_verra_espacial.py",
    "21_reparar_deforestacion_panel.py",
    "06_enrich_panel_external.py",
    "05_qc_checks.py",
    "27_auditar_sector_proyectos.py",
    "28_clasificar_sector.py",
    "29_auditar_matching_municipal.py",
    "26_consolidar_fuentes_carbono.py",
    "reconstruir_eventos_intensidad.py",
    "30_recalcular_clima_gee_directo.py",
    "31_integrar_clima_gee_al_panel.py",
    "32_integrar_accesibilidad_conflicto_cede.py",
    "asignar_goldstandard_espacial.R",
    "01_preparar_datos_did.R",
    "02_matching_psm.R",
    "03_did_callaway_santanna.R",
    "03b_excluir_vecinos_de_control.R",
    "03c_diagnostico_composicion_dr_filtrado.R",
    "04_att_por_grupo.R",
    "04b_diagnostico_discrepancias_numericas.R",
    "04c_inspeccion_cohortes_2006_2025_y_columna_region.R",
    "05_did_tasa_deforestacion.R",
    "06_verificar_hallazgos.R",
    "07_auditar_n_efectivo_por_cohorte.R",
    "08_sensibilidad_excluir_cohortes_tempranas.R",
    "09_tablas_regresion.R",
    "10_spillovers_espaciales.R",
    "11_heterogeneidad_territorial.R",
    "13_sun_abraham.R",
    "14_intensidad_tratamiento.R",
    "15_diagnostico_twfe_vs_cs.R",
    "15_robustez_periodo_base_sutva.R",
    "15_tablas_modelsummary.R",
    "15_tablas_stargazer.R",
]


def run_scripts():
    print("Iniciando la ejecución del pipeline de obtención de la data final...\n")
    print(f"Directorio de trabajo: {PROJECT_ROOT}\n")
    start_time_total = time.time()

    # Lista para llevar el registro de los scripts que presenten errores
    failed_scripts = []

    for script in scripts_to_run:
        print(f"==================================================")
        print(f"Ejecutando: {script}")
        print(f"==================================================")

        script_path = SCRIPTS_DIR / script
        start_time = time.time()
        try:
            if script_path.suffix.lower() == ".r":
                if not RSCRIPT.exists():
                    raise FileNotFoundError(f"No se encontró Rscript en '{RSCRIPT}'.")
                command = [str(RSCRIPT), str(script_path)]
            else:
                command = [sys.executable, str(script_path)]

            subprocess.run(
                command,
                check=True,
                cwd=PROJECT_ROOT,
            )

            elapsed_time = time.time() - start_time
            print(f"\n✅ [ÉXITO] {script} completado en {elapsed_time:.2f} segundos.\n")

        except subprocess.CalledProcessError as e:
            print(f"\n❌ [ERROR] El script {script} falló durante su ejecución.")
            print(f"Código de salida: {e.returncode}")
            print("Continuando con el siguiente script en el pipeline...\n")
            failed_scripts.append(script)

        except FileNotFoundError:
            print(f"\n❌ [ERROR] No se pudo encontrar el archivo '{script_path}'.")
            print("Continuando con el siguiente script...\n")
            failed_scripts.append(script)

    total_time = time.time() - start_time_total

    # Resumen final de la ejecución
    print("==================================================")
    print(f"PIPELINE FINALIZADO")
    print(f"Tiempo total de ejecución: {total_time:.2f} segundos.")

    if failed_scripts:
        print("\n⚠️ ADVERTENCIA: Los siguientes scripts presentaron errores y no terminaron correctamente:")
        for fs in failed_scripts:
            print(f"  - {fs}")
    else:
        print("\n✅ Todos los scripts se ejecutaron exitosamente sin errores.")
    print("==================================================")

if __name__ == "__main__":
    run_scripts()
