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

# Scripts del flujo de obtención de la data final, en orden de ejecución.
# Ver scripts/README.md para el detalle de qué produce cada etapa.
scripts_to_run = [
    # 1) Base geoespacial y panel municipio-año
    "01_prepare_boundaries.py",
    "02_extract_loss_by_municipio.py",
    "03_expand_panel_years.py",
    "04_merge_covariates.py",
    # 2) Extracción y geolocalización de fuentes de proyectos de carbono
    #    (alimentan la consolidación del paso 4)
    "12_construir_bridge_offsetsdb.py",
    "13_extraer_goldstandard.py",
    "14_asignar_municipio_goldstandard_espacial.py",
    "20_corregir_coords_goldstandard.py",
    "16_extraer_renare.py",
    "17_matchear_renare_municipios.py",
    "19_matchear_cercarbono_excel.py",
    "24_extraer_verra_platts.py",
    "25_asignar_municipio_verra_espacial.py",
    # 3) Enriquecimiento externo (población, clima, proyectos de carbono) y QC
    "06_enrich_panel_external.py",
    "05_qc_checks.py",
    # 4) Consolidación de eventos de tratamiento (carbono) sobre el panel
    "26_consolidar_fuentes_carbono.py",
    # 5) Cierre de huecos de clima vía Google Earth Engine
    "30_recalcular_clima_gee_directo.py",
    "31_integrar_clima_gee_al_panel.py",
    # 6) Covariables de accesibilidad/conflicto (CEDE) -> dataset final para PSM/DiD
    "32_integrar_accesibilidad_conflicto_cede.py",
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
            subprocess.run(
                [sys.executable, str(script_path)],
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

    input("\nPresiona Enter para cerrar esta ventana...")


if __name__ == "__main__":
    run_scripts()
