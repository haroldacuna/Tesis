import subprocess
import time

# Lista de los scripts principales del pipeline en orden de ejecución
scripts_to_run = [
    #"01_prepare_boundaries.py",
    #"02_extract_loss_by_municipio.py",
    #"03_expand_panel_years.py",
    #"04_merge_covariates.py",
    #"05_qc_checks.py",
    #"06_enrich_panel_external.py",
    #"07_monitor_progress.py",
    #"10_diagnostico_offsetsdb_detallado.py",
    #"11_diagnostico_ronda2.py",
    #"12_construir_bridge_offsetsdb.py",
    #"13_extraer_goldstandard.py",
    #"14_asignar_municipio_goldstandard_espacial.py",
    #"15_reextraer_verra_desde_offsetsdb.py",
    #"16_extraer_renare.py",
    #"17_matchear_renare_municipios.py",
    #"18_explorar_cercarbono.py",
    #"19_matchear_cercarbono_excel.py",
    "20_corregir_coords_goldstandard.py",
    #"21_diagnostico_verra_bloqueo.py",
    #"22_probar_verra_api.py",
]

def run_scripts():
    print("Iniciando la ejecución del pipeline espacial y econométrico...\n")
    start_time_total = time.time()
    
    # Lista para llevar el registro de los scripts que presenten errores
    failed_scripts = [] 

    for script in scripts_to_run:
        print(f"==================================================")
        print(f"Ejecutando: {script}")
        print(f"==================================================")
        
        start_time = time.time()
        try:
            # Ejecutamos el script
            subprocess.run(["python", script], check=True)
            
            elapsed_time = time.time() - start_time
            print(f"\n✅ [ÉXITO] {script} completado en {elapsed_time:.2f} segundos.\n")
            
        except subprocess.CalledProcessError as e:
            print(f"\n❌ [ERROR] El script {script} falló durante su ejecución.")
            print(f"Código de salida: {e.returncode}")
            print("Continuando con el siguiente script en el pipeline...\n")
            failed_scripts.append(script) 
            
        except FileNotFoundError:
            print(f"\n❌ [ERROR] No se pudo encontrar el comando 'python' o el archivo '{script}'.")
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
    
    # --- NUEVA LÍNEA AÑADIDA AQUÍ ---
    input("\nPresiona Enter para cerrar esta ventana...")

if __name__ == "__main__":
    run_scripts()