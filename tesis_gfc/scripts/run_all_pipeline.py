#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
run_all_pipeline.py  (tarea B.5)
================================

Corrida final de la tesis, del tratamiento a las robusteces, en el orden que
exigen las dependencias. Cada paso deja su salida en logs/run_all_<fecha>/;
el pipeline se detiene en el primer error y dice desde donde retomarlo.

Etapas
  datos        26 -> 38 -> 39_reconciliar -> 62 -> 63
  clima (gee)  61   (solo con --gee; si no, usa el CSV ya calculado)
  panel        01 -> 02
  principal    40 -> 66 (regla SINAP; si cambia la configuracion, vuelve a correr 40) -> 67 (referencias)
  principal    + 71 (grupo de comparacion, Tabla 4)
  fase_A       58, 59, 44, 60, 45, 46_did_mapbiomas, 46_event_study, 48, 49, 50, 51, 53
  descriptivos 64, 65
  celdas       39 MapBiomas, 52, 54, 55, 56, 57   (solo con --completo; Earth Engine y horas de computo)

No se re-ejecuta la construccion del panel base desde datos crudos (01_prepare_boundaries.py
a 32_*.py): sus salidas en data/final/ son insumo fijo.

USO (desde tesis_gfc/):
  python scripts\\run_all_pipeline.py --lista                    # solo muestra los pasos
  python scripts\\run_all_pipeline.py                            # corrida estandar
  python scripts\\run_all_pipeline.py --gee --ee-project <ID>    # + clima de linea base
  python scripts\\run_all_pipeline.py --desde 40_did_depurada_y_dosis.R   # retomar
  python scripts\\run_all_pipeline.py --completo --ee-project <ID>        # todo
"""
from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import os
import subprocess
import sys
import time
from pathlib import Path

PASOS = [
    ("datos", "26_consolidar_fuentes_carbono.py"),
    ("datos", "38_construir_dosis_tratamiento.py"),
    ("datos", "39_reconciliar_dosis_panel.py"),
    # 76 usa las salidas de 52 y 55 (etapa celdas); si faltan, se detiene con aviso
    ("datos", "76_dosis_poligonos.py"),
    ("clima", "61_clima_linea_base.py"),
    ("datos", "62_areas_protegidas.py"),
    ("datos", "63_fiscal_linea_base.py"),
    ("panel", "01_preparar_datos_did.R"),
    ("panel", "02_matching_psm.R"),
    ("principal", "40_did_depurada_y_dosis.R"),
    ("principal", "66_regla_sinap.R"),
    ("principal", "67_actualizar_referencias.R"),
    ("principal", "71_grupo_comparacion.R"),
    ("principal", "77_gradiente_dosis.R"),
    ("principal", "75_descriptivos.R"),
    ("fase_A", "58_contraste_mecanismo_escalera.R"),
    ("fase_A", "59_periodo_base_y_poisson.R"),
    ("fase_A", "44_honestdid.R"),
    ("fase_A", "60_robusteces.R"),
    ("fase_A", "45_spillovers_depurada.R"),
    ("fase_A", "46_did_mapbiomas.R"),
    ## 46_event_study_tres_estimadores.R fuera: usa otra muestra (sin controles limpios)
    ("fase_A", "48_soporte_comun.R"),
    ("fase_A", "49_controles_no_contiguos.R"),
    ("fase_A", "50_cohortes_loo_base.R"),
    ("fase_A", "74_loo_ic.R"),
    ("fase_A", "51_psm_principal.R"),
    ("fase_A", "53_controles_sin_poligonos.R"),
    ("fase_A", "68_heterogeneidad_fiscal.R"),
    ("descriptivos", "64_verificar_placebo.py"),
    ("descriptivos", "65_territorios_proponentes.py"),
    ("celdas", "39_transiciones_mapbiomas.py"),
    ("celdas", "52_poligonos_proyectos.py"),
    ("celdas", "54_celdas_gee_export.py"),
    ("celdas", "55_panel_celdas.py"),
    ("celdas", "56_did_celdas.R"),
    ("celdas", "57_figura_mde_celdas.R"),
    ("celdas", "72_distancia_borde.py"),
    ("celdas", "73_rdd_espacial.R"),
    # 78 reconcilia conteos con las salidas finales (40, 60 A10/A11, 71, 73)
    ("celdas", "78_construccion_muestra.py"),
]
CONFIG = Path("scripts/00_config_did.R")
USAN_EE = {"61_clima_linea_base.py", "39_transiciones_mapbiomas.py", "54_celdas_gee_export.py"}


def hash_cfg() -> str:
    return hashlib.md5(CONFIG.read_bytes()).hexdigest() if CONFIG.exists() else ""


def correr(script: str, log: Path, ee_project: str | None) -> int:
    ruta = Path("scripts") / script
    cmd = (["Rscript", str(ruta)] if script.endswith(".R") else [sys.executable, str(ruta)])
    if script in USAN_EE and ee_project:
        cmd += ["--ee-project", ee_project]
    env = dict(os.environ, PYTHONIOENCODING="utf-8", PYTHONUTF8="1")
    with open(log, "w", encoding="utf-8") as f:
        f.write(f"$ {' '.join(cmd)}\n\n")
        p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=env)
        for linea in iter(p.stdout.readline, b""):
            txt = linea.decode("utf-8", errors="replace")
            sys.stdout.write("    " + txt); f.write(txt)
        return p.wait()


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--lista", action="store_true")
    ap.add_argument("--gee", action="store_true", help="recalcular el clima (61) en Earth Engine")
    ap.add_argument("--completo", action="store_true", help="incluir MapBiomas y celdas (Earth Engine)")
    ap.add_argument("--ee-project", default=None)
    ap.add_argument("--desde", default=None, help="retomar desde este script")
    a = ap.parse_args()
    if not Path("data/.raiz_canonica").exists():
        sys.exit("Corre desde tesis_gfc/ (raiz canonica).")

    activas = {"datos", "panel", "principal", "fase_A", "descriptivos"}
    if a.gee or a.completo: activas.add("clima")
    if a.completo: activas.add("celdas")
    pasos = [(e, s) for e, s in PASOS if e in activas]
    if a.desde:
        nombres = [s for _, s in pasos]
        if a.desde not in nombres: sys.exit(f"--desde {a.desde}: no esta entre los pasos activos")
        pasos = pasos[nombres.index(a.desde):]
    faltan = [s for _, s in pasos if not (Path("scripts") / s).exists()]
    if faltan: sys.exit(f"Faltan scripts: {faltan}")
    if (("clima" in activas) or ("celdas" in activas)) and not a.ee_project:
        sys.exit("--gee/--completo requieren --ee-project <ID>")
    if "clima" not in activas and not Path("data/interim/clima_linea_base_municipio.csv").exists():
        sys.exit("No hay clima de linea base calculado: corre con --gee --ee-project <ID>")

    print(f"{len(pasos)} pasos | etapas: {', '.join(sorted(activas))}")
    for i, (e, s) in enumerate(pasos, 1): print(f"  {i:2d}. [{e}] {s}")
    if a.lista: return 0

    carpeta = Path("logs") / f"run_all_{dt.datetime.now():%Y%m%d_%H%M}"
    carpeta.mkdir(parents=True, exist_ok=True)
    resumen, t0 = [], time.time()
    i = 0
    while i < len(pasos):
        etapa, script = pasos[i]
        print(f"\n=== [{etapa}] {script} ===")
        t = time.time(); antes = hash_cfg()
        rc = correr(script, carpeta / f"{len(resumen) + 1:02d}_{script}.log", a.ee_project)
        dur = time.time() - t
        if script == "66_regla_sinap.R" and rc == 10:
            ## La regla cambio la lista de covariables: la principal debe volver a correr
            print("    -> la regla de SINAP cambio 00_config_did.R: se vuelve a correr el 40")
            resumen.append((script, "cambio config", dur))
            pasos.insert(i + 1, ("principal", "40_did_depurada_y_dosis.R"))
            i += 1; continue
        resumen.append((script, "ok" if rc == 0 else f"ERROR {rc}", dur))
        if rc != 0:
            print(f"\n[X] {script} fallo (codigo {rc}). Log: {carpeta}")
            print(f"    Retomar con: python scripts\\run_all_pipeline.py --desde {script}")
            break
        if hash_cfg() != antes and script != "67_actualizar_referencias.R":
            print(f"    [!] {script} modifico 00_config_did.R")
        i += 1
    print("\n" + "=" * 70 + "\nRESUMEN\n" + "=" * 70)
    for s, est, d in resumen: print(f"  {s:45s} {est:14s} {d / 60:6.1f} min")
    print(f"  Total: {(time.time() - t0) / 60:.1f} min | logs en {carpeta}")
    return 0 if all(e in ("ok", "cambio config") for _, e, _ in resumen) and len(resumen) >= len(pasos) else 1


if __name__ == "__main__":
    sys.exit(main())
