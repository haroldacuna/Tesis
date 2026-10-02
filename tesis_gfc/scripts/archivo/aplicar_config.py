"""B.1 -- Conecta los scripts de estimacion a scripts/00_config_did.R.

Para cada script de la lista:
  1. Inserta  source("scripts/00_config_did.R")  despues de los library().
  2. Quita las definiciones locales de SEMILLA, OUTCOME, VENTANA, BALANCE_E,
     BITERS y COVARIABLES_XFORMLA, PERO solo si su valor es IDENTICO al de la
     configuracion. Si difiere, la deja y la reporta como DIFIERE: puede ser un
     override legitimo (p. ej. OUTCOME = tasa en el 05) o una lista vieja.
  3. COVARIABLES_XFORMLA distinta: con --reemplazar-covariables se quita
     igual (el script pasa a usar la lista oficial). Es lo que corrige 05, 09
     y 11, que usaban la lista vieja de 5 variables.

USO (desde tesis_gfc/):
  python scripts/aplicar_config.py                      # simulacion: solo reporta
  python scripts/aplicar_config.py --aplicar --reemplazar-covariables
"""
import re
import sys
from pathlib import Path

SCRIPTS = ["05_did_tasa_deforestacion.R", "09_tablas_regresion.R", "11_heterogeneidad_territorial.R",
           "40_did_depurada_y_dosis.R", "43_robustez_anticipacion_y_loo.R", "44_honestdid.R",
           "45_spillovers_depurada.R", "46_did_mapbiomas.R", "48_soporte_comun.R",
           "49_controles_no_contiguos.R", "50_cohortes_loo_base.R", "51_psm_principal.R",
           "53_controles_sin_poligonos.R", "58_contraste_mecanismo_escalera.R",
           "59_periodo_base_y_poisson.R", "60_robusteces.R"]
CONFIG = Path("scripts/00_config_did.R")
LINEA_SOURCE = 'source("scripts/00_config_did.R")   # B.1: parametros comunes (no redefinir aqui)'
APLICAR = "--aplicar" in sys.argv
REEMPLAZAR_COV = "--reemplazar-covariables" in sys.argv
SIMPLES = ["SEMILLA", "OUTCOME", "VENTANA", "BALANCE_E", "BITERS"]

norm = lambda v: re.sub(r"\s+", "", v.split("#")[0]).rstrip(";")

def valores_config(txt):
    vals = {}
    for p in SIMPLES:
        m = re.search(rf"^{p}\s*<-\s*(.+)$", txt, re.M)
        vals[p] = norm(m.group(1))
    m = re.search(r"^COVARIABLES_XFORMLA\s*<-\s*(~[^\n]*(?:\n[^\n]*)*?)\n\s*\n", txt, re.M)
    vals["COVARIABLES_XFORMLA"] = norm(m.group(1).replace("\n", " "))
    return vals

def procesar(ruta, cfg):
    crudo = ruta.read_bytes().decode("utf-8")
    nl = "\r\n" if "\r\n" in crudo else "\n"
    lineas = crudo.replace("\r\n", "\n").split("\n")
    rep = []
    if any("00_config_did.R" in l for l in lineas):
        return None, ["ya conectado a 00_config_did.R (sin cambios)"]
    salida, i = [], 0
    while i < len(lineas):
        l = lineas[i]
        ## COVARIABLES_XFORMLA (puede ocupar varias lineas terminadas en '+')
        if re.match(r"^COVARIABLES_XFORMLA\s*<-", l):
            bloque = [l]
            while bloque[-1].split("#")[0].rstrip().endswith("+") and i + 1 < len(lineas):
                i += 1; bloque.append(lineas[i])
            valor = norm(" ".join(bloque).split("<-", 1)[1])
            if valor == cfg["COVARIABLES_XFORMLA"]:
                rep.append("COVARIABLES_XFORMLA: identica -> se quita")
            elif REEMPLAZAR_COV:
                rep.append(f"COVARIABLES_XFORMLA: DIFIERE ({valor[:60]}...) -> se quita y pasa a la lista oficial")
            else:
                rep.append(f"COVARIABLES_XFORMLA: DIFIERE ({valor[:60]}...) -> se deja (usa --reemplazar-covariables)")
                salida.extend(bloque)
            i += 1; continue
        ## Parametros simples (puede haber varios en una linea separados por ';')
        if re.match(rf"^({'|'.join(SIMPLES)})\s*<-", l):
            partes, quedan = [p for p in l.split("#")[0].split(";") if p.strip()], []
            for p in partes:
                nom, val = [x.strip() for x in p.split("<-", 1)]
                if nom in cfg and norm(val) == cfg[nom]:
                    rep.append(f"{nom}: identico -> se quita")
                else:
                    rep.append(f"{nom}: DIFIERE ({norm(val)} vs {cfg.get(nom)}) -> se deja como override local")
                    quedan.append(p.strip())
            if quedan:
                salida.append("; ".join(quedan))
            i += 1; continue
        salida.append(l); i += 1
    ## Insertar el source despues del PRIMER bloque de carga de paquetes
    ## (suppressPackageStartupMessages({...}) o una racha de library()), no
    ## despues del ultimo library(), que puede estar dentro de una funcion.
    pos = None
    ini = next((j for j, l in enumerate(salida)
                if re.match(r"^(library|suppressPackageStartupMessages|require)\(", l)), None)
    if ini is not None:
        if salida[ini].startswith("suppressPackageStartupMessages") and not salida[ini].rstrip().endswith(")"):
            pos = next(j for j in range(ini, len(salida)) if re.match(r"^\s*\}\)\s*$", salida[j]))
        else:
            pos = ini
            while pos + 1 < len(salida) and re.match(r"^(library|require)\(", salida[pos + 1]):
                pos += 1
    if pos is None:
        pos = next(j for j, l in enumerate(salida) if l.strip() and not l.startswith("#")) - 1
    salida.insert(pos + 1, LINEA_SOURCE)
    rep.append(f"source() insertado en la linea {pos + 2}")
    return nl.join(salida), rep

if not CONFIG.exists():
    sys.exit(f"Falta {CONFIG}: copialo antes de correr este script.")
cfg = valores_config(CONFIG.read_text(encoding="utf-8"))
print("MODO:", "APLICAR" if APLICAR else "SIMULACION (no escribe nada)",
      "| reemplazar covariables:", REEMPLAZAR_COV, "\n")
for nombre in SCRIPTS:
    ruta = Path("scripts") / nombre
    if not ruta.exists():
        print(f"== {nombre}: no existe (se omite)"); continue
    nuevo, rep = procesar(ruta, cfg)
    print(f"== {nombre}")
    for r in rep: print("   ", r)
    if nuevo is not None and APLICAR:
        ruta.write_bytes(nuevo.encode("utf-8"))
print("\nListo." if APLICAR else "\nSimulacion terminada. Si el reporte se ve bien: --aplicar --reemplazar-covariables")
