"""B.1 (cierre): conecta 46_event_study_tres_estimadores.R y 47_linea_base_bootstrap.R
a 00_config_did.R. Reemplaza COVS <- c(...) por la lista oficial e inserta el
source() despues del primer bloque de carga de paquetes. Ya verificado (ver_covs.R)
que ambos COVS son IGUALES a la lista oficial."""
import re
from pathlib import Path

ARCHIVOS = ["scripts/46_event_study_tres_estimadores.R", "scripts/47_linea_base_bootstrap.R"]
SOURCE = 'source("scripts/00_config_did.R")   # B.1: parametros comunes (no redefinir aqui)'
ALIAS = "COVS <- all.vars(COVARIABLES_XFORMLA)   # B.1: lista oficial de 00_config_did.R"

for f in ARCHIVOS:
    ruta = Path(f)
    crudo = ruta.read_bytes().decode("utf-8"); nl = "\r\n" if "\r\n" in crudo else "\n"
    lin = crudo.replace("\r\n", "\n").split("\n")
    if any("00_config_did.R" in l for l in lin):
        print(f"{ruta.name}: ya conectado, sin cambios"); continue
    ## 1. COVS <- c(...) en una o varias lineas: hasta balancear parentesis
    i = next(j for j, l in enumerate(lin) if re.match(r"^\s*COVS\s*<-\s*c\(", l))
    j, prof = i, 0
    while True:
        cod = lin[j].split("#")[0]
        prof += cod.count("(") - cod.count(")")
        if prof <= 0: break
        j += 1
    sangria = re.match(r"^(\s*)", lin[i]).group(1)
    lin[i:j + 1] = [sangria + ALIAS]
    ## 2. source() tras el primer bloque de paquetes
    ini = next(k for k, l in enumerate(lin) if re.match(r"^(library|suppressPackageStartupMessages|require)\(", l))
    if lin[ini].startswith("suppressPackageStartupMessages") and not lin[ini].rstrip().endswith(")"):
        pos = next(k for k in range(ini, len(lin)) if re.match(r"^\s*\}\)\s*$", lin[k]))
    else:
        pos = ini
        while pos + 1 < len(lin) and re.match(r"^(library|require)\(", lin[pos + 1]): pos += 1
    lin.insert(pos + 1, SOURCE)
    ruta.write_bytes(nl.join(lin).encode("utf-8"))
    print(f"{ruta.name}: COVS (lineas {i + 1}-{j + 1}) -> alias; source() en la linea {pos + 2}")
