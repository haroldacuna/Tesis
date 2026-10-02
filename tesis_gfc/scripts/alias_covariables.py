"""B.1 (cierre): reemplaza las copias locales de la formula de covariables
(X, X_40) por COVARIABLES_XFORMLA de 00_config_did.R, solo si son identicas."""
import re
import sys
from pathlib import Path

APLICAR = "--aplicar" in sys.argv
norm = lambda v: re.sub(r"\s+", "", v.split("#")[0])
cfg_txt = Path("scripts/00_config_did.R").read_text(encoding="utf-8")
cfg = norm(re.search(r"^COVARIABLES_XFORMLA\s*<-\s*(~[^\n]*(?:\n[^\n]*)*?)\n\s*\n", cfg_txt, re.M)
           .group(1).replace("\n", " "))

for ruta in sorted(Path("scripts").glob("*.R")):
    if ruta.name.startswith(("00_", "03")) or ruta.name.startswith("08"):
        continue
    crudo = ruta.read_bytes().decode("utf-8"); nl = "\r\n" if "\r\n" in crudo else "\n"
    lin = crudo.replace("\r\n", "\n").split("\n"); out, i, cambios = [], 0, []
    while i < len(lin):
        m = re.match(r"^(X|X_40)\s*<-\s*~", lin[i])
        if m:
            bloque = [lin[i]]
            while bloque[-1].split("#")[0].rstrip().endswith("+") and i + 1 < len(lin):
                i += 1; bloque.append(lin[i])
            val = norm(" ".join(bloque).split("<-", 1)[1])
            if val == cfg:
                out.append(f"{m.group(1)} <- COVARIABLES_XFORMLA   # B.1: lista oficial de 00_config_did.R")
                cambios.append(f"{m.group(1)}: identica -> alias a COVARIABLES_XFORMLA")
            else:
                out.extend(bloque); cambios.append(f"{m.group(1)}: DIFIERE -> se deja ({val[:70]}...)")
            i += 1; continue
        if re.match(r"^\s*COVS\s*<-", lin[i]):
            cambios.append(f"COVS (revisar a mano): {lin[i].strip()[:110]}")
        out.append(lin[i]); i += 1
    if cambios:
        print(f"== {ruta.name}"); [print("   ", c) for c in cambios]
        if APLICAR and any("alias" in c for c in cambios):
            ruta.write_bytes(nl.join(out).encode("utf-8"))
print("\nAplicado." if APLICAR else "\nSimulacion. Si se ve bien: --aplicar")