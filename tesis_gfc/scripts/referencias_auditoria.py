"""Reemplaza la referencia vieja de la principal (-207.9..., corrida de sept.)
por REF_ATT_DR de 00_config_did.R en los scripts de la auditoria (48-53).
Simulacion por defecto: imprime cada linea antes/despues. Con --aplicar escribe.
Si un script no carga 00_config_did.R, lo avisa y no lo toca."""
import re
import sys
from pathlib import Path

APLICAR = "--aplicar" in sys.argv
PAT = re.compile(r"-207\.9\d*")
for ruta in sorted(Path("scripts").glob("5[0-3]_*.R")) + sorted(Path("scripts").glob("4[89]_*.R")):
    crudo = ruta.read_bytes().decode("utf-8"); nl = "\r\n" if "\r\n" in crudo else "\n"
    lin = crudo.replace("\r\n", "\n").split("\n")
    hits = [i for i, l in enumerate(lin) if PAT.search(l.split("#")[0])]
    if not hits:
        continue
    conectado = any("00_config_did.R" in l for l in lin)
    print(f"== {ruta.name}" + ("" if conectado else "  [!] no carga 00_config_did.R: no se modifica"))
    for i in hits:
        nuevo = PAT.sub("REF_ATT_DR", lin[i].split("#")[0]) + ("#" + "#".join(lin[i].split("#")[1:]) if "#" in lin[i] else "")
        print(f"   {i + 1}: {lin[i].strip()[:100]}\n   -> {nuevo.strip()[:100]}")
        if conectado:
            lin[i] = nuevo
    if APLICAR and conectado:
        ruta.write_bytes(nl.join(lin).encode("utf-8"))
print("\nAplicado." if APLICAR else "\nSimulacion. Si se ve bien: --aplicar")
