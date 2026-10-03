"""Reemplaza las referencias escritas a mano en 44, 58, 59 y 60 por las de
00_config_did.R (REF_*), para que la corrida final no dependa de numeros viejos.
Imprime cada cambio; si un patron no aparece, lo avisa y sigue."""
import re
from pathlib import Path

CAMBIOS = {
    "59_periodo_base_y_poisson.R": [
        (r"^REF_40\s*<-\s*-?[0-9.]+.*$", "REF_40    <- REF_ATT_SINCOV   # de 00_config_did.R (corrida final)"),
        (r"^REF_40_DR\s*<-\s*-?[0-9.]+.*$", "REF_40_DR <- REF_ATT_DR       # de 00_config_did.R (corrida final)"),
    ],
    "60_robusteces.R": [
        (r"^REF_40_DR\s*<-\s*-?[0-9.]+.*$", "REF_40_DR <- REF_ATT_DR       # de 00_config_did.R (corrida final)"),
    ],
    "44_honestdid.R": [
        (r"^REFERENCIA_BAL\s*<-\s*c\(.*\)\s*$",
         "REFERENCIA_BAL <- c(dr = REF_BAL_DR, sincov = REF_BAL_SINCOV)   # de 00_config_did.R"),
    ],
    "58_contraste_mecanismo_escalera.R": [
        (r"sum\(mun\$G_dep == 0\) != 986 \|\| sum\(mun\$G_dep > 0\) != 36",
         "sum(mun$G_dep == 0) != REF_N_CONTROLES || sum(mun$G_dep > 0) != REF_N_TRATADOS"),
    ],
}
for nombre, reglas in CAMBIOS.items():
    ruta = Path("scripts") / nombre
    crudo = ruta.read_bytes().decode("utf-8"); nl = "\r\n" if "\r\n" in crudo else "\n"
    s = crudo.replace("\r\n", "\n")
    for patron, nuevo in reglas:
        s2, n = re.subn(patron, lambda m: nuevo, s, flags=re.M)
        print(f"{nombre}: {'OK' if n else 'NO ENCONTRADO'} ({n}) -> {nuevo[:70]}")
        s = s2
    ruta.write_bytes(s.replace("\n", nl).encode("utf-8"))
