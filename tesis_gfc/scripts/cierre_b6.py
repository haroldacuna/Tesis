"""Cierre de la B.6 antes del tag:
 1. Agrega REF_EE_DR (error estandar de la principal) a 00_config_did.R y hace
    que 67 lo actualice.
 2. En 50, 51 y 53 cambia el EE de septiembre (198.5), usado como umbral de
    "1 EE" en sus reglas, por REF_EE_DR.
 3. Saca 46_event_study_tres_estimadores.R del run_all: usa otra muestra (sin
    controles limpios) y la comparacion de estimadores ya la cubre el 59.
Imprime cada cambio; si algo ya estaba hecho, lo dice."""
import re
from pathlib import Path


def editar(ruta, fn):
    p = Path(ruta); crudo = p.read_bytes().decode("utf-8"); nl = "\r\n" if "\r\n" in crudo else "\n"
    s = crudo.replace("\r\n", "\n"); s2 = fn(s)
    if s2 != s:
        p.write_bytes(s2.replace("\n", nl).encode("utf-8"))
    return s2 != s


def cfg(s):
    if re.search(r"^REF_EE_DR\s*<-", s, re.M):
        print("config: REF_EE_DR ya existe"); return s
    print("config: agrego REF_EE_DR <- 186.53")
    return re.sub(r"^(REF_ATT_DR\s*<-.*)$", r"\1\nREF_EE_DR          <- 186.53      # EE del ATT simple, DR (principal)", s, count=1, flags=re.M)
editar("scripts/00_config_did.R", cfg)


def r67(s):
    if "REF_EE_DR" in s:
        print("67: ya escribe REF_EE_DR"); return s
    print("67: ahora tambien escribe REF_EE_DR")
    return s.replace("REF_ATT_DR = round(dr$att_simple, 2), ",
                     "REF_ATT_DR = round(dr$att_simple, 2), REF_EE_DR = round(dr$ee_simple, 2), ")
editar("scripts/67_actualizar_referencias.R", r67)

for f in ["50_cohortes_loo_base.R", "51_psm_principal.R", "53_controles_sin_poligonos.R"]:
    ok = editar(f"scripts/{f}", lambda s: re.sub(r"ee\s*=\s*198\.5\b", "ee = REF_EE_DR", s))
    print(f"{f}: {'ee = 198.5 -> REF_EE_DR' if ok else 'sin cambios (ya estaba o no aparece)'}")

ok = editar("scripts/run_all_pipeline.py",
            lambda s: s.replace('    ("fase_A", "46_event_study_tres_estimadores.R"),\n',
                                '    ## 46_event_study_tres_estimadores.R fuera: usa otra muestra (sin controles limpios)\n'))
print(f"run_all: {'46_event_study fuera del pipeline' if ok else 'sin cambios'}")
