"""
parche_filtro_cercarbono.py  (uso unico)

Reemplaza la rama `if f == "cercarbono":` de scripts/filtro_sectorial.py por
la version que separa sectores mixtos con ";". Deja copia en
filtro_sectorial.py.bak. Correr desde tesis_gfc\\:

    python parche_filtro_cercarbono.py
"""
import re
import shutil
import sys
from pathlib import Path

RUTA = Path("scripts/filtro_sectorial.py")
ANCLA_INI = '    if f == "cercarbono":'
ANCLA_FIN = '    # -- respaldo por nombre'

RAMA = '''    if f == "cercarbono":
        crudo = str(fila.get("sector__Sector", "") or "")
        partes = {normalizar(p) for p in crudo.split(";") if p.strip()}
        if not partes:
            return ETIQUETA_PENDIENTE, "sin campo Sector (revisar cols_base en 19 / COLS_SECTOR en 28)"
        etiquetas = {CERC_SECTOR.get(p, "DESCONOCIDO") for p in partes}
        if etiquetas == {"AFOLU"}:
            return "AFOLU", f"Sector: {crudo[:60]}"
        if etiquetas == {"NO_AFOLU"}:
            return "NO_AFOLU", f"Sector: {crudo[:60]}"
        return ETIQUETA_PENDIENTE, f"Sector mixto o no mapeado: {crudo[:60]}"

'''

DICT = '''CERC_SECTOR = {
    "land use (afolu)": "AFOLU",
    "energy industries": "NO_AFOLU",
    "waste handling and disposal": "NO_AFOLU",
    "fugitive emissions from fuels (solids, oil and gas)": "NO_AFOLU",
    "combined (sectors 1 and 13)": "NO_AFOLU",
    "manufacturing industry": "NO_AFOLU",
}
'''

COMENTARIO = '''# Cercarbono: categorias del campo 'Sector' del Projects Report (sept. 2026).
# "Combined (Sectors 1 and 13)" = alcances 1 (energia) y 13 (residuos).
'''

s = RUTA.read_text(encoding="utf-8")
# Idempotente: reescribe rama y diccionario completos en cada corrida.

ini, fin = s.find(ANCLA_INI), s.find(ANCLA_FIN)
if ini == -1 or fin == -1 or fin < ini:
    sys.exit(f"[X] No encuentro las anclas (ini={ini}, fin={fin}). Pega la rama a mano.")

shutil.copy(RUTA, RUTA.with_name("filtro_sectorial.py.bak"))
s = s[:ini] + RAMA + s[fin:]
patron_dict = re.compile(r"^CERC_SECTOR = \{.*?^\}\n", re.S | re.M)
if patron_dict.search(s):
    s = patron_dict.sub(lambda _: DICT, s, count=1)      # dict viejo o incompleto
else:
    k = s.find("def _match(")
    s = s[:k] + COMENTARIO + DICT + "\n\n" + s[k:]
RUTA.write_text(s, encoding="utf-8")

# Prueba minima
sys.path.insert(0, "scripts")
import pandas as pd
from filtro_sectorial import clasificar_automatico
casos = {
    "Land use (AFOLU)": "AFOLU",
    "Energy industries; Waste handling and disposal": "NO_AFOLU",
    "Energy industries; Land use (AFOLU)": "REVISAR",
    "": "REVISAR",
}
for sector, esperado in casos.items():
    got, motivo = clasificar_automatico("cercarbono", pd.Series({"nombre_proyecto": "x", "sector__Sector": sector}))
    estado = "ok" if got == esperado else "FALLA"
    print(f"[{estado}] {sector or '(vacio)'!r:50} -> {got}  ({motivo})")
