"""Aplica las exclusiones D2 al 26 sin tocar nada mas.
Cada reemplazo debe ocurrir exactamente una vez; si no, se detiene sin escribir."""
import ast
from pathlib import Path

RUTA = Path("scripts/26_consolidar_fuentes_carbono.py")
crudo = RUTA.read_bytes().decode("utf-8")
nl = "\r\n" if "\r\n" in crudo else "\n"          # conserva el fin de linea de Windows
s = crudo.replace("\r\n", "\n")

CAMBIOS = [
    (   # 1. import
        "from filtro_sectorial import anotar_sector, anotar_redd, cargar_tabla\n",
        "from filtro_sectorial import anotar_sector, anotar_redd, cargar_tabla\n"
        "from filtro_sectorial import marcar_exclusiones\n",
    ),
    (   # 2. marcar antes de guardar el consolidado (despues de la validacion de vacio)
        "    OUT_EVENTOS.parent.mkdir(parents=True, exist_ok=True)\n",
        "    # Exclusiones D2: se MARCAN sobre todos los eventos. El consolidado se\n"
        "    # guarda completo (con excluido_d2) porque un proyecto excluido sigue\n"
        "    # siendo un vinculo para la regla de controles limpios (D3).\n"
        "    eventos = marcar_exclusiones(eventos)\n\n"
        "    OUT_EVENTOS.parent.mkdir(parents=True, exist_ok=True)\n",
    ),
    (   # 3. el tratamiento se construye sin los excluidos; se conserva 'estricto'
        "    _construir_panel_tratamiento(eventos, estricto)\n",
        "    eventos_tratamiento = eventos[~eventos[\"excluido_d2\"]].copy()\n"
        "    print(f\"\\nEventos para tratamiento: {len(eventos_tratamiento)} \"\n"
        "          f\"({eventos['excluido_d2'].sum()} excluidos por D2)\")\n"
        "    _construir_panel_tratamiento(eventos_tratamiento, estricto)\n",
    ),
]

if "marcar_exclusiones" in s:
    raise SystemExit("El 26 ya tiene marcar_exclusiones: restauralo con git checkout antes de parchear.")
for viejo, nuevo in CAMBIOS:
    n = s.count(viejo)
    if n != 1:
        raise SystemExit(f"[X] Se esperaba 1 coincidencia y hay {n}:\n{viejo}")
    s = s.replace(viejo, nuevo)

ast.parse(s)                                   # no escribe si queda con error de sintaxis
RUTA.write_bytes(s.replace("\n", nl).encode("utf-8"))
print("Parche aplicado.\n")

arbol = ast.parse(s)
main = next(n for n in arbol.body if isinstance(n, ast.FunctionDef) and n.name == "main")
print(ast.get_source_segment(s, main))