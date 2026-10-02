"""Reemplaza _cargar_verra() por la version con proyecto_id y elimina SOSPECHOSOS_VERRA.
Las exclusiones quedan solo en filtro_sectorial.EXCLUSIONES (decision D2)."""
import ast
from pathlib import Path

RUTA = Path("scripts/26_consolidar_fuentes_carbono.py")
crudo = RUTA.read_bytes().decode("utf-8")
nl = "\r\n" if "\r\n" in crudo else "\n"
s = crudo.replace("\r\n", "\n")

NUEVA = '''def _cargar_verra() -> pd.DataFrame:
    if not VERRA_FILE.exists():
        print(f"Aviso: no encuentro {VERRA_FILE}, se omite Verra.")
        return pd.DataFrame()

    df = pd.read_csv(VERRA_FILE, low_memory=False)
    df = df[df["COD_DANE"].notna()].copy()

    # El ID es la llave de las exclusiones D2 (filtro_sectorial.EXCLUSIONES).
    col_id = next((c for c in ["resourceIdentifier", "id", "ID"] if c in df.columns), None)
    if col_id is None:
        raise KeyError(f"Verra sin columna de ID; columnas: {list(df.columns)[:20]}")
    print(f"Verra: columna de ID: {col_id}")

    # Las exclusiones ya NO se aplican aqui: las marca filtro_sectorial
    # (marcar_exclusiones, en main). Los eventos excluidos se conservan en el
    # consolidado para la regla de controles limpios (D3).

    # projectStartDate primero: se verifico que coincide con
    # creditPeriodStartDate en los 44 REDD+ comparables.
    campo_fecha = next(
        (c for c in ["projectStartDate", "creditPeriodStartDate", "startDate"] if c in df.columns),
        None,
    )
    campo_fecha_fin = next((c for c in ["projectEndDate", "creditPeriodEndDate"] if c in df.columns), None)

    out = pd.DataFrame({
        "COD_DANE": df["COD_DANE"].astype(str).str.zfill(5),
        "fuente": "verra",
        # "1657.0" -> "1657": pandas lee el ID como float si hay vacios
        "proyecto_id": df[col_id].astype(str).str.strip().str.replace(r"\\.0$", "", regex=True),
        "nombre_proyecto": df.get("projectName", ""),
        "anio_inicio": df[campo_fecha].apply(_parse_fecha) if campo_fecha else None,
        "anio_fin": df[campo_fecha_fin].apply(_parse_fecha) if campo_fecha_fin else None,
        "confianza": "alta",
        "metodo": "punto_en_poligono",
        # Solo Verra publica estado de registro. Se arrastra para poder
        # distinguir proyectos vivos de retirados o rechazados.
        "estado_registro": df["status"].values if "status" in df.columns else None,
    })
    print(f"Verra: {len(out)} eventos cargados ({out['anio_inicio'].notna().sum()} con anio de inicio valido).")
    return out
'''

arbol = ast.parse(s)
lineas = s.split("\n")
tramos = []  # (inicio, fin) en indices de linea, base 0
for nodo in arbol.body:
    if isinstance(nodo, ast.FunctionDef) and nodo.name == "_cargar_verra":
        tramos.append((nodo.lineno - 1, nodo.end_lineno, NUEVA.rstrip("\n").split("\n")))
    if isinstance(nodo, ast.Assign) and any(getattr(t, "id", "") == "SOSPECHOSOS_VERRA" for t in nodo.targets):
        tramos.append((nodo.lineno - 1, nodo.end_lineno, []))
if len(tramos) != 2:
    raise SystemExit(f"[X] Esperaba encontrar _cargar_verra y SOSPECHOSOS_VERRA; encontre {len(tramos)} tramo(s).")

for ini, fin, nuevo in sorted(tramos, reverse=True):   # de abajo hacia arriba
    lineas[ini:fin] = nuevo
s = "\n".join(lineas)

if "SOSPECHOSOS_VERRA" in s:
    raise SystemExit("[X] Quedan referencias a SOSPECHOSOS_VERRA; revisalas a mano.")
ast.parse(s)
RUTA.write_bytes(s.replace("\n", nl).encode("utf-8"))
print("Parche aplicado: _cargar_verra con proyecto_id; SOSPECHOSOS_VERRA eliminado.")