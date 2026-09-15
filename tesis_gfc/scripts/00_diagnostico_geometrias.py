"""
00_diagnostico_geometrias.py

Localiza el punto exacto del pipeline en que Antioquia (DANE 05) y Atlantico
(DANE 08) pierden su cobertura de bosque y su perdida anual.

Hallazgo que motiva este script: los 125 municipios de Antioquia y los 23 de
Atlantico tienen baseline_forest = 0 y loss_area_ha = 0,00 en los 24 anios del
panel. Ningun otro departamento presenta un solo caso.

Se puede ejecutar desde cualquier carpeta: el script localiza la raiz del
proyecto por si mismo. Solo LEE archivos; no escribe ni modifica nada.

USO
---
    python 00_diagnostico_geometrias.py
"""

from pathlib import Path

import pandas as pd

try:
    import geopandas as gpd
except ImportError:
    gpd = None

try:
    import rasterio
except ImportError:
    rasterio = None


DPTOS_SOSPECHOSOS = {"05", "08"}
NOMBRES_DPTO = {"05": "ANTIOQUIA", "08": "ATLANTICO"}


# =============================================================================
# ETAPA 0 — Localizar la raiz del proyecto
# =============================================================================

def encontrar_raiz() -> Path:
    """Busca hacia arriba, desde el script y desde el directorio actual, las
    carpetas que contienen un subdirectorio 'data', y se queda con la que
    tiene el arbol de datos mas completo.

    El desempate importa: si el pipeline se corrio alguna vez desde 'scripts/',
    existe un scripts/data/ huerfano y casi vacio que competiria con el arbol
    real. Se puntua por cantidad y peso de los archivos bajo data/."""
    candidatas = []
    origenes = [Path(__file__).resolve().parent, Path.cwd().resolve()]
    for origen in origenes:
        for c in [origen, *origen.parents]:
            if (c / "data").is_dir() and c not in candidatas:
                candidatas.append(c)

    if not candidatas:
        return Path.cwd().resolve()

    def puntaje(c: Path) -> tuple[int, int]:
        archivos = [p for p in (c / "data").rglob("*") if p.is_file()]
        peso = sum(p.stat().st_size for p in archivos)
        return (len(archivos), peso)

    puntuadas = sorted(((puntaje(c), c) for c in candidatas), reverse=True)

    if len(puntuadas) > 1:
        print("  Aviso: se detectaron varios arboles data/ en la jerarquia:")
        for (n, peso), c in puntuadas:
            print(f"    {c}  ({n} archivos, {peso/1e6:,.1f} MB)")

    return puntuadas[0][1]


def etapa_0(raiz: Path) -> None:
    print("=" * 78)
    print("ETAPA 0 — Ubicacion de archivos")
    print("=" * 78)
    print(f"  Directorio actual : {Path.cwd()}")
    print(f"  Raiz detectada    : {raiz}")

    # Copias duplicadas de la capa de municipios: sintoma de corridas hechas
    # desde carpetas distintas, cada una con su propio arbol data/.
    copias = sorted(raiz.rglob("municipios_clean.gpkg"))
    print(f"\n  Copias de municipios_clean.gpkg encontradas: {len(copias)}")
    for ruta in copias:
        tam = ruta.stat().st_size
        n = "?"
        if gpd is not None:
            try:
                n = len(gpd.read_file(ruta, layer="municipios_clean"))
            except Exception:
                n = "error al leer"
        marca = "  <-- VACIA" if n == 0 else ""
        print(f"    {ruta.relative_to(raiz)}  ({tam:,} bytes, {n} registros){marca}")

    if len(copias) > 1:
        print("\n  >>> Hay mas de una copia. El pipeline se corrio desde carpetas")
        print("      distintas y cada corrida genero su propio arbol data/.")
        print("      Determinar cual alimento realmente al panel final.")

    arboles = sorted({p.parent.parent for p in raiz.rglob("data/interim")})
    if len(arboles) > 1:
        print(f"\n  Arboles data/ distintos dentro del proyecto: {len(arboles)}")
        for a in arboles:
            print(f"    {a.relative_to(raiz) if a != raiz else '.'}")


# =============================================================================
# ETAPA 1 — Shapefile de limites original
# =============================================================================

def etapa_1(raiz: Path):
    print("\n" + "=" * 78)
    print("ETAPA 1 — Limites originales (data/raw/boundaries)")
    print("=" * 78)

    if gpd is None:
        print("  geopandas no esta instalado. Se omite.")
        return None

    carpeta = raiz / "data" / "raw" / "boundaries"
    if not carpeta.is_dir():
        print(f"  No existe {carpeta}")
        alternativas = [p for p in raiz.rglob("*.gpkg") if "boundar" in str(p).lower()]
        alternativas += [p for p in raiz.rglob("*.shp")]
        if alternativas:
            print("  Archivos espaciales encontrados en el proyecto:")
            for p in alternativas[:15]:
                print(f"    {p.relative_to(raiz)}")
        return None

    # Misma logica de seleccion que _pick_best_boundaries_file() del script 01.
    prioridad = {".gpkg": 0, ".shp": 1, ".geojson": 2, ".json": 3}
    candidatos = []
    for ext, rango_ext in prioridad.items():
        for ruta in carpeta.glob(f"*{ext}"):
            rango_nombre = 0 if "municip" in ruta.stem.lower() else 1
            candidatos.append((rango_ext, rango_nombre, ruta.name.lower(), ruta))

    if not candidatos:
        print(f"  Sin archivos .gpkg/.shp/.geojson en {carpeta}")
        print("  >>> Con esta carpeta vacia, el script 01 escribe una capa VACIA")
        print("      via _write_empty_municipios() y el pipeline continua.")
        return None

    candidatos.sort(key=lambda x: (x[0], x[1], x[2]))
    archivo = candidatos[0][3]
    mun = gpd.read_file(archivo)

    print(f"  Archivo seleccionado : {archivo.name}")
    print(f"  CRS                  : {mun.crs}")
    print(f"  Registros            : {len(mun)}")

    if len(mun) == 0:
        print("  >>> El insumo original esta VACIO.")
        return mun

    col_cod = next(
        (c for c in mun.columns if "COD" in c.upper() and ("MPIO" in c.upper() or "DANE" in c.upper())),
        next((c for c in mun.columns if "COD" in c.upper()), None),
    )
    if col_cod is None:
        print("  No se identifico columna de codigo. Columnas:", list(mun.columns))
        return mun

    print(f"  Columna de codigo    : {col_cod} (dtype {mun[col_cod].dtype})")

    # El tipo de la columna importa: si es entero, los ceros a la izquierda de
    # Antioquia (05) y Atlantico (08) ya se perdieron en la lectura.
    if pd.api.types.is_integer_dtype(mun[col_cod]) or pd.api.types.is_float_dtype(mun[col_cod]):
        print("  >>> ALERTA: la columna de codigo es NUMERICA. Los codigos 05xxx y")
        print("      08xxx pierden el cero inicial y quedan como 5xxx / 8xxx.")
        print("      Esta es la hipotesis principal del defecto.")

    mun["_dpto"] = mun[col_cod].astype(str).str.zfill(5).str[:2]
    sosp = mun[mun["_dpto"].isin(DPTOS_SOSPECHOSOS)]
    print(f"\n  Municipios de Antioquia + Atlantico: {len(sosp)} (esperado: 148)")

    if len(sosp) > 0:
        print(f"    geometrias nulas    : {sosp.geometry.isna().sum()}")
        print(f"    geometrias vacias   : {sosp.geometry.is_empty.sum()}")
        n_inval = int((~sosp.geometry.is_valid).sum())
        print(f"    geometrias invalidas: {n_inval}")
        if n_inval > 0:
            from shapely.validation import explain_validity
            print("\n    Motivos (primeros 5):")
            for _, fila in sosp[~sosp.geometry.is_valid].head(5).iterrows():
                print(f"      {fila[col_cod]}: {explain_validity(fila.geometry)[:80]}")
            print("\n    >>> El buffer(0) del script 01 es el sospechoso confirmado.")
        else:
            print("\n    Sin geometrias defectuosas en el original.")

    return mun


# =============================================================================
# ETAPA 2 — municipios_clean, salida del script 01
# =============================================================================

def etapa_2(raiz: Path):
    print("\n" + "=" * 78)
    print("ETAPA 2 — municipios_clean (salida de 01_prepare_boundaries.py)")
    print("=" * 78)

    if gpd is None:
        print("  geopandas no esta instalado. Se omite.")
        return None

    ruta = raiz / "data" / "interim" / "municipios_clean.gpkg"
    if not ruta.exists():
        print(f"  No existe {ruta}")
        return None

    mun = gpd.read_file(ruta, layer="municipios_clean")
    print(f"  Archivo   : {ruta.relative_to(raiz)}")
    print(f"  CRS       : {mun.crs}")
    print(f"  Registros : {len(mun)}")

    if len(mun) == 0:
        print("\n  >>> CAPA VACIA. Producida por _write_empty_municipios() del script 01,")
        print("      que se ejecuta cuando no encuentra data/raw/boundaries y NO falla.")
        print("      Toda extraccion posterior sobre esta capa devuelve cero, y el")
        print("      fillna(0) del script 03 lo convierte en 'deforestacion nula'.")
        print("      Corregir la ruta de limites y re-correr el pipeline completo.")
        return mun

    if "COD_DANE" not in mun.columns:
        print("  Falta COD_DANE. Columnas:", list(mun.columns))
        return mun

    print(f"  COD_DANE dtype: {mun['COD_DANE'].dtype}")
    mun["_dpto"] = mun["COD_DANE"].astype(str).str.zfill(5).str[:2]

    filas = []
    for dpto, grupo in mun.groupby("_dpto"):
        filas.append({
            "dpto": dpto,
            "n": len(grupo),
            "vacias": int(grupo.geometry.is_empty.sum()),
            "nulas": int(grupo.geometry.isna().sum()),
            "invalidas": int((~grupo.geometry.is_valid).sum()),
        })

    tabla = pd.DataFrame(filas)
    if tabla.empty or "vacias" not in tabla.columns:
        print("  Sin departamentos que resumir.")
        return mun

    problem = tabla[(tabla.vacias > 0) | (tabla.nulas > 0) | (tabla.invalidas > 0)]
    if len(problem) > 0:
        print("\n  Departamentos con geometrias defectuosas:")
        print(problem.to_string(index=False))
        print("\n  >>> DEFECTO LOCALIZADO EN LA ETAPA 2 (buffer(0) del script 01).")
    else:
        print("\n  Ninguna geometria vacia, nula o invalida.")

    print(f"\n  Departamentos presentes: {tabla['dpto'].nunique()} (esperado: 33)")
    for cod, nombre in NOMBRES_DPTO.items():
        n = int(tabla.loc[tabla.dpto == cod, "n"].sum()) if cod in set(tabla.dpto) else 0
        print(f"    {cod} {nombre}: {n} municipios")
        if n == 0:
            print(f"      >>> {nombre} NO APARECE en la capa. El defecto esta aguas arriba,")
            print("          en la lectura o el filtrado de los limites originales.")

    # Areas: una geometria formalmente valida pero degenerada tambien falla.
    try:
        proj = mun.to_crs("EPSG:3116")
        sosp = proj[proj["_dpto"].isin(DPTOS_SOSPECHOSOS)]
        resto = proj[~proj["_dpto"].isin(DPTOS_SOSPECHOSOS)]
        if len(sosp) > 0:
            print(f"\n  Area mediana Antioquia+Atlantico: {sosp.geometry.area.median()/1e6:,.1f} km2")
            print(f"  Area mediana resto del pais     : {resto.geometry.area.median()/1e6:,.1f} km2")
    except Exception as exc:
        print(f"  No se pudo calcular areas: {exc}")

    return mun


# =============================================================================
# ETAPA 3 — Cruce por COD_DANE en los intermedios
# =============================================================================

def etapa_3(raiz: Path) -> None:
    print("\n" + "=" * 78)
    print("ETAPA 3 — Cruce por COD_DANE en los archivos intermedios")
    print("=" * 78)

    objetivos = [
        raiz / "data" / "interim" / "loss_by_municipio_year.csv",
        raiz / "data" / "interim" / "loss_by_municipio.csv",
        raiz / "data" / "interim" / "baseline_forest.csv",
    ]

    for ruta in objetivos:
        print(f"\n  --- {ruta.name} ---")
        if not ruta.exists():
            print("      No existe.")
            continue

        df = pd.read_csv(ruta, dtype={"COD_DANE": str})
        print(f"      Filas: {len(df):,}")
        if len(df) == 0:
            print("      >>> ARCHIVO VACIO (producido por _write_empty_outputs()).")
            continue
        if "COD_DANE" not in df.columns:
            print(f"      Sin COD_DANE. Columnas: {list(df.columns)}")
            continue

        cods = df["COD_DANE"].astype(str)
        longitudes = cods.str.len().value_counts().sort_index()
        print(f"      Longitudes de COD_DANE: {dict(longitudes)}")
        if (longitudes.index < 5).any():
            print("      >>> ALERTA: hay codigos de menos de 5 digitos. Los ceros a la")
            print("          izquierda se perdieron; 05001 quedo como 5001 y el join")
            print("          contra el panel falla en silencio para Antioquia/Atlantico.")

        dptos = cods.str.zfill(5).str[:2]
        for cod, nombre in NOMBRES_DPTO.items():
            n = int((dptos == cod).sum())
            print(f"      {cod} {nombre}: {n} filas")

        col_valor = next((c for c in ["loss_area_ha", "baseline_forest"] if c in df.columns), None)
        if col_valor:
            for cod, nombre in NOMBRES_DPTO.items():
                sel = df.loc[dptos == cod, col_valor]
                if len(sel) > 0:
                    print(f"        {nombre}: suma {col_valor} = {sel.sum():,.1f}")


if __name__ == "__main__":
    raiz = encontrar_raiz()
    etapa_0(raiz)
    etapa_1(raiz)
    etapa_2(raiz)
    etapa_3(raiz)
    print("\n" + "=" * 78)
    print("Diagnostico terminado. Ninguna escritura realizada.")
    print("=" * 78)
