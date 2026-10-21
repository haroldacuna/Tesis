"""Cuantos proyectos REDD+ de Cercarbono quedaron con municipio en el tratamiento."""
import re
import unicodedata
import pandas as pd

def norm(s):
    s = unicodedata.normalize("NFKD", str(s)).encode("ascii", "ignore").decode().lower()
    return re.sub(r"[^a-z0-9]+", " ", s).strip()

cl = pd.read_csv("data/interim/clasificacion_sectorial.csv")
redd = cl[(cl.fuente == "cercarbono") & (cl.clase_redd_final == "REDD")].copy()
redd["k"] = redd.nombre_proyecto.map(norm)

ev = pd.read_csv("data/interim/eventos_carbono_consolidado.csv", dtype=str)
print("columnas de eventos:", list(ev.columns))
col_f = next(c for c in ev.columns if re.search("fuente|source", c, re.I))
col_n = next(c for c in ev.columns if re.search("nombre|name", c, re.I))
col_m = next(c for c in ev.columns if re.search("cod_dane", c, re.I))
e = ev[ev[col_f].str.contains("cercarbono", case=False, na=False)].copy()
e["k"] = e[col_n].map(norm)

m = redd.merge(e[["k", col_m]].drop_duplicates(), on="k", how="left")
ub = m.dropna(subset=[col_m]).groupby("nombre_proyecto")[col_m].apply(lambda x: ", ".join(sorted(set(x))))
sin = sorted(set(m.loc[m[col_m].isna(), "nombre_proyecto"]) - set(ub.index))
print(f"\nREDD+ de Cercarbono: {redd.nombre_proyecto.nunique()} | con municipio: {len(ub)} | sin municipio: {len(sin)}")
print("\nCon municipio:"); print(ub.to_string())
print("\nSin municipio:"); print("\n".join(sin))