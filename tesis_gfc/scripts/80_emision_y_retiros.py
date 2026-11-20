"""
80_emision_y_retiros.py

Respuesta al segundo jurado (pp. 3 y 13). Dos diagnosticos sobre los 44
proyectos REDD+ de Verra con ubicacion valida (sin CO2ROZO):

(a) ¿"No emitio" refleja la fecha de corte de los datos?
    Para las cohortes 2010-2016 se mide cuantos anios pasan entre el inicio del
    periodo de acreditacion y el registro y la primera emision. Con ese ritmo
    se calcula cuantos proyectos de 2017-2019 habrian emitido (o registrado) ya,
    dado el seguimiento que cada uno tiene hasta el corte de los datos:

        E[emisores tardios] = sum_j  F_temprana(seguimiento_j)

    donde F_temprana(s) es la proporcion de proyectos tempranos que emitieron en
    s anios o menos. Si lo observado queda muy por debajo, la ausencia de
    emision no se explica solo por el corte.

(b) ¿Que parte de los creditos se usa para la no causacion?
    Con las transacciones de OffsetsDB (CarbonPlan), se clasifican los retiros:
    cota inferior = la nota menciona explicitamente el impuesto al carbono o la
    no causacion (Ley 1819 de 2016, Decreto 926 de 2017, etc.); cota superior =
    ademas, neutralizaciones a nombre de empresas colombianas con NIT, la forma
    tipica de los certificados para la DIAN. Solo cubre Verra.

USO (PowerShell, desde tesis_gfc/):  python scripts\\80_emision_y_retiros.py
Insumos: data/interim/verra_con_metodologia.csv,
         data/interim/offsetsdb_credits_colombia.csv
Salidas: outputs/tablas/80_tiempo_emision.csv, 80_tiempo_emision_resumen.csv,
         80_retiros_no_causacion.csv
"""
from pathlib import Path
import sys

import numpy as np
import pandas as pd


def raiz() -> Path:
    d = Path.cwd().resolve()
    for p in (d, *d.parents):
        if (p / "data" / ".raiz_canonica").exists():
            return p
    sys.exit("No encuentro data/.raiz_canonica: corre desde tesis_gfc/")


R = raiz()
INT, TAB = R / "data" / "interim", R / "outputs" / "tablas"
TAB.mkdir(parents=True, exist_ok=True)
METODOS_REDD = r"VM0006|VM0007|VM0009|VM0015"

# --- Proyectos REDD+ de Verra -------------------------------------------------------
v = pd.read_csv(INT / "verra_con_metodologia.csv", encoding="utf-8-sig")
es_redd = (v["methodology"].astype(str).str.contains(METODOS_REDD, na=False)
           | v["afolu_activities"].astype(str).str.contains("REDD", na=False))
v = v[es_redd & ~v["projectName"].str.contains("CO2ROZO", case=False, na=False)].copy()
v["pid"] = "VCS" + v["projectId"].astype(str)
v["inicio"] = pd.to_datetime(v["projectStartDate"], errors="coerce", utc=True)
v["registro"] = pd.to_datetime(v["verraProjectRegistrationDate"].fillna(v["projectRegistrationDate"]),
                               errors="coerce", utc=True)
# Fecha de corte del registro: la actualizacion mas reciente de la descarga
corte_verra = pd.to_datetime(v["esUpdatedOn"], errors="coerce", utc=True).max().normalize()

cr = pd.read_csv(INT / "offsetsdb_credits_colombia.csv")
cr["fecha"] = pd.to_datetime(cr["transaction_date"], errors="coerce", utc=True)
corte_offsets = cr["fecha"].max()
emis = cr[cr["transaction_type"] == "issuance"].groupby("project_id")["fecha"].min().rename("primera_emision")
v = v.merge(emis, left_on="pid", right_index=True, how="left")

anios = lambda a, b: (a - b).dt.days / 365.25
v["anio_inicio"] = v["inicio"].dt.year
v["anios_a_registro"] = anios(v["registro"], v["inicio"])
v["anios_a_emision"] = anios(v["primera_emision"], v["inicio"])
# El seguimiento se mide hasta el corte mas temprano de las dos fuentes (conservador)
corte = min(corte_verra, corte_offsets)
v["seguimiento"] = anios(pd.Series(corte, index=v.index), v["inicio"])
v["cohorte"] = pd.cut(v["anio_inicio"], [2000, 2016, 2019, 2100], labels=["2010-2016", "2017-2019", "2020 o despues"])
print(f"REDD+ de Verra: {len(v)} | corte Verra {corte_verra.date()} | corte OffsetsDB {corte_offsets.date()}")

temp = v[v["cohorte"] == "2010-2016"]
tard = v[v["cohorte"] == "2017-2019"]
F = lambda t, n, s: np.array([(t.dropna() <= x).sum() / n for x in s])
resumen = pd.DataFrame([{
    "cohortes_tempranas": len(temp),
    "tempranas_con_emision": int(temp["primera_emision"].notna().sum()),
    "mediana_anios_a_emision_tempranas": temp["anios_a_emision"].median(),
    "mediana_anios_a_registro_tempranas": temp["anios_a_registro"].median(),
    "cohortes_tardias": len(tard),
    "seguimiento_min_tardias": tard["seguimiento"].min(),
    "seguimiento_max_tardias": tard["seguimiento"].max(),
    "emision_esperada_tardias": F(temp["anios_a_emision"], len(temp), tard["seguimiento"]).sum(),
    "emision_observada_tardias": int(tard["primera_emision"].notna().sum()),
    "registro_esperado_tardias": F(temp["anios_a_registro"], len(temp), tard["seguimiento"]).sum(),
    "registro_observado_tardias": int(tard["registro"].notna().sum()),
    "corte": corte.date(),
}])
print(resumen.T.round(1).to_string(header=False))
cols = ["pid", "projectName", "anio_inicio", "cohorte", "status", "methodology", "registro", "primera_emision",
        "anios_a_registro", "anios_a_emision", "seguimiento"]
v[cols].sort_values("anio_inicio").to_csv(TAB / "80_tiempo_emision.csv", index=False, encoding="utf-8")
resumen.to_csv(TAB / "80_tiempo_emision_resumen.csv", index=False, encoding="utf-8")

# --- Retiros y no causacion ---------------------------------------------------------
x = cr[cr["project_id"].isin(set(v["pid"]))]
emitidos = x.loc[x["transaction_type"] == "issuance", "quantity"].sum()
ret = x[x["transaction_type"] == "retirement"].copy()
txt = (ret["retirement_note"].fillna("") + " | " + ret["retirement_reason"].fillna("") + " | "
       + ret["retirement_beneficiary"].fillna("")).str.lower()
explicito = txt.str.contains(r"no causaci|no-causaci|impuesto nacional al carbono|impuesto al carbono|"
                             r"decretos? 926|ley 1819|carbon tax")
nit = txt.str.contains(r"\bnit\b|neutraliz")
sin_nota = txt.str.replace("|", "").str.strip() == ""
q = lambda m: ret.loc[m, "quantity"].sum()
ret_tot = ret["quantity"].sum()
tabla = pd.DataFrame([
    ("Creditos emitidos", emitidos, 100.0),
    ("Retirados", ret_tot, 100 * ret_tot / emitidos),
    ("Sin retirar", emitidos - ret_tot, 100 * (emitidos - ret_tot) / emitidos),
    ("Retirados con mencion explicita a la no causacion (cota inferior)", q(explicito), 100 * q(explicito) / emitidos),
    ("Explicitos o neutralizacion de empresa colombiana con NIT (cota superior)", q(explicito | nit),
     100 * q(explicito | nit) / emitidos),
    ("Retirados sin ninguna nota", q(sin_nota), 100 * q(sin_nota) / emitidos),
], columns=["concepto", "creditos", "pct_de_emitidos"])
tabla.to_csv(TAB / "80_retiros_no_causacion.csv", index=False, encoding="utf-8")
print(tabla.assign(creditos=lambda d: (d.creditos / 1e6).round(1), pct_de_emitidos=lambda d: d.pct_de_emitidos.round(0))
      .to_string(index=False))
