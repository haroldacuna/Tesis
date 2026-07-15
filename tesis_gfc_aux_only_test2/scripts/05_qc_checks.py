from pathlib import Path

import pandas as pd


FINAL_PANEL_FILE = Path("data/final/panel_municipio_year.csv")
QC_REPORT_FILE = Path("outputs/tables_figures/qc_report.txt")
START_YEAR = 2001
END_YEAR = 2024


def main() -> None:
    if not FINAL_PANEL_FILE.exists():
        report_lines = [
            "QC Report - panel_municipio_year",
            "=" * 40,
            f"Warning: Missing final panel file: {FINAL_PANEL_FILE}",
            "QC skipped due to missing inputs.",
        ]
        QC_REPORT_FILE.parent.mkdir(parents=True, exist_ok=True)
        QC_REPORT_FILE.write_text("\n".join(report_lines), encoding="utf-8")
        for line in report_lines:
            print(line)
        return

    df = pd.read_csv(FINAL_PANEL_FILE)
    required_cols = {
        "COD_DANE",
        "NOMBRE_MPI",
        "DPTO_CNMBR",
        "year",
        "loss_pixels",
        "loss_area_ha",
        "baseline_forest",
    }
    missing = required_cols - set(df.columns)
    if missing:
        raise ValueError(f"Final panel missing required columns: {sorted(missing)}")

    checks: list[str] = []
    failures: list[str] = []

    if df.empty:
        report_lines = [
            "QC Report - panel_municipio_year",
            "=" * 40,
            "Rows: 0",
            "Warning: final panel is empty.",
            "QC completed in degraded mode.",
        ]
        QC_REPORT_FILE.parent.mkdir(parents=True, exist_ok=True)
        QC_REPORT_FILE.write_text("\n".join(report_lines), encoding="utf-8")
        for line in report_lines:
            print(line)
        return

    n_rows = len(df)
    n_municipios = df["COD_DANE"].astype(str).nunique()
    checks.append(f"Rows: {n_rows}")
    checks.append(f"Municipalities: {n_municipios}")

    duplicate_n = int(df.duplicated(["COD_DANE", "year"]).sum())
    checks.append(f"Duplicate (COD_DANE, year): {duplicate_n}")
    if duplicate_n > 0:
        failures.append("Found duplicate municipality-year rows.")

    year_min = int(df["year"].min())
    year_max = int(df["year"].max())
    checks.append(f"Year range: {year_min}-{year_max}")
    if year_min != START_YEAR or year_max != END_YEAR:
        failures.append(f"Unexpected year range (expected {START_YEAR}-{END_YEAR}).")

    expected_rows = n_municipios * (END_YEAR - START_YEAR + 1)
    checks.append(f"Expected rows if balanced: {expected_rows}")
    if n_rows != expected_rows:
        failures.append("Panel is not balanced over municipality-year.")

    nulls = df[["loss_pixels", "loss_area_ha", "baseline_forest"]].isna().sum().to_dict()
    checks.append(f"Null counts: {nulls}")
    if any(value > 0 for value in nulls.values()):
        failures.append("Found nulls in core numeric columns.")

    negative_count = int((df[["loss_pixels", "loss_area_ha", "baseline_forest"]] < 0).sum().sum())
    checks.append(f"Negative numeric values: {negative_count}")
    if negative_count > 0:
        failures.append("Found negative values in core numeric columns.")

    report_lines = ["QC Report - panel_municipio_year", "=" * 40, *checks]
    if failures:
        report_lines.extend(["", "FAILURES:"])
        report_lines.extend(f"- {item}" for item in failures)
    else:
        report_lines.extend(["", "All QC checks passed."])

    QC_REPORT_FILE.parent.mkdir(parents=True, exist_ok=True)
    QC_REPORT_FILE.write_text("\n".join(report_lines), encoding="utf-8")

    for line in report_lines:
        print(line)

    if failures:
        raise ValueError("QC checks failed. Review outputs/tables_figures/qc_report.txt")


if __name__ == "__main__":
    main()
