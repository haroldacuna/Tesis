from __future__ import annotations

import argparse
import csv
import re
import subprocess
import time
from datetime import datetime
from pathlib import Path


PROGRESS_RE = re.compile(r"Processing municipality\s+(\d+)/(\d+)")


def _latest_progress_line(log_path: Path) -> tuple[str, str, str]:
    if not log_path.exists():
        return "", "", ""

    latest = ""
    current = ""
    total = ""

    try:
        with log_path.open("r", encoding="utf-8", errors="replace") as f:
            for line in f:
                line = line.rstrip("\n")
                m = PROGRESS_RE.search(line)
                if m:
                    latest = line
                    current = m.group(1)
                    total = m.group(2)
    except OSError:
        return "", "", ""

    return latest, current, total


def _checkpoint_rows(checkpoint_path: Path) -> str:
    if not checkpoint_path.exists():
        return ""

    try:
        with checkpoint_path.open("r", encoding="utf-8-sig", errors="replace") as f:
            # Header + data rows.
            return str(max(sum(1 for _ in f) - 1, 0))
    except OSError:
        return ""


def _active_process_count(process_pattern: str) -> int:
    # Use PowerShell process query to avoid extra dependencies.
    ps_cmd = (
        "Get-CimInstance Win32_Process | "
        "Where-Object { $_.Name -match '^python(\\.exe)?$' -and $_.CommandLine -match '"
        + process_pattern.replace("'", "''")
        + "' } | Measure-Object | Select-Object -ExpandProperty Count"
    )

    try:
        proc = subprocess.run(
            ["powershell", "-NoProfile", "-Command", ps_cmd],
            capture_output=True,
            text=True,
            check=False,
        )
        out = (proc.stdout or "").strip()
        return int(out) if out.isdigit() else 0
    except Exception:
        return 0


def _ensure_header(out_csv: Path) -> None:
    out_csv.parent.mkdir(parents=True, exist_ok=True)
    if out_csv.exists():
        return

    with out_csv.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["timestamp", "status", "current", "total", "checkpoint_rows", "latest_line"])


def _append_snapshot(out_csv: Path, status: str, current: str, total: str, checkpoint_rows: str, latest: str) -> None:
    with out_csv.open("a", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow([
            datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            status,
            current,
            total,
            checkpoint_rows,
            latest,
        ])


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Periodically append pipeline progress snapshots to a CSV log.")
    parser.add_argument("--log", type=Path, default=Path("logs/06_full_external_refresh_v4.log"))
    parser.add_argument("--checkpoint", type=Path, default=Path("data/interim/panel_external_checkpoint.csv"))
    parser.add_argument("--out", type=Path, default=Path("logs/progress_monitor.csv"))
    parser.add_argument("--interval-sec", type=int, default=60)
    parser.add_argument("--process-pattern", type=str, default=r"06_enrich_panel_external\.py")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    _ensure_header(args.out)

    while True:
        latest, current, total = _latest_progress_line(args.log)
        rows = _checkpoint_rows(args.checkpoint)
        active = _active_process_count(args.process_pattern)
        status = "RUNNING" if active > 0 else "STOPPED"

        _append_snapshot(args.out, status, current, total, rows, latest)
        print(f"Snapshot saved: status={status}, progress={current}/{total}, rows={rows}")

        if status == "STOPPED":
            break

        time.sleep(max(args.interval_sec, 5))


if __name__ == "__main__":
    main()
