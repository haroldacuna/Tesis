#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
actualizar_imagenes.py
===========================
Inserta o reemplaza las tablas y figuras de cualquier capitulo con las versiones mas
recientes que genero scripts/70_tablas_figuras.R.

- Un parrafo con el texto [[IMG:nombre.png|ancho]] se reemplaza por la imagen.
- Una imagen ya insertada por este script (lleva la etiqueta IMG:nombre.png en
  su descripcion) se reemplaza por la version nueva, con el mismo ancho.
Si una imagen no existe, el marcador queda visible para completarla despues.

USO (desde tesis_gfc/):
  python scripts\\actualizar_imagenes.py Capitulo_6_Resultados.docx
  python scripts\\actualizar_imagenes.py Capitulo_6_Resultados.docx --dir outputs\\tesis
Requiere: pip install python-docx
"""
import re
import sys
from pathlib import Path

from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Inches, Emu

PAT = re.compile(r"^\[\[IMG:([^|\]]+)\|?([0-9.]*)\]\]$")


def buscar(nombre, base):
    for c in (base / "tablas" / nombre, base / "figuras" / nombre, base / nombre):
        if c.exists():
            return c
    return None


def poner(parrafo, ruta, ancho_emu, nombre):
    for r in list(parrafo.runs):
        r._r.getparent().remove(r._r)
    run = parrafo.add_run()
    run.add_picture(str(ruta), width=Emu(ancho_emu))
    pr = run._r.xpath(".//wp:docPr")[0]
    pr.set("descr", f"IMG:{nombre}"); pr.set("name", nombre)
    parrafo.alignment = WD_ALIGN_PARAGRAPH.CENTER
    parrafo.paragraph_format.keep_with_next = True


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    doc_in = Path(sys.argv[1])
    base = Path(sys.argv[sys.argv.index("--dir") + 1]) if "--dir" in sys.argv else Path("outputs/tesis")
    d = Document(doc_in)
    hechos, faltan = [], []
    for p in d.paragraphs:
        m = PAT.match(p.text.strip())
        if m:
            nombre, ancho = m.group(1), float(m.group(2) or 6.0)
            ruta = buscar(nombre, base)
            if ruta:
                poner(p, ruta, int(Inches(ancho)), nombre); hechos.append(nombre)
            else:
                faltan.append(nombre)
            continue
        for pr in p._p.xpath(".//wp:docPr"):
            descr = pr.get("descr", "")
            if descr.startswith("IMG:"):
                nombre = descr[4:]
                ext = p._p.xpath(".//wp:extent")
                ancho = int(ext[0].get("cx")) if ext else int(Inches(6))
                ruta = buscar(nombre, base)
                if ruta:
                    poner(p, ruta, ancho, nombre); hechos.append(nombre)
                break
    d.save(doc_in)
    print(f"Insertadas o actualizadas: {len(hechos)} -> {', '.join(hechos)}")
    if faltan:
        print(f"Sin archivo (el marcador queda visible): {', '.join(faltan)}")


if __name__ == "__main__":
    main()
