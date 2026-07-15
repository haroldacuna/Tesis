import requests
import re

search_terms = [
    'proyecciones de poblacion municipio dane',
    'proyecciones y retroproyecciones de población',
    'poblacion municipio año dane',
    'dpmp a_o total poblacion',
]

candidates = {}
session = requests.Session()

for term in search_terms:
    r = session.get('https://www.datos.gov.co/api/search/views.json', params={'q': term, 'limit': 120}, timeout=60)
    if r.status_code != 200:
        continue
    for item in r.json().get('results', []):
        v = item.get('view', {})
        vid = v.get('id')
        if vid:
            candidates[vid] = v.get('name', '')

print('SEARCH_CANDIDATES', len(candidates))

hits = []
for vid, name in candidates.items():
    try:
        m = session.get(f'https://www.datos.gov.co/api/views/{vid}', timeout=30)
        if m.status_code != 200:
            continue
        meta = m.json()
        asset = meta.get('assetType', '')
        display = meta.get('displayType', '')
        viewtype = meta.get('viewType', '')
        cols = [c.get('fieldName', '') for c in meta.get('columns', []) if isinstance(c, dict)]
        lowcols = [c.lower() for c in cols]

        # Prefer real table-like datasets over federated hrefs.
        if asset in ('federated_href',) or display in ('federated',):
            continue

        has_year = any(c in lowcols for c in ['a_o', 'anio', 'ano', 'año', 'year'])
        has_total = any('total' == c or c.endswith('_total') or c == 'poblacion_nacional' for c in lowcols)
        has_muni = any(c in lowcols for c in ['municipio', 'mpio', 'cod_muni', 'cod_dane', 'dpmp'])

        score = int(has_year) + int(has_total) + int(has_muni)
        if score >= 2:
            hits.append((score, vid, name, asset, display, viewtype, cols[:20]))
    except Exception:
        pass

hits.sort(reverse=True)
print('HITS', len(hits))
for row in hits[:30]:
    score, vid, name, asset, display, viewtype, cols = row
    print(f'{vid} | score={score} | {asset}/{display}/{viewtype} | {name}')
    print('  cols=', ','.join(cols))
