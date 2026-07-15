import pandas as pd

df = pd.read_csv('data/final/panel_municipio_year_enriched.csv')
print('Columns:', list(df.columns))
print('\nFirst 10 rows con totales:')
print(df[['COD_DANE','year','proyectos_carbono_co','proyectos_berkeley_co','proyectos_carbono_total']].head(10))
print('\nStats:')
print(f'Rows: {len(df)}')
print(f'proyectos_carbono_total > 0: {(df["proyectos_carbono_total"] > 0).sum()}')
print(f'Max proyectos_carbono_total: {df["proyectos_carbono_total"].max()}')
print(f'\nSample rows where total > 0:')
print(df[df['proyectos_carbono_total'] > 0][['COD_DANE','year','proyectos_carbono_co','proyectos_berkeley_co','proyectos_carbono_total']].head(10))
