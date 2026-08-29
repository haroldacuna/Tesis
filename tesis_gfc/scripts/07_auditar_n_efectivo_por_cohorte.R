## =============================================================================
## 07_auditar_n_efectivo_por_cohorte.R
##
## Se confirmo que la cohorte 2012 tenia n=3 nominal pero n=1 EFECTIVO en el
## analisis de tasa_deforestacion, porque 2 de sus 3 municipios tienen
## baseline_forest_base = 0 (sin bosque que perder en el año base -> tasa
## indefinida -> NA en todos los años). Este script audita TODAS las
## cohortes para encontrar otros casos silenciosos como este, en vez de
## descubrirlos uno por uno.
##
## Requiere haber corrido 01_preparar_datos_did.R.
##
## USO
## ---
##   Rscript 07_auditar_n_efectivo_por_cohorte.R
## =============================================================================

library(dplyr)
library(readr)

panel <- readRDS("output/panel_analisis_did.rds")
dir.create("output/tablas", showWarnings = FALSE)

auditar <- function(panel, col_gname, etiqueta) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("AUDITORIA DE N EFECTIVO POR COHORTE --", etiqueta, "\n")
  cat(strrep("=", 70), "\n")

  municipios_unicos <- panel %>% distinct(COD_DANE, .data[[col_gname]], baseline_forest_base)

  resumen <- municipios_unicos %>%
    filter(.data[[col_gname]] > 0) %>%
    group_by(cohorte = .data[[col_gname]]) %>%
    summarise(
      n_nominal = n(),
      n_sin_bosque_base = sum(baseline_forest_base == 0, na.rm = TRUE),
      n_efectivo_tasa = sum(baseline_forest_base > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(pct_perdido = round(100 * n_sin_bosque_base / n_nominal, 0)) %>%
    arrange(cohorte)

  print(resumen, n = Inf)

  problematicas <- resumen %>% filter(n_efectivo_tasa <= 2 & n_nominal > n_efectivo_tasa)
  if (nrow(problematicas) > 0) {
    cat("\n*** Cohortes donde el n efectivo para tasa_deforestacion queda en 1-2",
        "(no confiar en su significancia si aparece en los resultados de tasa):\n")
    print(problematicas$cohorte)
  } else {
    cat("\nNinguna cohorte queda con n efectivo <= 2 para tasa_deforestacion.\n")
  }

  # Detalle: cuales municipios especificos se pierden, por si se quiere revisar
  perdidos <- municipios_unicos %>%
    filter(.data[[col_gname]] > 0, baseline_forest_base == 0) %>%
    left_join(
      panel %>% distinct(COD_DANE, NOMBRE_MPI, DPTO_CNMBR),
      by = "COD_DANE"
    )
  if (nrow(perdidos) > 0) {
    cat("\nMunicipios tratados excluidos del analisis de tasa (baseline_forest_base = 0):\n")
    print(perdidos %>% select(COD_DANE, NOMBRE_MPI, DPTO_CNMBR, cohorte = .data[[col_gname]]), n = Inf)
  }

  resumen
}

resumen_alta <- auditar(panel, "first_treat_alta", "Confianza alta")
resumen_todas <- auditar(panel, "first_treat_todas", "Todas las fuentes")

write_csv(resumen_alta, "output/tablas/n_efectivo_tasa_alta_confianza.csv")
write_csv(resumen_todas, "output/tablas/n_efectivo_tasa_todas_fuentes.csv")
cat("\n\nGuardado: output/tablas/n_efectivo_tasa_alta_confianza.csv\n")
cat("Guardado: output/tablas/n_efectivo_tasa_todas_fuentes.csv\n")
cat(
  "\n*** Antes de reportar CUALQUIER resultado de tasa_deforestacion por cohorte,",
  "cruza contra esta tabla: si n_efectivo_tasa es 1 o 2 para esa cohorte, no la",
  "reportes como hallazgo aunque salga 'significativa' -- es el mismo problema",
  "que encontramos en la cohorte 2012. ***\n"
)
