## =============================================================================
## 09_tablas_regresion.R
##
## Tablas de regresion en formato academico estandar (estimador, error
## estandar entre parentesis, estrellas de significancia, columnas = modelos)
## a partir de los resultados de did::att_gt()/aggte() -- que no son objetos
## de regresion clasicos, pero modelsummary::get_estimates() sabe leerlos
## nativamente (objetos de clase AGGTEobj).
##
## Incluye ademas una comparacion complementaria: una regresion de efectos
## fijos bidireccionales (TWFE) clasica via fixest::feols(), que SI es un
## objeto de regresion en el sentido tradicional (con R^2, etc.) y sirve
## como punto de comparacion frente al estimador de Callaway y Sant'Anna --
## la literatura reciente (Goodman-Bacon 2021; de Chaisemartin y
## D'Haultfoeuille 2020) muestra que TWFE puede estar sesgado con
## tratamiento escalonado, asi que la comparacion es en si misma
## informativa, no solo decorativa.
##
## Requiere haber corrido 01_preparar_datos_did.R.
##
## USO
## ---
##   Rscript 09_tablas_regresion.R
## =============================================================================

library(dplyr)
library(did)
library(fixest)
library(modelsummary)

panel <- readRDS("output/panel_analisis_did.rds")
dir.create("output/tablas", showWarnings = FALSE)

COVARIABLES_XFORMLA <- ~ baseline_forest_base + temp_media_c_base +
  disbogota_base + H_coca_base + homicidios_base

## =============================================================================
## 1. Tabla de regresion: las 6 especificaciones de att_gt(), ATT simple
## =============================================================================

correr_simple <- function(data, col_gname, xformla) {
  data_modelo <- data %>% rename(.gname = all_of(col_gname))
  set.seed(20260824)
  att_gt_out <- att_gt(
    yname = "loss_area_ha", tname = "year", idname = "id_num", gname = ".gname",
    xformla = xformla, data = data_modelo, control_group = "notyettreated",
    est_method = "dr", bstrap = TRUE, cband = TRUE, clustervars = "id_num"
  )
  aggte(att_gt_out, type = "simple", na.rm = TRUE)
}

cat("Estimando las 6 especificaciones (puede tardar varios minutos)...\n\n")

m_alta_sin <- correr_simple(panel, "first_treat_alta", ~1)
m_alta_dr <- correr_simple(panel, "first_treat_alta", COVARIABLES_XFORMLA)
m_todas_sin <- correr_simple(panel, "first_treat_todas", ~1)
m_todas_dr <- correr_simple(panel, "first_treat_todas", COVARIABLES_XFORMLA)

modelos_did <- list(
  "Alta (sin cov.)" = m_alta_sin,
  "Alta (DR)" = m_alta_dr,
  "Todas (sin cov.)" = m_todas_sin,
  "Todas (DR)" = m_todas_dr
)

## NOTA: modelsummary() puede extraer AGGTEobj automaticamente SOLO en
## versiones recientes (posteriores a la publicada en CRAN al momento de
## escribir esto) -- si tienes la version de CRAN, fallara con un error de
## "could not extract...". Por eso esta tabla se construye a mano, sin
## depender de esa funcionalidad: es mas robusto y no depende de version.
estrellas_de <- function(est, se) {
  z <- est / se
  p <- 2 * pnorm(-abs(z))
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
}

extraer_fila <- function(agg) {
  data.frame(
    estimador = sprintf("%.3f%s", agg$overall.att, estrellas_de(agg$overall.att, agg$overall.se)),
    error_estandar = sprintf("(%.3f)", agg$overall.se)
  )
}

filas <- lapply(modelos_did, extraer_fila)
tabla_did <- data.frame(
  fila = c("ATT", "Error estándar"),
  do.call(cbind, lapply(filas, function(f) c(f$estimador, f$error_estandar)))
)
names(tabla_did) <- c("", names(modelos_did))

cat("\n=== Tabla de regresion -- especificaciones did::att_gt() ===\n")
print(tabla_did, row.names = FALSE)
cat("\nNotas: *** p<0.01, ** p<0.05, * p<0.1. Errores estandar entre parentesis,\n")
cat("clusterizados por municipio. N = 1,122 municipios, panel 2001-2024.\n")

write.csv(tabla_did, "output/tablas/tabla_regresion_did.csv", row.names = FALSE)
cat("\nGuardado: output/tablas/tabla_regresion_did.csv\n")
cat("(la exportacion directa a .docx de modelsummary requiere una version de\n")
cat("'tinytable' con soporte de render a Word que no esta disponible en este\n")
cat("entorno -- usa el CSV para construir la tabla en Word con el mismo\n")
cat("patron de docx-js ya usado para los otros documentos de la tesis)\n")

## =============================================================================
## 2. Comparacion complementaria: TWFE clasico (feols) vs. Callaway-Sant'Anna
##
## Especificacion: dummy de tratamiento (1 si el municipio ya esta tratado
## en ese anio, segun cada definicion) + efectos fijos de municipio y de
## anio + errores estandar clusterizados por municipio. Es la regresion
## "ingenua" con la que se compara el estimador corregido.
## =============================================================================

panel_twfe <- panel %>%
  mutate(
    tratado_alta = as.integer(first_treat_alta > 0 & year >= first_treat_alta),
    tratado_todas = as.integer(first_treat_todas > 0 & year >= first_treat_todas)
  )

cat("\n\nEstimando TWFE clasico (feols) para comparar...\n")

twfe_alta <- feols(loss_area_ha ~ tratado_alta | COD_DANE + year, data = panel_twfe, cluster = ~COD_DANE)
twfe_todas <- feols(loss_area_ha ~ tratado_todas | COD_DANE + year, data = panel_twfe, cluster = ~COD_DANE)

modelos_twfe <- list("TWFE - Alta confianza" = twfe_alta, "TWFE - Todas las fuentes" = twfe_todas)

cat("\n=== Tabla TWFE (comparacion) ===\n")
etable(twfe_alta, twfe_todas, cluster = ~COD_DANE)

tabla_twfe <- modelsummary(
  modelos_twfe,
  output = "data.frame",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "r2.within")
)
write.csv(tabla_twfe, "output/tablas/tabla_regresion_twfe.csv", row.names = FALSE)
cat("\nGuardado: output/tablas/tabla_regresion_twfe.csv\n")

cat(
  "\n\n*** Interpretacion de la comparacion: si el coeficiente de TWFE y el ATT",
  "de Callaway-Sant'Anna difieren MUCHO en magnitud o signo, es evidencia de que",
  "el sesgo de TWFE bajo tratamiento escalonado (Goodman-Bacon 2021) esta",
  "afectando la estimacion ingenua -- en ese caso, reporta el estimador",
  "corregido (Callaway-Sant'Anna) como principal y menciona la discrepancia",
  "con TWFE como motivacion metodologica explicita para usar el estimador",
  "moderno. Si son parecidos, es una prueba de robustez adicional a tu favor. ***\n"
)
