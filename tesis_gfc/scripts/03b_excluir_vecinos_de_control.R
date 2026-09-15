## =============================================================================
## 03b_excluir_vecinos_de_control.R
##
## PUNTO 4 de la revision: robustez frente a la violacion de SUTVA.
##
## La Tabla 6.6 (10_spillovers_espaciales.R) encuentra un efecto POSITIVO
## sobre municipios nunca tratados directamente que tienen un vecino tratado
## (posible fuga). Si eso es cierto, usar esos municipios como parte del
## grupo "aun no tratado" en la especificacion PRINCIPAL (03_did_callaway_
## santanna.R) viola el supuesto de no interferencia entre unidades y sesga
## el ATT principal hacia valores mas negativos de lo que corresponde.
##
## Este script reestima el ATT principal (sin covariables y doblemente
## robusto, para las dos definiciones de tratamiento) EXCLUYENDO del panel
## a los municipios nunca tratados directamente que en algun momento del
## periodo tuvieron un vecino tratado (contiguidad tipo "reina"). Los
## municipios tratados se conservan siempre, sean o no vecinos de otro
## tratado.
##
## Reutiliza la tabla de vecindad ya calculada por 10_spillovers_espaciales.R
## (output/resultados_spillover.rds) - no vuelve a leer el shapefile.
##
## REQUIERE haber corrido, en este orden:
##   01_preparar_datos_did.R
##   03_did_callaway_santanna.R   (para poder comparar contra la Tabla 6.1)
##   10_spillovers_espaciales.R   (genera output/resultados_spillover.rds)
##
## USO
## ---
##   Rscript 03b_excluir_vecinos_de_control.R
##
## SALIDA
## ------
##   output/tablas/robustez_excluyendo_vecinos_tratados.csv
##
## COMO LEER EL RESULTADO
## -----------------------
##   Compara cada fila contra la fila equivalente de
##   output/tablas/resumen_robustez_att.csv (Tabla 6.1 original):
##   - Si el ATT cambia menos de ~15% (mismo umbral que ya usas en la
##     Tabla 6.9 para la sensibilidad de cohortes tempranas), el sesgo de
##     SUTVA no es sustantivo: puedes decirlo asi en la tesis, con este
##     resultado como evidencia directa.
##   - Si cambia mas de eso, es un hallazgo que hay que reportar como
##     limitacion reconocida del ATT principal, no ocultar.
##   En cualquiera de los dos casos, el resultado fortalece la tesis frente
##   a la objecion de un jurado, porque muestra que la revisaste.
## =============================================================================

library(readr)
library(dplyr)
library(did)

OUTPUT_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs"
PANEL_ANALISIS <- file.path(OUTPUT_ROOT, "panel_analisis_did.rds")
RUTA_SPILLOVER <- file.path(OUTPUT_ROOT, "resultados_spillover.rds")
RUTA_ORIGINAL  <- file.path(OUTPUT_ROOT, "tablas/resumen_robustez_att.csv")
SEMILLA <- 20260824

if (!file.exists(RUTA_SPILLOVER)) {
  stop("No encuentro ", RUTA_SPILLOVER, " - corre primero 10_spillovers_espaciales.R.")
}
if (!file.exists(PANEL_ANALISIS)) {
  stop("No encuentro ", PANEL_ANALISIS, " - corre primero 01_preparar_datos_did.R.")
}

panel <- readRDS(PANEL_ANALISIS)
spillover <- readRDS(RUTA_SPILLOVER)

## Misma lista parsimoniosa de covariables que 03_did_callaway_santanna.R,
## para que la comparacion sea entre iguales.
COVARIABLES_XFORMLA <- ~ baseline_forest_base + temp_media_c_base +
  disbogota_base + H_coca_base + homicidios_base

## =============================================================================
## 1) Identificar, para cada definicion, los municipios a excluir del panel:
##    nunca tratados directamente Y alguna vez vecinos de un tratado.
## =============================================================================

construir_exclusion <- function(panel, col_first_treat, tabla_vecinos) {
  primer_trat <- panel %>%
    distinct(COD_DANE, .data[[col_first_treat]]) %>%
    rename(first_treat = all_of(col_first_treat))

  primer_trat %>%
    left_join(tabla_vecinos, by = "COD_DANE") %>%
    filter(first_treat == 0, !is.na(first_treat_vecino), first_treat_vecino > 0) %>%
    pull(COD_DANE)
}

excluir_alta  <- construir_exclusion(panel, "first_treat_alta",  spillover$alta$tabla_vecinos)
excluir_todas <- construir_exclusion(panel, "first_treat_todas", spillover$todas$tabla_vecinos)

cat(strrep("=", 70), "\n")
cat("MUNICIPIOS EXCLUIDOS DEL PANEL (nunca tratados + vecinos de tratado)\n")
cat(strrep("=", 70), "\n")
cat("Confianza alta:   ", length(excluir_alta), "de",
    panel %>% distinct(COD_DANE) %>% nrow(), "municipios del panel\n")
cat("Todas las fuentes:", length(excluir_todas), "de",
    panel %>% distinct(COD_DANE) %>% nrow(), "municipios del panel\n")

## =============================================================================
## 2) Reestimar att_gt() sobre el panel filtrado (misma logica que
##    03_did_callaway_santanna.R, reproducida aqui para no depender de
##    correr ese script completo de nuevo).
## =============================================================================

correr_did_filtrado <- function(data, col_gname, xformla, outcome, etiqueta) {
  data_modelo <- data %>% rename(.gname = all_of(col_gname))

  set.seed(SEMILLA)
  att_gt_out <- tryCatch({
    att_gt(
      yname = outcome, tname = "year", idname = "id_num", gname = ".gname",
      xformla = xformla, data = data_modelo, control_group = "notyettreated",
      est_method = "dr", bstrap = TRUE, cband = TRUE, clustervars = "id_num"
    )
  }, error = function(e) {
    cat("ERROR en", etiqueta, ":", conditionMessage(e), "\n")
    NULL
  })

  if (is.null(att_gt_out)) return(NULL)
  agg_simple <- aggte(att_gt_out, type = "simple", na.rm = TRUE)
  list(att = agg_simple$overall.att, se = agg_simple$overall.se, etiqueta = etiqueta)
}

resultados_filtrados <- list()

especificaciones <- list(
  list(def = "alta",  col = "first_treat_alta",  excluir = excluir_alta,  nombre = "Confianza alta"),
  list(def = "todas", col = "first_treat_todas", excluir = excluir_todas, nombre = "Todas las fuentes")
)

for (esp in especificaciones) {
  panel_filtrado <- panel %>% filter(!COD_DANE %in% esp$excluir)

  cat("\n", strrep("-", 70), "\n", sep = "")
  cat(esp$nombre, "- panel filtrado:",
      panel_filtrado %>% distinct(COD_DANE) %>% nrow(), "municipios\n")

  resultados_filtrados[[paste0(esp$def, "_sin_cov")]] <- correr_did_filtrado(
    panel_filtrado, esp$col, ~1, "loss_area_ha",
    paste0(esp$nombre, " - sin covariables - excl. vecinos de tratados")
  )

  resultados_filtrados[[paste0(esp$def, "_dr")]] <- correr_did_filtrado(
    panel_filtrado, esp$col, COVARIABLES_XFORMLA, "loss_area_ha",
    paste0(esp$nombre, " - doblemente robusto - excl. vecinos de tratados")
  )
}

## =============================================================================
## 3) Tabla comparativa
## =============================================================================

tabla_nueva <- do.call(rbind, lapply(names(resultados_filtrados), function(nombre) {
  r <- resultados_filtrados[[nombre]]
  if (is.null(r)) return(NULL)
  data.frame(
    especificacion = r$etiqueta,
    att_excluyendo_vecinos = round(r$att, 3),
    se_excluyendo_vecinos = round(r$se, 3)
  )
}))

cat("\n\n", strrep("=", 70), "\n", sep = "")
cat("RESULTADO: ATT EXCLUYENDO DEL CONTROL A VECINOS DE TRATADOS\n")
cat(strrep("=", 70), "\n")
print(tabla_nueva, row.names = FALSE)

if (file.exists(RUTA_ORIGINAL)) {
  original <- read_csv(RUTA_ORIGINAL, show_col_types = FALSE)
  cat("\nTABLA 6.1 ORIGINAL (para comparar especificacion por especificacion,\n")
  cat("mismo nombre sin el sufijo '- excl. vecinos de tratados'):\n")
  print(original, row.names = FALSE)
}

dir.create(file.path(OUTPUT_ROOT, "tablas"), showWarnings = FALSE, recursive = TRUE)
write_csv(tabla_nueva, file.path(OUTPUT_ROOT, "tablas/robustez_excluyendo_vecinos_tratados.csv"))
cat("\nGuardado: output/tablas/robustez_excluyendo_vecinos_tratados.csv\n")
