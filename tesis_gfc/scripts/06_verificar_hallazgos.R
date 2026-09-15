## =============================================================================
## 06_verificar_hallazgos.R
##
## Dos verificaciones puntuales sobre los resultados de tasa_deforestacion:
##
##   A) La cohorte 2012 salio con un error estandar sospechosamente chico
##      (un orden de magnitud menor que cualquier otra cohorte, con solo
##      n=3 municipios tratados) -- hay que mirar si es una senal real o un
##      caso limite de estimacion.
##
##   B) Las cohortes 2002 (y 2008 en "todas las fuentes") son las mas
##      tempranas del panel y podrian caer antes o en el mismo anio que se
##      usa como base para las covariables de conflicto tardias (2003) --
##      si es asi, esas covariables "pre-tratamiento" en realidad no lo son.
##
## Requiere haber corrido 01_preparar_datos_did.R y 05_did_tasa_deforestacion.R.
##
## USO
## ---
##   Rscript 06_verificar_hallazgos.R
## =============================================================================

library(dplyr)
library(did)

panel <- readRDS("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/panel_analisis_did.rds")
resultados_tasa <- readRDS("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/resultados_did_tasa.rds")

ANIO_BASE_CONFLICTO_TARDIO <- 2003L  # debe coincidir con el usado en 01_preparar_datos_did.R

## =============================================================================
## A) Diagnostico de la cohorte 2012
## =============================================================================

diagnosticar_cohorte <- function(resultado, col_gname, cohorte_objetivo, etiqueta) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("A) DIAGNOSTICO COHORTE", cohorte_objetivo, "--", etiqueta, "\n")
  cat(strrep("=", 70), "\n")

  # 1. Municipios que componen esta cohorte, y su serie de tasa_deforestacion
  municipios_cohorte <- panel %>%
    filter(.data[[col_gname]] == cohorte_objetivo) %>%
    distinct(COD_DANE) %>%
    pull(COD_DANE)

  cat("Municipios en la cohorte", cohorte_objetivo, ":", length(municipios_cohorte),
      "-", paste(municipios_cohorte, collapse = ", "), "\n\n")

  serie <- panel %>%
    filter(COD_DANE %in% municipios_cohorte) %>%
    select(COD_DANE, year, tasa_deforestacion, loss_area_ha, baseline_forest_base) %>%
    arrange(COD_DANE, year)

  cat("Serie de tasa_deforestacion de estos municipios (revisar si son sospechosamente\n")
  cat("identicas/constantes entre si o en el tiempo -- eso explicaria un SE casi cero):\n")
  print(serie, n = Inf)

  varianza_por_anio <- serie %>%
    group_by(year) %>%
    summarise(var_tasa = var(tasa_deforestacion, na.rm = TRUE), n_validos = sum(!is.na(tasa_deforestacion)))
  cat("\nVarianza de tasa_deforestacion ENTRE los municipios de la cohorte, por año:\n")
  cat("(si sale ~0 en varios años, esa es la causa directa del SE chico)\n")
  print(varianza_por_anio, n = Inf)

  # 2. Celdas (grupo, tiempo) especificas de att_gt() para esta cohorte
  att_gt_obj <- resultado$att_gt
  idx <- which(att_gt_obj$group == cohorte_objetivo)
  cat("\nCeldas (grupo, tiempo) individuales de att_gt() para esta cohorte:\n")
  tabla_celdas <- data.frame(
    grupo = att_gt_obj$group[idx],
    tiempo = att_gt_obj$t[idx],
    att = att_gt_obj$att[idx],
    se = att_gt_obj$se[idx]
  )
  print(tabla_celdas, row.names = FALSE)

  # 3. Cuantas unidades "no tratadas todavia" estaban disponibles como
  #    comparacion en los periodos relevantes -- un pool de comparacion muy
  #    chico tambien puede producir varianza degenerada en el bootstrap.
  for (t_periodo in unique(tabla_celdas$tiempo)) {
    n_comparacion <- panel %>%
      filter(year == t_periodo) %>%
      filter(.data[[col_gname]] == 0 | .data[[col_gname]] > t_periodo) %>%
      distinct(COD_DANE) %>%
      nrow()
    cat("  Periodo", t_periodo, "-> unidades disponibles como 'no tratadas todavia':", n_comparacion, "\n")
  }
}

diagnosticar_cohorte(resultados_tasa$alta, "first_treat_alta", 2012, "Confianza alta")
diagnosticar_cohorte(resultados_tasa$todas, "first_treat_todas", 2012, "Todas las fuentes")

## =============================================================================
## B) Cohortes tempranas: ¿su año de tratamiento cae antes o en el mismo año
##    que el año base tardío (2003) usado para homicidios/secuestros/
##    acc_subversivas? Si sí, esas covariables no son estrictamente
##    pre-tratamiento para esos municipios.
## =============================================================================

cat("\n\n", strrep("=", 70), "\n", sep = "")
cat("B) COHORTES TEMPRANAS -- riesgo de covariables no pre-tratamiento\n")
cat(strrep("=", 70), "\n")

verificar_cohortes_tempranas <- function(panel, col_gname, etiqueta) {
  cat("\n---", etiqueta, "---\n")
  tempranas <- panel %>%
    filter(.data[[col_gname]] > 0, .data[[col_gname]] <= ANIO_BASE_CONFLICTO_TARDIO) %>%
    distinct(COD_DANE, NOMBRE_MPI, DPTO_CNMBR, .data[[col_gname]])

  if (nrow(tempranas) == 0) {
    cat("Ninguna cohorte cae en o antes de", ANIO_BASE_CONFLICTO_TARDIO, "-- no hay riesgo para este supuesto.\n")
    return(invisible(NULL))
  }

  cat(nrow(tempranas), "municipio(s) con cohorte <=", ANIO_BASE_CONFLICTO_TARDIO, ":\n")
  print(tempranas, n = Inf)

  cat(
    "\nPara estos municipios, homicidios_base/secuestros_base/acc_subversivas_base",
    "se midieron en", ANIO_BASE_CONFLICTO_TARDIO,
    "-- si su tratamiento empezo ANTES de ese año, esas covariables ya podrian",
    "estar 'contaminadas' por el tratamiento (dejan de ser estrictamente",
    "pre-tratamiento). baseline_forest_base/temp_media_c_base/etc. (año base 2001)",
    "no tienen este problema salvo que la cohorte sea exactamente 2001.\n"
  )
}

verificar_cohortes_tempranas(panel, "first_treat_alta", "Confianza alta")
verificar_cohortes_tempranas(panel, "first_treat_todas", "Todas las fuentes")

cat(
  "\n\n*** Si aparecen municipios en la seccion B, una forma simple de sensibilidad:",
  "vuelve a correr 03/05 EXCLUYENDO esos municipios del todo (no como control, no",
  "como tratado) y compara si el ATT de la cohorte temprana cambia mucho -- si el",
  "resultado es parecido con y sin ellos, el problema no es grave en la practica. ***\n"
)
