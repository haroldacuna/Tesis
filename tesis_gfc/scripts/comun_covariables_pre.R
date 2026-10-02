## =============================================================================
## comun_covariables_pre.R
##
## Une al panel las covariables de deforestacion previa que exporta
## 02_matching_psm.R, REEMPLAZANDO cualquier version que ya traiga el panel.
##
## Por que: 03, 40 y 46 solo leian el CSV "si las columnas no estaban", pero
## panel_analisis_did.rds (armado por 01) ya las traia de una corrida vieja.
## Resultado: la correccion de defor_pre_tendencia del 02 (27/09/2026) nunca
## llegaba a las estimaciones. Detectado porque el 46 con covariables del 40
## reprodujo exactamente la variante V0 (tendencia vieja) del 47.
##
## Uso:
##   source(file.path(RAIZ_PROYECTO, "scripts/comun_covariables_pre.R"))
##   panel <- unir_covariables_pre(panel, file.path(DIR_OUT, "tablas/covariables_pre_municipio.csv"))
##
## Escrito en R base.
## =============================================================================

COLS_PRE_02 <- c("log_defor_pre_media", "defor_pre_tendencia", "log_baseline_forest")

unir_covariables_pre <- function(panel, ruta_csv, cols = COLS_PRE_02, verbose = TRUE) {
  if (!file.exists(ruta_csv)) stop("Falta ", ruta_csv, ". Corre antes 02_matching_psm.R.")
  cp <- utils::read.csv(ruta_csv, colClasses = c(COD_DANE = "character"))
  falt <- setdiff(cols, names(cp))
  if (length(falt)) stop(ruta_csv, " no tiene: ", paste(falt, collapse = ", "),
                         ". Corre la version actual de 02_matching_psm.R.")
  ## Firma de la version corregida del 02 (27/09/2026): solo ella exporta
  ## defor_pre_cambio_log. Sin esta columna el CSV trae la tendencia VIEJA y
  ## cualquier estimacion saldria con el error que se queria corregir.
  if (!"defor_pre_cambio_log" %in% names(cp))
    stop(ruta_csv, " fue generado por una version VIEJA de 02_matching_psm.R ",
         "(no tiene defor_pre_cambio_log; su defor_pre_tendencia es el cambio log ",
         "entre extremos, no la pendiente OLS). Reemplaza scripts/02_matching_psm.R ",
         "por la version corregida y vuelve a correrlo antes de estimar.")
  cp$COD_DANE <- formatC(as.integer(cp$COD_DANE), width = 5, flag = "0")
  if (anyDuplicated(cp$COD_DANE)) stop("COD_DANE duplicado en ", ruta_csv)
  tenia <- intersect(cols, names(panel))
  cod <- formatC(as.integer(panel$COD_DANE), width = 5, flag = "0")
  idx <- match(cod, cp$COD_DANE)
  if (verbose) {
    cambio <- vapply(tenia, function(cl) {
      viejo <- panel[[cl]]; nuevo <- cp[[cl]][idx]
      sum(xor(is.na(viejo), is.na(nuevo)) | (!is.na(viejo) & !is.na(nuevo) & abs(viejo - nuevo) > 1e-9))
    }, numeric(1))
    cat("[covariables pre] se toman de", basename(ruta_csv),
        "(version corregida del 02: tendencia = pendiente OLS; reemplazan las del panel).\n")
    if (!length(tenia)) cat("  el panel no traia estas columnas: se agregan desde el CSV\n")
    for (cl in tenia) cat(sprintf("  %-22s filas con valor distinto al del panel: %d de %d\n",
                                  cl, as.integer(cambio[[cl]]), nrow(panel)))
    sin_par <- sum(is.na(idx))
    if (sin_par) cat("  AVISO:", sin_par, "filas del panel sin municipio en el CSV (quedan NA)\n")
  }
  for (cl in cols) panel[[cl]] <- cp[[cl]][idx]
  panel
}
