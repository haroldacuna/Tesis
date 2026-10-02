## =============================================================================
## comun_controles_limpios.R
##
## Regla de CONTROLES LIMPIOS (fijada el 27/09/2026, antes de estimar con ella):
##   Un municipio solo puede servir de control si no tiene NINGUN vinculo con
##   proyectos de carbono, en NINGUNA fuente ni en NINGUN anio:
##     - ninguna columna anio_inicio_tratamiento_* con valor > 0 (cubre todas
##       las definiciones: AFOLU, sin filtro sectorial, alta confianza, REDD+
##       original, sin ubicacion manual, depurada), y
##     - ninguna columna proyectos_* > 0 en ningun anio (registros Ecoregistry,
##       Gold Standard, ACR, Climate Action Data, Berkeley, OffsetsDB, CDM).
##   Justificacion: un municipio con un proyecto ARR, de energia, o un REDD+
##   excluido de la D1 (retirado, rechazado, de ubicacion ambigua) no es un
##   contrafactual de "sin proyecto de carbono".
##   La regla se aplica SOLO a los controles; los tratados no cambian.
##
## Uso desde cualquier script del pipeline:
##   source(file.path(RAIZ_PROYECTO, "scripts/comun_controles_limpios.R"))
##   cont <- municipios_con_proyecto(ruta_panel_tratamiento)
##   datos <- aplicar_controles_limpios(datos, cont, col_g = "G_dep")
##
## Escrito en R base (sin dplyr/readr) para que se pueda probar aislado.
## =============================================================================

municipios_con_proyecto <- function(ruta, verbose = TRUE) {
  if (!file.exists(ruta)) stop("No existe ", ruta)
  x <- utils::read.csv(ruta, colClasses = c(COD_DANE = "character"), check.names = FALSE)
  x$COD_DANE <- formatC(as.integer(x$COD_DANE), width = 5, flag = "0")

  cols_inicio <- grep("^anio_inicio_tratamiento_", names(x), value = TRUE)
  cols_proy   <- grep("^proyectos_", names(x), value = TRUE)
  if (!length(cols_inicio) && !length(cols_proy))
    stop("El panel no tiene columnas anio_inicio_tratamiento_* ni proyectos_*")

  positivo <- function(v) { v <- suppressWarnings(as.numeric(v)); !is.na(v) & v > 0 }
  marca <- lapply(c(cols_inicio, cols_proy), function(cl) tapply(positivo(x[[cl]]), x$COD_DANE, any))
  names(marca) <- c(cols_inicio, cols_proy)
  out <- data.frame(COD_DANE = names(marca[[1]]), stringsAsFactors = FALSE)
  for (cl in names(marca)) out[[cl]] <- as.logical(marca[[cl]][out$COD_DANE])
  out$contaminado <- Reduce(`|`, out[names(marca)])
  if (verbose) {
    cat(sprintf("[controles limpios] %d de %d municipios tienen algun vinculo con proyectos de carbono\n",
                sum(out$contaminado), nrow(out)))
  }
  out
}

## Saca de la muestra a los CONTROLES (G == 0) contaminados. Imprime cuantos y
## por que fuente, para que la restriccion quede declarada en el log.
aplicar_controles_limpios <- function(datos, cont, col_g = "G_dep", verbose = TRUE) {
  datos$COD_DANE <- formatC(as.integer(datos$COD_DANE), width = 5, flag = "0")
  g_mun <- tapply(datos[[col_g]], datos$COD_DANE, function(v) max(v, na.rm = TRUE))
  controles <- names(g_mun)[g_mun == 0]
  sucios <- intersect(controles, cont$COD_DANE[cont$contaminado])
  if (verbose) {
    cat(sprintf("[controles limpios] controles antes: %d | contaminados que salen: %d | quedan: %d\n",
                length(controles), length(sucios), length(controles) - length(sucios)))
    if (length(sucios)) {
      fuentes <- setdiff(names(cont), c("COD_DANE", "contaminado"))
      sub <- cont[cont$COD_DANE %in% sucios, fuentes, drop = FALSE]
      n <- colSums(sub)
      n <- n[n > 0]
      cat("  vinculo por columna (un municipio puede tener varios):\n")
      for (k in names(sort(n, decreasing = TRUE))) cat(sprintf("    %-58s %4d\n", k, n[[k]]))
    }
  }
  datos[!datos$COD_DANE %in% sucios, , drop = FALSE]
}
