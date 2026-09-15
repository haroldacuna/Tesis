## =============================================================================
## 04c_inspeccion_cohortes_2006_2025_y_columna_region.R
##
## Dos tareas puntuales de seguimiento al diagnostico de la Tabla 6.3:
##
##   1) Identificar el/los municipio(s) de las cohortes 2006 y 2025 en cada
##      definicion, y revisar sus covariables de linea base, para saber
##      EXACTAMENTE por que salen NA (util antes de escribir la nota al pie
##      de la Tabla 6.3 y, sobre todo, antes de decidir que hacer con la
##      cohorte 2025, que cae fuera de la ventana de observacion del panel
##      2001-2024 declarada en la Tabla 4.1).
##
##   2) Listar las columnas del panel para encontrar la que efectivamente
##      contiene la region DANE (el bloque 3 del script anterior no
##      encontro "region_dane").
##
## REQUIERE: output/panel_analisis_did.rds
##
## USO
## ---
##   Rscript 04c_inspeccion_cohortes_2006_2025_y_columna_region.R
## =============================================================================

library(dplyr)

OUTPUT_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs"
panel <- readRDS(file.path(OUTPUT_ROOT, "panel_analisis_did.rds"))

COVARIABLES <- c("baseline_forest_base", "temp_media_c_base",
                  "disbogota_base", "H_coca_base", "homicidios_base")

## -----------------------------------------------------------------------
## 1) Inspeccion de las cohortes 2006 y 2025
## -----------------------------------------------------------------------

inspeccionar_cohorte <- function(panel, col_first_treat, anio, etiqueta) {
  cat("\n---", etiqueta, "- cohorte", anio, "---\n")

  municipios <- panel %>%
    filter(.data[[col_first_treat]] == anio) %>%
    distinct(COD_DANE)

  if (nrow(municipios) == 0) {
    cat("  (ningun municipio en esta cohorte bajo esta definicion)\n")
    return(invisible(NULL))
  }

  cat("  COD_DANE:", paste(municipios$COD_DANE, collapse = ", "), "\n")

  ## Fila de covariables de linea base para esos municipios (una fila por
  ## municipio, tomando el primer registro disponible).
  fila_cov <- panel %>%
    filter(COD_DANE %in% municipios$COD_DANE) %>%
    distinct(COD_DANE, .keep_all = TRUE) %>%
    select(COD_DANE, any_of(COVARIABLES))

  cat("  Covariables de linea base:\n")
  print(fila_cov)

  na_por_cov <- sapply(fila_cov[COVARIABLES[COVARIABLES %in% names(fila_cov)]],
                        function(x) sum(is.na(x)))
  if (any(na_por_cov > 0)) {
    cat("  -> Hay valores NA en:", names(na_por_cov)[na_por_cov > 0], "\n")
    cat("     Esto por si solo puede causar que el ajuste doblemente robusto\n")
    cat("     no se pueda estimar para este municipio en ningun periodo.\n")
  } else {
    cat("  -> Sin NA en las covariables. Si aun asi sale NA en att_gt(), revisa\n")
    cat("     valores extremos (outliers) que puedan generar pesos de\n")
    cat("     propension degenerados (cercanos a 0 o 1).\n")
  }

  ## Si es la cohorte 2025 (o cualquier anio posterior al ultimo anio del
  ## panel), senalar explicitamente el problema de ventana de observacion.
  ultimo_anio_panel <- max(panel$year, na.rm = TRUE)
  if (anio > ultimo_anio_panel) {
    cat(sprintf("  -> AVISO: el panel de resultados solo llega hasta %d.\n", ultimo_anio_panel))
    cat("     Un first_treat posterior a ese anio no tiene NINGUN periodo\n")
    cat("     postratamiento observable - es estructuralmente inestimable,\n")
    cat("     no un problema de covariables. Verifica de que fuente y con\n")
    cat("     que fecha de inicio de vigencia proviene este municipio, y\n")
    cat("     decide si debe contarse como 'tratado' en la Tabla 4.1 o\n")
    cat("     excluirse/anotarse aparte por no aportar evidencia observable.\n")
  }
}

for (def in list(list(col = "first_treat_alta", nombre = "Confianza alta"),
                  list(col = "first_treat_todas", nombre = "Todas las fuentes"))) {
  inspeccionar_cohorte(panel, def$col, 2006, def$nombre)
  inspeccionar_cohorte(panel, def$col, 2025, def$nombre)
}

## -----------------------------------------------------------------------
## 2) Buscar la columna de region DANE
## -----------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n", sep = "")
cat("COLUMNAS DEL PANEL (para encontrar la de region DANE)\n")
cat(strrep("=", 70), "\n")
print(names(panel))

cat("\nBusca en esta lista la columna que contenga Andina/Caribe/Pacifica/\n")
cat("Orinoquia/Amazonia (puede llamarse 'region', 'REGION', 'dane_region',\n")
cat("'region_natural', etc.). Si la encuentras, dime el nombre exacto y el\n")
cat("valor exacto que toma para Orinoquia (con o sin tilde, mayusculas) y\n")
cat("te devuelvo el bloque 3 corregido.\n")
