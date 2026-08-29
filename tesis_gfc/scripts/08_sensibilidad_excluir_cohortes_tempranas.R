## =============================================================================
## 08_sensibilidad_excluir_cohortes_tempranas.R
##
## Chequeo de sensibilidad: excluye del panel a los municipios cuya cohorte
## de tratamiento (2002) cae en o antes del año base usado para las
## covariables de conflicto tardias (2003) -- Caceres, Anzoategui, y
## Chinchina (esta ultima solo en "todas las fuentes") -- confirmados por
## 06_verificar_hallazgos.R. Para esos municipios, homicidios_base/
## secuestros_base/acc_subversivas_base no son estrictamente pre-tratamiento.
##
## Se re-corre la especificacion doblemente robusta (hectareas) SIN esos
## municipios (ni como tratados ni como control -- se eliminan del todo) y
## se compara el ATT simple contra el resultado original. Si el resultado
## es parecido, el problema no cambia las conclusiones; si cambia mucho,
## hay que discutirlo explicitamente como limitacion en la tesis.
##
## Requiere haber corrido 01_preparar_datos_did.R.
##
## USO
## ---
##   Rscript 08_sensibilidad_excluir_cohortes_tempranas.R
## =============================================================================

library(readr)
library(dplyr)
library(did)

panel <- readRDS("output/panel_analisis_did.rds")
dir.create("output/tablas", showWarnings = FALSE)

ANIO_BASE_CONFLICTO_TARDIO <- 2003L

COVARIABLES_XFORMLA <- ~ baseline_forest_base + temp_media_c_base +
  disbogota_base + H_coca_base + homicidios_base

correr_att_simple <- function(data, col_gname) {
  data_modelo <- data %>% rename(.gname = all_of(col_gname))
  set.seed(20260824)
  att_gt_out <- att_gt(
    yname = "loss_area_ha", tname = "year", idname = "id_num", gname = ".gname",
    xformla = COVARIABLES_XFORMLA, data = data_modelo,
    control_group = "notyettreated", est_method = "dr",
    bstrap = TRUE, cband = TRUE, clustervars = "id_num"
  )
  aggte(att_gt_out, type = "simple", na.rm = TRUE)
}

comparar <- function(col_gname, etiqueta) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("SENSIBILIDAD --", etiqueta, "\n")
  cat(strrep("=", 70), "\n")

  municipios_tempranos <- panel %>%
    filter(.data[[col_gname]] > 0, .data[[col_gname]] <= ANIO_BASE_CONFLICTO_TARDIO) %>%
    distinct(COD_DANE, NOMBRE_MPI) %>%
    pull(COD_DANE)

  if (length(municipios_tempranos) == 0) {
    cat("No hay municipios de cohorte temprana para esta definicion -- nada que comparar.\n")
    return(invisible(NULL))
  }

  cat("Excluyendo", length(municipios_tempranos), "municipio(s):",
      paste(municipios_tempranos, collapse = ", "), "\n")

  cat("\n--- CON todos los municipios (resultado original) ---\n")
  agg_con <- correr_att_simple(panel, col_gname)
  cat(sprintf("ATT: %.4f   SE: %.4f   IC 95%%: [%.4f, %.4f]\n",
              agg_con$overall.att, agg_con$overall.se,
              agg_con$overall.att - 1.96 * agg_con$overall.se,
              agg_con$overall.att + 1.96 * agg_con$overall.se))

  panel_sin <- panel %>% filter(!COD_DANE %in% municipios_tempranos)
  cat("\n--- SIN los municipios de cohorte temprana ---\n")
  agg_sin <- correr_att_simple(panel_sin, col_gname)
  cat(sprintf("ATT: %.4f   SE: %.4f   IC 95%%: [%.4f, %.4f]\n",
              agg_sin$overall.att, agg_sin$overall.se,
              agg_sin$overall.att - 1.96 * agg_sin$overall.se,
              agg_sin$overall.att + 1.96 * agg_sin$overall.se))

  cambio_pct <- 100 * abs(agg_sin$overall.att - agg_con$overall.att) / abs(agg_con$overall.att)
  cat(sprintf("\nCambio en el ATT al excluirlos: %.1f%%\n", cambio_pct))
  if (cambio_pct < 15) {
    cat("-> Cambio pequeño: el resultado NO depende de forma importante de estos municipios.\n")
    cat("   El problema de covariables no-pre-tratamiento no parece estar sesgando el ATT agregado.\n")
  } else {
    cat("-> Cambio considerable: estos municipios SI influyen bastante en el resultado agregado.\n")
    cat("   Documentar esto explicitamente como limitacion, y considerar reportar ambas versiones.\n")
  }

  tibble::tibble(
    especificacion = c(paste(etiqueta, "- con cohortes tempranas"), paste(etiqueta, "- sin cohortes tempranas")),
    att = c(agg_con$overall.att, agg_sin$overall.att),
    se = c(agg_con$overall.se, agg_sin$overall.se),
    n_excluidos = c(0, length(municipios_tempranos))
  )
}

resultado_alta <- comparar("first_treat_alta", "Confianza alta")
resultado_todas <- comparar("first_treat_todas", "Todas las fuentes")

tabla_final <- bind_rows(resultado_alta, resultado_todas)
write_csv(tabla_final, "output/tablas/sensibilidad_cohortes_tempranas.csv")
cat("\n\nGuardado: output/tablas/sensibilidad_cohortes_tempranas.csv\n")
