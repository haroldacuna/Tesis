## =============================================================================
## 05_did_tasa_deforestacion.R
##
## Repite la especificacion doblemente robusta (la principal) usando
## tasa_deforestacion (loss_area_ha / baseline_forest_base) en vez de
## hectareas absolutas, como chequeo de robustez frente a la sensibilidad a
## valores extremos que se detecto en la cohorte 2018 (Confianza alta):
## un intervalo de confianza de -6000 a +6000 ha, un orden de magnitud mas
## ancho que cualquier otra cohorte -- señal de que uno o dos municipios
## con perdidas extremas estan dominando esa celda.
##
## Requiere haber corrido 01_preparar_datos_did.R antes.
##
## USO
## ---
##   Rscript 05_did_tasa_deforestacion.R
## =============================================================================

library(readr)
library(dplyr)
library(did)
library(ggplot2)

panel <- readRDS("output/panel_analisis_did.rds")
dir.create("output/tablas", showWarnings = FALSE)
dir.create("output/figuras", showWarnings = FALSE)

COVARIABLES_XFORMLA <- ~ baseline_forest_base + temp_media_c_base +
  disbogota_base + H_coca_base + homicidios_base

n_sin_tasa <- sum(is.na(panel$tasa_deforestacion) & !is.na(panel$loss_area_ha))
cat("Municipios-año sin tasa_deforestacion calculable (baseline_forest_base = 0):", n_sin_tasa, "\n")
cat("(quedan fuera de este modelo -- municipios sin bosque base no tienen 'tasa de perdida' definida)\n\n")

correr_did_tasa <- function(data, col_gname, etiqueta) {
  cat("\n", strrep("-", 70), "\n", sep = "")
  cat("Modelo:", etiqueta, "| outcome: tasa_deforestacion\n")
  cat(strrep("-", 70), "\n")

  data_modelo <- data %>% rename(.gname = all_of(col_gname))
  set.seed(20260824)

  att_gt_out <- att_gt(
    yname = "tasa_deforestacion", tname = "year", idname = "id_num", gname = ".gname",
    xformla = COVARIABLES_XFORMLA, data = data_modelo,
    control_group = "notyettreated", est_method = "dr",
    bstrap = TRUE, cband = TRUE, clustervars = "id_num"
  )

  n_na <- sum(is.na(att_gt_out$att))
  cat("NA en att_gt:", n_na, "de", length(att_gt_out$att),
      sprintf("(%.0f%%)\n", 100 * n_na / length(att_gt_out$att)))

  agg_simple <- aggte(att_gt_out, type = "simple", na.rm = TRUE)
  agg_grupo <- aggte(att_gt_out, type = "group", na.rm = TRUE)

  cat(sprintf("\nATT simple (tasa): %.5f   SE: %.5f   IC 95%%: [%.5f, %.5f]\n",
              agg_simple$overall.att, agg_simple$overall.se,
              agg_simple$overall.att - 1.96 * agg_simple$overall.se,
              agg_simple$overall.att + 1.96 * agg_simple$overall.se))
  cat("(interpretacion: cambio en la FRACCION del bosque base perdida por año, no hectareas)\n")

  list(att_gt = att_gt_out, simple = agg_simple, grupo = agg_grupo, etiqueta = etiqueta)
}

resultado_alta <- correr_did_tasa(panel, "first_treat_alta", "Confianza alta")
resultado_todas <- correr_did_tasa(panel, "first_treat_todas", "Todas las fuentes")

## =============================================================================
## Comparar el ancho de los intervalos por cohorte: tasa vs. hectareas
## absolutas -- confirmar si el cambio de outcome reduce la sensibilidad a
## valores extremos (el objetivo de este script).
## =============================================================================

comparar_ancho_ic <- function(resultado, etiqueta) {
  g <- resultado$grupo
  ancho_ic <- 2 * g$crit.val.egt * g$se.egt
  tibble(cohorte = g$egt, ancho_ic_tasa = ancho_ic) %>%
    arrange(desc(ancho_ic_tasa))
}

cat("\n\n=== Ancho del IC por cohorte (tasa_deforestacion) -- Confianza alta ===\n")
print(comparar_ancho_ic(resultado_alta, "alta"), n = Inf)

## =============================================================================
## Guardar
## =============================================================================

saveRDS(list(alta = resultado_alta, todas = resultado_todas), "output/resultados_did_tasa.rds")

tabla_comparacion <- tibble(
  especificacion = c("Confianza alta - tasa", "Todas las fuentes - tasa"),
  att = c(resultado_alta$simple$overall.att, resultado_todas$simple$overall.att),
  se = c(resultado_alta$simple$overall.se, resultado_todas$simple$overall.se)
)
write_csv(tabla_comparacion, "output/tablas/resumen_att_tasa_deforestacion.csv")

cat("\nGuardado: output/resultados_did_tasa.rds\n")
cat("Guardado: output/tablas/resumen_att_tasa_deforestacion.csv\n")
cat("\nCompara el 'ancho_ic_tasa' de arriba contra los IC en hectareas de 04_att_por_grupo.R --\n")
cat("si la cohorte 2018 (u otras) ya no domina desproporcionadamente, confirma que el outcome\n")
cat("en hectareas absolutas era sensible a valores extremos.\n")
