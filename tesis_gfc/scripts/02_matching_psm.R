## =============================================================================
## 02_matching_psm.R
##
## Emparejamiento por puntaje de propensión (PSM), sobre las covariables de
## línea base (deforestación histórica, clima, accesibilidad, conflicto
## armado) preparadas en 01_preparar_datos_did.R.
##
## El emparejamiento se usa aquí para DOS propósitos:
##   1. Diagnóstico de balance/soporte común: ¿los municipios tratados y no
##      tratados son comparables en sus características de línea base?
##   2. Construir una submuestra emparejada, para correr el DiD escalonado
##      (03_did_callaway_santanna.R) también sobre esa submuestra como
##      chequeo de robustez frente a la especificación con todos los
##      controles disponibles.
##
## Corre el emparejamiento por separado para las dos definiciones de
## tratamiento (alta_confianza / todas_fuentes), porque el conjunto de
## municipios tratados —y por tanto el de controles comparables— es distinto
## en cada caso.
##
## USO
## ---
##   Rscript 02_matching_psm.R
## =============================================================================

library(readr)
library(dplyr)
library(MatchIt)
library(ggplot2)

PANEL_ANALISIS <- "output/panel_analisis_did.rds"
dir.create("output", showWarnings = FALSE)
dir.create("output/figuras", showWarnings = FALSE)

# Covariables de línea base a usar en el modelo de propensión. Se excluyen
# distancia_mercado/coca/acc_subversivas si tienen cobertura baja en tu corrida
# real — revisa el resumen de cobertura impreso por el script 01 antes de
# confiar en esta lista tal cual.
COVARIABLES_PSM <- c(
  "baseline_forest_base", "temp_media_c_base", "prec_anual_mm_base",
  "discapital_base", "disbogota_base", "altura_base", "distancia_mercado_base",
  "H_coca_base", "homicidios_base", "secuestros_base", "o_desplaza_base", "acc_subversivas_base"
)

## =============================================================================
## Función principal: corre el matching para una definición de tratamiento
## =============================================================================

correr_matching <- function(panel, col_first_treat, etiqueta) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("MATCHING —", etiqueta, "\n")
  cat(strrep("=", 70), "\n")

  # Dataset a nivel municipio (una fila), con el estado de tratamiento y las
  # covariables de línea base. El PSM se hace sobre la sección transversal,
  # no sobre el panel completo.
  datos_mun <- panel %>%
    distinct(COD_DANE, .keep_all = TRUE) %>%
    mutate(tratado = as.integer(.data[[col_first_treat]] > 0)) %>%
    select(COD_DANE, tratado, all_of(COVARIABLES_PSM))

  covs_disponibles <- COVARIABLES_PSM[sapply(datos_mun[COVARIABLES_PSM], function(x) mean(!is.na(x)) > 0.5)]
  if (length(covs_disponibles) < length(COVARIABLES_PSM)) {
    excluidas <- setdiff(COVARIABLES_PSM, covs_disponibles)
    cat("Aviso: excluidas del modelo de propensión por baja cobertura (<50%):",
        paste(excluidas, collapse = ", "), "\n")
  }

  datos_completos <- datos_mun %>% filter(if_all(all_of(covs_disponibles), ~ !is.na(.)))
  cat("Municipios con covariables completas:", nrow(datos_completos), "de", nrow(datos_mun),
      "(", sum(datos_completos$tratado), "tratados,", sum(datos_completos$tratado == 0), "controles )\n")

  formula_ps <- as.formula(paste("tratado ~", paste(covs_disponibles, collapse = " + ")))

  # Nearest-neighbor 1:1 con calibre de 0.2 desviaciones estándar del logit
  # del puntaje de propensión — el estándar de facto en la literatura
  # (Austin, 2011) para reducir sesgo residual sin perder demasiadas
  # observaciones.
  m_out <- matchit(
    formula_ps,
    data = datos_completos,
    method = "nearest",
    distance = "glm",
    caliper = 0.2,
    std.caliper = TRUE
  )

  cat("\n--- Resumen del emparejamiento ---\n")
  print(summary(m_out, standardize = TRUE)$nn)

  # Balance estandarizado antes/después
  balance <- summary(m_out, standardize = TRUE)$sum.matched
  balance_antes <- summary(m_out, standardize = TRUE)$sum.all

  tabla_balance <- data.frame(
    covariable = rownames(balance_antes),
    dif_estandarizada_antes = balance_antes[, "Std. Mean Diff."],
    dif_estandarizada_despues = balance[match(rownames(balance_antes), rownames(balance)), "Std. Mean Diff."]
  )
  cat("\n--- Balance estandarizado (diferencia de medias / DE conjunta) ---\n")
  cat("Regla práctica: |dif| < 0.1 se considera buen balance.\n")
  print(tabla_balance, row.names = FALSE)

  # Love plot
  tabla_balance_larga <- tabla_balance %>%
    tidyr::pivot_longer(cols = starts_with("dif_estandarizada"), names_to = "momento", values_to = "dif") %>%
    mutate(momento = ifelse(momento == "dif_estandarizada_antes", "Antes del emparejamiento", "Despues del emparejamiento"))

  p_love <- ggplot(tabla_balance_larga, aes(x = dif, y = covariable, color = momento)) +
    geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed", color = "grey60") +
    geom_vline(xintercept = 0, color = "grey40") +
    geom_point(size = 2.5) +
    labs(
      title = paste("Balance de covariables —", etiqueta),
      x = "Diferencia estandarizada de medias", y = NULL, color = NULL
    ) +
    theme_minimal()

  ruta_plot <- paste0("output/figuras/love_plot_", gsub("[^a-z0-9]", "_", tolower(etiqueta)), ".png")
  ggsave(ruta_plot, p_love, width = 8, height = 5, dpi = 150)
  cat("\nLove plot guardado en:", ruta_plot, "\n")

  # Municipios que quedaron emparejados (para usar como submuestra en el DiD)
  matched_data <- match.data(m_out)
  municipios_emparejados <- matched_data$COD_DANE

  list(
    matchit_obj = m_out,
    municipios_emparejados = municipios_emparejados,
    tabla_balance = tabla_balance
  )
}

## =============================================================================
## Ejecutar para las dos definiciones de tratamiento
## =============================================================================

panel <- readRDS(PANEL_ANALISIS)

resultado_alta <- correr_matching(panel, "first_treat_alta", "Confianza alta (Verra + Gold Standard)")
resultado_todas <- correr_matching(panel, "first_treat_todas", "Todas las fuentes (+ RENARE + Cercarbono)")

saveRDS(resultado_alta, "output/matching_alta_confianza.rds")
saveRDS(resultado_todas, "output/matching_todas_fuentes.rds")

cat("\n\nGuardado: output/matching_alta_confianza.rds y output/matching_todas_fuentes.rds\n")
cat("(estos contienen la lista de municipios emparejados, usada por 03_did_callaway_santanna.R)\n")
