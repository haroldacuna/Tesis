## =============================================================================
## 03_did_callaway_santanna.R
##
## Modelo principal: diferencias en diferencias con adopción escalonada
## (Callaway y Sant'Anna, 2021), sobre la variable de tratamiento construida
## en esta tesis (presencia de proyectos de bonos de carbono).
##
## Corre TRES especificaciones, para cada una de las DOS definiciones de
## tratamiento (alta_confianza / todas_fuentes) = 6 modelos en total:
##
##   (a) Sin covariables - supone tendencias paralelas incondicionales.
##   (b) Doblemente robusto CON covariables de línea base (deforestación
##       histórica, clima, accesibilidad, conflicto armado) - la
##       especificación principal recomendada, porque no requiere descartar
##       observaciones como sí lo hace el emparejamiento.
##   (c) Sobre la submuestra emparejada por PSM (02_matching_psm.R) - para
##       cumplir explícitamente con el plan de emparejamiento de la
##       propuesta, como chequeo de robustez frente a (b).
##
## Para cada modelo se calculan: el ATT simple (promedio), el estudio de
## eventos (dinámico, por tiempo desde el tratamiento), por cohorte y por
## periodo calendario.
##
## USO
## ---
##   Rscript 03_did_callaway_santanna.R
##
## NOTA: att_gt() con bootstrap (bstrap = TRUE) puede tardar varios minutos
## por especificación con paneles grandes - normal, no es un error.
## =============================================================================

library(readr)
library(dplyr)
library(did)
library(ggplot2)

PANEL_ANALISIS <- "output/panel_analisis_did.rds"
SEMILLA <- 20260824

dir.create("output", showWarnings = FALSE)
dir.create("output/figuras", showWarnings = FALSE)
dir.create("output/tablas", showWarnings = FALSE)

panel <- readRDS(PANEL_ANALISIS)

## Covariables de línea base para la especificación doblemente robusta.
##
## AVISO METODOLÓGICO IMPORTANTE: con cohortes de tratamiento pequeñas
## (varias tienen 1-3 municipios - normal dado que solo 88-99 municipios en
## total están tratados, repartidos en ~17 años de cohorte), el estimador
## doblemente robusto puede no poder estimarse para varios pares grupo-tiempo
## específicos (matriz de covarianza singular / insuficientes grados de
## libertad). Se usa aquí una lista PARSIMONIOSA (6 covariables, no las 13
## disponibles) para mitigar esto - agregar más covariables reduce aún más
## las celdas estimables. Si tu corrida real muestra una tasa alta de NA
## (revisa el aviso que imprime correr_did() más abajo), considera:
##   - reducir aún más esta lista,
##   - agrupar cohortes adyacentes con muy pocos municipios,
##   - o apoyarte más en las especificaciones (a) y (c), que no dependen de
##     ajustar por tantas covariables a la vez.
COVARIABLES_XFORMLA <- ~ baseline_forest_base + temp_media_c_base +
  disbogota_base + H_coca_base + homicidios_base

## =============================================================================
## Función auxiliar: corre att_gt() + las 4 agregaciones, para una combinación
## de (definición de tratamiento) x (especificación) x (variable de outcome)
## =============================================================================

correr_did <- function(data, col_gname, xformla, outcome, etiqueta) {
  cat("\n", strrep("-", 70), "\n", sep = "")
  cat("Modelo:", etiqueta, "| outcome:", outcome, "\n")
  cat(strrep("-", 70), "\n")

  data_modelo <- data %>% rename(.gname = all_of(col_gname))

  set.seed(SEMILLA)
  att_gt_out <- tryCatch({
    att_gt(
      yname = outcome,
      tname = "year",
      idname = "id_num",
      gname = ".gname",
      xformla = xformla,
      data = data_modelo,
      control_group = "notyettreated",
      est_method = "dr",
      bstrap = TRUE,
      cband = TRUE,
      clustervars = "id_num"
    )
  }, error = function(e) {
    cat("ERROR en att_gt():", conditionMessage(e), "\n")
    NULL
  })

  if (is.null(att_gt_out)) return(NULL)

  agg_simple <- aggte(att_gt_out, type = "simple", na.rm = TRUE)
  agg_dinamico <- aggte(att_gt_out, type = "dynamic", min_e = -10, max_e = 10, na.rm = TRUE)
  agg_grupo <- aggte(att_gt_out, type = "group", na.rm = TRUE)
  agg_calendario <- aggte(att_gt_out, type = "calendar", na.rm = TRUE)

  n_na <- sum(is.na(att_gt_out$att))
  if (n_na > 0) {
    cat("Aviso:", n_na, "de", length(att_gt_out$att),
        sprintf("(%.0f%%)", 100 * n_na / length(att_gt_out$att)),
        "pares grupo-tiempo con estimacion NA - excluidos de las agregaciones via na.rm = TRUE.\n")
    grupos_afectados <- att_gt_out$group[is.na(att_gt_out$att)]
    tabla_na <- sort(table(grupos_afectados), decreasing = TRUE)
    cat("  Cohortes mas afectadas (grupo = anio, n = cuantos pares grupo-tiempo de ese grupo salieron NA):\n")
    print(head(tabla_na, 8))
    cat("  Si esto es una fraccion alta (>30-40%), probablemente refleja cohortes muy chicas\n")
    cat("  (1-3 municipios) en tus datos reales, no un error del script - considera agrupar\n")
    cat("  cohortes adyacentes chicas, o apoyarte mas en las especificaciones (a)/(c).\n")
  }

  cat("\nATT simple (promedio, todas las cohortes y periodos post-tratamiento):\n")
  cat(sprintf("  Estimado: %.4f   Error estandar: %.4f   IC 95%%: [%.4f, %.4f]\n",
              agg_simple$overall.att, agg_simple$overall.se,
              agg_simple$overall.att - 1.96 * agg_simple$overall.se,
              agg_simple$overall.att + 1.96 * agg_simple$overall.se))

  list(
    att_gt = att_gt_out,
    simple = agg_simple,
    dinamico = agg_dinamico,
    grupo = agg_grupo,
    calendario = agg_calendario,
    etiqueta = etiqueta
  )
}

## =============================================================================
## Preparar la submuestra emparejada (para la especificación (c))
## =============================================================================

cargar_submuestra_emparejada <- function(panel, ruta_matching) {
  if (!file.exists(ruta_matching)) {
    cat("Aviso: no encuentro", ruta_matching, "- corre primero 02_matching_psm.R. Se omite la especificación (c).\n")
    return(NULL)
  }
  m <- readRDS(ruta_matching)
  panel %>% filter(COD_DANE %in% m$municipios_emparejados)
}

## =============================================================================
## Ejecutar las 6 combinaciones (2 definiciones x 3 especificaciones)
## outcome principal: loss_area_ha. (tasa_deforestacion queda preparada como
## robustez adicional - descomentar para correrla también.)
## =============================================================================

resultados <- list()

definiciones <- list(
  alta = list(col = "first_treat_alta", ruta_matching = "output/matching_alta_confianza.rds", nombre = "Confianza alta"),
  todas = list(col = "first_treat_todas", ruta_matching = "output/matching_todas_fuentes.rds", nombre = "Todas las fuentes")
)

for (def in names(definiciones)) {
  info <- definiciones[[def]]

  # (a) sin covariables
  resultados[[paste0(def, "_sin_cov")]] <- correr_did(
    panel, info$col, ~1, "loss_area_ha",
    paste0(info$nombre, " - sin covariables")
  )

  # (b) doblemente robusto con covariables
  resultados[[paste0(def, "_dr")]] <- correr_did(
    panel, info$col, COVARIABLES_XFORMLA, "loss_area_ha",
    paste0(info$nombre, " - doblemente robusto (con covariables)")
  )

  # (c) submuestra emparejada
  panel_emparejado <- cargar_submuestra_emparejada(panel, info$ruta_matching)
  if (!is.null(panel_emparejado)) {
    resultados[[paste0(def, "_matched")]] <- correr_did(
      panel_emparejado, info$col, ~1, "loss_area_ha",
      paste0(info$nombre, " - submuestra emparejada (PSM)")
    )
  }
}

## =============================================================================
## Tabla resumen de robustez: ATT simple en las 6 especificaciones
## =============================================================================

tabla_resumen <- do.call(rbind, lapply(names(resultados), function(nombre) {
  r <- resultados[[nombre]]
  if (is.null(r)) return(NULL)
  data.frame(
    especificacion = r$etiqueta,
    att = r$simple$overall.att,
    se = r$simple$overall.se,
    ic_inf = r$simple$overall.att - 1.96 * r$simple$overall.se,
    ic_sup = r$simple$overall.att + 1.96 * r$simple$overall.se
  )
}))

cat("\n\n", strrep("=", 70), "\n", sep = "")
cat("TABLA RESUMEN DE ROBUSTEZ - ATT simple por especificacion\n")
cat(strrep("=", 70), "\n")
print(tabla_resumen, row.names = FALSE)

write_csv(tabla_resumen, "output/tablas/resumen_robustez_att.csv")

## =============================================================================
## Grafico de estudio de eventos - especificacion principal (b): doblemente
## robusto con covariables, para cada definicion de tratamiento.
## =============================================================================

for (def in names(definiciones)) {
  r <- resultados[[paste0(def, "_dr")]]
  if (is.null(r)) next

  p <- ggdid(r$dinamico) +
    labs(
      title = paste("Estudio de eventos -", definiciones[[def]]$nombre),
      subtitle = "Especificacion doblemente robusta, con covariables de linea base",
      x = "Anios desde el inicio del tratamiento", y = "ATT (efecto sobre hectareas deforestadas)"
    ) +
    theme_minimal()

  ruta <- paste0("output/figuras/event_study_", def, ".png")
  ggsave(ruta, p, width = 9, height = 5.5, dpi = 150)
  cat("\nGrafico guardado:", ruta, "\n")
}

## =============================================================================
## Guardar todos los resultados para inspeccion posterior
## =============================================================================

saveRDS(resultados, "output/resultados_did_completos.rds")
cat("\nGuardado: output/resultados_did_completos.rds (todos los att_gt/aggte de las 6 especificaciones)\n")
cat("Guardado: output/tablas/resumen_robustez_att.csv\n")
