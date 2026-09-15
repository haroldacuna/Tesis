## =============================================================================
## 03c_diagnostico_composicion_dr_filtrado.R
##
## SEGUIMIENTO al resultado de 03b_excluir_vecinos_de_control.R.
##
## La especificacion doblemente robusta cambio mucho mas que la especificacion
## sin covariables al excluir del panel a los vecinos de municipios tratados
## (Confianza alta DR: -56.40 -> -10.73; Todas las fuentes DR: -50.79 ->
## +19.05, cambio de signo). Hay dos explicaciones posibles y no son
## excluyentes:
##
##   (A) DESCONTAMINACION GENUINA: el ATT original estaba inflado porque el
##       grupo de control incluia vecinos con deforestacion elevada por
##       spillover: al quitarlos, el ATT sube hacia su valor "limpio".
##
##   (B) ARTEFACTO DE COMPOSICION: excluir ~300-350 municipios del panel deja
##       muy pocos controles con traslape de covariables suficiente para el
##       modelo doblemente robusto en varios pares grupo-tiempo, que antes
##       eran estimables y ahora salen NA. El ATT agregado terminaria
##       promediando un subconjunto distinto (y mas chico) de cohortes, no
##       necesariamente un contrafactual mas limpio.
##
## Este script compara, ANTES y DESPUES de excluir vecinos de tratados:
##   1) la tasa de pares grupo-tiempo con estimacion NA bajo doblemente
##      robusto (si sube mucho, apoya la hipotesis B);
##   2) el ATT por cohorte bajo ambos escenarios, para ver si el cambio
##      viene de una o dos cohortes puntuales o es generalizado (si es
##      generalizado y las mismas cohortes siguen estimables, apoya la
##      hipotesis A).
##
## REQUIERE: output/panel_analisis_did.rds
##           output/resultados_did_completos.rds   (de 03_did_callaway_santanna.R)
##           output/resultados_spillover.rds        (de 10_spillovers_espaciales.R)
##
## USO
## ---
##   Rscript 03c_diagnostico_composicion_dr_filtrado.R
## =============================================================================

library(dplyr)
library(did)

OUTPUT_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs"
PANEL_ANALISIS <- file.path(OUTPUT_ROOT, "panel_analisis_did.rds")
SEMILLA <- 20260824

panel <- readRDS(PANEL_ANALISIS)
resultados_originales <- readRDS(file.path(OUTPUT_ROOT, "resultados_did_completos.rds"))
spillover <- readRDS(file.path(OUTPUT_ROOT, "resultados_spillover.rds"))

COVARIABLES_XFORMLA <- ~ baseline_forest_base + temp_media_c_base +
  disbogota_base + H_coca_base + homicidios_base

construir_exclusion <- function(panel, col_first_treat, tabla_vecinos) {
  primer_trat <- panel %>%
    distinct(COD_DANE, .data[[col_first_treat]]) %>%
    rename(first_treat = all_of(col_first_treat))
  primer_trat %>%
    left_join(tabla_vecinos, by = "COD_DANE") %>%
    filter(first_treat == 0, !is.na(first_treat_vecino), first_treat_vecino > 0) %>%
    pull(COD_DANE)
}

diagnosticar <- function(def, col, nombre, tabla_vecinos, resultado_original) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("DIAGNOSTICO --", nombre, "-- especificacion doblemente robusta\n")
  cat(strrep("=", 70), "\n")

  ## --- 1) Tasa de NA en el modelo ORIGINAL (sin filtrar) ---
  att_original <- resultado_original$att_gt
  n_total_orig <- length(att_original$att)
  n_na_orig <- sum(is.na(att_original$att))
  cat(sprintf("\n[ORIGINAL, sin filtrar]\n"))
  cat(sprintf("  Pares grupo-tiempo totales: %d\n", n_total_orig))
  cat(sprintf("  Pares con NA: %d (%.1f%%)\n", n_na_orig, 100 * n_na_orig / n_total_orig))

  ## --- 2) Reestimar EXCLUYENDO vecinos, guardando el objeto completo ---
  excluir <- construir_exclusion(panel, col, tabla_vecinos)
  panel_filtrado <- panel %>% filter(!COD_DANE %in% excluir)
  cat(sprintf("\nMunicipios excluidos: %d | Panel filtrado: %d municipios\n",
              length(excluir), panel_filtrado %>% distinct(COD_DANE) %>% nrow()))

  data_modelo <- panel_filtrado %>% rename(.gname = all_of(col))
  set.seed(SEMILLA)
  att_filtrado <- tryCatch({
    att_gt(
      yname = "loss_area_ha", tname = "year", idname = "id_num", gname = ".gname",
      xformla = COVARIABLES_XFORMLA, data = data_modelo, control_group = "notyettreated",
      est_method = "dr", bstrap = TRUE, cband = TRUE, clustervars = "id_num"
    )
  }, error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })

  if (is.null(att_filtrado)) return(invisible(NULL))

  n_total_filt <- length(att_filtrado$att)
  n_na_filt <- sum(is.na(att_filtrado$att))
  cat(sprintf("\n[FILTRADO, excluyendo vecinos de tratados]\n"))
  cat(sprintf("  Pares grupo-tiempo totales: %d\n", n_total_filt))
  cat(sprintf("  Pares con NA: %d (%.1f%%)\n", n_na_filt, 100 * n_na_filt / n_total_filt))

  cat(sprintf("\n>>> Si el %% de NA subio mucho (ej. de %.0f%% a %.0f%%+), la hipotesis\n",
              100 * n_na_orig / n_total_orig, 100 * n_na_orig / n_total_orig + 15))
  cat(">>> B (perdida de traslape / artefacto de composicion) gana fuerza.\n")
  cat(">>> Si el %% de NA se mantiene similar, el cambio en el ATT es mas\n")
  cat(">>> creible como descontaminacion genuina (hipotesis A).\n")

  ## --- 3) Comparar ATT por cohorte, original vs. filtrado ---
  agg_grupo_orig <- resultado_original$grupo
  agg_grupo_filt <- tryCatch(aggte(att_filtrado, type = "group", na.rm = TRUE),
                              error = function(e) NULL)

  cat("\n--- ATT por cohorte: ORIGINAL vs. FILTRADO ---\n")
  tabla_orig <- tibble(cohorte = agg_grupo_orig$egt, att_original = round(agg_grupo_orig$att.egt, 2))

  if (!is.null(agg_grupo_filt)) {
    tabla_filt <- tibble(cohorte = agg_grupo_filt$egt, att_filtrado = round(agg_grupo_filt$att.egt, 2))
    comparacion <- full_join(tabla_orig, tabla_filt, by = "cohorte") %>% arrange(cohorte)
    print(comparacion, n = Inf)

    cohortes_perdidas <- setdiff(tabla_orig$cohorte, tabla_filt$cohorte)
    if (length(cohortes_perdidas) > 0) {
      cat("\n>>> Cohortes que eran estimables en el modelo ORIGINAL y dejaron de\n")
      cat(">>> serlo al filtrar (salieron NA en todas sus celdas):", cohortes_perdidas, "\n")
      cat(">>> Si son cohortes con ATT muy negativo en el original (revisa la\n")
      cat(">>> columna att_original arriba), su desaparicion por si sola puede\n")
      cat(">>> explicar gran parte del cambio en el ATT agregado -> hipotesis B.\n")
    } else {
      cat("\n>>> Todas las cohortes originales siguen siendo estimables tras\n")
      cat(">>> filtrar. El cambio en el ATT agregado no se explica por perdida\n")
      cat(">>> de cohortes, sino por un cambio genuino en las estimaciones ->\n")
      cat(">>> esto apoya la hipotesis A (descontaminacion).\n")
    }
  } else {
    cat("No se pudo calcular aggte(type='group') sobre el modelo filtrado.\n")
  }

  invisible(list(att_original = att_original, att_filtrado = att_filtrado))
}

resultado_alta  <- diagnosticar("alta",  "first_treat_alta",  "Confianza alta",
                                 spillover$alta$tabla_vecinos,  resultados_originales$alta_dr)
resultado_todas <- diagnosticar("todas", "first_treat_todas", "Todas las fuentes",
                                 spillover$todas$tabla_vecinos, resultados_originales$todas_dr)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("FIN DEL DIAGNOSTICO\n")
cat(strrep("=", 70), "\n")
