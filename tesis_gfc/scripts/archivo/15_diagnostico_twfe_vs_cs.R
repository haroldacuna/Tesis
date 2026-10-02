## =============================================================================
## 15_diagnostico_twfe_vs_cs.R
##
## Cierra la pregunta de por que TWFE (+53,56) y Callaway-Sant'Anna (-125,46)
## divergen, cuando la descomposicion de Goodman-Bacon ya descarto el mecanismo
## canonico: las comparaciones prohibidas concentran solo el 2,8% del peso y
## apuntan en direccion negativa, hacia CS y no en contra.
##
## LOGICA DEL DIAGNOSTICO
## ----------------------
## El 97,2% del peso del TWFE esta en comparaciones contra nunca tratados, con
## un estimado de +58,99. CS tambien compara contra unidades no tratadas. Si
## ambos usan las mismas comparaciones limpias, la divergencia solo puede venir
## de dos fuentes, y este script las separa:
##
##   (A) ESTIMACION INTRA-COHORTE. Para una misma cohorte g, el 2x2 de Bacon y
##       el ATT(g) de CS podrian diferir. Bacon contrasta el promedio de todo el
##       periodo post contra el promedio de todo el pre; CS contrasta cada
##       periodo t contra el periodo base g-1. Si los estimados por cohorte
##       difieren, el mecanismo es este.
##
##   (B) PONDERACION ENTRE COHORTES. Bacon pondera cada 2x2 por la varianza del
##       tratamiento dentro del par, lo que favorece cohortes tratadas hacia la
##       mitad del panel. CS agrega por tamano de cohorte. Si los estimados por
##       cohorte coinciden pero los promedios ponderados difieren, el mecanismo
##       es este.
##
## La prueba decisiva es cruzar los pesos: aplicar los pesos de Bacon a los
## estimados de CS. Si eso reproduce el valor del TWFE, la ponderacion explica
## toda la brecha. Si no, la explicacion esta en (A).
##
## Se re-estima CS con control_group = "nevertreated" para que la comparacion
## sea homologa: el bloque "Treated vs Untreated" de Bacon usa solo unidades
## nunca tratadas, mientras el pipeline principal usa "notyettreated".
##
## USO
## ---
##   Rscript 15_diagnostico_twfe_vs_cs.R
## =============================================================================

library(dplyr)
library(did)
library(fixest)
library(bacondecomp)
library(readr)

DATA_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/data"
OUTPUT_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs"
PANEL <- file.path(OUTPUT_ROOT, "panel_analisis_did.rds")
dir.create(file.path(OUTPUT_ROOT, "tablas"), showWarnings = FALSE, recursive = TRUE)

panel <- readRDS(PANEL)
panel$id_num <- as.numeric(panel$COD_DANE)

diagnosticar <- function(panel, col_gname, etiqueta) {

  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("DIAGNOSTICO TWFE vs CALLAWAY-SANT'ANNA -- ", etiqueta, "\n", sep = "")
  cat(strrep("=", 78), "\n")

  p <- panel %>%
    mutate(.g = .data[[col_gname]],
           tratado = as.integer(.g > 0 & year >= .g))

  ## --- 1. TWFE de referencia -------------------------------------------------
  m_twfe <- feols(loss_area_ha ~ tratado | COD_DANE + year, data = p, cluster = ~COD_DANE)
  b_twfe <- as.numeric(coef(m_twfe)["tratado"])
  cat(sprintf("\nTWFE                       : %8.2f\n", b_twfe))

  ## --- 2. Descomposicion de Goodman-Bacon ------------------------------------
  bd <- bacon(loss_area_ha ~ tratado, data = p, id_var = "COD_DANE", time_var = "year")

  limpias <- bd %>% filter(type == "Treated vs Untreated")
  cat(sprintf("Bacon, solo comparaciones limpias: %8.2f  (peso total %.4f)\n",
              weighted.mean(limpias$estimate, limpias$weight), sum(limpias$weight)))

  ## Pesos de Bacon por cohorte, renormalizados dentro del bloque limpio.
  bacon_cohorte <- limpias %>%
    group_by(cohorte = treated) %>%
    summarise(est_bacon = weighted.mean(estimate, weight),
              peso_bacon = sum(weight), .groups = "drop") %>%
    mutate(peso_bacon_norm = peso_bacon / sum(peso_bacon))

  ## --- 3. CS con el MISMO grupo de control (nunca tratados) ------------------
  set.seed(20260824)
  att_nt <- att_gt(
    yname = "loss_area_ha", tname = "year", idname = "id_num", gname = ".g",
    xformla = ~1, data = p, control_group = "nevertreated",
    est_method = "dr", bstrap = TRUE, cband = TRUE, clustervars = "id_num"
  )
  agg_simple <- aggte(att_nt, type = "simple", na.rm = TRUE)
  agg_group  <- aggte(att_nt, type = "group",  na.rm = TRUE)

  cat(sprintf("CS (nevertreated), simple  : %8.2f\n", agg_simple$overall.att))
  cat(sprintf("CS (nevertreated), group   : %8.2f\n", agg_group$overall.att))

  ## Pesos de CS: proporcionales al tamano de cada cohorte.
  tam <- p %>%
    filter(.g > 0) %>% distinct(COD_DANE, .g) %>% count(.g, name = "n_mun")

  cs_cohorte <- data.frame(cohorte = agg_group$egt, est_cs = agg_group$att.egt) %>%
    left_join(tam, by = c("cohorte" = ".g")) %>%
    filter(!is.na(est_cs)) %>%
    mutate(peso_cs_norm = n_mun / sum(n_mun))

  ## --- 4. Comparacion cohorte por cohorte ------------------------------------
  comp <- full_join(bacon_cohorte, cs_cohorte, by = "cohorte") %>%
    arrange(cohorte) %>%
    mutate(diferencia = est_bacon - est_cs)

  cat("\n--- Estimado por cohorte: 2x2 de Bacon frente a ATT(g) de CS ---\n")
  print(comp %>%
          transmute(cohorte, n_mun,
                    est_bacon = round(est_bacon, 1),
                    est_cs = round(est_cs, 1),
                    dif = round(diferencia, 1),
                    peso_bacon = round(peso_bacon_norm, 4),
                    peso_cs = round(peso_cs_norm, 4)),
        row.names = FALSE)

  ## Correlacion entre ambos conjuntos de estimados por cohorte.
  ok <- comp %>% filter(!is.na(est_bacon), !is.na(est_cs))
  if (nrow(ok) >= 3) {
    cat(sprintf("\nCorrelacion entre estimados por cohorte: %.3f  (n = %d cohortes)\n",
                cor(ok$est_bacon, ok$est_cs), nrow(ok)))
  }

  ## --- 5. PRUEBA DECISIVA: cruzar los pesos ----------------------------------
  ##
  ## Si aplicar los pesos de Bacon a los estimados de CS reproduce el TWFE,
  ## la ponderacion explica toda la brecha. Si el resultado sigue siendo
  ## negativo, la explicacion esta en la estimacion intra-cohorte.
  cruz <- ok %>% mutate(w = peso_bacon_norm / sum(peso_bacon_norm))
  cs_con_pesos_bacon <- sum(cruz$w * cruz$est_cs)

  cruz2 <- ok %>% mutate(w = peso_cs_norm / sum(peso_cs_norm))
  bacon_con_pesos_cs <- sum(cruz2$w * cruz2$est_bacon)

  cat("\n", strrep("-", 78), "\n", sep = "")
  cat("PRUEBA DECISIVA -- cruce de ponderaciones\n")
  cat(strrep("-", 78), "\n")
  cat(sprintf("  TWFE observado                          : %8.2f\n", b_twfe))
  cat(sprintf("  Estimados de CS con pesos de Bacon      : %8.2f\n", cs_con_pesos_bacon))
  cat(sprintf("  Estimados de Bacon con pesos de CS      : %8.2f\n", bacon_con_pesos_cs))
  cat(sprintf("  CS agregacion group                     : %8.2f\n", agg_group$overall.att))

  cat("\n  Lectura:\n")
  if (abs(cs_con_pesos_bacon - b_twfe) < 0.4 * abs(b_twfe - agg_group$overall.att)) {
    cat("  Los estimados de CS reponderados con los pesos de Bacon se acercan al\n")
    cat("  TWFE. La PONDERACION ENTRE COHORTES explica la mayor parte de la brecha.\n")
  } else if (abs(bacon_con_pesos_cs - agg_group$overall.att) <
             0.4 * abs(b_twfe - agg_group$overall.att)) {
    cat("  Los 2x2 de Bacon reponderados con los pesos de CS se acercan a CS.\n")
    cat("  La PONDERACION ENTRE COHORTES explica la mayor parte de la brecha.\n")
  } else {
    cat("  Ninguna reponderacion cierra la brecha: los estimados POR COHORTE\n")
    cat("  difieren entre metodos. El mecanismo es la ESTIMACION INTRA-COHORTE,\n")
    cat("  es decir el periodo base de comparacion (promedio de todo el pre en\n")
    cat("  Bacon/TWFE, frente al periodo g-1 en CS). Ver la columna 'dif'.\n")
  }

  ## --- 6. Sensibilidad al periodo base en CS ---------------------------------
  ##
  ## base_period = "universal" fija el periodo base en g-1 para todos los
  ## periodos; "varying" (el usado por defecto) lo mueve. Comparar ambos aisla
  ## el efecto de esa eleccion.
  set.seed(20260824)
  att_univ <- tryCatch({
    att_gt(
      yname = "loss_area_ha", tname = "year", idname = "id_num", gname = ".g",
      xformla = ~1, data = p, control_group = "nevertreated",
      base_period = "universal",
      est_method = "dr", bstrap = TRUE, cband = TRUE, clustervars = "id_num"
    )
  }, error = function(e) { cat("  (base_period universal fallo:", conditionMessage(e), ")\n"); NULL })

  if (!is.null(att_univ)) {
    a_u <- aggte(att_univ, type = "simple", na.rm = TRUE)
    cat(sprintf("\n  CS con base_period universal            : %8.2f\n", a_u$overall.att))
    cat(sprintf("  CS con base_period varying (por defecto): %8.2f\n", agg_simple$overall.att))
  }

  write_csv(comp, file.path(OUTPUT_ROOT, "tablas", sprintf("diagnostico_twfe_cs_%s.csv", col_gname)))
  invisible(comp)
}

for (d in list(list(c = "first_treat_alta",  n = "Confianza alta"),
               list(c = "first_treat_todas", n = "Todas las fuentes"))) {
  diagnosticar(panel, d$c, d$n)
}

cat("\n", strrep("=", 78), "\n", sep = "")
cat("Tablas guardadas en output/tablas/diagnostico_twfe_cs_*.csv\n")
