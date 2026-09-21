## =============================================================================
## 43_robustez_anticipacion_y_loo.R
##
## Dos pruebas de robustez sobre la ESPECIFICACION PRINCIPAL (D1 depurada,
## doblemente robusto con covariables, notyettreated, cohortes 2013-2019):
##
##   1. ANTICIPACION. La descomposicion de 42 mostro que, en la submuestra con
##      dosis y sin covariables, cambiar la base de g-1 al promedio de g-5..g-1
##      reduce el ATT en hectareas de -402,6 a -55,6. Eso sugiere que el anio
##      g-1 es un pico de deforestacion en los municipios grandes: si los
##      proyectos se instalan donde la deforestacion acaba de dispararse, el
##      regreso a la normalidad aparece como efecto (reversion a la media, el
##      patron de Ashenfelter). Con anticipation = a, did usa g-1-a como base.
##      Si el ATT se sostiene con a = 1 y a = 2, el pico no lo estaba generando.
##
##   2. EXCLUSION UNO A UNO. En la submuestra con dosis, Cartagena del Chaira
##      aporta -240 de las -342 ha del ATT promedio. Se reestima quitando cada
##      tratado y se mide su influencia; despues, con bootstrap completo, sin
##      Cartagena del Chaira y sin los tres municipios mas influyentes.
##
## Metrica de comparacion: el ATT del estudio de eventos BALANCEADO e = 0..5,
## porque no depende de como did clasifique los periodos de anticipacion en la
## agregacion simple. El ATT simple se reporta al lado como referencia.
##
## USO
##   Rscript scripts/43_robustez_anticipacion_y_loo.R            (todo)
##   Rscript scripts/43_robustez_anticipacion_y_loo.R --sin-loo  (solo anticipacion)
##
## La exclusion uno a uno son 37 reestimaciones DR sin bootstrap: puede tardar
## entre 15 y 40 minutos. Guarda checkpoint despues de cada una y reanuda.
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(did)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
CORRER_LOO <- !("--sin-loo" %in% args)

RAIZ <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "data/.raiz_canonica"))) {
    p <- dirname(d); if (identical(p, d)) stop("Sin data/.raiz_canonica"); d <- p
  }
  d
})
DIR_OUT <- file.path(RAIZ, "outputs")
DIR_TAB <- file.path(DIR_OUT, "tablas")
DIR_FIG <- file.path(DIR_OUT, "figuras")
for (x in c(DIR_TAB, DIR_FIG)) dir.create(x, showWarnings = FALSE, recursive = TRUE)

## --- Parametros: identicos a la especificacion principal de 40 ---------------
SEMILLA   <- 20260824
OUTCOME   <- "loss_area_ha"
VENTANA   <- 2013:2019
BALANCE_E <- 5
BITERS    <- 999
ANTICIPACIONES <- 0:2
REFERENCIA_40  <- -424.99   # ATT simple de la principal en 40, para validar la corrida a = 0

COVARIABLES_XFORMLA <- ~ log_baseline_forest + temp_media_c_base +
  prec_anual_mm_base + discapital_base + disbogota_base + distancia_mercado_base +
  H_coca_base + homicidios_base + log_defor_pre_media + defor_pre_tendencia

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(t) cat("\n", strrep("=", 76), "\n", t, "\n", strrep("=", 76), "\n", sep = "")

## =============================================================================
## 1. Datos: la misma muestra que el bloque A de 40
## =============================================================================
panel <- readRDS(file.path(DIR_OUT, "panel_analisis_did.rds"))
panel$COD_DANE <- pad5(panel$COD_DANE)

COLS_PRE <- c("log_defor_pre_media", "defor_pre_tendencia", "log_baseline_forest")
if (!all(COLS_PRE %in% names(panel))) {
  cov_pre <- read_csv(file.path(DIR_TAB, "covariables_pre_municipio.csv"),
                      col_types = cols(COD_DANE = col_character()), show_col_types = FALSE)
  cov_pre$COD_DANE <- pad5(cov_pre$COD_DANE)
  cols_unir <- intersect(c("COD_DANE", COLS_PRE), names(cov_pre))
  panel <- panel %>% select(-any_of(setdiff(cols_unir, "COD_DANE"))) %>%
    left_join(cov_pre[, cols_unir], by = "COD_DANE")
}

trat <- read_csv(file.path(RAIZ, "data/final/panel_con_tratamiento_actualizado.csv"),
                 col_select = c(COD_DANE, anio_inicio_tratamiento_todas_fuentes_redd_depurada,
                                muestra_redd_depurada),
                 col_types = cols(COD_DANE = col_character(), .default = col_double()),
                 show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>%
  group_by(COD_DANE) %>%
  summarise(G_dep = coalesce(min_na(anio_inicio_tratamiento_todas_fuentes_redd_depurada), 0),
            muestra_dep = min(muestra_redd_depurada), .groups = "drop")

nombres <- read_csv(file.path(RAIZ, "data/interim/dosis_tratamiento_final.csv"),
                    col_types = cols(COD_DANE = col_character()), show_col_types = FALSE) %>%
  transmute(COD_DANE = pad5(COD_DANE), NOMBRE_MPI, DPTO_CNMBR)

panel <- panel %>%
  select(-any_of(c("G_dep", "muestra_dep"))) %>%
  inner_join(trat, by = "COD_DANE")

n_anios <- n_distinct(panel$year)
incompletos <- panel %>% group_by(COD_DANE) %>%
  summarise(ok = n() == n_anios && all(!is.na(.data[[OUTCOME]])), .groups = "drop") %>%
  filter(!ok) %>% pull(COD_DANE)
panel <- panel %>% filter(!COD_DANE %in% incompletos)

muestra <- panel %>%
  filter(muestra_dep == 1, G_dep == 0 | G_dep %in% VENTANA)

tratados <- muestra %>% filter(G_dep > 0) %>% distinct(COD_DANE, G_dep) %>%
  left_join(nombres, by = "COD_DANE") %>%
  mutate(NOMBRE_MPI = coalesce(NOMBRE_MPI, COD_DANE)) %>%
  arrange(G_dep, NOMBRE_MPI)
cat(sprintf("Muestra principal: %d municipios | %d tratados | %d nunca tratados\n",
            n_distinct(muestra$COD_DANE), nrow(tratados),
            n_distinct(muestra$COD_DANE[muestra$G_dep == 0])))

## =============================================================================
## 2. Estimador (misma llamada que la principal)
## =============================================================================
estimar <- function(data, anticipacion = 0, bstrap = TRUE, etiqueta = "") {
  set.seed(SEMILLA)
  out <- tryCatch(
    suppressWarnings(att_gt(
      yname = OUTCOME, tname = "year", idname = "id_num", gname = "G_dep",
      xformla = COVARIABLES_XFORMLA, data = data,
      control_group = "notyettreated", est_method = "dr",
      anticipation = anticipacion,
      bstrap = bstrap, biters = BITERS, cband = bstrap, clustervars = "id_num")),
    error = function(e) { cat("ERROR [", etiqueta, "]:", conditionMessage(e), "\n"); NULL })
  if (is.null(out)) return(NULL)

  agg <- function(...) tryCatch(suppressWarnings(aggte(out, na.rm = TRUE, ...)),
                                error = function(e) NULL)
  simple <- agg(type = "simple")
  bal    <- agg(type = "dynamic", balance_e = BALANCE_E, min_e = 0, max_e = BALANCE_E)
  din    <- agg(type = "dynamic", min_e = -6, max_e = BALANCE_E)
  list(att_gt = out, simple = simple, balanceado = bal, dinamico = din,
       na_pct = 100 * mean(is.na(out$att)), etiqueta = etiqueta)
}

resumir <- function(r) {
  if (is.null(r)) return(data.frame(att_simple = NA, ee_simple = NA,
                                    att_bal = NA, ee_bal = NA, na_pct = NA))
  data.frame(
    att_simple = if (is.null(r$simple)) NA else r$simple$overall.att,
    ee_simple  = if (is.null(r$simple)) NA else r$simple$overall.se,
    att_bal    = if (is.null(r$balanceado)) NA else r$balanceado$overall.att,
    ee_bal     = if (is.null(r$balanceado)) NA else r$balanceado$overall.se,
    na_pct     = r$na_pct
  )
}

## =============================================================================
## 3. Anticipacion
## =============================================================================
titulo("1. ANTICIPACION: base g-1 (a=0), g-2 (a=1), g-3 (a=2)")

res_ant <- lapply(ANTICIPACIONES, function(a) {
  cat(sprintf("\n-- anticipation = %d (base g-%d) --\n", a, a + 1))
  estimar(muestra, anticipacion = a, bstrap = TRUE, etiqueta = paste("a =", a))
})
names(res_ant) <- paste0("a", ANTICIPACIONES)

tab_ant <- do.call(rbind, lapply(seq_along(ANTICIPACIONES), function(k) {
  cbind(anticipacion = ANTICIPACIONES[k], base = paste0("g-", ANTICIPACIONES[k] + 1),
        resumir(res_ant[[k]]))
})) %>%
  mutate(ic_bal_inf = att_bal - 1.96 * ee_bal, ic_bal_sup = att_bal + 1.96 * ee_bal,
         cambio_vs_a0_pct = 100 * (att_bal / att_bal[anticipacion == 0] - 1))
print(tab_ant %>% mutate(across(where(is.numeric), ~ round(.x, 2))), row.names = FALSE)
write_csv(tab_ant, file.path(DIR_TAB, "43_anticipacion.csv"))

## Validacion: a = 0 debe reproducir la principal de 40
a0 <- tab_ant$att_simple[tab_ant$anticipacion == 0]
cat(sprintf("\nValidacion a = 0: ATT simple %.2f vs %.2f de 40 -> %s\n", a0, REFERENCIA_40,
            if (isTRUE(abs(a0 - REFERENCIA_40) < 0.01)) "COINCIDE" else "NO COINCIDE: revisar la muestra"))

## El pico previo: coeficientes e = -1, -2, -3 con a = 0
din0 <- res_ant$a0$dinamico
if (!is.null(din0)) {
  pre <- data.frame(e = din0$egt, att = din0$att.egt, ee = din0$se.egt) %>%
    filter(e %in% -3:-1) %>%
    mutate(ic_inf = att - 1.96 * ee, ic_sup = att + 1.96 * ee)
  cat("\nCoeficientes previos con a = 0 (base variable: cada uno es Y_e - Y_{e-1} relativo a controles):\n")
  print(pre %>% mutate(across(where(is.numeric), ~ round(.x, 1))), row.names = FALSE)
}

## Grafico: estudio de eventos superpuesto
din_df <- do.call(rbind, lapply(names(res_ant), function(nm) {
  dd <- res_ant[[nm]]$dinamico
  if (is.null(dd)) return(NULL)
  data.frame(anticipacion = sub("a", "a = ", nm), e = dd$egt, att = dd$att.egt, ee = dd$se.egt)
}))
if (!is.null(din_df)) {
  p <- ggplot(din_df, aes(e, att, color = anticipacion)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
    geom_pointrange(aes(ymin = att - 1.96 * ee, ymax = att + 1.96 * ee),
                    position = position_dodge(width = 0.5), size = 0.35) +
    labs(title = "Estudio de eventos de la especificacion principal segun el anio base",
         subtitle = "D1 depurada, DR con covariables. a = 0: base g-1; a = 1: g-2; a = 2: g-3. IC 95% puntual.",
         x = "Anios desde la adopcion", y = "ATT (hectareas deforestadas)", color = NULL) +
    theme_minimal()
  ggsave(file.path(DIR_FIG, "43_event_study_anticipacion.png"), p, width = 10, height = 5.5, dpi = 150)
}

## =============================================================================
## 4. Exclusion uno a uno
## =============================================================================
tab_loo <- NULL
if (CORRER_LOO) {
  titulo("2. EXCLUSION UNO A UNO (sin bootstrap, solo el punto)")
  ruta_ck <- file.path(DIR_TAB, "43_leave_one_out.csv")
  hechos <- if (file.exists(ruta_ck)) {
    read_csv(ruta_ck, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE)
  } else NULL
  if (!is.null(hechos)) cat("Checkpoint:", nrow(hechos), "exclusiones ya calculadas, se reanuda\n")

  base_full <- resumir(res_ant$a0)
  for (k in seq_len(nrow(tratados))) {
    cod <- tratados$COD_DANE[k]
    if (!is.null(hechos) && cod %in% hechos$COD_DANE) next
    t0 <- Sys.time()
    r <- estimar(muestra %>% filter(COD_DANE != cod), anticipacion = 0, bstrap = FALSE,
                 etiqueta = paste("sin", tratados$NOMBRE_MPI[k]))
    s <- resumir(r)
    fila <- data.frame(COD_DANE = cod, NOMBRE_MPI = tratados$NOMBRE_MPI[k],
                       DPTO_CNMBR = tratados$DPTO_CNMBR[k], cohorte = tratados$G_dep[k],
                       att_simple_sin_i = s$att_simple, att_bal_sin_i = s$att_bal,
                       influencia_simple = base_full$att_simple - s$att_simple,
                       influencia_bal = base_full$att_bal - s$att_bal)
    hechos <- bind_rows(hechos, fila)
    write_csv(hechos, ruta_ck)
    cat(sprintf("  %2d/%d  sin %-25s  ATT bal %9.1f  (influencia %+8.1f)  [%.0f s]\n",
                k, nrow(tratados), substr(tratados$NOMBRE_MPI[k], 1, 25),
                s$att_bal, base_full$att_bal - s$att_bal,
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  tab_loo <- hechos %>% arrange(desc(abs(influencia_bal)))
  write_csv(tab_loo, ruta_ck)

  cat("\nLos diez municipios mas influyentes (sobre el ATT balanceado):\n")
  print(head(tab_loo %>% select(NOMBRE_MPI, cohorte, att_bal_sin_i, influencia_bal) %>%
               mutate(across(where(is.numeric), ~ round(.x, 1))), 10), row.names = FALSE)
  cat(sprintf("\nRango del ATT balanceado al excluir uno: [%.1f, %.1f] | completo: %.1f\n",
              min(tab_loo$att_bal_sin_i, na.rm = TRUE), max(tab_loo$att_bal_sin_i, na.rm = TRUE),
              base_full$att_bal))

  p <- ggplot(tab_loo %>% filter(!is.na(att_bal_sin_i)),
              aes(reorder(NOMBRE_MPI, att_bal_sin_i), att_bal_sin_i)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = base_full$att_bal, color = "firebrick") +
    geom_point(size = 2) +
    coord_flip() +
    labs(title = "ATT balanceado al excluir cada municipio tratado",
         subtitle = sprintf("Linea roja: muestra completa (%.1f ha). Especificacion principal, a = 0.",
                            base_full$att_bal),
         x = NULL, y = "ATT balanceado e = 0..5 sin el municipio (hectareas)") +
    theme_minimal() + theme(axis.text.y = element_text(size = 7))
  ggsave(file.path(DIR_FIG, "43_leave_one_out.png"), p, width = 8, height = 8, dpi = 150)
}

## =============================================================================
## 5. Exclusiones con bootstrap completo
## =============================================================================
titulo("3. EXCLUSIONES CON BOOTSTRAP COMPLETO")
exclusiones <- list()
cod_chaira <- tratados$COD_DANE[grepl("CHAIR", toupper(tratados$NOMBRE_MPI))]
if (length(cod_chaira)) exclusiones[["sin Cartagena del Chaira"]] <- cod_chaira
if (!is.null(tab_loo)) {
  top3 <- head(tab_loo$COD_DANE[!is.na(tab_loo$influencia_bal)], 3)
  exclusiones[[paste("sin los 3 mas influyentes:",
                     paste(tab_loo$NOMBRE_MPI[match(top3, tab_loo$COD_DANE)], collapse = ", "))]] <- top3
}
tab_exc <- do.call(rbind, lapply(names(exclusiones), function(nm) {
  cat("\n--", nm, "--\n")
  r <- estimar(muestra %>% filter(!COD_DANE %in% exclusiones[[nm]]), anticipacion = 0,
               bstrap = TRUE, etiqueta = nm)
  cbind(especificacion = nm, n_excluidos = length(exclusiones[[nm]]), resumir(r))
}))
if (!is.null(tab_exc)) {
  tab_exc <- tab_exc %>% mutate(ic_bal_inf = att_bal - 1.96 * ee_bal,
                                ic_bal_sup = att_bal + 1.96 * ee_bal)
  print(tab_exc %>% mutate(across(where(is.numeric), ~ round(.x, 2))), row.names = FALSE)
  write_csv(tab_exc, file.path(DIR_TAB, "43_exclusiones.csv"))
}

saveRDS(list(anticipacion = res_ant, tabla_anticipacion = tab_ant,
             leave_one_out = tab_loo, exclusiones = tab_exc),
        file.path(DIR_OUT, "43_robustez_anticipacion_y_loo.rds"))
cat("\nGuardado: outputs/43_robustez_anticipacion_y_loo.rds y tablas/figuras 43_*\n")
