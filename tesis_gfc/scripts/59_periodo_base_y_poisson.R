## =============================================================================
## 59_periodo_base_y_poisson.R
##
## Tarea A.2 (OE2) -- ¿El efecto depende del periodo base?
##   (a) CS sin covariables, base g-1 (referencia; debe reproducir el 40)
##   (b) CS con anticipacion 1..4 (base g-2 ... g-5)
##   (c) Imputacion de Borusyak, Jaravel y Spiess (2024): contrafactual con
##       efectos fijos estimados sobre todas las observaciones no tratadas
##   (d) ETWFE de Wooldridge: todos los anios previos como referencia
## Tarea A.4 -- Robustez: ETWFE Poisson (family = "poisson")
##
## Regla fijada antes de correr (D13): si el signo cambia o el efecto se
## reduce en mas de la mitad frente a (a), se concluye que el efecto depende
## de la linea base. Las comparaciones son SIN covariables porque (c) y (d)
## no tienen el ajuste doblemente robusto.
##
## Muestra: la del 40 + controles limpios (36 tratados, 986 controles), igual
## que el 58. Errores agrupados por municipio en todos los estimadores.
## Salidas:
##   outputs/tablas/59_periodo_base_y_poisson.csv
##   outputs/tablas/59_estudio_eventos_cs_vs_imputacion.csv
##   outputs/figuras/59_estudio_eventos_cs_vs_imputacion.png
## =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(did)
})
for (p in c("didimputation", "etwfe", "fixest", "marginaleffects"))
  if (!requireNamespace(p, quietly = TRUE)) stop("Falta el paquete ", p, ": install.packages('", p, "')")

## --- Raiz canonica -----------------------------------------------------------
RAIZ <- local({
  d <- normalizePath(getwd(), winslash = "/")
  repeat {
    if (file.exists(file.path(d, "data/.raiz_canonica"))) break
    p <- dirname(d); if (p == d) stop("No encuentro data/.raiz_canonica: corre desde tesis_gfc/"); d <- p
  }
  d
})
DIR_OUT <- file.path(RAIZ, "outputs"); DIR_TAB <- file.path(DIR_OUT, "tablas"); DIR_FIG <- file.path(DIR_OUT, "figuras")
for (d in c(DIR_TAB, DIR_FIG)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

## --- Parametros (iguales al 40) ---------------------------------------------
OUTCOME   <- "loss_area_ha"
SEMILLA   <- 20260824
BITERS    <- 999
REF_40    <- -183.29          # CS sin covariables del 40: la referencia debe reproducirlo
E_PRE     <- 5
E_POST    <- 5

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
titulo <- function(t) cat("\n", strrep("=", 76), "\n", t, "\n", strrep("=", 76), "\n", sep = "")
fila   <- function(est, base, att, ee, nota = "") data.frame(
  estimador = est, periodo_base = base, att = att, ee = ee,
  ic_inf = att - 1.96 * ee, ic_sup = att + 1.96 * ee, nota = nota)

## =============================================================================
## 1. Muestra (40 + controles limpios), identica a la del 58
## =============================================================================
muestra <- readRDS(file.path(DIR_OUT, "40_muestra_A.rds"))
muestra$COD_DANE <- pad5(muestra$COD_DANE)
source(file.path(RAIZ, "scripts/comun_controles_limpios.R"))
cont <- municipios_con_proyecto(file.path(RAIZ, "data/final/panel_con_tratamiento_actualizado.csv"))
muestra <- aplicar_controles_limpios(muestra, cont, col_g = "G_dep")

mun <- muestra %>% distinct(COD_DANE, G_dep)
cat(sprintf("Muestra: %d municipios | %d tratados | %d controles\n",
            nrow(mun), sum(mun$G_dep > 0), sum(mun$G_dep == 0)))
if (sum(mun$G_dep == 0) != 986 || sum(mun$G_dep > 0) != 36)
  stop("La muestra no coincide con la principal (36 tratados, 986 controles).")

d <- muestra %>% select(id_num, COD_DANE, year, G_dep, all_of(OUTCOME)) %>%
  mutate(id_num = as.integer(id_num), year = as.integer(year), G_dep = as.integer(G_dep))
stopifnot(!anyNA(d[[OUTCOME]]))
tab <- list()

## =============================================================================
## 2. CS: referencia (g-1) y anticipacion 1..4
## =============================================================================
titulo("2. CALLAWAY Y SANT'ANNA, SIN COVARIABLES: PERIODO BASE g-1 ... g-5")
cs <- function(antic, base_period = "varying") {
  set.seed(SEMILLA)
  tryCatch(suppressWarnings(att_gt(
    yname = OUTCOME, tname = "year", idname = "id_num", gname = "G_dep", xformla = ~1,
    data = d, control_group = "notyettreated", anticipation = antic, base_period = base_period,
    bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "id_num")),
    error = function(e) { cat("ERROR CS (antic", antic, "):", conditionMessage(e), "\n"); NULL })
}
for (k in 0:4) {
  out <- cs(k); if (is.null(out)) next
  s <- aggte(out, type = "simple", na.rm = TRUE)
  base <- sprintf("g-%d", k + 1)
  cat(sprintf("  base %-4s ATT %8.2f (EE %7.2f)\n", base, s$overall.att, s$overall.se))
  tab[[length(tab) + 1]] <- fila("Callaway y Sant'Anna", base, s$overall.att, s$overall.se,
                                 if (k == 0) "referencia" else "")
  if (k == 0 && abs(s$overall.att - REF_40) > 0.5)
    stop(sprintf("La referencia (%.2f) no reproduce el 40 (%.2f): la muestra difiere.", s$overall.att, REF_40))
}
REF <- tab[[1]]$att

## Estudio de eventos CS con base universal (g-1), para la figura
cs_u <- cs(0, base_period = "universal")
es_cs <- if (!is.null(cs_u)) {
  e <- aggte(cs_u, type = "dynamic", min_e = -E_PRE, max_e = E_POST, na.rm = TRUE)
  data.frame(estimador = "Callaway y Sant'Anna (base g-1)", e = e$egt, att = e$att.egt, ee = e$se.egt)
} else NULL

## =============================================================================
## 3. Imputacion de Borusyak, Jaravel y Spiess (2024)
## =============================================================================
titulo("3. IMPUTACION (BORUSYAK, JARAVEL Y SPIESS, 2024)")
imp <- tryCatch(didimputation::did_imputation(
  data = d, yname = OUTCOME, gname = "G_dep", tname = "year", idname = "id_num",
  cluster_var = "id_num"), error = function(e) { cat("ERROR imputacion:", conditionMessage(e), "\n"); NULL })
if (!is.null(imp)) {
  cat(sprintf("  ATT %8.2f (EE %7.2f)\n", imp$estimate[1], imp$std.error[1]))
  tab[[length(tab) + 1]] <- fila("Imputacion (BJS)", "todo el periodo previo", imp$estimate[1], imp$std.error[1])
}
imp_es <- tryCatch(didimputation::did_imputation(
  data = d, yname = OUTCOME, gname = "G_dep", tname = "year", idname = "id_num",
  cluster_var = "id_num", horizon = 0:E_POST, pretrends = -E_PRE:-1),
  error = function(e) { cat("ERROR imputacion (eventos):", conditionMessage(e), "\n"); NULL })
es_imp <- if (!is.null(imp_es)) {
  data.frame(estimador = "Imputacion (BJS)", e = as.integer(imp_es$term),
             att = imp_es$estimate, ee = imp_es$std.error)
} else NULL
if (!is.null(es_imp)) {
  cat("  Coeficientes pretratamiento (prueba de tendencias):\n")
  print(es_imp %>% filter(e < 0) %>% mutate(ic_inf = att - 1.96 * ee, ic_sup = att + 1.96 * ee), digits = 4)
}

## =============================================================================
## 4. ETWFE de Wooldridge: lineal (A.2) y Poisson (A.4)
## =============================================================================
titulo("4. ETWFE (WOOLDRIDGE): LINEAL Y POISSON")
etw <- function(familia) {
  m <- tryCatch(suppressWarnings(
    if (is.null(familia)) etwfe::etwfe(fml = loss_area_ha ~ 0, tvar = year, gvar = G_dep,
                                       data = d, vcov = ~id_num)
    else etwfe::etwfe(fml = loss_area_ha ~ 0, tvar = year, gvar = G_dep,
                      data = d, vcov = ~id_num, family = familia)),
    error = function(e) { cat("ERROR etwfe:", conditionMessage(e), "\n"); NULL })
  if (is.null(m)) return(NULL)
  tryCatch(etwfe::emfx(m, type = "simple"), error = function(e) { cat("ERROR emfx:", conditionMessage(e), "\n"); NULL })
}
w_lin <- etw(NULL)
if (!is.null(w_lin)) {
  cat(sprintf("  Lineal:  ATT %8.2f (EE %7.2f)\n", w_lin$estimate[1], w_lin$std.error[1]))
  tab[[length(tab) + 1]] <- fila("ETWFE lineal (Wooldridge)", "todo el periodo previo",
                                 w_lin$estimate[1], w_lin$std.error[1])
}
w_poi <- etw("poisson")
if (!is.null(w_poi)) {
  ## Efecto proporcional: ATT sobre el contrafactual medio de los tratados en
  ## el periodo post (observado - ATT).
  post <- d %>% filter(G_dep > 0, year >= G_dep)
  y_obs <- mean(post[[OUTCOME]]); rel <- w_poi$estimate[1] / (y_obs - w_poi$estimate[1])
  cat(sprintf("  Poisson: ATT %8.2f (EE %7.2f) | efecto proporcional %.1f %%\n",
              w_poi$estimate[1], w_poi$std.error[1], 100 * rel))
  tab[[length(tab) + 1]] <- fila("ETWFE Poisson (A.4)", "todo el periodo previo",
                                 w_poi$estimate[1], w_poi$std.error[1],
                                 sprintf("proporcional %.1f %%", 100 * rel))
}

## =============================================================================
## 5. Regla D13 y salidas
## =============================================================================
titulo("5. REGLA D13: ¿EL EFECTO DEPENDE DE LA LINEA BASE?")
res <- do.call(rbind, tab) %>%
  mutate(cambio_vs_ref_pct = 100 * (att - REF) / abs(REF),
         cambia_signo = sign(att) != sign(REF),
         reduce_mas_mitad = abs(att) < 0.5 * abs(REF),
         dispara_regla = (cambia_signo | reduce_mas_mitad) & nota != "referencia" &
           !grepl("Poisson", estimador))
print(res %>% mutate(across(where(is.numeric), ~ round(.x, 2))), row.names = FALSE)
alt <- res %>% filter(estimador %in% c("Imputacion (BJS)", "ETWFE lineal (Wooldridge)"))
cat(sprintf("\nEstimadores con todo el periodo previo que disparan la regla: %d de %d\n",
            sum(alt$dispara_regla), nrow(alt)))
cat(if (any(alt$dispara_regla))
  "-> Segun D13: el efecto DEPENDE de la linea base.\n" else
    "-> Segun D13: el efecto NO depende de la linea base.\n")

write_csv(res, file.path(DIR_TAB, "59_periodo_base_y_poisson.csv"))
es <- bind_rows(es_cs, es_imp)
if (nrow(es)) {
  write_csv(es, file.path(DIR_TAB, "59_estudio_eventos_cs_vs_imputacion.csv"))
  p <- ggplot(es, aes(e, att, colour = estimador)) +
    geom_hline(yintercept = 0, colour = "grey50") + geom_vline(xintercept = -0.5, linetype = 2, colour = "grey60") +
    geom_pointrange(aes(ymin = att - 1.96 * ee, ymax = att + 1.96 * ee),
                    position = position_dodge(width = 0.4)) +
    labs(x = "Anios desde la adopcion", y = "Efecto sobre la deforestacion (ha)", colour = NULL,
         title = "El efecto estimado segun el periodo de referencia",
         subtitle = "D1 depurada, 36 tratados, 986 controles limpios; IC 95 % puntuales") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggsave(file.path(DIR_FIG, "59_estudio_eventos_cs_vs_imputacion.png"), p, width = 9, height = 5.5, dpi = 200)
}
cat("\nGuardado: outputs/tablas/59_*.csv y outputs/figuras/59_estudio_eventos_cs_vs_imputacion.png\n")