## =============================================================================
## 48_soporte_comun.R
##
## Robustez de la especificacion principal a la falta de soporte comun.
## Motivacion: en cada estimacion DR hay 127-138 avisos "glm.fit: fitted
## probabilities numerically 0 or 1": el modelo de propension separa casi
## perfectamente tratados y controles, y con 37 tratados el estimador queda
## apoyado en pocos controles con pesos enormes.
##
## Diseno FIJADO ANTES DE CORRER (27/09/2026):
##   1. Diagnostico: propension municipal p_i (logit, mismas covariables de la
##      especificacion principal), distribucion por tramos, tamanio muestral
##      efectivo de los controles con pesos ATT w_i = p_i / (1 - p_i):
##          ESS_C = (sum w)^2 / sum w^2
##      y controles que concentran el peso.
##   2. Tres estimadores de Callaway-Sant'Anna sobre la misma muestra:
##      dr (principal), reg (regresion del outcome, no usa propension) e ipw.
##   3. Recorte por soporte comun, a tratados y controles:
##        R1: p en [0.01, 0.99]   (regla anunciada antes de correr)
##        R2: p en [0.10, 0.90]   (Crump, Hotz, Imbens y Mitnik 2009; sensibilidad)
##   REGLA DE LECTURA: la principal (DR, muestra completa) es ROBUSTA al soporte
##   comun si dr, reg e ipw en la muestra R1 tienen su mismo signo y estan a
##   menos de 1 EE de ella.
##
## Especificacion base = la del 40: D1 depurada, cohortes 2013-2019, controles
## limpios, notyettreated, covariables del 02 corregido, Hansen 2001-2024.
## La celda dr x completa debe reproducir -207.9 ha (control de consistencia).
##
## Nota: la propension de att_gt se estima por par (g, t); aqui se estima UNA
## propension municipal (tratado en ventana vs nunca tratado) solo como
## instrumento de diagnostico y de recorte, que es la practica habitual.
##
## USO
##   Rscript scripts/48_soporte_comun.R
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(did)
  library(ggplot2)
})

RAIZ_PROYECTO <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "data/.raiz_canonica"))) {
    p <- dirname(d)
    if (identical(p, d)) stop("No encuentro data/.raiz_canonica subiendo desde ", getwd())
    d <- p
  }
  d
})
DIR_OUT <- file.path(RAIZ_PROYECTO, "outputs")
DIR_TAB <- file.path(DIR_OUT, "tablas")
DIR_FIG <- file.path(DIR_OUT, "figuras")
for (d in c(DIR_TAB, DIR_FIG)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
source(file.path(RAIZ_PROYECTO, "scripts/comun_controles_limpios.R"))
source(file.path(RAIZ_PROYECTO, "scripts/comun_covariables_pre.R"))

## --- Parametros (fijados antes de correr) ------------------------------------
SEMILLA   <- 20260824
VENTANA   <- 2013:2019
BITERS    <- 999
BAL_E     <- 5
OUTCOME   <- "loss_area_ha"
RECORTES  <- list(completa = c(0, 1), R1 = c(0.01, 0.99), R2 = c(0.10, 0.90))
METODOS   <- c("dr", "reg", "ipw")
PRINCIPAL_ESPERADA <- -207.9            # 40 / 47 V1 x limpios
X <- ~ log_baseline_forest + temp_media_c_base + prec_anual_mm_base +
  discapital_base + disbogota_base + distancia_mercado_base + H_coca_base +
  homicidios_base + log_defor_pre_media + defor_pre_tendencia

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(t) cat("\n", strrep("=", 74), "\n", t, "\n", strrep("=", 74), "\n", sep = "")

## =============================================================================
## Funciones de diagnostico (R base; probadas por separado)
## =============================================================================

## Propension municipal: logit de "tratado en ventana" sobre las covariables
## de linea base (invariantes en el tiempo: se toma una fila por municipio).
propension_municipal <- function(d, formula_x) {
  vars <- all.vars(formula_x)
  m <- d[!duplicated(d$COD_DANE), c("COD_DANE", "G_dep", vars)]
  m$tratado <- as.integer(m$G_dep > 0)
  m <- m[stats::complete.cases(m[, vars]), ]
  f <- stats::update(formula_x, tratado ~ .)
  n_sep <- 0L
  fit <- withCallingHandlers(stats::glm(f, data = m, family = stats::binomial()),
    warning = function(w) {
      if (grepl("fitted probabilities numerically 0 or 1", conditionMessage(w))) n_sep <<- n_sep + 1L
      invokeRestart("muffleWarning")
    })
  m$p <- stats::fitted(fit)
  attr(m, "separacion") <- n_sep > 0
  m
}

## Tamanio muestral efectivo de los controles con pesos ATT (odds)
ess_controles <- function(p_control) {
  p <- pmin(p_control, 1 - 1e-12)
  w <- p / (1 - p)
  if (!length(w) || sum(w) == 0) return(NA_real_)
  sum(w)^2 / sum(w^2)
}

## Participacion de los k controles de mayor peso en el peso total
concentracion_peso <- function(p_control, k = 10) {
  w <- p_control / (1 - pmin(p_control, 1 - 1e-12))
  s <- sort(w, decreasing = TRUE)
  sum(s[seq_len(min(k, length(s)))]) / sum(s)
}

tramos <- function(p, grupo) {
  cortes <- c(0, 0.001, 0.01, 0.05, 0.10, 0.25, 0.50, 0.90, 0.99, 1)
  t <- table(cut(p, cortes, include.lowest = TRUE), grupo)
  as.data.frame.matrix(t)
}

## =============================================================================
## Datos: igual que el 40 (D1 depurada, ventana, controles limpios, covariables
## del 02 corregido)
## =============================================================================
panel <- readRDS(file.path(DIR_OUT, "panel_analisis_did.rds"))
panel$COD_DANE <- pad5(panel$COD_DANE)
panel <- unir_covariables_pre(panel, file.path(DIR_TAB, "covariables_pre_municipio.csv"))
falt <- setdiff(all.vars(X), names(panel))
if (length(falt)) stop("Faltan covariables: ", paste(falt, collapse = ", "))

RUTA_TRAT <- file.path(RAIZ_PROYECTO, "data/final/panel_con_tratamiento_actualizado.csv")
COL_DEP <- "anio_inicio_tratamiento_todas_fuentes_redd_depurada"
COL_MUE <- "muestra_redd_depurada"
trat <- read_csv(RUTA_TRAT, col_select = all_of(c("COD_DANE", COL_DEP, COL_MUE)),
                 col_types = cols(COD_DANE = col_character(), .default = col_double()),
                 show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>% group_by(COD_DANE) %>%
  summarise(G_dep = coalesce(min_na(.data[[COL_DEP]]), 0),
            muestra_dep = min(.data[[COL_MUE]]), .groups = "drop")

base <- panel %>% select(-any_of(c("G_dep", "muestra_dep"))) %>%
  inner_join(trat, by = "COD_DANE") %>%
  filter(muestra_dep == 1, G_dep == 0 | G_dep %in% VENTANA)
base <- aplicar_controles_limpios(as.data.frame(base), municipios_con_proyecto(RUTA_TRAT), "G_dep")

## =============================================================================
## 1. Diagnostico de solapamiento
## =============================================================================
titulo("1. DIAGNOSTICO DE SOPORTE COMUN")
ps <- propension_municipal(base, X)
cat(sprintf("Municipios en el modelo de propension: %d (%d tratados, %d controles)%s\n",
            nrow(ps), sum(ps$tratado), sum(ps$tratado == 0),
            if (attr(ps, "separacion")) " | AVISO: separacion (p numericamente 0 o 1)" else ""))
cat("\nDistribucion de la propension por tramo:\n")
print(tramos(ps$p, ifelse(ps$tratado == 1, "tratados", "controles")))

pc <- ps$p[ps$tratado == 0]
ess <- ess_controles(pc)
cat(sprintf("\nTamanio muestral efectivo de los controles (pesos ATT): %.1f de %d (%.1f%%)\n",
            ess, length(pc), 100 * ess / length(pc)))
cat(sprintf("Peso concentrado en los 10 controles mas pesados: %.1f%%\n",
            100 * concentracion_peso(pc, 10)))

top <- ps[ps$tratado == 0, ] %>% mutate(peso = p / (1 - pmin(p, 1 - 1e-12))) %>%
  arrange(desc(peso)) %>% mutate(peso_pct = 100 * peso / sum(peso)) %>% head(15)
nombres <- panel %>% distinct(COD_DANE, .keep_all = TRUE) %>%
  select(any_of(c("COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR")))
top <- top %>% left_join(nombres, by = "COD_DANE")
cat("\nControles con mayor peso:\n")
print(as.data.frame(top %>% select(any_of(c("COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR")), p, peso_pct)),
      row.names = FALSE, digits = 3)

ps_out <- ps %>% left_join(nombres, by = "COD_DANE")
write_csv(ps_out, file.path(DIR_TAB, "48_propension_municipal.csv"))

p_fig <- ggplot(ps, aes(p, fill = factor(tratado, labels = c("Controles", "Tratados")))) +
  geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
  geom_vline(xintercept = c(0.01, 0.10, 0.90, 0.99), linetype = "dashed", color = "grey40") +
  scale_x_sqrt() +
  labs(title = "Soporte comun: propension municipal a tener un proyecto REDD+",
       subtitle = "Logit con las covariables de la especificacion principal. Lineas: recortes 0,01-0,99 y 0,10-0,90",
       x = "Propension estimada (escala raiz cuadrada)", y = "Municipios", fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(DIR_FIG, "48_soporte_comun.png"), p_fig, width = 9, height = 5.5, dpi = 150)

## =============================================================================
## 2-3. Estimadores x recortes
## =============================================================================
correr <- function(d, metodo, xformla, etq) {
  cat("\n--- ", etq, " ---\n", sep = "")
  d <- as.data.frame(d); d$.gname <- d$G_dep
  avisos <- character(0)
  set.seed(SEMILLA)
  out <- withCallingHandlers(
    tryCatch(att_gt(yname = OUTCOME, tname = "year", idname = "id_num", gname = ".gname",
                    xformla = xformla, data = d, control_group = "notyettreated",
                    est_method = metodo, bstrap = TRUE, biters = BITERS, cband = TRUE,
                    clustervars = "id_num"),
             error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }),
    warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") })
  if (is.null(out)) return(NULL)
  s   <- aggte(out, type = "simple", na.rm = TRUE)
  bal <- tryCatch(aggte(out, type = "dynamic", balance_e = BAL_E, min_e = 0, max_e = BAL_E,
                        na.rm = TRUE), error = function(e) NULL)
  n_sep <- sum(grepl("fitted probabilities numerically 0 or 1", avisos))
  cat(sprintf("ATT %.1f (EE %.1f) IC95 [%.1f, %.1f] | g-t NA %d/%d | avisos de separacion: %d\n",
              s$overall.att, s$overall.se, s$overall.att - 1.96 * s$overall.se,
              s$overall.att + 1.96 * s$overall.se, sum(is.na(out$att)), length(out$att), n_sep))
  data.frame(att = s$overall.att, ee = s$overall.se,
             ic_inf = s$overall.att - 1.96 * s$overall.se,
             ic_sup = s$overall.att + 1.96 * s$overall.se,
             att_balanceado = if (is.null(bal)) NA_real_ else bal$overall.att,
             gt_na = sum(is.na(out$att)), gt_total = length(out$att), avisos_separacion = n_sep,
             n_tratados = n_distinct(d$COD_DANE[d$G_dep > 0]),
             n_controles = n_distinct(d$COD_DANE[d$G_dep == 0]), row.names = NULL)
}

titulo("2-3. ESTIMADORES x RECORTE DE SOPORTE COMUN")
filas <- list(); recortados <- list()
for (rc in names(RECORTES)) {
  lim <- RECORTES[[rc]]
  dentro <- ps$COD_DANE[ps$p >= lim[1] & ps$p <= lim[2]]
  fuera  <- ps[!ps$COD_DANE %in% dentro, ]
  d_rc   <- base[base$COD_DANE %in% dentro, ]
  cat(sprintf("\n[%s] p en [%.2f, %.2f]: salen %d tratados y %d controles | quedan %d tratados, %d controles\n",
              rc, lim[1], lim[2], sum(fuera$tratado), sum(fuera$tratado == 0),
              sum(ps$tratado[ps$COD_DANE %in% dentro]), sum(ps$tratado[ps$COD_DANE %in% dentro] == 0)))
  if (sum(fuera$tratado)) {
    tr_fuera <- fuera[fuera$tratado == 1, ] %>% left_join(nombres, by = "COD_DANE")
    cat("  tratados fuera del soporte:\n")
    print(as.data.frame(tr_fuera %>% select(any_of(c("COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR")), G_dep, p)),
          row.names = FALSE, digits = 3)
  }
  if (nrow(fuera)) recortados[[rc]] <- cbind(recorte = rc, fuera)
  cat(sprintf("  ESS de controles tras el recorte: %.1f\n",
              ess_controles(ps$p[ps$COD_DANE %in% dentro & ps$tratado == 0])))
  for (m in METODOS) {
    r <- correr(d_rc, m, X, sprintf("%s | est_method = %s", rc, m))
    if (!is.null(r)) filas[[paste(rc, m)]] <- cbind(recorte = rc, metodo = m, r)
  }
  r0 <- correr(d_rc, "dr", ~1, sprintf("%s | sin covariables (efecto de la muestra sola)", rc))
  if (!is.null(r0)) filas[[paste(rc, "sincov")]] <- cbind(recorte = rc, metodo = "sincov", r0)
}
tabla <- do.call(rbind, filas)
write_csv(tabla, file.path(DIR_TAB, "48_soporte_comun_estimaciones.csv"))
if (length(recortados)) write_csv(do.call(rbind, recortados) %>% left_join(nombres, by = "COD_DANE"),
                                  file.path(DIR_TAB, "48_municipios_fuera_de_soporte.csv"))

## =============================================================================
## Regla de lectura
## =============================================================================
titulo("RESUMEN Y REGLA DE LECTURA")
print(tabla[, c("recorte", "metodo", "n_tratados", "n_controles", "att", "ee", "ic_inf", "ic_sup",
                "gt_na", "avisos_separacion")], row.names = FALSE, digits = 3)

ppal <- tabla[tabla$recorte == "completa" & tabla$metodo == "dr", ]
if (nrow(ppal)) {
  cat(sprintf("\nControl de consistencia: DR muestra completa = %.1f (esperado %.1f) -> %s\n",
              ppal$att, PRINCIPAL_ESPERADA,
              if (abs(ppal$att - PRINCIPAL_ESPERADA) < 1) "OK" else "NO COINCIDE: revisar insumos"))
  r1 <- tabla[tabla$recorte == "R1" & tabla$metodo %in% METODOS, ]
  ok_signo <- all(sign(r1$att) == sign(ppal$att))
  ok_dist  <- all(abs(r1$att - ppal$att) < ppal$ee)
  cat(sprintf("Regla (R1, dr/reg/ipw): mismo signo = %s | todos a < 1 EE (%.1f) de la principal = %s\n",
              ok_signo, ppal$ee, ok_dist))
  cat(sprintf(">>> La especificacion principal %s ROBUSTA al soporte comun.\n",
              if (ok_signo && ok_dist && nrow(r1) == length(METODOS)) "ES" else "NO ES"))
}
cat("\nGuardado: outputs/tablas/48_soporte_comun_estimaciones.csv\n")
cat("Guardado: outputs/tablas/48_propension_municipal.csv, 48_municipios_fuera_de_soporte.csv\n")
cat("Figura:   outputs/figuras/48_soporte_comun.png\n")
