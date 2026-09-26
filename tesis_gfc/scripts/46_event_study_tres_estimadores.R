## =============================================================================
## 46_event_study_tres_estimadores.R
##
## Estudio de eventos de la especificacion principal (D1 depurada, cohortes
## 2013-2019) con tres estimadores robustos a efectos heterogeneos, sobre la
## MISMA muestra, la MISMA ventana de eventos y la MISMA base universal g-1:
##
##   CS   Callaway y Sant'Anna (2021): DR, notyettreated, base_period universal.
##   SA   Sun y Abraham (2021), fixest::sunab, ref.p = -1, controles = nunca
##        tratados. Covariables de linea base interactuadas con el anio
##        (X_i' gamma_t), que es la forma de condicionar tendencias en una
##        regresion con efectos fijos de municipio.
##   ETW  Wooldridge (2021), paquete etwfe, cgroup = "never": estima tambien
##        los coeficientes previos contra g-1 y es comparable en la figura.
##   ETW-NY (solo post) cgroup = "notyet": la linea base es TODO el periodo
##        previo (equivale al estimador de imputacion de Borusyak et al.).
##        No entra a la figura: entra al texto, porque habla directamente del
##        problema del anio base g-1.
##
## Salidas:
##   outputs/tablas/46_event_study_tres_estimadores.csv
##   outputs/tablas/46_promedios_post.csv
##   outputs/figuras/46_event_study_tres_estimadores.png  (paleta del poster)
##
## USO (PowerShell, desde cualquier subcarpeta del repo):
##   Rscript scripts/46_event_study_tres_estimadores.R
## =============================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(ggplot2); library(did)
})
for (pkg in c("fixest", "etwfe")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Falta el paquete '", pkg, "'. Instala con install.packages('", pkg, "').")
  }
}

## --- Raiz canonica (mismo centinela que 40) ----------------------------------
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
DIR_TAB <- file.path(DIR_OUT, "tablas"); DIR_FIG <- file.path(DIR_OUT, "figuras")
for (d in c(DIR_TAB, DIR_FIG)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

## --- Parametros: IDENTICOS a 40/44. Revisar E_MIN contra 44_honestdid.R -------
SEMILLA   <- 20260824
OUTCOME   <- "loss_area_ha"
VENTANA   <- 2013:2019
E_MIN     <- -5          # ventana previa de la figura 44 (ajustar si 44 usa otra)
E_MAX     <- 5           # BALANCE_E: 2019 + 5 = 2024
BITERS    <- 999
COVS <- c("log_baseline_forest", "temp_media_c_base", "prec_anual_mm_base",
          "discapital_base", "disbogota_base", "distancia_mercado_base",
          "H_coca_base", "homicidios_base", "log_defor_pre_media", "defor_pre_tendencia")
CS_REF_BALANCEADO <- -464.89   # valor de la franja del poster: control de reproduccion

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)

## =============================================================================
## 1. Muestra (replica exacta de la seccion 1 de 40_did_depurada_y_dosis.R)
##    Si cambias 40, cambia esto: lo ideal es mover este bloque a R/muestra.R
## =============================================================================
panel <- readRDS(file.path(DIR_OUT, "panel_analisis_did.rds"))
panel$COD_DANE <- pad5(panel$COD_DANE)

COLS_PRE <- c("log_defor_pre_media", "defor_pre_tendencia", "log_baseline_forest")
if (!all(COLS_PRE %in% names(panel))) {
  ruta_cov <- file.path(DIR_TAB, "covariables_pre_municipio.csv")
  if (!file.exists(ruta_cov)) stop("Falta ", ruta_cov, ". Corre antes 02_matching_psm.R.")
  cov_pre <- read_csv(ruta_cov, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE)
  cov_pre$COD_DANE <- pad5(cov_pre$COD_DANE)
  cols_unir <- intersect(c("COD_DANE", COLS_PRE), names(cov_pre))
  panel <- panel %>% select(-any_of(setdiff(cols_unir, "COD_DANE"))) %>%
    left_join(cov_pre[, cols_unir], by = "COD_DANE")
}
faltan <- setdiff(COVS, names(panel))
if (length(faltan)) stop("Faltan covariables: ", paste(faltan, collapse = ", "))

trat <- read_csv(file.path(RAIZ_PROYECTO, "data/final/panel_con_tratamiento_actualizado.csv"),
                 col_select = all_of(c("COD_DANE", "anio_inicio_tratamiento_todas_fuentes_redd_depurada",
                                       "muestra_redd_depurada")),
                 col_types = cols(COD_DANE = col_character(), .default = col_double()),
                 show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>%
  group_by(COD_DANE) %>%
  summarise(G_dep = coalesce(min_na(anio_inicio_tratamiento_todas_fuentes_redd_depurada), 0),
            muestra_dep = min(muestra_redd_depurada), .groups = "drop")

panel <- panel %>% select(-any_of(c("G_dep", "muestra_dep"))) %>% inner_join(trat, by = "COD_DANE")

n_anios <- n_distinct(panel$year)
incompletos <- panel %>% group_by(COD_DANE) %>%
  summarise(ok = n() == n_anios && all(!is.na(.data[[OUTCOME]])), .groups = "drop") %>%
  filter(!ok) %>% pull(COD_DANE)
panel <- panel %>% filter(!COD_DANE %in% incompletos)

muestra <- panel %>% filter(muestra_dep == 1, G_dep == 0 | G_dep %in% VENTANA)
## Covariables con NA: fixest y etwfe descartarian filas en silencio y la
## muestra dejaria de ser la de CS. Se exige muestra completa y se reporta.
sin_cov <- unique(muestra$COD_DANE[!complete.cases(muestra[, COVS])])
if (length(sin_cov)) {
  cat("Aviso:", length(sin_cov), "municipios con covariables faltantes, fuera de las tres estimaciones\n")
  muestra <- muestra %>% filter(!COD_DANE %in% sin_cov)
}
m1 <- distinct(muestra, COD_DANE, G_dep)
cat(sprintf("Muestra comun: %d municipios | %d tratados | %d nunca tratados\n",
            nrow(m1), sum(m1$G_dep > 0), sum(m1$G_dep == 0)))

## Covariables centradas: no cambia los coeficientes de tratamiento en SA,
## pero en ETWFE los efectos se evaluan en la media de X y centrar hace
## explicita esa eleccion (Wooldridge 2021, sec. 6).
for (v in COVS) muestra[[v]] <- muestra[[v]] - mean(muestra[[v]])

res <- list()

## =============================================================================
## 2. Callaway y Sant'Anna, base universal
## =============================================================================
set.seed(SEMILLA)
cs <- tryCatch(
  att_gt(yname = OUTCOME, tname = "year", idname = "id_num", gname = "G_dep",
         xformla = reformulate(COVS), data = muestra, control_group = "notyettreated",
         est_method = "dr", base_period = "universal",
         bstrap = TRUE, biters = BITERS, cband = FALSE, clustervars = "id_num"),
  error = function(e) { cat("ERROR CS:", conditionMessage(e), "\n"); NULL })
if (!is.null(cs)) {
  es <- aggte(cs, type = "dynamic", min_e = E_MIN, max_e = E_MAX, na.rm = TRUE)
  res$cs <- data.frame(estimador = "Callaway-Sant'Anna (DR)", e = es$egt,
                       att = es$att.egt, se = es$se.egt)
  bal <- aggte(cs, type = "dynamic", balance_e = E_MAX, min_e = 0, max_e = E_MAX, na.rm = TRUE)
  cat(sprintf("CS balanceado e=0..%d: %.2f (EE %.2f) | poster: %.2f\n",
              E_MAX, bal$overall.att, bal$overall.se, CS_REF_BALANCEADO))
  if (abs(bal$overall.att - CS_REF_BALANCEADO) > 5)
    warning("CS no reproduce la franja del poster: revisar muestra/parametros contra 44 antes de usar la figura.")
}

## =============================================================================
## 3. Sun y Abraham (fixest::sunab)
##    Y_it = a_i + l_t + sum_g sum_{e != -1} d_{g,e} 1{G=g}1{t-g=e} + X_i' g_t + u_it
##    El coeficiente por e es el promedio de d_{g,e} ponderado por la
##    participacion de cada cohorte entre las observadas en e.
## =============================================================================
muestra$G_sa <- ifelse(muestra$G_dep == 0, 10000, muestra$G_dep)   # nunca tratados fuera del rango
anio_ref <- min(muestra$year)
fml_sa <- as.formula(paste0(
  OUTCOME, " ~ sunab(G_sa, year, ref.p = -1) + ",
  paste0("i(year, ", COVS, ", ref = ", anio_ref, ")", collapse = " + "),
  " | id_num + year"))
sa <- tryCatch(fixest::feols(fml_sa, data = muestra, cluster = ~id_num),
               error = function(e) { cat("ERROR SA:", conditionMessage(e), "\n"); NULL })
if (!is.null(sa)) {
  ct <- as.data.frame(fixest::coeftable(sa))           # agregado por periodo relativo
  ct$nombre <- rownames(ct)
  ct <- ct[grepl("^year::-?\\d+$", ct$nombre), ]         # descarta las interacciones X x anio
  res$sa <- data.frame(estimador = "Sun-Abraham", e = as.integer(sub("^year::", "", ct$nombre)),
                       att = ct[, 1], se = ct[, 2]) %>% filter(e >= E_MIN, e <= E_MAX)
}

## =============================================================================
## 4. Wooldridge (ETWFE)
## =============================================================================
fml_et <- as.formula(paste(OUTCOME, "~", paste(COVS, collapse = " + ")))
correr_etwfe <- function(cgroup) {
  tryCatch({
    m <- etwfe::etwfe(fml = fml_et, tvar = year, gvar = G_dep, data = muestra,
                      ivar = id_num, cgroup = cgroup, vcov = ~id_num)
    etwfe::emfx(m, type = "event", post_only = (cgroup == "notyet"))
  }, error = function(e) { cat("ERROR ETWFE (", cgroup, "):", conditionMessage(e), "\n"); NULL })
}
et_never <- correr_etwfe("never")      # figura: previos y posteriores contra g-1
et_notyet <- correr_etwfe("notyet")    # texto: base = todo el periodo previo
a_df <- function(x, etiqueta) {
  if (is.null(x)) return(NULL)
  x <- as.data.frame(x)
  data.frame(estimador = etiqueta, e = as.integer(x$event), att = x$estimate, se = x$std.error) %>%
    filter(e >= E_MIN, e <= E_MAX)
}
res$et  <- a_df(et_never,  "Wooldridge (ETWFE)")
res$etn <- a_df(et_notyet, "Wooldridge, base = todo el previo")

## =============================================================================
## 5. Tabla, promedios post y figura
## =============================================================================
tab <- bind_rows(res) %>%
  mutate(ic_inf = att - 1.96 * se, ic_sup = att + 1.96 * se)
## Normalizacion explicita de la base: e = -1 vale 0 por construccion en los
## estimadores anclados en g-1 (no en ETWFE not-yet, que no tiene previos).
anclados <- setdiff(unique(tab$estimador), "Wooldridge, base = todo el previo")
tab <- bind_rows(tab, data.frame(estimador = anclados, e = -1L, att = 0, se = 0,
                                 ic_inf = 0, ic_sup = 0)) %>%
  distinct(estimador, e, .keep_all = TRUE) %>% arrange(estimador, e)
write_csv(tab, file.path(DIR_TAB, "46_event_study_tres_estimadores.csv"))

## Promedio simple de e = 0..E_MAX (misma agregacion que el ATT balanceado de CS).
## El EE ignora la covarianza entre coeficientes: es orientativo. Para CS usar
## el EE de aggte impreso arriba.
prom <- tab %>% filter(e >= 0, e <= E_MAX) %>% group_by(estimador) %>%
  summarise(att_post_prom = mean(att), n_e = n(), .groups = "drop")
prom_pre <- tab %>% filter(e < -1) %>% group_by(estimador) %>%
  summarise(att_pre_prom = mean(att), .groups = "drop")
prom <- left_join(prom, prom_pre, by = "estimador")
print(prom)
write_csv(prom, file.path(DIR_TAB, "46_promedios_post.csv"))

## Figura con la paleta del poster. Textos con escapes Unicode para evitar la
## corrupcion de acentos vista en ggsave bajo el locale de Windows.
colores <- c("Callaway-Sant'Anna (DR)" = "#B8860F", "Sun-Abraham" = "#1D4F3F",
             "Wooldridge (ETWFE)" = "#8F3B1F")
fig <- tab %>% filter(estimador %in% names(colores)) %>%
  mutate(estimador = factor(estimador, levels = names(colores)))
p <- ggplot(fig, aes(e, att, color = estimador, shape = estimador)) +
  geom_hline(yintercept = 0, color = "#5B6E63", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "#5B6E63", linewidth = 0.4) +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup),
                  position = position_dodge(width = 0.55), linewidth = 0.6, size = 0.45) +
  scale_color_manual(values = colores, name = NULL) +
  scale_shape_manual(values = c(16, 15, 17), name = NULL) +
  scale_x_continuous(breaks = E_MIN:E_MAX) +
  labs(x = "A\u00f1os desde la adopci\u00f3n (e)",
       y = "Efecto sobre la p\u00e9rdida anual de bosque (ha)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F2F4EF", color = NA),
        text = element_text(color = "#12261E"))
dispositivo <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
ggsave(file.path(DIR_FIG, "46_event_study_tres_estimadores.png"), p,
       width = 9, height = 5.5, dpi = 300, device = dispositivo, bg = "#F2F4EF")
cat("Guardado: outputs/figuras/46_event_study_tres_estimadores.png\n",
    "         outputs/tablas/46_event_study_tres_estimadores.csv\n")
