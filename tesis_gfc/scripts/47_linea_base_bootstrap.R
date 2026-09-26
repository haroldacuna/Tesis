## =============================================================================
## 47_linea_base_bootstrap.R
##
## Corrige y completa 46_event_study_tres_estimadores.R.
##
## PROBLEMA EN 46: los EE de etwfe son invalidos. etwfe interactua CADA
## covariable con cada celda cohorte x anio. Con 10 covariables y cohortes de
## 5-6 municipios, cada celda tiene ~11 parametros para 5-6 tratados: el ajuste
## de los tratados es casi exacto, sus residuos son ~0 y el sandwich agrupado
## no ve variacion (EE de 17 ha en e = -2 contra 324 en Sun-Abraham). El aviso
## "VCOV matrix is not positive definite" es el sintoma.
##
## SOLUCION: se estiman los dos estimadores de regresion directamente en fixest
## con covariables como X_i' gamma_t (igual que Sun-Abraham en 46) e inferencia
## por bootstrap de municipios estratificado por cohorte:
##
##   SA   celdas cohorte x anio para todo t != g-1  -> controles = nunca tratados.
##        Es numericamente Sun-Abraham, y tambien Wooldridge (2021) con
##        cgroup = "never": los dos saturan cohorte x tiempo y anclan en g-1.
##   WNY  celdas cohorte x anio solo para t >= g  -> Wooldridge (2021) con
##        aun-no-tratados. Linea base = TODOS los anios previos de cada tratado
##        (equivalente al estimador de imputacion de Borusyak et al., 2024).
##
## Salidas:
##   outputs/tablas/47_event_study_bootstrap.csv
##   outputs/tablas/47_resumen_linea_base.csv
##   outputs/figuras/47_event_study_linea_base.png
##
## USO:  Rscript scripts/47_linea_base_bootstrap.R      (~10-20 min con B = 499)
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

B_BOOT <- 499
PRE_PROM <- -5:-2          # ventana previa para el contraste "post contra promedio previo"
POST     <- 0:E_MAX

## =============================================================================
## 2. Estimador de regresion saturado (SA / Wooldridge) en fixest
## =============================================================================
anio_ref <- min(muestra$year)
rhs_x <- paste0("i(year, ", COVS, ", ref = ", anio_ref, ")", collapse = " + ")
fml   <- as.formula(paste0(OUTCOME, " ~ i(celda, ref = 'ref') + ", rhs_x, " | id_b + year"))

## Devuelve los coeficientes por e (promedio de celdas ponderado por tamanio de
## cohorte, como sunab y aggte) y los tres resumenes.
estimar <- function(d, modo) {
  tr  <- d$G_dep > 0
  cel <- if (modo == "SA") tr & d$year != d$G_dep - 1 else tr & d$year >= d$G_dep
  d$celda <- ifelse(cel, paste0(d$G_dep, "_", d$year), "ref")
  m <- tryCatch(fixest::feols(fml, data = d, notes = FALSE, warn = FALSE),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  b <- coef(m); b <- b[grepl("^celda::", names(b))]
  cc <- data.frame(nm = sub("^celda::", "", names(b)), b = unname(b))
  cc$g <- as.integer(sub("_.*", "", cc$nm)); cc$e <- as.integer(sub(".*_", "", cc$nm)) - cc$g
  n_g <- d %>% filter(G_dep > 0) %>% distinct(id_b, G_dep) %>% count(G_dep, name = "n")
  ev <- cc %>% inner_join(n_g, by = c("g" = "G_dep")) %>%
    filter(e >= E_MIN, e <= E_MAX) %>% group_by(e) %>%
    summarise(att = sum(n * b) / sum(n), .groups = "drop")
  post <- mean(ev$att[ev$e %in% POST])
  pre  <- if (modo == "SA") mean(ev$att[ev$e %in% PRE_PROM]) else NA_real_
  list(ev = ev, res = c(post = post, pre = pre, post_menos_pre = post - pre))
}

muestra$id_b <- muestra$id_num
est <- list(SA = estimar(muestra, "SA"), WNY = estimar(muestra, "WNY"))
if (any(vapply(est, is.null, logical(1)))) stop("Fallo la estimacion puntual.")

## Control: SA aqui debe reproducir el Sun-Abraham de 46 (misma muestra y X x anio)
ruta46 <- file.path(DIR_TAB, "46_event_study_tres_estimadores.csv")
if (file.exists(ruta46)) {
  sa46 <- read_csv(ruta46, show_col_types = FALSE) %>% filter(estimador == "Sun-Abraham", e != -1)
  dif <- max(abs(sa46$att - est$SA$ev$att[match(sa46$e, est$SA$ev$e)]), na.rm = TRUE)
  cat(sprintf("Control SA (47 vs sunab en 46): diferencia maxima %.4f ha\n", dif))
  if (dif > 1) warning("SA no reproduce sunab: revisar antes de usar.")
}

## =============================================================================
## 3. Bootstrap de municipios, estratificado por cohorte (incluye G = 0)
## =============================================================================
unid <- distinct(muestra, id_num, G_dep)
por_g <- split(unid$id_num, unid$G_dep)
set.seed(SEMILLA)
boot <- vector("list", B_BOOT)
for (b in seq_len(B_BOOT)) {
  ids <- unlist(lapply(por_g, function(v) v[sample.int(length(v), length(v), replace = TRUE)]),
                use.names = FALSE)
  mapa <- data.frame(id_num = ids, id_b = seq_along(ids))   # id nuevo por copia
  d_b <- inner_join(mapa, select(muestra, -id_b), by = "id_num", relationship = "many-to-many")
  r <- lapply(c(SA = "SA", WNY = "WNY"), function(mo) estimar(d_b, mo))
  boot[[b]] <- r
  if (b %% 50 == 0) cat("  bootstrap", b, "/", B_BOOT, "\n")
}
boot <- Filter(function(r) !is.null(r$SA) && !is.null(r$WNY), boot)
cat("Replicas validas:", length(boot), "de", B_BOOT, "\n")

resumir <- function(modo) {
  ev_b <- bind_rows(lapply(boot, function(r) r[[modo]]$ev))
  ev <- est[[modo]]$ev %>% left_join(
    ev_b %>% group_by(e) %>% summarise(se = sd(att), ic_inf = quantile(att, .025),
                                       ic_sup = quantile(att, .975), .groups = "drop"), by = "e")
  rs_b <- do.call(rbind, lapply(boot, function(r) r[[modo]]$res))
  rs <- data.frame(estimador = modo, estadistico = names(est[[modo]]$res),
                   valor = est[[modo]]$res, se = apply(rs_b, 2, sd, na.rm = TRUE),
                   ic_inf = apply(rs_b, 2, quantile, .025, na.rm = TRUE),
                   ic_sup = apply(rs_b, 2, quantile, .975, na.rm = TRUE), row.names = NULL)
  list(ev = mutate(ev, estimador = modo), res = filter(rs, !is.na(valor)))
}
S <- lapply(c("SA", "WNY"), resumir)
ev_all <- bind_rows(lapply(S, `[[`, "ev"))
res_all <- bind_rows(lapply(S, `[[`, "res"))
write_csv(ev_all, file.path(DIR_TAB, "47_event_study_bootstrap.csv"))
write_csv(res_all, file.path(DIR_TAB, "47_resumen_linea_base.csv"))
print(res_all)

## =============================================================================
## 4. Figura: CS (de 46, EE propios) + SA y Wooldridge con IC bootstrap
## =============================================================================
etq <- c(CS  = "Callaway-Sant'Anna (DR) \u00b7 base g\u22121",
         SA  = "Sun-Abraham \u00b7 base g\u22121",
         WNY = "Wooldridge \u00b7 base = todos los a\u00f1os previos")
cs46 <- read_csv(ruta46, show_col_types = FALSE) %>%
  filter(estimador == "Callaway-Sant'Anna (DR)") %>% transmute(e, att, ic_inf, ic_sup, estimador = "CS")
fig <- bind_rows(cs46, select(ev_all, e, att, ic_inf, ic_sup, estimador),
                 data.frame(e = -1L, att = 0, ic_inf = NA, ic_sup = NA, estimador = "SA")) %>%
  mutate(ic_inf = ifelse(e == -1, 0, ic_inf), ic_sup = ifelse(e == -1, 0, ic_sup),
         estimador = factor(etq[estimador], levels = etq))
colores <- setNames(c("#B8860F", "#1D4F3F", "#8F3B1F"), etq)
p <- ggplot(fig, aes(e, att, color = estimador, shape = estimador)) +
  geom_hline(yintercept = 0, color = "#5B6E63", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "#5B6E63", linewidth = 0.4) +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup), na.rm = TRUE,
                  position = position_dodge(width = 0.55), linewidth = 0.6, size = 0.45) +
  scale_color_manual(values = colores, name = NULL) +
  scale_shape_manual(values = setNames(c(16, 15, 2), etq), name = NULL) +
  scale_x_continuous(breaks = E_MIN:E_MAX) +
  guides(color = guide_legend(nrow = 3)) +
  labs(x = "A\u00f1os desde la adopci\u00f3n (e)",
       y = "Efecto sobre la p\u00e9rdida anual de bosque (ha)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", legend.justification = "left", panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F2F4EF", color = NA),
        text = element_text(color = "#12261E"))
dispositivo <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
ggsave(file.path(DIR_FIG, "47_event_study_linea_base.png"), p,
       width = 9, height = 6, dpi = 300, device = dispositivo, bg = "#F2F4EF")
cat("Guardado: outputs/figuras/47_event_study_linea_base.png\n")
