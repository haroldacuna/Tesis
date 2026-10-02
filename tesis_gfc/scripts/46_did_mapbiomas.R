## =============================================================================
## 46_did_mapbiomas.R
##
## Paso 5 del WBS de MapBiomas. Misma especificacion principal que 40
## (D1 depurada, cohortes 2013-2019, Callaway-Sant'Anna doblemente robusto,
## notyettreated, bootstrap con banda simultanea), cambiando el OUTCOME.
##
## Outcome principal de MapBiomas, fijado ANTES de estimar por la regla de
## 39c (decision_outcome_mapbiomas.json + diagnostico_est5.json):
##     DEF^est5_p1 = bosque estable 5 anios -> antropico en t, persistente en t+1
##     panel 2001-2023 (la persistencia no existe para 2024)
##
## Bloques:
##   A. ATT por outcome, con dos juegos de covariables:
##        - "propias": las covariables de deforestacion previa se construyen
##          con el MISMO outcome (media y tendencia 2001-2012) y el bosque base
##          con la MISMA fuente. Es la unica forma de comparar fuentes sin que
##          una herede el condicionamiento de la otra.
##        - "sincov": sin covariables (neutral respecto de la fuente).
##      y, solo para Hansen 2001-2024, "40" = covariables originales del 40,
##      para verificar que este script reproduce la estimacion principal.
##   B. Event study por fuente (Hansen vs MapBiomas en la MISMA ventana
##      2001-2023) con prueba conjunta de Wald de los coeficientes previos.
##      Responde a R3 de 39c: la brecha entre fuentes tiene tendencia
##      diferencial pre-tratamiento (p < 0,001); este bloque dice cual fuente
##      la tiene.
##   Controles: por defecto LIMPIOS (sin ningun vinculo con proyectos de
##   carbono; ver comun_controles_limpios.R). CONTROLES_LIMPIOS <- FALSE
##   reproduce la corrida del 27/09/2026 en la manana.
##   C. ATT de la brecha Delta_it = Hansen_it - DEF^est5_p1_it (ha).
##      ATT(Delta) ~ 0 y pre-tendencias planas -> el resultado no depende de
##      la fuente. Pre-tendencias en Delta -> error de medicion correlacionado
##      con el tratamiento (ya anticipado por R3).
##
## NO incluye la dosis con denominador MapBiomas: requiere intersectar los
## poligonos de proyecto con el bosque estable (extraccion espacial aparte).
##
## USO
##   Rscript scripts/46_did_mapbiomas.R
##
## Insumos:
##   outputs/panel_analisis_did.rds                                (01)
##   outputs/tablas/covariables_pre_municipio.csv                  (02)
##   data/final/panel_con_tratamiento_actualizado.csv              (26)
##   data/interim/mapbiomas/deforestacion_mapbiomas_municipio_anio.csv          (39 v2)
##   data/interim/mapbiomas/deforestacion_estable_mapbiomas_municipio_anio.csv  (39b)
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(did)
  library(ggplot2)
})
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")

## --- Raiz canonica ------------------------------------------------------------
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
DIR_MB  <- file.path(RAIZ_PROYECTO, "data/interim/mapbiomas")
for (d in c(DIR_OUT, DIR_TAB, DIR_FIG)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

## --- Parametros (decididos ANTES de ver resultados) --------------------------
SEMILLA  <- 20260824          # la misma del 40
VENTANA  <- 2013:2019
BITERS   <- 999
PRE_COV  <- 2001:2012         # anterior a la primera cohorte: comun a todas
MIN_E    <- -8                # coeficientes previos del event study
ALFA_PRE <- 0.05

## Outcomes: columna, ultimo anio del panel, fuente (para el bosque base)
OUTCOMES <- list(
  H24        = list(col = "loss_area_ha",   fin = 2024, fuente = "hansen",
                    etq = "Hansen 2001-2024 (referencia del 40)"),
  H23        = list(col = "loss_area_ha",   fin = 2023, fuente = "hansen",
                    etq = "Hansen 2001-2023 (misma ventana que MB principal)"),
  MB_est5_p1 = list(col = "def_est5_p1_ha", fin = 2023, fuente = "mb",
                    etq = "MapBiomas bosque estable 5a + persistencia (PRINCIPAL MB)"),
  MB_est5    = list(col = "def_est5_ha",    fin = 2024, fuente = "mb",
                    etq = "MapBiomas bosque estable 5a, sin persistencia"),
  MB_est10   = list(col = "def_est10_ha",   fin = 2024, fuente = "mb",
                    etq = "MapBiomas bosque estable 10a"),
  MB_p1      = list(col = "def_mb_p1_ha",   fin = 2023, fuente = "mb",
                    etq = "MapBiomas bruta + persistencia"),
  BRECHA     = list(col = "brecha_ha",      fin = 2023, fuente = "brecha",
                    etq = "Brecha Hansen - MB principal")
)

COV_COMUNES <- c("temp_media_c_base", "prec_anual_mm_base", "discapital_base",
                 "disbogota_base", "distancia_mercado_base", "H_coca_base",
                 "homicidios_base")
X_40 <- ~ log_baseline_forest + temp_media_c_base + prec_anual_mm_base +
  discapital_base + disbogota_base + distancia_mercado_base + H_coca_base +
  homicidios_base + log_defor_pre_media + defor_pre_tendencia
X_PROPIAS <- as.formula(paste("~ cov_lbosque +", paste(COV_COMUNES, collapse = " + "),
                              "+ cov_nivel_pre + cov_tend_pre"))

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(txt) cat("\n", strrep("=", 74), "\n", txt, "\n", strrep("=", 74), "\n", sep = "")

## =============================================================================
## Funciones auxiliares (R base: probadas por separado)
## =============================================================================

## Covariables de deforestacion previa construidas con el PROPIO outcome.
## Nivel: asinh de la media 2001-2012 (asinh admite la brecha, que puede ser
## negativa). Tendencia: pendiente MCO de y sobre el anio en 2001-2012.
cov_pre_propias <- function(d, ycol, anios = PRE_COV) {
  p <- d[d$year %in% anios, c("COD_DANE", "year", ycol)]
  sp <- split(p, p$COD_DANE)
  out <- do.call(rbind, lapply(sp, function(g) {
    y <- g[[ycol]]; t <- g$year
    data.frame(COD_DANE = g$COD_DANE[1],
               cov_nivel_pre = asinh(mean(y, na.rm = TRUE)),
               cov_tend_pre  = if (stats::var(t) > 0) stats::cov(t, y) / stats::var(t) else NA_real_)
  }))
  rownames(out) <- NULL
  out
}

## Prueba conjunta de Wald de los coeficientes previos (e < 0) del event
## study, con la matriz de covarianza de las funciones de influencia que
## devuelve aggte(). Var(theta_e) = sum_i psi_ie^2 / n^2, coherente con los
## EE analiticos del paquete did (agrupados por unidad).
wald_pre <- function(din) {
  e   <- din$egt
  idx <- which(e < 0 & !is.na(din$att.egt))
  inf <- din$inf.function$dynamic.inf.func.e
  if (!length(idx) || is.null(inf)) return(c(W = NA, gl = 0, p = NA))
  if (ncol(inf) != length(e)) {         # columnas desalineadas con egt: no arriesgar
    cat("Aviso: funciones de influencia no alineadas con los tiempos del event study; Wald omitido\n")
    return(c(W = NA, gl = 0, p = NA))
  }
  n <- nrow(inf)
  V <- crossprod(inf[, idx, drop = FALSE]) / n^2
  b <- din$att.egt[idx]
  W <- as.numeric(t(b) %*% MASS::ginv(V) %*% b)
  c(W = W, gl = length(idx), p = stats::pchisq(W, length(idx), lower.tail = FALSE))
}

## Media pretratamiento del outcome entre tratados (para el ATT relativo)
media_pre <- function(d, ycol) {
  t <- d[d$.gname > 0 & d$year < d$.gname, ]
  if (!nrow(t)) return(NA_real_)
  mean(tapply(t[[ycol]], t$COD_DANE, mean, na.rm = TRUE), na.rm = TRUE)
}

## =============================================================================
## 1. Datos (misma logica que 40)
## =============================================================================
panel <- readRDS(file.path(DIR_OUT, "panel_analisis_did.rds"))
panel$COD_DANE <- pad5(panel$COD_DANE)

## El CSV del 02 SIEMPRE reemplaza lo que traiga el panel (correccion 27/09/2026)
source(file.path(RAIZ_PROYECTO, "scripts/comun_covariables_pre.R"))
panel <- unir_covariables_pre(panel, file.path(DIR_TAB, "covariables_pre_municipio.csv"))
faltan <- setdiff(c(all.vars(X_40), COV_COMUNES), names(panel))
if (length(faltan)) stop("Faltan covariables en el panel: ", paste(faltan, collapse = ", "))

COL_DEP <- "anio_inicio_tratamiento_todas_fuentes_redd_depurada"
COL_MUE <- "muestra_redd_depurada"
trat <- read_csv(file.path(RAIZ_PROYECTO, "data/final/panel_con_tratamiento_actualizado.csv"),
                 col_select = all_of(c("COD_DANE", COL_DEP, COL_MUE)),
                 col_types = cols(COD_DANE = col_character(), .default = col_double()),
                 show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>%
  group_by(COD_DANE) %>%
  summarise(G_dep = coalesce(min_na(.data[[COL_DEP]]), 0),
            muestra_dep = min(.data[[COL_MUE]]), .groups = "drop")

## MapBiomas: transiciones (v2) + bosque estable (39b)
leer_mb <- function(archivo, cols) {
  ruta <- file.path(DIR_MB, archivo)
  if (!file.exists(ruta)) stop("Falta ", ruta, ". Corre 39_transiciones_mapbiomas.py.")
  x <- read_csv(ruta, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE)
  falt <- setdiff(cols, names(x))
  if (length(falt)) stop(archivo, " no tiene: ", paste(falt, collapse = ", "))
  x %>% mutate(COD_DANE = pad5(COD_DANE)) %>% select(all_of(c("COD_DANE", "anio", cols))) %>%
    rename(year = anio)
}
mb <- leer_mb("deforestacion_mapbiomas_municipio_anio.csv", c("def_mb_p1_ha")) %>%
  inner_join(leer_mb("deforestacion_estable_mapbiomas_municipio_anio.csv",
                     c("def_est5_ha", "def_est5_p1_ha", "def_est10_ha", "bosque_est5_2000_ha")),
             by = c("COD_DANE", "year"))

panel <- panel %>%
  select(-any_of(c("G_dep", "muestra_dep", setdiff(names(mb), c("COD_DANE", "year"))))) %>%
  inner_join(trat, by = "COD_DANE") %>%
  left_join(mb, by = c("COD_DANE", "year")) %>%
  mutate(brecha_ha = loss_area_ha - def_est5_p1_ha)

## Muestra principal: D1 depurada, ventana 2013-2019 (fuera de ventana SALEN)
muestra_ppal <- panel %>% filter(muestra_dep == 1, G_dep == 0 | G_dep %in% VENTANA)
## Controles limpios (regla del 27/09/2026, comun_controles_limpios.R): los
## controles no pueden tener ningun vinculo con proyectos de carbono.
## CONTROLES_LIMPIOS <- FALSE reproduce la corrida anterior.
CONTROLES_LIMPIOS <- TRUE
if (CONTROLES_LIMPIOS) {
  source(file.path(RAIZ_PROYECTO, "scripts/comun_controles_limpios.R"))
  muestra_ppal <- aplicar_controles_limpios(
    as.data.frame(muestra_ppal),
    municipios_con_proyecto(file.path(RAIZ_PROYECTO, "data/final/panel_con_tratamiento_actualizado.csv")),
    "G_dep")
}
m <- distinct(muestra_ppal, COD_DANE, .keep_all = TRUE)
cat(sprintf("Muestra principal: %d municipios | %d tratados | %d nunca tratados\n",
            nrow(m), sum(m$G_dep > 0), sum(m$G_dep == 0)))

## Panel balanceado por outcome, con sus covariables propias
preparar <- function(o) {
  d <- muestra_ppal %>% filter(year <= o$fin)
  n_anios <- n_distinct(d$year)
  malos <- d %>% group_by(COD_DANE) %>%
    summarise(ok = n() == n_anios && all(is.finite(.data[[o$col]])), .groups = "drop") %>%
    filter(!ok) %>% pull(COD_DANE)
  if (length(malos)) {
    cat(sprintf("  [%s] %d municipios con outcome incompleto, fuera\n", o$col, length(malos)))
    d <- d %>% filter(!COD_DANE %in% malos)
  }
  cp <- cov_pre_propias(as.data.frame(d), o$col)
  d <- d %>% select(-any_of(c("cov_nivel_pre", "cov_tend_pre"))) %>% left_join(cp, by = "COD_DANE")
  d$cov_lbosque <- if (o$fuente == "hansen") d$log_baseline_forest else log1p(d$bosque_est5_2000_ha)
  d
}

## =============================================================================
## 2. Estimador (misma especificacion que correr_cs() del 40)
## =============================================================================
correr_cs <- function(d, ycol, xformla, etiqueta, balance_e) {
  cat("\n", strrep("-", 74), "\n", "Modelo: ", etiqueta, "\n", strrep("-", 74), "\n", sep = "")
  d$.gname <- d$G_dep
  set.seed(SEMILLA)
  out <- tryCatch(
    att_gt(yname = ycol, tname = "year", idname = "id_num", gname = ".gname",
           xformla = xformla, data = d, control_group = "notyettreated", est_method = "dr",
           bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "id_num"),
    error = function(e) { cat("ERROR en att_gt():", conditionMessage(e), "\n"); NULL })
  if (is.null(out)) return(NULL)
  simple  <- aggte(out, type = "simple", na.rm = TRUE)
  din     <- aggte(out, type = "dynamic", min_e = MIN_E, max_e = balance_e, na.rm = TRUE)
  din_bal <- tryCatch(aggte(out, type = "dynamic", balance_e = balance_e, min_e = 0,
                            max_e = balance_e, na.rm = TRUE), error = function(e) NULL)
  mp <- media_pre(d, ycol)
  wp <- wald_pre(din)
  cat(sprintf("ATT simple: %.2f (EE %.2f)  IC95 [%.2f, %.2f] | relativo %.1f%% de la media pre (%.1f ha)\n",
              simple$overall.att, simple$overall.se,
              simple$overall.att - 1.96 * simple$overall.se,
              simple$overall.att + 1.96 * simple$overall.se,
              100 * simple$overall.att / mp, mp))
  cat(sprintf("Pre-tendencias (Wald conjunto, e = %d..-1): W = %.2f, gl = %d, p = %.3f\n",
              MIN_E, wp["W"], as.integer(wp["gl"]), wp["p"]))
  list(att_gt = out, simple = simple, dinamico = din, balanceado = din_bal,
       media_pre = mp, wald = wp, etiqueta = etiqueta,
       n_trat = n_distinct(d$COD_DANE[d$.gname > 0]))
}

fila <- function(r, clave, spec) {
  if (is.null(r)) return(NULL)
  b <- r$balanceado
  data.frame(outcome = clave, especificacion = spec, etiqueta = r$etiqueta,
             n_tratados = r$n_trat, media_pre_tratados = r$media_pre,
             att = r$simple$overall.att, ee = r$simple$overall.se,
             ic_inf = r$simple$overall.att - 1.96 * r$simple$overall.se,
             ic_sup = r$simple$overall.att + 1.96 * r$simple$overall.se,
             att_rel_pct = 100 * r$simple$overall.att / r$media_pre,
             att_balanceado = if (is.null(b)) NA_real_ else b$overall.att,
             ee_balanceado  = if (is.null(b)) NA_real_ else b$overall.se,
             wald_pre_W = r$wald["W"], wald_pre_gl = r$wald["gl"], wald_pre_p = r$wald["p"],
             row.names = NULL)
}

## =============================================================================
## A. ATT por outcome
## =============================================================================
titulo("A. ATT POR OUTCOME (D1 depurada, cohortes 2013-2019, DR, notyettreated)")
res <- list()
for (k in names(OUTCOMES)) {
  o <- OUTCOMES[[k]]
  d <- preparar(o)
  bal_e <- o$fin - max(VENTANA)                 # 5 si termina en 2024, 4 si en 2023
  res[[paste0(k, "__propias")]] <- correr_cs(d, o$col, X_PROPIAS, paste(o$etq, "- DR covariables propias"), bal_e)
  res[[paste0(k, "__sincov")]]  <- correr_cs(d, o$col, ~1,        paste(o$etq, "- sin covariables"), bal_e)
  if (k == "H24")
    res[[paste0(k, "__40")]]    <- correr_cs(d, o$col, X_40,      paste(o$etq, "- covariables del 02/40 (tendencia OLS corregida)"), bal_e)
}

## =============================================================================
## B. Event study por fuente, misma ventana 2001-2023, en escala RELATIVA
##    (ATT_e / media pre de los tratados): las fuentes miden magnitudes
##    distintas y solo en terminos relativos los perfiles son comparables.
## =============================================================================
titulo("B. EVENT STUDY POR FUENTE (2001-2023)")
es_rel <- function(r, fuente) {
  if (is.null(r)) return(NULL)
  dd <- r$dinamico
  data.frame(fuente = fuente, e = dd$egt,
             att_rel = 100 * dd$att.egt / r$media_pre,
             ee_rel  = 100 * dd$se.egt / r$media_pre)
}
for (spec in c("propias", "sincov")) {
  es <- rbind(es_rel(res[[paste0("H23__", spec)]], "Hansen"),
              es_rel(res[[paste0("MB_est5_p1__", spec)]], "MapBiomas estable 5a + p1"))
  if (is.null(es) || !nrow(es)) next
  write_csv(es, file.path(DIR_TAB, paste0("46_event_study_por_fuente_", spec, ".csv")))
  p <- ggplot(es, aes(e, att_rel, color = fuente)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
    geom_pointrange(aes(ymin = att_rel - 1.96 * ee_rel, ymax = att_rel + 1.96 * ee_rel),
                    position = position_dodge(width = 0.45), size = 0.3) +
    labs(title = "Estudio de eventos por fuente de deforestacion",
         subtitle = paste0("D1 depurada, cohortes 2013-2019, 2001-2023. Covariables: ", spec,
                           ". IC 95% puntual."),
         x = "Anios desde el inicio del tratamiento",
         y = "ATT (% de la media pretratamiento)", color = NULL) +
    theme_minimal() + theme(legend.position = "bottom")
  ggsave(file.path(DIR_FIG, paste0("46_event_study_por_fuente_", spec, ".png")),
         p, width = 9, height = 5.5, dpi = 150)
  for (f in unique(es$fuente)) {
    r <- res[[paste0(if (f == "Hansen") "H23" else "MB_est5_p1", "__", spec)]]
    cat(sprintf("  %-28s [%s] pre-tendencias: p = %.3f %s\n", f, spec, r$wald["p"],
                if (isTRUE(r$wald["p"] < ALFA_PRE)) "<- RECHAZA tendencias paralelas previas" else ""))
  }
}

## =============================================================================
## C. Brecha: ya estimada en A como outcome "BRECHA"; aqui solo la lectura
## =============================================================================
titulo("C. BRECHA HANSEN - MAPBIOMAS")
for (spec in c("propias", "sincov")) {
  r <- res[[paste0("BRECHA__", spec)]]
  if (is.null(r)) next
  ic <- r$simple$overall.att + c(-1.96, 1.96) * r$simple$overall.se
  cat(sprintf("[%s] ATT(Delta) = %.2f ha, IC95 [%.2f, %.2f] -> %s | pre-tendencias p = %.3f\n",
              spec, r$simple$overall.att, ic[1], ic[2],
              if (ic[1] > 0 || ic[2] < 0) "el resultado DEPENDE de la fuente" else "no se rechaza igualdad entre fuentes",
              r$wald["p"]))
}

## =============================================================================
## Resumen y guardado
## =============================================================================
titulo("RESUMEN")
tabla <- do.call(rbind, lapply(names(res), function(n) {
  partes <- strsplit(n, "__", fixed = TRUE)[[1]]
  fila(res[[n]], partes[1], partes[2])
}))
if (!is.null(tabla)) {
  print(tabla[, c("outcome", "especificacion", "n_tratados", "att", "ee",
                  "att_rel_pct", "att_balanceado", "wald_pre_p")], row.names = FALSE, digits = 3)
  write_csv(tabla, file.path(DIR_TAB, "46_resumen_mapbiomas.csv"))
}
saveRDS(list(resultados = res,
             parametros = list(ventana = VENTANA, semilla = SEMILLA, min_e = MIN_E,
                               pre_cov = PRE_COV, outcomes = OUTCOMES)),
        file.path(DIR_OUT, "46_resultados_mapbiomas.rds"))
cat("\nGuardado: outputs/46_resultados_mapbiomas.rds\n")
cat("Guardado: outputs/tablas/46_resumen_mapbiomas.csv\n")
cat("Figuras:  outputs/figuras/46_event_study_por_fuente_*.png\n")
