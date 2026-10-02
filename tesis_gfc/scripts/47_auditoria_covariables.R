## =============================================================================
## 47_auditoria_covariables.R
##
## Audita por que el ATT de Hansen cambia de -425 ha (covariables del 40) a
## -211 ha (covariables "propias" del 46) con la misma muestra y estimador.
##
## Hallazgo de la auditoria del 02 (27/09/2026): defor_pre_tendencia NO era
## la pendiente OLS documentada, sino log1p(media 2008-09) - log1p(media
## 2001-02). La ventana 2001-2009 NO se solapa con las cohortes 2013-2019, asi
## que no hay mal control; la diferencia es de definicion.
##
## Descomposicion: se cambia UN elemento a la vez (misma muestra, mismo
## estimador que el 40: CS doblemente robusto, notyettreated, D1 depurada,
## cohortes 2013-2019, outcome Hansen 2001-2024):
##   V0  02 original:     log1p(media 01-09) + cambio log extremos + bosque
##   V1  02 corregida:    log1p(media 01-09) + pendiente OLS ha 01-09 + bosque
##   V2  ventana 2012:    log1p(media 01-12) + pendiente OLS ha 01-12 + bosque
##   V3  como el 46:      asinh(media 01-12) + pendiente OLS ha 01-12 + bosque
##   V4  pendiente log:   log1p(media 01-09) + pendiente OLS de log1p(L) 01-09
##   V5  sin covariables
##   (todas con las mismas covariables comunes del 40)
## cruzado con dos grupos de control:
##   actuales = nunca tratados bajo la D1 depurada (como hasta hoy)
##   limpios  = ademas, sin NINGUN vinculo con proyectos de carbono
##              (regla en comun_controles_limpios.R)
##
## ESPECIFICACION PRINCIPAL, fijada ANTES de correr este script:
##   V1 (definicion documentada, implementacion corregida) x controles limpios.
## Las demas celdas son la descomposicion y se reportan todas.
##
## USO
##   Rscript scripts/47_auditoria_covariables.R
## Insumos: los mismos del 40 + scripts/comun_controles_limpios.R
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(did)
})
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")

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
dir.create(DIR_TAB, showWarnings = FALSE, recursive = TRUE)
source(file.path(RAIZ_PROYECTO, "scripts/comun_controles_limpios.R"))

SEMILLA <- 20260824
VENTANA <- 2013:2019
BITERS  <- 999
MIN_E   <- -8
BAL_E   <- 5
OUTCOME <- "loss_area_ha"
PRINCIPAL <- c(variante = "V1", controles = "limpios")

COMUNES <- c("temp_media_c_base", "prec_anual_mm_base", "discapital_base", "disbogota_base",
             "distancia_mercado_base", "H_coca_base", "homicidios_base")
fx <- function(...) as.formula(paste("~", paste(c(...), collapse = " + ")))
VARIANTES <- list(
  V0 = list(f = fx("log_baseline_forest", COMUNES, "niv_log_09", "tend_extremos_09"),
            etq = "02 original (cambio log entre extremos)"),
  V1 = list(f = fx("log_baseline_forest", COMUNES, "niv_log_09", "tend_ols_ha_09"),
            etq = "02 corregida (pendiente OLS, documentada)"),
  V2 = list(f = fx("log_baseline_forest", COMUNES, "niv_log_12", "tend_ols_ha_12"),
            etq = "ventana 2001-2012"),
  V3 = list(f = fx("log_baseline_forest", COMUNES, "niv_asinh_12", "tend_ols_ha_12"),
            etq = "como el 46 (asinh, 2001-2012)"),
  V4 = list(f = fx("log_baseline_forest", COMUNES, "niv_log_09", "tend_ols_log_09"),
            etq = "pendiente OLS en log1p"),
  V5 = list(f = ~1, etq = "sin covariables")
)

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(t) cat("\n", strrep("=", 74), "\n", t, "\n", strrep("=", 74), "\n", sep = "")

## --- Covariables de deforestacion previa en todas sus variantes (R base) ---
pendiente <- function(t, y) {
  ok <- is.finite(y)
  if (sum(ok) < 3 || stats::var(t[ok]) == 0) return(NA_real_)
  stats::cov(t[ok], y[ok]) / stats::var(t[ok])
}
covariables_pre <- function(d) {
  sp <- split(d[, c("year", OUTCOME)], d$COD_DANE)
  out <- do.call(rbind, lapply(names(sp), function(k) {
    g <- sp[[k]]; t <- g$year; L <- g[[OUTCOME]]
    w09 <- t >= 2001 & t <= 2009; w12 <- t >= 2001 & t <= 2012
    ini <- mean(L[t %in% 2001:2002], na.rm = TRUE); fin <- mean(L[t %in% 2008:2009], na.rm = TRUE)
    data.frame(COD_DANE = k,
               niv_log_09       = log1p(mean(L[w09], na.rm = TRUE)),
               niv_log_12       = log1p(mean(L[w12], na.rm = TRUE)),
               niv_asinh_12     = asinh(mean(L[w12], na.rm = TRUE)),
               tend_extremos_09 = if (is.finite(ini) && is.finite(fin)) log1p(fin) - log1p(ini) else NA_real_,
               tend_ols_ha_09   = pendiente(t[w09], L[w09]),
               tend_ols_ha_12   = pendiente(t[w12], L[w12]),
               tend_ols_log_09  = pendiente(t[w09], log1p(L[w09])),
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

wald_pre <- function(din) {
  e <- din$egt; idx <- which(e < 0 & !is.na(din$att.egt))
  inf <- din$inf.function$dynamic.inf.func.e
  if (!length(idx) || is.null(inf) || ncol(inf) != length(e)) return(c(W = NA, gl = 0, p = NA))
  V <- crossprod(inf[, idx, drop = FALSE]) / nrow(inf)^2
  b <- din$att.egt[idx]
  W <- as.numeric(t(b) %*% MASS::ginv(V) %*% b)
  c(W = W, gl = length(idx), p = stats::pchisq(W, length(idx), lower.tail = FALSE))
}

## =============================================================================
## Datos
## =============================================================================
panel <- readRDS(file.path(DIR_OUT, "panel_analisis_did.rds"))
panel$COD_DANE <- pad5(panel$COD_DANE)
if (!"log_baseline_forest" %in% names(panel)) {
  cp <- read_csv(file.path(DIR_TAB, "covariables_pre_municipio.csv"),
                 col_types = cols(COD_DANE = col_character()), show_col_types = FALSE)
  panel <- panel %>% left_join(cp %>% transmute(COD_DANE = pad5(COD_DANE), log_baseline_forest),
                               by = "COD_DANE")
}
falt <- setdiff(c(COMUNES, "log_baseline_forest"), names(panel))
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
cov <- covariables_pre(as.data.frame(base))
base <- base %>% left_join(cov, by = "COD_DANE")

## Que tan distintas son las dos "tendencias" (la del 02 original y la corregida)
cat(sprintf("Correlacion entre municipios: tend_extremos_09 vs tend_ols_ha_09 = %.3f | vs tend_ols_log_09 = %.3f\n",
            cor(cov$tend_extremos_09, cov$tend_ols_ha_09, use = "complete.obs"),
            cor(cov$tend_extremos_09, cov$tend_ols_log_09, use = "complete.obs")))

contaminados <- municipios_con_proyecto(RUTA_TRAT)
MUESTRAS <- list(actuales = base,
                 limpios  = aplicar_controles_limpios(as.data.frame(base), contaminados, "G_dep"))

## =============================================================================
## Estimacion
## =============================================================================
correr <- function(d, xformla, etq) {
  cat("\n--- ", etq, " ---\n", sep = "")
  d <- as.data.frame(d); d$.gname <- d$G_dep
  avisos <- character(0)
  set.seed(SEMILLA)
  out <- withCallingHandlers(
    tryCatch(att_gt(yname = OUTCOME, tname = "year", idname = "id_num", gname = ".gname",
                    xformla = xformla, data = d, control_group = "notyettreated",
                    est_method = "dr", bstrap = TRUE, biters = BITERS, cband = TRUE,
                    clustervars = "id_num"),
             error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }),
    warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") })
  if (is.null(out)) return(NULL)
  s   <- aggte(out, type = "simple", na.rm = TRUE)
  din <- aggte(out, type = "dynamic", min_e = MIN_E, max_e = BAL_E, na.rm = TRUE)
  bal <- tryCatch(aggte(out, type = "dynamic", balance_e = BAL_E, min_e = 0, max_e = BAL_E,
                        na.rm = TRUE), error = function(e) NULL)
  wp  <- wald_pre(din)
  n_na <- sum(is.na(out$att))
  cat(sprintf("ATT %.1f (EE %.1f) IC95 [%.1f, %.1f] | pre p = %.3f | g-t NA: %d/%d | avisos: %d\n",
              s$overall.att, s$overall.se, s$overall.att - 1.96 * s$overall.se,
              s$overall.att + 1.96 * s$overall.se, wp["p"], n_na, length(out$att), length(avisos)))
  if (length(avisos)) {
    tab <- sort(table(substr(avisos, 1, 110)), decreasing = TRUE)
    for (k in seq_len(min(3, length(tab)))) cat(sprintf("    [%dx] %s\n", tab[[k]], names(tab)[k]))
  }
  data.frame(att = s$overall.att, ee = s$overall.se,
             ic_inf = s$overall.att - 1.96 * s$overall.se, ic_sup = s$overall.att + 1.96 * s$overall.se,
             att_balanceado = if (is.null(bal)) NA_real_ else bal$overall.att,
             wald_pre_p = wp["p"], gt_na = n_na, gt_total = length(out$att),
             n_avisos = length(avisos),
             aviso_mas_frecuente = if (length(avisos)) names(sort(table(avisos), decreasing = TRUE))[1] else "",
             n_tratados = n_distinct(d$COD_DANE[d$G_dep > 0]),
             n_controles = n_distinct(d$COD_DANE[d$G_dep == 0]), row.names = NULL)
}

titulo("DESCOMPOSICION: covariables x grupo de control (Hansen 2001-2024, D1 depurada)")
filas <- list()
for (cg in names(MUESTRAS)) for (v in names(VARIANTES)) {
  r <- correr(MUESTRAS[[cg]], VARIANTES[[v]]$f, paste0(v, " ", VARIANTES[[v]]$etq, " | controles ", cg))
  if (!is.null(r)) filas[[paste(v, cg)]] <- cbind(variante = v, descripcion = VARIANTES[[v]]$etq,
                                                   controles = cg, r)
}
tabla <- do.call(rbind, filas)
tabla$principal <- tabla$variante == PRINCIPAL["variante"] & tabla$controles == PRINCIPAL["controles"]
write_csv(tabla, file.path(DIR_TAB, "47_descomposicion_covariables.csv"))

titulo("RESUMEN")
print(tabla[, c("variante", "controles", "n_controles", "att", "ee", "ic_inf", "ic_sup",
                "wald_pre_p", "gt_na", "n_avisos", "principal")], row.names = FALSE, digits = 3)

paso <- function(a, b, cg, nombre) {
  x <- tabla$att[tabla$variante == a & tabla$controles == cg]
  y <- tabla$att[tabla$variante == b & tabla$controles == cg]
  if (length(x) && length(y)) cat(sprintf("  %-44s %s -> %s: %+7.1f ha\n", nombre, a, b, y - x))
}
cat("\nDescomposicion del cambio (controles actuales):\n")
paso("V0", "V1", "actuales", "correccion de la tendencia (bug del 02)")
paso("V1", "V2", "actuales", "ventana 2009 -> 2012")
paso("V2", "V3", "actuales", "log1p -> asinh en el nivel")
cat("Efecto de limpiar los controles:\n")
for (v in names(VARIANTES)) {
  x <- tabla$att[tabla$variante == v & tabla$controles == "actuales"]
  y <- tabla$att[tabla$variante == v & tabla$controles == "limpios"]
  if (length(x) && length(y)) cat(sprintf("  %s: actuales %.1f -> limpios %.1f (%+.1f ha)\n", v, x, y, y - x))
}
p <- tabla[tabla$principal, ]
if (nrow(p)) cat(sprintf("\n>>> ESPECIFICACION PRINCIPAL (V1 x limpios): ATT %.1f ha, IC95 [%.1f, %.1f], pre p = %.3f\n",
                         p$att, p$ic_inf, p$ic_sup, p$wald_pre_p))
cat("Guardado: outputs/tablas/47_descomposicion_covariables.csv\n")
