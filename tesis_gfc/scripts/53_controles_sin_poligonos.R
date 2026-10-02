## =============================================================================
## 53_controles_sin_poligonos.R
##
## Reestimacion de la especificacion principal excluyendo los controles que
## estan cubiertos por poligonos de proyectos de deforestacion evitada (AD),
## segun 52_poligonos_proyectos.py (base de Karnik et al. 2025).
##
## Disparador (regla fijada antes de correr el 52): los controles con cobertura
## AD >= 5 % del area municipal (y >= 100 ha) concentran 11,8 % del peso ATT
## del 48 (umbral: 10 %). Varios estan cubiertos al 67-96 %: son municipios
## con proyecto REDD+ que la D1 trataba como controles (sobre todo proyectos
## de Cercarbono sin municipio asignado en el pipeline).
##
## Diseno FIJADO ANTES DE CORRER (27/09/2026):
##   P1 (principal de este script): excluir controles con cobertura AD >= 5 %
##   Sensibilidad: >= 1 % y >= 10 % (mismo minimo de 100 ha)
##   Estimadores: dr (principal), reg y sin covariables.
##   REGLA: la principal (-207.9, EE 198.5) es robusta a la contaminacion de
##   controles si el DR en P1 tiene el mismo signo y queda a < 1 EE.
##   Direccion (informativa): si controles parcialmente tratados atenuaban el
##   efecto, excluirlos lo hace MAS negativo.
##
## Base = la del 40 (D1 depurada, cohortes 2013-2019, controles limpios,
## notyettreated, covariables del 02 corregido, Hansen 2001-2024).
##
## USO
##   Rscript scripts/53_controles_sin_poligonos.R
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(did)
})
source("scripts/00_config_did.R")   # B.1: parametros comunes (no redefinir aqui)

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
source(file.path(RAIZ_PROYECTO, "scripts/comun_controles_limpios.R"))
source(file.path(RAIZ_PROYECTO, "scripts/comun_covariables_pre.R"))

BAL_E     <- 5
PRINCIPAL <- c(att = -207.9, ee = 198.5)
UMBRALES  <- c(P1 = 5, S_1 = 1, S_10 = 10)   # % del area municipal
MIN_HA    <- 100
X <- COVARIABLES_XFORMLA   # B.1: lista oficial de 00_config_did.R

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(t) cat("\n", strrep("=", 74), "\n", t, "\n", strrep("=", 74), "\n", sep = "")

## Controles a excluir segun cobertura AD (R base; probada por separado)
controles_cubiertos <- function(cob, controles, umbral_pct, min_ha = MIN_HA) {
  c2 <- cob[cob$COD_DANE %in% controles, ]
  c2$COD_DANE[!is.na(c2$pct_ad) & c2$pct_ad >= umbral_pct & c2$ha_ad >= min_ha]
}

## --- Datos (igual que el 40) -------------------------------------------------
panel <- readRDS(file.path(DIR_OUT, "panel_analisis_did.rds"))
panel$COD_DANE <- pad5(panel$COD_DANE)
panel <- unir_covariables_pre(panel, file.path(DIR_TAB, "covariables_pre_municipio.csv"))
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
nombres <- panel %>% distinct(COD_DANE, .keep_all = TRUE) %>%
  select(any_of(c("COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR")))

ruta_cob <- file.path(RAIZ_PROYECTO, "data/interim/proyectos_poligonos/cobertura_municipal.csv")
if (!file.exists(ruta_cob)) stop("Falta ", ruta_cob, ". Corre 52_poligonos_proyectos.py.")
cob <- read_csv(ruta_cob, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE))
controles <- unique(base$COD_DANE[base$G_dep == 0])

## --- Peso ATT que sale (48) ---------------------------------------------------
ruta_ps <- file.path(DIR_TAB, "48_propension_municipal.csv")
peso_sale <- function(cods) {
  if (!file.exists(ruta_ps)) return(NA_real_)
  ps <- read_csv(ruta_ps, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE) %>%
    mutate(COD_DANE = pad5(COD_DANE)) %>% filter(tratado == 0) %>% mutate(w = p / (1 - pmin(p, 1 - 1e-12)))
  100 * sum(ps$w[ps$COD_DANE %in% cods]) / sum(ps$w)
}

## --- Estimacion ---------------------------------------------------------------
correr <- function(d, metodo, xformla, etq) {
  cat("\n--- ", etq, " ---\n", sep = "")
  d <- as.data.frame(d); d$.gname <- d$G_dep
  set.seed(SEMILLA)
  out <- suppressWarnings(tryCatch(
    att_gt(yname = OUTCOME, tname = "year", idname = "id_num", gname = ".gname",
           xformla = xformla, data = d, control_group = "notyettreated",
           est_method = metodo, bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "id_num"),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }))
  if (is.null(out)) return(NULL)
  s   <- aggte(out, type = "simple", na.rm = TRUE)
  bal <- tryCatch(aggte(out, type = "dynamic", balance_e = BAL_E, min_e = 0, max_e = BAL_E,
                        na.rm = TRUE), error = function(e) NULL)
  cat(sprintf("ATT %.1f (EE %.1f) IC95 [%.1f, %.1f] | g-t NA %d/%d\n", s$overall.att, s$overall.se,
              s$overall.att - 1.96 * s$overall.se, s$overall.att + 1.96 * s$overall.se,
              sum(is.na(out$att)), length(out$att)))
  data.frame(att = s$overall.att, ee = s$overall.se,
             ic_inf = s$overall.att - 1.96 * s$overall.se, ic_sup = s$overall.att + 1.96 * s$overall.se,
             att_balanceado = if (is.null(bal)) NA_real_ else bal$overall.att,
             gt_na = sum(is.na(out$att)),
             n_tratados = n_distinct(d$COD_DANE[d$G_dep > 0]),
             n_controles = n_distinct(d$COD_DANE[d$G_dep == 0]), row.names = NULL)
}

titulo("CONTROLES CUBIERTOS POR POLIGONOS AD")
filas <- list()
r0 <- correr(base, "dr", X, "completa | dr (debe reproducir -207.9)")
if (!is.null(r0)) filas[["completa dr"]] <- cbind(muestra = "completa", umbral_pct = NA, especificacion = "dr",
                                                   peso_att_excluido_pct = 0, r0)
for (k in names(UMBRALES)) {
  excl <- controles_cubiertos(cob, controles, UMBRALES[[k]])
  cat(sprintf("\n[%s] cobertura AD >= %g %%: salen %d controles (%.1f %% del peso ATT)\n",
              k, UMBRALES[[k]], length(excl), peso_sale(excl)))
  if (length(excl)) print(as.data.frame(cob %>% filter(COD_DANE %in% excl) %>%
                                         left_join(nombres, by = "COD_DANE", suffix = c("", ".p")) %>%
                                         select(any_of(c("COD_DANE", "NOMBRE_MPI", "pct_ad", "proyectos_ad"))) %>%
                                         arrange(desc(pct_ad))), row.names = FALSE, digits = 3)
  d_k <- base[!base$COD_DANE %in% excl, ]
  specs <- if (k == "P1") list(dr = X, reg = X, sincov = ~1) else list(dr = X)
  for (s in names(specs)) {
    r <- correr(d_k, if (s == "sincov") "dr" else s, specs[[s]], sprintf("%s | %s", k, s))
    if (!is.null(r)) filas[[paste(k, s)]] <- cbind(muestra = k, umbral_pct = UMBRALES[[k]], especificacion = s,
                                                   peso_att_excluido_pct = peso_sale(excl), r)
  }
}
tabla <- do.call(rbind, filas)
write_csv(tabla, file.path(DIR_TAB, "53_controles_sin_poligonos.csv"))

titulo("RESUMEN Y REGLA")
print(tabla[, c("muestra", "especificacion", "n_controles", "peso_att_excluido_pct", "att", "ee",
                "ic_inf", "ic_sup", "gt_na")], row.names = FALSE, digits = 3)
if (nrow(tabla)) {
  c0 <- tabla[tabla$muestra == "completa", ]
  if (nrow(c0)) cat(sprintf("\nControl de consistencia: completa = %.1f (esperado %.1f) -> %s\n", c0$att,
                            PRINCIPAL["att"], if (abs(c0$att - PRINCIPAL["att"]) < 1) "OK" else "NO COINCIDE"))
  p1 <- tabla[tabla$muestra == "P1" & tabla$especificacion == "dr", ]
  if (nrow(p1)) {
    ok <- sign(p1$att) == sign(PRINCIPAL["att"]) && abs(p1$att - PRINCIPAL["att"]) < PRINCIPAL["ee"]
    cat(sprintf("Regla: DR sin controles cubiertos >= 5 %% = %.1f vs principal %.1f (1 EE = %.1f) -> %s\n",
                p1$att, PRINCIPAL["att"], PRINCIPAL["ee"],
                if (ok) "la principal es ROBUSTA a la contaminacion de controles"
                else "la principal NO es robusta a la contaminacion de controles"))
    cat(sprintf("Direccion (informativa): |ATT| %s de %.1f a %.1f -> %s\n",
                if (abs(p1$att) > abs(PRINCIPAL["att"])) "sube" else "baja", abs(PRINCIPAL["att"]), abs(p1$att),
                if (abs(p1$att) > abs(PRINCIPAL["att"]))
                  "compatible con controles parcialmente tratados que atenuaban el efecto"
                else "los controles cubiertos no atenuaban el efecto"))
  }
}
cat("Guardado: outputs/tablas/53_controles_sin_poligonos.csv\n")
