## =============================================================================
## 51_psm_principal.R
##
## Cumple el plan de emparejamiento para la especificacion principal ACTUAL
## (D1 depurada, cohortes 2013-2019, controles limpios). Hasta el 27/09/2026 el
## 02 solo emparejaba las definiciones anteriores (todas_redd como "principal").
##
## Insumo: outputs/matching_d1_depurada.rds (02_matching_psm.R, seccion 5b):
## emparejamiento completo (MatchIt, method = "full", discard = "both"), con
## los mismos parametros que las demas definiciones.
##
## Estimaciones (Callaway-Sant'Anna, notyettreated, Hansen 2001-2024):
##   (c)  submuestra emparejada, sin covariables, ponderada por peso_psm
##        (misma especificacion que la (c) del 03)
##   (c') submuestra emparejada, DR con covariables, ponderada por peso_psm
##   (ref) DR principal sin ponderar sobre la MISMA submuestra emparejada, para
##        separar el efecto de la muestra del efecto de los pesos
##
## REGLA (fijada antes de correr): la estimacion ponderada (c) es CONSISTENTE
## con la principal (-207.9, EE 198.5) si tiene el mismo signo y esta a menos
## de 1 EE de ella. Si el emparejamiento descarta tratados (discard = "both"),
## el estimando cambia (ATT sobre los tratados con soporte): se listan.
##
## USO
##   Rscript scripts/51_psm_principal.R
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
PRINCIPAL <- c(att = REF_ATT_DR, ee = REF_EE_DR)
X <- COVARIABLES_XFORMLA   # B.1: lista oficial de 00_config_did.R

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(t) cat("\n", strrep("=", 74), "\n", t, "\n", strrep("=", 74), "\n", sep = "")

## --- Emparejamiento de la principal -----------------------------------------
ruta_m <- file.path(DIR_OUT, "matching_d1_depurada.rds")
if (!file.exists(ruta_m)) stop("Falta ", ruta_m, ". Corre la version actual de 02_matching_psm.R.")
m <- readRDS(ruta_m)
if (is.null(m)) stop("El emparejamiento de la principal no se pudo estimar (ver log del 02).")
pesos <- m$pesos %>% mutate(COD_DANE = pad5(COD_DANE))
if (anyDuplicated(pesos$COD_DANE)) stop("COD_DANE duplicado en los pesos del emparejamiento")

## --- Muestra principal (igual que el 40) ------------------------------------
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

## --- Quien queda y quien sale -----------------------------------------------
titulo("SUBMUESTRA EMPAREJADA")
mun_base <- base %>% distinct(COD_DANE, G_dep)
fuera_emp <- setdiff(mun_base$COD_DANE, pesos$COD_DANE)
ajenos    <- setdiff(pesos$COD_DANE, mun_base$COD_DANE)
if (length(ajenos)) stop("El emparejamiento contiene municipios fuera de la muestra principal: ",
                         paste(head(ajenos, 10), collapse = ", "),
                         ". El .rds no corresponde a esta muestra: vuelve a correr el 02.")
tr_fuera <- mun_base %>% filter(G_dep > 0, COD_DANE %in% fuera_emp) %>% left_join(nombres, by = "COD_DANE")
cat(sprintf("Muestra principal: %d tratados, %d controles\n",
            sum(mun_base$G_dep > 0), sum(mun_base$G_dep == 0)))
cat(sprintf("Emparejados: %d tratados, %d controles | pesos en [%.4f, %.4f]\n",
            sum(mun_base$G_dep > 0 & mun_base$COD_DANE %in% pesos$COD_DANE),
            sum(mun_base$G_dep == 0 & mun_base$COD_DANE %in% pesos$COD_DANE),
            min(pesos$peso_psm), max(pesos$peso_psm)))
if (nrow(tr_fuera)) {
  cat("Tratados descartados por el emparejamiento (el estimando pasa a ser el ATT sobre\n")
  cat("los tratados con soporte comun):\n")
  print(as.data.frame(tr_fuera), row.names = FALSE)
} else cat("No se descarta ningun tratado: el estimando es el mismo ATT de la principal.\n")
w <- pesos$peso_psm[pesos$COD_DANE %in% mun_base$COD_DANE[mun_base$G_dep == 0]]
cat(sprintf("Tamanio muestral efectivo de los controles emparejados: %.1f de %d\n",
            sum(w)^2 / sum(w^2), length(w)))

sub <- base %>% filter(COD_DANE %in% pesos$COD_DANE) %>%
  left_join(pesos %>% select(COD_DANE, peso_psm), by = "COD_DANE")
if (anyNA(sub$peso_psm)) stop("Municipios emparejados sin peso")

## --- Estimacion -------------------------------------------------------------
correr <- function(d, xformla, pesos_col, etq) {
  cat("\n--- ", etq, " ---\n", sep = "")
  d <- as.data.frame(d); d$.gname <- d$G_dep
  set.seed(SEMILLA)
  out <- suppressWarnings(tryCatch(
    att_gt(yname = OUTCOME, tname = "year", idname = "id_num", gname = ".gname",
           xformla = xformla, data = d, weightsname = pesos_col,
           control_group = "notyettreated", est_method = "dr",
           bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "id_num"),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }))
  if (is.null(out)) return(NULL)
  s   <- aggte(out, type = "simple", na.rm = TRUE)
  bal <- tryCatch(aggte(out, type = "dynamic", balance_e = BAL_E, min_e = 0, max_e = BAL_E,
                        na.rm = TRUE), error = function(e) NULL)
  cat(sprintf("ATT %.1f (EE %.1f) IC95 [%.1f, %.1f] | g-t NA %d/%d\n", s$overall.att, s$overall.se,
              s$overall.att - 1.96 * s$overall.se, s$overall.att + 1.96 * s$overall.se,
              sum(is.na(out$att)), length(out$att)))
  data.frame(especificacion = etq, att = s$overall.att, ee = s$overall.se,
             ic_inf = s$overall.att - 1.96 * s$overall.se, ic_sup = s$overall.att + 1.96 * s$overall.se,
             att_balanceado = if (is.null(bal)) NA_real_ else bal$overall.att,
             gt_na = sum(is.na(out$att)),
             n_tratados = n_distinct(d$COD_DANE[d$G_dep > 0]),
             n_controles = n_distinct(d$COD_DANE[d$G_dep == 0]), row.names = NULL)
}

titulo("ESTIMACION")
tabla <- rbind(
  correr(sub, ~1, "peso_psm", "(c) emparejada, sin covariables, ponderada por PSM"),
  correr(sub, X,  "peso_psm", "(c') emparejada, DR con covariables, ponderada por PSM"),
  correr(sub, X,  NULL,       "(ref) DR principal sin ponderar, misma submuestra")
)
write_csv(tabla, file.path(DIR_TAB, "51_psm_principal.csv"))

titulo("RESUMEN Y REGLA")
print(tabla[, c("especificacion", "n_tratados", "n_controles", "att", "ee", "ic_inf", "ic_sup", "gt_na")],
      row.names = FALSE, digits = 3)
c_psm <- tabla[grepl("^\\(c\\) ", tabla$especificacion), ]
if (nrow(c_psm)) {
  ok <- sign(c_psm$att) == sign(PRINCIPAL["att"]) && abs(c_psm$att - PRINCIPAL["att"]) < PRINCIPAL["ee"]
  cat(sprintf("\nRegla: (c) = %.1f vs principal %.1f (1 EE = %.1f) -> %s\n", c_psm$att, PRINCIPAL["att"],
              PRINCIPAL["ee"], if (ok) "CONSISTENTE con la principal" else "NO consistente con la principal"))
}
cat("Guardado: outputs/tablas/51_psm_principal.csv\n")
