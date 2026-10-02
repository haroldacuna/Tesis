## =============================================================================
## 00_config_did.R
##
## Fuente UNICA de los parametros de estimacion (tarea B.1). Todos los scripts
## de estimacion la cargan con:
##
##   source("scripts/00_config_did.R")      # correr desde tesis_gfc/
##
## y NO redefinen nada de lo que esta aqui. Cambiar un valor de este archivo es
## una decision de especificacion: se registra en docs/decisiones_pre_estimacion.md
## antes de volver a correr.
##
## Contenido:
##   1. Raiz del proyecto y carpetas de salida
##   2. Parametros de la especificacion principal (D1, D4, D7, D12, D13)
##   3. Valores de referencia de resultados-v1 (para validar que cada script
##      trabaja sobre la misma muestra que el 40)
##   4. Rutas de insumos compartidos
##   5. Utilidades
##   6. cargar_muestra_principal(): la muestra exacta de la principal
## =============================================================================

if (exists(".CONFIG_DID_CARGADA") && isTRUE(.CONFIG_DID_CARGADA)) {
  ## Ya cargada en esta sesion: no repetir mensajes
} else {

## --- 1. Raiz y carpetas -------------------------------------------------------
RAIZ_PROYECTO <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "data/.raiz_canonica"))) {
    p <- dirname(d)
    if (identical(p, d)) stop("No encuentro data/.raiz_canonica subiendo desde ", getwd())
    d <- p
  }
  d
})
RAIZ    <- RAIZ_PROYECTO                      # alias usado por 44, 58, 59, 60
DIR_OUT <- file.path(RAIZ_PROYECTO, "outputs")
DIR_TAB <- file.path(DIR_OUT, "tablas")
DIR_FIG <- file.path(DIR_OUT, "figuras")
for (.d in c(DIR_OUT, DIR_TAB, DIR_FIG)) dir.create(.d, showWarnings = FALSE, recursive = TRUE)

## --- 2. Parametros (decididos ANTES de ver resultados) -----------------------
SEMILLA    <- 20260824
OUTCOME    <- "loss_area_ha"
VENTANA    <- 2013:2019        # cohortes reportables (D1)
BALANCE_E  <- 5                # 2019 + 5 = 2024: todas las cohortes de la ventana llegan
BITERS     <- 999
E_PRE      <- 5                # ventana de eventos para HonestDiD y figuras
E_POST     <- 5
MIN_COH    <- 3                # minimo de municipios para reportar una cohorte (D1)
UMBRAL_PCT <- 5                # % del municipio cubierto por un poligono (A.7, A.9, 53)

## D4: lista oficial de covariables del modelo doblemente robusto. La
## deforestacion previa entra en ambos modelos (did no los separa); la
## sensibilidad sin esas dos variables se reporta aparte.
COVARIABLES_XFORMLA <- ~ log_baseline_forest + temp_media_c_base +
  prec_anual_mm_base + discapital_base + disbogota_base + distancia_mercado_base +
  H_coca_base + homicidios_base + log_defor_pre_media + defor_pre_tendencia

## --- 3. Referencias de resultados-v1 ------------------------------------------
## Salen del 40 (corrida sin Mogotes, oct-2026). Los scripts que trabajan sobre
## la muestra principal deben reproducirlas; si no, se detienen.
REF_N_TRATADOS     <- 36
REF_N_CONTROLES    <- 986
REF_ATT_DR         <- -213.73     # ATT simple, DR con covariables (principal)
REF_ATT_SINCOV     <- -183.29     # ATT simple, sin covariables
REF_BAL_DR         <- -235.31     # ATT balanceado e = 0..5, DR
REF_BAL_SINCOV     <- -247.35     # ATT balanceado e = 0..5, sin covariables

## --- 4. Rutas de insumos compartidos ------------------------------------------
RUTA_PANEL_DID   <- file.path(DIR_OUT, "panel_analisis_did.rds")                       # 01
RUTA_MUESTRA_A   <- file.path(DIR_OUT, "40_muestra_A.rds")                             # 40
RUTA_COV_PRE     <- file.path(DIR_TAB, "covariables_pre_municipio.csv")                # 02/47
RUTA_PANEL_TRAT  <- file.path(RAIZ_PROYECTO, "data/final/panel_con_tratamiento_actualizado.csv")  # 26
RUTA_DOSIS       <- file.path(RAIZ_PROYECTO, "data/interim/dosis_tratamiento_final.csv")          # 39
RUTA_COBERTURA   <- file.path(RAIZ_PROYECTO, "data/interim/proyectos_poligonos/cobertura_municipal.csv")  # 52
RUTA_MUNICIPIOS  <- file.path(RAIZ_PROYECTO, "data/interim/municipios_clean.gpkg")
RUTA_VA          <- file.path(RAIZ_PROYECTO, "data/raw/renare_geovisor_iniciativas.geojson")
RUTA_FILTRO_SECT <- file.path(RAIZ_PROYECTO, "scripts/filtro_sectorial.py")       # ids D2

## --- 5. Utilidades -------------------------------------------------------------
pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(txt) cat("\n", strrep("=", 76), "\n", txt, "\n", strrep("=", 76), "\n", sep = "")

## --- 6. Muestra principal ------------------------------------------------------
## La muestra exacta de la especificacion principal: la que guarda el 40 mas
## los controles limpios (idempotente). Verifica tamanios contra las referencias.
cargar_muestra_principal <- function(verificar = TRUE) {
  if (!file.exists(RUTA_MUESTRA_A))
    stop("Falta ", RUTA_MUESTRA_A, ": corre primero scripts/40_did_depurada_y_dosis.R")
  m <- readRDS(RUTA_MUESTRA_A)
  m$COD_DANE <- pad5(m$COD_DANE)
  source(file.path(RAIZ_PROYECTO, "scripts/comun_controles_limpios.R"), local = TRUE)
  cont <- municipios_con_proyecto(RUTA_PANEL_TRAT)
  m <- aplicar_controles_limpios(m, cont, col_g = "G_dep")
  mun <- unique(m[, c("COD_DANE", "G_dep")])
  n_t <- sum(mun$G_dep > 0); n_c <- sum(mun$G_dep == 0)
  cat(sprintf("Muestra principal: %d municipios | %d tratados | %d controles\n", nrow(mun), n_t, n_c))
  if (verificar && (n_t != REF_N_TRATADOS || n_c != REF_N_CONTROLES))
    stop(sprintf("La muestra (%d tratados, %d controles) no coincide con la referencia (%d, %d).",
                 n_t, n_c, REF_N_TRATADOS, REF_N_CONTROLES))
  m
}

.CONFIG_DID_CARGADA <- TRUE
cat(sprintf("[config] 00_config_did.R | semilla %d | ventana %d-%d | %d covariables | raiz %s\n",
            SEMILLA, min(VENTANA), max(VENTANA), length(all.vars(COVARIABLES_XFORMLA)), RAIZ_PROYECTO))
}
