## =============================================================================
## 44_honestdid.R
##
## Sensibilidad de Rambachan y Roth (2023) sobre la ESPECIFICACION PRINCIPAL:
## D1 depurada, doblemente robusto con covariables, notyettreated, cohortes
## 2013-2019. Cierra el campo [• Rambachan y Roth: Mbar] del poster.
##
## Dos familias de restricciones:
##
##   Magnitudes relativas, Delta^RM(Mbar): la violacion de tendencias paralelas
##   entre periodos post consecutivos no excede Mbar veces la mayor violacion
##   observada entre periodos previos consecutivos. Mbar = 1: "la violacion
##   post no es peor que la peor observada antes".
##
##   Suavidad, Delta^SD(M): la pendiente de la tendencia diferencial cambia a
##   lo sumo M hectareas de un periodo al siguiente. M = 0 equivale a permitir
##   una tendencia diferencial lineal extrapolada desde el periodo previo.
##
## Valor de ruptura: el mayor Mbar (o M) para el que el intervalo robusto aun
## excluye el cero.
##
## REQUISITO: base_period = "universal". Con la base variable, cada coeficiente
## previo es la diferencia de un anio al siguiente y no son comparables entre
## si; HonestDiD necesita que todos se midan contra g-1. Los coeficientes POST
## son identicos en ambos casos (siempre contra g-1): el script lo verifica
## reproduciendo el ATT balanceado de la principal.
##
## Parametros de interes:
##   (a) e = 0, el efecto en el anio de adopcion (el caso estandar del paquete);
##   (b) el promedio de e = 0..5, que es la metrica del poster.
##
## Ventana previa: e = -5..-1. Con 18 periodos previos, la "mayor violacion
## observada" la fijaria el anio mas ruidoso de hace dos decadas; cinco anios
## es la ventana relevante para la dinamica cercana a la adopcion.
##
## USO:  Rscript scripts/44_honestdid.R
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(did)
  library(ggplot2)
})

if (!requireNamespace("HonestDiD", quietly = TRUE)) {
  stop("Falta HonestDiD. Instala con:\n",
       "  install.packages('HonestDiD', repos = 'https://cloud.r-project.org')\n",
       "y si CRAN no lo tiene disponible para tu version de R:\n",
       "  install.packages('remotes'); remotes::install_github('asheshrambachan/HonestDiD')")
}
library(HonestDiD)

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

## --- Parametros (identicos a la principal) -----------------------------------
SEMILLA   <- 20260824
OUTCOME   <- "loss_area_ha"
VENTANA   <- 2013:2019
E_PRE     <- 5
E_POST    <- 5
BITERS    <- 999
REFERENCIA_BAL <- -464.89      # ATT balanceado de la principal (40 y 43)

## Grillas: Mbar adimensional; M en hectareas por periodo al cuadrado.
MBAR_GRID <- seq(0, 2, by = 0.25)
M_GRID    <- c(0, 10, 25, 50, 100, 200)

COVARIABLES_XFORMLA <- ~ log_baseline_forest + temp_media_c_base +
  prec_anual_mm_base + discapital_base + disbogota_base + distancia_mercado_base +
  H_coca_base + homicidios_base + log_defor_pre_media + defor_pre_tendencia

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(t) cat("\n", strrep("=", 76), "\n", t, "\n", strrep("=", 76), "\n", sep = "")

cat("HonestDiD", as.character(packageVersion("HonestDiD")),
    "| did", as.character(packageVersion("did")), "\n")

## =============================================================================
## 1. Muestra principal (igual que 40 y 43)
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

panel <- panel %>% select(-any_of(c("G_dep", "muestra_dep"))) %>% inner_join(trat, by = "COD_DANE")
n_anios <- n_distinct(panel$year)
incompletos <- panel %>% group_by(COD_DANE) %>%
  summarise(ok = n() == n_anios && all(!is.na(.data[[OUTCOME]])), .groups = "drop") %>%
  filter(!ok) %>% pull(COD_DANE)
muestra <- panel %>%
  filter(!COD_DANE %in% incompletos, muestra_dep == 1, G_dep == 0 | G_dep %in% VENTANA)
cat(sprintf("Muestra: %d municipios | %d tratados\n", n_distinct(muestra$COD_DANE),
            n_distinct(muestra$COD_DANE[muestra$G_dep > 0])))

## =============================================================================
## 2. Estimacion con base universal
## =============================================================================
titulo("1. ESPECIFICACION PRINCIPAL CON base_period = 'universal'")
set.seed(SEMILLA)
cs <- suppressWarnings(att_gt(
  yname = OUTCOME, tname = "year", idname = "id_num", gname = "G_dep",
  xformla = COVARIABLES_XFORMLA, data = muestra,
  control_group = "notyettreated", est_method = "dr", base_period = "universal",
  bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "id_num"))

es <- aggte(cs, type = "dynamic", min_e = -E_PRE, max_e = E_POST,
            balance_e = E_POST, na.rm = TRUE)

## Validacion: el promedio de e = 0..5 debe ser el ATT balanceado de la principal.
post <- es$egt >= 0
att_bal <- mean(es$att.egt[post])
cat(sprintf("Promedio e = 0..%d con base universal: %.2f | principal: %.2f -> %s\n",
            E_POST, att_bal, REFERENCIA_BAL,
            if (abs(att_bal - REFERENCIA_BAL) < 0.5) "COINCIDE"
            else "NO COINCIDE: la muestra o la especificacion difieren"))

coef_es <- data.frame(e = es$egt, att = es$att.egt, ee = es$se.egt) %>%
  mutate(ic_inf = att - 1.96 * ee, ic_sup = att + 1.96 * ee)
cat("\nEstudio de eventos, base universal (e = -1 normalizado a cero):\n")
print(coef_es %>% mutate(across(where(is.numeric), ~ round(.x, 1))), row.names = FALSE)
write_csv(coef_es, file.path(DIR_TAB, "44_event_study_base_universal.csv"))

p <- ggdid(es) +
  labs(title = "Estudio de eventos de la especificacion principal, base universal (g-1)",
       subtitle = "Todos los coeficientes medidos contra el anio previo a la adopcion",
       x = "Anios desde la adopcion", y = "ATT (hectareas deforestadas)") +
  theme_minimal()
ggsave(file.path(DIR_FIG, "44_event_study_base_universal.png"), p, width = 9, height = 5.5, dpi = 150)

## =============================================================================
## 3. Puente did -> HonestDiD
## Adaptado del metodo honest_did.AGGTEobj de la documentacion de HonestDiD:
## la matriz de covarianzas sale de la funcion de influencia del estudio de
## eventos, y el coeficiente de referencia (e = -1, fijado en cero) se elimina.
## =============================================================================
preparar <- function(es) {
  if (es$type != "dynamic") stop("Se requiere una agregacion dinamica")
  if (es$DIDparams$base_period != "universal") stop("Se requiere base_period = 'universal'")
  IF <- es$inf.function$dynamic.inf.func.e
  n  <- nrow(IF)
  V  <- crossprod(IF) / n / n
  ref <- which(es$egt == -1)
  list(beta = es$att.egt[-ref], sigma = V[-ref, -ref, drop = FALSE],
       npre = sum(es$egt < -1), npost = sum(es$egt >= 0))
}
h <- preparar(es)
cat(sprintf("\nPeriodos previos usados: %d | posteriores: %d\n", h$npre, h$npost))

vectores_l <- list(
  e0          = basisVector(index = 1, size = h$npost),
  promedio_0_5 = matrix(rep(1 / h$npost, h$npost), ncol = 1)
)

ruptura <- function(tab, par) {
  sig <- tab$ub < 0 | tab$lb > 0
  if (!any(sig)) return(0)
  ## El mayor valor de la grilla con intervalo que excluye el cero
  max(tab[[par]][sig])
}

resultados <- list()
titulo("2. SENSIBILIDAD")
for (nm in names(vectores_l)) {
  l <- vectores_l[[nm]]
  cat("\n---- Parametro:", if (nm == "e0") "efecto en e = 0" else "promedio de e = 0..5", "----\n")

  orig <- constructOriginalCS(betahat = h$beta, sigma = h$sigma,
                              numPrePeriods = h$npre, numPostPeriods = h$npost, l_vec = l)
  cat(sprintf("Intervalo convencional: [%.1f ; %.1f]\n", orig$lb, orig$ub))

  rm <- tryCatch(
    createSensitivityResults_relativeMagnitudes(
      betahat = h$beta, sigma = h$sigma, numPrePeriods = h$npre, numPostPeriods = h$npost,
      l_vec = l, Mbarvec = MBAR_GRID),
    error = function(e) { cat("ERROR magnitudes relativas:", conditionMessage(e), "\n"); NULL })
  sd <- tryCatch(
    createSensitivityResults(
      betahat = h$beta, sigma = h$sigma, numPrePeriods = h$npre, numPostPeriods = h$npost,
      l_vec = l, Mvec = M_GRID),
    error = function(e) { cat("ERROR suavidad:", conditionMessage(e), "\n"); NULL })

  if (!is.null(rm)) {
    cat("\nMagnitudes relativas:\n")
    print(as.data.frame(rm) %>% mutate(across(where(is.numeric), ~ round(.x, 1))), row.names = FALSE)
    cat(sprintf("Valor de ruptura Mbar: %s\n", format(ruptura(rm, "Mbar"))))
    g <- tryCatch(createSensitivityPlot_relativeMagnitudes(rm, orig) +
                    labs(title = paste("Magnitudes relativas -", nm),
                         y = "IC robusto (hectareas)") + theme_minimal(),
                  error = function(e) NULL)
    if (!is.null(g)) ggsave(file.path(DIR_FIG, paste0("44_honestdid_rm_", nm, ".png")),
                            g, width = 8, height = 5, dpi = 150)
  }
  if (!is.null(sd)) {
    cat("\nSuavidad:\n")
    print(as.data.frame(sd) %>% mutate(across(where(is.numeric), ~ round(.x, 1))), row.names = FALSE)
    cat(sprintf("Valor de ruptura M: %s\n", format(ruptura(sd, "M"))))
    g <- tryCatch(createSensitivityPlot(sd, orig) +
                    labs(title = paste("Suavidad -", nm), y = "IC robusto (hectareas)") +
                    theme_minimal(),
                  error = function(e) NULL)
    if (!is.null(g)) ggsave(file.path(DIR_FIG, paste0("44_honestdid_sd_", nm, ".png")),
                            g, width = 8, height = 5, dpi = 150)
  }
  resultados[[nm]] <- list(original = orig, magnitudes_relativas = rm, suavidad = sd,
                           ruptura_Mbar = if (is.null(rm)) NA else ruptura(rm, "Mbar"),
                           ruptura_M = if (is.null(sd)) NA else ruptura(sd, "M"))
}

## =============================================================================
## 4. Tabla resumen para el poster y el capitulo
## =============================================================================
titulo("3. RESUMEN")
fila_rm <- function(nm, mb) {
  rm <- resultados[[nm]]$magnitudes_relativas
  if (is.null(rm)) return(c(NA, NA))
  r <- rm[abs(rm$Mbar - mb) < 1e-9, ]
  if (!nrow(r)) c(NA, NA) else c(r$lb, r$ub)
}
resumen <- do.call(rbind, lapply(names(resultados), function(nm) {
  o <- resultados[[nm]]$original
  data.frame(parametro = nm,
             ic_convencional = sprintf("[%.1f ; %.1f]", o$lb, o$ub),
             ic_rm_Mbar_0.5  = do.call(sprintf, c("[%.1f ; %.1f]", as.list(fila_rm(nm, 0.5)))),
             ic_rm_Mbar_1    = do.call(sprintf, c("[%.1f ; %.1f]", as.list(fila_rm(nm, 1)))),
             ruptura_Mbar = resultados[[nm]]$ruptura_Mbar,
             ruptura_M    = resultados[[nm]]$ruptura_M)
}))
print(resumen, row.names = FALSE)
write_csv(resumen, file.path(DIR_TAB, "44_honestdid_resumen.csv"))

saveRDS(list(att_gt = cs, event_study = es, honestdid = resultados,
             parametros = list(e_pre = E_PRE, e_post = E_POST, mbar = MBAR_GRID, m = M_GRID)),
        file.path(DIR_OUT, "44_honestdid.rds"))
cat("\nGuardado: outputs/44_honestdid.rds, tablas/44_*.csv, figuras/44_*.png\n")
