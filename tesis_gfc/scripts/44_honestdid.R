## =============================================================================
## 44_honestdid.R
##
## Tarea A.3 (OE2). Sensibilidad de Rambachan y Roth (2023) sobre la
## especificacion principal (D1 depurada, DR, notyettreated, cohortes
## 2013-2019, controles limpios) y sobre su version sin covariables, que es la
## referencia del script 59.
##
## Version oct-2026: misma muestra que 58 y 59 (40_muestra_A.rds + controles
## limpios: 36 tratados, 986 controles). La version anterior construia su
## propia muestra SIN controles limpios.
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
## Referencia empirica para M (suavidad): M_obs = mayor |segunda diferencia|
## de los coeficientes previos (e = -5..-1). Es la mayor curvatura que la
## tendencia diferencial ya mostro antes de la adopcion.
##
## Interpretacion fijada ANTES de correr (D13):
##   - Magnitudes relativas: valor de ruptura Mbar < 0,5 -> interpretacion
##     causal fragil.
##   - Suavidad: si el intervalo con M = M_obs incluye el cero, el efecto no
##     sobrevive a una curvatura como la ya observada -> fragil. Si solo excluye
##     el cero con M cercano a 0, el resultado depende de extrapolar en linea
##     recta la aceleracion previa a la adopcion; con un perfil de pico (A.2),
##     esa extrapolacion no es creible y no se lee como efecto.
##
## USO:  Rscript scripts/44_honestdid.R
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(did)
  library(ggplot2)
})
source("scripts/00_config_did.R")   # B.1: parametros comunes (no redefinir aqui)

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
E_PRE     <- 5
E_POST    <- 5
## ATT balanceado e = 0..5 del 40 (corrida sin Mogotes, oct-2026). El script
## verifica que el promedio post con base universal los reproduzca.
REFERENCIA_BAL <- c(dr = -235.31, sincov = -247.35)
ESPECIFICACIONES <- list(dr = "DR con covariables (principal)", sincov = "sin covariables (referencia del 59)")

## Grillas: Mbar adimensional; M en hectareas por periodo al cuadrado.
MBAR_GRID <- seq(0, 2, by = 0.25)
M_GRID    <- c(0, 10, 25, 50, 100, 200)


pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(t) cat("\n", strrep("=", 76), "\n", t, "\n", strrep("=", 76), "\n", sep = "")

cat("HonestDiD", as.character(packageVersion("HonestDiD")),
    "| did", as.character(packageVersion("did")), "\n")

## =============================================================================
## 1. Muestra principal (igual que 40 y 43)
## =============================================================================
ruta_m <- file.path(DIR_OUT, "40_muestra_A.rds")
if (!file.exists(ruta_m)) stop("Falta ", ruta_m, ": corre el 40 con el saveRDS(muestra_A, ...).")
muestra <- readRDS(ruta_m)
muestra$COD_DANE <- pad5(muestra$COD_DANE)

## Controles limpios con la misma funcion comun que 40, 58 y 59 (idempotente).
source(file.path(RAIZ, "scripts/comun_controles_limpios.R"))
cont <- municipios_con_proyecto(file.path(RAIZ, "data/final/panel_con_tratamiento_actualizado.csv"))
muestra <- aplicar_controles_limpios(muestra, cont, col_g = "G_dep")

faltan <- setdiff(c(OUTCOME, "year", "id_num", "G_dep", all.vars(COVARIABLES_XFORMLA)), names(muestra))
if (length(faltan)) stop("La muestra no trae: ", paste(faltan, collapse = ", "))
mun <- muestra %>% distinct(COD_DANE, G_dep)
cat(sprintf("Muestra: %d municipios | %d tratados | %d controles\n",
            nrow(mun), sum(mun$G_dep > 0), sum(mun$G_dep == 0)))
if (sum(mun$G_dep == 0) != 986 || sum(mun$G_dep > 0) != 36)
  stop("La muestra no coincide con la principal (36 tratados, 986 controles).")

## =============================================================================
## 2. Estimacion con base universal
## =============================================================================
estimar <- function(spec) {
  xf <- if (spec == "dr") COVARIABLES_XFORMLA else ~1
  titulo(paste0("1. ", toupper(ESPECIFICACIONES[[spec]]), ", base_period = 'universal'"))
  set.seed(SEMILLA)
  cs <- suppressWarnings(att_gt(
    yname = OUTCOME, tname = "year", idname = "id_num", gname = "G_dep",
    xformla = xf, data = muestra,
    control_group = "notyettreated", est_method = "dr",   # con ~1, dr = reg = ipw
    base_period = "universal",
    bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "id_num"))
  es <- aggte(cs, type = "dynamic", min_e = -E_PRE, max_e = E_POST,
              balance_e = E_POST, na.rm = TRUE)

  ## Validacion: el promedio de e = 0..5 debe ser el ATT balanceado del 40.
  att_bal <- mean(es$att.egt[es$egt >= 0])
  ok <- abs(att_bal - REFERENCIA_BAL[[spec]]) < 0.5
  cat(sprintf("Promedio e = 0..%d con base universal: %.2f | 40: %.2f -> %s\n",
              E_POST, att_bal, REFERENCIA_BAL[[spec]],
              if (ok) "COINCIDE" else "NO COINCIDE: la muestra o la especificacion difieren"))
  if (!ok) stop("Detenido: sin coincidencia con el 40 los intervalos robustos no son comparables.")

  coef_es <- data.frame(e = es$egt, att = es$att.egt, ee = es$se.egt) %>%
    mutate(ic_inf = att - 1.96 * ee, ic_sup = att + 1.96 * ee)
  cat("\nEstudio de eventos, base universal (e = -1 normalizado a cero):\n")
  print(coef_es %>% mutate(across(where(is.numeric), ~ round(.x, 1))), row.names = FALSE)
  write_csv(coef_es, file.path(DIR_TAB, paste0("44_event_study_base_universal_", spec, ".csv")))

  p <- ggdid(es) +
    labs(title = paste("Estudio de eventos, base universal (g-1) -", ESPECIFICACIONES[[spec]]),
         subtitle = "Todos los coeficientes medidos contra el anio previo a la adopcion",
         x = "Anios desde la adopcion", y = "ATT (hectareas deforestadas)") +
    theme_minimal()
  ggsave(file.path(DIR_FIG, paste0("44_event_study_base_universal_", spec, ".png")), p,
         width = 9, height = 5.5, dpi = 150)
  list(cs = cs, es = es)
}

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
sensibilidad <- function(es, spec) {
  h <- preparar(es)
  cat(sprintf("\nPeriodos previos usados: %d | posteriores: %d\n", h$npre, h$npost))

  ## Referencia empirica para M: mayor curvatura observada antes de la adopcion.
  ## Se agrega el cero de e = -1 para que la ultima segunda diferencia lo incluya.
  pre <- c(h$beta[seq_len(h$npre)], 0)
  M_obs <- if (length(pre) >= 3) max(abs(diff(pre, differences = 2))) else NA_real_
  cat(sprintf("M_obs (mayor |segunda diferencia| previa): %.1f ha\n", M_obs))
  m_grid <- sort(unique(c(M_GRID, if (is.finite(M_obs)) round(M_obs) else NULL)))

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
  titulo(paste("2. SENSIBILIDAD -", ESPECIFICACIONES[[spec]]))
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
    ## Suavidad: el metodo por defecto (FLCI) usa CVXR, y HonestDiD 0.2.8 es
    ## incompatible con las versiones recientes de CVXR. Si falla, se usa el
    ## metodo condicional hibrido (C-LF), el mismo de magnitudes relativas: es
    ## valido para Delta^SD aunque puede dar intervalos algo mas anchos.
    sd <- tryCatch(
      createSensitivityResults(
        betahat = h$beta, sigma = h$sigma, numPrePeriods = h$npre, numPostPeriods = h$npost,
        l_vec = l, Mvec = m_grid),
      error = function(e) {
        cat("Aviso: FLCI fallo (", conditionMessage(e), "); se reintenta con method = 'C-LF'\n")
        tryCatch(
          createSensitivityResults(
            betahat = h$beta, sigma = h$sigma, numPrePeriods = h$npre, numPostPeriods = h$npost,
            l_vec = l, Mvec = m_grid, method = "C-LF"),
          error = function(e2) { cat("ERROR suavidad (C-LF):", conditionMessage(e2), "\n"); NULL })
      })

    if (!is.null(rm)) {
      cat("\nMagnitudes relativas:\n")
      print(as.data.frame(rm) %>% mutate(across(where(is.numeric), ~ round(.x, 1))), row.names = FALSE)
      cat(sprintf("Valor de ruptura Mbar: %s\n", format(ruptura(rm, "Mbar"))))
      g <- tryCatch(createSensitivityPlot_relativeMagnitudes(rm, orig) +
                      labs(title = paste("Magnitudes relativas -", spec, "-", nm),
                           y = "IC robusto (hectareas)") + theme_minimal(),
                    error = function(e) NULL)
      if (!is.null(g)) ggsave(file.path(DIR_FIG, paste0("44_honestdid_rm_", spec, "_", nm, ".png")),
                              g, width = 8, height = 5, dpi = 150)
    }
    if (!is.null(sd)) {
      cat("\nSuavidad:\n")
      print(as.data.frame(sd) %>% mutate(across(where(is.numeric), ~ round(.x, 1))), row.names = FALSE)
      cat(sprintf("Valor de ruptura M: %s\n", format(ruptura(sd, "M"))))
      r_obs <- sd[abs(sd$M - round(M_obs)) < 1e-9, ]
      if (nrow(r_obs)) cat(sprintf("Con M = M_obs (%.0f): IC [%.1f ; %.1f] -> %s\n", round(M_obs), r_obs$lb, r_obs$ub,
          if (r_obs$lb > 0 | r_obs$ub < 0) "excluye el cero" else "incluye el cero: FRAGIL segun D13"))
      g <- tryCatch(createSensitivityPlot(sd, orig) +
                      labs(title = paste("Suavidad -", spec, "-", nm), y = "IC robusto (hectareas)") +
                      theme_minimal(),
                    error = function(e) NULL)
      if (!is.null(g)) ggsave(file.path(DIR_FIG, paste0("44_honestdid_sd_", spec, "_", nm, ".png")),
                              g, width = 8, height = 5, dpi = 150)
    }
    resultados[[nm]] <- list(original = orig, magnitudes_relativas = rm, suavidad = sd,
                             ruptura_Mbar = if (is.null(rm)) NA else ruptura(rm, "Mbar"),
                             ruptura_M = if (is.null(sd)) NA else ruptura(sd, "M"),
                             M_obs = M_obs)
  }
  resultados
}

## =============================================================================
## 4. Correr ambas especificaciones y resumir
## =============================================================================
fila_rm <- function(resultados, nm, mb) {
  rm <- resultados[[nm]]$magnitudes_relativas
  if (is.null(rm)) return(c(NA, NA))
  r <- rm[abs(rm$Mbar - mb) < 1e-9, ]
  if (!nrow(r)) c(NA, NA) else c(r$lb, r$ub)
}
fila_sd <- function(resultados, nm, m) {
  sd <- resultados[[nm]]$suavidad
  if (is.null(sd) || !is.finite(m)) return(c(NA, NA))
  r <- sd[abs(sd$M - m) < 1e-9, ]
  if (!nrow(r)) c(NA, NA) else c(r$lb, r$ub)
}
ic <- function(v) do.call(sprintf, c("[%.1f ; %.1f]", as.list(v)))

todo <- list(); resumen <- list()
for (spec in names(ESPECIFICACIONES)) {
  est <- estimar(spec)
  res <- sensibilidad(est$es, spec)
  todo[[spec]] <- list(att_gt = est$cs, event_study = est$es, honestdid = res)
  for (nm in names(res)) {
    o <- res[[nm]]$original; mo <- round(res[[nm]]$M_obs)
    resumen[[length(resumen) + 1]] <- data.frame(
      especificacion = spec, parametro = nm,
      ic_convencional = sprintf("[%.1f ; %.1f]", o$lb, o$ub),
      ic_rm_Mbar_0.5  = ic(fila_rm(res, nm, 0.5)),
      ic_rm_Mbar_1    = ic(fila_rm(res, nm, 1)),
      ic_sd_M_0       = ic(fila_sd(res, nm, 0)),
      M_obs           = mo,
      ic_sd_M_obs     = ic(fila_sd(res, nm, mo)),
      ruptura_Mbar = res[[nm]]$ruptura_Mbar,
      ruptura_M    = res[[nm]]$ruptura_M)
  }
}

titulo("3. RESUMEN")
resumen <- do.call(rbind, resumen)
print(resumen, row.names = FALSE)
write_csv(resumen, file.path(DIR_TAB, "44_honestdid_resumen.csv"))

saveRDS(list(resultados = todo,
             parametros = list(e_pre = E_PRE, e_post = E_POST, mbar = MBAR_GRID, m = M_GRID)),
        file.path(DIR_OUT, "44_honestdid.rds"))
cat("\nGuardado: outputs/44_honestdid.rds, tablas/44_*.csv, figuras/44_*.png\n")
