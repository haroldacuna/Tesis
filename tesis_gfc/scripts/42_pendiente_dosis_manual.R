## =============================================================================
## 42_pendiente_dosis_manual.R
##
## Estima la pendiente dosis-respuesta a mano, con errores estandar creibles.
##
## Motivacion: contdid 0.1.1 reporta para la ACRT errores estandar ~200 veces
## menores que los del ATT. El diagnostico: con la especificacion lineal,
##     ATT(d) = E[dY | D = d] - E[dY | D = 0] = (a + b d) - mu0,
## el grupo de control solo aporta la constante mu0, asi que la pendiente b sale
## UNICAMENTE de los tratados y no puede depender de los controles. En las
## corridas, b fue identica con todos los controles y con los regionales,
## mientras su EE cambiaba: el EE del paquete refleja el lado equivocado.
##
## Procedimiento:
##   1. Descompone el ATT balanceado de Callaway-Sant'Anna (e = 0..5, base g-1,
##      nunca tratados) en la contribucion ATT_i de cada municipio tratado.
##      VALIDACION: el promedio simple de ATT_i debe coincidir con el ATT
##      balanceado del puente binario de 40. Si coincide, la descomposicion es
##      exactamente la de did.
##   2. Regresa ATT_i sobre la dosis: agrupado y con efectos fijos de cohorte
##      (este ultimo identifica b dentro de cohorte, como contdid).
##   3. Inferencia triple:
##        - HC3 (robusto a heterocedasticidad, corregido para muestras chicas)
##        - bootstrap estratificado: remuestrea tratados (dentro de cohorte) Y
##          controles, recalculando el contrafactual en cada replica
##        - permutacion de la dosis entre tratados (exacta en muestra finita,
##          sin supuestos asintoticos)
##
## Grilla: outcome (relativo, hectareas) x base (g-1, promedio g-5..g-1)
##         x muestra (21 de contdid, 25 con dosis, sin censura) x especificacion.
##
## USO:  Rscript scripts/42_pendiente_dosis_manual.R
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
})

RAIZ <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "data/.raiz_canonica"))) {
    p <- dirname(d); if (identical(p, d)) stop("Sin data/.raiz_canonica"); d <- p
  }
  d
})
DIR_TAB <- file.path(RAIZ, "outputs/tablas")
DIR_FIG <- file.path(RAIZ, "outputs/figuras")
for (x in c(DIR_TAB, DIR_FIG)) dir.create(x, showWarnings = FALSE, recursive = TRUE)

## --- Parametros (los mismos de 40) --------------------------------------------
OUTCOME   <- "loss_area_ha"
VENTANA   <- 2013:2019
PRE_COMUN <- 2001:2012
E_MAX     <- 5          # ventana post balanceada e = 0..5
E_PRE     <- 5          # base alternativa: promedio de g-5..g-1
MIN_DOSIS_DISTINTAS <- 3
B_BOOT    <- 1999
B_PERM    <- 9999
SEMILLA   <- 20260824

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(t) cat("\n", strrep("=", 76), "\n", t, "\n", strrep("=", 76), "\n", sep = "")

## =============================================================================
## 1. Datos
## =============================================================================
panel <- readRDS(file.path(RAIZ, "outputs/panel_analisis_did.rds"))
panel$COD_DANE <- pad5(panel$COD_DANE)

trat <- read_csv(file.path(RAIZ, "data/final/panel_con_tratamiento_actualizado.csv"),
                 col_select = c(COD_DANE, anio_inicio_tratamiento_todas_fuentes_redd_depurada,
                                muestra_redd_depurada),
                 col_types = cols(COD_DANE = col_character(), .default = col_double()),
                 show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>%
  group_by(COD_DANE) %>%
  summarise(G = coalesce(min_na(anio_inicio_tratamiento_todas_fuentes_redd_depurada), 0),
            muestra = min(muestra_redd_depurada), .groups = "drop")

dosis <- read_csv(file.path(RAIZ, "data/interim/dosis_tratamiento_final.csv"),
                  col_types = cols(COD_DANE = col_character()), show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>%
  select(COD_DANE, NOMBRE_MPI, tiene_dosis, dosis_bosque)

d <- panel %>%
  transmute(COD_DANE, year = as.integer(year), Y = as.numeric(.data[[OUTCOME]])) %>%
  inner_join(trat, by = "COD_DANE") %>%
  left_join(dosis, by = "COD_DANE") %>%
  filter(muestra == 1, G == 0 | (G %in% VENTANA & tiene_dosis %in% 1))

## Panel balanceado y outcome relativo (igual que 40)
n_anios <- n_distinct(d$year)
ok <- d %>% group_by(COD_DANE) %>%
  summarise(ok = n() == n_anios && !anyNA(Y), .groups = "drop") %>% filter(ok) %>% pull(COD_DANE)
d <- d %>% filter(COD_DANE %in% ok)
base_pre <- d %>% filter(year %in% PRE_COMUN) %>% group_by(COD_DANE) %>%
  summarise(mb = mean(Y), .groups = "drop")
d <- d %>% left_join(base_pre, by = "COD_DANE") %>% mutate(Yrel = Y / mb)
d <- d %>% group_by(COD_DANE) %>% filter(all(is.finite(Yrel))) %>% ungroup()

## Matrices municipio x anio
a_matriz <- function(v) {
  m <- tapply(d[[v]], list(d$COD_DANE, d$year), function(z) z[1])
  m[, order(as.integer(colnames(m))), drop = FALSE]
}
M <- list(relativo = a_matriz("Yrel"), hectareas = a_matriz("Y"))

unidades <- d %>% distinct(COD_DANE, G, dosis_bosque, NOMBRE_MPI)
ctrl  <- unidades$COD_DANE[unidades$G == 0]
tr    <- unidades %>% filter(G > 0) %>% arrange(G, dosis_bosque)
cohs  <- sort(unique(tr$G))
cat(sprintf("Controles: %d | Tratados con dosis en ventana: %d | Cohortes: %s\n",
            length(ctrl), nrow(tr), paste(cohs, collapse = ", ")))

## =============================================================================
## 2. Descomposicion en contribuciones por municipio
## =============================================================================
cols_de <- function(anios) as.character(anios)

cambio <- function(Mx, ids, g, base) {
  post <- cols_de(g:(g + E_MAX))
  pre  <- if (base == "g_menos_1") cols_de(g - 1) else cols_de((g - E_PRE):(g - 1))
  faltan <- setdiff(c(post, pre), colnames(Mx))
  if (length(faltan)) stop("Anios fuera del panel para la cohorte ", g, ": ", paste(faltan, collapse = ", "))
  rowMeans(Mx[ids, post, drop = FALSE]) - rowMeans(Mx[ids, pre, drop = FALSE])
}

descomponer <- function(outcome, base) {
  Mx <- M[[outcome]]
  ## Cambio de cada control para cada cohorte: matriz n_ctrl x n_cohortes.
  ## Se guarda completa para que el bootstrap remuestree controles sin recalcular.
  cc <- sapply(cohs, function(g) cambio(Mx, ctrl, g, base))
  colnames(cc) <- as.character(cohs)
  tchg <- vapply(seq_len(nrow(tr)), function(k)
    cambio(Mx, tr$COD_DANE[k], tr$G[k], base), numeric(1))
  att <- tchg - colMeans(cc)[as.character(tr$G)]
  list(cc = cc, tchg = tchg, att = att)
}

## =============================================================================
## 3. Regresion, HC3, bootstrap y permutacion
## =============================================================================
ols_hc3 <- function(y, X) {
  XtXi <- solve(crossprod(X))
  b <- drop(XtXi %*% crossprod(X, y))
  u <- drop(y - X %*% b)
  h <- rowSums((X %*% XtXi) * X)
  if (any(h > 1 - 1e-8)) return(list(b = b, se = rep(NA_real_, length(b))))
  V <- XtXi %*% crossprod(X * (u / (1 - h))) %*% XtXi
  list(b = b, se = sqrt(diag(V)))
}

disenio <- function(dosis, coh, spec) {
  if (spec == "agrupada") cbind(`(Intercepto)` = 1, dosis = dosis)
  else cbind(model.matrix(~ factor(coh) - 1), dosis = dosis)
}

pendiente <- function(y, dosis, coh, spec) {
  X <- disenio(dosis, coh, spec)
  if (qr(X)$rank < ncol(X)) return(NA_real_)
  b <- tryCatch(drop(solve(crossprod(X), crossprod(X, y))), error = function(e) NULL)
  if (is.null(b)) NA_real_ else b[["dosis"]]
}

estimar <- function(des, idx, spec) {
  y <- des$att[idx]; dz <- tr$dosis_bosque[idx]; g <- tr$G[idx]
  ## Con efectos fijos, las cohortes de un solo municipio no aportan a la
  ## pendiente (quedan absorbidas) y ademas tienen apalancamiento 1: se sacan.
  if (spec == "ef_cohorte") {
    keep <- ave(seq_along(g), g, FUN = length) >= 2
    y <- y[keep]; dz <- dz[keep]; g <- g[keep]; idx <- idx[keep]
  }
  n <- length(y); X <- disenio(dz, g, spec); k <- ncol(X)
  f <- ols_hc3(y, X)
  b <- f$b[["dosis"]]; se <- f$se[length(f$se)]
  tcrit <- qt(0.975, df = max(n - k, 1))

  ## Bootstrap estratificado: tratados dentro de cohorte + controles.
  set.seed(SEMILLA)
  grupos <- split(seq_along(idx), g)
  nctrl <- nrow(des$cc)
  bb <- vapply(seq_len(B_BOOT), function(r) {
    pos <- unlist(lapply(grupos, function(v) v[sample.int(length(v), length(v), replace = TRUE)]),
                  use.names = FALSE)
    ic  <- sample.int(nctrl, nctrl, replace = TRUE)
    mu  <- colMeans(des$cc[ic, , drop = FALSE])
    ii  <- idx[pos]
    yb  <- des$tchg[ii] - mu[as.character(tr$G[ii])]
    pendiente(yb, tr$dosis_bosque[ii], tr$G[ii], spec)
  }, numeric(1))
  fallidas <- sum(is.na(bb))

  ## Permutacion de la dosis: global en la agrupada, dentro de cohorte con EF.
  set.seed(SEMILLA + 1)
  bp <- vapply(seq_len(B_PERM), function(r) {
    dz_p <- if (spec == "agrupada") sample(dz) else ave(dz, g, FUN = function(v) v[sample.int(length(v))])
    pendiente(y, dz_p, g, spec)
  }, numeric(1))

  data.frame(
    n = n, pendiente = b,
    ee_hc3 = se, ic_hc3_inf = b - tcrit * se, ic_hc3_sup = b + tcrit * se,
    ee_boot = sd(bb, na.rm = TRUE),
    ic_boot_inf = unname(quantile(bb, 0.025, na.rm = TRUE)),
    ic_boot_sup = unname(quantile(bb, 0.975, na.rm = TRUE)),
    boot_fallidas = fallidas,
    p_permutacion = mean(abs(bp) >= abs(b), na.rm = TRUE),
    media_att_i = mean(y)
  )
}

## =============================================================================
## 4. Muestras y validacion contra el puente de 40
## =============================================================================
var_coh <- tr %>% group_by(G) %>% summarise(k = n_distinct(dosis_bosque), .groups = "drop")
coh_ok  <- var_coh$G[var_coh$k >= MIN_DOSIS_DISTINTAS]
muestras <- list(
  contdid_21  = which(tr$G %in% coh_ok),
  todos_25    = seq_len(nrow(tr)),
  sin_censura = which(tr$dosis_bosque < 0.9999)
)
for (nm in names(muestras)) cat(sprintf("  muestra %-12s %2d tratados\n", nm, length(muestras[[nm]])))

titulo("VALIDACION: promedio de ATT_i vs ATT balanceado del puente binario (40)")
ruta40 <- file.path(RAIZ, "outputs/40_resultados_depurada_y_dosis.rds")
ref <- list(hectareas = NA_real_, relativo = NA_real_)
if (file.exists(ruta40)) {
  r40 <- readRDS(ruta40)$binario_y_terciles
  if (!is.null(r40$C_puente_binario$balanceado))  ref$hectareas <- r40$C_puente_binario$balanceado$overall.att
  if (!is.null(r40$C_puente_relativo$balanceado)) ref$relativo  <- r40$C_puente_relativo$balanceado$overall.att
}
desc <- list()
for (oc in c("relativo", "hectareas")) for (bs in c("g_menos_1", "promedio_pre5")) {
  desc[[paste(oc, bs)]] <- descomponer(oc, bs)
}
for (oc in c("relativo", "hectareas")) {
  m <- mean(desc[[paste(oc, "g_menos_1")]]$att[muestras$contdid_21])
  cat(sprintf("  %-10s promedio ATT_i (21, base g-1): %12.4f | puente did: %12.4f | %s\n",
              oc, m, ref[[oc]],
              if (is.na(ref[[oc]])) "sin referencia"
              else if (abs(m - ref[[oc]]) <= 1e-6 * max(1, abs(ref[[oc]]))) "COINCIDE"
              else "NO COINCIDE -> revisar antes de leer la pendiente"))
}

## =============================================================================
## 5. Grilla de estimaciones
## =============================================================================
titulo("PENDIENTE DOSIS-RESPUESTA (dosis = proporcion del bosque municipal con REDD+)")
res <- list()
for (oc in c("relativo", "hectareas")) for (bs in c("g_menos_1", "promedio_pre5"))
  for (nm in names(muestras)) for (sp in c("agrupada", "ef_cohorte")) {
    if (nm == "sin_censura" && sp == "ef_cohorte") next
    r <- estimar(desc[[paste(oc, bs)]], muestras[[nm]], sp)
    res[[length(res) + 1]] <- cbind(outcome = oc, base = bs, muestra = nm, especificacion = sp, r)
  }
tabla <- do.call(rbind, res)
num <- vapply(tabla, is.numeric, logical(1))
tab_print <- tabla; tab_print[num] <- lapply(tab_print[num], function(z) signif(z, 4))
print(tab_print, row.names = FALSE)
write_csv(tabla, file.path(DIR_TAB, "42_pendiente_dosis_manual.csv"))

cat("\nReferencia contdid (ACRT, muestra de 21): relativo 0,7644 | hectareas 2386,9\n")
cat("Su EE reportado (0,0009 y 2,9) omitia la incertidumbre de los tratados.\n")

## =============================================================================
## 6. Contribuciones por municipio y graficos
## =============================================================================
contrib <- tr %>%
  mutate(att_relativo  = desc[["relativo g_menos_1"]]$att,
         att_hectareas = desc[["hectareas g_menos_1"]]$att,
         en_contdid    = G %in% coh_ok)
write_csv(contrib, file.path(DIR_TAB, "42_contribuciones_por_municipio.csv"))

graficar <- function(var, etiqueta_y, archivo) {
  p <- ggplot(contrib, aes(dosis_bosque, .data[[var]])) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "grey30", linewidth = 0.6) +
    geom_point(aes(color = factor(G), shape = en_contdid), size = 2.6) +
    geom_text(aes(label = NOMBRE_MPI), size = 2.4, vjust = -0.9, color = "grey25") +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                       labels = c(`TRUE` = "en la muestra de contdid", `FALSE` = "cohorte excluida de contdid")) +
    labs(title = "Contribucion de cada municipio al ATT balanceado frente a su dosis",
         subtitle = "e = 0..5, base g-1, contra nunca tratados. Linea: MCO agrupado sobre los 25.",
         x = "Dosis: proporcion del bosque municipal con REDD+ (truncada en 1)",
         y = etiqueta_y, color = "Cohorte", shape = NULL) +
    theme_minimal()
  ggsave(file.path(DIR_FIG, archivo), p, width = 10, height = 6.5, dpi = 150)
}
graficar("att_relativo",  "ATT_i relativo a la deforestacion base 2001-2012", "42_dosis_vs_att_relativo.png")
graficar("att_hectareas", "ATT_i (hectareas)",                                 "42_dosis_vs_att_hectareas.png")

cat("\nGuardado: outputs/tablas/42_pendiente_dosis_manual.csv\n")
cat("Guardado: outputs/tablas/42_contribuciones_por_municipio.csv\n")
cat("Guardado: outputs/figuras/42_dosis_vs_att_relativo.png y _hectareas.png\n")
