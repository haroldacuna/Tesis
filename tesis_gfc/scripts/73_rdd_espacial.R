## =============================================================================
## 73_rdd_espacial.R
##
## RDD espacial en el borde de los poligonos REDD+ (tarea 0.2 del plan,
## comentario de Jorge p. 13: "aqui tienen los elementos para correr un RDD").
##
## DISENO (fijado antes de estimar, 20/10/2026)
##   Unidad: celda de 2 x 2 km del 55, a <= 20 km del borde de un proyecto AD de
##     las cohortes del 56 (2013, 2014, 2015, 2017, 2018). Variable de asignacion
##     d = distancia con signo al borde (km; > 0 dentro), del 72.
##   Parametro: salto en d = 0 de la perdida de bosque natural (def_p1, ha por
##     celda y anio), con la cohorte g del proyecto del borde mas cercano:
##       tau = lim_{d->0+} E[Y | d] - lim_{d->0-} E[Y | d]
##   Resultados
##     (a) post:    media de def_p1 en g..2023 (efecto en niveles)
##     (b) cambio:  post - media en g-5..g-1 (RDD en diferencias: absorbe
##                  discontinuidades fijas del borde, como rios o resguardos
##                  que coinciden con el limite del proyecto)  <- PRINCIPAL
##     (c) placebo: media en g-5..g-1 (antes del proyecto; debe ser ~0)
##   Estimador: rdrobust, polinomio local lineal, kernel triangular, ventana
##     MSE-optima comun (mserd), inferencia robusta con correccion de sesgo
##     (Calonico, Cattaneo y Titiunik 2014), errores agrupados por proyecto.
##   Sensibilidad: con covariables (elevacion, pendiente, % bosque 2000);
##     donas de 1 y 2 km (sacan las celdas cortadas por el borde, donde el lado
##     asignado por el centroide es impreciso).
##   Validez: continuidad de covariables en el borde y prueba de densidad
##     (rddensity, si esta instalado).
##   Lectura: tau < 0 con IC robusto que excluye el cero = menos perdida dentro.
##     Una dona o un placebo que cambian la conclusion se reportan, no se
##     esconden. Limite: el RDD mide el efecto EN EL BORDE; si hay fuga hacia el
##     lado de afuera, tau sobrestima la reduccion (el control tambien cambia).
##
## Insumos:  data/interim/celdas/rdd_celdas.csv (72), panel_celdas_nocero.csv.gz (55)
## Salidas:  outputs/tablas/73_rdd_espacial.csv, 73_rdd_balance.csv
##           outputs/figuras/73_rdd_cambio.png
## USO (desde tesis_gfc/):  Rscript scripts/73_rdd_espacial.R
## =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
for (pk in c("rdrobust")) {
  if (!requireNamespace(pk, quietly = TRUE)) {
    cat("Instalando", pk, "...\n")
    install.packages(pk, repos = "https://cloud.r-project.org", type = "binary")
  }
}
library(rdrobust)
TIENE_DENSIDAD <- requireNamespace("rddensity", quietly = TRUE)
if (!TIENE_DENSIDAD) cat("Aviso: rddensity no esta instalado; se omite la prueba de densidad.",
                         "Instalalo con install.packages('rddensity', type = 'binary').\n")

source("scripts/00_config_did.R")
DIR_CEL  <- file.path(RAIZ_PROYECTO, "data/interim/celdas")
ANIO_FIN <- 2023          # ultimo anio con persistencia observada (def_p1, ver 55)
K_PRE    <- 5
DONAS    <- c(0, 1, 2)
COVS     <- c("elev", "pend", "pct_bosque2000")

## -----------------------------------------------------------------------------
## 1. Datos: resultados por celda (pre, post, cambio)
## -----------------------------------------------------------------------------
titulo("1. DATOS")
ruta_rdd <- file.path(DIR_CEL, "rdd_celdas.csv")
if (!file.exists(ruta_rdd)) stop("Falta ", ruta_rdd, ": corre primero python scripts/72_distancia_borde.py")
r <- fread(ruta_rdd)
pan <- fread(file.path(DIR_CEL, "panel_celdas_nocero.csv.gz"), select = c("celda", "anio", "def_p1"))
pan <- pan[celda %in% r$celda & anio <= ANIO_FIN]

## El panel del 55 solo trae celda-anio con algun valor > 0: se completan ceros
g <- CJ(celda = r$celda, anio = 2001:ANIO_FIN)
pan <- pan[g, on = .(celda, anio)][is.na(def_p1), def_p1 := 0]
pan <- merge(pan, r[, .(celda, cohorte)], by = "celda")
res <- pan[, .(post = mean(def_p1[anio >= cohorte]),
               pre  = mean(def_p1[anio >= cohorte - K_PRE & anio <= cohorte - 1])), by = celda]
r <- merge(r, res, by = "celda")
r[, cambio := post - pre]
r[, cl := as.integer(factor(proyecto))]
cat(sprintf("Celdas: %d (dentro %d, fuera %d) | proyectos: %d\n",
            nrow(r), sum(r$dentro == 1), sum(r$dentro == 0), uniqueN(r$proyecto)))
print(r[, .(celdas = .N, post = mean(post), pre = mean(pre), cambio = mean(cambio)), by = .(dentro)][order(dentro)])

## -----------------------------------------------------------------------------
## 2. Estimacion
## -----------------------------------------------------------------------------
estimar <- function(y, dona = 0, covs = FALSE, etiqueta = y) {
  d <- r[abs(dist_borde_km) >= dona]
  z <- if (covs) as.matrix(d[, ..COVS]) else NULL
  e <- tryCatch(rdrobust(y = d[[y]], x = d$dist_borde_km, c = 0, p = 1, kernel = "triangular",
                         bwselect = "mserd", cluster = d$cl, covs = z),
                error = function(err) { cat("  ERROR rdrobust [", etiqueta, "]:", conditionMessage(err), "\n"); NULL })
  if (is.null(e)) return(NULL)
  ## media del lado de afuera dentro de la ventana: escala del efecto
  h <- e$bws[1, 1]
  m_fuera <- mean(d[[y]][d$dist_borde_km < 0 & d$dist_borde_km >= -h])
  out <- data.table(resultado = y, dona_km = dona, covariables = covs,
                    tau = e$coef[1], ee_convencional = e$se[1], ee_robusto = e$se[3],
                    ic_rob_inf = e$ci[3, 1], ic_rob_sup = e$ci[3, 2], p_robusto = e$pv[3],
                    h_km = h, n_fuera = e$N_h[1], n_dentro = e$N_h[2],
                    proyectos = uniqueN(d[abs(dist_borde_km) <= h, proyecto]),
                    media_fuera = m_fuera)
  cat(sprintf("  %-8s dona %d km %-9s tau %7.3f | IC robusto [%6.3f; %6.3f] p = %.3f | h = %.1f km | N %d/%d\n",
              y, dona, if (covs) "con cov" else "sin cov", out$tau, out$ic_rob_inf, out$ic_rob_sup,
              out$p_robusto, h, out$n_fuera, out$n_dentro))
  out
}

titulo("2. SALTO EN EL BORDE (rdrobust, ventana MSE-optima, EE por proyecto)")
tab <- rbindlist(c(
  lapply(DONAS, function(k) estimar("cambio", k)),
  list(estimar("cambio", 0, covs = TRUE)),
  lapply(DONAS, function(k) estimar("post", k)),
  lapply(DONAS, function(k) estimar("pre", k))
))
fwrite(tab, file.path(DIR_TAB, "73_rdd_espacial.csv"))

## -----------------------------------------------------------------------------
## 3. Validez: covariables y densidad
## -----------------------------------------------------------------------------
titulo("3. CONTINUIDAD DE COVARIABLES EN EL BORDE")
bal <- rbindlist(lapply(COVS, function(v) estimar(v, 0)))
fwrite(bal, file.path(DIR_TAB, "73_rdd_balance.csv"))
if (TIENE_DENSIDAD) {
  dn <- rddensity::rddensity(X = r$dist_borde_km, c = 0)
  cat(sprintf("Densidad (Cattaneo, Jansson y Ma): p = %.3f\n", dn$test$p_jk))
  cat("  Nota: con una grilla regular la densidad de celdas es mecanica; un rechazo indica",
      "poligonos con forma irregular, no manipulacion.\n")
}

## -----------------------------------------------------------------------------
## 4. Figura: cambio en la perdida contra la distancia al borde
## -----------------------------------------------------------------------------
bins <- r[abs(dist_borde_km) <= 10, .(cambio = mean(cambio), n = .N),
          by = .(x = (floor(dist_borde_km / 0.5) + 0.5) * 0.5)]
p <- ggplot(bins, aes(x, cambio)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.3) +
  geom_point(aes(size = n, colour = x > 0), alpha = 0.85) +
  geom_smooth(data = r[abs(dist_borde_km) <= 10 & dist_borde_km < 0], aes(dist_borde_km, cambio),
              method = "lm", formula = y ~ x, colour = "#B4532A", fill = "#B4532A", alpha = 0.15) +
  geom_smooth(data = r[abs(dist_borde_km) <= 10 & dist_borde_km > 0], aes(dist_borde_km, cambio),
              method = "lm", formula = y ~ x, colour = "#2E6B4F", fill = "#2E6B4F", alpha = 0.15) +
  scale_colour_manual(values = c(`FALSE` = "#B4532A", `TRUE` = "#2E6B4F"),
                      labels = c("Fuera del proyecto", "Dentro del proyecto"), name = NULL) +
  scale_size_continuous(range = c(0.8, 3), guide = "none") +
  labs(x = "Distancia al borde del pol\u00edgono (km; positiva dentro)",
       y = "Cambio en la p\u00e9rdida de bosque\n(ha por celda y a\u00f1o, despu\u00e9s \u2212 antes)") +
  theme_classic(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(DIR_FIG, "73_rdd_cambio.png"), p, width = 6.5, height = 4, dpi = 300)

titulo("RESUMEN")
print(tab[, .(resultado, dona_km, covariables, tau = round(tau, 3), ee = round(ee_robusto, 3),
              ic = sprintf("[%.3f; %.3f]", ic_rob_inf, ic_rob_sup), p = round(p_robusto, 3),
              h_km = round(h_km, 1), n_fuera, n_dentro, proyectos)])
cat("\nGuardado: outputs/tablas/73_rdd_espacial.csv, 73_rdd_balance.csv | figuras/73_rdd_cambio.png\n")
