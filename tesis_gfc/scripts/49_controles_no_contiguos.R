## =============================================================================
## 49_controles_no_contiguos.R
##
## Robustez de la especificacion principal a DERRAMES hacia los controles.
##
## Motivacion (48_soporte_comun.R): el tamanio muestral efectivo de los
## controles es ~37 de 987, y los que concentran el peso son municipios del
## Choco y del Pacifico caucano (El Litoral del San Juan 11 %, Medio Atrato,
## Bojaya, Tado, Medio San Juan, Timbiqui...), muchos vecinos de tratados.
##   - Si REDD+ desplaza deforestacion al vecino (fuga), esos controles tienen
##     MAS deforestacion por el tratamiento y el ATT EXAGERA la reduccion.
##   - Si un territorio colectivo con proyecto se extiende sobre un municipio
##     control, ese control esta parcialmente tratado y el ATT se ATENUA.
## La regla de controles limpios no captura ninguno de los dos casos.
##
## Diseno FIJADO ANTES DE CORRER (27/09/2026):
##   Municipio "expuesto" = contiguo (tipo reina, misma construccion que el 45)
##   a algun municipio con proyecto REDD+ en la D1 depurada (cualquier cohorte)
##   o a uno de los municipios con proyecto fallido (muestra_redd_depurada = 0).
##     C1: se excluyen de los controles los expuestos de 1er orden  (principal)
##     C2: se excluyen los de 1er y 2do orden                        (sensibilidad)
##   Estimadores: dr (principal), reg, y sin covariables.
##   REGLA DE LECTURA: el resultado NO depende de derrames hacia los controles
##   si dr en C1 tiene el mismo signo que la principal y esta a < 1 EE de ella.
##   Direccion (se reporta, no decide): |ATT| cae hacia 0 en C1 -> compatible
##   con fuga que inflaba la reduccion; |ATT| crece -> compatible con controles
##   parcialmente tratados que atenuaban el efecto.
##
## DIAGNOSTICO DE COMPOSICION (agregado el 27/09/2026, regla fijada antes de
## correrlo): en C1 el DR deja pares (g,t) sin estimar, asi que su ATT no
## promedia sobre las mismas celdas que la principal. Sea S el conjunto de pares
## post-tratamiento que el DR SI estima en C1. Se reagrega el DR de la muestra
## completa solo sobre S, con los pesos por tamanio de cohorte de
## aggte(type = "simple"):
##     theta_completa|S = sum_{(g,t) en S} w_g ATT(g,t)
##   Si theta_completa|S queda mas cerca de theta_C1 que de theta_completa ->
##     el cambio de signo es COMPOSICION de celdas (lectura B).
##   Si queda mas cerca de theta_completa -> sacar a los vecinos cambia el
##     efecto DENTRO de las mismas celdas (lectura A, compatible con fuga).
##   Igual para C2. Validacion: el agregador propio sobre todas las celdas debe
##   reproducir los ATT simples de aggte().
##   EE: funcion de influencia sin el termino de estimacion de los pesos
##   (aproximacion; se reporta para orden de magnitud, la regla usa puntos).
##
## Especificacion base = la del 40 (D1 depurada, cohortes 2013-2019, controles
## limpios, notyettreated, covariables del 02 corregido, Hansen 2001-2024).
## La celda dr x completa debe reproducir -207.9 ha.
##
## USO
##   Rscript scripts/49_controles_no_contiguos.R
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(sf)
  library(spdep)
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
dir.create(DIR_TAB, showWarnings = FALSE, recursive = TRUE)
source(file.path(RAIZ_PROYECTO, "scripts/comun_controles_limpios.R"))
source(file.path(RAIZ_PROYECTO, "scripts/comun_covariables_pre.R"))

BAL_E   <- 5
PRINCIPAL_ESPERADA <- -207.9
X <- COVARIABLES_XFORMLA   # B.1: lista oficial de 00_config_did.R

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(t) cat("\n", strrep("=", 74), "\n", t, "\n", strrep("=", 74), "\n", sep = "")

## =============================================================================
## Funciones de vecindad (probadas por separado)
## =============================================================================

## Codigos a distancia <= `orden` (en saltos de contiguidad) de algun origen.
## No incluye a los propios origenes.
vecinos_de <- function(origen, codigos, vec, orden = 1) {
  idx_o <- which(codigos %in% origen)
  alcanzados <- idx_o
  frontera <- idx_o
  for (k in seq_len(orden)) {
    nuevos <- unique(unlist(vec[frontera]))
    nuevos <- nuevos[nuevos > 0 & !nuevos %in% alcanzados]
    alcanzados <- c(alcanzados, nuevos)
    frontera <- nuevos
    if (!length(frontera)) break
  }
  setdiff(codigos[alcanzados], origen)
}

## Propension municipal y ESS (misma definicion que el 48)
propension_municipal <- function(d, formula_x) {
  vars <- all.vars(formula_x)
  m <- d[!duplicated(d$COD_DANE), c("COD_DANE", "G_dep", vars)]
  m$tratado <- as.integer(m$G_dep > 0)
  m <- m[stats::complete.cases(m[, vars]), ]
  fit <- suppressWarnings(stats::glm(stats::update(formula_x, tratado ~ .), data = m,
                                     family = stats::binomial()))
  m$p <- stats::fitted(fit)
  m
}
peso_att <- function(p) p / (1 - pmin(p, 1 - 1e-12))
ess <- function(w) if (!length(w) || sum(w) == 0) NA_real_ else sum(w)^2 / sum(w^2)

## Agregacion "simple" de un objeto att_gt sobre un subconjunto de celdas.
## Reproduce aggte(type = "simple"): promedio de ATT(g,t) post-tratamiento no NA
## ponderado por el tamanio de la cohorte. `celdas` = claves "g t" a conservar.
agregar_simple <- function(out, n_por_cohorte, celdas = NULL) {
  clave <- paste(out$group, out$t)
  post  <- out$t >= out$group & !is.na(out$att)
  if (!is.null(celdas)) post <- post & clave %in% celdas
  if (!any(post)) return(list(att = NA_real_, ee = NA_real_, n_celdas = 0L, celdas = character(0)))
  w <- as.numeric(n_por_cohorte[as.character(out$group[post])])
  if (any(is.na(w))) stop("Cohorte sin tamanio en n_por_cohorte")
  w <- w / sum(w)
  theta <- sum(w * out$att[post])
  inf <- as.numeric(as.matrix(out$inffunc[, post, drop = FALSE]) %*% w)
  list(att = theta, ee = sqrt(mean(inf^2) / length(inf)), n_celdas = sum(post),
       celdas = clave[post])
}

## =============================================================================
## Datos (igual que el 40)
## =============================================================================
mun <- st_read(file.path(RAIZ_PROYECTO, "data/interim/municipios_clean.gpkg"),
               layer = "municipios_clean", quiet = TRUE)
mun$COD_DANE <- pad5(mun$COD_DANE)
if (any(!st_is_valid(mun))) mun <- st_make_valid(mun)
vec <- poly2nb(mun, queen = TRUE)
cat(sprintf("Vecindad reina: %d municipios | vecinos promedio %.1f | sin vecinos %d\n",
            nrow(mun), mean(card(vec)), sum(card(vec) == 0)))

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

sin_geom <- setdiff(trat$COD_DANE, mun$COD_DANE)
if (length(sin_geom)) cat("AVISO: codigos sin geometria (no pueden evaluarse como vecinos):",
                          paste(sin_geom, collapse = ", "), "\n")

base <- panel %>% select(-any_of(c("G_dep", "muestra_dep"))) %>%
  inner_join(trat, by = "COD_DANE") %>%
  filter(muestra_dep == 1, G_dep == 0 | G_dep %in% VENTANA)
base <- aplicar_controles_limpios(as.data.frame(base), municipios_con_proyecto(RUTA_TRAT), "G_dep")

## Origenes de la exposicion: cualquier REDD+ de la D1 (cualquier cohorte) y
## los municipios con proyecto fallido
origen <- trat$COD_DANE[trat$G_dep > 0 | trat$muestra_dep == 0]
cat(sprintf("Origenes de exposicion: %d municipios (REDD+ en la D1 o proyecto fallido)\n",
            length(origen)))
expuestos <- list(C1 = vecinos_de(origen, mun$COD_DANE, vec, 1),
                  C2 = vecinos_de(origen, mun$COD_DANE, vec, 2))

## =============================================================================
## Diagnostico: cuanto peso tenian los controles expuestos
## =============================================================================
titulo("DIAGNOSTICO: PESO DE LOS CONTROLES CONTIGUOS")
ps <- propension_municipal(base, X)
ctrl <- ps[ps$tratado == 0, ]
ctrl$w <- peso_att(ctrl$p)
nombres <- panel %>% distinct(COD_DANE, .keep_all = TRUE) %>%
  select(any_of(c("COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR")))
cat(sprintf("Muestra completa: %d controles | ESS %.1f\n", nrow(ctrl), ess(ctrl$w)))
for (k in names(expuestos)) {
  exp_k <- ctrl$COD_DANE %in% expuestos[[k]]
  cat(sprintf("[%s] controles expuestos: %d (%.1f%%) | concentran %.1f%% del peso ATT | ESS restante %.1f\n",
              k, sum(exp_k), 100 * mean(exp_k), 100 * sum(ctrl$w[exp_k]) / sum(ctrl$w),
              ess(ctrl$w[!exp_k])))
}
top <- ctrl %>% arrange(desc(w)) %>% head(15) %>%
  mutate(peso_pct = 100 * w / sum(ctrl$w),
         contiguo_1er = COD_DANE %in% expuestos$C1,
         contiguo_2do = COD_DANE %in% expuestos$C2 & !contiguo_1er) %>%
  left_join(nombres, by = "COD_DANE")
cat("\nLos 15 controles de mayor peso y su exposicion:\n")
print(as.data.frame(top %>% select(any_of(c("COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR")),
                                   p, peso_pct, contiguo_1er, contiguo_2do)),
      row.names = FALSE, digits = 3)
write_csv(ctrl %>% mutate(contiguo_1er = COD_DANE %in% expuestos$C1,
                          contiguo_hasta_2do = COD_DANE %in% expuestos$C2) %>%
            left_join(nombres, by = "COD_DANE"),
          file.path(DIR_TAB, "49_controles_exposicion.csv"))

## =============================================================================
## Estimacion
## =============================================================================
correr <- function(d, metodo, xformla, etq) {
  cat("\n--- ", etq, " ---\n", sep = "")
  d <- as.data.frame(d); d$.gname <- d$G_dep
  set.seed(SEMILLA)
  out <- suppressWarnings(tryCatch(
    att_gt(yname = OUTCOME, tname = "year", idname = "id_num", gname = ".gname",
           xformla = xformla, data = d, control_group = "notyettreated",
           est_method = metodo, bstrap = TRUE, biters = BITERS, cband = TRUE,
           clustervars = "id_num"),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }))
  if (is.null(out)) return(NULL)
  s   <- aggte(out, type = "simple", na.rm = TRUE)
  bal <- tryCatch(aggte(out, type = "dynamic", balance_e = BAL_E, min_e = 0, max_e = BAL_E,
                        na.rm = TRUE), error = function(e) NULL)
  cat(sprintf("ATT %.1f (EE %.1f) IC95 [%.1f, %.1f] | g-t NA %d/%d\n",
              s$overall.att, s$overall.se, s$overall.att - 1.96 * s$overall.se,
              s$overall.att + 1.96 * s$overall.se, sum(is.na(out$att)), length(out$att)))
  fila <- data.frame(att = s$overall.att, ee = s$overall.se,
             ic_inf = s$overall.att - 1.96 * s$overall.se,
             ic_sup = s$overall.att + 1.96 * s$overall.se,
             att_balanceado = if (is.null(bal)) NA_real_ else bal$overall.att,
             gt_na = sum(is.na(out$att)), gt_total = length(out$att),
             n_tratados = n_distinct(d$COD_DANE[d$G_dep > 0]),
             n_controles = n_distinct(d$COD_DANE[d$G_dep == 0]), row.names = NULL)
  n_coh <- tapply(d$COD_DANE[d$G_dep > 0], d$G_dep[d$G_dep > 0], function(x) length(unique(x)))
  list(fila = fila, out = out, n_coh = n_coh)
}

titulo("ESTIMACION: CONTROLES CON Y SIN VECINOS DE TRATADOS")
muestras <- list(completa = base)
for (k in names(expuestos)) {
  excl <- setdiff(expuestos[[k]], base$COD_DANE[base$G_dep > 0])   # solo controles salen
  muestras[[k]] <- base[!(base$COD_DANE %in% excl & base$G_dep == 0), ]
}
filas <- list(); objetos <- list()
for (k in names(muestras)) {
  specs <- if (k == "completa") list(dr = X, reg = X) else list(dr = X, reg = X, sincov = ~1)
  for (s in names(specs)) {
    metodo <- if (s == "sincov") "dr" else s
    r <- correr(muestras[[k]], metodo, specs[[s]], sprintf("%s | %s", k, s))
    if (!is.null(r)) {
      filas[[paste(k, s)]] <- cbind(muestra = k, especificacion = s, r$fila)
      objetos[[paste(k, s)]] <- r
    }
  }
}
tabla <- do.call(rbind, filas)
write_csv(tabla, file.path(DIR_TAB, "49_controles_no_contiguos.csv"))

titulo("RESUMEN Y REGLA DE LECTURA")
print(tabla[, c("muestra", "especificacion", "n_tratados", "n_controles", "att", "ee",
                "ic_inf", "ic_sup", "gt_na")], row.names = FALSE, digits = 3)
ppal <- tabla[tabla$muestra == "completa" & tabla$especificacion == "dr", ]
c1   <- tabla[tabla$muestra == "C1" & tabla$especificacion == "dr", ]
if (nrow(ppal) && nrow(c1)) {
  cat(sprintf("\nControl de consistencia: DR completa = %.1f (esperado %.1f) -> %s\n", ppal$att,
              PRINCIPAL_ESPERADA, if (abs(ppal$att - PRINCIPAL_ESPERADA) < 1) "OK" else "NO COINCIDE"))
  ok <- sign(c1$att) == sign(ppal$att) && abs(c1$att - ppal$att) < ppal$ee
  cat(sprintf("Regla: DR en C1 = %.1f vs principal %.1f (1 EE = %.1f) -> %s\n", c1$att, ppal$att,
              ppal$ee, if (ok) "el resultado NO depende de derrames hacia los controles"
                       else "el resultado SI es sensible a los controles contiguos"))
  cat(sprintf("Direccion (informativa): |ATT| %s de %.1f a %.1f -> %s\n",
              if (abs(c1$att) < abs(ppal$att)) "baja" else "sube", abs(ppal$att), abs(c1$att),
              if (abs(c1$att) < abs(ppal$att)) "compatible con fuga que inflaba la reduccion"
              else "compatible con controles parcialmente tratados que atenuaban el efecto"))
}
## =============================================================================
## Diagnostico de composicion de celdas
## =============================================================================
titulo("DIAGNOSTICO DE COMPOSICION DE CELDAS (regla fijada antes de correr)")
comp <- list()
o_full <- objetos[["completa dr"]]
for (k in c("C1", "C2")) {
  o_k <- objetos[[paste(k, "dr")]]
  if (is.null(o_full) || is.null(o_k)) next
  ## Validacion del agregador
  a_full <- agregar_simple(o_full$out, o_full$n_coh)
  a_k    <- agregar_simple(o_k$out, o_k$n_coh)
  f_full <- tabla$att[tabla$muestra == "completa" & tabla$especificacion == "dr"]
  f_k    <- tabla$att[tabla$muestra == k & tabla$especificacion == "dr"]
  cat(sprintf("\n[%s] validacion del agregador: completa %.1f (aggte %.1f) | %s %.1f (aggte %.1f) -> %s\n",
              k, a_full$att, f_full, k, a_k$att, f_k,
              if (abs(a_full$att - f_full) < 0.5 && abs(a_k$att - f_k) < 0.5) "OK"
              else "NO REPRODUCE: no interpretar la regla"))
  ## Celdas perdidas por cohorte
  post <- o_k$out$t >= o_k$out$group
  perdidas <- table(o_k$out$group[post & is.na(o_k$out$att)])
  if (length(perdidas)) {
    cat("  pares post-tratamiento sin estimar en", k, "por cohorte:",
        paste(sprintf("%s: %d", names(perdidas), as.integer(perdidas)), collapse = " | "), "\n")
  }
  ## Reagregacion sobre S
  a_full_S <- agregar_simple(o_full$out, o_full$n_coh, celdas = a_k$celdas)
  o_reg <- objetos[["completa reg"]]
  a_reg_S <- if (!is.null(o_reg)) agregar_simple(o_reg$out, o_reg$n_coh, celdas = a_k$celdas) else NULL
  cat(sprintf("  celdas en S: %d de %d post-tratamiento\n", a_k$n_celdas, sum(!is.na(o_full$out$att) & o_full$out$t >= o_full$out$group)))
  cat(sprintf("  DR completa, todas las celdas : %8.1f\n", a_full$att))
  cat(sprintf("  DR completa, solo celdas S    : %8.1f (EE aprox. %.1f)\n", a_full_S$att, a_full_S$ee))
  if (!is.null(a_reg_S)) cat(sprintf("  REG completa, solo celdas S   : %8.1f\n", a_reg_S$att))
  cat(sprintf("  DR %s                          : %8.1f\n", k, a_k$att))
  lectura <- if (abs(a_full_S$att - a_k$att) < abs(a_full_S$att - a_full$att))
    "B: el cambio de signo es COMPOSICION de celdas" else
    "A: el efecto cambia DENTRO de las mismas celdas (compatible con fuga)"
  cat("  >>> Lectura", k, ":", lectura, "\n")
  comp[[k]] <- data.frame(muestra = k, dr_completa = a_full$att, dr_completa_S = a_full_S$att,
                          ee_aprox_completa_S = a_full_S$ee,
                          reg_completa_S = if (is.null(a_reg_S)) NA_real_ else a_reg_S$att,
                          dr_muestra = a_k$att, celdas_S = a_k$n_celdas, lectura = lectura)
}
if (length(comp)) write_csv(do.call(rbind, comp), file.path(DIR_TAB, "49_composicion_celdas.csv"))
saveRDS(lapply(objetos, function(o) o$out), file.path(DIR_OUT, "49_att_gt_objetos.rds"))

cat("\nGuardado: outputs/tablas/49_controles_no_contiguos.csv, 49_controles_exposicion.csv\n")
cat("Guardado: outputs/tablas/49_composicion_celdas.csv, outputs/49_att_gt_objetos.rds\n")
