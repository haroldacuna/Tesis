## =============================================================================
## 41_diagnostico_contdid.R
##
## cont_did() falla sobre nuestros datos con "valor ausente donde TRUE/FALSE
## es necesario": un if() interno del paquete recibe un NA. En lugar de
## adivinar la causa, este script:
##
##   0. Corre el ejemplo simulado del propio paquete. Si falla, el problema es
##      de instalacion, no de nuestros datos. Si corre, muestra COMO codifica
##      el paquete G y D para los nunca tratados, para compararlo con lo nuestro.
##   1. Arma la muestra de contdid con SOLO las cinco columnas que necesita
##      (id, anio, outcome, D, G), sin NA y con tipos simples. Si el fallo venia
##      de columnas ajenas con NA, esto lo elimina.
##   2. Verifica los supuestos de formato: panel balanceado, D constante por
##      municipio, D = 0 si y solo si G = 0.
##   3. Corre cont_did() capturando la PILA DE LLAMADAS en el momento del error,
##      para saber en que funcion interna ocurre.
##
## Usa biters bajo: aqui no importan los errores estandar, solo que corra.
##
## USO:  Rscript scripts/41_diagnostico_contdid.R
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(contdid)
})

RAIZ <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "data/.raiz_canonica"))) {
    p <- dirname(d); if (identical(p, d)) stop("Sin data/.raiz_canonica"); d <- p
  }
  d
})
OUTCOME <- "loss_area_ha"
VENTANA <- 2013:2019
pad5    <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na  <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
linea   <- function(t) cat("\n", strrep("=", 70), "\n", t, "\n", strrep("=", 70), "\n", sep = "")

cat("R", R.version$major, ".", R.version$minor, " | contdid ",
    as.character(packageVersion("contdid")), " | ptetools ",
    as.character(packageVersion("ptetools")), "\n", sep = "")

## -----------------------------------------------------------------------------
## Ejecuta cont_did() y, si falla, imprime la pila de llamadas del error.
## -----------------------------------------------------------------------------
probar <- function(etiqueta, ...) {
  cat("\n--- ", etiqueta, " ---\n", sep = "")
  pila <- NULL
  r <- tryCatch(
    withCallingHandlers(contdid::cont_did(...),
                        error = function(e) pila <<- sys.calls()),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
  if (!is.null(pila)) {
    txt <- vapply(pila, function(x) paste(deparse(x, nlines = 1), collapse = ""), "")
    txt <- txt[!grepl("^(tryCatch|withCallingHandlers|doTryCatch|tryCatchList|tryCatchOne|probar)", txt)]
    cat("Pila de llamadas (de la mas externa a la mas interna):\n")
    cat(paste0("  ", seq_along(txt), ". ", substr(txt, 1, 110)), sep = "\n")
  } else if (!is.null(r)) {
    cat("OK. Resumen:\n"); print(summary(r))
  }
  invisible(r)
}

## =============================================================================
## 0. Ejemplo del paquete
## =============================================================================
linea("0. EJEMPLO SIMULADO DEL PAQUETE")
set.seed(1)
sim <- tryCatch(simulate_contdid_data(), error = function(e) {
  cat("simulate_contdid_data() fallo:", conditionMessage(e), "\n"); NULL })
if (!is.null(sim)) {
  cat("Columnas:", paste(names(sim), collapse = ", "), "\n")
  gcol <- intersect(c("G", "g", "group"), names(sim))[1]
  dcol <- intersect(c("D", "d", "dose"), names(sim))[1]
  if (!is.na(gcol)) {
    cat("Valores de G:\n"); print(table(sim[[gcol]]))
    g0 <- sort(unique(sim[[gcol]]))[1]
    cat("Codificacion de nunca tratados: G =", g0, "\n")
    if (!is.na(dcol)) {
      cat("D entre nunca tratados:\n"); print(summary(sim[[dcol]][sim[[gcol]] == g0]))
      cat("D entre tratados:\n");       print(summary(sim[[dcol]][sim[[gcol]] != g0]))
    }
  }
  tcol <- intersect(c("time_period", "period", "t", "year"), names(sim))[1]
  icol <- intersect(c("id", "ID"), names(sim))[1]
  ycol <- intersect(c("Y", "y"), names(sim))[1]
  if (!any(is.na(c(gcol, dcol, tcol, icol, ycol)))) {
    probar("ejemplo del paquete, level/dose",
           yname = ycol, tname = tcol, idname = icol, dname = dcol, gname = gcol,
           data = sim, target_parameter = "level", aggregation = "dose",
           treatment_type = "continuous", control_group = "nevertreated",
           num_knots = 0, degree = 1, biters = 50, cband = FALSE)
  }
}

## =============================================================================
## 1. Nuestra muestra, minima
## =============================================================================
linea("1. MUESTRA DE CONTDID, SOLO CINCO COLUMNAS")
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
  mutate(COD_DANE = pad5(COD_DANE)) %>% select(COD_DANE, tiene_dosis, dosis_bosque)

d <- panel %>%
  select(COD_DANE, id_num, year, all_of(OUTCOME)) %>%
  inner_join(trat, by = "COD_DANE") %>%
  left_join(dosis, by = "COD_DANE") %>%
  filter(muestra == 1, G == 0 | (G %in% VENTANA & tiene_dosis %in% 1)) %>%
  mutate(D = if_else(G == 0, 0, dosis_bosque))

## Cohortes sin variacion de dosis (mismo criterio que 40)
malas <- d %>% filter(G > 0) %>% distinct(id_num, G, D) %>% group_by(G) %>%
  summarise(k = n_distinct(D), .groups = "drop") %>% filter(k < 3) %>% pull(G)
d <- d %>% filter(!G %in% malas)

## Municipios con outcome incompleto: fuera
comp <- d %>% group_by(id_num) %>% summarise(ok = all(!is.na(.data[[OUTCOME]])), .groups = "drop")
d <- d %>% filter(id_num %in% comp$id_num[comp$ok])

dmin <- data.frame(
  id   = as.integer(d$id_num),
  year = as.integer(d$year),
  Y    = as.numeric(d[[OUTCOME]]),
  D    = as.numeric(d$D),
  G    = as.integer(d$G)
) %>% arrange(id, year)

## =============================================================================
## 2. Chequeos de formato
## =============================================================================
linea("2. CHEQUEOS DE FORMATO")
chk <- function(txt, ok) cat(sprintf("  [%s] %s\n", if (isTRUE(ok)) "ok" else "FALLA", txt))
n_t <- n_distinct(dmin$year)
chk("sin NA en id, year, Y, D, G",       !anyNA(dmin))
chk(sprintf("panel balanceado (%d anios por municipio)", n_t),
    all(table(dmin$id) == n_t))
chk("D constante por municipio",         all(tapply(dmin$D, dmin$id, function(x) length(unique(x))) == 1))
chk("D = 0 si y solo si G = 0",          all((dmin$D == 0) == (dmin$G == 0)))
chk("D > 0 en todos los tratados",       all(dmin$D[dmin$G > 0] > 0))
cat(sprintf("  municipios: %d | tratados: %d | cohortes: %s\n",
            n_distinct(dmin$id), n_distinct(dmin$id[dmin$G > 0]),
            paste(sort(unique(dmin$G[dmin$G > 0])), collapse = ", ")))
cat("  rango de D en tratados:", paste(round(range(dmin$D[dmin$G > 0]), 4), collapse = " - "), "\n")

## =============================================================================
## 3. Corridas con pila de llamadas
## =============================================================================
linea("3. CONT_DID SOBRE NUESTRA MUESTRA")
base <- list(yname = "Y", tname = "year", idname = "id", dname = "D", gname = "G",
             data = dmin, target_parameter = "level", treatment_type = "continuous",
             num_knots = 0, degree = 1, biters = 50, cband = FALSE)

do.call(probar, c(list("nevertreated, dose"),      base,
                  list(aggregation = "dose",       control_group = "nevertreated")))
do.call(probar, c(list("notyettreated, dose"),     base,
                  list(aggregation = "dose",       control_group = "notyettreated")))
do.call(probar, c(list("nevertreated, eventstudy"), base,
                  list(aggregation = "eventstudy", control_group = "nevertreated")))

## Variante: anios recodificados a 1..T. Algunos paquetes asumen periodos
## consecutivos que empiezan en 1; si esta corre y las anteriores no, es eso.
dmin_t <- dmin %>% mutate(year = year - min(year) + 1L,
                          G = if_else(G == 0L, 0L, G - min(dmin$year) + 1L))
base_t <- modifyList(base, list(data = dmin_t))
do.call(probar, c(list("periodos recodificados 1..T, nevertreated, dose"), base_t,
                  list(aggregation = "dose", control_group = "nevertreated")))

cat("\nFin del diagnostico. Pega toda la salida.\n")
