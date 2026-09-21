## =============================================================================
## 40_did_depurada_y_dosis.R
##
## Tres estimaciones sobre la D1 DEPURADA (sin los 6 municipios cuyo unico
## vinculo REDD+ es un proyecto retirado o rechazado; ver la seccion
## "D1 DEPURADA" en 26_consolidar_fuentes_carbono.py):
##
##   A. ATT binario (Callaway-Sant'Anna), misma especificacion que 03:
##      doblemente robusto, notyettreated, bootstrap con banda simultanea.
##      Se compara contra la D1 ORIGINAL en la misma ventana de cohortes.
##
##   B. ATT por TERCIL de dosis, con el estudio de eventos agregado en
##      ventana BALANCEADA (balance_e). Sin esto, comparar terciles mezcla
##      dosis con tiempo de exposicion: el tercil bajo concentra cohortes
##      2013-2014 y el medio 2017-2018.
##
##   C. contdid (Callaway, Goodman-Bacon y Sant'Anna): tratamiento continuo
##      con adopcion escalonada, ATT(d) lineal en la dosis. Exploratorio.
##      El paquete NO admite covariables: la identificacion descansa en
##      tendencias paralelas incondicionales. Para condicionar por diseno se
##      corre tambien con controles restringidos a los departamentos que
##      tienen al menos un municipio tratado.
##
## Toda la muestra de analisis respeta la ventana de cohortes 2013-2019 de la
## especificacion principal: los tratados de cohortes fuera de la ventana se
## SACAN de la muestra, no pasan a control.
##
## El tratamiento se toma directamente de panel_con_tratamiento_actualizado.csv
## (salida del 26) y no de first_treat_* del .rds, porque el .rds lo arma 01 y
## puede estar desactualizado (por ejemplo, con Barranquilla aun tratada).
## Del .rds se toman el outcome, id_num y las covariables.
##
## USO
##   Rscript scripts/40_did_depurada_y_dosis.R
##
## Insumos:
##   outputs/panel_analisis_did.rds                  (01_preparar_datos_did.R)
##   outputs/tablas/covariables_pre_municipio.csv    (02_matching_psm.R)
##   data/final/panel_con_tratamiento_actualizado.csv (26)
##   data/interim/dosis_tratamiento_final.csv         (39)
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(did)
  library(ggplot2)
})

## --- Raiz canonica ------------------------------------------------------------
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
DIR_FIG <- file.path(DIR_OUT, "figuras")
for (d in c(DIR_OUT, DIR_TAB, DIR_FIG)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

## --- Parametros (decididos ANTES de ver resultados) --------------------------
SEMILLA   <- 20260824
OUTCOME   <- "loss_area_ha"
VENTANA   <- 2013:2019          # cohortes reportables de la especificacion principal
BALANCE_E <- 5                  # 2019 + 5 = 2024: todas las cohortes de la ventana llegan
BITERS    <- 999
## contdid estima la curva dosis-respuesta DENTRO de cada cohorte, con una
## base de B-splines sobre las dosis de esa cohorte. Una cohorte con una sola
## dosis distinta no tiene rango y el ajuste falla con "Cannot set boundary
## knots from x". Se exige un minimo de dosis distintas por cohorte; con 3 el
## ajuste lineal tolera ademas los remuestreos del bootstrap.
MIN_DOSIS_DISTINTAS <- 3

## Outcome normalizado para el bloque C. El ATT en hectareas escala con el
## tamanio de la deforestacion de cada municipio, y la dosis esta correlacionada
## con ese tamanio: sin normalizar, la curva dosis-respuesta mide composicion.
## Se divide por el promedio propio en un periodo base COMUN a todos, anterior a
## la primera cohorte de la ventana (2013), de modo que no depende de G.
## No se usa la tasa sobre bosque porque comparte denominador con dosis_bosque.
PRE_COMUN   <- 2001:2012
OUTCOME_REL <- "loss_rel_pre"

COVARIABLES_XFORMLA <- ~ log_baseline_forest + temp_media_c_base +
  prec_anual_mm_base + discapital_base + disbogota_base + distancia_mercado_base +
  H_coca_base + homicidios_base + log_defor_pre_media + defor_pre_tendencia

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(txt) cat("\n", strrep("=", 74), "\n", txt, "\n", strrep("=", 74), "\n", sep = "")

## =============================================================================
## 1. Datos
## =============================================================================

panel <- readRDS(file.path(DIR_OUT, "panel_analisis_did.rds"))
panel$COD_DANE <- pad5(panel$COD_DANE)

## Covariables de deforestacion previa (mismo mecanismo que 03)
COLS_PRE <- c("log_defor_pre_media", "defor_pre_tendencia", "log_baseline_forest")
if (!all(COLS_PRE %in% names(panel))) {
  ruta_cov <- file.path(DIR_TAB, "covariables_pre_municipio.csv")
  if (!file.exists(ruta_cov)) stop("Falta ", ruta_cov, ". Corre antes 02_matching_psm.R.")
  cov_pre <- read_csv(ruta_cov, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE)
  cov_pre$COD_DANE <- pad5(cov_pre$COD_DANE)
  cols_unir <- intersect(c("COD_DANE", COLS_PRE), names(cov_pre))
  panel <- panel %>% select(-any_of(setdiff(cols_unir, "COD_DANE"))) %>%
    left_join(cov_pre[, cols_unir], by = "COD_DANE")
}
faltan_cov <- setdiff(all.vars(COVARIABLES_XFORMLA), names(panel))
if (length(faltan_cov)) stop("Faltan covariables en el panel: ", paste(faltan_cov, collapse = ", "))

## Tratamiento, a nivel municipio, desde la salida del 26
COL_DEP  <- "anio_inicio_tratamiento_todas_fuentes_redd_depurada"
COL_ORIG <- "anio_inicio_tratamiento_todas_fuentes_redd"
COL_MUE  <- "muestra_redd_depurada"
ruta_trat <- file.path(RAIZ_PROYECTO, "data/final/panel_con_tratamiento_actualizado.csv")
trat <- read_csv(ruta_trat, col_select = all_of(c("COD_DANE", COL_DEP, COL_ORIG, COL_MUE)),
                 col_types = cols(COD_DANE = col_character(), .default = col_double()),
                 show_col_types = FALSE)
if (!all(c(COL_DEP, COL_MUE) %in% names(trat))) {
  stop("El panel del 26 no tiene las columnas depuradas. Corre la version actual de ",
       "26_consolidar_fuentes_carbono.py.")
}
trat_mpio <- trat %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>%
  group_by(COD_DANE) %>%
  summarise(G_dep  = min_na(.data[[COL_DEP]]),
            G_orig = min_na(.data[[COL_ORIG]]),
            muestra_dep = min(.data[[COL_MUE]]), .groups = "drop") %>%
  mutate(G_dep = coalesce(G_dep, 0), G_orig = coalesce(G_orig, 0))

## Dosis, desde la salida del 39
dosis <- read_csv(file.path(RAIZ_PROYECTO, "data/interim/dosis_tratamiento_final.csv"),
                  col_types = cols(COD_DANE = col_character()), show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>%
  select(COD_DANE, tiene_dosis, tercil_dosis, dosis_bosque, dosis_area)

panel <- panel %>%
  select(-any_of(c("G_dep", "G_orig", "muestra_dep", names(dosis)[-1]))) %>%
  inner_join(trat_mpio, by = "COD_DANE") %>%
  left_join(dosis, by = "COD_DANE") %>%
  mutate(dpto = substr(COD_DANE, 1, 2))

## --- Panel balanceado: se sacan municipios con outcome faltante en algun anio.
##     did lo tolera, contdid no (no admite paneles desbalanceados).
n_anios <- n_distinct(panel$year)
incompletos <- panel %>% group_by(COD_DANE) %>%
  summarise(ok = n() == n_anios && all(!is.na(.data[[OUTCOME]])), .groups = "drop") %>%
  filter(!ok) %>% pull(COD_DANE)
if (length(incompletos)) {
  cat("Aviso:", length(incompletos), "municipios con outcome incompleto, fuera de la muestra:",
      paste(head(incompletos, 10), collapse = ", "), "\n")
  panel <- panel %>% filter(!COD_DANE %in% incompletos)
}

## Outcome relativo al promedio propio en el periodo base comun
base_pre <- panel %>% filter(year %in% PRE_COMUN) %>% group_by(COD_DANE) %>%
  summarise(media_base = mean(.data[[OUTCOME]], na.rm = TRUE), .groups = "drop")
panel <- panel %>% select(-any_of(c("media_base", OUTCOME_REL))) %>%
  left_join(base_pre, by = "COD_DANE") %>%
  mutate(!!OUTCOME_REL := .data[[OUTCOME]] / media_base)
sin_base <- unique(panel$COD_DANE[!is.finite(panel[[OUTCOME_REL]])])
cat("Municipios sin deforestacion en el periodo base (outcome relativo indefinido,",
    "fuera del bloque C):", length(sin_base), "\n")

## Restriccion de ventana: tratados fuera de 2013-2019 SALEN, no pasan a control
en_ventana <- function(d, gcol) d %>% filter(.data[[gcol]] == 0 | .data[[gcol]] %in% VENTANA)

describir <- function(d, gcol, etiqueta) {
  m <- d %>% distinct(COD_DANE, .keep_all = TRUE)
  cat(sprintf("  %-46s %5d municipios | %3d tratados | %4d nunca tratados\n",
              etiqueta, nrow(m), sum(m[[gcol]] > 0), sum(m[[gcol]] == 0)))
}

## =============================================================================
## 2. Estimador binario (misma logica que correr_did() en 03)
## =============================================================================

correr_cs <- function(data, gcol, xformla, etiqueta, control = "notyettreated") {
  cat("\n", strrep("-", 74), "\n", "Modelo: ", etiqueta, "\n", strrep("-", 74), "\n", sep = "")
  d <- data %>% mutate(.gname = .data[[gcol]])
  set.seed(SEMILLA)
  out <- tryCatch(
    att_gt(yname = OUTCOME, tname = "year", idname = "id_num", gname = ".gname",
           xformla = xformla, data = d, control_group = control, est_method = "dr",
           bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "id_num"),
    error = function(e) { cat("ERROR en att_gt():", conditionMessage(e), "\n"); NULL })
  if (is.null(out)) return(NULL)

  n_na <- sum(is.na(out$att))
  if (n_na) cat(sprintf("Aviso: %d de %d pares grupo-tiempo NA (%.0f%%), excluidos con na.rm.\n",
                        n_na, length(out$att), 100 * n_na / length(out$att)))

  simple <- aggte(out, type = "simple", na.rm = TRUE)
  din    <- aggte(out, type = "dynamic", min_e = -6, max_e = BALANCE_E, na.rm = TRUE)
  ## Estudio de eventos BALANCEADO: solo cohortes observadas las BALANCE_E+1
  ## duraciones completas, para comparar a igual tiempo de exposicion.
  din_bal <- tryCatch(aggte(out, type = "dynamic", balance_e = BALANCE_E, min_e = 0,
                            max_e = BALANCE_E, na.rm = TRUE), error = function(e) NULL)

  crit <- tryCatch(as.numeric(simple$crit.val.egt), error = function(e) NA_real_)
  cat(sprintf("ATT simple: %.2f (EE %.2f)  IC95 normal [%.2f, %.2f]\n",
              simple$overall.att, simple$overall.se,
              simple$overall.att - 1.96 * simple$overall.se,
              simple$overall.att + 1.96 * simple$overall.se))
  if (!is.null(din_bal)) {
    cat(sprintf("ATT balanceado e=0..%d: %.2f (EE %.2f)\n",
                BALANCE_E, din_bal$overall.att, din_bal$overall.se))
  }
  mp <- media_pre(d, ".gname")
  cat(sprintf("Media pretratamiento de los tratados: %.1f ha  ->  ATT relativo: %.1f%%\n",
              mp, 100 * simple$overall.att / mp))
  list(att_gt = out, simple = simple, dinamico = din, balanceado = din_bal,
       crit = crit, etiqueta = etiqueta, media_pre = mp,
       n_trat = n_distinct(d$COD_DANE[d$.gname > 0]))
}

## Media pretratamiento del outcome entre los tratados: el ATT en hectareas
## escala con el nivel de deforestacion del municipio, y la dosis esta
## correlacionada con ese nivel (los proyectos cubren municipios chocoanos
## pequenios enteros y solo una fraccion de los amazonicos grandes). Sin
## normalizar, comparar terciles compara tamanio y region, no intensidad.
media_pre <- function(d, gcol) {
  t <- d %>% filter(.data[[gcol]] > 0, year < .data[[gcol]])
  if (!nrow(t)) return(NA_real_)
  t %>% group_by(COD_DANE) %>% summarise(m = mean(.data[[OUTCOME]], na.rm = TRUE),
                                         .groups = "drop") %>% pull(m) %>% mean(na.rm = TRUE)
}

fila <- function(r, bloque) {
  if (is.null(r)) return(NULL)
  b <- r$balanceado
  data.frame(
    bloque = bloque, especificacion = r$etiqueta, n_tratados = r$n_trat,
    media_pre_tratados = r$media_pre,
    att_rel_pct = 100 * r$simple$overall.att / r$media_pre,
    att_simple = r$simple$overall.att, ee_simple = r$simple$overall.se,
    ic_inf = r$simple$overall.att - 1.96 * r$simple$overall.se,
    ic_sup = r$simple$overall.att + 1.96 * r$simple$overall.se,
    att_balanceado = if (is.null(b)) NA_real_ else b$overall.att,
    ee_balanceado  = if (is.null(b)) NA_real_ else b$overall.se
  )
}

resultados <- list()

## =============================================================================
## A. ATT binario: D1 depurada (principal) vs D1 original (sensibilidad)
## =============================================================================
titulo("A. ATT BINARIO: D1 DEPURADA vs D1 ORIGINAL (ventana 2013-2019)")

muestra_A  <- panel %>% filter(muestra_dep == 1) %>% en_ventana("G_dep")
muestra_A0 <- panel %>% en_ventana("G_orig")
describir(muestra_A,  "G_dep",  "D1 depurada, muestra depurada")
describir(muestra_A0, "G_orig", "D1 original, muestra completa")

resultados$A_dep_dr     <- correr_cs(muestra_A,  "G_dep",  COVARIABLES_XFORMLA, "D1 depurada - DR con covariables (PRINCIPAL)")
resultados$A_dep_sincov <- correr_cs(muestra_A,  "G_dep",  ~1,                  "D1 depurada - sin covariables")
resultados$A_orig_dr    <- correr_cs(muestra_A0, "G_orig", COVARIABLES_XFORMLA, "D1 original - DR con covariables")

if (!is.null(resultados$A_dep_dr)) {
  p <- ggdid(resultados$A_dep_dr$dinamico) +
    labs(title = "Estudio de eventos - D1 depurada (especificacion principal)",
         subtitle = "Doblemente robusto, cohortes 2013-2019, sin municipios de tratamiento ambiguo",
         x = "Anios desde el inicio del tratamiento", y = "ATT (hectareas deforestadas)") +
    theme_minimal()
  ggsave(file.path(DIR_FIG, "40_event_study_d1_depurada.png"), p, width = 9, height = 5.5, dpi = 150)
}

## =============================================================================
## B. ATT por tercil de dosis, a igual tiempo de exposicion
## Cada tercil contra los MISMOS nunca tratados. Los tratados sin dosis y los
## de otros terciles quedan fuera (no son controles: estan tratados). Por eso
## control_group = "nevertreated" en este bloque.
## =============================================================================
titulo("B. ATT POR TERCIL DE DOSIS (bosque), ventana balanceada e = 0..5")

base_B <- muestra_A
for (k in c("bajo", "medio", "alto")) {
  d_k <- base_B %>% filter(G_dep == 0 | (tiene_dosis %in% 1 & tercil_dosis == k))
  describir(d_k, "G_dep", paste("Tercil", k))
  resultados[[paste0("B_", k, "_sincov")]] <- correr_cs(
    d_k, "G_dep", ~1, paste("Tercil", k, "- sin covariables"), control = "nevertreated")
  resultados[[paste0("B_", k, "_dr")]] <- correr_cs(
    d_k, "G_dep", COVARIABLES_XFORMLA, paste("Tercil", k, "- DR"), control = "nevertreated")
}

## Grafico: ATT balanceado por tercil
terc <- do.call(rbind, lapply(c("bajo", "medio", "alto"), function(k) {
  do.call(rbind, lapply(c("sincov", "dr"), function(s) {
    r <- resultados[[paste0("B_", k, "_", s)]]
    if (is.null(r) || is.null(r$balanceado)) return(NULL)
    data.frame(tercil = k, spec = s, att = r$balanceado$overall.att, se = r$balanceado$overall.se)
  }))
}))
if (!is.null(terc) && nrow(terc)) {
  terc$tercil <- factor(terc$tercil, levels = c("bajo", "medio", "alto"))
  p <- ggplot(terc, aes(tercil, att, color = spec)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
                    position = position_dodge(width = 0.4)) +
    labs(title = "ATT por tercil de dosis (proporcion del bosque municipal con REDD+)",
         subtitle = sprintf("Promedio del estudio de eventos balanceado, e = 0..%d. IC 95%% normal.", BALANCE_E),
         x = "Tercil de dosis", y = "ATT (hectareas deforestadas)", color = "Especificacion") +
    theme_minimal()
  ggsave(file.path(DIR_FIG, "40_att_por_tercil.png"), p, width = 8, height = 5, dpi = 150)
}

## =============================================================================
## C. contdid: tratamiento continuo con adopcion escalonada
## =============================================================================
titulo("C. CONTDID - ATT(d) LINEAL EN LA DOSIS (exploratorio)")

res_cd <- list()
if (!requireNamespace("contdid", quietly = TRUE)) {
  cat("contdid no esta instalado. Instala con install.packages(\"contdid\") y vuelve a correr.\n",
      "Se omite el bloque C.\n")
} else {
  library(contdid)
  cat("Version de contdid:", as.character(packageVersion("contdid")),
      "(alfa: registrar la version en el capitulo)\n")

  ## Muestra: nunca tratados (D = 0) + tratados en ventana CON dosis.
  base_C <- muestra_A %>% filter(G_dep == 0 | tiene_dosis %in% 1)

  ## Cohortes sin variacion de dosis suficiente: se sacan de la muestra de
  ## contdid (no pasan a control: estan tratadas). Se reporta cuales y cuanto
  ## cuesta, porque es una restriccion de muestra que hay que declarar.
  var_coh <- base_C %>% filter(G_dep > 0) %>% distinct(COD_DANE, G_dep, dosis_bosque) %>%
    group_by(G_dep) %>% summarise(n = n(), n_dosis = n_distinct(dosis_bosque), .groups = "drop")
  cat("\nVariacion de dosis por cohorte (muestra de contdid):\n")
  print(as.data.frame(var_coh), row.names = FALSE)
  coh_malas <- var_coh$G_dep[var_coh$n_dosis < MIN_DOSIS_DISTINTAS]
  if (length(coh_malas)) {
    n_fuera <- n_distinct(base_C$COD_DANE[base_C$G_dep %in% coh_malas])
    cat(sprintf("Cohortes con menos de %d dosis distintas, excluidas de contdid: %s (%d municipios)\n",
                MIN_DOSIS_DISTINTAS, paste(coh_malas, collapse = ", "), n_fuera))
    base_C <- base_C %>% filter(!G_dep %in% coh_malas)
  }
  dptos_trat <- unique(base_C$dpto[base_C$G_dep > 0])

  ## contdid 0.1.1 asume periodos numerados 1..T: con anios calendario falla
  ## en overall_weights() ("valor ausente donde TRUE/FALSE es necesario") y el
  ## estudio de eventos devuelve NaN. Diagnosticado con 41_diagnostico_contdid.R.
  ## Se recodifican anio y cohorte con el MISMO desplazamiento, de modo que los
  ## tiempos relativos (e = t - G) no cambian.
  T0 <- min(panel$year) - 1L
  correr_cd <- function(d, dname, target, agg, etiqueta, yname = OUTCOME_REL) {
    dd <- d %>%
      filter(!COD_DANE %in% sin_base) %>%
      transmute(id = as.integer(id_num),
                year = as.integer(year - T0),
                Y = as.numeric(.data[[yname]]),
                D = as.numeric(if_else(G_dep == 0, 0, .data[[dname]])),
                G = as.integer(if_else(G_dep == 0, 0, G_dep - T0)))
    set.seed(SEMILLA)
    r <- tryCatch(
      contdid::cont_did(yname = "Y", tname = "year", idname = "id", dname = "D",
                        gname = "G", data = as.data.frame(dd),
                        target_parameter = target, aggregation = agg,
                        treatment_type = "continuous", control_group = "nevertreated",
                        num_knots = 0, degree = 1, biters = BITERS, cband = TRUE),
      error = function(e) { cat("ERROR en cont_did() [", etiqueta, "]:", conditionMessage(e), "\n"); NULL })
    if (is.null(r)) return(NULL)
    cat("\n--- ", etiqueta, " ---\n", sep = "")
    txt <- capture.output(summary(r))
    cat(txt, sep = "\n")
    writeLines(txt, file.path(DIR_TAB, paste0("40_contdid_", gsub("[^a-z0-9]+", "_", tolower(etiqueta)), ".txt")))
    r
  }

  muestras_C <- list(
    todos_controles     = base_C,
    controles_regionales = base_C %>% filter(G_dep > 0 | dpto %in% dptos_trat),
    sin_censura         = base_C %>% filter(G_dep == 0 | .data[["dosis_bosque"]] < 0.9999)
  )
  for (nm in names(muestras_C)) describir(muestras_C[[nm]], "G_dep", paste("contdid:", nm))

  ## Outcome RELATIVO: el reportable. ATT(d) = cambio en la deforestacion como
  ## fraccion del promedio propio 2001-2012.
  for (dose in c("dosis_bosque", "dosis_area")) {
    for (nm in names(muestras_C)) {
      if (dose == "dosis_area" && nm == "sin_censura") next
      et <- paste("relativo", dose, nm)
      res_cd[[paste0(et, "_dose")]] <- correr_cd(muestras_C[[nm]], dose, "level", "dose", paste(et, "ATT(d)"))
      res_cd[[paste0(et, "_es")]]   <- correr_cd(muestras_C[[nm]], dose, "level", "eventstudy", paste(et, "estudio de eventos"))
    }
  }
  ## Hectareas: solo como referencia, para mostrar el artefacto de escala.
  res_cd[["hectareas_dose"]] <- correr_cd(base_C, "dosis_bosque", "level", "dose",
                                          "hectareas dosis_bosque todos_controles ATT(d) - referencia",
                                          yname = OUTCOME)
  ## Pendiente: exploratoria. Exige tendencias paralelas FUERTES para leerse
  ## como efecto causal de aumentar la dosis. Ojo: en la corrida de diagnostico
  ## su error estandar salio degenerado (2,1 frente a 321 del ATT); comparar
  ## siempre la magnitud del EE de la ACRT contra la del ATT antes de reportarla.
  res_cd[["pendiente"]] <- correr_cd(base_C, "dosis_bosque", "slope", "dose",
                                     "relativo dosis_bosque todos_controles ACRT(d) - exploratorio")

  ## Puente: el ATT binario sin covariables sobre la MISMA muestra de contdid.
  ## El ATT global de contdid deberia parecerse a este; si no, la diferencia
  ## viene de la ponderacion por dosis, no de la muestra.
  resultados$C_puente_binario <- correr_cs(base_C, "G_dep", ~1,
    "Puente: binario sin covariables, muestra de contdid", control = "nevertreated")
  ## Mismo puente en outcome relativo: es el que se compara con el ATT global
  ## de contdid en su version relativa.
  OUTCOME_ORIG <- OUTCOME
  OUTCOME <- OUTCOME_REL
  resultados$C_puente_relativo <- correr_cs(base_C %>% filter(!COD_DANE %in% sin_base), "G_dep", ~1,
    "Puente relativo: binario sin covariables, outcome / base 2001-2012", control = "nevertreated")
  OUTCOME <- OUTCOME_ORIG

  for (nm in names(res_cd)) {
    r <- res_cd[[nm]]
    if (is.null(r)) next
    p <- tryCatch(contdid::ggcont_did(r) + theme_minimal() + labs(title = nm),
                  error = function(e) NULL)
    if (!is.null(p)) ggsave(file.path(DIR_FIG, paste0("40_contdid_", gsub("[^a-z0-9]+", "_", tolower(nm)), ".png")),
                            p, width = 8, height = 5, dpi = 150)
  }
}

## =============================================================================
## 3. Tabla resumen y guardado
## =============================================================================
titulo("RESUMEN")
tabla <- do.call(rbind, c(
  lapply(grep("^A_", names(resultados), value = TRUE), function(n) fila(resultados[[n]], "A. binario")),
  lapply(grep("^B_", names(resultados), value = TRUE), function(n) fila(resultados[[n]], "B. terciles")),
  lapply(grep("^C_", names(resultados), value = TRUE), function(n) fila(resultados[[n]], "C. puente"))
))
if (!is.null(tabla)) {
  print(tabla, row.names = FALSE, digits = 4)
  write_csv(tabla, file.path(DIR_TAB, "40_resumen_depurada_y_dosis.csv"))
}
saveRDS(list(binario_y_terciles = resultados, contdid = res_cd,
             parametros = list(ventana = VENTANA, balance_e = BALANCE_E, semilla = SEMILLA,
                               contdid_version = if (requireNamespace("contdid", quietly = TRUE))
                                 as.character(packageVersion("contdid")) else NA)),
        file.path(DIR_OUT, "40_resultados_depurada_y_dosis.rds"))
cat("\nGuardado: outputs/40_resultados_depurada_y_dosis.rds\n")
cat("Guardado: outputs/tablas/40_resumen_depurada_y_dosis.csv\n")
