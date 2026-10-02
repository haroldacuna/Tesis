## =============================================================================
## 56_did_celdas.R
##
## Evaluacion de REDD+ a nivel de POLIGONO (comentario 3 del asesor): celdas de
## 2 x 2 km dentro y fuera de los 32 proyectos de deforestacion evitada (AD)
## con poligono (Karnik et al. 2025), resultados de MapBiomas por tipo de
## bosque (origen) y tipo de uso (destino). Insumos del 55 (verificado contra
## los totales municipales de 39b: diferencia maxima 0,03 %).
##
## DISENO FIJADO ANTES DE VER EFECTOS (28/09/2026)
##   Muestra
##     - cohortes con >= 3 proyectos: 2013, 2014, 2015, 2017, 2018 (analogo al
##       umbral de 3 municipios del analisis municipal); fuera 2010, 2016, 2019,
##       2020 (1-2 proyectos)
##     - celdas sin municipio (centroide en el mar o fuera del pais) fuera
##     - tratada = >= 50 % dentro de un proyecto AD; control = sin cobertura y a
##       > 10 km de cualquier proyecto; parcial y no elegible fuera (ver 55)
##   Emparejamiento previo (MatchIt): 2 controles por tratada, vecino mas
##     cercano en propension, sin reemplazo, calibre 0,2 DE, exacto por region.
##     Covariables de linea base: % bosque 2000, % bosque inundable, elevacion,
##     pendiente, deforestacion propia 2001-2009 (media y pendiente OLS) y
##     presion del entorno 2001-2009 (media en el vecindario 5x5, sin la celda).
##   Estimador: Callaway-Sant'Anna DR, notyettreated, 2001-2023, las mismas
##     covariables (sin region: ya es exacta), ponderado por los pesos del
##     emparejamiento (weightsname), bootstrap 999.
##   CAMBIO 28/09/2026 (tras la primera corrida, antes de ver 'reg'): el DR
##     produjo errores estandar degenerados (EE del estudio de eventos entre
##     0,21 y 16,6 sin patron; EE global 5,5 veces la media previa), firma de
##     propensiones casi separadas dentro de cohortes de 3-4 proyectos. Como el
##     emparejamiento ya disena la comparacion, el estimador PRINCIPAL pasa a
##     ser la regresion del resultado (est_method = "reg", no usa propension).
##     DR y sin covariables quedan como sensibilidad del resultado principal,
##     y se reporta el ATT por cohorte para localizar la inestabilidad.
##   Inferencia: conglomerados = PROYECTO para tratadas y MUNICIPIO para
##     controles (el tratamiento se asigna al proyecto, no a la celda).
##   Resultados
##     principal  def_p1: bosque estable 5a -> antropico persistente (ha/celda)
##     tasa       def_p1 / bosque 2000 (% anual)
##     por tipo   solo si >= 1.000 celda-anios tratados con valor > 0 y
##                >= 3 proyectos que lo aportan (regla fijada tras ver la
##                incidencia, antes de ver efectos). El resto: "no identificable".
##   Fuga: celdas de amortiguacion (sin cobertura, <= 10 km) con la cohorte del
##     proyecto AD mas cercano, contra sus propios controles emparejados.
##     ATT > 0 = desplazamiento hacia el entorno.
##
## USO
##   Rscript scripts/56_did_celdas.R
## =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(MatchIt)
  library(did)
  library(sf)
})
for (pk in c("MASS", "jsonlite"))
  if (!requireNamespace(pk, quietly = TRUE)) install.packages(pk)

RAIZ_PROYECTO <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "data/.raiz_canonica"))) {
    p <- dirname(d)
    if (identical(p, d)) stop("No encuentro data/.raiz_canonica subiendo desde ", getwd())
    d <- p
  }
  d
})
DIR_CEL <- file.path(RAIZ_PROYECTO, "data/interim/celdas")
DIR_OUT <- file.path(RAIZ_PROYECTO, "outputs")
DIR_TAB <- file.path(DIR_OUT, "tablas")
DIR_FIG <- file.path(DIR_OUT, "figuras")
for (d in c(DIR_TAB, DIR_FIG)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

## --- Parametros (fijados antes de ver efectos) --------------------------------
SEMILLA        <- 20260928
COHORTES       <- c(2013, 2014, 2015, 2017, 2018)
ANIO_FIN       <- 2023
PRE_COV        <- 2001:2009
RATIO          <- 2
CALIBRE        <- 0.2
BITERS         <- 999
MIN_E          <- -8
MIN_CELDA_ANIO <- 1000
MIN_PROYECTOS  <- 3
METODO_PPAL    <- "reg"     # ver CAMBIO 28/09/2026 en el encabezado
COVS <- c("pct_bosque2000", "pct_inundable", "elev", "pend",
          "def_pre_media", "def_pre_tend", "presion_pre")
TIPOS <- c("def_p1_tierra_firme", "def_p1_inundable", "def_p1_manglar", "def_p1_arena",
           "def_p1_rastrojo", "def_p1_agropecuario", "def_p1_mineria", "def_p1_otros_antropicos",
           "def_p1_palma", "def_p1_silvicultura", "def_p1_platano", "def_p1_infraestructura")
REGION_DPTO <- c("91" = "Amazonia", "94" = "Amazonia", "97" = "Amazonia", "18" = "Amazonia",
                 "86" = "Amazonia", "95" = "Amazonia", "27" = "Pacifico", "52" = "Pacifico",
                 "76" = "Pacifico", "19" = "Pacifico", "50" = "Orinoquia", "81" = "Orinoquia",
                 "85" = "Orinoquia", "99" = "Orinoquia")
titulo <- function(t) cat("\n", strrep("=", 74), "\n", t, "\n", strrep("=", 74), "\n", sep = "")

## =============================================================================
## Funciones (R base / data.table; probadas por separado)
## =============================================================================

## Pendiente OLS por celda en la ventana previa, sobre panel completo (ceros)
covariables_previas <- function(pan, celdas, anios = PRE_COV) {
  g <- CJ(celda = celdas, anio = anios)
  g <- pan[, .(celda, anio, def_p1)][g, on = .(celda, anio)]
  g[is.na(def_p1), def_p1 := 0]
  g[, .(def_pre_media = mean(def_p1),
        def_pre_tend = { t <- anio - mean(anio); sum(t * def_p1) / sum(t^2) }), by = celda]
}

## Presion del entorno: media anual 2001-2009 de def_p1 en el vecindario 5x5
## (24 celdas vecinas; las vecinas sin registro cuentan 0; la propia no cuenta)
presion_vecinal <- function(pan, celdas, anios = PRE_COV, radio = 2L) {
  pre <- pan[anio %in% anios, .(dp = sum(def_p1, na.rm = TRUE)), by = celda]
  pre[, `:=`(ix = celda %/% 10000L, iy = celda %% 10000L)]
  obj <- data.table(celda = celdas, ix = celdas %/% 10000L, iy = celdas %% 10000L, suma = 0)
  for (dx in -radio:radio) for (dy in -radio:radio) {
    if (dx == 0L && dy == 0L) next
    vec <- pre[, .(ix = ix - dx, iy = iy - dy, dp)]
    obj[vec, on = .(ix, iy), suma := suma + i.dp]
  }
  n_vec <- (2L * radio + 1L)^2 - 1L
  obj[, .(celda, presion_pre = suma / (n_vec * length(anios)))]
}

## Regla de identificabilidad por tipo
identificable <- function(pan_trat, proyecto_de, col) {
  pos <- pan_trat[get(col) > 0]
  c(celda_anios = nrow(pos), proyectos = uniqueN(proyecto_de[as.character(pos$celda)]))
}

## Wald conjunto de los coeficientes previos con varianza AGRUPADA: las
## funciones de influencia se suman dentro de cada conglomerado antes de formar
## la varianza (did no reporta su pre-test al agrupar mas alla de la unidad).
## `cl`: conglomerado de cada fila de la funcion de influencia (ids ordenados).
wald_pre <- function(din, cl, g_trat = Inf) {
  e <- din$egt; idx <- which(e < 0 & !is.na(din$att.egt))
  if (g_trat <= length(idx)) {       # varianza casi singular: la prueba no es informativa
    cat(sprintf("  (Wald previo no reportado: %d conglomerados tratados para %d coeficientes previos)\n",
                as.integer(g_trat), length(idx)))
    return(c(W = NA, gl = 0, p = NA))
  }
  inf <- din$inf.function$dynamic.inf.func.e
  if (!length(idx) || is.null(inf) || ncol(inf) != length(e) || nrow(inf) != length(cl))
    return(c(W = NA, gl = 0, p = NA))
  n <- nrow(inf); G <- length(unique(cl))
  S <- rowsum(as.matrix(inf[, idx, drop = FALSE]), cl)
  V <- (G / (G - 1)) * crossprod(S) / n^2
  b <- din$att.egt[idx]
  W <- as.numeric(t(b) %*% MASS::ginv(V) %*% b)
  gl <- min(length(idx), G - 1)
  c(W = W, gl = gl, p = stats::pchisq(W, gl, lower.tail = FALSE))
}

smd <- function(x, t, w = rep(1, length(x))) {
  m1 <- weighted.mean(x[t == 1], w[t == 1]); m0 <- weighted.mean(x[t == 0], w[t == 0])
  (m1 - m0) / sqrt((var(x[t == 1]) + var(x[t == 0])) / 2)
}

## =============================================================================
## 1. Datos
## =============================================================================
titulo("1. DATOS")
at <- fread(file.path(DIR_CEL, "celdas_atributos.csv"), colClasses = list(character = "COD_DANE"))
## Lectura blindada: una lectura incompleta del .csv.gz (28/09/2026) produjo
## resultados invalidos sin error. Cualquier aviso de fread detiene el script,
## y las filas y la suma de control deben coincidir con lo que escribio el 55.
## La descompresion se hace EN MEMORIA (gzfile + readLines): fread con R.utils
## descomprime primero a la carpeta temporal del disco, y con el disco casi
## lleno la escritura se corta en silencio (28/09/2026: 2,83 de 4,09 millones
## de filas).
leer_gz_memoria <- function(ruta) {
  con <- gzfile(ruta, open = "rt"); on.exit(close(con))
  fread(text = readLines(con), showProgress = FALSE)
}
avisos_lectura <- character(0)
pan <- withCallingHandlers(leer_gz_memoria(file.path(DIR_CEL, "panel_celdas_nocero.csv.gz")),
                           warning = function(w) { avisos_lectura <<- c(avisos_lectura, conditionMessage(w))
                                                   invokeRestart("muffleWarning") })
if (length(avisos_lectura))
  stop("fread emitio avisos al leer el panel (lectura posiblemente incompleta):\n  ",
       paste(unique(avisos_lectura), collapse = "\n  "))
at[, COD_DANE := { v <- suppressWarnings(as.integer(COD_DANE))
                   fifelse(is.na(v), NA_character_, formatC(v, width = 5, flag = "0")) }]
at[, region := fifelse(substr(COD_DANE, 1, 2) %in% names(REGION_DPTO),
                       REGION_DPTO[substr(COD_DANE, 1, 2)], "Andes_Caribe")]
at[, pct_inundable := bosque2000_inundable_ha / area_ha]
qc <- jsonlite::fromJSON(file.path(DIR_CEL, "qc_55.json"))
if (!isTRUE(qc$verificacion_ok))
  stop("qc_55.json no tiene verificacion_ok = true: no se estima sobre un panel no verificado.")
if (is.null(qc$panel_filas))
  stop("qc_55.json no registra panel_filas: vuelve a correr la version actual de 55_panel_celdas.py.")
suma_leida <- round(sum(pan$def_est5), 2)
cat(sprintf("Panel leido: %s filas (esperadas %s) | suma def_est5 %.2f (esperada %.2f)\n",
            format(nrow(pan), big.mark = ","), format(qc$panel_filas, big.mark = ","),
            suma_leida, qc$panel_suma_def_est5))
if (nrow(pan) != qc$panel_filas || abs(suma_leida - qc$panel_suma_def_est5) > 1)
  stop("El panel leido no coincide con el que escribio el 55: lectura incompleta o archivo cambiado.")

## Cohorte del proyecto AD mas cercano para la zona de amortiguacion
man <- jsonlite::fromJSON(file.path(DIR_CEL, "manifiesto_54.json"))
proy <- st_read(file.path(DIR_CEL, "proyectos_gee.geojson"), quiet = TRUE)
proy_ad <- st_transform(proy[proy$tipo == "AD", ], man$crs_grilla)
amo <- at[grupo == "amortiguacion"]
pts <- st_as_sf(data.frame(celda = amo$celda,
                           x = (amo$celda %/% 10000 + 0.5) * man$celda_m,
                           y = (amo$celda %% 10000 + 0.5) * man$celda_m),
                coords = c("x", "y"), crs = man$crs_grilla)
cerca <- st_nearest_feature(pts, proy_ad)
at[grupo == "amortiguacion", `:=`(cohorte_vecino = proy_ad$anio[cerca],
                                   proyecto_vecino = proy_ad$ProjectID[cerca])]

base <- at[!is.na(COD_DANE) & grupo %in% c("tratada", "control", "amortiguacion")]
base <- base[grupo != "tratada" | cohorte %in% COHORTES]
base <- base[grupo != "amortiguacion" | cohorte_vecino %in% COHORTES]
cat(sprintf("Celdas: %s tratadas, %s controles, %s de amortiguacion\n",
            format(base[grupo == "tratada", .N], big.mark = ","),
            format(base[grupo == "control", .N], big.mark = ","),
            format(base[grupo == "amortiguacion", .N], big.mark = ",")))
print(base[grupo == "tratada", .(proyectos = uniqueN(proyecto_ad), celdas = .N), by = cohorte][order(cohorte)])

cov_pre <- covariables_previas(pan, base$celda)
pres <- presion_vecinal(pan, base$celda)
base <- merge(base, cov_pre, by = "celda")
base <- merge(base, pres, by = "celda")
faltan <- base[!complete.cases(base[, ..COVS]), .N]
if (faltan) cat("Celdas con covariables faltantes (salen):", faltan, "\n")
base <- base[complete.cases(base[, ..COVS])]

## =============================================================================
## 2. Emparejamiento (principal y fuga)
## =============================================================================
emparejar <- function(dt, grupo_trat, etq) {
  d <- dt[grupo %in% c(grupo_trat, "control")]
  d[, trat := as.integer(grupo == grupo_trat)]
  f <- as.formula(paste("trat ~", paste(COVS, collapse = " + ")))
  set.seed(SEMILLA)
  m <- matchit(f, data = as.data.frame(d), method = "nearest", distance = "glm",
               ratio = RATIO, replace = FALSE, caliper = CALIBRE, exact = ~ region)
  md <- as.data.table(match.data(m))
  bal <- rbindlist(lapply(COVS, function(v) data.table(
    covariable = v,
    smd_antes = smd(d[[v]], d$trat),
    smd_despues = smd(md[[v]], md$trat, md$weights))))
  cat(sprintf("\n[%s] emparejadas: %d tratadas de %d | %d controles\n", etq,
              md[trat == 1, .N], d[trat == 1, .N], md[trat == 0, .N]))
  print(bal[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)], row.names = FALSE)
  fwrite(bal, file.path(DIR_TAB, sprintf("56_balance_%s.csv", etq)))
  md
}
titulo("2. EMPAREJAMIENTO")
emp_ppal <- emparejar(base, "tratada", "principal")
emp_fuga <- emparejar(base[grupo != "tratada"], "amortiguacion", "fuga")

## =============================================================================
## 3. Panel balanceado y estimacion
## =============================================================================
armar_panel <- function(md, col_coh, col_proy) {
  cel <- md[, c("celda", "trat", "COD_DANE", col_coh, col_proy, "bosque2000_ha", "weights", COVS), with = FALSE]
  setnames(cel, "weights", "w_match")
  setnames(cel, c(col_coh, col_proy), c("G", "proyecto"))
  cel[trat == 0, G := 0]
  cel[, cluster := fifelse(trat == 1, paste0("P_", proyecto), paste0("M_", COD_DANE))]
  cel[, cluster_id := as.integer(factor(cluster))]
  g <- CJ(celda = cel$celda, anio = 2001:ANIO_FIN)
  cols <- c("def_p1", intersect(TIPOS, names(pan)))
  p <- pan[anio <= ANIO_FIN, c("celda", "anio", cols), with = FALSE][g, on = .(celda, anio)]
  for (cc in cols) set(p, which(is.na(p[[cc]])), cc, 0)
  p <- merge(p, cel, by = "celda")
  p[, tasa_pct := 100 * def_p1 / bosque2000_ha]
  p[, id := as.integer(factor(celda))]
  p[]
}

correr <- function(p, y, etq, metodo = METODO_PPAL, covs = TRUE, por_cohorte = FALSE) {
  cat("\n--- ", etq, " [", metodo, if (!covs) ", sin covariables", "] ---\n", sep = "")
  xf <- if (covs) as.formula(paste("~", paste(COVS, collapse = " + "))) else ~1
  set.seed(SEMILLA)
  out <- suppressWarnings(tryCatch(
    att_gt(yname = y, tname = "anio", idname = "id", gname = "G", xformla = xf,
           data = as.data.frame(p), weightsname = "w_match",
           control_group = "notyettreated", est_method = metodo,
           bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "cluster_id"),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }))
  if (is.null(out)) return(NULL)
  s <- aggte(out, type = "simple", na.rm = TRUE)
  din <- aggte(out, type = "dynamic", min_e = MIN_E, na.rm = TRUE)
  cl <- p[order(id), .(cl = cluster_id[1]), by = id]$cl
  wp <- wald_pre(din, cl, g_trat = uniqueN(p[G > 0, cluster]))
  pre <- p[G > 0 & anio < G, mean(get(y))]
  cat(sprintf("ATT %.3f (EE %.3f) IC95 [%.3f, %.3f] | relativo %.1f %% de la media pre (%.3f) | pre p = %.3f\n",
              s$overall.att, s$overall.se, s$overall.att - 1.96 * s$overall.se,
              s$overall.att + 1.96 * s$overall.se, 100 * s$overall.att / pre, pre, wp["p"]))
  coh <- NULL
  if (por_cohorte) {
    gr <- tryCatch(aggte(out, type = "group", na.rm = TRUE), error = function(e) NULL)
    if (!is.null(gr)) {
      coh <- data.table(resultado = y, metodo = metodo, cohorte = gr$egt, att = gr$att.egt, ee = gr$se.egt,
                        proyectos = sapply(gr$egt, function(g) uniqueN(p[G == g, cluster])),
                        celdas = sapply(gr$egt, function(g) uniqueN(p[G == g, celda])))
      cat("  ATT por cohorte:\n"); print(coh[, .(cohorte, att = round(att, 3), ee = round(ee, 3), proyectos, celdas)],
                                          row.names = FALSE)
    }
  }
  list(coh = coh, fila = data.table(resultado = y, analisis = etq, metodo = metodo, covariables = covs,
                         att = s$overall.att, ee = s$overall.se,
                         ic_inf = s$overall.att - 1.96 * s$overall.se,
                         ic_sup = s$overall.att + 1.96 * s$overall.se,
                         media_pre_tratadas = pre, att_rel_pct = 100 * s$overall.att / pre,
                         wald_pre_p = wp["p"], celdas_tratadas = uniqueN(p[G > 0, celda]),
                         celdas_control = uniqueN(p[G == 0, celda]),
                         conglomerados_tratados = uniqueN(p[G > 0, cluster])),
       din = din)
}

titulo("3. ESTIMACION")
p_ppal <- armar_panel(emp_ppal, "cohorte", "proyecto_ad")
p_fuga <- armar_panel(emp_fuga, "cohorte_vecino", "proyecto_vecino")
proyecto_de <- setNames(p_ppal[G > 0, proyecto], p_ppal[G > 0, as.character(celda)])

res <- list(); dins <- list(); cohs <- list()
for (y in c("def_p1", "tasa_pct")) {
  r <- correr(p_ppal, y, paste("principal |", y), por_cohorte = TRUE)
  if (!is.null(r)) { res[[length(res) + 1]] <- r$fila; dins[[y]] <- r$din; cohs[[y]] <- r$coh }
}
## Sensibilidad del resultado principal: DR (con su advertencia) y sin covariables
r <- correr(p_ppal, "def_p1", "sensibilidad | def_p1", metodo = "dr", por_cohorte = TRUE)
if (!is.null(r)) { res[[length(res) + 1]] <- r$fila; dins[["def_p1_dr"]] <- r$din; cohs[["dr"]] <- r$coh }
r <- correr(p_ppal, "def_p1", "sensibilidad | def_p1", metodo = "dr", covs = FALSE)
if (!is.null(r)) res[[length(res) + 1]] <- r$fila

cat("\nIdentificabilidad por tipo (>= ", MIN_CELDA_ANIO, " celda-anios y >= ", MIN_PROYECTOS, " proyectos):\n", sep = "")
ident <- rbindlist(lapply(intersect(TIPOS, names(p_ppal)), function(tp) {
  v <- identificable(p_ppal[G > 0], proyecto_de, tp)
  data.table(resultado = tp, celda_anios_pos = v[["celda_anios"]], proyectos = v[["proyectos"]],
             identificable = v[["celda_anios"]] >= MIN_CELDA_ANIO & v[["proyectos"]] >= MIN_PROYECTOS)
}))
print(ident, row.names = FALSE)
fwrite(ident, file.path(DIR_TAB, "56_identificabilidad_tipos.csv"))
for (tp in ident[identificable == TRUE, resultado]) {
  r <- correr(p_ppal, tp, paste("por tipo |", tp))
  if (!is.null(r)) res[[length(res) + 1]] <- r$fila
}
r <- correr(p_fuga, "def_p1", "fuga (amortiguacion vs control) | def_p1")
if (!is.null(r)) { res[[length(res) + 1]] <- r$fila; dins[["fuga"]] <- r$din }

tabla <- rbindlist(res, fill = TRUE)
if (length(cohs)) fwrite(rbindlist(cohs), file.path(DIR_TAB, "56_att_por_cohorte.csv"))
fwrite(tabla, file.path(DIR_TAB, "56_resultados_celdas.csv"))
es <- rbindlist(lapply(names(dins), function(k) data.table(modelo = k, e = dins[[k]]$egt,
                                                           att = dins[[k]]$att.egt, ee = dins[[k]]$se.egt)))
fwrite(es, file.path(DIR_TAB, "56_event_study_celdas.csv"))

titulo("RESUMEN")
print(tabla[, .(resultado, analisis = substr(analisis, 1, 20), metodo, covariables, att = round(att, 3), ee = round(ee, 3),
                att_rel_pct = round(att_rel_pct, 1), wald_pre_p = round(wald_pre_p, 3),
                celdas_tratadas, conglomerados_tratados)], row.names = FALSE)
cat("\nGuardado: outputs/tablas/56_resultados_celdas.csv, 56_event_study_celdas.csv,",
    "56_att_por_cohorte.csv, 56_balance_*.csv, 56_identificabilidad_tipos.csv\n")
