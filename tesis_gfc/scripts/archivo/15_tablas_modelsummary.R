## =============================================================================
## 15_tablas_modelsummary.R
##
## Tablas de la tesis en formato academico, generadas con modelsummary +
## tinytable y exportadas como PNG.
##
## DOS NOTAS DE COMPATIBILIDAD (importantes)
## -----------------------------------------
## (1) NO usar gof_map = NA. En modelsummary 2.2.0, gof_map se espera como
##     data.frame con columnas raw/clean/fmt, y el paquete hace gof_map$raw
##     internamente. Pasarle NA (vector atomico) produce
##         Error en x$raw: $ operator is invalid for atomic vectors
##     La via correcta es gof_omit, que recibe una expresion regular:
##     gof_omit = ".*" suprime todos los estadisticos de ajuste.
##
## (2) NO usar output = "tinytable" con tinytable 0.5.0. modelsummary 2.2.0
##     llama a internos de tinytable que cambiaron de forma entre versiones.
##     Este script pide output = "data.frame" y construye la tabla con
##     tinytable::tt() por separado.
##
## REQUISITOS
##   install.packages(c("modelsummary", "tinytable"))
##   Quarto CLI para exportar a PNG:  https://quarto.org/docs/get-started/
##
## Requiere haber corrido 01, 02, 03 y, opcionalmente, 08 y 13.
## USO:  Rscript 15_tablas_modelsummary.R
## =============================================================================

library(dplyr)
library(modelsummary)
library(tinytable)

OUTPUT_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs"
DIR_TABLAS <- file.path(OUTPUT_ROOT, "tablas")
DIR_IMG    <- file.path(OUTPUT_ROOT, "figuras/tablas")

dir.create(DIR_TABLAS, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_IMG, showWarnings = FALSE, recursive = TRUE)

ESTRELLAS <- c("*" = 0.10, "**" = 0.05, "***" = 0.01)

## Si TRUE, el titulo se dibuja dentro del PNG (Typst lo pone abajo y en gris).
## Si FALSE, el PNG sale limpio y el titulo se imprime en consola para que lo
## pegues como "Título" de tabla en Word: asi obtienes numeracion automatica
## y referencias cruzadas. Recomendado FALSE.
TITULO_EN_IMAGEN <- FALSE

## =============================================================================
## 0. Motor de exportacion
## =============================================================================

HAY_QUARTO  <- nzchar(Sys.which("quarto"))
HAY_WEBSHOT <- requireNamespace("webshot2", quietly = TRUE)

cat("Motor de exportacion:\n")
cat("  Quarto CLI (PNG via Typst):", if (HAY_QUARTO) "SI" else "NO", "\n")
cat("  webshot2 (respaldo HTML):  ", if (HAY_WEBSHOT) "SI" else "NO", "\n\n")

if (!HAY_QUARTO && !HAY_WEBSHOT) {
  stop("No hay motor de exportacion. Instala Quarto o el paquete webshot2.")
}

guardar_png <- function(tab, archivo, titulo = NULL) {
  destino <- file.path(DIR_IMG, archivo)

  if (HAY_QUARTO) {
    ok <- tryCatch({
      tinytable::save_tt(tab, output = destino, overwrite = TRUE); TRUE
    }, error = function(e) {
      cat("  Aviso: Quarto fallo (", conditionMessage(e), "). Uso el respaldo HTML.\n", sep = "")
      FALSE
    })
    if (!ok) {
      ruta_html <- file.path(tempdir(), sub("\\.png$", ".html", archivo))
      tinytable::save_tt(tab, output = ruta_html, overwrite = TRUE)
      webshot2::webshot(paste0("file://", normalizePath(ruta_html)),
                        file = destino, selector = "table", zoom = 4)
    }
  } else {
    ruta_html <- file.path(tempdir(), sub("\\.png$", ".html", archivo))
    tinytable::save_tt(tab, output = ruta_html, overwrite = TRUE)
    webshot2::webshot(paste0("file://", normalizePath(ruta_html)),
                      file = destino, selector = "table", zoom = 4)
  }

  cat("Tabla guardada:", destino, "\n")
  if (!TITULO_EN_IMAGEN && !is.null(titulo)) cat("   Titulo para Word: ", titulo, "\n", sep = "")
  invisible(destino)
}

formato_academico <- function(tab) {
  n <- ncol(tab)
  tab |>
    tinytable::style_tt(j = 1, align = "l") |>
    tinytable::style_tt(j = 2:n, align = "c")
}

## =============================================================================
## 1. FORMATEO NUMERICO EN CONVENCION CASTELLANA
##
## Antes convivian "-121.02" (punto decimal) y "1.122" (punto de millar) en la
## misma tabla, lo que se puede leer mal. Aqui se unifica: coma decimal y
## punto de millar en todo. Ambos formateadores van con tryCatch porque
## fmt_decimal() y fmt_significant() no existen en todas las versiones de
## modelsummary; si faltan, se cae al comportamiento numerico simple.
## =============================================================================

## fmt_decimal() no existe en modelsummary 2.2.0 y fmt_significant() puede
## faltar segun version; ademas format_tt(digits=) de tinytable cuenta CIFRAS
## SIGNIFICATIVAS, no decimales, y degrada la precision. Por eso el formateo
## no se delega a ninguno de los dos paquetes: se convierte a mano, al final,
## sobre la tabla ya armada. Asi las siete tablas comparten convencion.

## Cifras significativas para la Tabla 3, con respaldo si no existe la funcion.
FMT_SIG <- function(n = 3) {
  tryCatch(modelsummary::fmt_significant(n), error = function(e) "%.3g")
}

## Convierte una celda de texto a convencion castellana: coma decimal y punto
## de millar. Maneja los tres formatos que aparecen en estas tablas:
##   numero puro        "1121"          -> "1.121"
##   celda compuesta    "(83.97)"       -> "(83,97)"
##   notacion cientifica "1.81e-06***"  -> "1,81e-06***"
convertir_es <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return(s)
    if (grepl("^-?[0-9]+(\\.[0-9]+)?$", s)) {
      dec <- if (grepl("\\.", s)) nchar(sub("^.*\\.", "", s)) else 0
      return(formatC(as.numeric(s), format = "f", digits = dec,
                     big.mark = ".", decimal.mark = ","))
    }
    gsub("([0-9])\\.([0-9])", "\\1,\\2", s)
  }, character(1), USE.NAMES = FALSE)
}

## Formatea las columnas numericas de un data.frame leido de CSV. Usa numero
## fijo de decimales (no cifras significativas) para no perder precision, y
## detecta las columnas enteras para no escribir "0,00" donde va "0".
formatear_columnas <- function(d, dec = 2) {
  for (j in names(d)) {
    x <- d[[j]]
    if (is.numeric(x)) {
      dg <- if (all(x == round(x), na.rm = TRUE)) 0 else dec
      d[[j]] <- formatC(x, format = "f", digits = dg,
                        big.mark = ".", decimal.mark = ",")
    }
  }
  d
}

## Separador de millares para los conteos que se escriben a mano.
mil <- function(x) format(x, big.mark = ".", scientific = FALSE)

## =============================================================================
## 2. UTILIDADES
## =============================================================================

ms_manual <- function(estimate, std.error, term = "ATT", p.value = NULL, glance = NULL) {
  if (is.null(p.value)) p.value <- 2 * stats::pnorm(-abs(estimate / std.error))
  out <- list(
    tidy = data.frame(term = term, estimate = estimate,
                      std.error = std.error, p.value = p.value,
                      stringsAsFactors = FALSE),
    glance = if (is.null(glance)) data.frame(nobs = NA_integer_) else glance
  )
  class(out) <- "modelsummary_list"
  out
}

ms_desde_fixest <- function(m, term = "ATT") {
  ct <- as.data.frame(fixest::coeftable(m))
  ms_manual(estimate = ct[1, 1], std.error = ct[1, 2], term = term, p.value = ct[1, 4])
}

## Puente modelsummary -> data.frame -> tinytable.
construir_tabla <- function(modelos, filas_extra = NULL, titulo = NULL,
                            notas = NULL, dec = 2, ...) {

  d <- modelsummary(
    modelos, output = "data.frame",
    estimate = "{estimate}{stars}", statistic = "({std.error})",
    stars = ESTRELLAS, fmt = dec, ...
  )

  cols_modelo <- setdiff(names(d), c("part", "statistic", "term"))
  est <- d[d$part == "estimates", , drop = FALSE]
  gof <- d[d$part == "gof", , drop = FALSE]
  if (nrow(est) > 0) est$term[est$statistic != "estimate"] <- ""

  cuerpo <- rbind(est, gof)[, c("term", cols_modelo), drop = FALSE]

  ## Convencion castellana. Se aplica ANTES de pegar filas_extra porque esas
  ## filas ya vienen formateadas con mil() y volverlas a convertir estropearia
  ## el separador de millares.
  cuerpo[cols_modelo] <- lapply(cuerpo[cols_modelo], convertir_es)

  if (!is.null(filas_extra)) {
    filas_extra <- as.data.frame(filas_extra, stringsAsFactors = FALSE)
    names(filas_extra) <- names(cuerpo)
    cuerpo <- rbind(cuerpo, filas_extra)
  }

  names(cuerpo)[1] <- ""
  rownames(cuerpo) <- NULL

  tinytable::tt(cuerpo,
                caption = if (TITULO_EN_IMAGEN) titulo else NULL,
                notes = notas)
}

## =============================================================================
## 3. TABLA 1 -- ATT de Callaway y Sant'Anna, seis especificaciones
## =============================================================================

ruta_did <- file.path(OUTPUT_ROOT, "resultados_did_completos.rds")
res <- NULL

if (file.exists(ruta_did)) {
  res <- readRDS(ruta_did)

  orden <- c("alta_sin_cov", "alta_dr", "alta_matched",
             "todas_sin_cov", "todas_dr", "todas_matched")
  orden <- orden[orden %in% names(res)]
  orden <- orden[!vapply(res[orden], is.null, logical(1))]

  etiquetas_col <- c(
    alta_sin_cov  = "(1) Sin cov.", alta_dr  = "(2) DR", alta_matched  = "(3) PSM",
    todas_sin_cov = "(4) Sin cov.", todas_dr = "(5) DR", todas_matched = "(6) PSM"
  )

  modelos <- lapply(orden, function(k) {
    ms_manual(res[[k]]$simple$overall.att, res[[k]]$simple$overall.se, term = "ATT")
  })
  names(modelos) <- etiquetas_col[orden]

  n_muni <- vapply(orden, function(k) {
    n <- tryCatch(res[[k]]$att_gt$DIDparams$n, error = function(e) NULL)
    if (is.null(n) || length(n) != 1 || is.na(n)) "—" else mil(n)
  }, character(1))

  ## "Grupo de control" era identico en las seis columnas: como fila ocupaba
  ## dos lineas por el ajuste de texto sin aportar variacion. Va en la nota.
  filas_extra <- data.frame(
    term = c("Covariables de línea base", "Muestra", "Municipios"),
    rbind(
      ifelse(grepl("_dr$", orden), "Sí", "No"),
      ifelse(grepl("_matched$", orden), "Emparejada", "Completa"),
      n_muni
    ),
    stringsAsFactors = FALSE
  )

  titulo1 <- "Efecto promedio del tratamiento sobre los tratados (ATT) sobre la pérdida de bosque"

  tab1 <- construir_tabla(
    modelos, filas_extra, dec = 2, gof_omit = ".*", titulo = titulo1,
    notas = c(
      "Estimador de Callaway y Sant'Anna (2021), agregación simple. Variable dependiente: hectáreas de pérdida de bosque por municipio-año.",
      "Columnas (1)–(3): definición de tratamiento de alta confianza (Verra y Gold Standard). Columnas (4)–(6): todas las fuentes (añade RENARE y Cercarbono).",
      "Grupo de control en todas las especificaciones: municipios no tratados aún. Errores estándar bootstrap agrupados a nivel municipal, entre paréntesis. Panel 2001–2024.",
      "* p<0,1; ** p<0,05; *** p<0,01."
    )
  )

  ## Encabezados de grupo. Si group_tt() falla por versión, la nota al pie ya
  ## explica qué columnas corresponden a cada definición, así que la tabla
  ## sigue siendo interpretable.
  n_alta  <- sum(grepl("^alta",  orden))
  n_todas <- sum(grepl("^todas", orden))
  tab1 <- tryCatch(
    tinytable::group_tt(tab1, j = setNames(
      list(1 + seq_len(n_alta), 1 + n_alta + seq_len(n_todas)),
      c("Alta confianza", "Todas las fuentes")
    )),
    error = function(e) { cat("  Aviso: group_tt() no aplico los encabezados de grupo.\n"); tab1 }
  )

  guardar_png(formato_academico(tab1), "tabla_01_att_callaway_santanna.png", titulo1)

} else {
  cat("Aviso: no encuentro", ruta_did, "— corre 03_did_callaway_santanna.R. Se omite la Tabla 1.\n")
}

## =============================================================================
## 4. TABLA 2 -- TWFE frente a Callaway y Sant'Anna
## =============================================================================

ruta_panel <- file.path(OUTPUT_ROOT, "panel_analisis_did.rds")
panel <- NULL

if (file.exists(ruta_panel) && requireNamespace("fixest", quietly = TRUE)) {
  library(fixest)
  panel <- readRDS(ruta_panel)

  panel_twfe <- panel %>%
    mutate(
      tratado_alta  = as.integer(first_treat_alta  > 0 & year >= first_treat_alta),
      tratado_todas = as.integer(first_treat_todas > 0 & year >= first_treat_todas)
    )

  twfe_alta  <- feols(loss_area_ha ~ tratado_alta  | COD_DANE + year,
                      data = panel_twfe, cluster = ~COD_DANE)
  twfe_todas <- feols(loss_area_ha ~ tratado_todas | COD_DANE + year,
                      data = panel_twfe, cluster = ~COD_DANE)

  modelos2 <- list(
    "(1) Alta"  = ms_desde_fixest(twfe_alta,  "Efecto estimado"),
    "(2) Todas" = ms_desde_fixest(twfe_todas, "Efecto estimado")
  )
  estimadores <- c("TWFE", "TWFE")
  obs <- c(mil(nobs(twfe_alta)), mil(nobs(twfe_todas)))

  if (!is.null(res) && !is.null(res$alta_dr) && !is.null(res$todas_dr)) {
    modelos2[["(3) Alta"]]  <- ms_manual(res$alta_dr$simple$overall.att,
                                         res$alta_dr$simple$overall.se, "Efecto estimado")
    modelos2[["(4) Todas"]] <- ms_manual(res$todas_dr$simple$overall.att,
                                         res$todas_dr$simple$overall.se, "Efecto estimado")
    estimadores <- c(estimadores, "Callaway–Sant'Anna", "Callaway–Sant'Anna")
    obs <- c(obs, "—", "—")
  }

  ## CORRECCION SUSTANTIVA: Callaway-Sant'Anna NO estima efectos fijos
  ## bidireccionales; agrega comparaciones 2x2 grupo-tiempo. Marcar "Sí" en
  ## esas columnas sugeriria que ambos estimadores comparten especificacion,
  ## que es exactamente lo contrario de lo que la tabla quiere mostrar.
  es_twfe <- estimadores == "TWFE"
  fe_muni <- ifelse(es_twfe, "Sí", "No aplica")
  fe_anio <- ifelse(es_twfe, "Sí", "No aplica")

  ## La fila de R2 intra-grupo se elimina: con efectos fijos de municipio y
  ## año y un unico regresor binario, salia 0,000 y no aporta al argumento.
  filas_extra2 <- data.frame(
    term = c("Estimador", "Efectos fijos de municipio", "Efectos fijos de año",
             "Observaciones"),
    rbind(estimadores, fe_muni, fe_anio, obs),
    stringsAsFactors = FALSE
  )

  titulo2 <- "Comparación de estimadores: TWFE frente a Callaway y Sant'Anna"

  tab2 <- construir_tabla(
    modelos2, filas_extra2, dec = 2, gof_omit = ".*", titulo = titulo2,
    notas = c(
      "Variable dependiente: hectáreas de pérdida de bosque por municipio-año. Definición de tratamiento: alta confianza en (1) y (3); todas las fuentes en (2) y (4).",
      "TWFE estimado con fixest::feols(), errores estándar agrupados a nivel municipal. Callaway y Sant'Anna (2021) no estima efectos fijos bidireccionales: agrega comparaciones grupo-tiempo, por lo que esas filas no aplican.",
      "Los estimadores puntuales tienen signos opuestos, en la dirección que predice el sesgo de TWFE bajo adopción escalonada (Goodman-Bacon, 2021). Ninguna estimación es significativa a los niveles convencionales, por lo que la tabla documenta una discrepancia de signo, no una diferencia estadísticamente establecida entre estimadores.",
      "* p<0,1; ** p<0,05; *** p<0,01."
    )
  )
  guardar_png(formato_academico(tab2), "tabla_02_twfe_vs_cs.png", titulo2)

} else {
  cat("Aviso: falta", ruta_panel, "o el paquete fixest. Se omite la Tabla 2.\n")
}

## =============================================================================
## 5. TABLA 3 -- Modelo de puntaje de propensión (PSM)
## =============================================================================

rutas_psm <- c("Alta confianza" = file.path(OUTPUT_ROOT, "matching_alta_confianza.rds"),
               "Todas las fuentes" = file.path(OUTPUT_ROOT, "matching_todas_fuentes.rds"))

if (all(file.exists(rutas_psm))) {
  glms <- lapply(rutas_psm, function(r) readRDS(r)$matchit_obj$model)

  mapa <- c(
    baseline_forest_base = "Bosque de línea base (ha)",
    temp_media_c_base    = "Temperatura media (°C)",
    disbogota_base       = "Distancia a Bogotá (km)",
    H_coca_base          = "Hectáreas de coca",
    homicidios_base      = "Homicidios",
    `(Intercept)`        = "Constante"
  )

  titulo3 <- "Modelo de puntaje de propensión"

  ## gof_map SI funciona cuando se pasa en su formato documentado: un
  ## data.frame con columnas raw/clean/fmt. Lo que rompia era gof_map = NA
  ## (vector atomico). Asi se traducen "Num.Obs." y "Log.Lik." al castellano
  ## y se eligen los estadisticos sin recurrir a una regex de descarte.
  gof_es <- data.frame(
    raw   = c("nobs", "aic", "logLik"),
    clean = c("Observaciones", "AIC", "Log-verosimilitud"),
    fmt   = c(0, 1, 2),
    stringsAsFactors = FALSE
  )

  tab3 <- construir_tabla(
    glms, dec = FMT_SIG(3), coef_map = mapa,
    gof_map = gof_es, titulo = titulo3,
    notas = c(
      "Regresión logística de la probabilidad de recibir tratamiento sobre covariables de línea base.",
      "Los coeficientes se presentan con tres cifras significativas: las variables están en sus unidades originales (hectáreas, kilómetros), por lo que las magnitudes por unidad son necesariamente pequeñas.",
      "Errores estándar entre paréntesis. * p<0,1; ** p<0,05; *** p<0,01."
    )
  )
  guardar_png(formato_academico(tab3), "tabla_03_psm_propension.png", titulo3)

} else {
  cat("Aviso: faltan los .rds de matching. Corre 02_matching_psm.R. Se omite la Tabla 3.\n")
}

## =============================================================================
## 6. TABLA 4 -- Estadísticas descriptivas
##
## OJO: verifica las unidades de 'homicidios_base' y 'tasa_deforestacion'
## contra 01_preparar_datos_did.R. Con maximo 2.118, homicidios_base parece un
## conteo anual, no una tasa por cien mil. Y si la tasa de deforestacion tiene
## maximo 0,17, esta en proporcion, no en puntos porcentuales: en ese caso el
## sufijo "(%)" es incorrecto y hay que decidir una escala unica para toda la
## tesis, porque los efectos regionales que reportas (-0,177) estan en la otra.
## =============================================================================

if (!is.null(panel)) {
  mapa_desc <- c(
    loss_area_ha         = "Pérdida de bosque (ha)",
    tasa_deforestacion   = "Tasa de deforestación",      # sin "%" hasta verificar la escala
    baseline_forest_base = "Bosque de línea base (ha)",
    temp_media_c_base    = "Temperatura media (°C)",
    disbogota_base       = "Distancia a Bogotá (km)",
    H_coca_base          = "Hectáreas de coca",
    homicidios_base      = "Homicidios"                   # sin "(tasa)" hasta verificar
  )
  vars <- intersect(names(mapa_desc), names(panel))
  d <- as.data.frame(panel[, vars, drop = FALSE])
  names(d) <- unname(mapa_desc[vars])

  desc <- datasummary(All(d) ~ Mean + SD + Min + Median + Max + N,
                      data = d, output = "data.frame", fmt = 2)

  ## Encabezados al castellano (venian como Mean/SD/Min/Median/Max/N).
  if (ncol(desc) == 7) {
    names(desc) <- c(" ", "Media", "Desv. típica", "Mínimo", "Mediana", "Máximo", "N")
  }
  desc[-1] <- lapply(desc[-1], convertir_es)

  ## Nota dinamica sobre las observaciones faltantes de la tasa.
  nota_na <- ""
  if ("tasa_deforestacion" %in% names(panel)) {
    n_na <- sum(is.na(panel$tasa_deforestacion))
    if (n_na > 0) {
      nota_na <- paste0(
        "La tasa de deforestación registra ", mil(n_na), " observaciones faltantes, ",
        "correspondientes a municipios-año sin bosque de línea base, donde la tasa es indefinida."
      )
    }
  }

  titulo4 <- "Estadísticas descriptivas del panel municipal"
  notas4 <- c("Unidad de observación: municipio-año. Panel de 1.122 municipios, 2001–2024. Elaboración propia.")
  if (nzchar(nota_na)) notas4 <- c(notas4, nota_na)

  tab4 <- tinytable::tt(desc,
                        caption = if (TITULO_EN_IMAGEN) titulo4 else NULL,
                        notes = notas4)
  guardar_png(formato_academico(tab4), "tabla_04_descriptivas.png", titulo4)
}

## =============================================================================
## 7. TABLAS DESDE CSV YA CALCULADOS
## =============================================================================

csv_a_png <- function(ruta_csv, archivo_png, titulo, nota = NULL, renombrar = NULL, dec = 2) {
  if (!file.exists(ruta_csv)) {
    cat("Aviso: no encuentro", ruta_csv, "— se omite", archivo_png, "\n")
    return(invisible(NULL))
  }
  d <- read.csv(ruta_csv, stringsAsFactors = FALSE, check.names = FALSE)
  d <- formatear_columnas(d, dec = dec)
  if (!is.null(renombrar)) {
    idx <- names(d) %in% names(renombrar)
    names(d)[idx] <- unname(renombrar[names(d)[idx]])
  }

  ## Sin format_tt(): las columnas ya vienen formateadas por
  ## formatear_columnas(). format_tt(digits=) cuenta cifras significativas y
  ## degradaria la precision (-163,906 se convertiria en -163,9).
  tab <- tinytable::tt(d,
                       caption = if (TITULO_EN_IMAGEN) titulo else NULL,
                       notes = nota)
  guardar_png(formato_academico(tab), archivo_png, titulo)
}

csv_a_png(
  file.path(DIR_TABLAS, "resumen_robustez_att.csv"),
  "tabla_05_robustez_att.png",
  "Robustez del ATT a través de especificaciones",
  "Intervalos de confianza al 95%. Elaboración propia.",
  renombrar = c(especificacion = "Especificación", att = "ATT", se = "Error estándar",
                ic_inf = "IC inferior", ic_sup = "IC superior")
)

csv_a_png(
  file.path(DIR_TABLAS, "sun_abraham_resultados.csv"),
  "tabla_06_sun_abraham.png",
  "Estimador de Sun y Abraham (2021)",
  "Agregación simple sobre todos los periodos posteriores al tratamiento. Elaboración propia.",
  renombrar = c(estimador = "Estimador", definicion = "Definición de tratamiento",
                att = "ATT", se = "Error estándar")
)

csv_a_png(
  file.path(DIR_TABLAS, "sensibilidad_cohortes_tempranas.csv"),
  "tabla_07_sensibilidad_cohortes.png",
  "Sensibilidad a la exclusión de cohortes tempranas",
  "Elaboración propia.",
  renombrar = c(especificacion = "Especificación", att = "ATT", se = "Error estándar",
                n_excluidos = "Excluidos (n)")
)

cat("\nListo. Imágenes en:", normalizePath(DIR_IMG, mustWork = FALSE), "\n")
if (!TITULO_EN_IMAGEN) {
  cat("Los títulos NO están dentro de los PNG: úsalos como título de tabla en Word.\n")
}
