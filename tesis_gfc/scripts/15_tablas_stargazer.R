## =============================================================================
## 15_tablas_stargazer.R
##
## Genera las tablas de la tesis en formato academico estandar usando
## stargazer, y las exporta como IMAGENES PNG (300 dpi) listas para insertar
## en el documento Word.
##
## POR QUE ESTE SCRIPT EXISTE
## --------------------------
## stargazer NO sabe leer objetos AGGTEobj (did::aggte) ni fixest::feols.
## La solucion estandar en la literatura aplicada es construir un "modelo
## fantasma" (un lm() con la estructura de columnas deseada) y sobrescribir
## sus coeficientes, errores estandar y valores p con los reales, usando los
## argumentos coef=, se=, p= de stargazer con t.auto=FALSE y p.auto=FALSE.
## Los numeros impresos son los reales; el lm() solo aporta el esqueleto.
##
## stargazer tampoco produce imagenes: produce LaTeX o HTML. Este script
## incluye un motor de render que compila la salida a PDF y la rasteriza a
## PNG (ruta LaTeX, preferida) o la captura desde HTML con Chrome headless
## (ruta de respaldo, si no hay LaTeX instalado).
##
## REQUISITOS
## ----------
##   install.packages(c("stargazer", "pdftools"))
##   # Ruta LaTeX (recomendada, salida identica a un paper):
##   install.packages("tinytex"); tinytex::install_tinytex()
##   # Ruta de respaldo (si no quieres instalar LaTeX):
##   install.packages("webshot2")   # requiere Chrome/Edge instalado
##
## Requiere haber corrido 01, 02, 03 y (opcionalmente) 13.
##
## USO
## ---
##   Rscript 15_tablas_stargazer.R
## =============================================================================

library(dplyr)
library(stargazer)

OUTPUT_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs"
DIR_TABLAS <- file.path(OUTPUT_ROOT, "tablas")
DIR_IMG    <- file.path(OUTPUT_ROOT, "figuras/tablas")
DPI        <- 300

dir.create(DIR_TABLAS, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_IMG, showWarnings = FALSE, recursive = TRUE)

## =============================================================================
## 1. MOTOR DE RENDER: stargazer -> PNG
## =============================================================================

detectar_motor <- function() {
  hay_latex <- (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()) ||
    nzchar(Sys.which("pdflatex"))
  hay_pdftools <- requireNamespace("pdftools", quietly = TRUE)
  if (hay_latex && hay_pdftools) return("latex")
  if (requireNamespace("webshot2", quietly = TRUE)) return("html")
  stop("No hay motor de render disponible. Instala tinytex + pdftools, o webshot2.")
}

PREAMBULO_TEX <- c(
  "\\documentclass[border=12pt,varwidth=22cm]{standalone}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[T1]{fontenc}",
  "\\usepackage[spanish]{babel}",
  "\\usepackage{mathptmx}",   # Times New Roman (clon), registro academico
  "\\usepackage{booktabs}",
  "\\usepackage{dcolumn}",
  "\\usepackage{multirow}",
  "\\begin{document}"
)

CSS_HTML <- paste(
  "<style>",
  "body { margin: 0; padding: 24px; background: #ffffff; }",
  "table { font-family: 'Times New Roman', Times, serif; font-size: 15px;",
  "        border-collapse: collapse; color: #000; }",
  "table td, table th { padding: 4px 12px; }",
  "</style>", sep = "\n"
)

#' Renderiza una llamada a stargazer como PNG.
#'
#' @param args lista de argumentos para stargazer (SIN 'type' ni 'out')
#' @param archivo nombre del PNG (sin ruta); se guarda en DIR_IMG
#' @param guardar_fuente si TRUE, guarda tambien el .tex/.html fuente
tabla_png <- function(args, archivo, guardar_fuente = TRUE) {
  motor <- detectar_motor()
  destino <- file.path(DIR_IMG, archivo)
  dir_tmp <- file.path(tempdir(), paste0("sg_", as.integer(runif(1, 1, 1e6))))
  dir.create(dir_tmp, showWarnings = FALSE, recursive = TRUE)
  wd0 <- getwd()
  on.exit(setwd(wd0), add = TRUE)

  if (motor == "latex") {
    ## float = FALSE -> devuelve solo el tabular, sin \begin{table}, que es
    ## lo unico que admite la clase standalone.
    cuerpo <- utils::capture.output(
      do.call(stargazer, c(args, list(type = "latex", float = FALSE, header = FALSE)))
    )
    tex <- c(PREAMBULO_TEX, cuerpo, "\\end{document}")
    ruta_tex <- file.path(dir_tmp, "tabla.tex")
    writeLines(tex, ruta_tex, useBytes = TRUE)

    setwd(dir_tmp)
    if (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()) {
      tinytex::latexmk("tabla.tex", engine = "pdflatex")
    } else {
      tools::texi2pdf("tabla.tex", clean = TRUE)
    }
    setwd(wd0)

    pdftools::pdf_convert(
      file.path(dir_tmp, "tabla.pdf"),
      format = "png", dpi = DPI, filenames = destino, verbose = FALSE
    )
    if (guardar_fuente) file.copy(ruta_tex, file.path(DIR_TABLAS, sub("\\.png$", ".tex", archivo)), overwrite = TRUE)

  } else {
    cuerpo <- utils::capture.output(
      do.call(stargazer, c(args, list(type = "html", header = FALSE)))
    )
    ruta_html <- file.path(dir_tmp, "tabla.html")
    writeLines(c(CSS_HTML, cuerpo), ruta_html, useBytes = TRUE)
    webshot2::webshot(paste0("file://", normalizePath(ruta_html)),
                      file = destino, selector = "table", zoom = 4)
    if (guardar_fuente) file.copy(ruta_html, file.path(DIR_TABLAS, sub("\\.png$", ".html", archivo)), overwrite = TRUE)
  }

  cat("Tabla guardada:", destino, sprintf("(motor: %s)\n", motor))
  invisible(destino)
}

## =============================================================================
## 2. UTILIDADES: modelo fantasma e inyeccion de resultados reales
## =============================================================================

#' Crea un lm() vacio con los nombres de coeficiente pedidos.
#' Solo sirve como esqueleto: sus numeros nunca se imprimen.
modelo_fantasma <- function(nombres_coef) {
  set.seed(20260903)
  n <- 60
  d <- as.data.frame(matrix(rnorm(n * length(nombres_coef)), nrow = n))
  names(d) <- nombres_coef
  d$y <- rnorm(n)
  lm(stats::reformulate(nombres_coef, response = "y", intercept = FALSE), data = d)
}

#' p-valor normal a dos colas (consistente con las bandas del paquete did,
#' que son asintoticas, no t de Student).
p_normal <- function(est, se) 2 * stats::pnorm(-abs(est / se))

## =============================================================================
## 3. TABLA 1 -- ATT de Callaway y Sant'Anna, seis especificaciones
## =============================================================================

ruta_did <- file.path(OUTPUT_ROOT, "resultados_did_completos.rds")
if (file.exists(ruta_did)) {
  res <- readRDS(ruta_did)

  orden <- c("alta_sin_cov", "alta_dr", "alta_matched",
             "todas_sin_cov", "todas_dr", "todas_matched")
  orden <- orden[orden %in% names(res)]
  orden <- orden[!sapply(res[orden], is.null)]

  att <- sapply(orden, function(k) res[[k]]$simple$overall.att)
  se  <- sapply(orden, function(k) res[[k]]$simple$overall.se)
  pv  <- p_normal(att, se)

  ## vapply(), no sapply(): si DIDparams$n no existe, format(NULL) devuelve
  ## character(0) sin error y sapply() devolveria una LISTA, lo que rompe
  ## add.lines mas adelante de forma dificil de diagnosticar.
  n_muni <- vapply(orden, function(k) {
    n <- tryCatch(res[[k]]$att_gt$DIDparams$n, error = function(e) NULL)
    if (is.null(n) || length(n) != 1 || is.na(n)) "--" else format(n, big.mark = ",")
  }, character(1))

  fantasma <- modelo_fantasma("ATT")
  modelos <- rep(list(fantasma), length(orden))

  args_t1 <- list(
    modelos,
    coef = unname(lapply(att, function(x) c(ATT = x))),
    se   = unname(lapply(se,  function(x) c(ATT = x))),
    p    = unname(lapply(pv,  function(x) c(ATT = x))),
    t.auto = FALSE, p.auto = FALSE,
    omit.table.layout = "s",   # elimina el bloque de estadisticos del modelo fantasma
    dep.var.caption  = "Variable dependiente: perdida de bosque (hectareas por municipio-anio)",
    dep.var.labels.include = FALSE,
    column.labels = c("Alta confianza", "Todas las fuentes"),
    column.separate = c(sum(grepl("^alta", orden)), sum(grepl("^todas", orden))),
    covariate.labels = "ATT",
    model.numbers = TRUE,
    ## as.character() + unname(): stargazer exige vectores de caracteres planos
    ## de longitud (n_columnas + 1). Un vector con nombres o una lista rompe.
    add.lines = list(
      unname(as.character(c("Covariables de linea base", ifelse(grepl("_dr$", orden), "Si", "No")))),
      unname(as.character(c("Muestra", ifelse(grepl("_matched$", orden), "PSM", "Completa")))),
      unname(as.character(c("Grupo de control", rep("No tratados aun", length(orden))))),
      unname(as.character(c("Municipios", n_muni)))
    ),
    star.cutoffs = c(0.10, 0.05, 0.01),
    star.char = c("*", "**", "***"),
    digits = 2, align = TRUE,
    notes.align = "l", notes.append = FALSE,
    notes = paste(
      "Estimador de Callaway y Sant'Anna (2021), agregacion simple.",
      "Errores estandar bootstrap clusterizados por municipio entre parentesis.",
      "Panel 2001-2024. * p<0.1; ** p<0.05; *** p<0.01."
    ),
    title = "Efecto promedio del tratamiento sobre los tratados (ATT)"
  )

  tabla_png(args_t1, "tabla_01_att_callaway_santanna.png")
} else {
  cat("Aviso: no encuentro", ruta_did, "- corre 03_did_callaway_santanna.R. Se omite la Tabla 1.\n")
}

## =============================================================================
## 4. TABLA 2 -- Comparacion TWFE vs. Callaway-Sant'Anna
##
## El contraste de signo es el argumento empirico central para justificar el
## estimador escalonado (Goodman-Bacon 2021), asi que la tabla los pone lado
## a lado en la misma escala.
## =============================================================================

ruta_panel <- file.path(OUTPUT_ROOT, "panel_analisis_did.rds")
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

  extraer_fixest <- function(m) {
    ct <- as.data.frame(coeftable(m))
    list(est = ct[1, 1], se = ct[1, 2], p = ct[1, 4],
         n = format(nobs(m), big.mark = ","),
         r2w = sprintf("%.3f", fixest::r2(m, "wr2")))
  }
  a <- extraer_fixest(twfe_alta); b <- extraer_fixest(twfe_todas)

  fantasma <- modelo_fantasma("Tratado")

  ## Columnas 3 y 4: los ATT de Callaway-Sant'Anna (especificacion DR) para
  ## el contraste directo, si el objeto de resultados esta disponible.
  cs_disponible <- exists("res") && !is.null(res$alta_dr) && !is.null(res$todas_dr)

  ests <- list(c(Tratado = a$est), c(Tratado = b$est))
  ses  <- list(c(Tratado = a$se),  c(Tratado = b$se))
  ps   <- list(c(Tratado = a$p),   c(Tratado = b$p))
  etiquetas_est <- c("TWFE", "TWFE")
  ns <- c(a$n, b$n)
  r2s <- c(a$r2w, b$r2w)

  if (cs_disponible) {
    for (k in c("alta_dr", "todas_dr")) {
      e <- res[[k]]$simple$overall.att; s <- res[[k]]$simple$overall.se
      ests <- c(ests, list(c(Tratado = e)))
      ses  <- c(ses,  list(c(Tratado = s)))
      ps   <- c(ps,   list(c(Tratado = p_normal(e, s))))
    }
    etiquetas_est <- c(etiquetas_est, "Callaway-Sant'Anna", "Callaway-Sant'Anna")
    ns <- c(ns, "--", "--"); r2s <- c(r2s, "--", "--")
  }

  args_t2 <- list(
    rep(list(fantasma), length(ests)),
    coef = ests, se = ses, p = ps,
    t.auto = FALSE, p.auto = FALSE,
    omit.table.layout = "s",
    dep.var.caption = "Variable dependiente: perdida de bosque (hectareas)",
    dep.var.labels.include = FALSE,
    column.labels = if (cs_disponible)
      c("Alta", "Todas", "Alta", "Todas") else c("Alta", "Todas"),
    covariate.labels = "Efecto estimado",
    add.lines = lapply(list(
      c("Estimador", etiquetas_est),
      c("Efectos fijos de municipio", rep("Si", length(ests))),
      c("Efectos fijos de anio", rep("Si", length(ests))),
      c("Observaciones", ns),
      c("R2 intra-grupo", r2s)
    ), function(x) unname(as.character(x))),
    star.cutoffs = c(0.10, 0.05, 0.01), star.char = c("*", "**", "***"),
    digits = 2, align = TRUE,
    notes.align = "l", notes.append = FALSE,
    notes = paste(
      "TWFE estimado con fixest::feols(); errores estandar clusterizados por municipio.",
      "La discrepancia de signo frente al estimador escalonado documenta el sesgo",
      "de TWFE bajo adopcion escalonada (Goodman-Bacon, 2021).",
      "* p<0.1; ** p<0.05; *** p<0.01."
    ),
    title = "Comparacion de estimadores: TWFE frente a Callaway y Sant'Anna"
  )

  tabla_png(args_t2, "tabla_02_twfe_vs_cs.png")
} else {
  cat("Aviso: falta", ruta_panel, "o el paquete fixest. Se omite la Tabla 2.\n")
}

## =============================================================================
## 5. TABLA 3 -- Modelo de propension (PSM). stargazer lee glm nativamente,
## asi que aqui NO hace falta el modelo fantasma.
## =============================================================================

rutas_psm <- c("Alta confianza" = file.path(OUTPUT_ROOT, "matching_alta_confianza.rds"),
               "Todas las fuentes" = file.path(OUTPUT_ROOT, "matching_todas_fuentes.rds"))

if (all(file.exists(rutas_psm))) {
  glms <- lapply(rutas_psm, function(r) readRDS(r)$matchit_obj$model)

  args_t3 <- list(
    glms,
    dep.var.caption = "Variable dependiente: probabilidad de recibir tratamiento",
    dep.var.labels.include = FALSE,
    column.labels = names(rutas_psm),
    covariate.labels = c("Bosque de linea base (ha)", "Temperatura media (C)",
                         "Distancia a Bogota (km)", "Hectareas de coca",
                         "Homicidios (tasa)"),
    keep.stat = c("n", "ll", "aic"),
    star.cutoffs = c(0.10, 0.05, 0.01), star.char = c("*", "**", "***"),
    digits = 3, align = TRUE,
    notes.align = "l", notes.append = FALSE,
    notes = paste("Regresion logistica del puntaje de propension.",
                  "Errores estandar entre parentesis. * p<0.1; ** p<0.05; *** p<0.01."),
    title = "Modelo de puntaje de propension (PSM)"
  )

  ## Si los nombres de las covariables no coinciden con covariate.labels,
  ## stargazer aborta: en ese caso, quita el argumento covariate.labels.
  tryCatch(tabla_png(args_t3, "tabla_03_psm_propension.png"),
           error = function(e) {
             cat("Reintentando la Tabla 3 sin etiquetas manuales de covariables...\n")
             args_t3$covariate.labels <- NULL
             tabla_png(args_t3, "tabla_03_psm_propension.png")
           })
} else {
  cat("Aviso: faltan los .rds de matching. Corre 02_matching_psm.R. Se omite la Tabla 3.\n")
}

## =============================================================================
## 6. TABLA 4 -- Estadisticas descriptivas del panel
## =============================================================================

if (exists("panel")) {
  vars_desc <- intersect(
    c("loss_area_ha", "tasa_deforestacion", "baseline_forest_base",
      "temp_media_c_base", "disbogota_base", "H_coca_base", "homicidios_base"),
    names(panel)
  )
  d <- as.data.frame(panel[, vars_desc, drop = FALSE])

  args_t4 <- list(
    d, summary = TRUE,
    covariate.labels = c("Perdida de bosque (ha)", "Tasa de deforestacion (\\%)",
                         "Bosque de linea base (ha)", "Temperatura media (C)",
                         "Distancia a Bogota (km)", "Hectareas de coca",
                         "Homicidios (tasa)")[seq_along(vars_desc)],
    digits = 2, align = TRUE,
    notes.align = "l", notes.append = FALSE,
    notes = "Panel de 1.122 municipios, 2001-2024. Elaboracion propia.",
    title = "Estadisticas descriptivas"
  )
  tryCatch(tabla_png(args_t4, "tabla_04_descriptivas.png"),
           error = function(e) {
             args_t4$covariate.labels <- NULL
             tabla_png(args_t4, "tabla_04_descriptivas.png")
           })
}

## =============================================================================
## 7. TABLA 5 -- Cualquier data.frame ya calculado (robustez, heterogeneidad,
## spillovers, Sun-Abraham). stargazer imprime data.frames con summary=FALSE.
## =============================================================================

csv_a_tabla <- function(ruta_csv, archivo_png, titulo, nota = "") {
  if (!file.exists(ruta_csv)) {
    cat("Aviso: no encuentro", ruta_csv, "- se omite", archivo_png, "\n"); return(invisible(NULL))
  }
  d <- read.csv(ruta_csv, stringsAsFactors = FALSE, check.names = FALSE)
  ## Redondear numericos para que la tabla no salga con 8 decimales
  d[] <- lapply(d, function(x) if (is.numeric(x)) round(x, 3) else x)
  args <- list(
    d, summary = FALSE, rownames = FALSE,
    digits = 3, align = FALSE,
    notes.align = "l", notes.append = FALSE,
    notes = nota, title = titulo
  )
  tabla_png(args, archivo_png)
}

csv_a_tabla(file.path(DIR_TABLAS, "resumen_robustez_att.csv"),
            "tabla_05_robustez_att.png",
            "Robustez del ATT a traves de especificaciones",
            "Intervalos de confianza al 95\\%. Elaboracion propia.")

csv_a_tabla(file.path(DIR_TABLAS, "sun_abraham_resultados.csv"),
            "tabla_06_sun_abraham.png",
            "Estimador de Sun y Abraham (2021)",
            "Comparacion con Callaway-Sant'Anna y TWFE.")

csv_a_tabla(file.path(DIR_TABLAS, "sensibilidad_cohortes_tempranas.csv"),
            "tabla_07_sensibilidad_cohortes.png",
            "Sensibilidad a la exclusion de cohortes tempranas")

cat("\nListo. Imagenes en:", normalizePath(DIR_IMG, mustWork = FALSE), "\n")
