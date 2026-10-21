## =============================================================================
## 70_tablas_figuras.R
##
## Tablas y figuras de la tesis a partir de las salidas de resultados-v1.
## No estima nada: solo lee outputs/tablas/*.csv y outputs/*.rds.
##
## TABLAS (estilo AER): cada tabla sale en dos formatos
##   outputs/tesis/tablas/tabla_X_Y.tex   booktabs + threeparttable, sin lineas
##                                        verticales, errores estandar entre
##                                        parentesis debajo del estimado y sin
##                                        asteriscos de significancia (politica
##                                        de la AER desde 2016).
##   outputs/tesis/tablas/tabla_X_Y.png   la misma tabla como imagen (300 dpi).
##   Numeros en convencion espanola: coma decimal y punto de miles (1.009,5).
##   Preambulo LaTeX necesario: outputs/tesis/tablas/preambulo.tex
##
## FIGURAS: outputs/tesis/figuras/figura_X_Y.png (300 dpi) y .pdf (cairo,
##   para que las tildes salgan bien). Escala de grises, fuente con serifas,
##   sin titulo dentro de la figura (el titulo va en el documento).
##
## Cada producto se genera por separado: si uno falla (falta un insumo), el
## script avisa y sigue con los demas. Al final imprime un resumen.
##
## Paquetes: dplyr, ggplot2, gridExtra, gtable, scales; sf (solo el mapa);
##           ragg (opcional, mejor calidad de PNG).
## USO:  Rscript scripts/70_tablas_figuras.R
## =============================================================================
source("scripts/00_config_did.R")   # B.1: parametros comunes (no redefinir aqui)
for (p in c("dplyr", "ggplot2", "gridExtra", "gtable", "scales"))
  if (!requireNamespace(p, quietly = TRUE)) stop("Falta el paquete ", p, ": install.packages('", p, "')")
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(grid) })

DIR_T <- file.path(DIR_OUT, "tesis", "tablas"); DIR_F <- file.path(DIR_OUT, "tesis", "figuras")
for (d in c(DIR_T, DIR_F)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
FUENTE <- "serif"
MENOS <- "\u2212"                      # signo menos tipografico
TAB <- function(f) file.path(DIR_TAB, f)
leer <- function(f) {
  r <- TAB(f); if (!file.exists(r)) stop("falta ", r)
  read.csv(r, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
}
N_CONTROLES_D1_ORIGINAL <- REF_N_CONTROLES + 1L   # Calamar (95015): unico ambiguo que era control

## -----------------------------------------------------------------------------
## 1. Formato numerico en espanol
## -----------------------------------------------------------------------------
num <- function(x, d = 1) {
  x <- suppressWarnings(as.numeric(x))
  s <- formatC(abs(x), format = "f", digits = d, big.mark = ".", decimal.mark = ",")
  neg <- !is.na(x) & round(x, d) < 0
  s[neg] <- paste0(MENOS, s[neg]); s[is.na(x)] <- ""; s
}
ee  <- function(x, d = 1) ifelse(is.na(suppressWarnings(as.numeric(x))), "", paste0("(", num(x, d), ")"))
ic  <- function(a, b, d = 0) ifelse(is.na(a) | is.na(b), "", paste0("[", num(a, d), "; ", num(b, d), "]"))
ent <- function(x) { x <- suppressWarnings(as.numeric(x)); ifelse(is.na(x), "", formatC(x, format = "d", big.mark = ".", decimal.mark = ",")) }
pct <- function(x, d = 1) ifelse(is.na(suppressWarnings(as.numeric(x))), "", paste0(num(x, d), " %"))
## Intervalos que vienen como texto "[-331.0 ; 226.0]" (HonestDiD)
ic_txt <- function(s, d = 0) {
  v <- regmatches(s, gregexpr("-?[0-9]+(\\.[0-9]+)?", s))
  vapply(v, function(z) if (length(z) == 2) ic(as.numeric(z[1]), as.numeric(z[2]), d) else "", "")
}

## -----------------------------------------------------------------------------
## 2. Motor de tablas: una estructura, dos salidas (LaTeX y PNG)
##    filas: data.frame con 'etq', columnas de valores y 'tipo' en
##           dato | ee | panel | sep (linea intermedia antes de la fila)
## -----------------------------------------------------------------------------
fila <- function(etq, valores, tipo = "dato") {
  data.frame(etq = etq, t(setNames(as.character(valores), paste0("v", seq_along(valores)))),
             tipo = tipo, stringsAsFactors = FALSE, check.names = FALSE)
}
panel <- function(texto, k) fila(texto, rep("", k), "panel")
apilar <- function(...) do.call(rbind, list(...))

lx <- function(s) {
  s <- gsub("&", "\\&", s, fixed = TRUE); s <- gsub("%", "\\%", s, fixed = TRUE)
  s <- gsub("_", "\\_", s, fixed = TRUE); s <- gsub("#", "\\#", s, fixed = TRUE)
  s <- gsub("M\u0304", "$\\bar{M}$", s, fixed = TRUE)
  s <- gsub(MENOS, "$-$", s, fixed = TRUE); s <- gsub("\u2265", "$\\geq$", s, fixed = TRUE)
  s <- gsub("\u2013", "--", s, fixed = TRUE); s <- gsub("\u2026", "\\ldots{}", s, fixed = TRUE)
  s
}

escribir_latex <- function(t) {
  k <- length(t$cab[[1]])
  L <- c("% Generado por scripts/70_tablas_figuras.R a partir de resultados-v1. No editar a mano.",
         "\\begin{table}[htbp]", "\\centering", "\\small",
         sprintf("\\caption{%s}", lx(t$titulo)), sprintf("\\label{tab:%s}", t$id),
         "\\begin{threeparttable}",
         sprintf("\\begin{tabular}{@{}l%s@{}}", paste(rep(if (is.null(t$alin)) "c" else t$alin, length.out = k), collapse = "")),
         "\\toprule")
  for (h in t$cab) L <- c(L, paste0(paste(c("", lx(h)), collapse = " & "), " \\\\"))
  L <- c(L, "\\midrule")
  for (i in seq_len(nrow(t$filas))) {
    f <- t$filas[i, ]; v <- unlist(f[grep("^v[0-9]+$", names(f))])
    if (f$tipo == "sep") L <- c(L, "\\midrule")
    if (f$tipo == "panel") {
      if (i > 1) L <- c(L, "\\addlinespace")
      L <- c(L, sprintf("\\multicolumn{%d}{@{}l}{\\textit{%s}} \\\\", k + 1, lx(f$etq)))
    } else {
      L <- c(L, paste0(paste(c(lx(f$etq), lx(v)), collapse = " & "), " \\\\"))
    }
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}",
         "\\begin{tablenotes}[flushleft]", "\\footnotesize",
         sprintf("\\item \\textit{Notas:} %s", lx(t$notas)))
  if (!is.null(t$fuente)) L <- c(L, sprintf("\\item \\textit{Fuente:} %s", lx(t$fuente)))
  L <- c(L, "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  writeLines(enc2utf8(L), file.path(DIR_T, paste0(t$id, ".tex")), useBytes = TRUE)
}

escribir_png <- function(t) {
  k <- length(t$cab[[1]]); f <- t$filas
  ## En PNG la barra combinada (U+0304) no se dibuja bien con Times: se usa
  ## la sobrelinea combinada (U+0305), que si queda sobre la M.
  barra <- function(x) gsub("M\u0304", "M\u0305", x, fixed = TRUE)
  f[] <- lapply(f, barra); t$cab <- lapply(t$cab, barra); t$notas <- barra(t$notas)
  cuerpo <- as.matrix(f[, c("etq", paste0("v", seq_len(k)))])
  m <- rbind(do.call(rbind, lapply(t$cab, function(h) c("", h))), cuerpo)
  nh <- length(t$cab); n <- nrow(m)
  cara <- matrix("plain", n, k + 1); cara[nh + which(f$tipo == "panel"), 1] <- "italic"
  hj <- matrix(rep(c(0, rep(0.5, k)), each = n), n, k + 1)
  tema <- gridExtra::ttheme_minimal(base_size = 10, base_family = FUENTE,
    core = list(fg_params = list(hjust = as.vector(hj), x = as.vector(ifelse(hj == 0, 0.02, 0.5)),
                                 fontface = as.vector(cara)),
                bg_params = list(fill = NA), padding = unit(c(5, 2.2), "mm")))
  g <- gridExtra::tableGrob(m, rows = NULL, cols = NULL, theme = tema)
  linea <- function(g, fila, arriba = TRUE, lwd = 0.8)
    gtable::gtable_add_grob(g, segmentsGrob(0, as.numeric(arriba), 1, as.numeric(arriba),
                            gp = gpar(lwd = lwd)), t = fila, b = fila, l = 1, r = k + 1, z = Inf,
                            name = paste0("regla", fila, arriba, runif(1)))
  g <- linea(g, 1, TRUE, 1.2); g <- linea(g, nh, FALSE); g <- linea(g, n, FALSE, 1.2)
  for (i in which(f$tipo == "sep")) g <- linea(g, nh + i, TRUE)
  ancho <- convertWidth(sum(g$widths), "in", valueOnly = TRUE)
  alto  <- convertHeight(sum(g$heights), "in", valueOnly = TRUE)
  envolver <- function(s, ancho_in) paste(strwrap(s, width = max(40, floor(ancho_in * 17.5))), collapse = "\n")
  txt_notas <- envolver(paste0("Notas: ", t$notas), ancho)
  if (!is.null(t$fuente)) txt_notas <- paste0(txt_notas, "\n", envolver(paste0("Fuente: ", t$fuente), ancho))
  notas <- textGrob(txt_notas, x = 0, hjust = 0, gp = gpar(fontsize = 8, fontfamily = FUENTE, lineheight = 1.15))
  titulo <- textGrob(t$titulo, gp = gpar(fontsize = 11, fontfamily = FUENTE))
  h_notas <- convertHeight(grobHeight(notas), "in", valueOnly = TRUE) + 0.15
  todo <- gridExtra::arrangeGrob(titulo, g, notas, ncol = 1,
                                 heights = unit(c(0.4, alto, h_notas), "in"))
  ruta <- file.path(DIR_T, paste0(t$id, ".png"))
  if (requireNamespace("ragg", quietly = TRUE))
    ragg::agg_png(ruta, width = ancho + 0.4, height = alto + h_notas + 0.6, units = "in", res = 300)
  else png(ruta, width = ancho + 0.4, height = alto + h_notas + 0.6, units = "in", res = 300, type = "cairo")
  grid.newpage(); pushViewport(viewport(width = unit(ancho, "in"))); grid.draw(todo); dev.off()
}

productos <- list()
hacer <- function(id, expr) {
  r <- tryCatch({ expr; "ok" }, error = function(e) paste("ERROR:", conditionMessage(e)))
  productos[[id]] <<- r
  cat(sprintf("  %-22s %s\n", id, r))
}
tabla <- function(t) { escribir_latex(t); escribir_png(t) }

## -----------------------------------------------------------------------------
## 3. Tema de figuras (AER: sobrio, escala de grises, serifas)
## -----------------------------------------------------------------------------
tema_aer <- theme_classic(base_size = 10, base_family = FUENTE) +
  theme(axis.line = element_line(linewidth = 0.3), axis.ticks = element_line(linewidth = 0.3),
        legend.position = "bottom", legend.title = element_blank(),
        strip.background = element_blank(), strip.text = element_text(face = "italic"),
        plot.title = element_blank(), plot.margin = margin(6, 8, 6, 6))
## Paleta: "color" usa Okabe-Ito (distinguible con daltonismo y legible al
## imprimir en grises); "grises" reproduce el estilo AER de impresion.
PALETA <- "color"
COL <- if (PALETA == "color") list(
  a = "#0072B2", b = "#D55E00", banda = "#DCEAF5", ventana = c("#0072B2", "grey75"),
  escalera = c("#D55E00", "#E69F00", "#009E73", "grey88"), borde = "#08306B") else list(
  a = "black", b = "grey55", banda = "grey90", ventana = c("grey20", "grey75"),
  escalera = c("grey15", "grey55", "grey85", "white"), borde = "black")
guardar_fig <- function(p, id, ancho = 6.5, alto = 4) {
  png_dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
  ggsave(file.path(DIR_F, paste0(id, ".png")), p, width = ancho, height = alto, dpi = 300, device = png_dev)
  tryCatch(ggsave(file.path(DIR_F, paste0(id, ".pdf")), p, width = ancho, height = alto, device = cairo_pdf),
           error = function(e) cat("    (sin PDF: cairo no disponible)\n"))
}
lab_e <- "A\u00f1os desde la adopci\u00f3n"
lab_ha <- "Efecto sobre la deforestaci\u00f3n (ha por a\u00f1o)"

titulo("TABLAS")

## =============================================================================
## Tabla 4.4 - Caracteristicas de linea base: tratados y controles
## =============================================================================
hacer("tabla_4_4", {
  m <- cargar_muestra_principal()
  pre <- m %>% filter(year >= 2001, year <= 2009) %>% group_by(COD_DANE) %>%
    summarise(defor_pre = mean(.data[[OUTCOME]], na.rm = TRUE), .groups = "drop")
  mun <- m %>% group_by(COD_DANE) %>% slice(1) %>% ungroup() %>% left_join(pre, by = "COD_DANE") %>%
    mutate(tratado = G_dep > 0)
  vars <- list(
    c("defor_pre", "P\u00e9rdida de bosque, promedio 2001\u20132009 (ha/a\u00f1o)", 1),
    c("defor_pre_tendencia", "Tendencia de la p\u00e9rdida 2001\u20132009 (pendiente, ha por a\u00f1o)", 1),
    c("baseline_forest_base", "Bosque en 2000 (miles de ha)", 1),
    c("temp_media_c_base", "Temperatura media, 2001 (\u00b0C)", 1),
    c("prec_anual_mm_base", "Precipitaci\u00f3n anual, 2001 (mm)", 0),
    c("pct_sinap_pub_base", "\u00c1rea en el SINAP p\u00fablico, 2012 (%)", 1),
    c("discapital_base", "Distancia a la capital departamental", 1),
    c("disbogota_base", "Distancia a Bogot\u00e1", 1),
    c("distancia_mercado_base", "Distancia al mercado mayorista", 1),
    c("H_coca_base", "Cultivos de coca (ha)", 1),
    c("homicidios_base", "Homicidios, 2003", 1))
  vars <- Filter(function(v) v[1] %in% names(mun), vars)
  filas <- NULL
  for (v in vars) {
    x <- as.numeric(mun[[v[1]]]); if (v[1] == "baseline_forest_base") x <- x / 1000
    d <- as.integer(v[3]); a <- x[mun$tratado]; b <- x[!mun$tratado]
    smd <- (mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE)) / sqrt((var(a, na.rm = TRUE) + var(b, na.rm = TRUE)) / 2)
    filas <- rbind(filas, fila(v[2], c(num(mean(a, na.rm = TRUE), d), num(mean(b, na.rm = TRUE), d), num(smd, 2))),
                   fila("", c(ee(sd(a, na.rm = TRUE), d), ee(sd(b, na.rm = TRUE), d), ""), "ee"))
  }
  filas <- rbind(filas, fila("Municipios", c(ent(sum(mun$tratado)), ent(sum(!mun$tratado)), ""), "sep"))
  tabla(list(id = "tabla_4_4",
    titulo = "Tabla 4.4\u2014Caracter\u00edsticas de l\u00ednea base de los municipios tratados y de control",
    cab = list(c("Tratados", "Controles", "Diferencia")), filas = filas,
    notas = paste0("Medias por grupo; desviaci\u00f3n est\u00e1ndar entre par\u00e9ntesis. La diferencia es estandarizada: ",
      "diferencia de medias dividida por la ra\u00edz del promedio de las varianzas. Muestra principal: municipios tratados ",
      "de la D1 depurada con adopci\u00f3n entre 2013 y 2019 y controles limpios, sin ning\u00fan v\u00ednculo con proyectos de carbono. ",
      "Las distancias y la tasa de homicidios est\u00e1n en las unidades del panel municipal del CEDE."),
    fuente = "Global Forest Change v1.12; ERA5-Land y CHIRPS; RUNAP; panel municipal del CEDE."))
})

## =============================================================================
## Tabla 4.6 - Territorio y tipo de proponente de los proyectos
## =============================================================================
hacer("tabla_4_6", {
  r <- file.path(RAIZ_PROYECTO, "data/interim/diagnostics/65_tabla_4_6.csv")
  if (!file.exists(r)) stop("falta ", r)
  t46 <- read.csv(r, stringsAsFactors = FALSE, encoding = "UTF-8")
  nom_t <- c("resguardo indigena" = "Resguardos ind\u00edgenas", "consejo comunitario" = "Consejos comunitarios", "otro" = "Otros territorios")
  nom_p <- c("comunitario" = "Comunitario", "empresa" = "Empresa", "ong" = "ONG", "publico" = "P\u00fablico",
             "multiples (sin detalle)" = "M\u00faltiples (sin detalle en el registro)", "sin clasificar" = "Sin clasificar")
  filas <- NULL
  for (terr in intersect(names(nom_t), unique(t46$territorio))) {
    s <- t46[t46$territorio == terr, ]
    filas <- rbind(filas, panel(nom_t[[terr]], 5))
    for (i in seq_len(nrow(s)))
      filas <- rbind(filas, fila(paste0("  ", ifelse(s$tipo_proponente[i] %in% names(nom_p), nom_p[s$tipo_proponente[i]], s$tipo_proponente[i])),
        c(ent(s$proyectos[i]), ent(s$municipios[i]), num(s$area_mha[i], 2), ent(s$con_creditos[i]), num(s$creditos_millones[i], 2))))
  }
  filas <- rbind(filas, fila("Total", c(ent(sum(t46$proyectos)), "", num(sum(t46$area_mha), 2),
                                         ent(sum(t46$con_creditos)), num(sum(t46$creditos_millones), 2)), "sep"))
  tabla(list(id = "tabla_4_6",
    titulo = "Tabla 4.6\u2014Proyectos REDD+ seg\u00fan tipo de territorio y de proponente",
    cab = list(c("Proyectos", "Municipios", "\u00c1rea (Mha)", "Con cr\u00e9ditos", "Cr\u00e9ditos (millones)")),
    filas = filas,
    notas = paste0("Proyectos REDD+ de Verra sin las exclusiones D2 y pol\u00edgonos de deforestaci\u00f3n evitada de Karnik et al. (2025) ",
      "en Colombia. El territorio es el que contiene al menos el 50 % del \u00e1rea del proyecto (o su punto, si no tiene pol\u00edgono); ",
      "\u00abotros territorios\u00bb incluye proyectos sin mayor\u00eda en un solo tipo. El proponente se clasific\u00f3 por su nombre ",
      "y se revis\u00f3 a mano; cuando el registro solo informa \u00abMultiple Project Proponents\u00bb se deja como categor\u00eda propia. ",
      "Los municipios no suman entre filas porque un municipio puede tener proyectos de varios tipos. Cr\u00e9ditos: total emitido. ",
      "Descriptivo, sin estimaci\u00f3n."),
    fuente = "Verra (API de Platts y exportaci\u00f3n del registro); Karnik et al. (2025); Agencia Nacional de Tierras; Berkeley VROD."))
})

## =============================================================================
## Tabla 6.1 - Efecto principal
## =============================================================================
hacer("tabla_6_1", {
  r40 <- leer("40_resumen_depurada_y_dosis.csv")
  g <- function(patron) r40[grepl(patron, r40$especificacion), ][1, ]
  dr <- g("PRINCIPAL"); sc <- g("^D1 depurada - sin covariables"); og <- g("^D1 original - DR")
  s66 <- tryCatch(leer("66_regla_sinap.csv"), error = function(e) NULL)
  s10 <- if (!is.null(s66)) s66[grepl("^Sin SINAP", s66$especificacion), ][1, ] else NULL
  ## Escenario A (71): solo nunca tratados y sin dosis desbordada
  r71 <- leer("71_grupo_comparacion.csv")
  q  <- function(etq) r71[r71$especificacion == etq, ][1, ]
  nv <- q("Solo nunca tratados"); sd <- q("Sin dosis desbordada")
  c71 <- leer("71_composicion_comparacion.csv")
  col <- function(att, se, lo, hi, bal = NA, sebal = NA, rel = NA, media = NA)
    list(att = att, se = se, lo = lo, hi = hi, bal = bal, sebal = sebal, rel = rel, media = media)
  c1 <- col(dr$att_simple, dr$ee_simple, dr$ic_inf, dr$ic_sup, dr$att_balanceado, dr$ee_balanceado, dr$att_rel_pct, dr$media_pre_tratados)
  c2 <- if (!is.null(s10)) col(s10$att, s10$ee, s10$att - 1.96 * s10$ee, s10$att + 1.96 * s10$ee,
                               rel = 100 * s10$att / dr$media_pre_tratados, media = dr$media_pre_tratados) else col(NA, NA, NA, NA)
  c3 <- col(sc$att_simple, sc$ee_simple, sc$ic_inf, sc$ic_sup, sc$att_balanceado, sc$ee_balanceado, sc$att_rel_pct, sc$media_pre_tratados)
  c4 <- col(og$att_simple, og$ee_simple, og$ic_inf, og$ic_sup, og$att_balanceado, og$ee_balanceado, og$att_rel_pct, og$media_pre_tratados)
  c5 <- col(nv$att, nv$ee, nv$ic_inf, nv$ic_sup, nv$att_bal, nv$ee_bal, nv$att_rel_pct, nv$media_pre)
  c6 <- col(sd$att, sd$ee, sd$ic_inf, sd$ic_sup, sd$att_bal, sd$ee_bal, sd$att_rel_pct, sd$media_pre)
  cs <- list(c1, c2, c3, c4, c5, c6); x <- function(campo, f) sapply(cs, function(z) f(z[[campo]]))
  rango_ant <- paste0(ent(min(c71$aun_no_tratados)), "\u2013", ent(max(c71$aun_no_tratados)))
  filas <- apilar(
    fila("ATT agregado", x("att", num)),
    fila("", x("se", ee), "ee"),
    fila("", mapply(function(z) ic(z$lo, z$hi), cs)),
    fila("ATT balanceado, e = 0\u20135", x("bal", num), "sep"),
    fila("", x("sebal", ee), "ee"),
    fila("Efecto relativo a la media previa", x("rel", pct)),
    fila("Media previa de los tratados (ha/a\u00f1o)", x("media", num)),
    fila("Covariables", c("11", "10", "No", "11", "11", "11"), "sep"),
    fila("Grupo de comparaci\u00f3n", c("NT + AN", "NT + AN", "NT + AN", "NT + AN", "NT", "NT + AN")),
    fila("Muestra de tratados", c("Principal", "Principal", "Principal", "Original", "Principal", "Sin desborde")),
    fila("Municipios tratados", c(ent(dr$n_tratados), ent(dr$n_tratados), ent(sc$n_tratados), ent(og$n_tratados),
                                  ent(nv$n_tratados), ent(sd$n_tratados))),
    fila("Nunca tratados (NT)", c(ent(REF_N_CONTROLES), ent(REF_N_CONTROLES), ent(REF_N_CONTROLES), ent(N_CONTROLES_D1_ORIGINAL),
                                  ent(nv$n_nunca), ent(sd$n_nunca))),
    fila("A\u00fan no tratados (AN) por par (g, t)", c(rango_ant, rango_ant, rango_ant, "\u2014", "0", "\u2014")))
  filas$v2[4] <- "\u2014"   # la regla de SINAP (66) no reporta el ATT balanceado
  tabla(list(id = "tabla_6_1",
    titulo = "Tabla 6.1\u2014Efecto de los proyectos REDD+ sobre la deforestaci\u00f3n municipal",
    cab = list(c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
               c("Principal", "Sin SINAP", "Sin covariables", "Original", "Solo nunca tratados", "Sin dosis > 1")),
    filas = filas,
    notas = paste0("Estimador de Callaway y Sant\u2019Anna (2021) doblemente robusto, cohortes de adopci\u00f3n 2013\u20132019. Variable ",
      "dependiente: p\u00e9rdida anual de cobertura arb\u00f3rea (ha). Errores est\u00e1ndar entre par\u00e9ntesis, por bootstrap multiplicador ",
      "agrupado por municipio (", BITERS, " repeticiones); intervalos de confianza al 95 % entre corchetes. Grupo de comparaci\u00f3n de ",
      "cada cohorte g en el a\u00f1o t: los municipios nunca tratados (NT) m\u00e1s los tratados que a\u00fan no adoptan en t (AN, G > t); desde ",
      "2019 no quedan AN y la comparaci\u00f3n es solo con NT. La columna (5) usa solo NT. Nunca tratados: controles limpios, sin ning\u00fan ",
      "v\u00ednculo con proyectos de carbono en ninguna fuente ni a\u00f1o. La columna (2) excluye el \u00e1rea en el SINAP; la (6) excluye los ",
      "municipios cuya \u00e1rea REDD+ supera el \u00e1rea de referencia de la dosis. La muestra principal excluye seis municipios cuyo ",
      "\u00fanico proyecto REDD+ fue retirado o rechazado. El efecto relativo divide el ATT agregado por la media previa de los tratados."),
    fuente = "C\u00e1lculos propios con Global Forest Change v1.12 y los registros Verra, RENARE y Cercarbono."))
})

## =============================================================================
## Tabla 6.2 - Sensibilidad al periodo de referencia
## =============================================================================
hacer("tabla_6_2", {
  p59 <- leer("59_periodo_base_y_poisson.csv")
  sel <- function(est, base) p59[p59$estimador == est & p59$periodo_base == base, ][1, ]
  filas <- panel("Panel A. Callaway y Sant\u2019Anna con distinto a\u00f1o de referencia", 2)
  for (k in 1:5) {
    b <- paste0("g-", k); d <- sel("CS doblemente robusto (principal)", b); s <- sel("Callaway y Sant'Anna", b)
    filas <- rbind(filas, fila(paste0("Referencia g ", MENOS, " ", k, if (k == 1) " (principal)" else ""), c(num(d$att), num(s$att))),
                   fila("", c(ee(d$ee), ee(s$ee)), "ee"))
  }
  filas <- rbind(filas, panel("Panel B. Todo el periodo previo como referencia", 2))
  for (e in list(c("Imputacion (BJS)", "Imputaci\u00f3n (Borusyak, Jaravel y Spiess)"),
                 c("ETWFE lineal (Wooldridge)", "Efectos fijos extendidos (Wooldridge), lineal"),
                 c("ETWFE Poisson (A.4)", "Efectos fijos extendidos (Wooldridge), Poisson"))) {
    s <- p59[p59$estimador == e[1], ][1, ]
    filas <- rbind(filas, fila(e[2], c("\u2014", num(s$att))), fila("", c("", ee(s$ee)), "ee"))
  }
  tabla(list(id = "tabla_6_2",
    titulo = "Tabla 6.2\u2014Sensibilidad del efecto al periodo de referencia",
    cab = list(c("(1)", "(2)"), c("Con covariables", "Sin covariables")), filas = filas,
    notas = paste0("ATT agregado en hect\u00e1reas por a\u00f1o; errores est\u00e1ndar entre par\u00e9ntesis, agrupados por municipio. Panel A: estimador ",
      "de Callaway y Sant\u2019Anna con referencia en el a\u00f1o g \u2212 k, donde g es el a\u00f1o de adopci\u00f3n (equivale a permitir k \u2212 1 a\u00f1os de ",
      "anticipaci\u00f3n). Panel B: estimadores que usan todos los a\u00f1os previos a la adopci\u00f3n; sin covariables, la imputaci\u00f3n y los efectos ",
      "fijos extendidos lineales coinciden por construcci\u00f3n. La columna (1) del panel B no se estima porque estos estimadores no tienen ",
      "ajuste doblemente robusto. Muestra principal (36 tratados, 986 controles)."),
    fuente = "C\u00e1lculos propios."))
})

## =============================================================================
## Tabla 6.3 - Cohortes tempranas y tardias, y el ano base
## =============================================================================
hacer("tabla_6_3", {
  b50 <- leer("50_cohortes_y_base.csv"); c50 <- leer("50_por_cohorte.csv")
  g <- function(base, grupo) b50[b50$base == base & b50$grupo == grupo, ][1, ]
  filas <- panel("Panel A. Efecto por grupo de cohortes", 2)
  for (gr in list(c("todas", "Todas las cohortes"), c("tempranas 2013-2016", "Cohortes tempranas, 2013\u20132016"),
                  c("tardias 2017-2019", "Cohortes tard\u00edas, 2017\u20132019"))) {
    a <- g("g-1", gr[1]); b <- g("promedio g-5..g-1", gr[1])
    filas <- rbind(filas, fila(gr[2], c(num(a$att), num(b$att))),
                   fila("", c(ee(a$ee_boot), ee(b$ee_boot)), "ee"),
                   fila("", c(ic(a$ic_inf, a$ic_sup), ic(b$ic_inf, b$ic_sup))))
  }
  filas2 <- panel("Panel B. Por cohorte", 2)
  for (i in seq_len(nrow(c50))) {
    z <- c50[i, ]
    filas2 <- rbind(filas2, fila(sprintf("%d (%d municipios)", z$g, z$n_municipios), c(num(z$att_base_g1), num(z$att_base_prom))))
  }
  tabla(list(id = "tabla_6_3",
    titulo = "Tabla 6.3\u2014El efecto por cohorte y seg\u00fan el a\u00f1o de referencia",
    cab = list(c("(1)", "(2)"), c(paste0("Referencia g ", MENOS, " 1"), "Referencia promedio")),
    filas = rbind(filas, filas2),
    notas = paste0("ATT en hect\u00e1reas por a\u00f1o, especificaci\u00f3n principal. Columna (1): referencia en el a\u00f1o previo a la adopci\u00f3n. ",
      "Columna (2): referencia igual al promedio de los cinco a\u00f1os previos (g \u2212 5 a g \u2212 1). Panel A: errores est\u00e1ndar entre ",
      "par\u00e9ntesis e intervalos al 95 % entre corchetes, por bootstrap de ", BOOT_COHORTES, " r\u00e9plicas estratificado por cohorte. Panel B: estimados ",
      "puntuales por cohorte; con tan pocos municipios por cohorte no se reportan intervalos."),
    fuente = "C\u00e1lculos propios."))
})

## =============================================================================
## Tabla 6.4 - Composicion de las cohortes por nivel de la escalera
## Tabla 6.5 - Efecto por nivel de la escalera
## =============================================================================
hacer("tabla_6_5", {
  c58 <- leer("58_composicion_por_cohorte.csv")
  filas <- NULL
  for (i in seq_len(nrow(c58))) {
    z <- c58[i, ]
    filas <- rbind(filas, fila(as.character(z$G_dep), c(ent(z$n), ent(z$solo_S1), ent(z$S2 - z$E), ent(z$E), ent(z$sin_estado))))
  }
  tot <- colSums(c58[, c("n", "solo_S1", "S2", "E", "sin_estado")])
  filas <- rbind(filas, fila("Total", c(ent(tot["n"]), ent(tot["solo_S1"]), ent(tot["S2"] - tot["E"]), ent(tot["E"]), ent(tot["sin_estado"])), "sep"))
  tabla(list(id = "tabla_6_5",
    titulo = "Tabla 6.5\u2014Municipios tratados por cohorte y estado de sus proyectos en el registro",
    cab = list(c("Tratados", "Nunca operaron", "Registrados sin cr\u00e9ditos", "Con cr\u00e9ditos", "Sin estado")),
    filas = filas,
    notas = paste0("Cada municipio se clasifica por el mejor estado entre sus proyectos REDD+ de Verra. Nunca operaron: proyectos que no ",
      "pasaron de solicitud, validaci\u00f3n o lista de espera (nivel S1). Registrados sin cr\u00e9ditos: alcanzaron registro efectivo pero no ",
      "emitieron cr\u00e9ditos. Con cr\u00e9ditos: emitieron cr\u00e9ditos seg\u00fan Berkeley VROD. Sin estado: municipios tratados solo por RENARE ",
      "o Cercarbono, que no publican estado de registro."),
    fuente = "Verra (API de Platts); Berkeley VROD; c\u00e1lculos propios."))
})
hacer("tabla_6_6", {
  r58 <- leer("58_contraste_mecanismo.csv")
  if ("control" %in% names(r58)) r58 <- r58[r58$control == CONTROL_PRINCIPAL, ]
  sel <- list(c("^solo_S1 \\(", "Nunca operaron (solo S1)"), c("^S2 \\(", "Registro efectivo (S2)"),
              c("^E \\(", "Con cr\u00e9ditos emitidos (E)"), c("^S3 \\(", "Registrado hoy (S3)"))
  filas <- NULL
  for (s in sel) {
    z <- r58[grepl(s[1], r58$especificacion), ][1, ]
    filas <- rbind(filas, fila(s[2], c(num(z$att), ent(z$n_tratados), ent(z$cohortes_ok))),
                   fila("", c(ee(z$ee), "", ""), "ee"))
  }
  tabla(list(id = "tabla_6_6",
    titulo = "Tabla 6.6\u2014El efecto seg\u00fan el estado de los proyectos en el registro",
    cab = list(c("(1)", "(2)", "(3)"), c("ATT", "Tratados", "Cohortes con 3 o m\u00e1s")), filas = filas,
    notas = paste0("ATT en hect\u00e1reas por a\u00f1o, sin covariables. Grupo de comparaci\u00f3n igual al de la especificaci\u00f3n principal: los 986 ",
      "nunca tratados limpios m\u00e1s los municipios del mismo grupo que a\u00fan no adoptan; errores est\u00e1ndar entre par\u00e9ntesis. Los grupos E y S3 est\u00e1n contenidos en S2. Ning\u00fan grupo alcanza tres o m\u00e1s ",
      "municipios en la mayor\u00eda de sus cohortes, por lo que el contraste se lee como descriptivo (decisi\u00f3n D13). E y S3 dependen en ",
      "parte del resultado: un proyecto emite cr\u00e9ditos o se mantiene registrado cuando verifica reducciones."),
    fuente = "C\u00e1lculos propios."))
})

## =============================================================================
## Tabla 6.6 - Analisis de sensibilidad de Rambachan y Roth
## =============================================================================
hacer("tabla_6_4", {
  h <- leer("44_honestdid_resumen.csv")
  nom <- c(dr = "Con covariables", sincov = "Sin covariables"); par <- c(e0 = "e = 0", promedio_0_5 = "Promedio e = 0\u20135")
  filas <- NULL
  for (sp in c("dr", "sincov")) {
    filas <- rbind(filas, panel(paste0("Panel ", if (sp == "dr") "A" else "B", ". ", nom[[sp]]), 5))
    for (pp in c("e0", "promedio_0_5")) {
      z <- h[h$especificacion == sp & h$parametro == pp, ][1, ]
      filas <- rbind(filas, fila(par[[pp]], c(ic_txt(z$ic_convencional), ic_txt(z$ic_rm_Mbar_0.5), ic_txt(z$ic_rm_Mbar_1),
                                                ic_txt(z$ic_sd_M_0), ic_txt(z$ic_sd_M_obs))))
    }
    mo <- h$M_obs[h$especificacion == sp][1]
    filas <- rbind(filas, fila(paste0("  M observado = ", num(mo, 0), " ha"), rep("", 5), "ee"))
  }
  tabla(list(id = "tabla_6_4",
    titulo = "Tabla 6.4\u2014Intervalos robustos a violaciones de tendencias paralelas",
    cab = list(c("(1)", "(2)", "(3)", "(4)", "(5)"),
               c("Convencional", "M\u0304 = 0,5", "M\u0304 = 1", "M = 0", "M = M obs.")),
    filas = filas,
    notas = paste0("Intervalos de confianza al 95 % (ha por a\u00f1o) seg\u00fan Rambachan y Roth (2023). Columnas (2) y (3): magnitudes relativas, ",
      "la violaci\u00f3n posterior puede ser hasta M\u0304 veces la mayor violaci\u00f3n previa. Columnas (4) y (5): suavidad, la pendiente de la ",
      "tendencia diferencial puede cambiar hasta M ha por a\u00f1o; M = 0 extrapola linealmente la tendencia previa y M observado es la mayor ",
      "curvatura previa a la adopci\u00f3n. Estudio de eventos con referencia universal en g \u2212 1. Intervalos de magnitudes relativas con el ",
      "m\u00e9todo condicional h\u00edbrido (C-LF) y de suavidad con el intervalo de longitud fija (FLCI), los predeterminados de HonestDiD. El valor de ",
      "ruptura es cero en todos los casos: el intervalo convencional ya incluye el cero."),
    fuente = "C\u00e1lculos propios."))
})

## =============================================================================
## Tabla 6.7 - Pruebas de robustez
## =============================================================================
hacer("tabla_6_9", {
  gv <- function(df, cands) { c <- intersect(cands, names(df))[1]; if (is.na(c)) NA else df[[c]][1] }
  fila_r <- function(df, etq, misma_muestra = TRUE) {
    if (is.null(df) || !nrow(df) || all(is.na(df[1, ]))) { cat("    (sin fila para: ", etq, ")\n", sep = ""); return(NULL) }
    nt <- gv(df, "n_tratados"); nc <- gv(df, "n_controles"); na <- gv(df, "gt_na")
    if (misma_muestra) { if (is.na(nt)) nt <- REF_N_TRATADOS; if (is.na(nc)) nc <- REF_N_CONTROLES }
    raya <- function(v) if (v == "") "\u2014" else v
    fila(etq, c(num(gv(df, c("att", "coef"))), ee(gv(df, "ee")), raya(ent(nt)), raya(ent(nc)), raya(ent(na))))
  }
  lee <- function(f) tryCatch(leer(f), error = function(e) { cat("    (sin ", f, ")\n", sep = ""); NULL })
  r60 <- lee("60_robusteces.csv"); r48 <- lee("48_soporte_comun_estimaciones.csv"); r49 <- lee("49_controles_no_contiguos.csv")
  r51 <- lee("51_psm_principal.csv"); r53 <- lee("53_controles_sin_poligonos.csv"); r46 <- lee("46_resumen_mapbiomas.csv")
  r45 <- lee("45_spillover_resumen.csv")
  q <- function(df, cond) {
    if (is.null(df)) return(NULL)
    i <- tryCatch(which(cond(df)), error = function(e) integer(0))
    if (!length(i)) NULL else df[i[1], , drop = FALSE]
  }
  princ <- data.frame(att = REF_ATT_DR, ee = REF_EE_DR, n_tratados = REF_N_TRATADOS, n_controles = REF_N_CONTROLES, gt_na = 0, gt_total = 161)
  filas <- apilar(
    fila_r(princ, "Especificaci\u00f3n principal"),
    panel("Panel A. Inferencia", 5),
    fila_r(q(r60, function(d) grepl("^Principal, cluster departamento", d$especificacion)), "Errores agrupados por departamento"),
    panel("Panel B. Definici\u00f3n del tratamiento", 5),
    fila_r(q(r60, function(d) grepl("^alta_redd .* - DR", d$especificacion)), "Solo registros de Verra"),
    fila_r(q(r60, function(d) grepl("^todas_redd_sinmanual - DR", d$especificacion)), "Sin ubicaci\u00f3n recuperada a mano"),
    fila_r(q(r60, function(d) grepl("^Cobertura de poligonos .* - DR", d$especificacion)), "Cobertura de pol\u00edgonos \u2265 5 %"),
    panel("Panel C. Grupo de control y soporte com\u00fan", 5),
    fila_r(q(r48, function(d) d$recorte == "R1" & d$metodo == "dr"), "Soporte com\u00fan, propensi\u00f3n en [0,01; 0,99]"),
    fila_r(q(r51, function(d) grepl("^\\(c'\\)", d$especificacion)), "Emparejamiento por propensi\u00f3n, con covariables"),
    fila_r(q(r51, function(d) grepl("^\\(c\\)", d$especificacion)), "Emparejamiento por propensi\u00f3n, sin covariables"),
    fila_r(q(r53, function(d) d$muestra == "P1" & d$especificacion == "dr"), "Sin controles cubiertos por pol\u00edgonos"),
    fila_r(q(r49, function(d) d$muestra == "C1" & d$especificacion == "dr"), "Sin controles contiguos a tratados"),
    panel("Panel D. Fuente del dato de deforestaci\u00f3n", 5),
    fila_r(q(r46, function(d) d$outcome == "MB_est5_p1" & d$especificacion == "propias"), "MapBiomas, bosque estable con persistencia"),
    panel("Panel E. Placebo, desplazamiento y heterogeneidad", 5),
    fila_r(q(r60, function(d) grepl("^Placebo AFOLU no-REDD\\+ - sin covariables", d$especificacion)), "Placebo: proyectos forestales sin REDD+"),
    fila_r(q(r45, function(d) grepl("^A", d$enfoque) & grepl("Doblemente", d$especificacion)), "Desplazamiento hacia municipios vecinos", misma_muestra = FALSE),
    fila_r(q(r60, function(d) grepl("^Fuera de VA .* - sin covariables", d$especificacion)), "Fuera del \u00e1rea de Visi\u00f3n Amazon\u00eda"),
    fila_r(q(r60, function(d) grepl("^Solo VA .* - sin covariables", d$especificacion)), "Solo el \u00e1rea de Visi\u00f3n Amazon\u00eda"))
  tabla(list(id = "tabla_6_9",
    titulo = "Tabla 6.9\u2014Pruebas de robustez",
    cab = list(c("(1)", "(2)", "(3)", "(4)", "(5)"), c("ATT", "Error est\u00e1ndar", "Tratados", "Controles", "Pares sin estimar")),
    filas = filas,
    notas = paste0("ATT agregado en hect\u00e1reas por a\u00f1o. Salvo indicaci\u00f3n, estimador doblemente robusto con las 11 covariables de la ",
      "especificaci\u00f3n principal y errores agrupados por municipio. El placebo, el \u00e1rea de Visi\u00f3n Amazon\u00eda y el emparejamiento sin ",
      "covariables se reportan sin covariables porque la versi\u00f3n doblemente robusta pierde muchos pares cohorte-a\u00f1o. Pares sin estimar: ",
      "pares cohorte-a\u00f1o que no pudieron estimarse (161 en la muestra principal). Desplazamiento: efecto sobre municipios no ",
      "tratados con al menos un vecino tratado, frente a no tratados sin vecinos tratados; usa otra muestra, por eso no se reportan sus tama\u00f1os. Sin controles contiguos: el cambio de signo se explica por la composici\u00f3n de los pares cohorte-a\u00f1o que pueden estimarse, no por desplazamiento."),
    fuente = "C\u00e1lculos propios."))
})


## =============================================================================
## Tabla 6.8 - Evaluacion por poligonos (celdas de 2 x 2 km)
## Tabla 6.9 - Efecto minimo detectable por cohorte
## =============================================================================
hacer("tabla_6_7", {
  r <- leer("56_resultados_celdas.csv")
  g <- function(res, patron) r[r$resultado == res & grepl(patron, r$analisis), ][1, ]
  f8 <- function(z, etq, d = 3) fila(etq, c(num(z$att, d), ee(z$ee, d), ic(z$ic_inf, z$ic_sup, 2),
                                           pct(z$att_rel_pct, 0), num(z$media_pre_tratadas, d)))
  pr <- g("def_p1", "^principal")
  filas <- apilar(
    panel("Panel A. Efecto principal", 5),
    f8(pr, "P\u00e9rdida de bosque natural (ha)"),
    f8(g("tasa_pct", "^principal"), "Tasa de p\u00e9rdida (% del bosque)"),
    panel("Panel B. Por tipo de bosque de origen", 5),
    f8(g("def_p1_tierra_firme", "^por tipo"), "Bosque de tierra firme"),
    f8(g("def_p1_inundable", "^por tipo"), "Bosque inundable"),
    panel("Panel C. Por uso de destino", 5),
    f8(g("def_p1_agropecuario", "^por tipo"), "Agropecuario"),
    f8(g("def_p1_mineria", "^por tipo"), "Miner\u00eda"),
    f8(g("def_p1_otros_antropicos", "^por tipo"), "Otros usos antr\u00f3picos"),
    panel("Panel D. Conversi\u00f3n de vegetaci\u00f3n secundaria (no incluida en el panel A)", 5),
    f8(g("def_p1_rastrojo", "^por tipo"), "Rastrojo a uso antr\u00f3pico"),
    panel("Panel E. Desplazamiento", 5),
    f8(g("def_p1", "^fuga"), "Celdas de la zona de amortiguaci\u00f3n"),
    panel("Panel F. Sensibilidad al estimador", 5),
    f8(r[r$resultado == "def_p1" & grepl("^sensibilidad", r$analisis) & r$covariables %in% c(TRUE, "TRUE"), ][1, ],
       "Doblemente robusto, con covariables"),
    f8(r[r$resultado == "def_p1" & grepl("^sensibilidad", r$analisis) & r$covariables %in% c(FALSE, "FALSE"), ][1, ],
       "Doblemente robusto, sin covariables"))
  fu <- g("def_p1", "^fuga")
  tabla(list(id = "tabla_6_7",
    titulo = "Tabla 6.7\u2014Efecto de los proyectos sobre la conversi\u00f3n de bosque en sus pol\u00edgonos",
    cab = list(c("(1)", "(2)", "(3)", "(4)", "(5)"),
               c("ATT", "Error est\u00e1ndar", "IC 95 %", "Relativo", "Media previa")),
    filas = filas,
    notas = paste0("Unidad: celda de 2 \u00d7 2 km (400 ha); ATT y media previa en hect\u00e1reas por celda y a\u00f1o, salvo la tasa (puntos ",
      "porcentuales). Celdas tratadas: las que caen dentro de los pol\u00edgonos de ", ent(pr$conglomerados_tratados),
      " proyectos de deforestaci\u00f3n evitada (Karnik et al., 2025); controles emparejados por bosque en 2000, \u00e1rea inundable, ",
      "elevaci\u00f3n, pendiente, deforestaci\u00f3n previa y presi\u00f3n de deforestaci\u00f3n en el entorno. Estimador de Callaway y ",
      "Sant\u2019Anna por regresi\u00f3n sobre la muestra emparejada (", ent(pr$celdas_tratadas), " celdas tratadas y ",
      ent(pr$celdas_control), " de control), con errores agrupados por proyecto. Paneles B y C: el bosque de origen y el uso de destino ",
      "descomponen la p\u00e9rdida del panel A; el manglar y los destinos con menos de 1.000 celda-a\u00f1os con p\u00e9rdida o de tres proyectos ",
      "(palma, infraestructura, silvicultura, pl\u00e1tano) no se estiman por separado y explican la diferencia residual con el total. Panel D: conversi\u00f3n de ",
      "vegetaci\u00f3n secundaria, que no es bosque estable y por eso queda fuera de la p\u00e9rdida del panel A. Panel E: celdas de la zona de amortiguaci\u00f3n frente ",
      "a controles (", ent(fu$celdas_tratadas), " y ", ent(fu$celdas_control), " celdas). El relativo divide el ATT por la media previa; ",
      "en miner\u00eda y otros usos la media es casi cero y el porcentaje no es informativo. Panel F: el estimador doblemente robusto ",
      "con covariables es inestable por la separaci\u00f3n casi completa en el modelo de propensi\u00f3n."),
    fuente = "MapBiomas Colombia, Colecci\u00f3n 3; Karnik et al. (2025); c\u00e1lculos propios."))
})
hacer("tabla_6_8", {
  e <- leer("57_efecto_minimo_detectable.csv"); e <- e[e$resultado == "def_p1", ]
  e <- e[order(e$cohorte == "todas", e$cohorte), ]
  filas <- NULL
  for (i in seq_len(nrow(e))) {
    z <- e[i, ]; tot <- z$cohorte == "todas"
    filas <- rbind(filas, fila(if (tot) "Todas las cohortes" else as.character(z$cohorte),
      c(ent(z$proyectos), ent(z$celdas), num(z$media_pre, 2), num(z$att, 2), num(z$emd, 2), pct(z$emd_rel_pct, 0)),
      if (tot) "sep" else "dato"),
      fila("", c("", "", "", ee(z$ee, 2), "", ""), "ee"))
  }
  tabla(list(id = "tabla_6_8",
    titulo = "Tabla 6.8\u2014Efecto m\u00ednimo detectable de la evaluaci\u00f3n por pol\u00edgonos",
    cab = list(c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
               c("Proyectos", "Celdas", "Media previa", "ATT", "EMD", "EMD / media")),
    filas = filas,
    notas = paste0("P\u00e9rdida de bosque natural en hect\u00e1reas por celda de 400 ha y a\u00f1o. Columna (4): ATT con su error est\u00e1ndar entre ",
      "par\u00e9ntesis. EMD: efecto m\u00ednimo detectable con 5 % de significancia (dos colas) y 80 % de potencia, igual a 2,8 veces el ",
      "error est\u00e1ndar. Columna (6): EMD como porcentaje de la p\u00e9rdida previa de las celdas tratadas; un valor mayor que 100 % ",
      "significa que ni siquiera eliminar toda la deforestaci\u00f3n ser\u00eda detectable. No hay cohorte 2016 ni 2019 con pol\u00edgono."),
    fuente = "MapBiomas Colombia, Colecci\u00f3n 3; Karnik et al. (2025); c\u00e1lculos propios."))
})


## =============================================================================
## Tabla 5.3 - Cohortes y anos de implementacion observados [JM p.40] (capitulo 5)
## =============================================================================
hacer("tabla_5_3", {
  m <- cargar_muestra_principal()
  tr <- m %>% filter(G_dep > 0) %>% distinct(COD_DANE, G_dep)
  ymin <- min(m$year); ymax <- max(m$year)
  c47 <- tr %>% count(G_dep) %>% arrange(G_dep)
  filas <- NULL
  for (i in seq_len(nrow(c47))) {
    g <- c47$G_dep[i]
    filas <- rbind(filas, fila(as.character(g), c(ent(c47$n[i]), ent(g - ymin), ent(ymax - g + 1),
      paste0(MENOS, ent(g - ymin), " a ", ent(ymax - g)), ifelse(g + 5 <= ymax, "S\u00ed", "No"))))
  }
  filas <- rbind(filas, fila("Total", c(ent(sum(c47$n)), "", "", "", ""), "sep"))
  tabla(list(id = "tabla_5_3",
    titulo = "Tabla 5.3\u2014Cohortes de adopci\u00f3n y a\u00f1os observados antes y despu\u00e9s de la adopci\u00f3n",
    cab = list(c("(1)", "(2)", "(3)", "(4)", "(5)"),
               c("Municipios", "A\u00f1os antes", "A\u00f1os despu\u00e9s", "Rango de e", "Entra en e = 0\u20135")),
    filas = filas,
    notas = paste0("Muestra principal: municipios tratados de la D1 depurada con adopci\u00f3n entre 2013 y 2019. A\u00f1os antes: a\u00f1os ",
      "observados desde ", ymin, " hasta el previo a la adopci\u00f3n. A\u00f1os despu\u00e9s: desde el a\u00f1o de adopci\u00f3n (e = 0) hasta ", ymax,
      ". Rango de e: tiempo relativo a la adopci\u00f3n que aporta cada cohorte al estudio de eventos. La \u00faltima columna indica si la ",
      "cohorte se observa completa en los seis primeros a\u00f1os, que es la ventana del efecto balanceado."),
    fuente = "C\u00e1lculos propios."))
})

## =============================================================================
## Tabla 6.10 - Heterogeneidad por gasto ambiental y desempeno fiscal [JM p.43]
## =============================================================================
hacer("tabla_6_10", {
  h <- leer("68_heterogeneidad_fiscal.csv")
  f10 <- function(v, g) {
    a <- h[h$variable == v & h$grupo == g & h$especificacion == "sincov", ][1, ]
    b <- h[h$variable == v & h$grupo == g & h$especificacion == "dr", ][1, ]
    dif <- grepl("^Diferencia", g)
    rbind(fila(if (dif) "  Diferencia (alto \u2212 bajo)" else if (grepl("alto$", g)) "  Alto" else "  Bajo",
               c(num(a$att), num(b$att), if (dif) "" else ent(a$n_tratados), if (dif) "" else ent(a$n_controles),
                 if (dif) "" else ent(a$cohortes_ok))),
          fila("", c(ee(a$ee), ee(b$ee), "", "", ""), "ee"))
  }
  filas <- apilar(
    panel("Panel A. Inversi\u00f3n municipal en medio ambiente, 2002\u20132009", 5),
    f10("inv_amb_pct_2002_2009", "Gasto ambiental alto"), f10("inv_amb_pct_2002_2009", "Gasto ambiental bajo"),
    f10("inv_amb_pct_2002_2009", "Diferencia (alto - bajo)"),
    panel("Panel B. \u00cdndice de Desempe\u00f1o Fiscal, 2001\u20132009", 5),
    f10("idf_2001_2009", "Desempe\u00f1o fiscal alto"), f10("idf_2001_2009", "Desempe\u00f1o fiscal bajo"),
    f10("idf_2001_2009", "Diferencia (alto - bajo)"))
  med <- tapply(h$mediana, h$variable, function(x) x[1])
  tabla(list(id = "tabla_6_10",
    titulo = "Tabla 6.10\u2014Heterogeneidad del efecto por gasto ambiental y desempe\u00f1o fiscal de l\u00ednea base",
    cab = list(c("(1)", "(2)", "(3)", "(4)", "(5)"),
               c("Sin covariables", "Con covariables", "Tratados", "Controles", "Cohortes con 3 o m\u00e1s")),
    filas = filas,
    notas = paste0("ATT en hect\u00e1reas por a\u00f1o, estimado por separado en cada grupo con la muestra principal; errores est\u00e1ndar entre ",
      "par\u00e9ntesis, agrupados por municipio. Los grupos se cortan en la mediana de todos los municipios de la muestra: ",
      num(med[["inv_amb_pct_2002_2009"]], 2), " % de la inversi\u00f3n total en medio ambiente y ", num(med[["idf_2001_2009"]], 1),
      " puntos del \u00edndice de desempe\u00f1o fiscal. La diferencia usa un error est\u00e1ndar conservador, sin covarianza entre grupos. ",
      "La columna (2) pierde muchos pares cohorte-a\u00f1o (", paste(paste0(h$gt_na[h$especificacion == "dr" & !is.na(h$gt_na)], " de ",
      h$gt_total[h$especificacion == "dr" & !is.na(h$gt_na)]), collapse = ", "), ", en el orden de las filas) y se reporta solo como referencia. ",
      "Regla fijada antes de correr: la diferencia se lee como hallazgo solo si ambos grupos tienen al menos dos cohortes con tres o m\u00e1s ",
      "tratados y su intervalo excluye el cero. Exploratoria: no corresponde a ninguna hip\u00f3tesis."),
    fuente = "Panel de Buen Gobierno del CEDE; c\u00e1lculos propios."))
})

## =============================================================================
## Tabla 6.11 - RDD espacial en el borde de los poligonos (73)
## =============================================================================
hacer("tabla_6_11", {
  r <- leer("73_rdd_espacial.csv"); b <- leer("73_rdd_balance.csv")
  g <- function(y, dona, cov = FALSE) r[r$resultado == y & r$dona_km == dona & as.logical(r$covariables) == cov, ][1, ]
  f <- function(z, etq, d = 3) fila(etq, c(num(z$tau, d), ee(z$ee_robusto, d), ic(z$ic_rob_inf, z$ic_rob_sup, min(d, 2)),
                                           num(z$h_km, 1), paste0(ent(z$n_fuera), " / ", ent(z$n_dentro))))
  filas <- apilar(
    panel("Panel A. Cambio en la p\u00e9rdida (principal)", 5),
    f(g("cambio", 0), "Sin dona"), f(g("cambio", 0, TRUE), "Con covariables"),
    f(g("cambio", 1), "Dona de 1 km"), f(g("cambio", 2), "Dona de 2 km"),
    panel("Panel B. P\u00e9rdida despu\u00e9s de la adopci\u00f3n", 5),
    f(g("post", 0), "Sin dona"), f(g("post", 1), "Dona de 1 km"), f(g("post", 2), "Dona de 2 km"),
    panel("Panel C. Placebo: p\u00e9rdida previa", 5),
    f(g("pre", 0), "Sin dona"), f(g("pre", 1), "Dona de 1 km"), f(g("pre", 2), "Dona de 2 km"),
    panel("Panel D. Covariables en el borde", 5),
    f(b[b$resultado == "elev", ][1, ], "Elevaci\u00f3n (m)", 1),
    f(b[b$resultado == "pend", ][1, ], "Pendiente (grados)", 2),
    f(b[b$resultado == "pct_bosque2000", ][1, ], "Bosque en 2000 (fracci\u00f3n)"))
  m0 <- g("cambio", 0)
  tabla(list(id = "tabla_6_11",
    titulo = "Tabla 6.11\u2014Discontinuidad en el borde de los pol\u00edgonos REDD+",
    cab = list(c("(1)", "(2)", "(3)", "(4)", "(5)"),
               c("Salto", "EE robusto", "IC 95 % robusto", "Ventana (km)", "Celdas fuera / dentro")),
    filas = filas,
    notas = paste0("Regresi\u00f3n discontinua espacial con celdas de 2 \u00d7 2 km a menos de 20 km del borde de los proyectos de ",
      "deforestaci\u00f3n evitada de las cohortes 2013, 2014, 2015, 2017 y 2018 (", ent(m0$proyectos), " proyectos; Karnik et al., 2025). Variable de asignaci\u00f3n: distancia ",
      "del centroide al borde, positiva dentro del pol\u00edgono. Resultado: p\u00e9rdida de bosque natural (ha por celda y a\u00f1o, MapBiomas), ",
      "con el a\u00f1o de adopci\u00f3n del proyecto del borde m\u00e1s cercano. Polinomio local lineal, kernel triangular, ventana \u00f3ptima ",
      "MSE com\u00fan a ambos lados e intervalos robustos con correcci\u00f3n de sesgo (Calonico, Cattaneo y Titiunik, 2014), agrupados por ",
      "proyecto. Un salto negativo indica menos p\u00e9rdida dentro del proyecto. La dona excluye las celdas a menos de 1 o 2 km del borde. ",
      "Panel A: p\u00e9rdida media desde la adopci\u00f3n hasta 2023 menos la media de los cinco a\u00f1os previos; panel C: media de los cinco a\u00f1os ",
      "previos. Covariables: elevaci\u00f3n, pendiente y bosque en 2000. Media del cambio fuera del borde, dentro de la ventana: ",
      num(m0$media_fuera, 3), " ha."),
    fuente = "MapBiomas Colombia, Colecci\u00f3n 3; Karnik et al. (2025); c\u00e1lculos propios."))
})

## =============================================================================
## FIGURAS
## =============================================================================
titulo("FIGURAS")

hacer("figura_4_1", {
  if (!requireNamespace("sf", quietly = TRUE)) stop("falta el paquete sf")
  mun <- sf::st_read(RUTA_MUNICIPIOS, quiet = TRUE)
  cc <- intersect(c("COD_DANE", "MPIO_CDPMP", "CODIGO"), names(mun))[1]
  mun$COD_DANE <- pad5(mun[[cc]])
  mun <- sf::st_simplify(sf::st_transform(mun, 9377), dTolerance = 800)
  pan <- readRDS(RUTA_PANEL_DID); pan$COD_DANE <- pad5(pan$COD_DANE)
  acum <- pan %>% group_by(COD_DANE) %>% summarise(perdida = sum(.data[[OUTCOME]], na.rm = TRUE) / 1000, .groups = "drop")
  tr <- read.csv(RUTA_PANEL_TRAT, colClasses = c(COD_DANE = "character"))
  tr <- tr %>% mutate(COD_DANE = pad5(COD_DANE)) %>% group_by(COD_DANE) %>%
    summarise(G = suppressWarnings(max(anio_inicio_tratamiento_todas_fuentes_redd_depurada, na.rm = TRUE)), .groups = "drop") %>%
    mutate(tratado = is.finite(G) & G > 0)
  mun <- mun %>% left_join(acum, by = "COD_DANE") %>% left_join(tr, by = "COD_DANE")
  ## Numero de proyectos REDD+ por municipio (Verra y poligonos de Karnik, script 65) [JM p.17]
  rp <- file.path(RAIZ_PROYECTO, "data/interim/diagnostics/65_proyectos_territorio_proponente.csv")
  npro <- if (file.exists(rp)) read.csv(rp, stringsAsFactors = FALSE) %>% mutate(COD_DANE = pad5(COD_DANE)) %>%
    count(COD_DANE, name = "proyectos") else data.frame(COD_DANE = character(), proyectos = integer())
  cen <- suppressWarnings(sf::st_point_on_surface(mun[mun$COD_DANE %in% npro$COD_DANE, "COD_DANE"])) %>%
    left_join(npro, by = "COD_DANE")
  p <- ggplot(mun) +
    geom_sf(aes(fill = perdida), color = NA) +
    geom_sf(data = mun[mun$tratado %in% TRUE, ], fill = NA, color = "grey35", linewidth = 0.25) +
    geom_sf(data = cen, aes(size = proyectos), shape = 21, fill = COL$a, color = "white", stroke = 0.3, alpha = 0.85) +
    scale_size_area(max_size = 6, breaks = function(l) unique(pmax(1, round(seq(1, l[2], length.out = 3)))),
                    name = "Proyectos REDD+") +
    (if (PALETA == "color")
       scale_fill_distiller(palette = "YlOrRd", direction = 1, trans = "sqrt", na.value = "white",
                            name = "P\u00e9rdida de bosque 2001\u20132024 (miles de ha)")
     else scale_fill_gradient(low = "grey95", high = "grey15", trans = "sqrt", na.value = "white",
                              name = "P\u00e9rdida de bosque 2001\u20132024 (miles de ha)")) +
    theme_void(base_family = FUENTE, base_size = 9) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_text(size = 8),
          legend.key.width = unit(1.2, "cm"), legend.margin = margin(2, 0, 2, 0))
  guardar_fig(p, "figura_4_1", ancho = 6, alto = 7.4)
})

hacer("figura_4_2", {
  tr <- read.csv(RUTA_PANEL_TRAT, colClasses = c(COD_DANE = "character"))
  g <- tr %>% mutate(COD_DANE = pad5(COD_DANE)) %>% group_by(COD_DANE) %>%
    summarise(G = suppressWarnings(max(anio_inicio_tratamiento_todas_fuentes_redd_depurada, na.rm = TRUE)), .groups = "drop") %>%
    filter(is.finite(G), G > 0) %>% count(G) %>%
    mutate(ventana = ifelse(G %in% VENTANA, "Cohortes reportadas (2013\u20132019)", "Fuera de la ventana"))
  p <- ggplot(g, aes(factor(G), n, fill = ventana)) + geom_col(width = 0.7) +
    scale_fill_manual(values = COL$ventana) +
    labs(x = "A\u00f1o de adopci\u00f3n", y = "Municipios tratados") + tema_aer
  guardar_fig(p, "figura_4_2", alto = 3.5)
})


hacer("figura_4_3", {
  rp <- file.path(RAIZ_PROYECTO, "data/interim/diagnostics/65_proyectos_territorio_proponente.csv")
  if (!file.exists(rp)) stop("falta ", rp)
  d <- read.csv(rp, stringsAsFactors = FALSE)
  d <- d[d$registro == "Verra", ]
  d$anio <- suppressWarnings(as.integer(sub("^.*?((19|20)[0-9]{2}).*$", "\\1", d$anio_inicio, perl = TRUE)))
  est <- tolower(d$estado); emit <- !is.na(d$emitidos) & d$emitidos > 0
  niv <- c("Emiti\u00f3 cr\u00e9ditos", "Registrado sin cr\u00e9ditos", "En tr\u00e1mite", "Retirado, rechazado o inactivo")
  d$grupo <- ifelse(emit, niv[1],
             ifelse(grepl("^registered|late to verify|^verification approval", est), niv[2],
             ifelse(grepl("withdrawn|rejected|inactive", est), niv[4], niv[3])))
  d <- d[!is.na(d$anio), ]; d$grupo <- factor(d$grupo, levels = niv)
  cat(sprintf("    figura 4.3: %d proyectos de Verra con a\u00f1o de inicio\n", nrow(d)))
  p <- ggplot(d, aes(factor(anio), fill = grupo)) + geom_bar(width = 0.7, color = "black", linewidth = 0.2) +
    scale_fill_manual(values = c(COL$escalera[3], COL$escalera[2], "grey85", COL$escalera[1]), drop = FALSE) +
    labs(x = "A\u00f1o de inicio del periodo de acreditaci\u00f3n", y = "Proyectos REDD+ de Verra") + tema_aer +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill = guide_legend(nrow = 2))
  guardar_fig(p, "figura_4_3", alto = 3.8)
})

hacer("figura_6_1", {
  a <- read.csv(TAB("44_event_study_base_universal_dr.csv")); a$esp <- "Con covariables (principal)"
  b <- read.csv(TAB("44_event_study_base_universal_sincov.csv")); b$esp <- "Sin covariables"
  d <- rbind(a, b); d$esp <- factor(d$esp, levels = c("Con covariables (principal)", "Sin covariables"))
  p <- ggplot(d, aes(e, att, shape = esp, color = esp)) +
    geom_hline(yintercept = 0, linewidth = 0.3) + geom_vline(xintercept = -0.5, linetype = 2, linewidth = 0.3) +
    geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup), position = position_dodge(0.45), size = 0.3, linewidth = 0.4, na.rm = TRUE) +
    scale_color_manual(values = c(COL$a, COL$b)) + scale_x_continuous(breaks = -5:5, labels = function(x) num(x, 0)) +
    scale_y_continuous(labels = function(x) num(x, 0)) +
    labs(x = lab_e, y = lab_ha) + tema_aer
  guardar_fig(p, "figura_6_1")
})

hacer("figura_6_2", {
  d <- read.csv(TAB("59_estudio_eventos_cs_vs_imputacion.csv"), stringsAsFactors = FALSE)
  d$est <- ifelse(grepl("Callaway", d$estimador), paste0("Callaway y Sant\u2019Anna (referencia g ", MENOS, " 1)"),
                  "Imputaci\u00f3n (referencia: todo el periodo previo)")
  p <- ggplot(d, aes(e, att, shape = est, color = est)) +
    geom_hline(yintercept = 0, linewidth = 0.3) + geom_vline(xintercept = -0.5, linetype = 2, linewidth = 0.3) +
    geom_pointrange(aes(ymin = att - 1.96 * ee, ymax = att + 1.96 * ee), position = position_dodge(0.45),
                    size = 0.3, linewidth = 0.4, na.rm = TRUE) +
    scale_color_manual(values = c(COL$a, COL$b)) + scale_x_continuous(breaks = -5:5, labels = function(x) num(x, 0)) +
    scale_y_continuous(labels = function(x) num(x, 0)) +
    labs(x = lab_e, y = lab_ha) + tema_aer
  guardar_fig(p, "figura_6_2")
})

hacer("figura_6_5", {
  c58 <- read.csv(TAB("58_composicion_por_cohorte.csv"))
  niv <- c("Nunca operaron", "Registrados sin cr\u00e9ditos", "Con cr\u00e9ditos", "Sin estado en Verra")
  d <- rbind(data.frame(G = c58$G_dep, nivel = niv[1], n = c58$solo_S1),
             data.frame(G = c58$G_dep, nivel = niv[2], n = c58$S2 - c58$E),
             data.frame(G = c58$G_dep, nivel = niv[3], n = c58$E),
             data.frame(G = c58$G_dep, nivel = niv[4], n = c58$sin_estado))
  d$nivel <- factor(d$nivel, levels = niv)
  p <- ggplot(d, aes(factor(G), n, fill = nivel)) + geom_col(width = 0.7, color = "black", linewidth = 0.2) +
    scale_fill_manual(values = COL$escalera) +
    labs(x = "Cohorte de adopci\u00f3n", y = "Municipios tratados") + tema_aer +
    guides(fill = guide_legend(nrow = 2))
  guardar_fig(p, "figura_6_5", alto = 3.8)
})

nombre_propio <- function(x) {
  x <- tools::toTitleCase(tolower(x))
  for (w in c("De", "Del", "La", "Las", "Los", "Y", "El")) x <- gsub(paste0(" ", w, " "), paste0(" ", tolower(w), " "), x)
  x
}
hacer("figura_6_3", {
  l <- read.csv(TAB("50_leave_one_out.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  l$etq <- ifelse(is.na(l$NOMBRE_MPI) | l$NOMBRE_MPI == "", "Cartagena del Chair\u00e1 y San Jos\u00e9 del Guaviare",
                  nombre_propio(l$NOMBRE_MPI))
  l$etq <- paste0(l$etq, " (", l$cohorte, ")")
  l <- l[order(l$att_g1), ]; l$etq <- factor(l$etq, levels = l$etq)
  p <- ggplot(l, aes(att_g1, etq)) +
    annotate("rect", xmin = REF_ATT_DR - REF_EE_DR, xmax = REF_ATT_DR + REF_EE_DR, ymin = -Inf, ymax = Inf, fill = COL$banda) +
    geom_vline(xintercept = REF_ATT_DR, linewidth = 0.4, color = COL$a) + geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3) +
    geom_point(size = 1.8, color = COL$a) + scale_x_continuous(labels = function(x) num(x, 0)) +
    labs(x = "ATT sin el municipio indicado (ha por a\u00f1o)", y = NULL) + tema_aer
  guardar_fig(p, "figura_6_3", alto = 4.2)
})

hacer("figura_6_4", {
  o <- readRDS(file.path(DIR_OUT, "44_honestdid.rds"))
  h <- o$resultados$dr$honestdid$promedio_0_5
  if (is.null(h)) stop("44_honestdid.rds no tiene la estructura esperada")
  rm <- data.frame(x = h$magnitudes_relativas$Mbar, lb = h$magnitudes_relativas$lb, ub = h$magnitudes_relativas$ub,
                   panel = "Magnitudes~relativas~(bar(M))")
  sd <- data.frame(x = h$suavidad$M, lb = h$suavidad$lb, ub = h$suavidad$ub,
                   panel = "Suavidad~(italic(M)*\", ha por a\u00f1o\")")
  d <- rbind(rm, sd)
  p <- ggplot(d, aes(x, ymin = lb, ymax = ub)) + geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_errorbar(width = 0, linewidth = 0.6, color = COL$a) +
    facet_wrap(~panel, scales = "free_x", labeller = label_parsed) + scale_y_continuous(labels = function(x) num(x, 0)) +
    scale_x_continuous(labels = function(x) ifelse(abs(x - round(x)) < 1e-9, num(x, 0), num(x, 1))) +
    labs(x = NULL, y = "Intervalo robusto al 95 % (ha por a\u00f1o)") + tema_aer
  guardar_fig(p, "figura_6_4", alto = 3.5)
})


hacer("figura_6_7", {
  e <- read.csv(TAB("57_efecto_minimo_detectable.csv"), stringsAsFactors = FALSE)
  e <- e[e$resultado == "def_p1", ]
  e$coh <- ifelse(e$cohorte == "todas", "Todas", e$cohorte)
  e$coh <- factor(e$coh, levels = c(sort(setdiff(e$coh, "Todas")), "Todas"))
  d <- rbind(data.frame(coh = e$coh, serie = "P\u00e9rdida previa de las celdas tratadas", v = e$media_pre),
             data.frame(coh = e$coh, serie = "Efecto m\u00ednimo detectable", v = e$emd))
  d$serie <- factor(d$serie, levels = c("P\u00e9rdida previa de las celdas tratadas", "Efecto m\u00ednimo detectable"))
  et <- data.frame(coh = e$coh, v = e$emd, txt = paste0(num(e$emd_rel_pct, 0), " %"))
  p <- ggplot(d, aes(coh, v, fill = serie)) +
    geom_col(position = position_dodge(0.75), width = 0.7) +
    geom_text(data = et, aes(coh, v, label = txt), inherit.aes = FALSE, nudge_x = 0.19, vjust = -0.4,
              size = 3, family = FUENTE) +
    scale_fill_manual(values = c("grey70", COL$b)) +
    scale_y_continuous(labels = function(x) num(x, 1), expand = expansion(mult = c(0, 0.08))) +
    labs(x = "Cohorte de adopci\u00f3n", y = "Hect\u00e1reas por celda y a\u00f1o") + tema_aer
  guardar_fig(p, "figura_6_7", alto = 3.8)
})

hacer("figura_6_6", {
  d <- read.csv(TAB("56_event_study_celdas.csv"), stringsAsFactors = FALSE)
  d <- d[d$modelo == "def_p1", ]
  p <- ggplot(d, aes(e, att)) +
    geom_hline(yintercept = 0, linewidth = 0.3) + geom_vline(xintercept = -0.5, linetype = 2, linewidth = 0.3) +
    geom_pointrange(aes(ymin = att - 1.96 * ee, ymax = att + 1.96 * ee), color = COL$a, size = 0.3, linewidth = 0.4) +
    scale_x_continuous(breaks = sort(unique(d$e)), labels = function(x) num(x, 0)) +
    scale_y_continuous(labels = function(x) num(x, 1)) +
    labs(x = lab_e, y = "Efecto sobre la p\u00e9rdida de bosque (ha por celda y a\u00f1o)") + tema_aer
  guardar_fig(p, "figura_6_6", alto = 3.8)
})

## =============================================================================
## Preambulo LaTeX e indice
## =============================================================================
writeLines(c("% Paquetes que necesitan las tablas de scripts/70_tablas_figuras.R",
             "\\usepackage[utf8]{inputenc}", "\\usepackage[T1]{fontenc}", "\\usepackage[spanish,es-noshorthands]{babel}",
             "\\usepackage{booktabs}", "\\usepackage{threeparttable}",
             "\\usepackage[labelformat=empty,font=small,skip=6pt]{caption}  % el titulo ya incluye 'Tabla X.Y--'"),
           file.path(DIR_T, "preambulo.tex"))

titulo("RESUMEN")
for (k in names(productos)) cat(sprintf("  %-22s %s\n", k, productos[[k]]))
cat("\nTablas:  ", DIR_T, "\nFiguras: ", DIR_F, "\n")
if (any(grepl("^ERROR", unlist(productos)))) quit(status = 1)
