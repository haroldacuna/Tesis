## =============================================================================
## 57_figura_mde_celdas.R
##
## Dos piezas para el Capitulo 6 a partir de las salidas del 56 (analisis a
## nivel de poligono, estimador principal = regresion del resultado):
##
##   1. Figura del estudio de eventos: principal (reg) y fuga, en ha por celda
##      de 400 ha, IC 95 % puntuales. Referencias:
##        - linea en -media previa: la reduccion del 100 % (deforestacion cero)
##        - sombreado desde e = 6: la cohorte 2018 deja de aportar
##          (2018 + 6 = 2024 > 2023), con ella la mayoria de las celdas
##   2. Efecto minimo detectable (EMD) por cohorte y global:
##        EMD = (z_0,975 + z_0,80) * EE  ~  2,80 * EE
##        EMD relativo = EMD / media previa de la cohorte
##      EMD relativo > 100 %: ni eliminar toda la deforestacion seria
##      detectable con este diseno.
##
## Los rotulos usan escapes Unicode para que los acentos no se corrompan al
## guardar la figura en Windows.
##
## USO
##   Rscript scripts/57_figura_mde_celdas.R
## Insumos: data/interim/celdas/{celdas_atributos.csv, panel_celdas_nocero.csv.gz}
##          outputs/tablas/56_{event_study_celdas, att_por_cohorte, resultados_celdas}.csv
## =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

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
DIR_TAB <- file.path(RAIZ_PROYECTO, "outputs/tablas")
DIR_FIG <- file.path(RAIZ_PROYECTO, "outputs/figuras")
dir.create(DIR_FIG, showWarnings = FALSE, recursive = TRUE)

COHORTES <- c(2013, 2014, 2015, 2017, 2018)     # las mismas del 56
ANIO_FIN <- 2023
FACTOR_EMD <- qnorm(0.975) + qnorm(0.80)        # 2,8016

res <- fread(file.path(DIR_TAB, "56_resultados_celdas.csv"))

## --- Media previa por cohorte (misma definicion que el 56: celda-anios con 0) --
leer_gz_memoria <- function(ruta) {
  con <- gzfile(ruta, open = "rt"); on.exit(close(con))
  fread(text = readLines(con), showProgress = FALSE)
}
at  <- fread(file.path(DIR_CEL, "celdas_atributos.csv"), colClasses = list(character = "COD_DANE"))
pan <- leer_gz_memoria(file.path(DIR_CEL, "panel_celdas_nocero.csv.gz"))
tr  <- at[grupo == "tratada" & !is.na(COD_DANE) & COD_DANE != "" & cohorte %in% COHORTES,
          .(celda, cohorte, bosque2000_ha)]
pre <- pan[tr, on = "celda", nomatch = 0][anio < cohorte & anio <= ANIO_FIN]
pre[, tasa := 100 * def_p1 / bosque2000_ha]
## (Dentro de un grupo, `cohorte` es un escalar: los celda-anios previos son
##  .N * (cohorte - 2001), no sum(cohorte - 2001).)
media_pre <- tr[, .(celdas = .N, celda_anios_pre = .N * (cohorte - 2001)), by = cohorte][
  pre[, .(suma_ha = sum(def_p1), suma_tasa = sum(tasa)), by = cohorte], on = "cohorte"]
media_pre[, `:=`(media_pre_ha = suma_ha / celda_anios_pre, media_pre_tasa = suma_tasa / celda_anios_pre)]
global_pre_ha <- media_pre[, sum(suma_ha) / sum(celda_anios_pre)]
global_pre_tasa <- media_pre[, sum(suma_tasa) / sum(celda_anios_pre)]

## --- Tabla de EMD ----------------------------------------------------------------
coh <- fread(file.path(DIR_TAB, "56_att_por_cohorte.csv"))[metodo == "reg"]
glob <- res[metodo == "reg" & grepl("^principal", analisis) & resultado %in% c("def_p1", "tasa_pct")]

emd <- rbind(
  coh[, .(resultado, cohorte = as.character(cohorte), proyectos, celdas, att, ee)],
  glob[, .(resultado, cohorte = "todas", proyectos = conglomerados_tratados, celdas = celdas_tratadas, att, ee)])
emd[, media_pre := fifelse(resultado == "def_p1",
                           fifelse(cohorte == "todas", global_pre_ha,
                                   media_pre$media_pre_ha[match(cohorte, as.character(media_pre$cohorte))]),
                           fifelse(cohorte == "todas", global_pre_tasa,
                                   media_pre$media_pre_tasa[match(cohorte, as.character(media_pre$cohorte))]))]
emd[, `:=`(emd = FACTOR_EMD * ee,
           att_rel_pct = 100 * att / media_pre,
           emd_rel_pct = 100 * FACTOR_EMD * ee / media_pre)]
emd[, detectable_reduccion_total := emd <= media_pre]
setorder(emd, resultado, cohorte)
fwrite(emd, file.path(DIR_TAB, "57_efecto_minimo_detectable.csv"))
cat(sprintf("Media previa global: %.3f ha/celda/anio (%.3f %% del bosque 2000)\n", global_pre_ha, global_pre_tasa))
media_56 <- res[metodo == "reg" & resultado == "def_p1" & grepl("^principal", analisis), media_pre_tratadas]
if (length(media_56) && abs(global_pre_ha - media_56) > 0.005)
  stop(sprintf("La media previa (%.3f) no coincide con la del 56 (%.3f): revisar insumos.", global_pre_ha, media_56))
print(emd[, .(resultado, cohorte, proyectos, celdas, media_pre = round(media_pre, 3), att = round(att, 3),
              ee = round(ee, 3), emd = round(emd, 3), att_rel_pct = round(att_rel_pct, 1),
              emd_rel_pct = round(emd_rel_pct, 0), detectable_reduccion_total)], row.names = FALSE)

## --- Figura del estudio de eventos ---------------------------------------------
es <- fread(file.path(DIR_TAB, "56_event_study_celdas.csv"))[modelo %in% c("def_p1", "fuga")]
es[, serie := fifelse(modelo == "def_p1",
                      "Celdas de proyecto vs. control (efecto directo)",
                      "Celdas vecinas (\u2264 10 km) vs. control (fuga)")]
es[, `:=`(lo = att - 1.96 * ee, hi = att + 1.96 * ee)]
e_sin_2018 <- ANIO_FIN - max(COHORTES) + 1       # 6
pct_2018 <- round(100 * media_pre[cohorte == 2018, celdas] / media_pre[, sum(celdas)])

g <- ggplot(es, aes(e, att, colour = serie)) +
  annotate("rect", xmin = e_sin_2018 - 0.5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.10) +
  geom_hline(yintercept = 0, colour = "grey30") +
  geom_hline(yintercept = -global_pre_ha, linetype = "dashed", colour = "firebrick") +
  annotate("text", x = min(es$e), y = -global_pre_ha, vjust = 1.6, hjust = 0, size = 3, colour = "firebrick",
           label = sprintf("Reducci\u00f3n del 100 %% (\u2212%s ha)",
                           formatC(global_pre_ha, format = "f", digits = 2, decimal.mark = ","))) +
  geom_vline(xintercept = -0.5, linetype = "dotted", colour = "grey50") +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.5), size = 0.3) +
  coord_cartesian(ylim = c(-2, 1.5)) +
  scale_colour_manual(values = c("#1f4e79", "#c07f00")) +
  labs(title = "Estudio de eventos a nivel de pol\u00edgono (celdas de 2 \u00d7 2 km)",
       subtitle = paste0("Conversi\u00f3n de bosque estable a uso antr\u00f3pico persistente, ha por celda de 400 ha. ",
                         "Regresi\u00f3n del resultado sobre muestra emparejada;\n",
                         "IC 95 % puntuales, errores agrupados por proyecto (25 conglomerados tratados). ",
                         "El eje vertical se recorta en [\u22122; 1,5]."),
      x = "A\u00f1os desde el inicio del proyecto", y = "ATT (ha por celda y a\u00f1o)", colour = NULL,
      caption = sprintf(paste0("Zona sombreada: la cohorte 2018 (%d %% de las celdas tratadas) deja de aportar ",
                        "desde el a\u00f1o %d; esos coeficientes no son comparables con los anteriores."),
                  pct_2018, e_sin_2018)) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9))
ggsave(file.path(DIR_FIG, "57_event_study_celdas.png"), g, width = 10, height = 6, dpi = 200)
recortados <- es[lo < -2 | hi > 1.5, .(serie, e)]
if (nrow(recortados)) cat("Intervalos que exceden el recorte del eje (se ven truncados):",
                          paste0(recortados$e, collapse = ", "), "\n")
cat("Guardado: outputs/figuras/57_event_study_celdas.png, outputs/tablas/57_efecto_minimo_detectable.csv\n")
