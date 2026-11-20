## =============================================================================
## 79_dosis_endogeneidad.R
##
## Respuesta al segundo jurado (pp. 10 y 12):
##   (a) La dosis puede ser endogena: el desarrollador elige el area y la
##       ubicacion del proyecto, posiblemente segun el bosque disponible o la
##       presion de deforestacion esperada. Fijar el denominador en 2000 no
##       resuelve la seleccion del numerador. Se reporta la correlacion de
##       Spearman de cada dosis con la deforestacion previa y las covariables
##       de linea base, entre los tratados con dosis.
##   (b) ¿Por que terciles? Histograma de cada dosis con los cortes de tercil.
##
## Por que Spearman: con 26 tratados y dosis acotadas en [0, 1] y muy
## asimetricas, la correlacion de rangos no depende de unos pocos valores
## extremos ni supone linealidad. Con n = 26, una correlacion de 0,39 es la
## minima significativa al 5 %, asi que el ejercicio solo detecta asociaciones
## fuertes: se reporta como diagnostico, no como prueba de exogeneidad.
##
## USO (PowerShell, desde tesis_gfc/):  Rscript scripts/79_dosis_endogeneidad.R
## Insumos: data/interim/dosis_tratamiento_poligonos.csv (76),
##          outputs/40_muestra_A.rds,
##          data/interim/territorios_colectivos_municipio.csv (62)
## Salidas: outputs/tablas/79_dosis_correlaciones.csv
##          outputs/tablas/79_dosis_terciles.csv
##          outputs/tesis/figuras/figura_dosis_histograma.png / .pdf
## =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })

RAIZ <- local({
  d <- normalizePath(getwd(), winslash = "/")
  repeat {
    if (file.exists(file.path(d, "data/.raiz_canonica"))) break
    p <- dirname(d); if (p == d) stop("No encuentro data/.raiz_canonica: corre desde tesis_gfc/"); d <- p
  }
  d
})
DIR_TAB <- file.path(RAIZ, "outputs/tablas"); DIR_F <- file.path(RAIZ, "outputs/tesis/figuras")
for (d in c(DIR_TAB, DIR_F)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
if (!isTRUE(l10n_info()$`UTF-8`))
  suppressWarnings(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "Spanish_Colombia.utf8" else "C.UTF-8"))
pad5 <- function(x) formatC(as.integer(x), width = 5, flag = "0")

## --- Datos ---------------------------------------------------------------------
dos <- read.csv(file.path(RAIZ, "data/interim/dosis_tratamiento_poligonos.csv"),
                colClasses = c(COD_DANE = "character"), encoding = "UTF-8")
dos$COD_DANE <- pad5(dos$COD_DANE)
dos$principal <- toupper(as.character(dos$principal)) %in% c("TRUE", "1")
dos <- dos %>% filter(principal, tiene_dosis == 1)
cat("Tratados de la muestra principal con dosis:", nrow(dos), "\n")
if (nrow(dos) < 10) stop("Muy pocos tratados con dosis: revisa dosis_tratamiento_poligonos.csv")

## Covariables de linea base: una fila por municipio (son invariantes en el tiempo)
cov <- readRDS(file.path(RAIZ, "outputs/40_muestra_A.rds")) %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>% group_by(COD_DANE) %>% slice(1) %>% ungroup()
VARS <- c(log_defor_pre_media = "Deforestación previa (log)",
          defor_pre_tendencia = "Tendencia de la deforestación previa",
          log_baseline_forest = "Bosque en 2000 (log)",
          pct_sinap_pub_base = "Área en el SINAP público",
          tc_total_hoy = "Área en territorios colectivos",
          distancia_mercado_base = "Distancia al mercado",
          discapital_base = "Distancia a la capital departamental",
          H_coca_base = "Coca",
          homicidios_base = "Homicidios",
          temp_media_c_base = "Temperatura media",
          prec_anual_mm_base = "Precipitación anual")
ruta_tc <- file.path(RAIZ, "data/interim/territorios_colectivos_municipio.csv")
if (file.exists(ruta_tc)) {
  tc <- read.csv(ruta_tc, colClasses = c(COD_DANE = "character")); tc$COD_DANE <- pad5(tc$COD_DANE)
  cov <- cov %>% select(-any_of("tc_total_hoy")) %>% left_join(tc[, c("COD_DANE", "tc_total_hoy")], by = "COD_DANE")
}
faltan <- setdiff(names(VARS), names(cov))
if (length(faltan)) { warning("Sin estas variables, se omiten: ", paste(faltan, collapse = ", ")); VARS <- VARS[!names(VARS) %in% faltan] }
d <- dos %>% select(COD_DANE, NOMBRE_MPI, dosis_bosque, dosis_area) %>%
  inner_join(cov[, c("COD_DANE", names(VARS))], by = "COD_DANE")
cat("Con covariables:", nrow(d), "\n")

## --- (a) Correlaciones de Spearman --------------------------------------------
corr <- lapply(c("dosis_bosque", "dosis_area"), function(dz) {
  lapply(names(VARS), function(v) {
    ok <- complete.cases(d[[dz]], d[[v]])
    ct <- suppressWarnings(cor.test(d[[dz]][ok], d[[v]][ok], method = "spearman", exact = FALSE))
    data.frame(dosis = dz, variable = v, etiqueta = VARS[[v]], n = sum(ok),
               rho = unname(ct$estimate), p_valor = ct$p.value)
  }) %>% bind_rows()
}) %>% bind_rows()
corr$significativa_5 <- corr$p_valor < 0.05
write.csv(corr, file.path(DIR_TAB, "79_dosis_correlaciones.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\nCorrelaciones de Spearman (tratados con dosis):\n")
print(corr %>% mutate(rho = round(rho, 2), p_valor = round(p_valor, 3)) %>% select(dosis, etiqueta, n, rho, p_valor), row.names = FALSE)
cat(sprintf("Umbral de significancia al 5 %% con n = %d: |rho| > %.2f aprox.\n", nrow(d), 1.96 / sqrt(nrow(d) - 1)))

## --- (b) Distribucion y cortes de tercil --------------------------------------
## Cortes: los de los terciles que usa la tesis (columnas tercil_dosis y
## tercil_area del 76), como punto medio entre grupos contiguos; si faltan,
## los cuantiles muestrales.
d <- d %>% left_join(dos[, c("COD_DANE", intersect(c("tercil_dosis", "tercil_area"), names(dos)))], by = "COD_DANE")
COL_TERCIL <- c(dosis_bosque = "tercil_dosis", dosis_area = "tercil_area")
cortes <- lapply(c("dosis_bosque", "dosis_area"), function(dz) {
  tcol <- COL_TERCIL[[dz]]
  q <- if (tcol %in% names(d) && all(c("bajo", "medio", "alto") %in% d[[tcol]])) {
    r <- function(k, f) f(d[[dz]][d[[tcol]] == k])
    c((r("bajo", max) + r("medio", min)) / 2, (r("medio", max) + r("alto", min)) / 2)
  } else quantile(d[[dz]], c(1/3, 2/3), na.rm = TRUE, type = 7)
  data.frame(dosis = dz, corte_1 = q[1], corte_2 = q[2], min = min(d[[dz]]), mediana = median(d[[dz]]),
             max = max(d[[dz]]), n_mayor_090 = sum(d[[dz]] > 0.90), n = sum(!is.na(d[[dz]])))
}) %>% bind_rows()
write.csv(cortes, file.path(DIR_TAB, "79_dosis_terciles.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(cortes, row.names = FALSE)

FUENTE <- if (.Platform$OS.type == "windows") "Times New Roman" else "Liberation Serif"
tema_aer <- theme_classic(base_size = 10, base_family = FUENTE) +
  theme(axis.line = element_line(linewidth = 0.3), axis.ticks = element_line(linewidth = 0.3),
        strip.background = element_blank(), strip.text = element_text(face = "italic"),
        panel.spacing = unit(2.5, "lines"), plot.margin = margin(6, 10, 6, 6))
largo <- bind_rows(
  data.frame(medida = "Dosis de bosque", valor = d$dosis_bosque),
  data.frame(medida = "Dosis de área", valor = d$dosis_area))
lin <- data.frame(medida = rep(c("Dosis de bosque", "Dosis de área"), each = 2),
                  corte = c(cortes$corte_1[1], cortes$corte_2[1], cortes$corte_1[2], cortes$corte_2[2]))
coma <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
p <- ggplot(largo, aes(valor)) +
  geom_histogram(breaks = seq(0, 1, 0.1), fill = "#0072B2", colour = "white", linewidth = 0.3) +
  geom_vline(data = lin, aes(xintercept = corte), linetype = "dashed", linewidth = 0.4, colour = "grey30") +
  facet_wrap(~ medida) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = coma, expand = c(0, 0.01)) +
  scale_y_continuous(breaks = function(l) seq(0, ceiling(l[2]), 2), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Proporción del municipio (área o bosque de 2000) cubierta por proyectos REDD+",
       y = "Municipios tratados") +
  tema_aer
png_dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
ggsave(file.path(DIR_F, "figura_dosis_histograma.png"), p, width = 6.5, height = 3.2, dpi = 300, device = png_dev)
tryCatch(ggsave(file.path(DIR_F, "figura_dosis_histograma.pdf"), p, width = 6.5, height = 3.2, device = cairo_pdf),
         error = function(e) cat("(sin PDF: cairo no disponible)\n"))
cat("\nGuardado: outputs/tablas/79_dosis_correlaciones.csv, 79_dosis_terciles.csv y figura_dosis_histograma\n")
