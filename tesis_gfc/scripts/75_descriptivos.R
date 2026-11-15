## =============================================================================
## 75_descriptivos.R
##
## Analisis descriptivo previo a los modelos (comentario de Jorge C43: "reforzar
## el analisis descriptivo previo a los modelos"; tareas 2.2, 2.4 y 2.5 del plan).
##
##   A. Flujo de la muestra: 1.122 -> tratados, fallidos, fuera de ventana ->
##      nunca tratados -> controles limpios (por fuente del vinculo).
##   B. Trayectorias brutas, sin modelo: perdida media por anio de los tratados
##      tempranos (2013-2016), tardios (2017-2019) y los controles, en nivel e
##      indice (2001-2009 = 100), y en tiempo relativo a la adopcion. Muestra el
##      pico previo a la adopcion sin ningun supuesto de estimacion.
##   C. Dentro del poligono frente al resto del mismo municipio: concilia que
##      los municipios tratados tengan alta deforestacion (Tabla 3) con que los
##      poligonos tengan poca (Tabla 8). Celdas de 2 x 2 km con >= 50 % de bosque
##      en 2000, en los municipios que contienen celdas tratadas.
##
## Salidas:  outputs/tablas/75_flujo_muestra.csv, 75_trayectorias.csv,
##           75_poligono_vs_municipio.csv; outputs/figuras/75_trayectorias.png
## USO (desde tesis_gfc/):  Rscript scripts/75_descriptivos.R
## =============================================================================

suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
source("scripts/00_config_did.R")
TEMPRANAS <- 2013:2016; TARDIAS <- 2017:2019; PRE <- 2001:2009

## -----------------------------------------------------------------------------
## A. Flujo de la muestra
## -----------------------------------------------------------------------------
titulo("A. FLUJO DE LA MUESTRA")
pt <- fread(RUTA_PANEL_TRAT, colClasses = c(COD_DANE = "character"))
pt[, COD_DANE := pad5(COD_DANE)]
mun <- pt[, .(G_dep  = min_na(anio_inicio_tratamiento_todas_fuentes_redd_depurada),
              G_orig = min_na(anio_inicio_tratamiento_todas_fuentes_redd),
              muestra_dep = min(muestra_redd_depurada)), by = COD_DANE]
source(file.path(RAIZ_PROYECTO, "scripts/comun_controles_limpios.R"))
cont <- as.data.table(municipios_con_proyecto(RUTA_PANEL_TRAT, verbose = FALSE))
mun <- merge(mun, cont[, .(COD_DANE, contaminado)], by = "COD_DANE", all.x = TRUE)
n_total   <- nrow(mun)
## muestra_redd_depurada == 0: municipios cuyo unico vinculo REDD+ es un proyecto
## retirado o rechazado (salen; no son tratados ni controles)
n_fallido <- mun[muestra_dep == 0, .N]
n_trat    <- mun[muestra_dep == 1 & !is.na(G_dep), .N]
n_fuera   <- mun[muestra_dep == 1 & !is.na(G_dep) & !G_dep %in% VENTANA, .N]
n_nunca   <- mun[muestra_dep == 1 & is.na(G_dep), .N]
n_sucios  <- mun[muestra_dep == 1 & is.na(G_dep) & contaminado %in% TRUE, .N]
flujo <- data.table(
  paso = c("Municipios del panel", "Tratados (definicion principal)", "  de ellos, fuera de 2013-2019",
           "Excluidos: unico proyecto retirado o rechazado", "Nunca tratados",
           "  menos: con algun vinculo con proyectos de carbono", "Controles limpios",
           "Muestra principal: tratados 2013-2019 + controles limpios"),
  municipios = c(n_total, n_trat, n_fuera, n_fallido, n_nunca, n_sucios, n_nunca - n_sucios,
                 (n_trat - n_fuera) + (n_nunca - n_sucios)))
print(flujo)
fwrite(flujo, file.path(DIR_TAB, "75_flujo_muestra.csv"))
if (n_nunca - n_sucios != REF_N_CONTROLES || n_trat - n_fuera != REF_N_TRATADOS)
  cat("[!] El flujo no reproduce 36 tratados y 986 controles: revisar antes de citarlo.\n")

## -----------------------------------------------------------------------------
## B. Trayectorias brutas
## -----------------------------------------------------------------------------
titulo("B. TRAYECTORIAS BRUTAS")
m <- as.data.table(cargar_muestra_principal(verificar = TRUE))
m[, grupo := fifelse(G_dep == 0, "Controles", fifelse(G_dep %in% TEMPRANAS, "Tratados 2013-2016", "Tratados 2017-2019"))]
tr <- m[, .(perdida = mean(get(OUTCOME)), municipios = uniqueN(COD_DANE)), by = .(grupo, year)]
tr[, indice := 100 * perdida / mean(perdida[year %in% PRE]), by = grupo]
fwrite(tr, file.path(DIR_TAB, "75_trayectorias.csv"))
print(dcast(tr, year ~ grupo, value.var = "perdida")[, lapply(.SD, round, 1)])

## Tasa relativa al bosque: la perdida en hectareas escala con el tamanio del
## municipio; la tasa (perdida / bosque 2000) mide la presion por hectarea.
tasa <- m[year %in% PRE, .(perdida = mean(get(OUTCOME)), bosque = baseline_forest[1]), by = .(COD_DANE, grupo)]
tasa <- tasa[bosque > 0, .(municipios = .N, perdida_media_ha = mean(perdida), bosque_medio_ha = mean(bosque),
                           tasa_media_pct = 100 * mean(perdida / bosque), tasa_agregada_pct = 100 * sum(perdida) / sum(bosque)),
             by = grupo]
cat("\nPerdida 2001-2009 en hectareas y como tasa del bosque de 2000:\n"); print(tasa[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
fwrite(tasa, file.path(DIR_TAB, "75_tasa_relativa.csv"))

## Tiempo relativo a la adopcion: perdida media de los tratados en cada e,
## indexada a su propio promedio de e = -5..-1
ev <- m[G_dep > 0, .(e = year - G_dep, y = get(OUTCOME), grupo, COD_DANE)]
ev <- ev[e >= -8 & e <= 5, .(perdida = mean(y), municipios = uniqueN(COD_DANE)), by = .(grupo, e)]
ev[, indice := 100 * perdida / mean(perdida[e %in% -5:-1]), by = grupo]
fwrite(ev, file.path(DIR_TAB, "75_trayectorias_evento.csv"))
cat("\nTiempo relativo (indice, e = -5..-1 = 100):\n")
print(dcast(ev, e ~ grupo, value.var = "indice")[, lapply(.SD, round, 0)])

cols <- c("Controles" = "grey45", "Tratados 2013-2016" = "#2E6B4F", "Tratados 2017-2019" = "#B4532A")
pA <- ggplot(tr, aes(year, indice, colour = grupo)) +
  annotate("rect", xmin = 2012.5, xmax = 2019.5, ymin = -Inf, ymax = Inf, fill = "grey92") +
  annotate("text", x = 2016, y = Inf, label = "Ventana de adopci\u00f3n", vjust = 1.4, size = 2.6, colour = "grey35") +
  geom_line(linewidth = 0.7) + geom_point(size = 1.2) + scale_colour_manual(values = cols, name = NULL) +
  scale_x_continuous(breaks = c(2001, 2005, 2009, 2013, 2017, 2021, 2024)) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(x = NULL, y = "\u00cdndice de p\u00e9rdida (2001\u20132009 = 100)", title = "A. Por a\u00f1o calendario") +
  theme_classic(base_size = 9) + theme(legend.position = "bottom")
pB <- ggplot(ev, aes(e, indice, colour = grupo)) +
  geom_vline(xintercept = -0.5, linetype = 2, colour = "grey40") +
  geom_line(linewidth = 0.7) + geom_point(size = 1.2) + scale_colour_manual(values = cols, name = NULL) +
  scale_x_continuous(breaks = -8:5) + guides(colour = guide_legend(nrow = 2)) +
  labs(x = "A\u00f1os desde la adopci\u00f3n", y = "\u00cdndice de p\u00e9rdida (e = \u22125 a \u22121 = 100)",
       title = "B. Tratados, en tiempo relativo a la adopci\u00f3n") +
  theme_classic(base_size = 9) + theme(legend.position = "bottom")
if (requireNamespace("gridExtra", quietly = TRUE)) {
  g <- gridExtra::arrangeGrob(pA, pB, ncol = 2)
  ggsave(file.path(DIR_FIG, "75_trayectorias.png"), g, width = 7.5, height = 3.6, dpi = 300)
}

## -----------------------------------------------------------------------------
## C. Dentro del poligono frente al resto del municipio
## -----------------------------------------------------------------------------
titulo("C. POLIGONO FRENTE AL RESTO DEL MUNICIPIO")
DIR_CEL <- file.path(RAIZ_PROYECTO, "data/interim/celdas")
cel <- fread(file.path(DIR_CEL, "celdas_atributos.csv"), colClasses = c(COD_DANE = "character"))
cel <- cel[pct_bosque2000 >= 0.5 & !is.na(COD_DANE)]
mun_trat <- unique(cel[grupo == "tratada", COD_DANE])
cel <- cel[COD_DANE %in% mun_trat]
cel[, zona := fifelse(grupo == "tratada", "Dentro del pol\u00edgono",
                fifelse(ha_ad == 0 & ha_otros_proyectos == 0, "Resto del municipio", NA_character_))]
cel <- cel[!is.na(zona)]
pan <- fread(file.path(DIR_CEL, "panel_celdas_nocero.csv.gz"), select = c("celda", "anio", "def_p1"))
pan <- pan[celda %in% cel$celda & anio %in% 2001:2012]
pan <- pan[CJ(celda = cel$celda, anio = 2001:2012), on = .(celda, anio)][is.na(def_p1), def_p1 := 0]
pre <- pan[, .(perdida = mean(def_p1)), by = celda]
cel <- merge(cel, pre, by = "celda")
cel[, tasa_pct := 100 * perdida / bosque2000_ha]
res <- cel[, .(celdas = .N, municipios = uniqueN(COD_DANE), bosque_ha = sum(bosque2000_ha),
               perdida_ha_celda = mean(perdida), tasa_pct = 100 * sum(perdida) / sum(bosque2000_ha),
               perdida_total_ha = sum(perdida)), by = zona]
res[, pct_bosque := 100 * bosque_ha / sum(bosque_ha)][, pct_perdida := 100 * perdida_total_ha / sum(perdida_total_ha)]
print(res[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
## Por municipio: razon de tasas dentro / resto
pm <- cel[, .(tasa = 100 * sum(perdida) / sum(bosque2000_ha)), by = .(COD_DANE, zona)]
pm <- dcast(pm, COD_DANE ~ zona, value.var = "tasa")
setnames(pm, c("COD_DANE", "dentro", "resto"))
pm <- pm[is.finite(dentro) & is.finite(resto)]
cat(sprintf("\nMunicipios con ambas zonas: %d | tasa dentro < resto en %d (%.0f %%) | mediana dentro/resto = %.2f\n",
            nrow(pm), sum(pm$dentro < pm$resto), 100 * mean(pm$dentro < pm$resto),
            median(pm$dentro / pmax(pm$resto, 1e-9))))
fwrite(res, file.path(DIR_TAB, "75_poligono_vs_municipio.csv"))
fwrite(pm, file.path(DIR_TAB, "75_poligono_vs_municipio_por_mpio.csv"))
cat("\nGuardado: outputs/tablas/75_*.csv | figuras/75_trayectorias.png\n")
