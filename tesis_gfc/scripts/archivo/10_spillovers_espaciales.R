## =============================================================================
## 10_spillovers_espaciales.R
##
## Objetivo específico 5: spillovers espaciales / leakage.
##
## Pregunta: ¿la deforestación se "escapa" hacia municipios VECINOS de uno
## tratado (fuga o leakage), o disminuye también en los vecinos (efecto
## protector indirecto, ej. por mayor vigilancia/monitoreo regional)?
##
## Estrategia (dos enfoques complementarios):
##
##   A) DiD espacial: se restringe la muestra a municipios que NUNCA
##      recibieron un proyecto de carbono directamente. Entre esos, se
##      define una nueva cohorte: el año en que un municipio se vuelve
##      VECINO de al menos un municipio tratado (contigüidad tipo "reina" --
##      comparte al menos un punto de frontera). Se corre el mismo
##      estimador de Callaway-Sant'Anna, pero con este "tratamiento
##      indirecto" en vez del directo.
##        - ATT positivo y significativo -> evidencia de fuga (leakage)
##        - ATT negativo -> evidencia de efecto protector indirecto
##        - No significativo -> sin evidencia de spillover detectable
##
##   B) Regresión con rezago espacial: TWFE con la proporción de vecinos
##      tratados como covariable adicional junto al tratamiento directo --
##      chequeo simple y directo, complementario al enfoque A.
##
## Requiere: shapefile de municipios (data/interim/municipios_clean.gpkg)
## y el panel preparado por 01_preparar_datos_did.R.
##
## USO
## ---
##   Rscript 10_spillovers_espaciales.R
## =============================================================================

library(dplyr)
library(sf)
library(spdep)
library(did)
library(fixest)
library(readr)

DATA_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/data"
OUTPUT_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs"
PANEL_ANALISIS <- file.path(OUTPUT_ROOT, "panel_analisis_did.rds")
MUNICIPIOS_GPKG <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/data/interim/municipios_clean.gpkg"
MUNICIPIOS_LAYER <- "municipios_clean"
dir.create(file.path(OUTPUT_ROOT, "tablas"), showWarnings = FALSE, recursive = TRUE)

## =============================================================================
## 1. Construir la lista de vecinos (contigüidad tipo "reina")
## =============================================================================

cat("Cargando shapefile de municipios...\n")
municipios_sf <- st_read(MUNICIPIOS_GPKG, layer = MUNICIPIOS_LAYER, quiet = TRUE)
municipios_sf$COD_DANE <- as.character(municipios_sf$COD_DANE)
cat("Municipios en el shapefile:", nrow(municipios_sf), "\n")

# Reparar geometrias invalidas -- mismo problema que ya vimos con el clima.
n_invalidas <- sum(!st_is_valid(municipios_sf))
if (n_invalidas > 0) {
  cat("Reparando", n_invalidas, "geometrías inválidas...\n")
  municipios_sf <- st_make_valid(municipios_sf)
}

vecinos <- poly2nb(municipios_sf, queen = TRUE)
cat("Vecindad construida. Promedio de vecinos por municipio:", round(mean(card(vecinos)), 1), "\n")
n_sin_vecinos <- sum(card(vecinos) == 0)
if (n_sin_vecinos > 0) {
  cat("Aviso:", n_sin_vecinos, "municipios sin ningún vecino detectado (islas, límites de datos) -- quedan sin exposición a spillover por construcción.\n")
}

# Mapa de posición -> COD_DANE, para traducir los indices de spdep
cod_dane_por_indice <- municipios_sf$COD_DANE

## =============================================================================
## 2. Cargar el panel y calcular, para cada municipio NUNCA tratado
##    directamente, el primer año en que tiene un vecino tratado.
## =============================================================================

panel <- readRDS(PANEL_ANALISIS)

correr_spillover <- function(panel, col_first_treat, etiqueta) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("SPILLOVER --", etiqueta, "\n")
  cat(strrep("=", 70), "\n")

  primer_trat <- panel %>%
    distinct(COD_DANE, .data[[col_first_treat]]) %>%
    rename(first_treat = all_of(col_first_treat))

  cod_a_first_treat <- setNames(primer_trat$first_treat, primer_trat$COD_DANE)

  # Para cada municipio, el primer año en que ALGUNO de sus vecinos esta
  # tratado (min de los first_treat de sus vecinos, excluyendo 0 = nunca).
  primer_anio_vecino_tratado <- sapply(seq_along(vecinos), function(i) {
    cod_propio <- cod_dane_por_indice[i]
    idx_vecinos <- vecinos[[i]]
    if (length(idx_vecinos) == 0 || idx_vecinos[1] == 0) return(NA_real_)
    cods_vecinos <- cod_dane_por_indice[idx_vecinos]
    ft_vecinos <- cod_a_first_treat[cods_vecinos]
    ft_vecinos <- ft_vecinos[!is.na(ft_vecinos) & ft_vecinos > 0]
    if (length(ft_vecinos) == 0) return(0)  # ningun vecino tratado nunca
    min(ft_vecinos)
  })
  tabla_vecinos <- data.frame(COD_DANE = cod_dane_por_indice, first_treat_vecino = primer_anio_vecino_tratado)

  # Restringir la muestra a municipios NUNCA tratados directamente --
  # el spillover se mide sobre quienes no recibieron proyecto propio.
  panel_spillover <- panel %>%
    filter(.data[[col_first_treat]] == 0) %>%
    left_join(tabla_vecinos, by = "COD_DANE") %>%
    mutate(first_treat_vecino = ifelse(is.na(first_treat_vecino), 0, first_treat_vecino))

  n_expuestos <- panel_spillover %>% filter(first_treat_vecino > 0) %>% distinct(COD_DANE) %>% nrow()
  n_total <- panel_spillover %>% distinct(COD_DANE) %>% nrow()
  cat("Municipios nunca tratados directamente:", n_total, "\n")
  cat("De esos, alguna vez vecinos de un municipio tratado:", n_expuestos, "\n")

  if (n_expuestos < 5) {
    cat("Muy pocos municipios expuestos a spillover -- resultado poco fiable, se reporta igual mas abajo.\n")
  }

  panel_spillover$id_num <- as.numeric(panel_spillover$COD_DANE)
  data_modelo <- panel_spillover %>% rename(.gname = first_treat_vecino)

  set.seed(20260824)
  att_gt_out <- tryCatch({
    att_gt(
      yname = "loss_area_ha", tname = "year", idname = "id_num", gname = ".gname",
      xformla = ~1, data = data_modelo, control_group = "notyettreated",
      est_method = "dr", bstrap = TRUE, cband = TRUE, clustervars = "id_num"
    )
  }, error = function(e) { cat("ERROR en att_gt():", conditionMessage(e), "\n"); NULL })

  if (is.null(att_gt_out)) return(NULL)

  agg_simple <- aggte(att_gt_out, type = "simple", na.rm = TRUE)
  cat(sprintf("\nATT del spillover (efecto de tener un vecino tratado): %.3f  SE: %.3f  IC95%%: [%.3f, %.3f]\n",
              agg_simple$overall.att, agg_simple$overall.se,
              agg_simple$overall.att - 1.96 * agg_simple$overall.se,
              agg_simple$overall.att + 1.96 * agg_simple$overall.se))

  if (agg_simple$overall.att > 0 & (agg_simple$overall.att - 1.96 * agg_simple$overall.se) > 0) {
    cat("-> Positivo y significativo: evidencia de FUGA (leakage) hacia municipios vecinos.\n")
  } else if (agg_simple$overall.att < 0 & (agg_simple$overall.att + 1.96 * agg_simple$overall.se) < 0) {
    cat("-> Negativo y significativo: evidencia de efecto protector indirecto en vecinos.\n")
  } else {
    cat("-> No significativo: sin evidencia clara de spillover en ninguna dirección.\n")
  }

  list(att_gt = att_gt_out, simple = agg_simple, tabla_vecinos = tabla_vecinos, etiqueta = etiqueta)
}

resultado_alta <- correr_spillover(panel, "first_treat_alta", "Confianza alta")
resultado_todas <- correr_spillover(panel, "first_treat_todas", "Todas las fuentes")

saveRDS(list(alta = resultado_alta, todas = resultado_todas), file.path(OUTPUT_ROOT, "resultados_spillover.rds"))

## =============================================================================
## 3. Enfoque B: regresion TWFE con proporcion de vecinos tratados
## =============================================================================

cat("\n\n", strrep("=", 70), "\n", sep = "")
cat("ENFOQUE B: regresion con rezago espacial del tratamiento\n")
cat(strrep("=", 70), "\n")

construir_prop_vecinos_tratados <- function(panel, col_first_treat) {
  primer_trat <- panel %>% distinct(COD_DANE, .data[[col_first_treat]]) %>% rename(first_treat = all_of(col_first_treat))
  cod_a_first_treat <- setNames(primer_trat$first_treat, primer_trat$COD_DANE)

  anios <- sort(unique(panel$year))
  filas <- list()
  for (i in seq_along(vecinos)) {
    cod_propio <- cod_dane_por_indice[i]
    idx_vecinos <- vecinos[[i]]
    if (length(idx_vecinos) == 0 || idx_vecinos[1] == 0) {
      cods_vecinos <- character(0)
    } else {
      cods_vecinos <- cod_dane_por_indice[idx_vecinos]
    }
    ft_vecinos <- cod_a_first_treat[cods_vecinos]
    for (anio in anios) {
      n_vecinos <- length(ft_vecinos)
      n_tratados <- sum(!is.na(ft_vecinos) & ft_vecinos > 0 & ft_vecinos <= anio)
      prop <- if (n_vecinos > 0) n_tratados / n_vecinos else NA_real_
      filas[[length(filas) + 1]] <- data.frame(COD_DANE = cod_propio, year = anio, prop_vecinos_tratados = prop)
    }
  }
  do.call(rbind, filas)
}

for (def in list(list(col = "first_treat_alta", nombre = "Alta confianza"), list(col = "first_treat_todas", nombre = "Todas las fuentes"))) {
  cat("\n---", def$nombre, "---\n")
  prop_vecinos <- construir_prop_vecinos_tratados(panel, def$col)
  panel_reg <- panel %>%
    mutate(tratado_directo = as.integer(.data[[def$col]] > 0 & year >= .data[[def$col]])) %>%
    left_join(prop_vecinos, by = c("COD_DANE", "year"))

  modelo <- feols(loss_area_ha ~ tratado_directo + prop_vecinos_tratados | COD_DANE + year, data = panel_reg, cluster = ~COD_DANE)
  print(summary(modelo))
}

cat(
  "\n\n*** Interpretacion del coeficiente 'prop_vecinos_tratados': si es positivo",
  "y significativo, cada vecino tratado adicional (como proporcion del total de",
  "vecinos) se asocia a MAS deforestacion propia -- evidencia de fuga. Si es",
  "negativo, sugiere un efecto protector regional que se extiende mas alla del",
  "municipio tratado. Compara el signo con el resultado del enfoque A (DiD",
  "espacial) -- si ambos coinciden, es una conclusion mas solida. ***\n"
)
