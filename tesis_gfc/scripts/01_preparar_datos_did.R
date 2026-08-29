## =============================================================================
## 01_preparar_datos_did.R
##
## Prepara el panel para el modelo de diferencias en diferencias con adopción
## escalonada (Callaway y Sant'Anna, 2021) y el emparejamiento por puntaje de
## propensión (PSM).
##
## Combina DOS archivos que quedaron en ramas separadas del pipeline:
##   - panel_con_psm_covariables.csv: outcome (deforestación) + covariables
##     (clima, accesibilidad, conflicto) — NO tiene las columnas de tratamiento.
##   - panel_con_tratamiento_actualizado.csv (de 26_consolidar_fuentes_carbono.py):
##     tiene anio_inicio_tratamiento_alta_confianza / _todas_fuentes — NO tiene
##     clima completo ni las covariables de accesibilidad/conflicto.
##
## Este script las une por COD_DANE + year en un solo dataset de análisis.
##
## DECISIÓN METODOLÓGICA: las covariables usadas para el emparejamiento y para
## el ajuste doblemente robusto en el DiD (xformla) se toman en el AÑO BASE
## (2001, el primer año del panel), no como variables que varían en el tiempo.
## Esto es la práctica estándar en DiD con covariables: deben ser pre-tratamiento
## y fijas, para evitar "bad controls" (covariables que el propio tratamiento
## podría afectar si se miden después de que empieza).
##
## USO
## ---
##   Rscript 01_preparar_datos_did.R
##
## Ajusta las rutas de INPUT más abajo si tus archivos están en otro lugar.
## =============================================================================

library(readr)
library(dplyr)
library(tidyr)

## --- Rutas de entrada (ajustar si es necesario) -----------------------------
RUTA_PANEL      <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/data/final/panel_con_psm_covariables.csv"
RUTA_TRATAMIENTO <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/data/final/panel_con_tratamiento_actualizado.csv"
ANIO_BASE       <- 2001L  # primer año del panel; usado como corte pre-tratamiento

# Algunas variables de conflicto (homicidios, secuestros, acc_subversivas) del
# CEDE no se miden antes de 2003 (0% de cobertura en 2001-2002) — para esas se
# usa un año base posterior, verificado empíricamente como el primero con
# cobertura >95%. Esto significa que, para el puñado de municipios cuya cohorte
# de tratamiento sea 2001-2002 (si los hay), el "año base" de estas 3 variables
# en particular podría no ser estrictamente pre-tratamiento — se documenta como
# limitación conocida, dado que no hay alternativa con datos disponibles antes.
ANIO_BASE_CONFLICTO_TARDIO <- 2003L
VARS_BASE_TARDIA <- c("homicidios", "secuestros", "acc_subversivas")

## --- Salidas -----------------------------------------------------------------
dir.create("output", showWarnings = FALSE)
OUT_PANEL_ANALISIS <- "output/panel_analisis_did.rds"
OUT_COVARIABLES_BASE <- "output/covariables_base_municipio.rds"

## =============================================================================
## 1. Cargar y unir
## =============================================================================

cat("Cargando panel principal:", RUTA_PANEL, "\n")
panel <- read_csv(RUTA_PANEL, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE)

cat("Cargando tratamiento:", RUTA_TRATAMIENTO, "\n")
tratamiento <- read_csv(RUTA_TRATAMIENTO, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE)

# El archivo de tratamiento puede venir a nivel municipio (una fila) o
# municipio-año (repetido) — nos quedamos solo con las columnas de interés,
# una fila por municipio, para evitar duplicar filas al unir.
cols_tratamiento <- intersect(
  c("COD_DANE", "anio_inicio_tratamiento_alta_confianza", "anio_inicio_tratamiento_todas_fuentes"),
  names(tratamiento)
)
if (length(cols_tratamiento) < 3) {
  stop(
    "El archivo de tratamiento no tiene las columnas esperadas. ",
    "Encontradas: ", paste(names(tratamiento), collapse = ", ")
  )
}
tratamiento_mun <- tratamiento %>%
  select(all_of(cols_tratamiento)) %>%
  distinct(COD_DANE, .keep_all = TRUE)

cat("Municipios con fecha de tratamiento (alta confianza):",
    sum(!is.na(tratamiento_mun$anio_inicio_tratamiento_alta_confianza)), "\n")
cat("Municipios con fecha de tratamiento (alta + media confianza):",
    sum(!is.na(tratamiento_mun$anio_inicio_tratamiento_todas_fuentes)), "\n")

panel <- panel %>%
  left_join(tratamiento_mun, by = "COD_DANE")

n_sin_match <- sum(!panel$COD_DANE %in% tratamiento_mun$COD_DANE)
if (n_sin_match > 0) {
  cat("Aviso:", length(unique(panel$COD_DANE[!panel$COD_DANE %in% tratamiento_mun$COD_DANE])),
      "municipios del panel no aparecen en el archivo de tratamiento — quedan como no tratados (NA).\n")
}

## =============================================================================
## 2. Variable de grupo (gname) para did::att_gt()
##
## Convención del paquete `did`: gname = 0 para las unidades NUNCA tratadas,
## y el año de inicio para las tratadas. NA en anio_inicio_tratamiento se
## interpreta como "nunca tratado" (0), no como dato faltante — un municipio
## sin ningún proyecto de carbono identificado es un control legítimo.
## =============================================================================

panel <- panel %>%
  mutate(
    first_treat_alta  = ifelse(is.na(anio_inicio_tratamiento_alta_confianza), 0L, anio_inicio_tratamiento_alta_confianza),
    first_treat_todas = ifelse(is.na(anio_inicio_tratamiento_todas_fuentes), 0L, anio_inicio_tratamiento_todas_fuentes)
  )

# id numérico: did::att_gt requiere idname numérico, no texto.
panel <- panel %>%
  mutate(id_num = as.numeric(COD_DANE))

## =============================================================================
## 3. Covariables en año base (pre-tratamiento) — una fila por municipio
## =============================================================================

cols_covariables <- c(
  "baseline_forest", "temp_media_c", "prec_anual_mm",
  "discapital", "disbogota", "altura", "distancia_mercado",
  "H_coca", "coca", "homicidios", "secuestros", "o_desplaza", "acc_subversivas"
)
cols_covariables <- intersect(cols_covariables, names(panel))

# Variables "normales": año base 2001. Variables de conflicto sin datos antes
# de 2003 (ver nota arriba): año base 2003, por separado.
cols_base_normal <- setdiff(cols_covariables, VARS_BASE_TARDIA)
cols_base_tardia <- intersect(cols_covariables, VARS_BASE_TARDIA)

covariables_base_normal <- panel %>%
  filter(year == ANIO_BASE) %>%
  select(COD_DANE, all_of(cols_base_normal)) %>%
  rename_with(~ paste0(.x, "_base"), all_of(cols_base_normal))

covariables_base_tardia <- panel %>%
  filter(year == ANIO_BASE_CONFLICTO_TARDIO) %>%
  select(COD_DANE, all_of(cols_base_tardia)) %>%
  rename_with(~ paste0(.x, "_base"), all_of(cols_base_tardia))

covariables_base <- covariables_base_normal %>%
  left_join(covariables_base_tardia, by = "COD_DANE")

cat("\nCobertura de covariables en su año base respectivo (", ANIO_BASE, " para la mayoría, ",
    ANIO_BASE_CONFLICTO_TARDIO, " para ", paste(VARS_BASE_TARDIA, collapse = "/"), "):\n", sep = "")
for (col in names(covariables_base)[-1]) {
  pct <- mean(!is.na(covariables_base[[col]]))
  cat(sprintf("  %-30s %.1f%%\n", col, pct * 100))
}

# Diagnóstico: ¿cuántos municipios tienen cohorte de tratamiento tan temprana
# (2001-2003) que el año base "tardío" (2003) para homicidios/secuestros/
# acc_subversivas podría no ser estrictamente pre-tratamiento?
n_cohortes_tempranas <- panel %>%
  filter(year == ANIO_BASE) %>%
  filter(
    (first_treat_alta > 0 & first_treat_alta <= ANIO_BASE_CONFLICTO_TARDIO) |
    (first_treat_todas > 0 & first_treat_todas <= ANIO_BASE_CONFLICTO_TARDIO)
  ) %>%
  nrow()
if (n_cohortes_tempranas > 0) {
  cat("\nAviso:", n_cohortes_tempranas, "municipio(s) con cohorte de tratamiento <=",
      ANIO_BASE_CONFLICTO_TARDIO, "— para esos, el año base de homicidios/secuestros/",
      "acc_subversivas coincide o es posterior al inicio del tratamiento. Revisar si ",
      "corresponde excluirlos del emparejamiento o documentarlo como limitación.\n")
}

panel <- panel %>%
  left_join(covariables_base, by = "COD_DANE")

## =============================================================================
## 4. Outcome alternativo para robustez: tasa de deforestación
##    (hectáreas perdidas / bosque base), en vez del valor absoluto.
##    NA cuando baseline_forest_base es 0 (no hay bosque que perder).
## =============================================================================

panel <- panel %>%
  mutate(
    tasa_deforestacion = ifelse(
      !is.na(baseline_forest_base) & baseline_forest_base > 0,
      loss_area_ha / baseline_forest_base,
      NA_real_
    )
  )

## =============================================================================
## 5. Diagnóstico de balance del panel (did::att_gt funciona mejor con panel
##    balanceado — confirmamos que cada municipio tiene el mismo número de años)
## =============================================================================

conteo_anios <- panel %>% count(COD_DANE, name = "n_anios")
if (length(unique(conteo_anios$n_anios)) > 1) {
  cat("\n*** AVISO: el panel NO está perfectamente balanceado —",
      "algunos municipios tienen distinto número de años. ***\n")
  print(table(conteo_anios$n_anios))
} else {
  cat("\nPanel balanceado: los", nrow(conteo_anios), "municipios tienen",
      unique(conteo_anios$n_anios), "años cada uno.\n")
}

## =============================================================================
## 6. Guardar
## =============================================================================

saveRDS(panel, OUT_PANEL_ANALISIS)
saveRDS(covariables_base, OUT_COVARIABLES_BASE)

cat("\nGuardado:", OUT_PANEL_ANALISIS, "(", nrow(panel), "filas,", length(unique(panel$COD_DANE)), "municipios )\n")
cat("Guardado:", OUT_COVARIABLES_BASE, "\n")

cat("\n=== Resumen de cohortes de tratamiento (alta confianza) ===\n")
print(panel %>% filter(year == ANIO_BASE) %>% count(first_treat_alta) %>% arrange(first_treat_alta))

cat("\n=== Resumen de cohortes de tratamiento (todas las fuentes) ===\n")
print(panel %>% filter(year == ANIO_BASE) %>% count(first_treat_todas) %>% arrange(first_treat_todas))
