## =============================================================================
## 11_heterogeneidad_territorial.R
##
## Objetivo específico 6: heterogeneidad territorial e institucional.
##
## TERRITORIAL: se usan las 5 dummies de región del Panel de Características
## Generales del CEDE (Andina, Caribe, Pacífica, Orinoquía, Amazonía) --
## disponibles y con cobertura completa (1,122 municipios).
##
## INSTITUCIONAL: *** AVISO IMPORTANTE ***. No se cuenta con el Panel de
## Variables Fiscales y de Buen Gobierno del CEDE (archivo distinto, no
## subido). En su lugar se usa el Índice de Pobreza Multidimensional (IPM)
## como PROXY de capacidad socioeconómica -- NO es lo mismo que capacidad
## institucional/fiscal en sentido estricto. Si consigues el panel real de
## Buen Gobierno del CEDE, reemplaza esta sección siguiendo el mismo patrón
## (agregar columnas en CARGAR_HETEROGENEIDAD() y correr por grupo).
##
## Estrategia: correr Callaway-Sant'Anna por separado en cada subgrupo
## (5 regiones + 2 grupos de IPM), y comparar los ATT resultantes.
##
## Requiere: 01_preparar_datos_did.R ya corrido, y el archivo .dta de
## características generales del CEDE disponible en la ruta de abajo.
##
## USO
## ---
##   Rscript 11_heterogeneidad_territorial.R
## =============================================================================

library(dplyr)
library(haven)
library(did)
library(readr)

PANEL_ANALISIS <- "output/panel_analisis_did.rds"
CEDE_CARACT <- "data/raw/auxiliary/PANEL_CARACTERISTICAS_GENERALES_2024_.dta"
ANIO_BASE <- 2001L
dir.create("output/tablas", showWarnings = FALSE)

COVARIABLES_XFORMLA <- ~ baseline_forest_base + temp_media_c_base + disbogota_base + H_coca_base + homicidios_base

## =============================================================================
## 1. Cargar region (territorial) e IPM (proxy institucional/socioeconomico)
## =============================================================================

cargar_heterogeneidad <- function() {
  caract <- read_dta(CEDE_CARACT)

  region_df <- caract %>%
    filter(ano == ANIO_BASE) %>%
    mutate(COD_DANE = formatC(as.integer(codmpio), width = 5, flag = "0")) %>%
    mutate(region = case_when(
      gandina == 1 ~ "Andina",
      gcaribe == 1 ~ "Caribe",
      gpacifica == 1 ~ "Pacífica",
      gorinoquia == 1 ~ "Orinoquía",
      gamazonia == 1 ~ "Amazonía",
      TRUE ~ NA_character_
    )) %>%
    select(COD_DANE, region)

  # IPM no esta documentado para el año base general (2001) -- el CEDE solo
  # lo reporta para 2005 y 2018. Se usa 2005 como año base especifico para
  # esta variable, mismo patron que homicidios/secuestros en el script 01.
  ipm_df <- NULL
  if ("IPM" %in% names(caract)) {
    candidatos_anio_ipm <- caract %>% filter(!is.na(IPM)) %>% distinct(ano) %>% pull(ano) %>% sort()
    if (length(candidatos_anio_ipm) > 0) {
      anio_ipm <- min(candidatos_anio_ipm)
      cat("Usando año", anio_ipm, "como base para IPM (no disponible en", ANIO_BASE, ").\n")
      ipm_df <- caract %>%
        filter(ano == anio_ipm) %>%
        mutate(COD_DANE = formatC(as.integer(codmpio), width = 5, flag = "0")) %>%
        select(COD_DANE, IPM)
      mediana_ipm <- median(ipm_df$IPM, na.rm = TRUE)
      ipm_df <- ipm_df %>% mutate(grupo_ipm = ifelse(IPM > mediana_ipm, "IPM alto (más pobreza)", "IPM bajo (menos pobreza)"))
      cat("Mediana de IPM (año", anio_ipm, "):", round(mediana_ipm, 2), "\n")
    }
  }
  if (is.null(ipm_df)) {
    cat("Aviso: IPM no disponible en ningún año de este archivo.\n")
    ipm_df <- data.frame(COD_DANE = region_df$COD_DANE, grupo_ipm = NA_character_)
  }

  region_df %>% left_join(ipm_df %>% select(COD_DANE, grupo_ipm), by = "COD_DANE")
}

het <- cargar_heterogeneidad()
cat("Distribución de municipios por región:\n")
print(table(het$region, useNA = "ifany"))
cat("\nDistribución de municipios por grupo de IPM:\n")
print(table(het$grupo_ipm, useNA = "ifany"))

panel <- readRDS(PANEL_ANALISIS) %>% left_join(het, by = "COD_DANE")

## =============================================================================
## 2. Funcion: correr Callaway-Sant'Anna sobre un subgrupo
## =============================================================================

correr_subgrupo <- function(data_sub, col_gname, etiqueta) {
  n_mun <- length(unique(data_sub$COD_DANE))
  n_tratados <- data_sub %>% filter(.data[[col_gname]] > 0) %>% distinct(COD_DANE) %>% nrow()
  cat(sprintf("\n%-35s municipios=%4d  tratados=%3d  ", etiqueta, n_mun, n_tratados))

  if (n_tratados < 3) {
    cat("-- omitido (menos de 3 tratados, insuficiente para estimar)\n")
    return(NULL)
  }

  data_modelo <- data_sub %>% rename(.gname = all_of(col_gname))
  set.seed(20260824)
  att_gt_out <- tryCatch({
    att_gt(
      yname = "loss_area_ha", tname = "year", idname = "id_num", gname = ".gname",
      xformla = ~1, data = data_modelo, control_group = "notyettreated",
      est_method = "dr", bstrap = TRUE, cband = TRUE, clustervars = "id_num"
    )
  }, error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })

  if (is.null(att_gt_out)) return(NULL)

  agg <- tryCatch(aggte(att_gt_out, type = "simple", na.rm = TRUE), error = function(e) NULL)
  if (is.null(agg)) { cat("-- no se pudo agregar (posiblemente todas las celdas NA)\n"); return(NULL) }

  cat(sprintf("ATT=%.2f  SE=%.2f  IC95%%=[%.2f, %.2f]\n",
              agg$overall.att, agg$overall.se,
              agg$overall.att - 1.96 * agg$overall.se, agg$overall.att + 1.96 * agg$overall.se))

  data.frame(
    grupo = etiqueta, n_municipios = n_mun, n_tratados = n_tratados,
    att = agg$overall.att, se = agg$overall.se,
    ic_inf = agg$overall.att - 1.96 * agg$overall.se, ic_sup = agg$overall.att + 1.96 * agg$overall.se
  )
}

## =============================================================================
## 3. Heterogeneidad TERRITORIAL -- por region, para cada definicion de tratamiento
## =============================================================================

regiones <- c("Andina", "Caribe", "Pacífica", "Orinoquía", "Amazonía")

for (col_gname in c("first_treat_alta", "first_treat_todas")) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("HETEROGENEIDAD TERRITORIAL --", col_gname, "\n")
  cat(strrep("=", 70), "\n")

  panel[[paste0("id_num")]] <- as.numeric(panel$COD_DANE)
  resultados <- lapply(regiones, function(r) {
    correr_subgrupo(panel %>% filter(region == r), col_gname, r)
  })
  tabla <- bind_rows(resultados)
  if (nrow(tabla) > 0) {
    write_csv(tabla, sprintf("output/tablas/heterogeneidad_regional_%s.csv", col_gname))
    cat("\nGuardado: output/tablas/heterogeneidad_regional_", col_gname, ".csv\n", sep = "")
  }
}

## =============================================================================
## 4. Heterogeneidad "INSTITUCIONAL" (proxy IPM) -- solo si IPM esta disponible
## =============================================================================

if (!all(is.na(panel$grupo_ipm))) {
  grupos_ipm <- unique(na.omit(panel$grupo_ipm))
  for (col_gname in c("first_treat_alta", "first_treat_todas")) {
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat("HETEROGENEIDAD POR IPM (proxy socioeconomico, NO institucional estricto) --", col_gname, "\n")
    cat(strrep("=", 70), "\n")

    resultados <- lapply(grupos_ipm, function(g) {
      correr_subgrupo(panel %>% filter(grupo_ipm == g), col_gname, g)
    })
    tabla <- bind_rows(resultados)
    if (nrow(tabla) > 0) {
      write_csv(tabla, sprintf("output/tablas/heterogeneidad_ipm_%s.csv", col_gname))
      cat("\nGuardado: output/tablas/heterogeneidad_ipm_", col_gname, ".csv\n", sep = "")
    }
  }
} else {
  cat("\n\nSe omite la heterogeneidad por IPM -- no disponible en el año base. ")
  cat("Para la heterogeneidad institucional real, integra el Panel de Variables Fiscales\n")
  cat("y de Buen Gobierno del CEDE con el mismo patrón usado aquí.\n")
}

cat(
  "\n\n*** Nota metodologica: algunas regiones (Orinoquia, Amazonia) tienen pocos",
  "municipios en total (59 cada una a nivel nacional) -- si el numero de tratados",
  "en esa region es muy chico, el resultado se omite automaticamente (menos de 3",
  "tratados) o debe interpretarse con la misma cautela que las cohortes pequenas",
  "de la seccion de heterogeneidad temporal. ***\n"
)
