## =============================================================================
## 11_heterogeneidad_territorial.R
##
## Objetivo específico 6: heterogeneidad territorial e institucional.
##
## TERRITORIAL: se usan las 5 dummies de región del Panel de Características
## Generales del CEDE (Andina, Caribe, Pacífica, Orinoquía, Amazonía) --
## disponibles y con cobertura completa (1,122 municipios).
##
## INSTITUCIONAL: se incorpora el Índice de Desempeño Fiscal (DF_desemp_fisc)
## del Panel de Variables Fiscales y de Buen Gobierno del CEDE, medido en el
## mismo año base del panel (ANIO_BASE, ver más abajo) -- la variable SÍ está
## documentada en ese año, a diferencia del IPM. Se conserva además el IPM
## como proxy socioeconómico secundario, documentado en el Capítulo 6 (§6.6)
## como hallazgo descartado (artefacto de concentración regional en la
## Amazonía, no un efecto socioeconómico independiente); se deja corriendo
## por trazabilidad y comparación, no como la heterogeneidad institucional
## objetivo del trabajo.
##
## Estrategia: correr Callaway-Sant'Anna por separado en cada subgrupo
## (5 regiones + 2 grupos de capacidad fiscal + 2 grupos de IPM), y comparar
## los ATT resultantes.
##
## Requiere: 01_preparar_datos_did.R ya corrido, y los archivos .dta de
## características generales y de Buen Gobierno del CEDE disponibles en las
## rutas de abajo.
##
## USO
## ---
##   Rscript 11_heterogeneidad_territorial.R
## =============================================================================

library(dplyr)
library(haven)
library(did)
library(readr)

DATA_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/data"
OUTPUT_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs"
PANEL_ANALISIS <- file.path(OUTPUT_ROOT, "panel_analisis_did.rds")
CEDE_CARACT <- file.path(DATA_ROOT, "raw/auxiliary/PANEL_CARACTERISTICAS_GENERALES(2024).dta")
CEDE_BUENGOBIERNO <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/data/raw/auxiliary/PANEL_BUEN_GOBIERNO(2024).dta"
ANIO_BASE <- 2001L
dir.create(file.path(OUTPUT_ROOT, "tablas"), showWarnings = FALSE, recursive = TRUE)

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

  # INSTITUCIONAL REAL: Índice de Desempeño Fiscal (DF_desemp_fisc) del Panel
  # de Variables Fiscales y de Buen Gobierno del CEDE. A diferencia del IPM,
  # esta variable SÍ está documentada en el año base del panel (ANIO_BASE),
  # con ~91% de cobertura municipal -- se usa por tanto el mismo año base que
  # el resto de covariables de línea base (Capítulo 5, sección 5.2), sin
  # necesidad de un año alternativo como el forzado para el IPM (2005).
  inst_df <- NULL
  if (file.exists(CEDE_BUENGOBIERNO)) {
    buengobierno <- read_dta(CEDE_BUENGOBIERNO)
    if ("DF_desemp_fisc" %in% names(buengobierno)) {
      inst_df <- buengobierno %>%
        filter(ano == ANIO_BASE, !is.na(DF_desemp_fisc)) %>%
        mutate(COD_DANE = formatC(as.integer(codmpio), width = 5, flag = "0")) %>%
        select(COD_DANE, DF_desemp_fisc)
      mediana_inst <- median(inst_df$DF_desemp_fisc, na.rm = TRUE)
      inst_df <- inst_df %>%
        mutate(grupo_institucional = ifelse(
          DF_desemp_fisc > mediana_inst,
          "Capacidad fiscal alta", "Capacidad fiscal baja"
        ))
      cat("Usando año", ANIO_BASE, "como base para DF_desemp_fisc (Panel Buen Gobierno).\n")
      cat("Mediana de desempeño fiscal (año", ANIO_BASE, "):", round(mediana_inst, 2),
          " | cobertura:", nrow(inst_df), "municipios\n")
    } else {
      cat("Aviso: DF_desemp_fisc no encontrado en el Panel de Buen Gobierno.\n")
    }
  } else {
    cat("Aviso: no se encontró el Panel de Buen Gobierno en", CEDE_BUENGOBIERNO, "\n")
  }
  if (is.null(inst_df)) {
    inst_df <- data.frame(COD_DANE = region_df$COD_DANE, grupo_institucional = NA_character_)
  }

  region_df %>%
    left_join(ipm_df %>% select(COD_DANE, grupo_ipm), by = "COD_DANE") %>%
    left_join(inst_df %>% select(COD_DANE, grupo_institucional), by = "COD_DANE")
}

het <- cargar_heterogeneidad()
cat("Distribución de municipios por región:\n")
print(table(het$region, useNA = "ifany"))
cat("\nDistribución de municipios por grupo de IPM:\n")
print(table(het$grupo_ipm, useNA = "ifany"))
cat("\nDistribución de municipios por capacidad fiscal (Buen Gobierno CEDE):\n")
print(table(het$grupo_institucional, useNA = "ifany"))

panel <- readRDS(PANEL_ANALISIS) %>% left_join(het, by = "COD_DANE")

## =============================================================================
## 2. Funcion: correr Callaway-Sant'Anna sobre un subgrupo
## =============================================================================

correr_subgrupo <- function(data_sub, col_gname, etiqueta, yname = "loss_area_ha") {
  n_mun <- length(unique(data_sub$COD_DANE))
  n_tratados <- data_sub %>% filter(.data[[col_gname]] > 0) %>% distinct(COD_DANE) %>% nrow()
  n_con_outcome <- data_sub %>% filter(!is.na(.data[[yname]])) %>% distinct(COD_DANE) %>% nrow()
  cat(sprintf("\n%-28s [%s] municipios=%4d  con_outcome=%4d  tratados=%3d  ",
              etiqueta, yname, n_mun, n_con_outcome, n_tratados))

  if (n_tratados < 3) {
    cat("-- omitido (menos de 3 tratados, insuficiente para estimar)\n")
    return(NULL)
  }

  data_modelo <- data_sub %>% rename(.gname = all_of(col_gname))
  set.seed(20260824)
  att_gt_out <- tryCatch({
    att_gt(
      yname = yname, tname = "year", idname = "id_num", gname = ".gname",
      xformla = ~1, data = data_modelo, control_group = "notyettreated",
      est_method = "dr", bstrap = TRUE, cband = TRUE, clustervars = "id_num"
    )
  }, error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })

  if (is.null(att_gt_out)) return(NULL)

  agg <- tryCatch(aggte(att_gt_out, type = "simple", na.rm = TRUE), error = function(e) NULL)
  if (is.null(agg)) { cat("-- no se pudo agregar (posiblemente todas las celdas NA)\n"); return(NULL) }

  cat(sprintf("ATT=%.5f  SE=%.5f  IC95%%=[%.5f, %.5f]\n",
              agg$overall.att, agg$overall.se,
              agg$overall.att - 1.96 * agg$overall.se, agg$overall.att + 1.96 * agg$overall.se))

  data.frame(
    grupo = etiqueta, outcome = yname, n_municipios = n_mun, n_con_outcome = n_con_outcome,
    n_tratados = n_tratados, att = agg$overall.att, se = agg$overall.se,
    ic_inf = agg$overall.att - 1.96 * agg$overall.se, ic_sup = agg$overall.att + 1.96 * agg$overall.se
  )
}

## =============================================================================
## 3. Heterogeneidad TERRITORIAL -- por region, para cada definicion de tratamiento
## =============================================================================

regiones <- c("Andina", "Caribe", "Pacífica", "Orinoquía", "Amazonía")

panel$id_num <- as.numeric(panel$COD_DANE)

for (col_gname in c("first_treat_alta", "first_treat_todas")) {
  for (outcome in c("loss_area_ha", "tasa_deforestacion")) {
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat("HETEROGENEIDAD TERRITORIAL --", col_gname, "| outcome:", outcome, "\n")
    cat(strrep("=", 70), "\n")
    
    resultados <- lapply(regiones, function(r) {
      correr_subgrupo(panel %>% filter(region == r), col_gname, r, yname = outcome)
    })
    tabla <- bind_rows(resultados)
    if (nrow(tabla) > 0) {
      ruta <- file.path(OUTPUT_ROOT, "tablas", sprintf("heterogeneidad_regional_%s_%s.csv", outcome, col_gname))
      write_csv(tabla, ruta)
      cat("\nGuardado:", ruta, "\n")
    }
  }
}

## =============================================================================
## 4. Heterogeneidad por IPM (proxy socioeconomico secundario -- ver seccion 5
##    para la heterogeneidad institucional real con el Panel de Buen Gobierno)
## =============================================================================

if (!all(is.na(panel$grupo_ipm))) {
  grupos_ipm <- unique(na.omit(panel$grupo_ipm))
  for (col_gname in c("first_treat_alta", "first_treat_todas")) {
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat("HETEROGENEIDAD POR IPM (proxy socioeconomico, NO institucional estricto) --", col_gname, "\n")
    cat(strrep("=", 70), "\n")

    resultados <- lapply(grupos_ipm, function(g) {
      correr_subgrupo(panel %>% filter(grupo_ipm == g), col_gname, g, yname = outcome)
    })
    tabla <- bind_rows(resultados)
    if (nrow(tabla) > 0) {
      write_csv(tabla, file.path(OUTPUT_ROOT, "tablas", sprintf("heterogeneidad_ipm_%s.csv", col_gname)))
      cat("\nGuardado: output/tablas/heterogeneidad_ipm_", col_gname, ".csv\n", sep = "")
    }
  }
} else {
  cat("\n\nSe omite la heterogeneidad por IPM -- no disponible en el año base. ")
  cat("Para la heterogeneidad institucional real, integra el Panel de Variables Fiscales\n")
  cat("y de Buen Gobierno del CEDE con el mismo patrón usado aquí.\n")
}

## =============================================================================
## 5. Heterogeneidad INSTITUCIONAL REAL -- Índice de Desempeño Fiscal
##    (Panel de Variables Fiscales y de Buen Gobierno del CEDE)
## =============================================================================

if (!all(is.na(panel$grupo_institucional))) {
  grupos_inst <- unique(na.omit(panel$grupo_institucional))

  # Diagnostico 1: cruce capacidad fiscal x region -- si "alta capacidad" esta
  # fuertemente concentrada en Andina/Caribe, el hallazgo institucional podria
  # seguir siendo, en el fondo, el mismo confusor regional ya identificado y
  # descartado para el IPM (seccion 6.6). Se imprime aqui para revisión manual.
  cat("\nCruce capacidad fiscal x región (diagnóstico de confusión regional):\n")
  print(table(het$grupo_institucional, het$region, useNA = "ifany"))

  # Diagnostico 2: replicar con tasa_deforestacion, igual que se hizo para el
  # IPM (Tabla 6.5) -- el hallazgo en hectareas absolutas puede ser sensible a
  # municipios con perdidas extremas; solo se reporta como hallazgo si
  # sobrevive tambien en tasa.
  for (col_gname in c("first_treat_alta", "first_treat_todas")) {
    for (outcome in c("loss_area_ha", "tasa_deforestacion")) {
      cat("\n", strrep("=", 70), "\n", sep = "")
      cat("HETEROGENEIDAD INSTITUCIONAL (Índice de Desempeño Fiscal, año base",
          ANIO_BASE, ") --", col_gname, "| outcome:", outcome, "\n")
      cat(strrep("=", 70), "\n")

      resultados <- lapply(grupos_inst, function(g) {
        correr_subgrupo(panel %>% filter(grupo_institucional == g), col_gname, g, yname = outcome)
      })
      tabla <- bind_rows(resultados)
      if (nrow(tabla) > 0) {
        ruta <- file.path(OUTPUT_ROOT, "tablas", sprintf("heterogeneidad_institucional_%s_%s.csv", outcome, col_gname))
        write_csv(tabla, ruta)
        cat("\nGuardado:", ruta, "\n")
      }
    }
  }
} else {
  cat("\n\nSe omite la heterogeneidad institucional real -- Panel de Buen Gobierno no ")
  cat("disponible en la ruta configurada, o DF_desemp_fisc sin cobertura en el año base.\n")
}

cat(
  "\n\n*** Nota metodologica: algunas regiones (Orinoquia, Amazonia) tienen pocos",
  "municipios en total (59 cada una a nivel nacional) -- si el numero de tratados",
  "en esa region es muy chico, el resultado se omite automaticamente (menos de 3",
  "tratados) o debe interpretarse con la misma cautela que las cohortes pequenas",
  "de la seccion de heterogeneidad temporal. El mismo criterio de omision (< 3",
  "tratados) aplica igual a los grupos de capacidad fiscal e IPM. ***\n"
)
