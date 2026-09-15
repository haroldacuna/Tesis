## =============================================================================
## 02_matching_psm.R  (versión revisada)
##
## Emparejamiento por puntaje de propensión (PSM) sobre covariables de línea
## base, para las dos definiciones de tratamiento (alta_confianza /
## todas_fuentes).
##
## CAMBIOS RESPECTO A LA VERSIÓN ANTERIOR
## --------------------------------------
## (1) CONJUNTO DE COVARIABLES FIJO Y EXPLÍCITO. Antes, cualquier covariable
##     con cobertura <50% se eliminaba en silencio dentro de la función, de
##     modo que el modelo de propensión efectivamente estimado podía diferir
##     entre las dos definiciones de tratamiento y no quedaba registrado en
##     ninguna parte. Ahora la lista es fija: si una covariable no alcanza el
##     umbral de cobertura, el script SE DETIENE y obliga a una decisión
##     explícita (moverla a COVARIABLES_EXCLUIDAS, con justificación escrita).
##
## (2) DEFORESTACIÓN PRE-TRATAMIENTO EN EL MODELO DE PROPENSIÓN. La versión
##     anterior incluía `baseline_forest_base`, que es el STOCK de bosque en
##     2000 (Hansen treecover2000, ver 04_merge_covariates.py), no el flujo
##     de pérdida. Se añaden aquí el nivel medio y la tendencia de
##     `loss_area_ha` sobre una ventana pre-tratamiento común, más la tasa
##     media. La selección de sitios REDD+ es endógena a la presión de
##     deforestación previa (las metodologías VM0006/VM0007 de Verra
##     construyen la línea base del proyecto sobre la tasa histórica), de
##     modo que omitir el flujo previo deja sin controlar el principal
##     determinante de la asignación.
##
##     CAVEAT: emparejar sobre el outcome pre-tratamiento y luego estimar DiD
##     puede inducir sesgo por regresión a la media (Daw y Hatfield, 2018).
##     Por eso (a) se usa un promedio plurianual y no un año único, (b) se
##     incluye la pendiente además del nivel, y (c) INCLUIR_DEFOR_PRE permite
##     correr la especificación sin estas variables para comparar. Reportar
##     ambas en la tesis.
##
## (3) DIAGNÓSTICO DE SOPORTE COMÚN. Solapamiento de las distribuciones del
##     puntaje, región de soporte común, tratados fuera de soporte, y conteo
##     de puntajes extremos (<0.01 o >0.99), que son los que generan pesos
##     inestables en el estimador doblemente robusto del script 03.
##
## (4) TABLA DE BALANCE COMPLETA antes/después: medias por grupo, diferencia
##     estandarizada, razón de varianzas y estadísticos eCDF, exportada a CSV.
##
## (5) SENSIBILIDAD POR COHORTES TEMPRANAS. Se re-corre todo excluyendo los
##     municipios cuya cohorte de tratamiento cae dentro de la ventana usada
##     para construir las covariables de línea base.
##
## USO
## ---
##   Rscript 02_matching_psm.R
## =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(MatchIt)
library(ggplot2)

PANEL_ANALISIS <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/panel_analisis_did.rds"
dir.create("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs", showWarnings = FALSE, recursive = TRUE)
dir.create("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/figuras", showWarnings = FALSE, recursive = TRUE)
dir.create("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/tablas", showWarnings = FALSE, recursive = TRUE)

## =============================================================================
## CONFIGURACIÓN
## =============================================================================

## --- Ventana pre-tratamiento para las variables de deforestación previa -----
##
## Debe cumplir dos condiciones en tensión:
##   - ser lo bastante larga para que el promedio y la pendiente no sean ruido
##     (mínimo ~5 años),
##   - terminar antes de la primera cohorte de tratamiento relevante.
##
## La cohorte más temprana observada es 2002 (Cáceres, Anzoátegui y, solo en
## "todas las fuentes", Chinchiná), confirmado por
## 08_sensibilidad_excluir_cohortes_tempranas.R. No existe ventana que sea
## simultáneamente larga y limpia para esa cohorte: por eso la ventana se fija
## en 2001-2005 y esos municipios se excluyen en la especificación de
## sensibilidad (ver EJECUCIÓN, más abajo).
VENTANA_PRE_INICIO <- 2001L
VENTANA_PRE_FIN    <- 2005L

## Mínimo de años con dato dentro de la ventana para estimar la pendiente.
MIN_ANIOS_TENDENCIA <- 3L

## --- Covariables del modelo de propensión (LISTA FIJA, NO AUTOMÁTICA) -------
##
## Agrupadas por bloque conceptual, para poder describirlas así en la tesis.
## Cualquier cambio a esta lista debe quedar registrado aquí, no inferirse de
## la cobertura de la corrida.

COVARIABLES_DEFORESTACION <- c(
  "log_defor_pre_media",     # log(1 + pérdida media anual, ha) en la ventana pre
  "defor_pre_tendencia",     # pendiente OLS de loss_area_ha sobre el año
  "log_baseline_forest"      # log(1 + stock de bosque 2000, ha)
)

COVARIABLES_BIOFISICAS <- c(
  "temp_media_c_base",
  "prec_anual_mm_base",
  "altura_base"
)

COVARIABLES_ACCESIBILIDAD <- c(
  "discapital_base",
  "disbogota_base",
  "distancia_mercado_base"
)

COVARIABLES_CONFLICTO <- c(
  "H_coca_base",
  "homicidios_base",
  "o_desplaza_base"
)

## Covariables deliberadamente EXCLUIDAS, con la razón. Documentar en la tesis.
##
##   distancia_mercado_base : cobertura insuficiente en el panel CEDE integrado
##                            (verificar el resumen impreso por el script 01).
##   secuestros_base        : altamente colineal con homicidios_base y con
##                            acc_subversivas_base; se conserva un solo
##                            indicador de violencia armada para no saturar el
##                            modelo de propensión con 88-99 tratados.
##   acc_subversivas_base   : ídem.
##   coca_base              : se prefiere H_coca_base (índice de Herfindahl,
##                            escala acotada) sobre el nivel absoluto de
##                            hectáreas, muy sesgado a la derecha.
COVARIABLES_EXCLUIDAS <- c(
  "secuestros_base", "acc_subversivas_base", "coca_base"
)

## Interruptor para la comparación exigida por el caveat de Daw y Hatfield.
INCLUIR_DEFOR_PRE <- TRUE

COVARIABLES_PSM <- c(
  if (INCLUIR_DEFOR_PRE) COVARIABLES_DEFORESTACION else "log_baseline_forest",
  COVARIABLES_BIOFISICAS,
  COVARIABLES_ACCESIBILIDAD,
  COVARIABLES_CONFLICTO
)

## --- Parámetros del emparejamiento ------------------------------------------
##
## 1:1 sin reemplazo con calibre de 0.2 DE del logit del puntaje (Austin, 2011).
##
## ADVERTENCIA SI SE CAMBIA `RATIO` O `REPLACE`: 03_did_callaway_santanna.R
## consume la submuestra emparejada como un simple filtro
## `panel %>% filter(COD_DANE %in% municipios_emparejados)`, SIN pesos. Con
## 1:1 sin reemplazo todos los pesos de match.data() valen 1 y ese filtro es
## correcto. Con ratio > 1, reemplazo, o method = "full", los pesos dejan de
## ser uniformes y el filtro produce estimaciones SESGADAS: habría que pasar
## los pesos a did::att_gt() mediante su argumento `weightsname`. El script
## exporta los pesos por municipio justamente para ese caso.
METODO_MATCHING <- "full"
CALIPER_SD      <- NULL 

## Emparejamiento exacto por departamento. Desactivado por defecto: con 88-99
## tratados repartidos en ~30 departamentos, forzarlo deja muchos tratados sin
## pareja. Útil como robustez si el balance regional queda mal.
EXACTO_DEPARTAMENTO <- FALSE

## Restringir el pool a municipios con bosque. Un municipio sin cobertura
## arbórea en 2000 no puede deforestar: incluirlo como control infla
## artificialmente el pool y comprime el puntaje de propensión.
UMBRAL_BOSQUE_MIN <- 0

## Umbral de cobertura que dispara la detención del script.
UMBRAL_COBERTURA <- 0.90

SEMILLA <- 20260824

## =============================================================================
## 1. CARGA Y CONSTRUCCIÓN DE VARIABLES DE DEFORESTACIÓN PRE-TRATAMIENTO
## =============================================================================

panel <- readRDS(PANEL_ANALISIS)

cat(strrep("=", 78), "\n")
cat("CONSTRUCCIÓN DE COVARIABLES DE DEFORESTACIÓN PRE-TRATAMIENTO\n")
cat(strrep("=", 78), "\n")
cat("Ventana pre-tratamiento:", VENTANA_PRE_INICIO, "-", VENTANA_PRE_FIN,
    sprintf("(%d años)\n", VENTANA_PRE_FIN - VENTANA_PRE_INICIO + 1L))

## Pendiente OLS de la pérdida anual sobre el año, calculada como
## Cov(t, L_it) / Var(t) dentro de la ventana. Equivale al coeficiente de una
## regresión simple por municipio, sin necesidad de ajustar 1.122 modelos.
defor_pre <- panel %>%
  filter(year >= VENTANA_PRE_INICIO, year <= VENTANA_PRE_FIN) %>%
  group_by(COD_DANE) %>%
  summarise(
    n_anios_pre        = sum(!is.na(loss_area_ha)),
    defor_pre_media_ha = mean(loss_area_ha, na.rm = TRUE),
    defor_pre_sd_ha    = sd(loss_area_ha, na.rm = TRUE),
    defor_pre_tendencia = {
      ini <- mean(loss_area_ha[year %in% c(VENTANA_PRE_INICIO, VENTANA_PRE_INICIO + 1L)],
                  na.rm = TRUE)
      fin <- mean(loss_area_ha[year %in% c(VENTANA_PRE_FIN - 1L, VENTANA_PRE_FIN)],
                  na.rm = TRUE)
      if (is.finite(ini) && is.finite(fin)) log1p(fin) - log1p(ini) else NA_real_
    },
  )

cat("Municipios con al menos", MIN_ANIOS_TENDENCIA, "años en la ventana:",
    sum(defor_pre$n_anios_pre >= MIN_ANIOS_TENDENCIA), "de", nrow(defor_pre), "\n")

## Sección transversal a nivel municipio.
datos_mun_base <- panel %>%
  distinct(COD_DANE, .keep_all = TRUE) %>%
  select(
    COD_DANE, NOMBRE_MPI, DPTO_CNMBR,
    first_treat_alta, first_treat_todas,
    any_of(c(
      "baseline_forest_base", "temp_media_c_base", "prec_anual_mm_base",
      "discapital_base", "disbogota_base", "altura_base",
      "distancia_mercado_base", "H_coca_base", "coca_base",
      "homicidios_base", "secuestros_base", "o_desplaza_base",
      "acc_subversivas_base"
    ))
  ) %>%
  left_join(defor_pre, by = "COD_DANE") %>%
  mutate(
    ## Transformación logarítmica: tanto el stock de bosque como la pérdida
    ## anual son distribuciones fuertemente sesgadas a la derecha (unos pocos
    ## municipios amazónicos concentran el grueso). Sin transformar, el
    ## puntaje de propensión queda dominado por la cola y el calibre en
    ## unidades de DE del logit pierde sentido.
    log_baseline_forest  = log1p(baseline_forest_base),
    log_defor_pre_media  = log1p(defor_pre_media_ha),
    ## Tasa media pre-tratamiento (no entra al modelo por defecto: es
    ## mecánicamente colineal con las dos anteriores). Se conserva para las
    ## tablas descriptivas.
    tasa_defor_pre = ifelse(
      !is.na(baseline_forest_base) & baseline_forest_base > 0,
      defor_pre_media_ha / baseline_forest_base, NA_real_
    )
  )

## =============================================================================
## 2. DIAGNÓSTICO DE COBERTURA — SIN EXCLUSIÓN AUTOMÁTICA
## =============================================================================

cat("\n", strrep("=", 78), "\n", sep = "")
cat("COBERTURA DE LAS COVARIABLES DEL MODELO DE PROPENSIÓN\n")
cat(strrep("=", 78), "\n")

faltantes <- setdiff(COVARIABLES_PSM, names(datos_mun_base))
if (length(faltantes) > 0) {
  stop(
    "Covariables declaradas en COVARIABLES_PSM que NO existen en el panel: ",
    paste(faltantes, collapse = ", "),
    "\nRevisa 01_preparar_datos_did.R o corrige la lista."
  )
}

cobertura <- vapply(datos_mun_base[COVARIABLES_PSM], function(x) mean(!is.na(x)), numeric(1))
for (nm in names(cobertura)) {
  marca <- if (cobertura[[nm]] >= UMBRAL_COBERTURA) "OK  " else "BAJA"
  cat(sprintf("  [%s] %-26s %6.1f%%\n", marca, nm, cobertura[[nm]] * 100))
}

bajas <- names(cobertura)[cobertura < UMBRAL_COBERTURA]
if (length(bajas) > 0) {
  stop(
    "\nCovariables con cobertura por debajo del ", UMBRAL_COBERTURA * 100, "%: ",
    paste(bajas, collapse = ", "),
    "\n\nEl script NO las elimina automáticamente, a propósito: el conjunto de ",
    "covariables del modelo de propensión debe ser una decisión explícita y ",
    "documentada, no un subproducto de la cobertura de la corrida.",
    "\nMueve la variable a COVARIABLES_EXCLUIDAS con su justificación, o ",
    "corrige el dato en el pipeline de preparación."
  )
}

cat("\nCovariables excluidas por decisión explícita:\n  ",
    paste(COVARIABLES_EXCLUIDAS, collapse = ", "), "\n")
cat("Modelo de propensión con", length(COVARIABLES_PSM), "covariables.\n")

## =============================================================================
## 3. DIAGNÓSTICO DE SOPORTE COMÚN
## =============================================================================

diagnosticar_soporte_comun <- function(ps, tratado, etiqueta, sufijo) {
  ps_t <- ps[tratado == 1]
  ps_c <- ps[tratado == 0]

  lim_inf <- max(min(ps_t), min(ps_c))
  lim_sup <- min(max(ps_t), max(ps_c))

  fuera <- sum(ps_t < lim_inf | ps_t > lim_sup)
  extremos_t <- sum(ps_t < 0.01 | ps_t > 0.99)
  extremos_c <- sum(ps_c < 0.01 | ps_c > 0.99)

  cat("\n--- Soporte común ---\n")
  cat(sprintf("  Puntaje tratados   : min %.4f | mediana %.4f | max %.4f\n",
              min(ps_t), median(ps_t), max(ps_t)))
  cat(sprintf("  Puntaje controles  : min %.4f | mediana %.4f | max %.4f\n",
              min(ps_c), median(ps_c), max(ps_c)))
  cat(sprintf("  Región de soporte común: [%.4f , %.4f]\n", lim_inf, lim_sup))
  cat(sprintf("  Tratados fuera del soporte común: %d de %d (%.1f%%)\n",
              fuera, length(ps_t), 100 * fuera / length(ps_t)))
  cat(sprintf("  Puntajes extremos (<0.01 o >0.99): %d tratados, %d controles\n",
              extremos_t, extremos_c))
  if (extremos_t > 0 || fuera > 0) {
    cat("  AVISO: los puntajes extremos generan pesos inestables en el estimador\n")
    cat("         doblemente robusto de 03_did_callaway_santanna.R. Revisa qué\n")
    cat("         municipios son (output/tablas/soporte_comun_*.csv).\n")
  }

  df_ps <- data.frame(
    ps = ps,
    grupo = factor(tratado, levels = c(0, 1), labels = c("Control", "Tratado"))
  )

  p <- ggplot(df_ps, aes(x = ps, fill = grupo)) +
    geom_density(alpha = 0.45, color = NA) +
    geom_vline(xintercept = c(lim_inf, lim_sup), linetype = "dashed", color = "grey40") +
    scale_x_continuous(limits = c(0, min(1, quantile(ps, 0.999) * 1.5))) +
    labs(
      title = paste("Soporte común del puntaje de propensión —", etiqueta),
      subtitle = "Líneas punteadas: límites de la región de soporte común",
      x = "Puntaje de propensión estimado", y = "Densidad", fill = NULL
    ) +
    theme_minimal()

  ggsave(paste0("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/figuras/soporte_comun_", sufijo, ".png"), p,
         width = 8, height = 4.5, dpi = 150)

  list(lim_inf = lim_inf, lim_sup = lim_sup, n_fuera = fuera,
       n_extremos_tratados = extremos_t)
}

## =============================================================================
## 4. FUNCIÓN PRINCIPAL
## =============================================================================

correr_matching <- function(datos, col_first_treat, etiqueta, sufijo,
                            excluir_cohortes_tempranas = FALSE) {

  cat("\n\n", strrep("=", 78), "\n", sep = "")
  cat("MATCHING — ", etiqueta,
      if (excluir_cohortes_tempranas) " [SIN COHORTES TEMPRANAS]" else "", "\n", sep = "")
  cat(strrep("=", 78), "\n")

  d <- datos %>%
    mutate(tratado = as.integer(.data[[col_first_treat]] > 0))

  ## --- Exclusión de cohortes tempranas --------------------------------------
  ##
  ## Criterio unificado: se excluye toda unidad tratada dentro de la ventana
  ## usada para construir las covariables de línea base. Esto cubre a la vez
  ## los dos problemas de pre-tratamiento del diseño:
  ##   (a) homicidios/secuestros/acc_subversivas del CEDE no existen antes de
  ##       2003, de modo que su "año base" no es estrictamente anterior al
  ##       tratamiento para las cohortes <= 2003;
  ##   (b) la ventana de deforestación pre-tratamiento (2001-2005) se solapa
  ##       con el tratamiento para las cohortes <= 2005.
  ## Un solo umbral (VENTANA_PRE_FIN) es más defendible que dos reglas ad hoc.
  if (excluir_cohortes_tempranas) {
    tempranos <- d %>%
      filter(.data[[col_first_treat]] > 0, .data[[col_first_treat]] <= VENTANA_PRE_FIN)
    if (nrow(tempranos) > 0) {
      cat("Municipios excluidos por cohorte <= ", VENTANA_PRE_FIN, ":\n", sep = "")
      print(tempranos %>% select(COD_DANE, NOMBRE_MPI, cohorte = all_of(col_first_treat)),
            row.names = FALSE)
      d <- d %>% filter(!COD_DANE %in% tempranos$COD_DANE)
    } else {
      cat("No hay cohortes <= ", VENTANA_PRE_FIN, " para esta definición.\n", sep = "")
    }
  }

  ## --- Restricción del pool a municipios con bosque -------------------------
  n_antes_bosque <- nrow(d)
  d <- d %>% filter(!is.na(baseline_forest_base), baseline_forest_base > UMBRAL_BOSQUE_MIN)
  cat("Restricción a municipios con bosque en 2000: ", nrow(d), " de ",
      n_antes_bosque, "\n", sep = "")

  ## --- Pérdida por datos faltantes, POR GRUPO -------------------------------
  ##
  ## Importa reportarlo por separado: si la eliminación por lista descarta
  ## tratados desproporcionadamente, cambia el estimando (deja de ser el ATT
  ## sobre el conjunto de tratados originalmente definido).
  n_t_antes <- sum(d$tratado == 1); n_c_antes <- sum(d$tratado == 0)
  d_completo <- d %>% filter(if_all(all_of(COVARIABLES_PSM), ~ !is.na(.)))
  n_t_desp <- sum(d_completo$tratado == 1); n_c_desp <- sum(d_completo$tratado == 0)

  cat(sprintf("Eliminación por lista: tratados %d -> %d (pierde %d, %.1f%%) | controles %d -> %d (pierde %d, %.1f%%)\n",
              n_t_antes, n_t_desp, n_t_antes - n_t_desp,
              100 * (n_t_antes - n_t_desp) / max(n_t_antes, 1),
              n_c_antes, n_c_desp, n_c_antes - n_c_desp,
              100 * (n_c_antes - n_c_desp) / max(n_c_antes, 1)))

  if (n_t_desp < n_t_antes) {
    cat("  AVISO: se perdieron tratados. Listar cuáles y verificar que no sean\n")
    cat("         sistemáticamente distintos (p. ej. todos amazónicos).\n")
    print(d %>% filter(tratado == 1, !COD_DANE %in% d_completo$COD_DANE) %>%
            select(COD_DANE, NOMBRE_MPI, DPTO_CNMBR), row.names = FALSE)
  }

  if (n_t_desp < 5) {
    cat("  *** Menos de 5 tratados: emparejamiento no informativo. Se omite. ***\n")
    return(NULL)
  }

  ## --- Modelo de propensión --------------------------------------------------
  formula_ps <- as.formula(paste("tratado ~", paste(COVARIABLES_PSM, collapse = " + ")))
  cat("\nModelo de propensión:\n  ", deparse1(formula_ps), "\n")

  args_matchit <- list(
    formula  = formula_ps,
    data     = d_completo,
    method   = METODO_MATCHING,
    distance = "glm",
    link     = "logit",
    ## Descarta unidades fuera del soporte comun antes de formar subclases.
    ## Con 1:1 esa exclusion la hacia implicitamente el calibre.
    discard  = "both"
  )
  
  if (METODO_MATCHING == "nearest") {
    args_matchit$replace <- REPLACE_MATCHING
    args_matchit$ratio   <- RATIO_MATCHING
    if (!is.null(CALIPER_SD)) {
      args_matchit$caliper     <- CALIPER_SD
      args_matchit$std.caliper <- TRUE
    }
  }
  
  if (EXACTO_DEPARTAMENTO) args_matchit$exact <- ~ DPTO_CNMBR

  set.seed(SEMILLA)
  m_out <- do.call(matchit, args_matchit)

  ## --- Soporte común (sobre el puntaje estimado, ANTES de emparejar) --------
  soporte <- diagnosticar_soporte_comun(
    ps = m_out$distance, tratado = d_completo$tratado,
    etiqueta = etiqueta, sufijo = sufijo
  )

  tabla_soporte <- d_completo %>%
    mutate(
      ps = as.numeric(m_out$distance),
      fuera_soporte = ps < soporte$lim_inf | ps > soporte$lim_sup,
      ps_extremo    = ps < 0.01 | ps > 0.99
    ) %>%
    select(COD_DANE, NOMBRE_MPI, DPTO_CNMBR, tratado, ps, fuera_soporte, ps_extremo) %>%
    arrange(desc(tratado), ps)
  write_csv(tabla_soporte, paste0("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/tablas/soporte_comun_", sufijo, ".csv"))

  ## --- Balance completo antes/después ----------------------------------------
  s <- summary(m_out, un = TRUE, interactions = FALSE)

  armar_bloque <- function(mat, momento) {
    if (is.null(mat)) return(NULL)
    df <- as.data.frame(mat)
    df$covariable <- rownames(mat)
    df$momento <- momento
    df
  }
  balance_largo <- bind_rows(
    armar_bloque(s$sum.all, "antes"),
    armar_bloque(s$sum.matched, "despues")
  )

  cols_deseadas <- intersect(
    c("Means Treated", "Means Control", "Std. Mean Diff.", "Var. Ratio", "eCDF Mean", "eCDF Max"),
    names(balance_largo)
  )
  balance_largo <- balance_largo %>%
    select(covariable, momento, all_of(cols_deseadas)) %>%
    rename(
      media_tratados = `Means Treated`,
      media_controles = `Means Control`,
      dif_estandarizada = `Std. Mean Diff.`
    ) %>%
    rename_with(~ c("razon_varianzas", "ecdf_media", "ecdf_max")[
      match(.x, c("Var. Ratio", "eCDF Mean", "eCDF Max"))],
      .cols = any_of(c("Var. Ratio", "eCDF Mean", "eCDF Max")))

  tabla_balance <- balance_largo %>%
    pivot_wider(
      names_from = momento, values_from = -c(covariable, momento),
      names_glue = "{.value}_{momento}"
    ) %>%
    mutate(mejora_abs = abs(dif_estandarizada_antes) - abs(dif_estandarizada_despues))

  cat("\n--- Balance de covariables (antes / después) ---\n")
  cat("Criterios: |dif. estandarizada| < 0,1 y razón de varianzas en [0,5 ; 2,0].\n\n")
  print(
    tabla_balance %>%
      select(covariable, dif_estandarizada_antes, dif_estandarizada_despues,
             any_of(c("razon_varianzas_despues", "ecdf_max_despues"))) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))),
    row.names = FALSE
  )

  desbalanceadas <- tabla_balance %>%
    filter(covariable != "distance", abs(dif_estandarizada_despues) > 0.1) %>%
    pull(covariable)
  if (length(desbalanceadas) > 0) {
    cat("\n  AVISO: covariables aún desbalanceadas tras emparejar: ",
        paste(desbalanceadas, collapse = ", "), "\n", sep = "")
    cat("  Opciones: reducir el calibre, añadir mahvars, o incluirlas de todos\n")
    cat("  modos en xformla del script 03 (ajuste doblemente robusto).\n")
  } else {
    cat("\n  Todas las covariables cumplen |dif. estandarizada| < 0,1 tras emparejar.\n")
  }

  write_csv(tabla_balance, paste0("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/tablas/balance_psm_", sufijo, ".csv"))

  ## --- Love plot -------------------------------------------------------------
  df_love <- tabla_balance %>%
    filter(covariable != "distance") %>%
    select(covariable, dif_estandarizada_antes, dif_estandarizada_despues) %>%
    pivot_longer(-covariable, names_to = "momento", values_to = "dif") %>%
    mutate(momento = ifelse(grepl("antes", momento),
                            "Antes del emparejamiento", "Después del emparejamiento"))

  orden <- df_love %>% filter(grepl("Antes", momento)) %>% arrange(abs(dif)) %>% pull(covariable)
  df_love$covariable <- factor(df_love$covariable, levels = orden)

  p_love <- ggplot(df_love, aes(x = dif, y = covariable, color = momento, shape = momento)) +
    geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed", color = "grey60") +
    geom_vline(xintercept = 0, color = "grey30") +
    geom_point(size = 2.6) +
    scale_color_manual(values = c("Antes del emparejamiento" = "#B2182B",
                                  "Después del emparejamiento" = "#2166AC")) +
    labs(
      title = paste("Balance de covariables —", etiqueta),
      subtitle = "Líneas punteadas: umbral convencional de |0,1|",
      x = "Diferencia estandarizada de medias", y = NULL, color = NULL, shape = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  ruta_plot <- paste0("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/figuras/love_plot_", sufijo, ".png")
  ggsave(ruta_plot, p_love, width = 8, height = 5.5, dpi = 150)
  cat("\nFiguras: ", ruta_plot, " y output/figuras/soporte_comun_", sufijo, ".png\n", sep = "")

  ## --- Submuestra emparejada -------------------------------------------------
  matched_data <- match.data(m_out)
  cat("Municipios emparejados:", nrow(matched_data),
      "(", sum(matched_data$tratado == 1), "tratados,",
      sum(matched_data$tratado == 0), "controles )\n")

  pesos <- matched_data %>% select(COD_DANE, peso_psm = weights, subclase = subclass)
  write_csv(pesos, paste0("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/tablas/pesos_psm_", sufijo, ".csv"))
  if (any(abs(pesos$peso_psm - 1) > 1e-8)) {
    cat("\n  *** ATENCIÓN: los pesos NO son uniformes. 03_did_callaway_santanna.R\n")
    cat("      filtra por COD_DANE sin usar pesos, lo que sería INCORRECTO aquí.\n")
    cat("      Pasa `weightsname` a did::att_gt() usando pesos_psm_", sufijo, ".csv ***\n", sep = "")
  }

  list(
    matchit_obj            = m_out,
    municipios_emparejados = matched_data$COD_DANE,
    pesos                  = pesos,
    tabla_balance          = tabla_balance,
    soporte_comun          = soporte,
    covariables_usadas     = COVARIABLES_PSM,
    ventana_pre            = c(VENTANA_PRE_INICIO, VENTANA_PRE_FIN),
    excluye_cohortes_tempranas = excluir_cohortes_tempranas
  )
}

## =============================================================================
## 5. EJECUCIÓN
## =============================================================================

etiqueta_alta  <- "Confianza alta (Verra + Gold Standard)"
etiqueta_todas <- "Todas las fuentes (+ RENARE + Cercarbono)"

## --- Especificación principal ------------------------------------------------
resultado_alta <- correr_matching(
  datos_mun_base, "first_treat_alta", etiqueta_alta, "alta_confianza"
)
resultado_todas <- correr_matching(
  datos_mun_base, "first_treat_todas", etiqueta_todas, "todas_fuentes"
)

saveRDS(resultado_alta, "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/matching_alta_confianza.rds")
saveRDS(resultado_todas, "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/matching_todas_fuentes.rds")

## --- Sensibilidad: sin cohortes tempranas ------------------------------------
resultado_alta_sc <- correr_matching(
  datos_mun_base, "first_treat_alta", etiqueta_alta, "alta_confianza_sin_tempranas",
  excluir_cohortes_tempranas = TRUE
)
resultado_todas_sc <- correr_matching(
  datos_mun_base, "first_treat_todas", etiqueta_todas, "todas_fuentes_sin_tempranas",
  excluir_cohortes_tempranas = TRUE
)

saveRDS(resultado_alta_sc,  "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/matching_alta_confianza_sin_tempranas.rds")
saveRDS(resultado_todas_sc, "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/matching_todas_fuentes_sin_tempranas.rds")

## --- Comparación del balance entre ambas especificaciones --------------------
comparar_balance <- function(r_base, r_sc, etiqueta) {
  if (is.null(r_base) || is.null(r_sc)) return(invisible(NULL))
  comp <- r_base$tabla_balance %>%
    select(covariable, smd_principal = dif_estandarizada_despues) %>%
    full_join(
      r_sc$tabla_balance %>% select(covariable, smd_sin_tempranas = dif_estandarizada_despues),
      by = "covariable"
    )
  cat("\n\n", strrep("-", 78), "\n", sep = "")
  cat("COMPARACIÓN DE BALANCE —", etiqueta, "\n")
  cat(strrep("-", 78), "\n")
  print(comp %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
  comp
}

comp_alta  <- comparar_balance(resultado_alta,  resultado_alta_sc,  etiqueta_alta)
comp_todas <- comparar_balance(resultado_todas, resultado_todas_sc, etiqueta_todas)

if (!is.null(comp_alta))  write_csv(comp_alta,  "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/tablas/comparacion_balance_alta.csv")
if (!is.null(comp_todas)) write_csv(comp_todas, "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/tablas/comparacion_balance_todas.csv")

cat("\n\n", strrep("=", 78), "\n", sep = "")
cat("SALIDAS GENERADAS\n")
cat(strrep("=", 78), "\n")
cat("  output/matching_{alta_confianza,todas_fuentes}.rds            <- usados por el script 03\n")
cat("  output/matching_*_sin_tempranas.rds                           <- sensibilidad\n")
cat("  output/tablas/balance_psm_*.csv                               <- tabla de balance completa\n")
cat("  output/tablas/soporte_comun_*.csv                             <- puntajes y banderas por municipio\n")
cat("  output/tablas/pesos_psm_*.csv                                 <- pesos (weightsname del script 03)\n")
cat("  output/figuras/love_plot_*.png, soporte_comun_*.png\n")
