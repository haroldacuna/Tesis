## =============================================================================
## 03_did_callaway_santanna.R
##
## Modelo principal: diferencias en diferencias con adopción escalonada
## (Callaway y Sant'Anna, 2021), sobre la variable de tratamiento construida
## en esta tesis (presencia de proyectos de bonos de carbono).
##
## Corre TRES especificaciones, para cada una de las DOS definiciones de
## tratamiento (alta_confianza / todas_fuentes) = 6 modelos en total:
##
##   (a) Sin covariables - supone tendencias paralelas incondicionales.
##   (b) Doblemente robusto CON covariables de línea base (deforestación
##       histórica, clima, accesibilidad, conflicto armado) - la
##       especificación principal recomendada, porque no requiere descartar
##       observaciones como sí lo hace el emparejamiento.
##   (c) Sobre la submuestra emparejada por PSM (02_matching_psm.R) - para
##       cumplir explícitamente con el plan de emparejamiento de la
##       propuesta, como chequeo de robustez frente a (b).
##
## Para cada modelo se calculan: el ATT simple (promedio), el estudio de
## eventos (dinámico, por tiempo desde el tratamiento), por cohorte y por
## periodo calendario.
##
## USO
## ---
##   Rscript 03_did_callaway_santanna.R
##
## NOTA: att_gt() con bootstrap (bstrap = TRUE) puede tardar varios minutos
## por especificación con paneles grandes - normal, no es un error.
## =============================================================================

library(readr)
library(dplyr)
library(did)
library(ggplot2)

## --- Raíz canónica (mismo centinela que 01, 02 y los scripts de Python) -----
RAIZ_PROYECTO <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "data/.raiz_canonica"))) {
    p <- dirname(d)
    if (identical(p, d)) stop("No encuentro data/.raiz_canonica subiendo desde ", getwd())
    d <- p
  }
  d
})
DIR_OUT <- paste0(RAIZ_PROYECTO, "/outputs")

## Definiciones a estimar. att_gt() con bootstrap es caro, asi que por defecto
## se corren solo las tres REDD+: la principal, la de alta confianza y la que
## excluye la ubicacion recuperada a mano. Agregar "todas" o "alta" aqui para
## comparar contra la definicion AFOLU previa.
DEFS_A_CORRER <- c("todas_redd", "alta_redd", "todas_redd_sinmanual")

PANEL_ANALISIS <- paste0(DIR_OUT, "/panel_analisis_did.rds")
SEMILLA <- 20260824

dir.create(DIR_OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(DIR_OUT, "/figuras"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(DIR_OUT, "/tablas"), showWarnings = FALSE, recursive = TRUE)

panel <- readRDS(PANEL_ANALISIS)

## Covariables de línea base para la especificación doblemente robusta.
##
## AVISO METODOLÓGICO IMPORTANTE: con cohortes de tratamiento pequeñas
## (varias tienen 1-3 municipios - normal dado que solo 88-99 municipios en
## total están tratados, repartidos en ~17 años de cohorte), el estimador
## doblemente robusto puede no poder estimarse para varios pares grupo-tiempo
## específicos (matriz de covarianza singular / insuficientes grados de
## libertad). Se usa aquí una lista PARSIMONIOSA (6 covariables, no las 13
## disponibles) para mitigar esto - agregar más covariables reduce aún más
## las celdas estimables. Si tu corrida real muestra una tasa alta de NA
## (revisa el aviso que imprime correr_did() más abajo), considera:
##   - reducir aún más esta lista,
##   - agrupar cohortes adyacentes con muy pocos municipios,
##   - o apoyarte más en las especificaciones (a) y (c), que no dependen de
##     ajustar por tantas covariables a la vez.
## Se añaden las covariables que 02_matching_psm.R reporta como aún
## desbalanceadas tras emparejar bajo la definición REDD+ (|SMD| > 0,1):
## log_defor_pre_media, defor_pre_tendencia, disbogota_base y
## distancia_mercado_base. El emparejamiento se degrada al restringir a REDD+
## porque el pool de controles comparables se reduce —los municipios REDD+ son
## amazónicos y del Pacífico, remotos y muy boscosos, con puntajes muy por
## encima del control típico—, de modo que el ajuste doblemente robusto pasa a
## cargar el peso de la corrección y estas variables tienen que estar en él.
##
## Las dos de deforestación previa las construye 02 a nivel municipio; se unen
## al panel más abajo para que estén disponibles en xformla.
##
## Decisión tomada ANTES de estimar los ATT.
COVARIABLES_XFORMLA <- ~ log_baseline_forest + temp_media_c_base +
  prec_anual_mm_base + discapital_base + disbogota_base + distancia_mercado_base +
  H_coca_base + homicidios_base + log_defor_pre_media + defor_pre_tendencia

## =============================================================================
## Función auxiliar: corre att_gt() + las 4 agregaciones, para una combinación
## de (definición de tratamiento) x (especificación) x (variable de outcome)
## =============================================================================

correr_did <- function(data, col_gname, xformla, outcome, etiqueta,
                       weightsname = NULL) {
  cat("\n", strrep("-", 70), "\n", sep = "")
  cat("Modelo:", etiqueta, "| outcome:", outcome, "\n")
  cat(strrep("-", 70), "\n")
  
  data_modelo <- data %>% rename(.gname = all_of(col_gname))
  
  set.seed(SEMILLA)
  att_gt_out <- tryCatch({
    att_gt(
      yname = outcome,
      tname = "year",
      idname = "id_num",
      gname = ".gname",
      xformla = xformla,
      data = data_modelo,
      weightsname = weightsname,
      control_group = "notyettreated",
      est_method = "dr",
      bstrap = TRUE,
      cband = TRUE,
      clustervars = "id_num"
    )
  }, error = function(e) {
    cat("ERROR en att_gt():", conditionMessage(e), "\n")
    NULL
  })
  
  if (is.null(att_gt_out)) return(NULL)
  
  agg_simple <- aggte(att_gt_out, type = "simple", na.rm = TRUE)
  agg_dinamico <- aggte(att_gt_out, type = "dynamic", min_e = -10, max_e = 10, na.rm = TRUE)
  agg_grupo <- aggte(att_gt_out, type = "group", na.rm = TRUE)
  agg_calendario <- aggte(att_gt_out, type = "calendar", na.rm = TRUE)
  
  n_na <- sum(is.na(att_gt_out$att))
  if (n_na > 0) {
    cat("Aviso:", n_na, "de", length(att_gt_out$att),
        sprintf("(%.0f%%)", 100 * n_na / length(att_gt_out$att)),
        "pares grupo-tiempo con estimacion NA - excluidos de las agregaciones via na.rm = TRUE.\n")
    grupos_afectados <- att_gt_out$group[is.na(att_gt_out$att)]
    tabla_na <- sort(table(grupos_afectados), decreasing = TRUE)
    cat("  Cohortes mas afectadas (grupo = anio, n = cuantos pares grupo-tiempo de ese grupo salieron NA):\n")
    print(head(tabla_na, 8))
    cat("  Si esto es una fraccion alta (>30-40%), probablemente refleja cohortes muy chicas\n")
    cat("  (1-3 municipios) en tus datos reales, no un error del script - considera agrupar\n")
    cat("  cohortes adyacentes chicas, o apoyarte mas en las especificaciones (a)/(c).\n")
  }
  
  ## Valor critico bootstrap frente al normal. did lo calcula con cband=TRUE y
  ## advierte que con grupos pequenos puede superar ampliamente 1,96. Se imprime
  ## para que la eleccion del critico usado en los intervalos quede documentada.
  crit <- tryCatch(as.numeric(agg_simple$crit.val.egt), error = function(e) NA_real_)
  if (length(crit) != 1 || !is.finite(crit)) crit <- NA_real_
  
  cat("\nATT simple (promedio, todas las cohortes y periodos post-tratamiento):\n")
  cat(sprintf("  Estimado: %.4f   Error estandar: %.4f   IC 95%%: [%.4f, %.4f]\n",
              agg_simple$overall.att, agg_simple$overall.se,
              agg_simple$overall.att - 1.96 * agg_simple$overall.se,
              agg_simple$overall.att + 1.96 * agg_simple$overall.se))
  if (!is.na(crit)) {
    cat(sprintf("  Valor critico bootstrap: %.4f (normal: 1,96)   IC bootstrap: [%.4f, %.4f]\n",
                crit,
                agg_simple$overall.att - crit * agg_simple$overall.se,
                agg_simple$overall.att + crit * agg_simple$overall.se))
  }
  
  list(
    att_gt = att_gt_out,
    simple = agg_simple,
    dinamico = agg_dinamico,
    grupo = agg_grupo,
    calendario = agg_calendario,
    crit_bootstrap = crit,
    etiqueta = etiqueta
  )
}

## =============================================================================
## Preparar la submuestra emparejada (para la especificación (c))
## =============================================================================

cargar_submuestra_emparejada <- function(panel, ruta_matching) {
  if (!file.exists(ruta_matching)) {
    cat("Aviso: no encuentro", ruta_matching,
        "- corre primero 02_matching_psm.R. Se omite la especificación (c).\n")
    return(NULL)
  }
  m <- readRDS(ruta_matching)
  
  ## Con emparejamiento completo los pesos NO son uniformes: filtrar por
  ## COD_DANE sin ponderar produce estimaciones sesgadas.
  sub <- panel %>%
    filter(COD_DANE %in% m$municipios_emparejados) %>%
    left_join(m$pesos %>% select(COD_DANE, peso_psm), by = "COD_DANE")
  
  if (any(is.na(sub$peso_psm))) {
    stop("Municipios emparejados sin peso asignado: ",
         paste(head(unique(sub$COD_DANE[is.na(sub$peso_psm)]), 10), collapse = ", "))
  }
  
  uniformes <- all(abs(sub$peso_psm - 1) < 1e-8)
  cat(sprintf("  Submuestra (c): %d municipios | pesos %s (rango %.4f-%.4f)\n",
              n_distinct(sub$COD_DANE),
              if (uniformes) "uniformes" else "NO uniformes",
              min(sub$peso_psm), max(sub$peso_psm)))
  sub
}

## =============================================================================
## Ejecutar las combinaciones (DEFS_A_CORRER x 3 especificaciones)
## outcome principal: loss_area_ha. (tasa_deforestacion queda preparada como
## robustez adicional - descomentar para correrla también.)
## =============================================================================

## --- Covariables de deforestación previa -------------------------------------
## log_defor_pre_media, defor_pre_tendencia y log_baseline_forest las construye
## 02_matching_psm.R a nivel municipio y las exporta a CSV. Aquí se unen al
## panel porque xformla las necesita dentro del data.frame que recibe att_gt().
COLS_PRE <- c("log_defor_pre_media", "defor_pre_tendencia", "log_baseline_forest")

if (!all(COLS_PRE %in% names(panel))) {
  ruta_cov <- paste0(DIR_OUT, "/tablas/covariables_pre_municipio.csv")
  if (!file.exists(ruta_cov)) {
    stop("Falta ", ruta_cov, ".\n",
         "  Corre antes 02_matching_psm.R con la version que exporta las\n",
         "  covariables de deforestacion previa.")
  }
  cov_pre <- read_csv(ruta_cov, col_types = cols(COD_DANE = col_character()),
                      show_col_types = FALSE)
  cols_unir <- intersect(c("COD_DANE", COLS_PRE), names(cov_pre))
  panel <- panel %>% left_join(cov_pre[, cols_unir], by = "COD_DANE")
  cat("Covariables de deforestacion previa unidas al panel:",
      paste(setdiff(cols_unir, "COD_DANE"), collapse = ", "), "\n")
}

faltan_cov <- setdiff(all.vars(COVARIABLES_XFORMLA), names(panel))
if (length(faltan_cov) > 0) {
  stop("xformla pide covariables que no estan en el panel: ",
       paste(faltan_cov, collapse = ", "))
}
na_cov <- sapply(all.vars(COVARIABLES_XFORMLA), function(x) sum(is.na(panel[[x]])))
if (any(na_cov > 0)) {
  cat("Aviso: NA en covariables de xformla ->",
      paste(names(na_cov)[na_cov > 0], na_cov[na_cov > 0], collapse = " | "), "\n")
  cat("  att_gt() descarta esas unidades; revisa que no sean sistematicas.\n")
}

resultados <- list()

## Las definiciones se leen del panel, no se nombran a mano: 01 genera una
## columna first_treat_<alias> por cada una y declara la principal en un
## atributo. El .rds de emparejamiento lo produce 02 con el mismo alias, de
## modo que los tres scripts quedan acoplados por el alias y no por rutas.
NOMBRES_DEF <- c(
  alta                 = "AFOLU, confianza alta",
  todas                = "AFOLU, todas las fuentes",
  alta_redd            = "REDD+ estricto, confianza alta",
  todas_redd           = "REDD+ estricto, todas las fuentes",
  todas_redd_sinmanual = "REDD+ estricto, sin ubicacion recuperada a mano"
)

alias_en_panel  <- setdiff(sub("^first_treat_", "", grep("^first_treat_", names(panel), value = TRUE)), "")
alias_principal <- attr(panel, "definicion_principal")
faltan <- setdiff(DEFS_A_CORRER, alias_en_panel)
if (length(faltan) > 0) {
  stop("Estas definiciones no estan en el panel: ", paste(faltan, collapse = ", "),
       "
  Disponibles: ", paste(alias_en_panel, collapse = ", "),
       "
  Vuelve a correr 01_preparar_datos_did.R.")
}

definiciones <- setNames(lapply(DEFS_A_CORRER, function(a) {
  list(col = paste0("first_treat_", a),
       ruta_matching = paste0(DIR_OUT, "/matching_", a, ".rds"),
       nombre = if (a %in% names(NOMBRES_DEF)) unname(NOMBRES_DEF[a]) else a)
}), DEFS_A_CORRER)

cat("
Definiciones a estimar:
")
for (a in DEFS_A_CORRER) {
  cat(sprintf("  %-22s %s%s
", a, definiciones[[a]]$nombre,
              if (identical(a, alias_principal)) "   <- PRINCIPAL" else ""))
}

for (def in names(definiciones)) {
  info <- definiciones[[def]]
  
  # (a) sin covariables
  resultados[[paste0(def, "_sin_cov")]] <- correr_did(
    panel, info$col, ~1, "loss_area_ha",
    paste0(info$nombre, " - sin covariables")
  )
  
  # (b) doblemente robusto con covariables
  resultados[[paste0(def, "_dr")]] <- correr_did(
    panel, info$col, COVARIABLES_XFORMLA, "loss_area_ha",
    paste0(info$nombre, " - doblemente robusto (con covariables)")
  )
  
  # (c) muestra reponderada por emparejamiento
  panel_emparejado <- cargar_submuestra_emparejada(panel, info$ruta_matching)
  if (!is.null(panel_emparejado)) {
    resultados[[paste0(def, "_matched")]] <- correr_did(
      panel_emparejado, info$col, ~1, "loss_area_ha",
      paste0(info$nombre, " - muestra reponderada (PSM)"),
      weightsname = "peso_psm"
    )
  }
}
## =============================================================================
## Tabla resumen de robustez: ATT simple en las 6 especificaciones
## =============================================================================

tabla_resumen <- do.call(rbind, lapply(names(resultados), function(nombre) {
  r <- resultados[[nombre]]
  if (is.null(r)) return(NULL)
  data.frame(
    especificacion = r$etiqueta,
    att = r$simple$overall.att,
    se = r$simple$overall.se,
    ic_inf = r$simple$overall.att - 1.96 * r$simple$overall.se,
    ic_sup = r$simple$overall.att + 1.96 * r$simple$overall.se
  )
}))

cat("\n\n", strrep("=", 70), "\n", sep = "")
cat("TABLA RESUMEN DE ROBUSTEZ - ATT simple por especificacion\n")
cat(strrep("=", 70), "\n")
print(tabla_resumen, row.names = FALSE)

write_csv(tabla_resumen, paste0(DIR_OUT, "/tablas/resumen_robustez_att.csv"))

## =============================================================================
## Grafico de estudio de eventos - especificacion principal (b): doblemente
## robusto con covariables, para cada definicion de tratamiento.
## =============================================================================

for (def in names(definiciones)) {
  r <- resultados[[paste0(def, "_dr")]]
  if (is.null(r)) next

  p <- ggdid(r$dinamico) +
    labs(
      title = paste("Estudio de eventos -", definiciones[[def]]$nombre),
      subtitle = "Especificación doblemente robusta, con covariables de linea base",
      x = "Años desde el inicio del tratamiento", y = "ATT (efecto sobre hectareas deforestadas)"
    ) +
    theme_minimal()

  ruta <- paste0(paste0(DIR_OUT, "/figuras/event_study_"), def, ".png")
  ggsave(ruta, p, width = 9, height = 5.5, dpi = 150)
  cat("\nGrafico guardado:", ruta, "\n")
}

## =============================================================================
## Guardar todos los resultados para inspeccion posterior
## =============================================================================

saveRDS(resultados, paste0(DIR_OUT, "/resultados_did_completos.rds"))
cat("\nGuardado: outputs/resultados_did_completos.rds\n")
cat("Guardado: output/tablas/resumen_robustez_att.csv\n")
