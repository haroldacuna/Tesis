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

## --- Raíz canónica -----------------------------------------------------------
## Se resuelve subiendo directorios hasta encontrar el centinela
## data/.raiz_canonica, igual que los scripts de Python 26/34/35/37. Sustituye
## a la ruta absoluta que había aquí: en este repo llegaron a coexistir cuatro
## árboles de datos (el real, scripts/data, y dos dentro del worktree de git),
## y una ruta absoluta funciona en esta máquina pero no documenta cuál es el
## árbol bueno ni protege contra correr desde el equivocado.
RAIZ_PROYECTO <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "data/.raiz_canonica"))) {
    p <- dirname(d)
    if (identical(p, d)) {
      stop("No encuentro data/.raiz_canonica subiendo desde ", getwd(),
           ".\n  Corre el script desde tesis_gfc/ o crea el centinela.")
    }
    d <- p
  }
  d
})
cat("Raíz canónica:", RAIZ_PROYECTO, "\n")

DATA_ROOT        <- file.path(RAIZ_PROYECTO, "data")
RUTA_PANEL       <- file.path(DATA_ROOT, "final/panel_con_psm_covariables_reparado.csv")
RUTA_TRATAMIENTO <- file.path(DATA_ROOT, "final/panel_con_tratamiento_actualizado.csv")
ANIO_BASE        <- 2001L  # primer año del panel; usado como corte pre-tratamiento

## --- Definiciones de tratamiento ---------------------------------------------
## 26_consolidar_fuentes_carbono.py produce varias columnas de año de inicio,
## una por definición. Aquí se generan las variables first_treat_* de todas las
## que existan, para poder estimar cualquiera sin volver a tocar este script.
##
##   alta / todas              AFOLU (definición amplia, D3)
##   alta_redd / todas_redd    REDD+ estricto: deforestación o degradación
##                             EVITADA, el único mecanismo que opera sobre el
##                             mismo margen que el outcome (pérdida de cobertura)
##   todas_redd_sinmanual      D1 excluyendo la ubicación recuperada a mano de
##                             las fichas del registro de Cercarbono; el
##                             contraste contra todas_redd es la robustez frente
##                             al error de cobertura documentado
##
## DEFINICION_PRINCIPAL fija cuál alimenta la columna `first_treat`, que es la
## que leen 02_matching_psm.R y 03_did_callaway_santanna.R. Cambiarla aquí
## cambia la especificación principal en toda la cadena, en un solo sitio.
DEFINICIONES <- c(
  alta                 = "anio_inicio_tratamiento_alta_confianza",
  todas                = "anio_inicio_tratamiento_todas_fuentes",
  alta_redd            = "anio_inicio_tratamiento_alta_confianza_redd",
  todas_redd           = "anio_inicio_tratamiento_todas_fuentes_redd",
  todas_redd_sinmanual = "anio_inicio_tratamiento_todas_fuentes_redd_sinmanual"
)
DEFINICION_PRINCIPAL <- "todas_redd"

## Umbral mínimo de municipios por cohorte para REPORTAR el ATT(g,t).
## Fijado antes de ver resultados: con 1 o 2 unidades el SE del bootstrap
## multiplicador colapsa artificialmente y la cohorte aparece significativa sin
## serlo. NO se filtra el panel aquí a propósito: quitar esas unidades también
## las sacaría del grupo de comparación y movería el ATT agregado por una razón
## distinta de la que se quiere. El filtro se aplica al reportar, en 03.
UMBRAL_COHORTE <- 3L

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

DIR_OUTPUT <- file.path(RAIZ_PROYECTO, "outputs")
dir.create(DIR_OUTPUT, showWarnings = FALSE, recursive = TRUE)
OUT_PANEL_ANALISIS   <- file.path(DIR_OUTPUT, "panel_analisis_did.rds")
OUT_COVARIABLES_BASE <- file.path(DIR_OUTPUT, "covariables_base_municipio.rds")

## =============================================================================
## 1. Cargar y unir
## =============================================================================

cat("Cargando panel principal:", RUTA_PANEL, "\n")
panel <- read_csv(RUTA_PANEL, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE)

## --- Verificacion de integridad del panel de entrada -------------------------
## El panel tuvo un defecto que dejo a Antioquia y Atlantico con deforestacion
## nula durante los 24 anios, por un cruce fallido enmascarado con fillna(0).
## Se verifica aqui para que una lectura del archivo equivocado falle de
## inmediato, en lugar de propagarse a toda la cadena de estimacion.
verif <- panel %>%
  mutate(dpto = substr(COD_DANE, 1, 2)) %>%
  filter(dpto %in% c("05", "08")) %>%
  summarise(municipios = n_distinct(COD_DANE), perdida_ha = sum(loss_area_ha, na.rm = TRUE))

cat(sprintf("Verificacion Antioquia+Atlantico: %d municipios, %.0f ha\n",
            verif$municipios, verif$perdida_ha))

if (verif$perdida_ha < 600000) {
  stop("PANEL DEFECTUOSO: Antioquia y Atlantico suman ", round(verif$perdida_ha),
       " ha, cuando deberian sumar ~665.048. Se esta leyendo el archivo sin reparar.\n",
       "Ruta usada: ", RUTA_PANEL)
}

cat("Cargando tratamiento:", RUTA_TRATAMIENTO, "\n")
tratamiento <- read_csv(RUTA_TRATAMIENTO, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE)

# El archivo de tratamiento puede venir a nivel municipio (una fila) o
# municipio-año (repetido) — nos quedamos solo con las columnas de interés,
# una fila por municipio, para evitar duplicar filas al unir.
defs_presentes <- DEFINICIONES[DEFINICIONES %in% names(tratamiento)]
defs_faltantes <- DEFINICIONES[!DEFINICIONES %in% names(tratamiento)]

if (length(defs_faltantes) > 0) {
  cat("Aviso: estas definiciones no están en el archivo de tratamiento y se omiten:\n")
  for (i in seq_along(defs_faltantes)) {
    cat("   ", names(defs_faltantes)[i], "->", defs_faltantes[i], "\n")
  }
}

## La principal no es opcional: si falta, el resto de la cadena estimaría en
## silencio sobre otra definición. Falla aquí antes que propagarse.
if (!DEFINICION_PRINCIPAL %in% names(defs_presentes)) {
  stop(
    "Falta la columna de la definición principal '", DEFINICION_PRINCIPAL, "' (",
    DEFINICIONES[[DEFINICION_PRINCIPAL]], ") en\n  ", RUTA_TRATAMIENTO,
    "\n  Vuelve a correr 26_consolidar_fuentes_carbono.py con la versión que",
    " genera el eje REDD.\n  Columnas encontradas: ",
    paste(grep("anio_inicio_tratamiento", names(tratamiento), value = TRUE), collapse = ", ")
  )
}

tratamiento_mun <- tratamiento %>%
  select(all_of(c("COD_DANE", unname(defs_presentes)))) %>%
  distinct(COD_DANE, .keep_all = TRUE)

cat("\nMunicipios tratados por definición:\n")
for (i in seq_along(defs_presentes)) {
  alias <- names(defs_presentes)[i]
  marca <- if (identical(alias, DEFINICION_PRINCIPAL)) "  <- PRINCIPAL" else ""
  cat(sprintf("  %-22s %3d%s\n", alias,
              sum(!is.na(tratamiento_mun[[defs_presentes[i]]])), marca))
}

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

for (i in seq_along(defs_presentes)) {
  alias <- names(defs_presentes)[i]
  col   <- unname(defs_presentes[i])
  panel[[paste0("first_treat_", alias)]] <- ifelse(is.na(panel[[col]]), 0L, as.integer(panel[[col]]))
}

## Alias estable: `first_treat` siempre apunta a la definición principal, para
## que 02 y 03 no tengan que saber cuál es. Cambiar DEFINICION_PRINCIPAL arriba
## cambia la especificación en toda la cadena.
panel$first_treat <- panel[[paste0("first_treat_", DEFINICION_PRINCIPAL)]]
attr(panel, "definicion_principal") <- DEFINICION_PRINCIPAL
attr(panel, "columna_principal")    <- unname(DEFINICIONES[[DEFINICION_PRINCIPAL]])

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
cols_ft <- paste0("first_treat_", names(defs_presentes))
n_cohortes_tempranas <- panel %>%
  filter(year == ANIO_BASE) %>%
  filter(if_any(all_of(cols_ft), ~ .x > 0 & .x <= ANIO_BASE_CONFLICTO_TARDIO)) %>%
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

if (!"baseline_forest_base" %in% names(panel)) {
  cat("\n*** AVISO: no hay baseline_forest_base; tasa_deforestacion queda en NA.\n",
      "    baseline_forest.csv estaba vacío; regenerarlo desde treecover2000 de\n",
      "    Hansen si se quiere usar la tasa como outcome o el bosque como\n",
      "    denominador de la medida de intensidad. ***\n", sep = "")
  panel$baseline_forest_base <- NA_real_
}

panel <- panel %>%
  mutate(
    tasa_deforestacion = ifelse(
      !is.na(baseline_forest_base) & baseline_forest_base > 0,
      loss_area_ha / baseline_forest_base,
      NA_real_
    )
  )

n_sin_bosque <- sum(is.na(panel$tasa_deforestacion))
if (n_sin_bosque > 0) {
  cat(sprintf("\nAviso: %d de %d filas sin tasa_deforestacion (sin bosque base).\n",
              n_sin_bosque, nrow(panel)))
}

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

## --- Cohortes por definición --------------------------------------------------
## Se marca con * la cohorte que no alcanza UMBRAL_COHORTE. Esas cohortes SÍ se
## estiman (quedan en el panel) pero no deben leerse una por una: su SE colapsa
## por construcción. La marca está aquí para que la decisión esté a la vista
## antes de ver los ATT, no después.
base_mun <- panel %>% filter(year == ANIO_BASE)

for (alias in names(defs_presentes)) {
  col <- paste0("first_treat_", alias)
  tab <- base_mun %>% filter(.data[[col]] > 0) %>% count(.data[[col]], name = "municipios")
  names(tab)[1] <- "cohorte"
  tab <- tab %>% arrange(cohorte) %>%
    mutate(reportable = ifelse(municipios >= UMBRAL_COHORTE, "", " *"))

  marca <- if (identical(alias, DEFINICION_PRINCIPAL)) "  [PRINCIPAL]" else ""
  cat("\n=== Cohortes:", alias, marca, "===\n")
  cat(sprintf("  %d municipios tratados | %d cohortes | %d cohortes con >= %d municipios (%d municipios)\n",
              sum(tab$municipios), nrow(tab),
              sum(tab$municipios >= UMBRAL_COHORTE), UMBRAL_COHORTE,
              sum(tab$municipios[tab$municipios >= UMBRAL_COHORTE])))
  for (k in seq_len(nrow(tab))) {
    cat(sprintf("    %d  %3d%s\n", tab$cohorte[k], tab$municipios[k], tab$reportable[k]))
  }
}
cat("\n  (*) por debajo del umbral de", UMBRAL_COHORTE, "municipios: no reportar por cohorte.\n")

## --- Contraste entre definiciones ---------------------------------------------
## Cuánto del tratamiento depende de la recuperación manual de ubicación.
if (all(c("todas_redd", "todas_redd_sinmanual") %in% names(defs_presentes))) {
  con_m <- base_mun$COD_DANE[base_mun$first_treat_todas_redd > 0]
  sin_m <- base_mun$COD_DANE[base_mun$first_treat_todas_redd_sinmanual > 0]
  solo  <- setdiff(con_m, sin_m)
  cat("\n=== Aporte de la recuperación manual de ubicación ===\n")
  cat("  con recuperación:", length(con_m), "| sin ella:", length(sin_m),
      "| solo por ella:", length(solo), "\n")
  if (length(solo) > 0) cat("  ", paste(sort(solo), collapse = ", "), "\n")
}

## --- Verificacion post-escritura ---------------------------------------------
## Releer el archivo recien guardado y confirmar que contiene lo que debe.
## Una fecha de modificacion reciente NO garantiza contenido correcto: puede
## venir de una corrida anterior que escribio en otra carpeta.
control <- readRDS(OUT_PANEL_ANALISIS)
ha_control <- sum(control$loss_area_ha[substr(control$COD_DANE, 1, 2) %in% c("05", "08")])

cat(sprintf("\nControl post-escritura: %s\n", normalizePath(OUT_PANEL_ANALISIS)))
cat(sprintf("  Antioquia+Atlantico en el archivo guardado: %.0f ha\n", ha_control))

if (ha_control < 600000) {
  stop("El archivo guardado NO contiene los datos reparados. Revisar la ruta de salida.")
}
cat("  Verificacion correcta.\n")
