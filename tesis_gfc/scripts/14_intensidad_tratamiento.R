## =============================================================================
## 14_intensidad_tratamiento.R
##
## Intensidad del tratamiento: en vez de un indicador binario (tratado/no
## tratado), se cuenta el NUMERO de proyectos de carbono activos por
## municipio-año, para evaluar si un mayor numero de proyectos simultaneos
## se asocia a un efecto mayor sobre la deforestacion (logica de
## dosis-respuesta).
##
## *** AVISO IMPORTANTE SOBRE LA MUESTRA ***
## Esta reconstruccion de eventos a nivel de proyecto individual (ver
## reconstruir_eventos_intensidad.py) integra las CUATRO fuentes: Verra (98
## proyectos con fecha valida), Gold Standard (12 proyectos, reconstruidos
## desde el archivo crudo mediante union espacial contra el shapefile real
## del DANE -- ver asignar_goldstandard_espacial.R), Cercarbono (16
## proyectos con municipio unico y fecha) y RENARE (31 proyectos, con el
## matching por texto RECONSTRUIDO desde cero en
## reconstruir_renare_matching.py, dado que el archivo de matching
## original disponible tenia la columna de municipio vacia).
##
## Con las cuatro fuentes, la muestra alcanza 104 municipios con al menos
## un proyecto -- cercano al rango de 88-99 municipios de la variable de
## tratamiento binaria usada en el resto de la tesis. Aun asi, el matching
## de RENARE es una reconstruccion propia del metodo documentado (36
## coincidencias unicas encontradas aqui, frente a 27 documentadas
## originalmente) y no es necesariamente identico al calculo original --
## se recomienda una revision manual antes de un uso extensivo en
## resultados publicables, igual que para el matching original.
##
## Dos enfoques complementarios:
##   A) Dosis continua: regresion TWFE con el numero de proyectos activos
##      como regresor continuo (mismo patron que el rezago espacial de
##      spillovers, script 10).
##   B) Categorias de intensidad: "baja" (1 proyecto) vs. "alta" (2+
##      proyectos simultaneos/sucesivos) como subgrupos de tratamiento,
##      cada uno estimado por separado con Callaway-Sant'Anna (mismo
##      patron que la heterogeneidad regional, script 11).
##
## Requiere: eventos_intensidad.csv (generado por
## reconstruir_eventos_intensidad.py) y el panel de 01_preparar_datos_did.R.
##
## USO
## ---
##   Rscript 14_intensidad_tratamiento.R
## =============================================================================

library(dplyr)
library(readr)
library(fixest)
library(did)

DATA_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/data"
OUTPUT_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs"
PANEL_ANALISIS <- file.path(OUTPUT_ROOT, "panel_analisis_did.rds")
EVENTOS_INTENSIDAD <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/scripts/data/interim/eventos_intensidad.csv"
dir.create(file.path(OUTPUT_ROOT, "tablas"), showWarnings = FALSE, recursive = TRUE)

panel <- readRDS(PANEL_ANALISIS)
eventos <- read_csv(EVENTOS_INTENSIDAD, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE)
eventos$COD_DANE <- formatC(as.integer(eventos$COD_DANE), width = 5, flag = "0")

cat("Eventos cargados:", nrow(eventos), "| municipios distintos:", n_distinct(eventos$COD_DANE), "\n")
cat("Fuentes incluidas:", paste(unique(eventos$fuente), collapse = ", "), "\n\n")

## =============================================================================
## 1. Calcular proyectos activos por municipio-año
##
## Un proyecto cuenta como activo en el año t si fecha_inicio <= t. Si tiene
## fecha_fin conocida, deja de contar cuando t > fecha_fin; si no tiene
## fecha_fin (frecuente en RENARE/Cercarbono, y en los pocos casos de Verra
## sin dato), se asume activo desde su inicio hasta el final del panel --
## un supuesto conservador que puede sobrestimar la duracion real.
## =============================================================================

anios_panel <- sort(unique(panel$year))

calcular_activos <- function(cod_dane_objetivo, anio_objetivo) {
  sub <- eventos %>% filter(COD_DANE == cod_dane_objetivo)
  if (nrow(sub) == 0) return(0)
  sum(
    sub$anio_inicio <= anio_objetivo &
    (is.na(sub$anio_fin) | sub$anio_fin >= anio_objetivo)
  )
}

# Vectorizado por eficiencia: se construye una tabla municipio-año con el
# conteo, en vez de aplicar la funcion fila por fila sobre el panel completo.
municipios_con_eventos <- unique(eventos$COD_DANE)
intensidad_tabla <- expand.grid(COD_DANE = municipios_con_eventos, year = anios_panel, stringsAsFactors = FALSE)
intensidad_tabla$n_proyectos_activos <- mapply(calcular_activos, intensidad_tabla$COD_DANE, intensidad_tabla$year)

panel <- panel %>%
  left_join(intensidad_tabla, by = c("COD_DANE", "year")) %>%
  mutate(n_proyectos_activos = ifelse(is.na(n_proyectos_activos), 0, n_proyectos_activos))

cat("Distribución de n_proyectos_activos (municipio-año, > 0 únicamente):\n")
print(table(panel$n_proyectos_activos[panel$n_proyectos_activos > 0]))

## =============================================================================
## Enfoque A: dosis continua (TWFE)
## =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("ENFOQUE A: dosis continua (regresion TWFE)\n")
cat(strrep("=", 70), "\n")

modelo_dosis <- feols(loss_area_ha ~ n_proyectos_activos | COD_DANE + year, data = panel, cluster = ~COD_DANE)
print(summary(modelo_dosis))
cat(
  "\nInterpretacion: el coeficiente de n_proyectos_activos estima el efecto marginal",
  "de tener UN proyecto de carbono adicional activo simultaneamente en el municipio.",
  "Si es negativo, es consistente con un efecto de dosis-respuesta (mas proyectos,",
  "menos deforestacion); si no es significativo, no hay evidencia de que la cantidad",
  "de proyectos (mas alla de la presencia/ausencia) importe.\n"
)

## =============================================================================
## Enfoque B: categorias de intensidad (Callaway-Sant'Anna por subgrupo)
## =============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("ENFOQUE B: categorias de intensidad (baja vs. alta)\n")
cat(strrep("=", 70), "\n")

max_intensidad_por_municipio <- panel %>%
  group_by(COD_DANE) %>%
  summarise(max_intensidad = max(n_proyectos_activos), .groups = "drop")

panel <- panel %>% left_join(max_intensidad_por_municipio, by = "COD_DANE")

correr_subgrupo_intensidad <- function(data_sub, etiqueta) {
  n_mun <- n_distinct(data_sub$COD_DANE)
  n_tratados <- data_sub %>% filter(first_treat_alta > 0) %>% distinct(COD_DANE) %>% nrow()
  cat(sprintf("\n%-20s municipios=%4d  tratados=%3d  ", etiqueta, n_mun, n_tratados))
  if (n_tratados < 3) { cat("-- omitido (menos de 3 tratados)\n"); return(NULL) }

  data_modelo <- data_sub %>% rename(.gname = first_treat_alta)
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
  if (is.null(agg)) { cat("-- no se pudo agregar\n"); return(NULL) }

  cat(sprintf("ATT=%.2f  SE=%.2f  IC95%%=[%.2f, %.2f]\n",
              agg$overall.att, agg$overall.se,
              agg$overall.att - 1.96 * agg$overall.se, agg$overall.att + 1.96 * agg$overall.se))

  data.frame(grupo = etiqueta, n_municipios = n_mun, n_tratados = n_tratados,
             att = agg$overall.att, se = agg$overall.se)
}

# Grupo "baja intensidad": tratados con maximo 1 proyecto simultaneo, mas
# todos los nunca-tratados como control comun.
panel_baja <- panel %>% filter(first_treat_alta == 0 | max_intensidad <= 1)
resultado_baja <- correr_subgrupo_intensidad(panel_baja, "Baja (1 proyecto)")

# Grupo "alta intensidad": tratados con 2+ proyectos simultaneos/sucesivos,
# mas todos los nunca-tratados como control comun.
panel_alta <- panel %>% filter(first_treat_alta == 0 | max_intensidad >= 2)
resultado_alta_int <- correr_subgrupo_intensidad(panel_alta, "Alta (2+ proyectos)")

tabla_intensidad <- bind_rows(resultado_baja, resultado_alta_int)
if (nrow(tabla_intensidad) > 0) {
  write_csv(tabla_intensidad, file.path(OUTPUT_ROOT, "tablas/intensidad_categorias.csv"))
  cat("\nGuardado: output/tablas/intensidad_categorias.csv\n")
}

cat(
  "\n\n*** Si el ATT de 'Alta (2+ proyectos)' es mas negativo (mayor efecto protector)",
  "que el de 'Baja (1 proyecto)', es evidencia de dosis-respuesta -- mas proyectos",
  "simultaneos se asocian a mayor reduccion de deforestacion. Esta reconstruccion ya",
  "integra las 4 fuentes (Verra, Gold Standard, RENARE, Cercarbono) -- 104 municipios,",
  "cercano al rango de 88-99 usado en el resto de la tesis. El matching de RENARE es",
  "una reconstruccion propia (36 coincidencias vs. 27 documentadas originalmente),",
  "por lo que se recomienda una revision manual antes de reportar como hallazgo final. ***\n"
)
