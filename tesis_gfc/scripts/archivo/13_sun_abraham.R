## =============================================================================
## 13_sun_abraham.R
##
## Estimador interaction-weighted de Sun y Abraham (2021), como verificacion
## adicional junto a Callaway-Sant'Anna (especificacion principal) y TWFE
## (comparacion de sesgo, script 09). Los tres estimadores abordan el mismo
## problema -- adopcion escalonada del tratamiento -- con supuestos y
## mecanicas de estimacion distintas; que coincidan en signo y magnitud
## aproximada es evidencia de robustez mas fuerte que cualquiera de los
## tres por separado.
##
## Implementado via fixest::feols() con sunab(), que ya viene instalado en
## este entorno (usado en el script 09 para TWFE). sunab() usa por defecto
## los "nunca tratados" (cohorte = 0, misma convencion que did::att_gt())
## como grupo de control, y agrega los coeficientes cohorte-tiempo en un
## unico ATT ponderado mediante aggregate(..., agg = "att").
##
## Requiere haber corrido 01_preparar_datos_did.R.
##
## USO
## ---
##   Rscript 13_sun_abraham.R
## =============================================================================

library(dplyr)
library(fixest)
library(readr)

DATA_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/data"
OUTPUT_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs"
PANEL_ANALISIS <- file.path(OUTPUT_ROOT, "panel_analisis_did.rds")
dir.create(file.path(OUTPUT_ROOT, "tablas"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_ROOT, "figuras"), recursive = TRUE, showWarnings = FALSE)

panel <- readRDS(PANEL_ANALISIS)

correr_sun_abraham <- function(data, col_gname, etiqueta) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("SUN & ABRAHAM (2021) --", etiqueta, "\n")
  cat(strrep("=", 70), "\n")

  data_modelo <- data %>% rename(.gname = all_of(col_gname))

  # sunab() requiere que la cohorte "nunca tratada" este marcada de forma
  # que el paquete la reconozca como control -- 0 ya cumple esta condicion
  # (mismo valor centinela usado en did::att_gt()).
  modelo <- feols(
    loss_area_ha ~ sunab(.gname, year) | COD_DANE + year,
    data = data_modelo,
    cluster = ~COD_DANE
  )

  cat("\n--- Coeficientes dinamicos (tiempo relativo al tratamiento) ---\n")
  print(summary(modelo))

  # ATT agregado (ponderado por tamano de cohorte-periodo), directamente
  # comparable con el ATT simple de Callaway-Sant'Anna.
  agg <- aggregate(modelo, agg = "att")
  cat("\n--- ATT agregado (ponderado, comparable con Callaway-Sant'Anna) ---\n")
  print(agg)

  list(modelo = modelo, att_agregado = agg, etiqueta = etiqueta)
}

resultado_alta <- correr_sun_abraham(panel, "first_treat_alta", "Confianza alta")
resultado_todas <- correr_sun_abraham(panel, "first_treat_todas", "Todas las fuentes")

## =============================================================================
## Grafico de coeficientes dinamicos (equivalente al estudio de eventos de
## Callaway-Sant'Anna, para comparacion visual directa)
## =============================================================================

for (nombre in c("alta", "todas")) {
  resultado <- if (nombre == "alta") resultado_alta else resultado_todas
  ruta <- file.path(OUTPUT_ROOT, "figuras", paste0("sun_abraham_event_study_", nombre, ".png"))
  png(ruta, width = 1350, height = 825, res = 150)
  iplot(resultado$modelo, main = paste("Sun & Abraham (2021) --", resultado$etiqueta),
        xlab = "Años desde el inicio del tratamiento", ylab = "Efecto sobre hectareas deforestadas")
  dev.off()
  cat("Grafico guardado:", ruta, "\n")
}

## =============================================================================
## Tabla comparativa: Sun-Abraham vs. Callaway-Sant'Anna vs. TWFE
##
## Los numeros de Callaway-Sant'Anna y TWFE de abajo son los ya obtenidos en
## corridas previas (03_did_callaway_santanna.R, 09_tablas_regresion.R) --
## se referencian aqui como texto para armar la comparacion completa; no se
## recalculan en este script.
## =============================================================================

tabla_comparativa <- data.frame(
  estimador = c("Sun y Abraham (2021)", "Sun y Abraham (2021)"),
  definicion = c("Confianza alta", "Todas las fuentes"),
  att = c(resultado_alta$att_agregado["ATT", "Estimate"], resultado_todas$att_agregado["ATT", "Estimate"]),
  se = c(resultado_alta$att_agregado["ATT", "Std. Error"], resultado_todas$att_agregado["ATT", "Std. Error"])
)
write_csv(tabla_comparativa, file.path(OUTPUT_ROOT, "tablas/sun_abraham_resultados.csv"))
cat("\n\nGuardado: output/tablas/sun_abraham_resultados.csv\n")

cat(
  "\n*** Compara esta tabla contra tus resultados ya obtenidos de Callaway-Sant'Anna",
  "(sin covariables: -121.02 / -101.55; doblemente robusto: -56.40 / -50.79) y TWFE",
  "(+49.66 / +35.48). Si Sun-Abraham coincide en signo con Callaway-Sant'Anna (negativo)",
  "y no con TWFE, tienes ahora TRES estimadores distintos confirmando la misma",
  "direccion -- una defensa metodologica mucho mas fuerte que un solo estimador.",
  "Si ademas la magnitud de Sun-Abraham se acerca a la de Callaway-Sant'Anna,",
  "puedes reportarlo como confirmacion cruzada explicita en la sintesis del",
  "Capitulo 6, y esto justificaria reinstaurar honestamente la mencion a",
  "Sun-Abraham como metodo 'empleado en este trabajo' en el Capitulo 2. ***\n"
)
