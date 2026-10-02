## =============================================================================
## 04b_diagnostico_discrepancias_numericas.R
##
## PUNTO 5 de la revision: resuelve cuatro discrepancias numericas
## detectadas en el borrador contra tus datos reales.
##
##   0) Solapamiento exacto entre "confianza alta" y "todas las fuentes",
##      para completar el parrafo nuevo propuesto para la Seccion 4.
##
##   1) La Tabla 6.3 (ATT por cohorte) suma 86 municipios en confianza alta
##      y 96 en todas las fuentes, mientras que la Tabla 4.1 reporta 88 y
##      99 tratados. Este bloque verifica si la diferencia se debe a
##      cohortes cuyo ATT(g,t) salio NA en TODOS sus pares grupo-tiempo, y
##      que por eso aggte(type="group") con na.rm=TRUE las omite de la
##      tabla final - comportamiento esperado del paquete, no un error de
##      captura, pero que debe documentarse explicitamente en la tesis.
##
##   2) La Seccion 4 menciona riesgo para "municipios cuya cohorte de
##      tratamiento es 2001 o 2002", pero no existe cohorte 2001 en la
##      Tabla 6.3. Este bloque confirma si existe o no un municipio con
##      first_treat == 2001 en el panel.
##
##   3) La Tabla 6.4 reporta a Orinoquia con valores identicos hasta el
##      segundo decimal en las cuatro columnas (alta/todas x
##      hectareas/tasa). Este bloque verifica si el conjunto de municipios
##      tratados de Orinoquia es literalmente identico entre definiciones,
##      lo que explicaria la coincidencia exacta.
##
## REQUIERE: output/panel_analisis_did.rds
##           output/resultados_did_completos.rds  (de 03_did_callaway_santanna.R,
##                                                  solo para el bloque 1)
##
## USO
## ---
##   Rscript 04b_diagnostico_discrepancias_numericas.R
##
## NOTA sobre el bloque 3: el nombre de columna de region se asume
## "region_dane". Si tu panel usa otro nombre (ej. "region", "REGION"),
## cambialo en la linea marcada mas abajo antes de correr.
## =============================================================================

library(dplyr)

OUTPUT_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs"
panel <- readRDS(file.path(OUTPUT_ROOT, "panel_analisis_did.rds"))

## -----------------------------------------------------------------------
## 0) Solapamiento entre definiciones de tratamiento
## -----------------------------------------------------------------------

cat(strrep("=", 70), "\n")
cat("0) SOLAPAMIENTO ENTRE 'CONFIANZA ALTA' Y 'TODAS LAS FUENTES'\n")
cat(strrep("=", 70), "\n")

tratados_alta <- panel %>% filter(first_treat_alta > 0) %>%
  distinct(COD_DANE) %>% pull(COD_DANE)
tratados_todas <- panel %>% filter(first_treat_todas > 0) %>%
  distinct(COD_DANE) %>% pull(COD_DANE)

n_alta <- length(tratados_alta)
n_todas <- length(tratados_todas)
n_interseccion <- length(intersect(tratados_alta, tratados_todas))
n_solo_alta <- length(setdiff(tratados_alta, tratados_todas))

cat("Tratados bajo confianza alta:  ", n_alta, "\n")
cat("Tratados bajo todas las fuentes:", n_todas, "\n")
cat("En la interseccion (en ambas):  ", n_interseccion, "\n")
cat("Solo en confianza alta (no deberia haber ninguno si es subconjunto):",
    n_solo_alta, "\n")
cat(sprintf("Porcentaje de 'confianza alta' contenido en 'todas las fuentes': %.1f%%\n",
            100 * n_interseccion / n_alta))

cat("\n>>> Usa estos numeros para completar el parrafo nuevo de la Seccion 4:\n")
cat(sprintf(">>> '%d de los %d municipios tratados bajo todas las fuentes\n", n_interseccion, n_todas))
cat(sprintf(">>> lo estan tambien bajo confianza alta (%.1f%% de solapamiento).'\n",
            100 * n_interseccion / n_alta))

if (n_solo_alta > 0) {
  cat("\nAVISO: hay", n_solo_alta, "municipios tratados bajo confianza alta que\n")
  cat("NO aparecen como tratados bajo todas las fuentes. Si 'todas las fuentes'\n")
  cat("deberia ser un superconjunto estricto de 'confianza alta', esto es un\n")
  cat("problema de construccion de la variable de tratamiento que hay que\n")
  cat("revisar antes de escribir el parrafo de la Seccion 4 - probablemente en\n")
  cat("el paso que consolida las columnas first_treat_alta / first_treat_todas.\n")
}

## -----------------------------------------------------------------------
## 1) Total de tratados vs. cohortes que aparecen en la tabla agregada
## -----------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n", sep = "")
cat("1) TOTAL DE TRATADOS vs. COHORTES ESTIMABLES EN aggte(type='group')\n")
cat(strrep("=", 70), "\n")

for (def in c("first_treat_alta", "first_treat_todas")) {
  cohortes <- panel %>%
    filter(.data[[def]] > 0) %>%
    distinct(COD_DANE, .data[[def]]) %>%
    count(.data[[def]], name = "n_municipios") %>%
    rename(cohorte = 1)

  cat("\n--", def, "--\n")
  cat("Cohortes presentes en el panel (por first_treat), suma =",
      sum(cohortes$n_municipios), "\n")
  print(cohortes, n = Inf)
}

if (file.exists(file.path(OUTPUT_ROOT, "resultados_did_completos.rds"))) {
  resultados <- readRDS(file.path(OUTPUT_ROOT, "resultados_did_completos.rds"))

  for (nombre in c("alta_dr", "todas_dr")) {
    r <- resultados[[nombre]]
    if (is.null(r)) next
    cat("\n--", nombre, "--\n")
    cat("Cohortes que SI aparecen en aggte(type='group') (las de la Tabla 6.3):\n")
    print(sort(r$grupo$egt))
  }

  cat("\n>>> Compara esta lista contra la de 'cohortes presentes en el panel'\n")
  cat(">>> de arriba. Cualquier cohorte que aparezca en el panel pero NO en\n")
  cat(">>> aggte(type='group') es la que explica la diferencia entre 88/99\n")
  cat(">>> (Tabla 4.1) y 86/96 (suma de la Tabla 6.3): se estimo con datos,\n")
  cat(">>> pero salio con TODOS sus pares (g,t) en NA, y aggte() con\n")
  cat(">>> na.rm=TRUE la excluye de la tabla por grupo. Esto debe agregarse\n")
  cat(">>> como nota al pie de la Tabla 6.3, no dejarse sin explicar.\n")
} else {
  cat("\nAviso: no encuentro output/resultados_did_completos.rds - corre\n")
  cat("primero 03_did_callaway_santanna.R para completar este bloque.\n")
}

## -----------------------------------------------------------------------
## 2) Existencia de cohorte 2001
## -----------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n", sep = "")
cat("2) EXISTENCIA DE COHORTE 2001\n")
cat(strrep("=", 70), "\n")

for (def in c("first_treat_alta", "first_treat_todas")) {
  n_2001 <- panel %>% filter(.data[[def]] == 2001) %>%
    distinct(COD_DANE) %>% nrow()
  cat(def, "- municipios con first_treat == 2001:", n_2001, "\n")
}
cat("\nSi ambos dan 0: la mencion a 'cohorte de tratamiento 2001' en la\n")
cat("Seccion 4 del borrador es un error y debe eliminarse, dejando solo 2002.\n")

## -----------------------------------------------------------------------
## 3) Orinoquia: ¿mismo conjunto de tratados en ambas definiciones?
## -----------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n", sep = "")
cat("3) ORINOQUIA - CONJUNTO DE TRATADOS, ALTA vs. TODAS\n")
cat(strrep("=", 70), "\n")

NOMBRE_COLUMNA_REGION <- "region_dane"  ## <-- cambia esto si tu columna se llama distinto
NOMBRE_VALOR_ORINOQUIA <- "Orinoquia"    ## <-- y esto si el valor exacto difiere (tildes, mayusculas)

if (NOMBRE_COLUMNA_REGION %in% names(panel)) {
  tratados_orinoquia_alta <- panel %>%
    filter(.data[[NOMBRE_COLUMNA_REGION]] == NOMBRE_VALOR_ORINOQUIA, first_treat_alta > 0) %>%
    distinct(COD_DANE) %>% pull(COD_DANE) %>% sort()

  tratados_orinoquia_todas <- panel %>%
    filter(.data[[NOMBRE_COLUMNA_REGION]] == NOMBRE_VALOR_ORINOQUIA, first_treat_todas > 0) %>%
    distinct(COD_DANE) %>% pull(COD_DANE) %>% sort()

  cat("Tratados Orinoquia (alta): ", length(tratados_orinoquia_alta), "\n")
  cat("Tratados Orinoquia (todas):", length(tratados_orinoquia_todas), "\n")
  cat("¿Conjuntos identicos?:",
      identical(tratados_orinoquia_alta, tratados_orinoquia_todas), "\n")

  if (identical(tratados_orinoquia_alta, tratados_orinoquia_todas)) {
    cat("\n-> Confirma la explicacion mas probable: en Orinoquia, RENARE y\n")
    cat("   Cercarbono no agregaron ningun municipio tratado adicional a los\n")
    cat("   ya identificados por Verra/Gold Standard. Los valores identicos\n")
    cat("   de la Tabla 6.4 son correctos y deben anotarse asi explicitamente\n")
    cat("   en la tabla (nota al pie: 'conjunto de tratados identico entre\n")
    cat("   definiciones en esta region'), no dejarse sin comentario.\n")
  } else {
    cat("\n-> Los conjuntos NO son identicos pese a dar el mismo ATT hasta el\n")
    cat("   segundo decimal. Esto es mas dificil de explicar por composicion\n")
    cat("   de la muestra y conviene revisar manualmente el codigo que\n")
    cat("   construyo la Tabla 6.4 para esta region especifica - podria haber\n")
    cat("   un error de referencia a la columna equivocada.\n")
  }
} else {
  cat("No encuentro la columna '", NOMBRE_COLUMNA_REGION, "' en el panel.\n", sep = "")
  cat("Columnas disponibles con 'reg' en el nombre:\n")
  print(grep("reg", names(panel), ignore.case = TRUE, value = TRUE))
  cat("Ajusta NOMBRE_COLUMNA_REGION arriba al nombre correcto y vuelve a correr.\n")
}

cat("\n", strrep("=", 70), "\n", sep = "")
cat("FIN DEL DIAGNOSTICO\n")
cat(strrep("=", 70), "\n")
