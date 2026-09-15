## =============================================================================
## 15_robustez_periodo_base_sutva.R
##
## Pruebas adicionales derivadas de la revisión del borrador (septiembre 2026).
## Cada bloque responde a un comentario concreto del documento revisado:
##
##   Bloque 1. Solapamiento entre definiciones de tratamiento
##             -> llena el marcador "X de los Y" de la Sección 4 y concilia
##                88/99 (Tabla 4.1) con 86/96 (suma de la Tabla 6.4).
##   Bloque 2. Sensibilidad al periodo base
##             -> CS (ancla en g-1) frente a imputación de Borusyak, Jaravel y
##                Spiess (2024), que usa todo el periodo previo, y TWFE.
##             -> Diagnóstico con signo del "pico preadopción" en g-1.
##   Bloque 3. Estudio de eventos con base universal + sensibilidad HonestDiD
##             (Rambachan y Roth, 2023).
##   Bloque 4. SUTVA: CS sin covariables excluyendo del control a los vecinos
##             (contigüidad reina) de municipios tratados.
##   Bloque 5. Amazonía: agregación por año calendario (¿el efecto arranca en
##             2016?) y balance de acciones subversivas 2003-2015.
##   Bloque 6. (Opcional) Plausibilidad por dilución: fracción del bosque
##             municipal cubierta por los proyectos.
##
## Requiere haber corrido 01_preparar_datos_did.R.
##
## USO (PowerShell, desde la carpeta scripts/):
##   Rscript 15_robustez_periodo_base_sutva.R
##
## Git: los .rds y .csv de output/ no deberían versionarse (tamaño). Verifica
## que output/ esté en .gitignore antes de hacer commit de este script.
## =============================================================================

## ---------------------------------------------------------------------------
## 0. Configuración
## ---------------------------------------------------------------------------

DATA_ROOT       <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/data"
OUTPUT_ROOT     <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs"
PANEL_ANALISIS  <- file.path(OUTPUT_ROOT, "panel_analisis_did.rds")
MUNICIPIOS_GPKG <- file.path(DATA_ROOT, "interim/municipios_clean.gpkg")
MUNICIPIOS_LAYER <- "municipios_clean"
CEDE_CARACT     <- file.path(DATA_ROOT, "raw/auxiliary/PANEL_CARACTERISTICAS_GENERALES(2024).dta")
# Opcional (bloque 6): CSV con columnas COD_DANE, area_proyecto_ha
PROYECTOS_AREA_CSV <- file.path(DATA_ROOT, "final/proyectos_area_por_municipio.csv")
OUT_DIR         <- file.path(OUTPUT_ROOT, "tablas/revision_sep2026")

SEMILLA  <- 20260824   # misma semilla que 03_did_callaway_santanna.R
BITERS   <- 999        # fijar el número de réplicas en TODOS los scripts
ANIO_MAX <- 2024L      # último año del panel
ANIO_ACUERDO <- 2016L  # Acuerdo Final (bloque 5)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

instalar_si_falta <- function(pkgs) {
  faltan <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltan) == 0) return(invisible(NULL))
  cat("Instalando:", paste(faltan, collapse = ", "), "\n")
  for (p in faltan) {
    ok <- tryCatch({ install.packages(p, repos = "https://cloud.r-project.org"); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    # HonestDiD puede no estar en CRAN para tu versión de R: respaldo desde GitHub
    if ((!ok || !requireNamespace(p, quietly = TRUE)) && p == "HonestDiD") {
      if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
      remotes::install_github("asheshrambachan/HonestDiD")
    }
  }
}
instalar_si_falta(c("dplyr", "readr", "haven", "did", "fixest",
                    "didimputation", "HonestDiD", "sf", "spdep"))

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(haven)
  library(did); library(fixest); library(didimputation)
  library(sf); library(spdep)
})

# Normalizador de códigos DANE: evita EXACTAMENTE el error de Antioquia/Atlántico
# ("05001" en texto frente a 5001 numérico) al cruzar panel y geometrías.
norm_dane <- function(x) formatC(as.integer(x), width = 5, flag = "0")

panel <- readRDS(PANEL_ANALISIS) %>% mutate(COD_DANE = norm_dane(COD_DANE))
stopifnot(all(c("first_treat_alta", "first_treat_todas", "loss_area_ha",
                "year", "id_num") %in% names(panel)))

# Cohortes posteriores al último año del panel (p. ej. 2025) no tienen ningún
# periodo tratado observado: dentro del panel son "nunca tratadas". Se recodifican
# a 0 explícitamente para que att_gt, didimputation y el PSM usen la misma regla.
preparar <- function(panel, col_g) {
  panel %>%
    mutate(.gname = .data[[col_g]],
           .gname = ifelse(.gname > ANIO_MAX, 0L, .gname),
           D = as.integer(.gname > 0 & year >= .gname))
}

# Envoltorio único de att_gt: misma semilla, mismo biters, mismos argumentos.
# Nota: base_period solo afecta a los coeficientes PRE; los ATT(g,t) post
# siempre se anclan en g-1-anticipation. Por eso el ATT simple no cambia
# entre "varying" y "universal", y hace falta BJS (bloque 2) para cambiar la
# base de los efectos post.
correr_cs <- function(d, yname = "loss_area_ha", xformla = NULL,
                      base = "varying", anticip = 0) {
  set.seed(SEMILLA)
  tryCatch(
    att_gt(yname = yname, tname = "year", idname = "id_num", gname = ".gname",
           xformla = xformla, data = d, control_group = "notyettreated",
           est_method = "dr", base_period = base, anticipation = anticip,
           bstrap = TRUE, biters = BITERS, cband = TRUE,
           clustervars = "id_num"),
    error = function(e) { cat("att_gt() falló:", conditionMessage(e), "\n"); NULL })
}

resumen_simple <- function(obj, etiqueta) {
  if (is.null(obj)) return(data.frame(especificacion = etiqueta, att = NA, se = NA))
  a <- aggte(obj, type = "simple", na.rm = TRUE)
  data.frame(especificacion = etiqueta, att = a$overall.att, se = a$overall.se)
}

## ---------------------------------------------------------------------------
## 1. Solapamiento entre definiciones (Sección 4, marcador "X de los Y")
## ---------------------------------------------------------------------------
cat("\n==== BLOQUE 1: solapamiento entre definiciones ====\n")

coh <- panel %>% distinct(COD_DANE, first_treat_alta, first_treat_todas)

resumen_def <- function(col) {
  g <- coh[[col]]
  c(registrados   = sum(g > 0),
    en_panel      = sum(g > 0 & g <= ANIO_MAX),
    cohorte_post  = sum(g > ANIO_MAX))
}
print(rbind(alta = resumen_def("first_treat_alta"),
            todas = resumen_def("first_treat_todas")))

en_alta  <- coh$first_treat_alta  > 0 & coh$first_treat_alta  <= ANIO_MAX
en_todas <- coh$first_treat_todas > 0 & coh$first_treat_todas <= ANIO_MAX
n_ambas   <- sum(en_alta & en_todas)
n_misma_g <- sum(en_alta & en_todas & coh$first_treat_alta == coh$first_treat_todas)
n_viola   <- sum(en_alta & !en_todas)   # debería ser 0 si el anidamiento es estricto

if (n_viola > 0) {
  cat("AVISO:", n_viola, "municipios tratados en 'alta' no aparecen en 'todas'.",
      "El anidamiento no es estricto; revisa 26_consolidar_fuentes_carbono.py\n")
}
cat(sprintf(paste0("\nTexto para la Sección 4:\n  'En este panel, %d de los %d municipios ",
                   "tratados bajo todas las fuentes lo están también bajo confianza alta ",
                   "(%.1f %%); de ellos, %d (%.1f %%) comparten el mismo año de adopción.'\n"),
            n_ambas, sum(en_todas), 100 * n_ambas / sum(en_todas),
            n_misma_g, 100 * n_misma_g / n_ambas))

# Municipios cuya cohorte se ADELANTA al añadir RENARE/Cercarbono
adelantados <- coh %>%
  filter(first_treat_alta > 0, first_treat_todas > 0,
         first_treat_todas < first_treat_alta)
cat("\nMunicipios que cambian de cohorte (todas < alta):", nrow(adelantados), "\n")
print(adelantados)
write_csv(adelantados, file.path(OUT_DIR, "b1_municipios_cambio_cohorte.csv"))

## ---------------------------------------------------------------------------
## 2. Sensibilidad al periodo base: CS vs. imputación BJS vs. TWFE
## ---------------------------------------------------------------------------
## Lógica. Para la cohorte g, CS compara contra g-1; un estimador que usa el
## promedio del periodo previo difiere en
##   -[ (Ybar^g_{g-1} - Ybar^g_{pre}) - (Ybar^C_{g-1} - Ybar^C_{pre}) ].
## Si la adopción responde a picos recientes de deforestación, el corchete es
## positivo y CS "encuentra" una reducción que puede ser reversión a la media.
## BJS imputa Y(0) con efectos fijos estimados SOLO en observaciones no tratadas
## (todo el periodo previo), es robusto a heterogeneidad y eficiente bajo
## errores esféricos.
cat("\n==== BLOQUE 2: periodo base ====\n")

fe_dos_vias <- function(y, i, t, tol = 1e-9, maxit = 5000) {
  # Proyecciones alternadas: alpha_i y delta_t sobre un panel posiblemente
  # desbalanceado (solo observaciones no tratadas). Sin dependencias externas.
  i <- as.character(i); t <- as.character(t)
  d <- tapply(y, t, mean) * 0
  a <- tapply(y, i, mean)
  for (k in seq_len(maxit)) {
    d_new <- tapply(y - a[i], t, mean)
    a_new <- tapply(y - d_new[t], i, mean)
    if (max(abs(a_new - a)) < tol && max(abs(d_new - d)) < tol) break
    a <- a_new; d <- d_new
  }
  list(alpha = a_new, delta = d_new, iter = k)
}

bjs_manual <- function(d, yname = "loss_area_ha") {
  u  <- d %>% filter(D == 0, !is.na(.data[[yname]]))
  fe <- fe_dos_vias(u[[yname]], u$id_num, u$year)
  tr <- d %>% filter(D == 1, !is.na(.data[[yname]]))
  y0 <- fe$alpha[as.character(tr$id_num)] + fe$delta[as.character(tr$year)]
  mean(tr[[yname]] - y0, na.rm = TRUE)
}

pico_preadopcion <- function(d, yname = "loss_area_ha") {
  # Desviación CON SIGNO de g-1 respecto del promedio previo (<= g-2),
  # tratados menos nunca tratados en los mismos años calendario.
  cohortes <- sort(unique(d$.gname[d$.gname >= 2003]))
  nunca <- d %>% filter(.gname == 0)
  res <- lapply(cohortes, function(g) {
    dev <- function(x) {
      x %>% group_by(id_num) %>%
        summarise(v = mean(.data[[yname]][year == g - 1], na.rm = TRUE) -
                      mean(.data[[yname]][year <= g - 2], na.rm = TRUE),
                  .groups = "drop") %>% pull(v)
    }
    vt <- dev(d %>% filter(.gname == g)); vc <- dev(nunca)
    data.frame(cohorte = g, n = length(vt),
               dev_tratados = mean(vt, na.rm = TRUE),
               dev_nunca = mean(vc, na.rm = TRUE))
  })
  out <- bind_rows(res) %>% mutate(diferencia = dev_tratados - dev_nunca)
  attr(out, "promedio_ponderado") <- with(out, sum(n * diferencia) / sum(n))
  out
}

tabla_b2 <- list()
for (col in c("first_treat_alta", "first_treat_todas")) {
  d <- preparar(panel, col)
  et <- ifelse(col == "first_treat_alta", "alta", "todas")

  cs0 <- resumen_simple(correr_cs(d), paste0("CS sin covariables (g-1) - ", et))

  # Imputación BJS con didimputation (0 = nunca tratado en gname)
  bjs <- tryCatch(
    did_imputation(data = d, yname = "loss_area_ha", gname = ".gname",
                   tname = "year", idname = "id_num", cluster_var = "id_num"),
    error = function(e) { cat("did_imputation falló:", conditionMessage(e), "\n"); NULL })
  bjs_df <- if (is.null(bjs)) data.frame(att = NA, se = NA) else
    data.frame(att = bjs$estimate[1], se = bjs$std.error[1])

  # Verificación cruzada de la estimación puntual (debe coincidir con didimputation)
  att_man <- bjs_manual(d)

  twfe <- feols(loss_area_ha ~ D | id_num + year, data = d, cluster = ~id_num)

  tabla_b2[[et]] <- bind_rows(
    cs0,
    data.frame(especificacion = paste0("Imputación BJS (todo el periodo previo) - ", et),
               att = bjs_df$att, se = bjs_df$se),
    data.frame(especificacion = paste0("BJS manual (control puntual) - ", et),
               att = att_man, se = NA),
    data.frame(especificacion = paste0("TWFE - ", et),
               att = unname(coef(twfe)["D"]), se = unname(se(twfe)["D"])))

  pico <- pico_preadopcion(d)
  cat("\nPico preadopción (", et, "): promedio ponderado tratados - nunca =",
      round(attr(pico, "promedio_ponderado"), 1), "ha\n")
  write_csv(pico, file.path(OUT_DIR, paste0("b2_pico_preadopcion_", et, ".csv")))

  # Estudio de eventos BJS con prueba de pretendencias (-5 a -1)
  bjs_es <- tryCatch(
    did_imputation(data = d, yname = "loss_area_ha", gname = ".gname",
                   tname = "year", idname = "id_num", cluster_var = "id_num",
                   horizon = 0:10, pretrends = -5:-1),
    error = function(e) NULL)
  if (!is.null(bjs_es)) write_csv(as.data.frame(bjs_es),
                                  file.path(OUT_DIR, paste0("b2_bjs_eventos_", et, ".csv")))
}
tabla_b2 <- bind_rows(tabla_b2) %>% mutate(t = att / se)
print(tabla_b2, digits = 4)
write_csv(tabla_b2, file.path(OUT_DIR, "b2_cs_bjs_twfe.csv"))
## Lectura: si BJS ≈ CS (negativo), la conclusión no depende de anclar en g-1.
## Si BJS se acerca a TWFE (≈ 0 o positivo) y el pico preadopción es positivo,
## el ATT negativo de CS es compatible con reversión a la media: debe
## reportarse así en la Sección 6 y en las conclusiones.

## ---------------------------------------------------------------------------
## 3. Estudio de eventos con base universal + HonestDiD
## ---------------------------------------------------------------------------
cat("\n==== BLOQUE 3: base universal + HonestDiD ====\n")

honest_cs <- function(es, Mbar = seq(0, 2, by = 0.5)) {
  # Varianza del vector de coeficientes dinámicos a partir de la función de
  # influencia (Callaway y Sant'Anna): V = IF'IF / n^2.
  inf <- es$inf.function$dynamic.inf.func.e
  n <- nrow(inf)
  V <- crossprod(inf) / n^2
  e <- es$egt; b <- es$att.egt
  ref <- which(e == -1)                  # normalizado a 0 con base universal
  b <- b[-ref]; V <- V[-ref, -ref]; e <- e[-ref]
  npre <- sum(e < 0); npost <- sum(e >= 0)
  lvec <- rep(1 / npost, npost)          # parámetro: promedio de efectos post
  list(
    original = HonestDiD::constructOriginalCS(
      betahat = b, sigma = V, numPrePeriods = npre, numPostPeriods = npost,
      l_vec = lvec),
    sensibilidad = HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat = b, sigma = V, numPrePeriods = npre, numPostPeriods = npost,
      Mbarvec = Mbar, l_vec = lvec))
}

for (col in c("first_treat_alta", "first_treat_todas")) {
  et <- ifelse(col == "first_treat_alta", "alta", "todas")
  d  <- preparar(panel, col)
  cs_u <- correr_cs(d, base = "universal")
  if (is.null(cs_u)) next
  es <- aggte(cs_u, type = "dynamic", min_e = -5, max_e = 5, na.rm = TRUE)

  png(file.path(OUT_DIR, paste0("b3_eventos_universal_", et, ".png")),
      width = 1350, height = 825, res = 150)
  print(ggdid(es, title = paste("Estudio de eventos, base universal -", et)))
  dev.off()

  h <- tryCatch(honest_cs(es), error = function(e) {
    cat("HonestDiD falló:", conditionMessage(e), "\n"); NULL })
  if (!is.null(h)) {
    cat("\nHonestDiD (", et, "): intervalos robustos para Mbar = 0, 0.5, ..., 2\n")
    print(h$sensibilidad)
    write_csv(as.data.frame(h$sensibilidad),
              file.path(OUT_DIR, paste0("b3_honestdid_", et, ".csv")))
  }
}
## Lectura: Mbar es el múltiplo de la mayor violación pre observada que se
## tolera en el periodo post. Reporta el "Mbar de quiebre" (el menor Mbar con
## el que el intervalo incluye 0). Con un ATT no significativo es esperable que
## el quiebre sea Mbar = 0: el valor de este bloque es documentarlo.

## ---------------------------------------------------------------------------
## 4. SUTVA: excluir del control a los vecinos de tratados
## ---------------------------------------------------------------------------
## Si hay fuga (vecinos deforestan más), los controles "aún no tratados" que son
## vecinos de tratados tienen un Y(0) inflado y el ATT se sesga ALEJÁNDOSE de
## cero. Se usa la versión sin covariables porque la DR pierde soporte común al
## excluir vecinos (hallazgo previo, que debe documentarse en la Sección 6).
cat("\n==== BLOQUE 4: SUTVA ====\n")

muni <- st_read(MUNICIPIOS_GPKG, layer = MUNICIPIOS_LAYER, quiet = TRUE)
if (any(!st_is_valid(muni))) muni <- st_make_valid(muni)
muni$COD_DANE <- norm_dane(muni$COD_DANE)
nb <- poly2nb(muni, queen = TRUE)
cat("Municipios sin vecinos (islas):", sum(card(nb) == 0), "\n")

vecinos_nunca_tratados <- function(d) {
  trat <- unique(d$COD_DANE[d$.gname > 0])
  idx  <- which(muni$COD_DANE %in% trat)
  v    <- unique(unlist(nb[idx])); v <- v[v > 0]
  nunca <- unique(d$COD_DANE[d$.gname == 0])
  intersect(muni$COD_DANE[v], nunca)
}

tabla_b4 <- list()
for (col in c("first_treat_alta", "first_treat_todas")) {
  et <- ifelse(col == "first_treat_alta", "alta", "todas")
  d  <- preparar(panel, col)
  vec <- vecinos_nunca_tratados(d)
  cat(et, ": se excluyen", length(vec), "municipios vecinos nunca tratados\n")
  tabla_b4[[et]] <- bind_rows(
    resumen_simple(correr_cs(d), paste0("CS sin covariables, control completo - ", et)),
    resumen_simple(correr_cs(d %>% filter(!COD_DANE %in% vec)),
                   paste0("CS sin covariables, sin vecinos - ", et)))
}
tabla_b4 <- bind_rows(tabla_b4)
print(tabla_b4, digits = 4)
write_csv(tabla_b4, file.path(OUT_DIR, "b4_sutva_sin_vecinos.csv"))
## Lectura: si el ATT se acerca a cero al excluir vecinos, parte del efecto
## estimado era contaminación del control por fuga.

## ---------------------------------------------------------------------------
## 5. Amazonía: ¿efecto de los proyectos o del choque posterior a 2016?
## ---------------------------------------------------------------------------
cat("\n==== BLOQUE 5: Amazonía ====\n")

region_df <- read_dta(CEDE_CARACT) %>%
  filter(ano == 2001) %>%
  transmute(COD_DANE = norm_dane(codmpio),
            region = case_when(gamazonia == 1 ~ "Amazonía", TRUE ~ "Otra"))

for (col in c("first_treat_alta", "first_treat_todas")) {
  et <- ifelse(col == "first_treat_alta", "alta", "todas")
  d  <- preparar(panel, col) %>% left_join(region_df, by = "COD_DANE") %>%
    filter(region == "Amazonía")

  cs_a <- correr_cs(d, yname = "tasa_deforestacion", base = "universal")
  if (is.null(cs_a)) next
  cal <- aggte(cs_a, type = "calendar", na.rm = TRUE)
  tab_cal <- data.frame(anio = cal$egt, att = cal$att.egt, se = cal$se.egt)
  cat("\nATT por año calendario (", et, "):\n"); print(tab_cal, digits = 3)
  write_csv(tab_cal, file.path(OUT_DIR, paste0("b5_amazonia_calendario_", et, ".csv")))
  # Lectura: si el efecto solo aparece desde 2016 y es nulo antes, la
  # explicación del Acuerdo Final gana peso frente al efecto de los proyectos.

  if ("acc_subversivas" %in% names(d)) {
    bal <- d %>% filter(year >= 2003, year < ANIO_ACUERDO) %>%
      group_by(COD_DANE) %>%
      summarise(tratado = any(.gname > 0),
                subv = mean(acc_subversivas, na.rm = TRUE), .groups = "drop")
    m <- tapply(bal$subv, bal$tratado, mean, na.rm = TRUE)
    s <- sqrt(mean(tapply(bal$subv, bal$tratado, var, na.rm = TRUE)))
    cat("Acciones subversivas 2003-2015 (media): control =", round(m["FALSE"], 2),
        "| tratados =", round(m["TRUE"], 2),
        "| dif. estandarizada =", round((m["TRUE"] - m["FALSE"]) / s, 3), "\n")
    print(wilcox.test(subv ~ tratado, data = bal, exact = FALSE))
  } else {
    cat("acc_subversivas no está en el panel anual; tómala del Panel CEDE de conflicto.\n")
  }
}

## ---------------------------------------------------------------------------
## 6. (Opcional) Plausibilidad por dilución
## ---------------------------------------------------------------------------
## ATT_mun ≈ s * tau_proy + (1 - s) * lambda, con s = área del proyecto / bosque
## base y lambda >= 0 si hay fuga intramunicipal. Con una reducción municipal
## de ~44 % del contrafactual, el efecto implícito dentro del proyecto sería
## tau_proy <= -0,44 / s: si s < 0,44 supera el 100 %, lo que es imposible.
if (file.exists(PROYECTOS_AREA_CSV)) {
  cat("\n==== BLOQUE 6: dilución ====\n")
  areas <- read_csv(PROYECTOS_AREA_CSV, show_col_types = FALSE) %>%
    mutate(COD_DANE = norm_dane(COD_DANE))
  dil <- panel %>% filter(year == 2001) %>%
    select(COD_DANE, baseline_forest_base) %>%
    inner_join(areas, by = "COD_DANE") %>%
    left_join(region_df, by = "COD_DANE") %>%
    mutate(s = pmin(area_proyecto_ha / baseline_forest_base, 1),
           tau_implicito = -0.44 / s)
  print(dil %>% group_by(region) %>%
          summarise(n = n(), s_mediana = median(s, na.rm = TRUE),
                    tau_implicito_mediano = median(tau_implicito, na.rm = TRUE)))
  write_csv(dil, file.path(OUT_DIR, "b6_dilucion.csv"))
} else {
  cat("\nBloque 6 omitido: no existe", PROYECTOS_AREA_CSV, "\n")
}

cat("\nListo. Resultados en:", OUT_DIR, "\n")
