## =============================================================================
## 71_grupo_comparacion.R
##
## Escenario A (18/10/2026), respuesta a los comentarios de Jorge sobre el
## grupo de control (pp. 11 y 14) y sobre el desborde de dosis (J29).
##
## La especificacion principal NO cambia. Este script:
##   1. Reproduce la principal (DR, notyettreated) y se detiene si no coincide
##      con REF_ATT_DR: garantiza que las columnas nuevas son comparables.
##   2. Contraste: solo nunca tratados como comparacion (nevertreated), DR y
##      sin covariables. Si difiere de la principal, muestra el sesgo que
##      anticipa Jorge (los nunca tratados son distintos de los tratados, Tabla 3).
##   3. Sin los municipios con dosis desbordada (area REDD+ DECLARADA > area de
##      referencia; COL_DESBORDE en 00_config). Se conserva por trazabilidad.
##   3b. (26/10/2026, lista de chequeo 1, 3 y 6) Corregida la geografia con
##      los poligonos (76), ya no hay desbordes: la columna 6 de la Tabla 4
##      pasa a ser la muestra de tratados con UBICACION VERIFICADA, los que
##      algun poligono AD cubre (clase_geo = "poligono"). Salen los reubicados
##      (su poligono esta en otro municipio) y los que no tienen poligono.
##   4. Composicion efectiva del grupo de comparacion por (g, t):
##          C_{g,t} = {i: G_i = 0}  U  {i: G_i > t}
##      cuantos nunca tratados y cuantos aun no tratados entran en cada par.
##      Sirve para la nota de la Tabla 4 y el texto del capitulo 5.
##
## USO (desde tesis_gfc/)
##   Rscript scripts/71_grupo_comparacion.R
##
## Insumos:  outputs/40_muestra_A.rds (40), data/interim/dosis_tratamiento_final.csv
## Salidas:  outputs/tablas/71_grupo_comparacion.csv
##           outputs/tablas/71_composicion_comparacion.csv
##           outputs/tablas/71_desborde_dosis.csv
## =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(did)
})
source("scripts/00_config_did.R")

muestra <- cargar_muestra_principal(verificar = TRUE)   # 36 tratados, 986 controles limpios

## -----------------------------------------------------------------------------
## Estimador (mismo que correr_cs() del 40)
## -----------------------------------------------------------------------------
media_pre <- function(d) {
  t <- d %>% filter(G_dep > 0, year < G_dep)
  if (!nrow(t)) return(NA_real_)
  t %>% group_by(COD_DANE) %>% summarise(m = mean(.data[[OUTCOME]], na.rm = TRUE), .groups = "drop") %>%
    pull(m) %>% mean(na.rm = TRUE)
}

estimar <- function(d, xformla, etiqueta, control) {
  cat("\n-- ", etiqueta, " | control = ", control, "\n", sep = "")
  dd <- d %>% mutate(.gname = G_dep)
  set.seed(SEMILLA)
  out <- tryCatch(
    att_gt(yname = OUTCOME, tname = "year", idname = "id_num", gname = ".gname",
           xformla = xformla, data = dd, control_group = control, est_method = "dr",
           bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "id_num"),
    error = function(e) { cat("   ERROR att_gt:", conditionMessage(e), "\n"); NULL })
  if (is.null(out)) return(NULL)
  n_na <- sum(is.na(out$att))
  if (n_na) cat(sprintf("   Aviso: %d de %d pares (g,t) NA, excluidos con na.rm\n", n_na, length(out$att)))
  s <- aggte(out, type = "simple", na.rm = TRUE)
  b <- tryCatch(aggte(out, type = "dynamic", balance_e = BALANCE_E, min_e = 0, max_e = BALANCE_E, na.rm = TRUE),
                error = function(e) NULL)
  mp <- media_pre(dd)
  mun <- distinct(dd, COD_DANE, G_dep)
  cat(sprintf("   ATT %.2f (EE %.2f) IC95 [%.1f; %.1f] | relativo %.1f%% | %d tratados, %d nunca tratados\n",
              s$overall.att, s$overall.se, s$overall.att - 1.96 * s$overall.se,
              s$overall.att + 1.96 * s$overall.se, 100 * s$overall.att / mp,
              sum(mun$G_dep > 0), sum(mun$G_dep == 0)))
  data.frame(especificacion = etiqueta, control = control,
             covariables = !identical(xformla, ~1),
             n_tratados = sum(mun$G_dep > 0), n_nunca = sum(mun$G_dep == 0),
             att = s$overall.att, ee = s$overall.se,
             ic_inf = s$overall.att - 1.96 * s$overall.se, ic_sup = s$overall.att + 1.96 * s$overall.se,
             att_bal = if (is.null(b)) NA_real_ else b$overall.att,
             ee_bal  = if (is.null(b)) NA_real_ else b$overall.se,
             media_pre = mp, att_rel_pct = 100 * s$overall.att / mp,
             pares_na = n_na, pares_total = length(out$att))
}

## =============================================================================
## 1. Principal (validacion)
## =============================================================================
titulo("1. PRINCIPAL (validacion contra resultados-v1)")
res <- list()
res$principal <- estimar(muestra, COVARIABLES_XFORMLA, "Principal", CONTROL_PRINCIPAL)
if (is.null(res$principal) || abs(res$principal$att - REF_ATT_DR) > 1)
  stop(sprintf("La principal da %.2f y la referencia es %.2f: la muestra o la configuracion cambiaron.",
               res$principal$att, REF_ATT_DR))
cat("   OK: reproduce REF_ATT_DR\n")
res$principal_sincov <- estimar(muestra, ~1, "Principal sin covariables", CONTROL_PRINCIPAL)

## =============================================================================
## 2. Solo nunca tratados
## =============================================================================
titulo("2. CONTRASTE: SOLO NUNCA TRATADOS")
res$nunca     <- estimar(muestra, COVARIABLES_XFORMLA, "Solo nunca tratados", CONTROL_CONTRASTE)
res$nunca_sin <- estimar(muestra, ~1, "Solo nunca tratados sin covariables", CONTROL_CONTRASTE)

## =============================================================================
## 3. Sin municipios con dosis desbordada
## =============================================================================
titulo(sprintf("3. SIN MUNICIPIOS CON DOSIS DESBORDADA (%s)", COL_DESBORDE))
dos <- read_csv(RUTA_DOSIS_DECLARADA, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE))
## Si el CSV no trae las banderas, se reconstruyen de las dosis crudas
if (!"desborda_bosque" %in% names(dos) && "dosis_bosque_cruda" %in% names(dos))
  dos$desborda_bosque <- as.integer(dos$dosis_bosque_cruda > 1)
if (!"desborda_area" %in% names(dos) && "dosis_area_cruda" %in% names(dos))
  dos$desborda_area <- as.integer(dos$dosis_area_cruda > 1)
if (!COL_DESBORDE %in% names(dos))
  stop("El CSV de dosis no trae ", COL_DESBORDE, " ni la dosis cruda. Columnas: ", paste(names(dos), collapse = ", "))

trat <- muestra %>% filter(G_dep > 0) %>% distinct(COD_DANE, G_dep) %>% left_join(dos, by = "COD_DANE")
cat(sprintf("Tratados de la principal con desborde: area = %s | bosque = %s\n",
            if ("desborda_area" %in% names(trat)) sum(trat$desborda_area %in% 1) else "NA",
            if ("desborda_bosque" %in% names(trat)) sum(trat$desborda_bosque %in% 1) else "NA"))
desb <- trat %>% filter(.data[[COL_DESBORDE]] %in% 1)
print(as.data.frame(desb %>% select(any_of(c("COD_DANE", "G_dep", "dosis_area_cruda", "dosis_bosque_cruda")))),
      row.names = FALSE)
write_csv(desb, file.path(DIR_TAB, "71_desborde_dosis.csv"))
if (nrow(desb) != 10)
  cat(sprintf("[!] Se esperaban 10 municipios con desborde (comentario de Jorge) y hay %d. ",
              nrow(desb)), "Revisa COL_DESBORDE en 00_config antes de reportar.\n")

m_sd <- muestra %>% filter(!COD_DANE %in% desb$COD_DANE)
coh <- m_sd %>% filter(G_dep > 0) %>% distinct(COD_DANE, G_dep) %>% count(G_dep)
cat("Cohortes que quedan sin los desbordados:\n"); print(as.data.frame(coh), row.names = FALSE)
res$sin_desborde     <- estimar(m_sd, COVARIABLES_XFORMLA, "Sin dosis desbordada", CONTROL_PRINCIPAL)
res$sin_desborde_sin <- estimar(m_sd, ~1, "Sin dosis desbordada sin covariables", CONTROL_PRINCIPAL)

## =============================================================================
## 3b. Solo tratados con ubicacion verificada por poligono (76)
## =============================================================================
titulo("3b. SOLO TRATADOS CON UBICACION VERIFICADA POR POLIGONO")
if (!file.exists(RUTA_DOSIS_POLIGONOS)) stop("Falta ", RUTA_DOSIS_POLIGONOS, ": corre scripts/76_dosis_poligonos.py")
geo <- read_csv(RUTA_DOSIS_POLIGONOS, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE))
tg <- muestra %>% filter(G_dep > 0) %>% distinct(COD_DANE, G_dep) %>%
  left_join(geo %>% select(COD_DANE, all_of(COL_CLASE_GEO)), by = "COD_DANE")
print(table(tg[[COL_CLASE_GEO]], useNA = "ifany"))
fuera_geo <- tg$COD_DANE[!tg[[COL_CLASE_GEO]] %in% "poligono"]
write_csv(tg, file.path(DIR_TAB, "71_clase_geografica.csv"))
m_pol <- muestra %>% filter(!COD_DANE %in% fuera_geo)
cat("Cohortes con ubicacion verificada:\n")
print(as.data.frame(m_pol %>% filter(G_dep > 0) %>% distinct(COD_DANE, G_dep) %>% count(G_dep)), row.names = FALSE)
res$con_poligono     <- estimar(m_pol, COVARIABLES_XFORMLA, "Ubicacion verificada por poligono", CONTROL_PRINCIPAL)
res$con_poligono_sin <- estimar(m_pol, ~1, "Ubicacion verificada por poligono sin covariables", CONTROL_PRINCIPAL)

## =============================================================================
## 4. Composicion efectiva del grupo de comparacion
## =============================================================================
titulo("4. COMPOSICION DEL GRUPO DE COMPARACION POR (g, t)")
G <- distinct(muestra, COD_DANE, G_dep)$G_dep
anios <- sort(unique(muestra$year))
comp <- do.call(rbind, lapply(sort(unique(G[G > 0])), function(g) {
  do.call(rbind, lapply(anios[anios >= g], function(t)
    data.frame(cohorte = g, anio = t, e = t - g, n_tratados_cohorte = sum(G == g),
               nunca_tratados = sum(G == 0), aun_no_tratados = sum(G > t))))
}))
comp$total_comparacion <- comp$nunca_tratados + comp$aun_no_tratados
comp$pct_aun_no <- 100 * comp$aun_no_tratados / comp$total_comparacion
write_csv(comp, file.path(DIR_TAB, "71_composicion_comparacion.csv"))
cat(sprintf("Pares (g,t) post: %d | aun no tratados por par: min %d, max %d, mediana %.0f\n",
            nrow(comp), min(comp$aun_no_tratados), max(comp$aun_no_tratados), median(comp$aun_no_tratados)))
cat("Por cohorte, en el anio de adopcion (t = g):\n")
print(comp %>% filter(e == 0) %>% select(cohorte, n_tratados_cohorte, nunca_tratados, aun_no_tratados),
      row.names = FALSE)
cat("Nota: desde t = 2019 ya no quedan aun no tratados; la comparacion es solo con nunca tratados.\n")

## =============================================================================
## Resumen
## =============================================================================
titulo("RESUMEN")
tab <- do.call(rbind, Filter(Negate(is.null), res))
print(tab[, c("especificacion", "control", "n_tratados", "att", "ee", "ic_inf", "ic_sup", "att_rel_pct")],
      row.names = FALSE, digits = 4)
dif <- tab$att[tab$especificacion == "Solo nunca tratados"] - tab$att[tab$especificacion == "Principal"]
cat(sprintf("\nSolo nunca tratados - principal: %.1f ha (%.2f EE de la principal)\n", dif, dif / REF_EE_DR))
write_csv(tab, file.path(DIR_TAB, "71_grupo_comparacion.csv"))
cat("Guardado: outputs/tablas/71_grupo_comparacion.csv, 71_composicion_comparacion.csv, 71_desborde_dosis.csv,",
    "71_clase_geografica.csv\n")
