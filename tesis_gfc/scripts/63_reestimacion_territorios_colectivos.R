## =============================================================================
## 63_reestimacion_territorios_colectivos.R
##
## Respuesta del jurado, comentario 3.8 (y 3.7): la especificacion principal
## controla por areas protegidas pero no por territorios colectivos, donde se
## ubica la mayoria de los proyectos REDD+ (25 registros en consejos y 23 en
## resguardos, Tabla A.3).
##
## Covariable nueva (salida de 62_territorios_colectivos.py):
##
##   TC_i = Area(Resguardos_i U Consejos_i) / Area municipal_i
##
## Dos versiones, porque las capas de la ANT solo traen la fecha del ULTIMO
## acto administrativo, no la del titulo original:
##   * tc_total_hoy (PRINCIPAL): titulacion vigente, sin corte de fecha.
##   * tc_total (SENSIBILIDAD): solo territorios cuyo ultimo acto es <= 2012.
##     Es una cota inferior: saca resguardos constituidos en los anios ochenta
##     y ampliados o aclarados despues (Gran Resguardo del Vaupes, ultimo acto
##     2024; Miriti-Parana, 2022), justo en la region de los tratados.
## La version vigente arriesga incluir titulos posteriores a la adopcion; la
## de corte, excluir titulos antiguos. Si el ATT es estable entre ambas, la
## eleccion no importa.
##
## Bloques
##   T0  Balance: medias de TC en tratados y controles y diferencia
##       estandarizada (fila nueva para la Tabla 3).
##   T1  Soporte comun: logit transversal del tratamiento con y sin TC.
##       Diagnostico aproximado: el estimador DR usa un propensity por cada
##       par (g, t), pero si el logit transversal ya separa, el de cada par
##       tambien. Se reporta el tamanio efectivo de Kish de los pesos de los
##       controles, p/(1 - p), comparable con el 26,8 del capitulo 6.
##   T2  Principal + TC (DR):
##         ATT(g,t) con X_i ampliado a las 11 covariables + tc_total_hoy
##         variantes: resguardos y consejos por separado; version con corte
##       y su estudio de eventos (e = -6..5), para revisar tendencias previas.
##   T3  Controles restringidos a municipios con territorios colectivos
##       (TC > 0 y TC >= 0,10, version vigente): compara municipios con
##       instituciones territoriales parecidas (comentario 3.7) y es la
##       respuesta al 3.8 si T2 pierde soporte comun.
##
## Regla de lectura (fijada ANTES de correr; se imprime al final)
##   R1  Si el ATT de T2 queda a menos de 0,5 EE del principal y su IC incluye
##       el cero: la conclusion no cambia. T2 entra como columna de
##       robustez (Tabla 10) y la principal se mantiene (escenario A).
##   R2  Si T2 falla, deja mas del 30 % de pares (g, t) en NA, o el Kish de
##       los controles cae a menos de la mitad: hay problema de soporte; se
##       reporta T3 como respuesta al 3.8 y se documenta la perdida de soporte.
##   R3  Si el ATT de T2 se mueve mas de 1 EE o su IC excluye el cero:
##       discutir con el asesor si TC debe entrar a la especificacion principal.
##
## USO (PowerShell, desde tesis_gfc/):
##   Rscript scripts/63_reestimacion_territorios_colectivos.R
##
## Insumos: outputs/40_muestra_A.rds, scripts/comun_controles_limpios.R,
##          data/final/panel_con_tratamiento_actualizado.csv,
##          data/interim/territorios_colectivos_municipio.csv   (62)
## Salidas: outputs/tablas/63_territorios_colectivos.csv
##          outputs/tablas/63_balance_tc.csv
##          outputs/tablas/63_soporte_comun.csv
##          outputs/tablas/63_estudio_eventos_tc.csv
##          outputs/figuras/63_estudio_eventos_tc.png
## =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(readr); library(did); library(ggplot2) })
## Las tildes de las figuras salen como ".." si la sesion no corre en UTF-8
## (causa de los acentos corruptos de ggsave). R >= 4.2 en Windows ya usa UTF-8.
if (!isTRUE(l10n_info()$`UTF-8`)) {
  ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "Spanish_Colombia.utf8" else "C.UTF-8"))
  if (!nzchar(ok)) warning("La sesion no esta en UTF-8: las tildes de la figura pueden salir mal. Usa R >= 4.2.")
}

RAIZ <- local({
  d <- normalizePath(getwd(), winslash = "/")
  repeat {
    if (file.exists(file.path(d, "data/.raiz_canonica"))) break
    p <- dirname(d); if (p == d) stop("No encuentro data/.raiz_canonica: corre desde tesis_gfc/"); d <- p
  }
  d
})
DIR_OUT <- file.path(RAIZ, "outputs"); DIR_TAB <- file.path(DIR_OUT, "tablas"); DIR_FIG <- file.path(DIR_OUT, "figuras")
for (d in c(DIR_TAB, DIR_FIG)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

## --- Parametros --------------------------------------------------------------
OUTCOME <- "loss_area_ha"; VENTANA <- 2013:2019
SEMILLA <- 20260824; BITERS <- 999
REF_PRINCIPAL <- -152.5     # ATT DR de la Tabla 4, columna (1), de la version entregada
TOL_REF       <- 0.5
UMBRALES_TC   <- c(0, 0.10) # T3: controles con TC > 0 y >= 0,10
TC_TOTAL  <- "tc_total_hoy"                              # version principal (vigente)
TC_PARTES <- c("tc_resguardos_hoy", "tc_consejos_hoy")
TC_CORTE  <- "tc_total"                                  # sensibilidad: ultimo acto <= 2012
## Nombre de la covariable de areas protegidas (RUNAP 2012) en 40_muestra_A.rds.
## Si no coincide, el script se detiene y lista las columnas candidatas.
VAR_AREAS_PROTEGIDAS <- "pct_sinap_pub_base"   # SINAP publico a 2012 (62_areas_protegidas.py, via 01)
COVS_BASE <- c("log_baseline_forest", "temp_media_c_base", "prec_anual_mm_base", "discapital_base",
               "disbogota_base", "distancia_mercado_base", "H_coca_base", "homicidios_base",
               "log_defor_pre_media", "defor_pre_tendencia")

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
titulo <- function(t) cat("\n", strrep("=", 76), "\n", t, "\n", strrep("=", 76), "\n", sep = "")
formula_de <- function(v) as.formula(paste("~", paste(v, collapse = " + ")))

## =============================================================================
## 1. Muestra principal (identica a la de 60_robusteces.R) y covariable TC
## =============================================================================
muestra <- readRDS(file.path(DIR_OUT, "40_muestra_A.rds"))
muestra$COD_DANE <- pad5(muestra$COD_DANE)
source(file.path(RAIZ, "scripts/comun_controles_limpios.R"))
cont <- municipios_con_proyecto(file.path(RAIZ, "data/final/panel_con_tratamiento_actualizado.csv"))
muestra <- aplicar_controles_limpios(muestra, cont, col_g = "G_dep")
muestra <- muestra %>% mutate(G = G_dep)

mun <- muestra %>% distinct(COD_DANE, G)
cat(sprintf("Muestra principal: %d tratados | %d controles\n", sum(mun$G > 0), sum(mun$G == 0)))
if (sum(mun$G == 0) != 986 || sum(mun$G > 0) != 36) stop("La muestra no coincide con la principal (36 / 986).")

if (!VAR_AREAS_PROTEGIDAS %in% names(muestra)) {
  cand <- grep("protegid|runap|sinap|pnn|^ap_|_ap$", names(muestra), value = TRUE, ignore.case = TRUE)
  stop("No encuentro '", VAR_AREAS_PROTEGIDAS, "' en 40_muestra_A.rds. Candidatas: ",
       if (length(cand)) paste(cand, collapse = ", ") else "(ninguna)",
       ". Ajusta VAR_AREAS_PROTEGIDAS al nombre que usa la especificacion principal.")
}
COVS_PRINCIPAL <- c(COVS_BASE, VAR_AREAS_PROTEGIDAS)
faltan <- setdiff(c(OUTCOME, "year", "id_num", COVS_PRINCIPAL), names(muestra))
if (length(faltan)) stop("40_muestra_A.rds no trae: ", paste(faltan, collapse = ", "))

ruta_tc <- file.path(RAIZ, "data/interim/territorios_colectivos_municipio.csv")
if (!file.exists(ruta_tc)) stop("Falta ", ruta_tc, ". Corre antes scripts/62_territorios_colectivos.py.")
tc <- read_csv(ruta_tc, col_types = cols(COD_DANE = col_character(), corte = col_character(), .default = col_double()),
               show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>%
  select(COD_DANE, all_of(c(TC_TOTAL, TC_PARTES, TC_CORTE)), corte)
if (any(grepl("contemporaneo|sin corte", tc$corte)))
  warning("TC se calculo SIN corte de fecha: no es de linea base. Declararlo en el texto.")
muestra <- muestra %>% select(-any_of(names(tc)[-1])) %>% left_join(tc, by = "COD_DANE")
sin_tc <- unique(muestra$COD_DANE[is.na(muestra[[TC_TOTAL]])])
if (length(sin_tc)) stop(length(sin_tc), " municipios de la muestra sin TC (p. ej. ", paste(head(sin_tc), collapse = ", "),
                         "). Revisa los codigos DANE de municipios_clean.gpkg.")

## =============================================================================
## 2. Estimacion (misma especificacion que 40/60: DR, notyettreated, bootstrap)
## =============================================================================
tab <- list()
correr <- function(d, vars, etiqueta, bloque) {
  xf <- if (length(vars)) formula_de(vars) else ~1
  set.seed(SEMILLA)
  avisos <- character(0)
  out <- tryCatch(withCallingHandlers(
    att_gt(yname = OUTCOME, tname = "year", idname = "id_num", gname = "G", xformla = xf,
           data = d %>% mutate(G = as.integer(G), id_num = as.integer(id_num)),
           control_group = "notyettreated", est_method = "dr",
           bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "id_num"),
    warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) { cat("   ERROR att_gt [", etiqueta, "]:", conditionMessage(e), "\n"); NULL })
  ## Avisos de soporte del DR (propensity cerca de 1, matriz singular): se cuentan, no se esconden
  n_sop <- sum(grepl("overlap|propensity|singular|ill-conditioned|fitted probabilities", avisos, ignore.case = TRUE))
  if (is.null(out)) {
    tab[[length(tab) + 1]] <<- data.frame(bloque = bloque, especificacion = etiqueta, n_tratados = NA, n_controles = NA,
      att = NA, ee = NA, ic_inf = NA, ic_sup = NA, gt_na = NA, gt_total = NA, avisos_soporte = n_sop, fallo = TRUE)
    return(invisible(NULL))
  }
  s <- aggte(out, type = "simple", na.rm = TRUE)
  cat(sprintf("   %-56s ATT %8.2f (EE %7.2f) IC [%8.2f; %8.2f] | NA %d/%d | avisos de soporte %d\n", etiqueta,
              s$overall.att, s$overall.se, s$overall.att - 1.96 * s$overall.se,
              s$overall.att + 1.96 * s$overall.se, sum(is.na(out$att)), length(out$att), n_sop))
  tab[[length(tab) + 1]] <<- data.frame(bloque = bloque, especificacion = etiqueta,
    n_tratados = n_distinct(d$COD_DANE[d$G > 0]), n_controles = n_distinct(d$COD_DANE[d$G == 0]),
    att = s$overall.att, ee = s$overall.se,
    ic_inf = s$overall.att - 1.96 * s$overall.se, ic_sup = s$overall.att + 1.96 * s$overall.se,
    gt_na = sum(is.na(out$att)), gt_total = length(out$att), avisos_soporte = n_sop, fallo = FALSE)
  invisible(list(att_gt = out, simple = s))
}

## --- Referencia: debe reproducir la Tabla 4 antes de agregar nada -----------
titulo("REFERENCIA: ESPECIFICACION PRINCIPAL")
ref <- correr(muestra, COVS_PRINCIPAL, "Principal (11 covariables)", "Ref")
if (is.null(ref) || abs(ref$simple$overall.att - REF_PRINCIPAL) > TOL_REF)
  stop(sprintf("La principal da %s y no reproduce %.1f: revisa COVS_PRINCIPAL o la muestra antes de seguir.",
               if (is.null(ref)) "error" else sprintf("%.2f", ref$simple$overall.att), REF_PRINCIPAL))

## --- T0. Balance ------------------------------------------------------------
titulo("T0. BALANCE DE TERRITORIOS COLECTIVOS (fila nueva para la Tabla 3)")
m_mun <- muestra %>% distinct(COD_DANE, G, across(all_of(c(TC_TOTAL, TC_PARTES, TC_CORTE)))) %>%
  mutate(tratado = as.integer(G > 0))
bal <- lapply(c(TC_TOTAL, TC_PARTES, TC_CORTE), function(v) {
  x1 <- m_mun[[v]][m_mun$tratado == 1]; x0 <- m_mun[[v]][m_mun$tratado == 0]
  data.frame(variable = v, media_tratados = mean(x1), media_controles = mean(x0),
             dif_estandarizada = (mean(x1) - mean(x0)) / sqrt((var(x1) + var(x0)) / 2),
             tratados_con_tc = sum(x1 > 0), controles_con_tc = sum(x0 > 0))
}) %>% bind_rows()
print(bal %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
write_csv(bal, file.path(DIR_TAB, "63_balance_tc.csv"))

## --- T1. Soporte comun -------------------------------------------------------
titulo("T1. SOPORTE COMUN (logit transversal, diagnostico)")
x_mun <- muestra %>% group_by(COD_DANE) %>% slice(1) %>% ungroup() %>% mutate(tratado = as.integer(G > 0))
soporte <- function(vars, etiqueta) {
  avisos <- character(0)
  fit <- withCallingHandlers(glm(reformulate(vars, "tratado"), data = x_mun, family = binomial()),
                             warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") })
  p <- fitted(fit); t <- x_mun$tratado
  w <- (p / (1 - p))[t == 0]                     # pesos de odds de los controles (ATT)
  data.frame(especificacion = etiqueta,
             controles_p_menor_001 = sum(p[t == 0] < 0.01), tratados_p_mayor_099 = sum(p[t == 1] > 0.99),
             p_min_tratados = min(p[t == 1]), p_max_controles = max(p[t == 0]),
             kish_controles = sum(w)^2 / sum(w^2),
             separacion = any(grepl("fitted probabilities numerically 0 or 1", avisos)))
}
sop <- bind_rows(soporte(COVS_PRINCIPAL, "Principal"),
                 soporte(c(COVS_PRINCIPAL, TC_TOTAL), "Principal + TC"),
                 soporte(c(COVS_PRINCIPAL, TC_PARTES), "Principal + resguardos y consejos"),
                 soporte(c(COVS_PRINCIPAL, TC_CORTE), "Principal + TC con corte 2012"))
print(sop %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
write_csv(sop, file.path(DIR_TAB, "63_soporte_comun.csv"))

## --- T2. Principal + TC ------------------------------------------------------
titulo("T2. PRINCIPAL + TERRITORIOS COLECTIVOS")
t2 <- correr(muestra, c(COVS_PRINCIPAL, TC_TOTAL), "Principal + TC (union)", "T2")
correr(muestra, c(COVS_PRINCIPAL, TC_PARTES), "Principal + resguardos y consejos", "T2")
correr(muestra, c(COVS_PRINCIPAL, TC_CORTE), "Principal + TC con corte 2012 (ultimo acto)", "T2")

if (!is.null(t2)) {
  din <- aggte(t2$att_gt, type = "dynamic", min_e = -6, max_e = 5, na.rm = TRUE)
  es <- data.frame(e = din$egt, att = din$att.egt, ee = din$se.egt, crit = rep(din$crit.val.egt, length(din$egt)), row.names = NULL) %>%
    mutate(inf = att - crit * ee, sup = att + crit * ee)
  write_csv(es, file.path(DIR_TAB, "63_estudio_eventos_tc.csv"))
  pre_sig <- es %>% filter(e < -1, inf > 0 | sup < 0)
  cat(sprintf("Estudio de eventos: %d coeficientes previos con banda simultanea que excluye el cero\n", nrow(pre_sig)))
  ## Etiquetas con escapes unicode: evita los acentos corruptos de ggsave en Windows
  g <- ggplot(es, aes(e, att)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept = -0.5, colour = "grey80") +
    geom_errorbar(aes(ymin = inf, ymax = sup), width = 0.15, colour = "grey30") +
    geom_point(size = 2) +
    labs(x = "Años desde la adopción", y = "ATT (hectáreas por año)",
         title = "Especificación principal con territorios colectivos",
         subtitle = "Doblemente robusto, referencia e = −1, bandas simultáneas al 95 %") +
    theme_minimal(base_size = 11)
  ggsave(file.path(DIR_FIG, "63_estudio_eventos_tc.png"), g, width = 8, height = 4.8, dpi = 300)
}

## --- T3. Controles con territorios colectivos -------------------------------
titulo("T3. CONTROLES RESTRINGIDOS A MUNICIPIOS CON TERRITORIOS COLECTIVOS")
for (u in UMBRALES_TC) {
  d3 <- muestra %>% filter(G > 0 | (if (u == 0) .data[[TC_TOTAL]] > 0 else .data[[TC_TOTAL]] >= u))
  et <- if (u == 0) "TC > 0" else sprintf("TC >= %.2f", u)
  cat(sprintf("\n[Controles %s] %d tratados | %d controles\n", et,
              n_distinct(d3$COD_DANE[d3$G > 0]), n_distinct(d3$COD_DANE[d3$G == 0])))
  correr(d3, COVS_PRINCIPAL, paste("Controles", et, "- DR"), "T3")
  correr(d3, character(0), paste("Controles", et, "- sin covariables"), "T3")
}

## =============================================================================
## 3. Resumen y regla de lectura
## =============================================================================
titulo("RESUMEN")
res <- bind_rows(tab)
print(res %>% mutate(across(where(is.numeric), ~ round(.x, 2))), row.names = FALSE)
write_csv(res, file.path(DIR_TAB, "63_territorios_colectivos.csv"))

titulo("REGLA DE LECTURA (fijada antes de correr)")
r0 <- res %>% filter(bloque == "Ref"); r2 <- res %>% filter(especificacion == "Principal + TC (union)")
k0 <- sop$kish_controles[sop$especificacion == "Principal"]; k2 <- sop$kish_controles[sop$especificacion == "Principal + TC"]
if (!nrow(r2) || isTRUE(r2$fallo) || r2$gt_na / r2$gt_total > 0.30 || k2 < k0 / 2) {
  cat("R2: problema de soporte comun al agregar TC (fallo, >30 % de pares NA o Kish a menos de la mitad:",
      sprintf("%.1f -> %.1f).", k0, k2), "\n    Reportar T3 como respuesta al 3.8 y documentar la perdida de soporte.\n")
} else {
  dif_ee <- abs(r2$att - r0$att) / r0$ee
  incluye0 <- r2$ic_inf <= 0 & r2$ic_sup >= 0
  cat(sprintf("Cambio del ATT: %.1f -> %.1f (%.2f EE) | IC con TC [%.1f; %.1f]\n", r0$att, r2$att, dif_ee, r2$ic_inf, r2$ic_sup))
  if (dif_ee < 0.5 && incluye0) {
    cat("R1: la conclusion no cambia. Agregar T2 como columna de robustez (Tabla 10); la principal se mantiene.\n")
  } else if (dif_ee > 1 || !incluye0) {
    cat("R3: el resultado se mueve. Discutir con el asesor si TC entra a la especificacion principal.\n")
  } else {
    cat("Zona intermedia (0,5-1 EE, IC incluye el cero): reportar como robustez y comentar el cambio en el texto.\n")
  }
}
cat("\nGuardado: outputs/tablas/63_territorios_colectivos.csv, 63_balance_tc.csv, 63_soporte_comun.csv,",
    "63_estudio_eventos_tc.csv y outputs/figuras/63_estudio_eventos_tc.png\n")
