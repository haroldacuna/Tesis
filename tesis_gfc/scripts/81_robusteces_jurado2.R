## =============================================================================
## 81_robusteces_jurado2.R
##
## Analisis que pide el segundo jurado. Todos usan la especificacion principal
## (D1 depurada, Callaway-Sant'Anna doblemente robusto, notyettreated,
## cohortes 2013-2019) definida en 00_config_did.R.
##
##   J1  ¿Que covariables absorben el efecto? (p. 14)
##       ATT agregando las covariables por bloques, de forma acumulada, cada
##       bloque solo y quitando un bloque a la vez. Si el nivel y la tendencia
##       de la deforestacion previa explican la mayor parte del cambio entre la
##       version sin covariables y la principal, se conecta con el argumento
##       de la linea base.
##   J2  Resultado incluyendo los 88 controles con algun vinculo con
##       proyectos de carbono (p. 9). La principal los excluye (controles
##       limpios, comun_controles_limpios.R).
##   J3  Heterogeneidad por gobernanza (pp. 15 y 20): tipo de territorio,
##       tipo de proponente y metodologia de Verra del proyecto que define la
##       adopcion de cada municipio (65_proyectos_territorio_proponente.csv).
##       Cada subgrupo de tratados contra los mismos 986 controles. Con pocos
##       tratados es DESCRIPTIVO; no se interpreta una diferencia entre
##       subgrupos salvo que su intervalo excluya el cero y cada subgrupo tenga
##       al menos dos cohortes con tres o mas municipios (regla D13).
##
## USO (PowerShell, desde tesis_gfc/):  Rscript scripts/81_robusteces_jurado2.R
##                                      Rscript scripts/81_robusteces_jurado2.R J1
## Salidas: outputs/tablas/81_bloques_covariables.csv, 81_con_88_controles.csv,
##          81_heterogeneidad_gobernanza.csv, 81_clasificacion_tratados.csv
## =============================================================================

source("scripts/00_config_did.R")
suppressPackageStartupMessages({ library(dplyr); library(did) })
if (!requireNamespace("tidyr", quietly = TRUE)) stop("Falta tidyr: install.packages(\"tidyr\")")
if (!isTRUE(l10n_info()$`UTF-8`))
  suppressWarnings(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "Spanish_Colombia.utf8" else "C.UTF-8"))

TODOS <- c("J1", "J2", "J3")
args <- toupper(commandArgs(trailingOnly = TRUE))
BLOQUES <- if (length(args)) intersect(args, TODOS) else TODOS
MIN_TRAT_SUBGRUPO <- 5          # subgrupos con menos tratados no se estiman

## --- Estimacion comun ----------------------------------------------------------
correr <- function(dat, vars, etiqueta, bloque, control = CONTROL_PRINCIPAL) {
  xf <- if (length(vars)) as.formula(paste("~", paste(vars, collapse = " + "))) else ~1
  nt <- n_distinct(dat$COD_DANE[dat$G > 0]); nc <- n_distinct(dat$COD_DANE[dat$G == 0])
  coh <- dat %>% filter(G > 0) %>% distinct(COD_DANE, G) %>% count(G)
  base <- data.frame(bloque = bloque, especificacion = etiqueta, n_tratados = nt, n_controles = nc,
                     cohortes_3mas = sum(coh$n >= MIN_COH))
  vacio <- cbind(base, att = NA, ee = NA, ic_inf = NA, ic_sup = NA, pre_promedio = NA, gt_na = NA, gt_total = NA)
  set.seed(SEMILLA)
  out <- tryCatch(suppressWarnings(att_gt(
    yname = OUTCOME, tname = "year", idname = "id_num", gname = "G", xformla = xf,
    data = dat %>% mutate(G = as.integer(G), id_num = as.integer(id_num)),
    control_group = control, est_method = "dr", bstrap = TRUE, biters = BITERS,
    cband = TRUE, clustervars = "id_num")), error = function(e) { cat("   ERROR [", etiqueta, "]:", conditionMessage(e), "\n"); NULL })
  if (is.null(out)) return(vacio)
  s <- tryCatch(aggte(out, type = "simple", na.rm = TRUE), error = function(e) NULL)
  if (is.null(s)) return(vacio)
  ## Promedio de los coeficientes previos (e = -5..-2): resume cuanto se mueve
  ## el estudio de eventos antes de la adopcion con cada conjunto de covariables
  din <- tryCatch(aggte(out, type = "dynamic", min_e = -E_PRE, max_e = E_POST, na.rm = TRUE), error = function(e) NULL)
  pre <- if (is.null(din)) NA else mean(din$att.egt[din$egt <= -2 & din$egt >= -E_PRE], na.rm = TRUE)
  r <- cbind(base, att = s$overall.att, ee = s$overall.se,
             ic_inf = s$overall.att - 1.96 * s$overall.se, ic_sup = s$overall.att + 1.96 * s$overall.se,
             pre_promedio = pre, gt_na = sum(is.na(out$att)), gt_total = length(out$att))
  cat(sprintf("   %-58s %2d trat | ATT %8.1f (EE %6.1f) | pre %7.1f | NA %d/%d\n",
              etiqueta, nt, r$att, r$ee, pre, r$gt_na, r$gt_total))
  r
}

m <- cargar_muestra_principal() %>% mutate(G = G_dep)
COVS <- all.vars(COVARIABLES_XFORMLA)

## =============================================================================
## J1. Covariables por bloques
## =============================================================================
if ("J1" %in% BLOQUES) {
  titulo("J1. COVARIABLES POR BLOQUES")
  GRUPOS <- list(
    "Deforestación previa (nivel y tendencia)" = c("log_defor_pre_media", "defor_pre_tendencia"),
    "Bosque en 2000" = "log_baseline_forest",
    "Clima" = c("temp_media_c_base", "prec_anual_mm_base"),
    "Accesibilidad" = c("discapital_base", "disbogota_base", "distancia_mercado_base"),
    "Coca y violencia" = c("H_coca_base", "homicidios_base"),
    "Áreas protegidas" = "pct_sinap_pub_base")
  stopifnot(setequal(unlist(GRUPOS), COVS))
  tab <- list(correr(m, character(0), "Sin covariables", "ninguna"))
  acum <- character(0)
  for (g in names(GRUPOS)) {                                   # acumulado, en el orden de la lista
    acum <- c(acum, GRUPOS[[g]])
    tab[[length(tab) + 1]] <- correr(m, acum, paste("+", g), "acumulado")
  }
  for (g in names(GRUPOS))                                     # cada bloque solo
    tab[[length(tab) + 1]] <- correr(m, GRUPOS[[g]], paste("Solo", tolower(g)), "solo")
  for (g in names(GRUPOS))                                     # todos menos uno
    tab[[length(tab) + 1]] <- correr(m, setdiff(COVS, GRUPOS[[g]]), paste("Todas menos", tolower(g)), "sin_bloque")
  j1 <- bind_rows(tab)
  ult <- j1$att[j1$bloque == "acumulado"]; ult <- ult[length(ult)]
  if (!is.na(ult) && abs(ult - REF_ATT_DR) > 0.5) warning("El acumulado completo no reproduce la principal (", round(ult, 2), ").")
  write.csv(j1, file.path(DIR_TAB, "81_bloques_covariables.csv"), row.names = FALSE)
}

## =============================================================================
## J2. Incluyendo los 88 controles con vinculo con proyectos de carbono
## =============================================================================
if ("J2" %in% BLOQUES) {
  titulo("J2. CON LOS 88 CONTROLES EXCLUIDOS")
  bruto <- readRDS(RUTA_MUESTRA_A); bruto$COD_DANE <- pad5(bruto$COD_DANE); bruto$G <- bruto$G_dep
  mun_b <- distinct(bruto, COD_DANE, G)
  cat(sprintf("Muestra sin depurar controles: %d tratados | %d controles (%d mas que la principal)\n",
              sum(mun_b$G > 0), sum(mun_b$G == 0), sum(mun_b$G == 0) - REF_N_CONTROLES))
  j2 <- bind_rows(
    correr(m, COVS, "Principal (controles limpios)", "J2"),
    correr(bruto, COVS, "Con los controles excluidos", "J2"),
    correr(m, character(0), "Principal sin covariables", "J2"),
    correr(bruto, character(0), "Con los controles excluidos, sin covariables", "J2"))
  write.csv(j2, file.path(DIR_TAB, "81_con_88_controles.csv"), row.names = FALSE)
}

## =============================================================================
## J3. Heterogeneidad por gobernanza
## =============================================================================
if ("J3" %in% BLOQUES) {
  titulo("J3. HETEROGENEIDAD POR GOBERNANZA (descriptiva)")
  ## Se lee sin reconvertir (encoding) y se quita a mano la marca BOM del primer
  ## nombre de columna: fileEncoding falla si el locale de R no es UTF-8
  leer <- function(r) {
    x <- read.csv(file.path(RAIZ_PROYECTO, r), colClasses = "character", encoding = "UTF-8", check.names = FALSE)
    names(x)[1] <- sub("^[^A-Za-z_]+", "", names(x)[1]); x
  }
  pr <- leer("data/interim/diagnostics/65_proyectos_territorio_proponente.csv")
  pr$COD_DANE <- pad5(pr$COD_DANE)
  ve <- leer("data/interim/verra_con_metodologia.csv")
  PAT <- "VM00(06|07|09|15|48)"
  ve$metodo <- ifelse(grepl(PAT, ve$methodology), sub(paste0("^.*?(", PAT, ").*$"), "\\1", ve$methodology, perl = TRUE), NA)
  ve <- distinct(ve, id, .keep_all = TRUE)
  pr <- pr %>% left_join(ve %>% select(id, projectId, metodo), by = "id") %>%
    ## clave comun con dosis_tratamiento_poligonos.csv: VCS<projectId> en Verra, el id del registro en los demas
    mutate(clave = ifelse(registro == "Verra" & !is.na(projectId), paste0("VCS", projectId), id))
  ## anio_inicio viene como texto ("2013", "01/0" en EcoRegistry): solo se usan los anios validos
  pr$anio <- suppressWarnings(as.numeric(pr$anio_inicio)); pr$anio[!(pr$anio %in% 1990:2030)] <- NA

  ## Vinculo proyecto-municipio. Dos fuentes: (1) el municipio de ubicacion
  ## registrado del proyecto (COD_DANE de la Tabla A.3) y (2) los proyectos cuyo
  ## poligono intersecta el municipio (columna proyectos_ad del 76). La (2)
  ## cubre los municipios tratados por un proyecto que se registro en otro.
  dz <- leer("data/interim/dosis_tratamiento_poligonos.csv"); dz$COD_DANE <- pad5(dz$COD_DANE)
  pol <- dz %>% filter(!is.na(proyectos_ad), proyectos_ad != "") %>%
    transmute(COD_DANE, clave = strsplit(proyectos_ad, ";")) %>% tidyr::unnest(clave) %>%
    mutate(clave = trimws(clave))
  vinc <- bind_rows(pr %>% select(COD_DANE, clave),
                    pol %>% filter(clave %in% pr$clave)) %>% distinct()
  tr <- m %>% filter(G > 0) %>% distinct(COD_DANE, G)
  ## Proyecto que define la adopcion: el de inicio mas temprano entre los
  ## vinculados (sin anio valido al final; Verra antes que otros registros)
  cls <- vinc %>% filter(COD_DANE %in% tr$COD_DANE) %>%
    left_join(pr %>% select(-COD_DANE) %>% distinct(clave, .keep_all = TRUE), by = "clave") %>%
    arrange(COD_DANE, is.na(anio), anio, registro != "Verra") %>%
    group_by(COD_DANE) %>% slice(1) %>% ungroup() %>%
    transmute(COD_DANE, proyecto = clave, nombre,
              territorio = case_when(territorio == "consejo comunitario" ~ "Consejo comunitario",
                                     territorio == "resguardo indigena" ~ "Resguardo indígena",
                                     TRUE ~ "Otro"),
              proponente = case_when(tipo_proponente == "empresa" ~ "Empresa",
                                     tipo_proponente == "comunitario" ~ "Comunitario",
                                     tipo_proponente %in% c("ong", "publico") ~ "ONG o público",
                                     TRUE ~ "Varios o sin detalle"),
              metodologia = case_when(!is.na(metodo) ~ metodo,
                                      registro == "Verra" ~ "Verra, otra metodología",
                                      TRUE ~ "Otro registro"))
  ## Los tratados sin proyecto en la Tabla A.3 (RENARE o Cercarbono sin poligono)
  tr <- tr %>% left_join(cls, by = "COD_DANE") %>%
    mutate(across(c(territorio, proponente, metodologia), ~ ifelse(is.na(.x), "Sin proyecto en la Tabla A.3", .x)))
  write.csv(tr, file.path(DIR_TAB, "81_clasificacion_tratados.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  for (v in c("territorio", "proponente", "metodologia")) { cat("\n", v, ":\n"); print(table(tr[[v]])) }

  res <- list()
  for (v in c("territorio", "proponente", "metodologia")) {
    for (k in sort(unique(tr[[v]]))) {
      ids <- tr$COD_DANE[tr[[v]] == k]
      if (length(ids) < MIN_TRAT_SUBGRUPO) { cat(sprintf("   %-58s %2d tratados -> se omite\n", paste(v, k), length(ids))); next }
      d <- m %>% filter(G == 0 | COD_DANE %in% ids)
      res[[length(res) + 1]] <- cbind(variable = v, grupo = k, correr(d, character(0), paste(v, "-", k, "- sin covariables"), "J3"))
      res[[length(res) + 1]] <- cbind(variable = v, grupo = k, correr(d, COVS, paste(v, "-", k, "- DR"), "J3"))
    }
  }
  write.csv(bind_rows(res), file.path(DIR_TAB, "81_heterogeneidad_gobernanza.csv"), row.names = FALSE)
}
cat("\nListo. Salidas en outputs/tablas/81_*.csv\n")
