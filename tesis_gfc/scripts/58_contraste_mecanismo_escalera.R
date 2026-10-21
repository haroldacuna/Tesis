## =============================================================================
## 58_contraste_mecanismo_escalera.R
##
## Tarea A.1 -- Contraste del mecanismo (OE3): ¿el efecto viene de proyectos
## que operaron o de municipios cuyos proyectos solo se inscribieron?
##
## Escalera (proyectos REDD+ de Verra, sin exclusiones D2):
##   S1  cualquier aparicion en el registro
##   S2  registro efectivo  (Registered, Inactive, Late to verify,
##                           Verification approval requested)
##   S3  estado actual "Registered"
##   E   unitsIssued > 0
## Nivel del municipio = mejor estado entre sus proyectos REDD+ (principal);
## sensibilidad: estado del proyecto que define el anio de adopcion.
##
## Interpretacion fijada ANTES de correr (D13):
##   - Grupo informativo: solo_S1 (nunca operaron). Un efecto ahi no tiene
##     mecanismo -> seleccion en la etapa de solicitud.
##   - E depende de reducciones verificadas, es decir, en parte del outcome:
##     un efecto en E es parcialmente mecanico y no prueba el mecanismo.
##   - Si los grupos no alcanzan 3 municipios por cohorte en 2013-2019, se
##     reporta descriptivo y se declara sin potencia.
##   - El estado es el ACTUAL (2026), no el del anio de adopcion: es una
##     variable posterior al tratamiento. Se documenta como limitacion.
##
## Insumos:
##   outputs/40_muestra_A.rds    (muestra principal exacta del 40)
##   data/interim/verra_platts_colombia_con_municipio.csv
##   scripts/filtro_sectorial.py (ids de EXCLUSIONES; se leen como texto)
## Salidas:
##   outputs/tablas/58_contraste_mecanismo.csv
##   outputs/tablas/58_composicion_por_cohorte.csv
## =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(did)
})
source("scripts/00_config_did.R")   # B.1: parametros comunes (no redefinir aqui)

## --- Raiz canonica -----------------------------------------------------------
RAIZ <- local({
  d <- normalizePath(getwd(), winslash = "/")
  repeat {
    if (file.exists(file.path(d, "data/.raiz_canonica"))) break
    p <- dirname(d); if (p == d) stop("No encuentro data/.raiz_canonica: corre desde tesis_gfc/"); d <- p
  }
  d
})
DIR_OUT <- file.path(RAIZ, "outputs"); DIR_TAB <- file.path(DIR_OUT, "tablas")
dir.create(DIR_TAB, showWarnings = FALSE, recursive = TRUE)

## --- Parametros (iguales al 40) ---------------------------------------------
MIN_COH   <- 3

METODOLOGIAS_REDD <- "VM0006|VM0007|VM0009|VM0015"
## Patrones de estado (minusculas). Un estado que no case con ninguno DETIENE
## el script: el mapeo debe ser una decision explicita, no un default.
PAT_S2 <- "^registered$|^inactive|late to verify|^verification approval requested"
PAT_S1 <- "^under validation|^under development|^registration requested|registration and verification approval requested|^on hold|withdrawn|rejected|^pipeline listing"

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
titulo <- function(t) cat("\n", strrep("=", 76), "\n", t, "\n", strrep("=", 76), "\n", sep = "")
num    <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.eE-]", "", as.character(x))))

## =============================================================================
## 1. Muestra principal (exacta del 40)
## =============================================================================
ruta_m <- file.path(DIR_OUT, "40_muestra_A.rds")
if (!file.exists(ruta_m)) stop("Falta ", ruta_m, ": agrega el saveRDS(muestra_A, ...) al 40 y correlo.")
muestra <- readRDS(ruta_m)
muestra$COD_DANE <- pad5(muestra$COD_DANE)
faltan <- setdiff(c(OUTCOME, "year", "id_num", "G_dep", all.vars(COVARIABLES_XFORMLA)), names(muestra))
if (length(faltan)) stop("La muestra del 40 no trae: ", paste(faltan, collapse = ", "))

## Controles limpios con la misma funcion comun que el 40 (D3). Es idempotente:
## si la muestra ya viene limpia no saca nada.
source(file.path(RAIZ, "scripts/comun_controles_limpios.R"))
cont <- municipios_con_proyecto(file.path(RAIZ, "data/final/panel_con_tratamiento_actualizado.csv"))
muestra <- aplicar_controles_limpios(muestra, cont, col_g = "G_dep")

mun <- muestra %>% distinct(COD_DANE, G_dep)
cat(sprintf("Muestra para el contraste: %d municipios | %d tratados | %d controles\n",
            nrow(mun), sum(mun$G_dep > 0), sum(mun$G_dep == 0)))
if (sum(mun$G_dep == 0) != REF_N_CONTROLES || sum(mun$G_dep > 0) != REF_N_TRATADOS)
  stop("La muestra no coincide con la principal del 40 (36 tratados, 986 controles). Revisa antes de seguir.")

## =============================================================================
## 2. Escalera por proyecto (Verra, REDD+, sin exclusiones D2)
## =============================================================================

titulo("2. ESCALERA DE ESTADO POR PROYECTO")
verra <- read_csv(file.path(RAIZ, "data/interim/verra_platts_colombia_con_municipio.csv"),
                  col_types = cols(.default = col_character()), show_col_types = FALSE)

## IDs D2 leidos de filtro_sectorial.py (unica fuente). Solo los ids de Platts
## (15 digitos) sirven para este archivo.
lin_fs <- readLines(file.path(RAIZ, "scripts/filtro_sectorial.py"), warn = FALSE)
ids_d2 <- gsub("\"", "", unlist(regmatches(lin_fs, gregexpr("\"1102[0-9]{11}\"", lin_fs))))
cat("IDs D2 (Platts) leidos de filtro_sectorial.py:", paste(ids_d2, collapse = ", "), "\n")
if (length(ids_d2) < 3) stop("Esperaba al menos 3 ids D2 de Platts en filtro_sectorial.py")

## Creditos emitidos (nivel E). En el archivo de Platts, unitsIssued viene
## vacio para todos los REDD+: se toman de Berkeley VROD, unidos por el ID de
## la VCS (projectId en Platts, "VCSnnnn" en Berkeley).
col_vcs <- intersect(c("projectId", "resourceIdentifier"), names(verra))[1]
if (is.na(col_vcs)) stop("El archivo de Platts no trae projectId: no se puede unir con Berkeley.")

ruta_b <- file.path(RAIZ, "data/interim/berkeley_vrod_projects_colombia.csv")
if (!file.exists(ruta_b)) stop("Falta ", ruta_b)
berk <- read_csv(ruta_b, col_types = cols(.default = col_character()), show_col_types = FALSE)
col_bid <- grep("project.?id", names(berk), ignore.case = TRUE, value = TRUE)[1]
col_iss <- c(grep("total.*issued", names(berk), ignore.case = TRUE, value = TRUE),
             grep("issued", names(berk), ignore.case = TRUE, value = TRUE))[1]
if (is.na(col_bid) || is.na(col_iss))
  stop("No identifico ID o emisiones en Berkeley. Columnas: ", paste(names(berk), collapse = " | "))
cat("Berkeley: ID =", col_bid, "| emisiones =", col_iss, "\n")

berk_e <- berk %>%
  filter(grepl("^VCS", .data[[col_bid]], ignore.case = TRUE)) %>%
  transmute(vcs_id = gsub("[^0-9]", "", .data[[col_bid]]),
            emitidos_b = num(.data[[col_iss]])) %>%
  group_by(vcs_id) %>%
  summarise(emitidos_b = suppressWarnings(max(emitidos_b, na.rm = TRUE)), .groups = "drop") %>%
  mutate(emitidos_b = ifelse(is.finite(emitidos_b), emitidos_b, NA_real_))

proy <- verra %>%
  mutate(id = sub("\\.0$", "", trimws(id)),
         vcs_id = sub("\\.0$", "", trimws(.data[[col_vcs]])),
         COD_DANE = pad5(gsub("[^0-9]", "", COD_DANE)),
         es_redd = grepl(METODOLOGIAS_REDD, toupper(methodologies)) | grepl("REDD", toupper(afoluNames)),
         estado = tolower(trimws(status)),
         anio_inicio = as.integer(substr(creditPeriodStartDate, 1, 4))) %>%
  filter(es_redd, !is.na(COD_DANE), !id %in% ids_d2) %>%
  distinct(id, .keep_all = TRUE) %>%
  left_join(berk_e, by = "vcs_id") %>%
  mutate(emitidos = coalesce(emitidos_b, 0))

cat(sprintf("REDD+ con match en Berkeley: %d de %d | con emision > 0: %d | total emitido: %s\n",
            sum(!is.na(proy$emitidos_b)), nrow(proy), sum(proy$emitidos > 0),
            format(sum(proy$emitidos), big.mark = ".", decimal.mark = ",")))
if (sum(!is.na(proy$emitidos_b)) < 0.8 * nrow(proy))
  cat("[!] Menos del 80% de los REDD+ encuentra su par en Berkeley: revisa la union antes de leer E.\n")
sin_mapa <- setdiff(unique(proy$estado[!grepl(PAT_S2, proy$estado) & !grepl(PAT_S1, proy$estado)]), NA)
if (length(sin_mapa)) stop("Estados sin mapear (agregalos a PAT_S1 o PAT_S2): ", paste(sin_mapa, collapse = " | "))

proy <- proy %>% mutate(s2 = grepl(PAT_S2, estado), s3 = estado == "registered", e = emitidos > 0)
cat(sprintf("Proyectos REDD+ de Verra (sin D2): %d | S2: %d | E: %d | S3: %d\n",
            nrow(proy), sum(proy$s2), sum(proy$e), sum(proy$s3)))
cat("Estados observados:\n"); print(table(proy$estado))
if (any(proy$e & !proy$s2)) {
  cat("[!] Proyectos con creditos emitidos fuera de S2 (revisar mapeo):\n")
  print(proy %>% filter(e, !s2) %>% select(id, projectName, estado, emitidos))
}

## =============================================================================
## 3. Nivel del municipio
## =============================================================================
titulo("3. NIVEL DEL MUNICIPIO")
nivel_max <- proy %>% group_by(COD_DANE) %>%
  summarise(s2 = any(s2), e = any(e), s3 = any(s3), n_proy = n(), .groups = "drop")

## Sensibilidad: estado del proyecto mas temprano del municipio
nivel_primero <- proy %>% group_by(COD_DANE) %>% slice_min(anio_inicio, n = 1, with_ties = TRUE) %>%
  summarise(s2_p = any(s2), e_p = any(e), .groups = "drop")

trat <- mun %>% filter(G_dep > 0) %>%
  left_join(nivel_max, by = "COD_DANE") %>% left_join(nivel_primero, by = "COD_DANE") %>%
  mutate(grupo = case_when(is.na(s2) ~ "sin_estado",
                           !s2       ~ "solo_S1",
                           TRUE      ~ "S2"))
cat("Tratados por grupo (nivel = mejor estado):\n"); print(table(trat$grupo))
cat("\nSin estado de Verra (tratados solo por RENARE/Cercarbono): no entran al contraste.\n")

comp <- trat %>% group_by(G_dep) %>%
  summarise(n = n(), solo_S1 = sum(grupo == "solo_S1"), S2 = sum(grupo == "S2"),
            E = sum(e %in% TRUE), S3 = sum(s3 %in% TRUE), sin_estado = sum(grupo == "sin_estado"),
            .groups = "drop")
cat("\nComposicion por cohorte:\n"); print(as.data.frame(comp))
write_csv(comp, file.path(DIR_TAB, "58_composicion_por_cohorte.csv"))

cat("\nCohorte 2018 (la que concentra el efecto), municipio por municipio:\n")
print(as.data.frame(trat %>% filter(G_dep == 2018) %>% select(COD_DANE, grupo, e, s3, n_proy)))

## =============================================================================
## 4. Estimacion por grupo
## CAMBIO 18/10/2026 (escenario A, comentario de Jorge): el grupo de comparacion
## principal es el MISMO de la especificacion principal (CONTROL_PRINCIPAL =
## "notyettreated"): nunca tratados limpios + municipios del mismo grupo que aun
## no adoptan en t. Los tratados de OTROS grupos salen de la submuestra, porque
## ya estan tratados y no son contrafactual de nadie. "nevertreated" se reporta
## como sensibilidad (columna control = nevertreated en el CSV).
## =============================================================================
correr <- function(cods_trat, xformla, etiqueta, control = CONTROL_PRINCIPAL) {
  d <- muestra %>% filter(G_dep == 0 | COD_DANE %in% cods_trat)
  coh <- d %>% filter(G_dep > 0) %>% distinct(COD_DANE, G_dep) %>% count(G_dep)
  n_ok <- sum(coh$n >= MIN_COH)
  cat(sprintf("\n-- %s: %d tratados en %d cohortes (%d con >= %d municipios)\n",
              etiqueta, sum(coh$n), nrow(coh), n_ok, MIN_COH))
  if (sum(coh$n) < MIN_COH) { cat("   sin potencia: menos de", MIN_COH, "tratados\n"); return(NULL) }
  d <- d %>% rename(.gname = G_dep)
  set.seed(SEMILLA)
  out <- tryCatch(suppressWarnings(att_gt(
    yname = OUTCOME, tname = "year", idname = "id_num", gname = ".gname", xformla = xformla,
    data = d, control_group = control, est_method = if (identical(xformla, ~1)) "reg" else "dr",
    bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "id_num")),
    error = function(e) { cat("   ERROR att_gt:", conditionMessage(e), "\n"); NULL })
  if (is.null(out)) return(NULL)
  s <- tryCatch(aggte(out, type = "simple", na.rm = TRUE), error = function(e) NULL)
  b <- tryCatch(aggte(out, type = "dynamic", balance_e = BALANCE_E, min_e = 0, max_e = BALANCE_E,
                      na.rm = TRUE), error = function(e) NULL)
  if (is.null(s)) return(NULL)
  cat(sprintf("   ATT simple: %.2f (EE %.2f) IC95 [%.2f, %.2f] | NA en att_gt: %d de %d\n",
              s$overall.att, s$overall.se, s$overall.att - 1.96 * s$overall.se,
              s$overall.att + 1.96 * s$overall.se, sum(is.na(out$att)), length(out$att)))
  data.frame(especificacion = etiqueta, control = control, n_tratados = sum(coh$n), cohortes_ok = n_ok,
             att = s$overall.att, ee = s$overall.se,
             att_bal = if (!is.null(b)) b$overall.att else NA_real_,
             ee_bal  = if (!is.null(b)) b$overall.se  else NA_real_)
}

titulo("4. ATT POR NIVEL DE LA ESCALERA (sin covariables = principal del contraste)")
g <- split(trat$COD_DANE, trat$grupo)
res_nt <- list(
  correr(trat$COD_DANE[trat$grupo != "sin_estado"], ~1, "Con estado Verra (S1 total)"),
  correr(g$solo_S1, ~1, "solo_S1 (nunca operaron)"),
  correr(g$S2,      ~1, "S2 (registro efectivo)"),
  correr(trat$COD_DANE[trat$e %in% TRUE],  ~1, "E (creditos emitidos)"),
  correr(trat$COD_DANE[trat$s3 %in% TRUE], ~1, "S3 (Registered hoy)"),
  correr(trat$COD_DANE[trat$grupo != "sin_estado" & !(trat$s2_p %in% TRUE)], ~1,
         "Sensibilidad: proyecto inicial solo_S1"),
  correr(trat$COD_DANE[trat$s2_p %in% TRUE], ~1, "Sensibilidad: proyecto inicial S2"),
  correr(g$solo_S1, COVARIABLES_XFORMLA, "solo_S1 - DR"),
  correr(g$S2,      COVARIABLES_XFORMLA, "S2 - DR")
)
## Sensibilidad: solo nunca tratados (la version de resultados-v1)
titulo(sprintf("4b. SENSIBILIDAD: grupo de comparacion = %s", CONTROL_CONTRASTE))
res_nev <- list(
  correr(g$solo_S1, ~1, "solo_S1 (nunca operaron)", CONTROL_CONTRASTE),
  correr(g$S2,      ~1, "S2 (registro efectivo)",   CONTROL_CONTRASTE),
  correr(trat$COD_DANE[trat$e %in% TRUE],  ~1, "E (creditos emitidos)", CONTROL_CONTRASTE),
  correr(trat$COD_DANE[trat$s3 %in% TRUE], ~1, "S3 (Registered hoy)",   CONTROL_CONTRASTE)
)
tab <- do.call(rbind, Filter(Negate(is.null), c(res_nt, res_nev)))

## Diferencia solo_S1 - S2. Comparten controles, asi que la covarianza es
## positiva y la EE que la ignora es CONSERVADORA (sobrestima la varianza).
i1 <- which(tab$especificacion == "solo_S1 (nunca operaron)" & tab$control == CONTROL_PRINCIPAL)
i2 <- which(tab$especificacion == "S2 (registro efectivo)" & tab$control == CONTROL_PRINCIPAL)
if (length(i1) && length(i2)) {
  dif <- tab$att[i1] - tab$att[i2]; ee_d <- sqrt(tab$ee[i1]^2 + tab$ee[i2]^2)
  cat(sprintf("\nDiferencia solo_S1 - S2: %.2f (EE conservadora %.2f) IC95 [%.2f, %.2f]\n",
              dif, ee_d, dif - 1.96 * ee_d, dif + 1.96 * ee_d))
}

titulo("RESUMEN")
print(tab, row.names = FALSE, digits = 4)
write_csv(tab, file.path(DIR_TAB, "58_contraste_mecanismo.csv"))
cat("\nGuardado: outputs/tablas/58_contraste_mecanismo.csv y 58_composicion_por_cohorte.csv\n")