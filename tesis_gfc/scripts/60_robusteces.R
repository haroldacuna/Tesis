## =============================================================================
## 60_robusteces.R
##
## Fase A, robusteces sobre la especificacion principal (D1 depurada, DR,
## notyettreated, cohortes 2013-2019, controles limpios: 36 tratados, 986
## controles). Cada bloque se reporta DR y sin covariables.
##
##   A.5  Inferencia agrupada por departamento: bootstrap multiplicador con
##        clustervars = c(id, departamento) (pesos de Rademacher, equivalente
##        al wild cluster bootstrap). El punto debe ser identico al del 40.
##   A.6  Placebo AFOLU no-REDD+: municipios con proyectos forestales que no
##        son REDD+ (anio_inicio_tratamiento_todas_fuentes > 0 y sin REDD+).
##        Regla D13: un efecto de magnitud similar a la principal indica
##        seleccion de municipios con proyectos de carbono, no mecanismo REDD+.
##   A.7  Tratamiento por cobertura de poligonos (Karnik et al.): tratado si
##        pct_ad >= 5 %; adopcion = min(anio actual, cohorte del poligono).
##        Agrega proyectos de EcoRegistry y reparte proyectos multimunicipio.
##   A.8  Definiciones alternativas: alta_redd y sinmanual, mismos controles.
##   A.9  Vision Amazonia (si RUTA_VA esta definida): (i) excluir municipios
##        con >= 5 % de su area en el poligono; (ii) solo esos municipios.
##
## Insumos: outputs/40_muestra_A.rds, data/final/panel_con_tratamiento_actualizado.csv,
##          data/interim/proyectos_poligonos/cobertura_municipal.csv,
##          data/interim/municipios_clean.gpkg (solo A.9), poligono de VA (A.9)
## Salida:  outputs/tablas/60_robusteces.csv
## =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(readr); library(did) })

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

## --- Parametros --------------------------------------------------------------
OUTCOME <- "loss_area_ha"; VENTANA <- 2013:2019
SEMILLA <- 20260824; BITERS <- 999
REF_40_DR <- -213.73
UMBRAL_PCT <- 5                       # % del municipio cubierto (igual que P1 del 53)
RUTA_VA <- NA_character_              # A.9: ruta al poligono de Vision Amazonia (preferido)
DPTOS_VA_PROXY <- c(18, 86, 91, 94, 95, 97)   # aproximacion si no hay poligono: Caqueta,
                                              # Putumayo, Amazonas, Guainia, Guaviare, Vaupes          # A.9: ruta al poligono de Vision Amazonia (completar)
FILTRO_VA <- "amazon"                 # texto para elegir el poligono si el archivo trae varios
COVARIABLES_XFORMLA <- ~ log_baseline_forest + temp_media_c_base +
  prec_anual_mm_base + discapital_base + disbogota_base + distancia_mercado_base +
  H_coca_base + homicidios_base + log_defor_pre_media + defor_pre_tendencia

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(t) cat("\n", strrep("=", 76), "\n", t, "\n", strrep("=", 76), "\n", sep = "")

## =============================================================================
## 1. Muestra principal y tratamientos alternativos
## =============================================================================
muestra <- readRDS(file.path(DIR_OUT, "40_muestra_A.rds"))
muestra$COD_DANE <- pad5(muestra$COD_DANE)
source(file.path(RAIZ, "scripts/comun_controles_limpios.R"))
cont <- municipios_con_proyecto(file.path(RAIZ, "data/final/panel_con_tratamiento_actualizado.csv"))
muestra <- aplicar_controles_limpios(muestra, cont, col_g = "G_dep")
muestra$dpto <- as.integer(substr(muestra$COD_DANE, 1, 2))

mun <- muestra %>% distinct(COD_DANE, G_dep)
cat(sprintf("Muestra principal: %d tratados | %d controles\n", sum(mun$G_dep > 0), sum(mun$G_dep == 0)))
if (sum(mun$G_dep == 0) != 986 || sum(mun$G_dep > 0) != 36) stop("La muestra no coincide con la principal.")
CONTROLES <- mun$COD_DANE[mun$G_dep == 0]

## Panel completo (todos los municipios) para placebo y definiciones alternativas:
## las filas de municipios que no estan en la muestra principal se toman del rds
## del panel de analisis, con las mismas covariables corregidas.
panel_full <- readRDS(file.path(DIR_OUT, "panel_analisis_did.rds"))
panel_full$COD_DANE <- pad5(panel_full$COD_DANE)
cov_cols <- all.vars(COVARIABLES_XFORMLA)
cov_mun <- muestra %>% distinct(COD_DANE, across(all_of(cov_cols)))
ruta_cov <- file.path(DIR_TAB, "covariables_pre_municipio.csv")
if (file.exists(ruta_cov)) {     # covariables corregidas (02/47) para municipios fuera de la muestra
  cp <- read_csv(ruta_cov, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE) %>%
    mutate(COD_DANE = pad5(COD_DANE))
  cols_pre <- intersect(c("log_defor_pre_media", "defor_pre_tendencia", "log_baseline_forest"), names(cp))
  panel_full <- panel_full %>% select(-any_of(cols_pre)) %>% left_join(cp[, c("COD_DANE", cols_pre)], by = "COD_DANE")
}
faltan <- setdiff(c(OUTCOME, "year", "id_num", cov_cols), names(panel_full))
if (length(faltan)) stop("panel_analisis_did.rds no trae: ", paste(faltan, collapse = ", "))
panel_full$dpto <- as.integer(substr(panel_full$COD_DANE, 1, 2))

trat <- read_csv(file.path(RAIZ, "data/final/panel_con_tratamiento_actualizado.csv"),
                 col_types = cols(COD_DANE = col_character(), .default = col_double()),
                 col_select = c(COD_DANE, starts_with("anio_inicio_tratamiento_"), muestra_redd_depurada),
                 show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>% group_by(COD_DANE) %>%
  summarise(G_afolu = min_na(anio_inicio_tratamiento_todas_fuentes),
            G_redd  = min_na(anio_inicio_tratamiento_todas_fuentes_redd),
            G_alta  = min_na(anio_inicio_tratamiento_alta_confianza_redd),
            G_sinm  = min_na(anio_inicio_tratamiento_todas_fuentes_redd_sinmanual),
            G_dep2  = min_na(anio_inicio_tratamiento_todas_fuentes_redd_depurada),
            muestra_dep = min(muestra_redd_depurada), .groups = "drop") %>%
  mutate(across(starts_with("G_"), ~ coalesce(.x, 0)))

## Construye una muestra: tratados = cods con su G (dentro de la ventana) + los
## 986 controles limpios de la principal. Todo lo demas sale.
armar <- function(g_tab, etiqueta) {
  g_tab <- g_tab %>% filter(G > 0, G %in% VENTANA, !COD_DANE %in% CONTROLES)
  ctl <- tibble(COD_DANE = CONTROLES, G = 0)
  ids <- bind_rows(g_tab, ctl)
  d <- panel_full %>% filter(COD_DANE %in% ids$COD_DANE) %>%
    select(-any_of(c("G", "G_dep"))) %>% inner_join(ids, by = "COD_DANE")
  coh <- g_tab %>% count(G)
  cat(sprintf("\n[%s] %d tratados en %d cohortes (%d con >= 3) | %d controles\n", etiqueta,
              nrow(g_tab), nrow(coh), sum(coh$n >= 3), length(CONTROLES)))
  d
}

## =============================================================================
## 2. Estimacion
## =============================================================================
tab <- list()
correr <- function(d, xf, etiqueta, bloque, clus = "id_num") {
  set.seed(SEMILLA)
  out <- tryCatch(suppressWarnings(att_gt(
    yname = OUTCOME, tname = "year", idname = "id_num", gname = "G", xformla = xf,
    data = d %>% mutate(G = as.integer(G), id_num = as.integer(id_num)),
    control_group = "notyettreated", est_method = "dr",
    bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = clus)),
    error = function(e) { cat("   ERROR [", etiqueta, "]:", conditionMessage(e), "\n"); NULL })
  if (is.null(out)) return(invisible(NULL))
  s <- tryCatch(aggte(out, type = "simple", na.rm = TRUE), error = function(e) NULL)
  if (is.null(s)) return(invisible(NULL))
  cat(sprintf("   %-48s ATT %8.2f (EE %7.2f) IC [%8.2f; %8.2f] | NA %d/%d\n", etiqueta,
              s$overall.att, s$overall.se, s$overall.att - 1.96 * s$overall.se,
              s$overall.att + 1.96 * s$overall.se, sum(is.na(out$att)), length(out$att)))
  tab[[length(tab) + 1]] <<- data.frame(bloque = bloque, especificacion = etiqueta,
                                        n_tratados = n_distinct(d$COD_DANE[d$G > 0]), att = s$overall.att, ee = s$overall.se,
                                        ic_inf = s$overall.att - 1.96 * s$overall.se, ic_sup = s$overall.att + 1.96 * s$overall.se,
                                        gt_na = sum(is.na(out$att)), gt_total = length(out$att))
  invisible(s)
}
dos_specs <- function(d, base, bloque, ...) {
  correr(d, COVARIABLES_XFORMLA, paste(base, "- DR"), bloque, ...)
  correr(d, ~1, paste(base, "- sin covariables"), bloque, ...)
}

## --- A.5 --------------------------------------------------------------------
titulo("A.5 INFERENCIA AGRUPADA POR DEPARTAMENTO")
d0 <- muestra %>% mutate(G = G_dep)
cat(sprintf("Departamentos: %d | con al menos un tratado: %d\n",
            n_distinct(d0$dpto), n_distinct(d0$dpto[d0$G > 0])))
s_ref <- correr(d0, COVARIABLES_XFORMLA, "Principal, cluster municipio", "A.5")
if (!is.null(s_ref) && abs(s_ref$overall.att - REF_40_DR) > 0.5)
  stop("La principal no reproduce el 40: revisa la muestra.")
correr(d0, COVARIABLES_XFORMLA, "Principal, cluster departamento", "A.5", clus = c("id_num", "dpto"))
correr(d0, ~1, "Sin covariables, cluster departamento", "A.5", clus = c("id_num", "dpto"))

## --- A.6 --------------------------------------------------------------------
titulo("A.6 PLACEBO: AFOLU NO-REDD+")
plac <- trat %>% filter(G_afolu > 0, G_redd == 0) %>% transmute(COD_DANE, G = G_afolu)
cat("Municipios AFOLU no-REDD+ (todas las cohortes):", nrow(plac), "\n")
dos_specs(armar(plac, "Placebo AFOLU no-REDD+"), "Placebo AFOLU no-REDD+", "A.6")

## --- A.7 --------------------------------------------------------------------
titulo(sprintf("A.7 TRATAMIENTO POR COBERTURA DE POLIGONOS (>= %d %%)", UMBRAL_PCT))
cob <- read_csv(file.path(RAIZ, "data/interim/proyectos_poligonos/cobertura_municipal.csv"),
                col_types = cols(COD_DANE = col_character(), .default = col_guess()), show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE), pct_ad = as.numeric(pct_ad), cohorte_poligono = as.numeric(cohorte_poligono))
pol <- cob %>% filter(pct_ad >= UMBRAL_PCT, !is.na(cohorte_poligono)) %>% select(COD_DANE, cohorte_poligono)
base7 <- mun %>% filter(G_dep > 0) %>% transmute(COD_DANE, G = G_dep)
a7 <- full_join(base7, pol, by = "COD_DANE") %>%
  mutate(G = pmin(coalesce(G, Inf), coalesce(cohorte_poligono, Inf)), G = ifelse(is.finite(G), G, 0)) %>%
  select(COD_DANE, G)
nuevos <- setdiff(a7$COD_DANE[a7$G %in% VENTANA], base7$COD_DANE)
cat(sprintf("Municipios cubiertos >= %d %%: %d | nuevos tratados en la ventana: %d (%d eran controles limpios)\n",
            UMBRAL_PCT, nrow(pol), length(nuevos), sum(nuevos %in% CONTROLES)))
cambian <- inner_join(base7, a7, by = "COD_DANE", suffix = c("_d1", "_a7")) %>% filter(G_d1 != G_a7)
if (nrow(cambian)) { cat("Tratados de D1 que adelantan su cohorte por el poligono:\n"); print(as.data.frame(cambian)) }
## Aqui los controles que pasan a tratados NO deben seguir en CONTROLES
CONTROLES_BAK <- CONTROLES; CONTROLES <- setdiff(CONTROLES, a7$COD_DANE[a7$G > 0])
d7 <- armar(a7 %>% mutate(G = ifelse(COD_DANE %in% CONTROLES_BAK & !(G %in% VENTANA), 0, G)), "Cobertura de poligonos")
dos_specs(d7, sprintf("Cobertura de poligonos >= %d %%", UMBRAL_PCT), "A.7")
CONTROLES <- CONTROLES_BAK

## --- A.8 --------------------------------------------------------------------
titulo("A.8 DEFINICIONES ALTERNATIVAS (muestra depurada)")
dep_ok <- trat$COD_DANE[trat$muestra_dep == 1]
for (def in c("G_alta", "G_sinm")) {
  g <- trat %>% filter(COD_DANE %in% dep_ok) %>% transmute(COD_DANE, G = .data[[def]])
  nom <- if (def == "G_alta") "alta_redd (solo Verra)" else "todas_redd_sinmanual"
  dos_specs(armar(g, nom), nom, "A.8")
}

## --- A.9 --------------------------------------------------------------------
titulo("A.9 VISION AMAZONIA")
if (is.na(RUTA_VA) || !file.exists(RUTA_VA)) {
  cat("Sin poligono de Vision Amazonia: se usa la APROXIMACION por departamentos",
      paste(DPTOS_VA_PROXY, collapse = ", "), "(no incluye el sur del Meta).\n")
  en_va <- unique(muestra$COD_DANE[muestra$dpto %in% DPTOS_VA_PROXY])
  cat(sprintf("Municipios en la aproximacion: %d (tratados de la muestra: %d)\n",
              length(en_va), sum(mun$COD_DANE[mun$G_dep > 0] %in% en_va)))
  dos_specs(muestra %>% filter(!COD_DANE %in% en_va) %>% mutate(G = G_dep),
            "Fuera de VA (aprox. departamentos)", "A.9")
  dos_specs(muestra %>% filter(COD_DANE %in% en_va) %>% mutate(G = G_dep),
            "Solo VA (aprox. departamentos)", "A.9")
} else {
  suppressPackageStartupMessages(library(sf))
  va <- st_read(RUTA_VA, quiet = TRUE)
  txt <- apply(st_drop_geometry(va), 1, paste, collapse = " ")
  if (nrow(va) > 1) va <- va[grepl(FILTRO_VA, txt, ignore.case = TRUE), ]
  if (!nrow(va)) stop("No encontre el poligono de Vision Amazonia con el filtro '", FILTRO_VA, "'.")
  va <- st_union(st_make_valid(st_transform(va, 9377)))
  mpios <- st_read(file.path(RAIZ, "data/interim/municipios_clean.gpkg"), quiet = TRUE) %>% st_transform(9377)
  col_cod <- intersect(c("COD_DANE", "MPIO_CDPMP", "CODIGO"), names(mpios))[1]
  mpios$COD_DANE <- pad5(mpios[[col_cod]]); mpios$a_mun <- as.numeric(st_area(mpios))
  inter <- suppressWarnings(st_intersection(st_make_valid(mpios[, c("COD_DANE", "a_mun")]), va))
  inter$pct <- 100 * as.numeric(st_area(inter)) / inter$a_mun
  en_va <- inter$COD_DANE[inter$pct >= UMBRAL_PCT]
  cat(sprintf("Municipios con >= %d %% en Vision Amazonia: %d (tratados de la muestra: %d)\n",
              UMBRAL_PCT, length(en_va), sum(mun$COD_DANE[mun$G_dep > 0] %in% en_va)))
  dos_specs(muestra %>% filter(!COD_DANE %in% en_va) %>% mutate(G = G_dep),
            "Fuera de Vision Amazonia", "A.9")
  dos_specs(muestra %>% filter(COD_DANE %in% en_va) %>% mutate(G = G_dep),
            "Solo Vision Amazonia", "A.9")
}

## =============================================================================
titulo("RESUMEN")
res <- do.call(rbind, tab)
print(res %>% mutate(across(where(is.numeric), ~ round(.x, 2))), row.names = FALSE)
write_csv(res, file.path(DIR_TAB, "60_robusteces.csv"))
cat("\nGuardado: outputs/tablas/60_robusteces.csv\n")