## =============================================================================
## 45_spillovers_depurada.R
##
## Desplazamiento espacial (leakage) con la D1 DEPURADA. Reemplaza, para la
## especificacion principal, a 10_spillovers_espaciales.R, que usaba las
## definiciones de tratamiento anteriores (first_treat_alta / _todas).
##
## Pregunta: la deforestacion se desplaza hacia municipios VECINOS de uno
## tratado (fuga), o baja tambien en ellos (efecto protector indirecto)?
##
##   A) DiD espacial (Callaway-Sant'Anna). Muestra: municipios NUNCA tratados
##      directamente. Cohorte de exposicion G_vec = primer anio en que algun
##      vecino (contiguidad tipo reina) adopta un proyecto REDD+.
##        ATT > 0 y significativo -> fuga
##        ATT < 0 y significativo -> efecto protector indirecto
##      Se corre sin covariables y doblemente robusto con las mismas diez
##      covariables de la principal, para que sea comparable con el efecto
##      directo.
##
##   B) TWFE con la proporcion de vecinos tratados. Se mantiene solo como
##      contraste: no es coherente descartar TWFE para el efecto directo y
##      apoyarse en el para afirmar fuga.
##
## Cambios respecto de 10:
##   - Tratamiento leido de panel_con_tratamiento_actualizado.csv (salida del
##     26), no de first_treat_* del .rds, que puede estar desactualizado.
##   - COD_DANE normalizado a 5 digitos en el shapefile Y en el panel. En 10 se
##     cruzaban sin rellenar ceros: si una fuente guardaba "5001" y la otra
##     "05001", los municipios con codigo 05-09 no encontraban a sus vecinos.
##   - EXPOSICION AMBIGUA: un municipio cuyo unico vecino "tratado" es uno de
##     los seis con proyecto retirado o rechazado (muestra_redd_depurada = 0)
##     sale de la muestra. No pasa a control, por la misma razon que esos seis
##     no pasan a control.
##   - Ventana de cohortes 2013-2019 tambien para la exposicion.
##   - Rutas ancladas a la raiz canonica.
##
## USO:  Rscript scripts/45_spillovers_depurada.R
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(sf)
  library(spdep)
  library(did)
  library(fixest)
  library(ggplot2)
})

RAIZ <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "data/.raiz_canonica"))) {
    p <- dirname(d); if (identical(p, d)) stop("Sin data/.raiz_canonica"); d <- p
  }
  d
})
DIR_OUT <- file.path(RAIZ, "outputs")
DIR_TAB <- file.path(DIR_OUT, "tablas")
DIR_FIG <- file.path(DIR_OUT, "figuras")
for (x in c(DIR_TAB, DIR_FIG)) dir.create(x, showWarnings = FALSE, recursive = TRUE)

SEMILLA   <- 20260824
OUTCOME   <- "loss_area_ha"
VENTANA   <- 2013:2019
BALANCE_E <- 5
BITERS    <- 999

COVARIABLES_XFORMLA <- ~ log_baseline_forest + temp_media_c_base +
  prec_anual_mm_base + discapital_base + disbogota_base + distancia_mercado_base +
  H_coca_base + homicidios_base + log_defor_pre_media + defor_pre_tendencia

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(t) cat("\n", strrep("=", 76), "\n", t, "\n", strrep("=", 76), "\n", sep = "")

## =============================================================================
## 1. Vecindad
## =============================================================================
titulo("1. VECINDAD TIPO REINA")
mun <- st_read(file.path(RAIZ, "data/interim/municipios_clean.gpkg"),
               layer = "municipios_clean", quiet = TRUE)
mun$COD_DANE <- pad5(mun$COD_DANE)
n_inv <- sum(!st_is_valid(mun))
if (n_inv > 0) { cat("Reparando", n_inv, "geometrias invalidas\n"); mun <- st_make_valid(mun) }

vec <- poly2nb(mun, queen = TRUE)
cat(sprintf("Municipios: %d | vecinos promedio: %.1f | sin vecinos: %d\n",
            nrow(mun), mean(card(vec)), sum(card(vec) == 0)))

aristas <- data.frame(
  desde = mun$COD_DANE[rep(seq_along(vec), card(vec))],
  hacia = mun$COD_DANE[unlist(vec[card(vec) > 0])]
)

## =============================================================================
## 2. Panel y tratamiento
## =============================================================================
panel <- readRDS(file.path(DIR_OUT, "panel_analisis_did.rds"))
panel$COD_DANE <- pad5(panel$COD_DANE)

COLS_PRE <- c("log_defor_pre_media", "defor_pre_tendencia", "log_baseline_forest")
if (!all(COLS_PRE %in% names(panel))) {
  cov_pre <- read_csv(file.path(DIR_TAB, "covariables_pre_municipio.csv"),
                      col_types = cols(COD_DANE = col_character()), show_col_types = FALSE)
  cov_pre$COD_DANE <- pad5(cov_pre$COD_DANE)
  cols_unir <- intersect(c("COD_DANE", COLS_PRE), names(cov_pre))
  panel <- panel %>% select(-any_of(setdiff(cols_unir, "COD_DANE"))) %>%
    left_join(cov_pre[, cols_unir], by = "COD_DANE")
}

trat <- read_csv(file.path(RAIZ, "data/final/panel_con_tratamiento_actualizado.csv"),
                 col_select = c(COD_DANE, anio_inicio_tratamiento_todas_fuentes_redd_depurada,
                                muestra_redd_depurada),
                 col_types = cols(COD_DANE = col_character(), .default = col_double()),
                 show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>%
  group_by(COD_DANE) %>%
  summarise(G_dep = coalesce(min_na(anio_inicio_tratamiento_todas_fuentes_redd_depurada), 0),
            muestra_dep = min(muestra_redd_depurada), .groups = "drop")

## Control del cruce que en 10 podia fallar en silencio
sin_geom  <- setdiff(trat$COD_DANE, mun$COD_DANE)
sin_panel <- setdiff(mun$COD_DANE, trat$COD_DANE)
cat(sprintf("Cruce shapefile-panel: %d codigos del panel sin geometria | %d geometrias sin panel\n",
            length(sin_geom), length(sin_panel)))
if (length(sin_geom)) cat("  sin geometria:", paste(head(sin_geom, 10), collapse = ", "), "\n")

panel <- panel %>% select(-any_of(c("G_dep", "muestra_dep"))) %>% inner_join(trat, by = "COD_DANE")
n_anios <- n_distinct(panel$year)
incompletos <- panel %>% group_by(COD_DANE) %>%
  summarise(ok = n() == n_anios && all(!is.na(.data[[OUTCOME]])), .groups = "drop") %>%
  filter(!ok) %>% pull(COD_DANE)
panel <- panel %>% filter(!COD_DANE %in% incompletos)

## =============================================================================
## 3. Exposicion por vecindad
## =============================================================================
titulo("2. EXPOSICION")
ar <- aristas %>%
  left_join(trat %>% rename(hacia = COD_DANE, G_v = G_dep, muestra_v = muestra_dep), by = "hacia")

expo <- ar %>% group_by(desde) %>%
  summarise(
    G_vec        = { g <- G_v[!is.na(G_v) & G_v > 0 & muestra_v %in% 1]; if (length(g)) min(g) else 0 },
    vec_ambiguo  = any(muestra_v == 0, na.rm = TRUE),
    n_vec_trat   = sum(!is.na(G_v) & G_v > 0 & muestra_v %in% 1),
    .groups = "drop") %>%
  rename(COD_DANE = desde) %>%
  mutate(exposicion_ambigua = vec_ambiguo & G_vec == 0)

base_spill <- panel %>%
  filter(G_dep == 0, muestra_dep == 1) %>%
  left_join(expo, by = "COD_DANE") %>%
  mutate(G_vec = coalesce(G_vec, 0), exposicion_ambigua = coalesce(exposicion_ambigua, FALSE))

amb <- base_spill %>% filter(exposicion_ambigua) %>% distinct(COD_DANE) %>% pull(COD_DANE)
fuera_ventana <- base_spill %>% filter(G_vec > 0, !G_vec %in% VENTANA) %>% distinct(COD_DANE) %>% pull(COD_DANE)

muestra_A <- base_spill %>% filter(!exposicion_ambigua, G_vec == 0 | G_vec %in% VENTANA)

resumen_muestra <- muestra_A %>% distinct(COD_DANE, G_vec)
cat(sprintf("Nunca tratados directamente (muestra depurada): %d\n", n_distinct(base_spill$COD_DANE)))
cat(sprintf("  excluidos por exposicion ambigua (unico vecino con proyecto fallido): %d\n", length(amb)))
cat(sprintf("  excluidos por cohorte de exposicion fuera de 2013-2019: %d\n", length(fuera_ventana)))
cat(sprintf("  muestra final: %d | expuestos: %d | no expuestos: %d\n",
            nrow(resumen_muestra), sum(resumen_muestra$G_vec > 0), sum(resumen_muestra$G_vec == 0)))
cat("Expuestos por cohorte de exposicion:\n")
print(table(resumen_muestra$G_vec[resumen_muestra$G_vec > 0]))

## =============================================================================
## 4. Enfoque A: DiD espacial
## =============================================================================
titulo("3. ENFOQUE A: CALLAWAY-SANT'ANNA SOBRE LA EXPOSICION")
correr <- function(xformla, etiqueta) {
  cat("\n--", etiqueta, "--\n")
  set.seed(SEMILLA)
  out <- tryCatch(suppressWarnings(att_gt(
    yname = OUTCOME, tname = "year", idname = "id_num", gname = "G_vec",
    xformla = xformla, data = muestra_A, control_group = "notyettreated", est_method = "dr",
    bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "id_num")),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
  if (is.null(out)) return(NULL)
  s <- aggte(out, type = "simple", na.rm = TRUE)
  b <- tryCatch(aggte(out, type = "dynamic", balance_e = BALANCE_E, min_e = 0,
                      max_e = BALANCE_E, na.rm = TRUE), error = function(e) NULL)
  d <- aggte(out, type = "dynamic", min_e = -6, max_e = BALANCE_E, na.rm = TRUE)
  cat(sprintf("ATT simple: %.2f (EE %.2f)  IC95 [%.2f, %.2f]\n", s$overall.att, s$overall.se,
              s$overall.att - 1.96 * s$overall.se, s$overall.att + 1.96 * s$overall.se))
  if (!is.null(b)) cat(sprintf("ATT balanceado e=0..%d: %.2f (EE %.2f)\n",
                               BALANCE_E, b$overall.att, b$overall.se))
  lo <- s$overall.att - 1.96 * s$overall.se; hi <- s$overall.att + 1.96 * s$overall.se
  cat("->", if (lo > 0) "positivo y significativo: evidencia de FUGA"
            else if (hi < 0) "negativo y significativo: efecto protector indirecto"
            else "sin evidencia detectable de desplazamiento en ninguna direccion", "\n")
  list(att_gt = out, simple = s, balanceado = b, dinamico = d, etiqueta = etiqueta,
       na_pct = 100 * mean(is.na(out$att)))
}
res_A <- list(sin_cov = correr(~1, "Sin covariables"),
              dr      = correr(COVARIABLES_XFORMLA, "Doblemente robusto con covariables"))

if (!is.null(res_A$dr)) {
  p <- ggdid(res_A$dr$dinamico) +
    labs(title = "Estudio de eventos del desplazamiento espacial (D1 depurada)",
         subtitle = "Municipios nunca tratados; exposicion = primer anio con un vecino tratado. DR.",
         x = "Anios desde la exposicion", y = "ATT sobre el vecino (hectareas)") +
    theme_minimal()
  ggsave(file.path(DIR_FIG, "45_event_study_spillover.png"), p, width = 9, height = 5.5, dpi = 150)
}

## =============================================================================
## 5. Enfoque B: TWFE con proporcion de vecinos tratados (solo contraste)
## =============================================================================
titulo("4. ENFOQUE B: TWFE CON REZAGO ESPACIAL (contraste)")
anios <- sort(unique(panel$year))
## Proporcion de vecinos con proyecto REDD+ vivo ya adoptado en cada anio.
## Los vecinos ambiguos cuentan en el denominador como no tratados.
ar_b <- ar %>% mutate(G_ef = ifelse(!is.na(G_v) & G_v > 0 & muestra_v %in% 1, G_v, Inf))
prop <- do.call(rbind, lapply(anios, function(a) {
  ar_b %>% group_by(desde) %>%
    summarise(prop_vecinos_tratados = mean(G_ef <= a), .groups = "drop") %>%
    mutate(year = a)
})) %>% rename(COD_DANE = desde)

panel_reg <- panel %>%
  filter(muestra_dep == 1) %>%
  mutate(tratado_directo = as.integer(G_dep > 0 & year >= G_dep)) %>%
  left_join(prop, by = c("COD_DANE", "year"))

mod_B <- feols(as.formula(paste(OUTCOME, "~ tratado_directo + prop_vecinos_tratados | COD_DANE + year")),
               data = panel_reg, cluster = ~COD_DANE)
print(summary(mod_B))
ct_B <- as.data.frame(coeftable(mod_B))

## =============================================================================
## 6. Resumen
## =============================================================================
titulo("5. RESUMEN")
fila <- function(r, enfoque) {
  if (is.null(r)) return(NULL)
  data.frame(enfoque = enfoque, especificacion = r$etiqueta,
             coef = r$simple$overall.att, ee = r$simple$overall.se,
             ic_inf = r$simple$overall.att - 1.96 * r$simple$overall.se,
             ic_sup = r$simple$overall.att + 1.96 * r$simple$overall.se,
             coef_balanceado = if (is.null(r$balanceado)) NA_real_ else r$balanceado$overall.att,
             ee_balanceado   = if (is.null(r$balanceado)) NA_real_ else r$balanceado$overall.se,
             na_pct = r$na_pct)
}
tabla <- rbind(
  fila(res_A$sin_cov, "A. DiD espacial"),
  fila(res_A$dr,      "A. DiD espacial"),
  data.frame(enfoque = "B. TWFE (contraste)", especificacion = "prop_vecinos_tratados",
             coef = ct_B["prop_vecinos_tratados", 1], ee = ct_B["prop_vecinos_tratados", 2],
             ic_inf = ct_B["prop_vecinos_tratados", 1] - 1.96 * ct_B["prop_vecinos_tratados", 2],
             ic_sup = ct_B["prop_vecinos_tratados", 1] + 1.96 * ct_B["prop_vecinos_tratados", 2],
             coef_balanceado = NA_real_, ee_balanceado = NA_real_, na_pct = NA_real_)
)
print(tabla %>% mutate(across(where(is.numeric), ~ round(.x, 2))), row.names = FALSE)
write_csv(tabla, file.path(DIR_TAB, "45_spillover_resumen.csv"))

saveRDS(list(enfoque_A = res_A, enfoque_B = mod_B, exposicion = expo,
             excluidos_ambiguos = amb, excluidos_fuera_ventana = fuera_ventana),
        file.path(DIR_OUT, "45_spillovers_depurada.rds"))
cat("\nGuardado: outputs/45_spillovers_depurada.rds, tablas/45_spillover_resumen.csv,",
    "figuras/45_event_study_spillover.png\n")
