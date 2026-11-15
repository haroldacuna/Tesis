## =============================================================================
## 77_gradiente_dosis.R
##
## Gradiente del efecto en la dosis (segunda prediccion de H1), en dos etapas.
## Complementa al 40: contdid 0.1.1 entrega el efecto promedio sobre la
## distribucion de dosis, pero su pendiente (ACRT) sale con un error estandar
## degenerado y cambia de signo segun la medida de dosis, de modo que no sirve
## para evaluar el gradiente. Este script estima ese gradiente directamente.
##
## Etapa 1. Efecto por municipio tratado, en la misma escala que el 40 (perdida
## dividida por la media propia de 2001-2012, b_i):
##
##   tau_i = [ mean_{t=g..g+5} Y_it/b_i - R_i ] - [ mean_{t=g..g+5} Y_jt/b_j - R_j ]_{j nunca tratado}
##
## con dos referencias R: el anio g-1 (la del estimador principal) y el promedio
## de g-5..g-1 (menos expuesta a un pico en el anio previo). La segunda resta es
## el cambio promedio de los nunca tratados limpios en los MISMOS anios: es el
## ATT(g, d_i) de Callaway, Goodman-Bacon y Sant'Anna con una sola unidad.
##
## Etapa 2. tau_i = alpha + beta * d_i (+ efectos fijos de cohorte) + u_i
##   beta: cambio del efecto al pasar de dosis 0 a dosis 1 (todo el bosque o el
##   municipio cubierto). Su lectura causal exige tendencias paralelas FUERTES
##   (Callaway et al., 2024): municipios con distinta dosis habrian tenido el
##   mismo efecto con la misma dosis. Con efectos fijos de cohorte, beta compara
##   solo municipios de la misma cohorte, porque la dosis baja se concentra en
##   las cohortes tardias.
##   Inferencia: errores HC2 y valor p por permutacion de la dosis (9.999),
##   que no depende de aproximaciones asintoticas con 26 unidades.
##
## USO:     Rscript scripts/77_gradiente_dosis.R
## Insumos: outputs/40_muestra_A.rds (40) y data/interim/dosis_tratamiento_poligonos.csv (76)
## Salida:  outputs/tablas/77_gradiente_dosis.csv
##          outputs/tablas/77_efecto_por_municipio.csv
## =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sandwich) })
source("scripts/00_config_did.R")

PRE_COMUN <- 2001:2012
N_PERM    <- 9999

## --- Datos ---------------------------------------------------------------------
## Muestra exacta de la principal (40 + controles limpios): el outcome es el
## mismo loss_area_ha reparado que usa el 40. OJO: no usar el loss_area_ha de
## panel_con_tratamiento_actualizado.csv, que conserva la version previa a la
## reparacion del 21 (p. ej., Mutata aparece con perdida cero todos los anios).
m <- cargar_muestra_principal(verificar = TRUE)
if (!file.exists(RUTA_DOSIS_POLIGONOS)) stop("Falta ", RUTA_DOSIS_POLIGONOS, ": corre scripts/76_dosis_poligonos.py")
dos <- read_csv(RUTA_DOSIS_POLIGONOS, col_types = cols(COD_DANE = col_character()), show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE))
controles <- unique(m$COD_DANE[m$G_dep == 0])
tratados  <- m %>% filter(G_dep > 0) %>% distinct(COD_DANE, G = G_dep) %>%
  inner_join(dos %>% filter(tiene_dosis == 1) %>% select(COD_DANE, dosis_bosque, dosis_area), by = "COD_DANE")
cat(sprintf("Controles limpios: %d | tratados con dosis en la muestra principal: %d\n", length(controles), nrow(tratados)))

## Perdida relativa a la media propia de 2001-2012
y <- m %>% select(COD_DANE, year, loss_area_ha = all_of(OUTCOME)) %>%
  group_by(COD_DANE) %>% mutate(b = mean(loss_area_ha[year %in% PRE_COMUN], na.rm = TRUE)) %>% ungroup() %>%
  filter(is.finite(b), b > 0) %>% mutate(yr = loss_area_ha / b)

cambio <- function(ids, g, ref) {
  d <- y %>% filter(COD_DANE %in% ids)
  post <- d %>% filter(year >= g, year <= min(g + BALANCE_E, max(y$year))) %>% group_by(COD_DANE) %>% summarise(p = mean(yr), .groups = "drop")
  pre  <- d %>% filter(if (ref == "g-1") year == g - 1 else year %in% (g - 5):(g - 1)) %>%
    group_by(COD_DANE) %>% summarise(r = mean(yr), .groups = "drop")
  inner_join(post, pre, by = "COD_DANE") %>% mutate(delta = p - r)
}

## --- Etapa 1 ---------------------------------------------------------------------
etapa1 <- do.call(rbind, lapply(c("g-1", "g-5..g-1"), function(ref) {
  do.call(rbind, lapply(sort(unique(tratados$G)), function(g) {
    c0 <- mean(cambio(controles, g, ref)$delta)
    cambio(tratados$COD_DANE[tratados$G == g], g, ref) %>% mutate(tau = delta - c0, G = g, referencia = ref)
  }))
})) %>% left_join(tratados %>% select(-G), by = "COD_DANE")
write_csv(etapa1, file.path(DIR_TAB, "77_efecto_por_municipio.csv"))

## --- Etapa 2 ---------------------------------------------------------------------
estimar <- function(d, dosis, fe) {
  f <- if (fe) as.formula(paste("tau ~", dosis, "+ factor(G)")) else as.formula(paste("tau ~", dosis))
  m <- lm(f, data = d); b <- unname(coef(m)[dosis]); se <- sqrt(diag(vcovHC(m, type = "HC2")))[dosis]
  set.seed(SEMILLA)
  perm <- replicate(N_PERM, {
    dd <- d
    dd[[dosis]] <- if (fe) ave(dd[[dosis]], dd$G, FUN = function(v) v[sample.int(length(v))]) else sample(dd[[dosis]])
    coef(lm(f, data = dd))[dosis]
  })
  data.frame(dosis = dosis, efectos_fijos_cohorte = fe, n = nrow(d), beta = b, ee_hc2 = unname(se),
             ic_inf = b - 1.96 * se, ic_sup = b + 1.96 * se,
             p_permutacion = (1 + sum(abs(perm) >= abs(b))) / (N_PERM + 1),
             att_promedio = mean(d$tau), cor_dosis_cohorte = cor(d[[dosis]], d$G))
}
res <- do.call(rbind, lapply(c("g-1", "g-5..g-1"), function(ref) {
  d <- etapa1 %>% filter(referencia == ref)
  cbind(referencia = ref, do.call(rbind, lapply(c("dosis_bosque", "dosis_area"), function(ds)
    rbind(estimar(d, ds, FALSE), estimar(d, ds, TRUE)))))
}))
titulo("GRADIENTE DEL EFECTO EN LA DOSIS (dos etapas)")
print(res %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
cat("\nLectura: beta es el cambio del efecto (en fracciones de la media propia 2001-2012) al pasar de dosis 0 a 1.\n",
    "att_promedio con referencia g-1 debe parecerse al ATT global del 40 (0,18).\n", sep = "")
write_csv(res, file.path(DIR_TAB, "77_gradiente_dosis.csv"))
cat("Guardado: outputs/tablas/77_gradiente_dosis.csv y 77_efecto_por_municipio.csv\n")
