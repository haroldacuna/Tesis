## =============================================================================
## 50_cohortes_loo_base.R
##
## Tres pruebas sobre la especificacion principal (D1 depurada, cohortes
## 2013-2019, controles limpios, DR, notyettreated, covariables del 02
## corregido, Hansen 2001-2024; ATT = -207.9 ha), motivadas por el 49: el
## efecto principal se concentra en las cohortes 2017-2019, en especial 2018
## (Cartagena del Chaira, San Jose del Guaviare), cuyo anio base g-1 = 2017
## coincide con el pico de deforestacion de la frontera amazonica.
##
## MOTOR. En Callaway-Sant'Anna los ATT post-tratamiento SIEMPRE usan g-1 como
## base (base_period solo cambia los pre). Para la prueba 3 se reimplementa el
## calculo de cada ATT(g,t) con el mismo estimador que usa did por dentro
## (DRDID::drdid_panel), los mismos controles (nunca tratados + aun no tratados
## en t) y la misma agregacion "simple" (pesos por tamanio de cohorte),
## cambiando solo la base:
##     base g-1 :  Y0 = Y_{i,g-1}
##     base prom:  Y0 = (1/5) sum_{s=g-5}^{g-1} Y_{is}
## VALIDACION OBLIGATORIA: con base g-1 el motor debe reproducir -207.9 ha
## (tolerancia 1 ha); si no, el script se detiene.
##
## INFERENCIA: bootstrap por municipio, B = 199, estratificado por cohorte y
## controles (cada replica conserva el tamanio de cada cohorte). IC percentil.
##
## REGLAS (fijadas ANTES de correr, 27/09/2026):
##   P1 Cohortes: el efecto principal se CONCENTRA en las tardias (2017-2019) si
##      el IC 95 % de las tempranas (2013-2016) incluye 0 y
##      |ATT tardias| > |ATT principal|.
##   P2 Leave-one-out: el resultado DEPENDE de un municipio si al excluirlo el
##      ATT cambia de signo o se mueve mas de 1 EE de la principal (198.5 ha).
##      Ademas se excluyen JUNTOS Cartagena del Chaira (18150) y San Jose del
##      Guaviare (95001).
##   P3 Periodo base: en las cohortes tardias, si |ATT base prom| <
##      0.5 |ATT base g-1|, la mayor parte de su efecto proviene del anio base.
##
## USO
##   Rscript scripts/50_cohortes_loo_base.R
## =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(DRDID)
  library(ggplot2)
})

RAIZ_PROYECTO <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "data/.raiz_canonica"))) {
    p <- dirname(d)
    if (identical(p, d)) stop("No encuentro data/.raiz_canonica subiendo desde ", getwd())
    d <- p
  }
  d
})
DIR_OUT <- file.path(RAIZ_PROYECTO, "outputs")
DIR_TAB <- file.path(DIR_OUT, "tablas")
DIR_FIG <- file.path(DIR_OUT, "figuras")
for (d in c(DIR_TAB, DIR_FIG)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
source(file.path(RAIZ_PROYECTO, "scripts/comun_controles_limpios.R"))
source(file.path(RAIZ_PROYECTO, "scripts/comun_covariables_pre.R"))

## --- Parametros (fijados antes de correr) ------------------------------------
SEMILLA   <- 20260824
VENTANA   <- 2013:2019
OUTCOME   <- "loss_area_ha"
TEMPRANAS <- 2013:2016
TARDIAS   <- 2017:2019
K_PROM    <- 5
B_BOOT    <- 199
PRINCIPAL <- c(att = -207.9, ee = 198.5)
EXCLUSION_CONJUNTA <- c("18150", "95001")   # Cartagena del Chaira, San Jose del Guaviare
X <- ~ log_baseline_forest + temp_media_c_base + prec_anual_mm_base +
  discapital_base + disbogota_base + distancia_mercado_base + H_coca_base +
  homicidios_base + log_defor_pre_media + defor_pre_tendencia

pad5   <- function(x) formatC(as.integer(x), width = 5, flag = "0")
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
titulo <- function(t) cat("\n", strrep("=", 74), "\n", t, "\n", strrep("=", 74), "\n", sep = "")

## =============================================================================
## Motor (probado contra did::att_gt con datos simulados)
## =============================================================================

## Estructuras: una fila por municipio (U), outcome en ancho (Y) y matriz de
## diseno de covariables con intercepto (XM), como la que att_gt pasa a DRDID.
preparar_motor <- function(d, xformla, outcome) {
  vars <- all.vars(xformla)
  U <- d[!duplicated(d$COD_DANE), c("COD_DANE", "G_dep", vars), drop = FALSE]
  U <- U[stats::complete.cases(U), , drop = FALSE]
  anios <- sort(unique(d$year))
  Y <- matrix(NA_real_, nrow(U), length(anios), dimnames = list(U$COD_DANE, anios))
  i <- match(d$COD_DANE, U$COD_DANE); j <- match(d$year, anios); ok <- !is.na(i)
  Y[cbind(i[ok], j[ok])] <- d[[outcome]][ok]
  if (anyNA(Y)) stop("Outcome incompleto en el panel: el motor requiere panel balanceado")
  XM <- stats::model.matrix(xformla, U)
  list(U = U, Y = Y, XM = XM, anios = anios)
}

## ATT(g,t) post-tratamiento para todas las cohortes. `idx` = filas de U que
## forman la muestra (con repeticiones en el bootstrap).
att_celdas <- function(M, base = c("g1", "prom"), idx = seq_len(nrow(M$U)), K = K_PROM) {
  base <- match.arg(base)
  G <- M$U$G_dep[idx]
  cohortes <- sort(unique(G[G > 0]))
  filas <- vector("list", 0)
  for (g in cohortes) {
    col_base <- if (base == "g1") as.character(g - 1) else as.character((g - K):(g - 1))
    if (!all(col_base %in% colnames(M$Y))) stop("Faltan anios base para la cohorte ", g)
    for (t in M$anios[M$anios >= g]) {
      sel <- idx[G == g | G == 0 | G > t]
      y1 <- M$Y[sel, as.character(t)]
      y0 <- if (base == "g1") M$Y[sel, col_base] else rowMeans(M$Y[sel, col_base, drop = FALSE])
      D  <- as.integer(M$U$G_dep[sel] == g)
      att <- tryCatch(suppressWarnings(
        DRDID::drdid_panel(y1 = y1, y0 = y0, D = D, covariates = M$XM[sel, , drop = FALSE])$ATT),
        error = function(e) NA_real_)
      filas[[length(filas) + 1]] <- data.frame(g = g, t = t, att = att, n_g = sum(G == g))
    }
  }
  do.call(rbind, filas)
}

## Agregacion "simple" de aggte(): promedio de celdas post no NA ponderado por
## el tamanio de la cohorte, opcionalmente restringido a un grupo de cohortes.
agregar <- function(cel, cohortes = NULL) {
  c2 <- cel[!is.na(cel$att), ]
  if (!is.null(cohortes)) c2 <- c2[c2$g %in% cohortes, ]
  if (!nrow(c2)) return(NA_real_)
  sum(c2$att * c2$n_g) / sum(c2$n_g)
}

resumen_grupos <- function(cel) c(todas = agregar(cel), tempranas = agregar(cel, TEMPRANAS),
                                  tardias = agregar(cel, TARDIAS))

## Indices bootstrap estratificados por cohorte (y controles)
indices_boot <- function(G) {
  unlist(lapply(split(seq_along(G), G), function(ix) ix[sample.int(length(ix), length(ix), replace = TRUE)]),
         use.names = FALSE)
}

## =============================================================================
## Datos (igual que el 40)
## =============================================================================
panel <- readRDS(file.path(DIR_OUT, "panel_analisis_did.rds"))
panel$COD_DANE <- pad5(panel$COD_DANE)
panel <- unir_covariables_pre(panel, file.path(DIR_TAB, "covariables_pre_municipio.csv"))
RUTA_TRAT <- file.path(RAIZ_PROYECTO, "data/final/panel_con_tratamiento_actualizado.csv")
COL_DEP <- "anio_inicio_tratamiento_todas_fuentes_redd_depurada"
COL_MUE <- "muestra_redd_depurada"
trat <- read_csv(RUTA_TRAT, col_select = all_of(c("COD_DANE", COL_DEP, COL_MUE)),
                 col_types = cols(COD_DANE = col_character(), .default = col_double()),
                 show_col_types = FALSE) %>%
  mutate(COD_DANE = pad5(COD_DANE)) %>% group_by(COD_DANE) %>%
  summarise(G_dep = coalesce(min_na(.data[[COL_DEP]]), 0),
            muestra_dep = min(.data[[COL_MUE]]), .groups = "drop")
base <- panel %>% select(-any_of(c("G_dep", "muestra_dep"))) %>%
  inner_join(trat, by = "COD_DANE") %>%
  filter(muestra_dep == 1, G_dep == 0 | G_dep %in% VENTANA)
base <- aplicar_controles_limpios(as.data.frame(base), municipios_con_proyecto(RUTA_TRAT), "G_dep")
nombres <- panel %>% distinct(COD_DANE, .keep_all = TRUE) %>%
  select(any_of(c("COD_DANE", "NOMBRE_MPI", "DPTO_CNMBR")))

M <- preparar_motor(base, X, OUTCOME)
cat(sprintf("Motor: %d municipios (%d tratados) x %d anios\n", nrow(M$U), sum(M$U$G_dep > 0), length(M$anios)))

## =============================================================================
## Validacion: base g-1 debe reproducir la principal
## =============================================================================
titulo("VALIDACION DEL MOTOR")
cel_g1   <- att_celdas(M, "g1")
cel_prom <- att_celdas(M, "prom")
v <- agregar(cel_g1)
cat(sprintf("Motor, base g-1: %.2f | principal (att_gt + aggte): %.1f\n", v, PRINCIPAL["att"]))
if (!is.finite(v) || abs(v - PRINCIPAL["att"]) > 1)
  stop("El motor NO reproduce la especificacion principal: no se interpretan las pruebas.")
cat("OK: el motor reproduce la principal; la unica diferencia entre bases es el anio de referencia.\n")
cat(sprintf("Celdas NA: base g-1 %d | base prom %d (de %d)\n",
            sum(is.na(cel_g1$att)), sum(is.na(cel_prom$att)), nrow(cel_g1)))

## =============================================================================
## Bootstrap (P1 y P3)
## =============================================================================
titulo(sprintf("BOOTSTRAP (B = %d, estratificado por cohorte)", B_BOOT))
set.seed(SEMILLA)
G_all <- M$U$G_dep
boot <- matrix(NA_real_, B_BOOT, 6,
               dimnames = list(NULL, c(paste0("g1_", c("todas", "tempranas", "tardias")),
                                       paste0("prom_", c("todas", "tempranas", "tardias")))))
t0 <- Sys.time()
for (b in seq_len(B_BOOT)) {
  ix <- indices_boot(G_all)
  boot[b, 1:3] <- resumen_grupos(att_celdas(M, "g1", ix))
  boot[b, 4:6] <- resumen_grupos(att_celdas(M, "prom", ix))
  if (b %% 20 == 0) cat(sprintf("  replica %d/%d (%.1f min)\n", b, B_BOOT,
                                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
pt <- c(resumen_grupos(cel_g1), resumen_grupos(cel_prom))
names(pt) <- colnames(boot)
tab <- data.frame(
  base  = rep(c("g-1", "promedio g-5..g-1"), each = 3),
  grupo = rep(c("todas", "tempranas 2013-2016", "tardias 2017-2019"), 2),
  att   = pt,
  ee_boot = apply(boot, 2, sd, na.rm = TRUE),
  ic_inf = apply(boot, 2, quantile, 0.025, na.rm = TRUE),
  ic_sup = apply(boot, 2, quantile, 0.975, na.rm = TRUE),
  replicas_validas = colSums(is.finite(boot)), row.names = NULL)
write_csv(tab, file.path(DIR_TAB, "50_cohortes_y_base.csv"))
print(tab, row.names = FALSE, digits = 4)

## Detalle por cohorte (puntos)
por_coh <- data.frame(g = sort(unique(cel_g1$g)))
por_coh$n_municipios <- sapply(por_coh$g, function(g) sum(G_all == g))
por_coh$att_base_g1   <- sapply(por_coh$g, function(g) agregar(cel_g1, g))
por_coh$att_base_prom <- sapply(por_coh$g, function(g) agregar(cel_prom, g))
por_coh$y_base_g1_tratados   <- sapply(por_coh$g, function(g) mean(M$Y[G_all == g, as.character(g - 1)]))
por_coh$y_base_prom_tratados <- sapply(por_coh$g, function(g)
  mean(rowMeans(M$Y[G_all == g, as.character((g - K_PROM):(g - 1)), drop = FALSE])))
cat("\nPor cohorte (el nivel del anio base de los tratados indica si g-1 fue un pico):\n")
print(por_coh, row.names = FALSE, digits = 4)
write_csv(por_coh, file.path(DIR_TAB, "50_por_cohorte.csv"))

## =============================================================================
## P2: leave-one-out (puntos, ambas bases)
## =============================================================================
titulo("LEAVE-ONE-OUT POR MUNICIPIO TRATADO")
tratados <- M$U$COD_DANE[M$U$G_dep > 0]
loo <- do.call(rbind, lapply(c(as.list(tratados), list(EXCLUSION_CONJUNTA)), function(cods) {
  ix <- which(!M$U$COD_DANE %in% cods)
  data.frame(excluido = paste(cods, collapse = "+"),
             cohorte = paste(unique(M$U$G_dep[M$U$COD_DANE %in% cods]), collapse = "+"),
             att_g1 = agregar(att_celdas(M, "g1", ix)),
             att_prom = agregar(att_celdas(M, "prom", ix)))
}))
loo <- loo %>% left_join(nombres, by = c("excluido" = "COD_DANE")) %>%
  mutate(cambio_g1 = att_g1 - PRINCIPAL["att"],
         depende = sign(att_g1) != sign(PRINCIPAL["att"]) | abs(cambio_g1) > PRINCIPAL["ee"]) %>%
  arrange(desc(abs(cambio_g1)))
write_csv(loo, file.path(DIR_TAB, "50_leave_one_out.csv"))
print(as.data.frame(loo %>% select(any_of(c("excluido", "NOMBRE_MPI", "cohorte")), att_g1,
                                   cambio_g1, att_prom, depende) %>% head(12)),
      row.names = FALSE, digits = 4)

p <- ggplot(loo %>% filter(!grepl("\\+", excluido)),
            aes(reorder(coalesce(NOMBRE_MPI, excluido), att_g1), att_g1)) +
  geom_hline(yintercept = PRINCIPAL["att"], color = "grey30") +
  geom_hline(yintercept = PRINCIPAL["att"] + c(-1, 1) * PRINCIPAL["ee"], linetype = "dashed", color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(aes(color = depende), size = 2) + coord_flip() +
  scale_color_manual(values = c(`FALSE` = "grey40", `TRUE` = "firebrick")) +
  labs(title = "Leave-one-out: ATT al excluir cada municipio tratado",
       subtitle = "Linea continua: principal (-207,9 ha); discontinuas: +/- 1 EE", x = NULL,
       y = "ATT (ha), base g-1", color = "Cambia signo o > 1 EE") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(DIR_FIG, "50_leave_one_out.png"), p, width = 8, height = 9, dpi = 150)

## =============================================================================
## Reglas
## =============================================================================
titulo("REGLAS FIJADAS ANTES DE CORRER")
temp <- tab[tab$base == "g-1" & tab$grupo == "tempranas 2013-2016", ]
tard <- tab[tab$base == "g-1" & tab$grupo == "tardias 2017-2019", ]
tard_p <- tab[tab$base == "promedio g-5..g-1" & tab$grupo == "tardias 2017-2019", ]
p1 <- temp$ic_inf <= 0 && temp$ic_sup >= 0 && abs(tard$att) > abs(PRINCIPAL["att"])
cat(sprintf("P1 tempranas %.1f [%.1f, %.1f] | tardias %.1f [%.1f, %.1f] -> %s\n",
            temp$att, temp$ic_inf, temp$ic_sup, tard$att, tard$ic_inf, tard$ic_sup,
            if (p1) "el efecto principal SE CONCENTRA en las cohortes tardias" else "no se cumple"))
dep <- loo[loo$depende, ]
cat(sprintf("P2 municipios de los que depende el resultado: %s\n",
            if (nrow(dep)) paste(sprintf("%s (%s: %.1f)", dep$excluido, coalesce(dep$NOMBRE_MPI, "conjunta"),
                                         dep$att_g1), collapse = "; ") else "ninguno"))
conj <- loo[loo$excluido == paste(EXCLUSION_CONJUNTA, collapse = "+"), ]
cat(sprintf("   exclusion conjunta 18150 + 95001: %.1f (base g-1) | %.1f (base prom)\n",
            conj$att_g1, conj$att_prom))
p3 <- abs(tard_p$att) < 0.5 * abs(tard$att)
cat(sprintf("P3 tardias: base g-1 %.1f vs base promedio %.1f [%.1f, %.1f] -> %s\n",
            tard$att, tard_p$att, tard_p$ic_inf, tard_p$ic_sup,
            if (p3) "la mayor parte del efecto de las tardias PROVIENE DEL ANIO BASE"
            else "el efecto de las tardias NO se explica por el anio base"))
saveRDS(list(celdas_g1 = cel_g1, celdas_prom = cel_prom, boot = boot, tabla = tab,
             por_cohorte = por_coh, loo = loo), file.path(DIR_OUT, "50_resultados.rds"))
cat("\nGuardado: outputs/tablas/50_cohortes_y_base.csv, 50_por_cohorte.csv, 50_leave_one_out.csv\n")
cat("Figura:   outputs/figuras/50_leave_one_out.png\n")
