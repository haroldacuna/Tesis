## =============================================================================
## 68_heterogeneidad_fiscal.R   (reemplaza a 11_heterogeneidad_territorial.R)
##
## Heterogeneidad del efecto por capacidad fiscal y gasto ambiental de linea
## base [JM p.43]. Exploratoria: no corresponde a ninguna hipotesis.
##
## Variables (script 63, Panel de Buen Gobierno del CEDE):
##   inv_amb_pct_2002_2009  inversion en medio ambiente / inversion total, promedio 2002-2009
##   idf_2001_2009          Indice de Desempeno Fiscal del DNP, promedio 2001-2009
## Los grupos se cortan en la mediana de TODOS los municipios de la muestra
## principal (tratados y controles), para que el corte no dependa del tratamiento.
##
## Estimacion: la misma de la principal (D1 depurada, cohortes 2013-2019,
## controles limpios, notyettreated), por subgrupo. Por el tamanio de los
## subgrupos se reporta SIN covariables (como el contraste del mecanismo, 58);
## la version DR se corre y se reporta con su numero de pares perdidos.
##
## REGLA DE LECTURA (fijada antes de correr):
##   - Un subgrupo se estima solo si tiene >= 3 tratados.
##   - La diferencia entre grupos se lee como hallazgo solo si ambos grupos
##     tienen al menos 2 cohortes con >= 3 tratados Y el IC95 de la diferencia
##     (EE conservadora, sin covarianza) excluye el cero. Si no, es descriptiva.
##   - Diagnostico de confusion: si un grupo concentra a los tratados amazonicos,
##     la diferencia puede ser regional y no fiscal (ver seccion 6.6).
##
## Salidas: outputs/tablas/68_heterogeneidad_fiscal.csv, 68_cruce_amazonia.csv
## USO: Rscript scripts/68_heterogeneidad_fiscal.R
## =============================================================================
source("scripts/00_config_did.R")
suppressPackageStartupMessages({ library(dplyr); library(did) })

m <- cargar_muestra_principal()
ruta_f <- file.path(RAIZ_PROYECTO, "data/interim/fiscal_linea_base_municipio.csv")
if (!file.exists(ruta_f)) stop("Falta ", ruta_f, ": corre el 63.")
fis <- read.csv(ruta_f, colClasses = c(COD_DANE = "character")) %>% mutate(COD_DANE = pad5(COD_DANE))
m <- m %>% select(-any_of(c("idf_2001_2009", "inv_amb_pct_2002_2009"))) %>%
  left_join(fis %>% select(COD_DANE, idf_2001_2009, inv_amb_pct_2002_2009), by = "COD_DANE") %>%
  mutate(id_num = as.integer(id_num), G_dep = as.integer(G_dep))

mun <- m %>% distinct(COD_DANE, G_dep, idf_2001_2009, inv_amb_pct_2002_2009)
AMAZONIA <- c("18", "86", "91", "94", "95", "97")   # Caqueta, Putumayo, Amazonas, Guainia, Guaviare, Vaupes
mun$amazonia <- substr(mun$COD_DANE, 1, 2) %in% AMAZONIA

VARS <- list(
  inv_amb_pct_2002_2009 = c("Gasto ambiental alto", "Gasto ambiental bajo"),
  idf_2001_2009 = c("Desempe\u00f1o fiscal alto", "Desempe\u00f1o fiscal bajo"))

correr <- function(dat, xf, etq) {
  nt <- n_distinct(dat$COD_DANE[dat$G_dep > 0]); nc <- n_distinct(dat$COD_DANE[dat$G_dep == 0])
  coh <- dat %>% filter(G_dep > 0) %>% distinct(COD_DANE, G_dep) %>% count(G_dep)
  base <- data.frame(subgrupo = etq, n_tratados = nt, n_controles = nc, cohortes_ok = sum(coh$n >= 3))
  if (nt < 3) { cat(sprintf("  %-40s %2d tratados -> se omite\n", etq, nt)); return(cbind(base, att = NA, ee = NA, gt_na = NA, gt_total = NA)) }
  set.seed(SEMILLA)
  out <- tryCatch(suppressWarnings(att_gt(yname = OUTCOME, tname = "year", idname = "id_num", gname = "G_dep",
    xformla = xf, data = dat, control_group = "notyettreated", est_method = "dr",
    bstrap = TRUE, biters = BITERS, clustervars = "id_num")), error = function(e) NULL)
  s <- if (!is.null(out)) tryCatch(aggte(out, type = "simple", na.rm = TRUE), error = function(e) NULL) else NULL
  if (is.null(s)) { cat(sprintf("  %-40s no se pudo estimar\n", etq)); return(cbind(base, att = NA, ee = NA, gt_na = NA, gt_total = NA)) }
  r <- cbind(base, att = s$overall.att, ee = s$overall.se, gt_na = sum(is.na(out$att)), gt_total = length(out$att))
  cat(sprintf("  %-40s %2d trat | %3d ctrl | coh>=3: %d | ATT %8.1f (EE %7.1f) | NA %d/%d\n",
              etq, nt, nc, r$cohortes_ok, r$att, r$ee, r$gt_na, r$gt_total))
  r
}

res <- list(); cruce <- list()
for (v in names(VARS)) {
  med <- median(mun[[v]], na.rm = TRUE)
  alto <- mun$COD_DANE[!is.na(mun[[v]]) & mun[[v]] > med]
  bajo <- mun$COD_DANE[!is.na(mun[[v]]) & mun[[v]] <= med]
  titulo(sprintf("%s (mediana de la muestra = %.2f)", v, med))
  cat(sprintf("  sin dato: %d municipios (%d tratados)\n", sum(is.na(mun[[v]])), sum(is.na(mun[[v]]) & mun$G_dep > 0)))
  for (esp in c("sincov", "dr")) {
    xf <- if (esp == "sincov") ~1 else COVARIABLES_XFORMLA
    for (k in 1:2) {
      ids <- if (k == 1) alto else bajo
      r <- correr(m %>% filter(COD_DANE %in% ids), xf, paste0(VARS[[v]][k], " - ", esp))
      res[[length(res) + 1]] <- cbind(variable = v, grupo = VARS[[v]][k], especificacion = esp, mediana = med, r)
    }
  }
  t <- mun %>% filter(!is.na(.data[[v]])) %>% mutate(grupo = ifelse(.data[[v]] > med, VARS[[v]][1], VARS[[v]][2])) %>%
    group_by(grupo) %>% summarise(tratados = sum(G_dep > 0), tratados_amazonia = sum(G_dep > 0 & amazonia),
                                  controles = sum(G_dep == 0), .groups = "drop") %>% mutate(variable = v)
  cat("  Cruce con la Amazonia (diagnostico de confusion regional):\n"); print(as.data.frame(t))
  cruce[[v]] <- t
}
tab <- bind_rows(res) %>% mutate(ic_inf = att - 1.96 * ee, ic_sup = att + 1.96 * ee)

titulo("REGLA DE LECTURA")
for (v in names(VARS)) for (esp in c("sincov", "dr")) {
  a <- tab[tab$variable == v & tab$especificacion == esp, ]
  if (nrow(a) < 2 || any(is.na(a$att))) { cat(sprintf("  %-24s %-6s: algun grupo sin estimacion -> descriptivo\n", v, esp)); next }
  d <- a$att[1] - a$att[2]; se <- sqrt(a$ee[1]^2 + a$ee[2]^2)
  ok_coh <- all(a$cohortes_ok >= 2); excl <- abs(d) > 1.96 * se
  cat(sprintf("  %-24s %-6s: diferencia alto - bajo = %8.1f (EE %7.1f) IC [%8.1f; %8.1f] | cohortes ok: %s -> %s\n",
              v, esp, d, se, d - 1.96 * se, d + 1.96 * se, ok_coh,
              if (ok_coh && excl) "HALLAZGO (exploratorio)" else "descriptivo"))
  tab <- rbind(tab, data.frame(variable = v, grupo = "Diferencia (alto - bajo)", especificacion = esp, mediana = a$mediana[1],
    subgrupo = paste0("Diferencia - ", esp), n_tratados = NA, n_controles = NA, cohortes_ok = NA,
    att = d, ee = se, gt_na = NA, gt_total = NA, ic_inf = d - 1.96 * se, ic_sup = d + 1.96 * se))
}
write.csv(tab, file.path(DIR_TAB, "68_heterogeneidad_fiscal.csv"), row.names = FALSE)
write.csv(bind_rows(cruce), file.path(DIR_TAB, "68_cruce_amazonia.csv"), row.names = FALSE)
cat("\nGuardado: outputs/tablas/68_heterogeneidad_fiscal.csv y 68_cruce_amazonia.csv\n")
