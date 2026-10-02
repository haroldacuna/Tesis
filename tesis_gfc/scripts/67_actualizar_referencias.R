## =============================================================================
## 67_actualizar_referencias.R
##
## Escribe en 00_config_did.R las referencias de la corrida final (REF_*), a
## partir del resumen del 40. Las usan 44, 58, 59 y 60 para verificar que
## trabajan sobre la misma muestra y especificacion que la principal.
## Solo cambia los numeros de las lineas REF_*; imprime cada cambio.
## =============================================================================
source("scripts/00_config_did.R")
ruta <- file.path(DIR_TAB, "40_resumen_depurada_y_dosis.csv")
if (!file.exists(ruta)) stop("Falta ", ruta, ": corre primero el 40.")
r <- read.csv(ruta, stringsAsFactors = FALSE)
dr <- r[grepl("PRINCIPAL", r$especificacion), ][1, ]
sc <- r[r$especificacion == "D1 depurada - sin covariables", ][1, ]
if (is.na(dr$att_simple) || is.na(sc$att_simple)) stop("El resumen del 40 no trae la principal o la version sin covariables.")

m <- cargar_muestra_principal(verificar = FALSE)
mun <- unique(m[, c("COD_DANE", "G_dep")])
nuevos <- c(REF_N_TRATADOS = sum(mun$G_dep > 0), REF_N_CONTROLES = sum(mun$G_dep == 0),
            REF_ATT_DR = round(dr$att_simple, 2), REF_ATT_SINCOV = round(sc$att_simple, 2),
            REF_BAL_DR = round(dr$att_balanceado, 2), REF_BAL_SINCOV = round(sc$att_balanceado, 2))

ruta_cfg <- file.path(RAIZ_PROYECTO, "scripts/00_config_did.R")
txt <- readLines(ruta_cfg, warn = FALSE)
titulo("REFERENCIAS DE LA CORRIDA FINAL (00_config_did.R)")
for (k in names(nuevos)) {
  i <- grep(paste0("^", k, "\\s*<-"), txt)
  if (length(i) != 1) stop("No encuentro una unica linea ", k, " en 00_config_did.R")
  viejo <- sub(paste0("^", k, "\\s*<-\\s*([-0-9.]+).*$"), "\\1", txt[i])
  txt[i] <- sub("<-(\\s*)[-0-9.]+", paste0("<-\\1", nuevos[[k]]), txt[i])
  cat(sprintf("  %-16s %10s -> %s\n", k, viejo, nuevos[[k]]))
}
writeLines(txt, ruta_cfg)
cat("Guardado:", ruta_cfg, "\n")
