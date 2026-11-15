## =============================================================================
## 74_loo_ic.R
##
## Leave-one-out CON intervalos de confianza (comentario de Jorge sobre la
## Figura A.1: "Y los intervalos de confianza?", tarea 1.3 del plan).
##
## El 50 calcula el leave-one-out solo en puntos, con su motor. Aqui cada
## exclusion se reestima con did::att_gt, con la especificacion principal
## exacta (DR, aun no tratados, 11 covariables, bootstrap multiplicador de 999
## replicas agrupado por municipio), de modo que cada punto trae su EE e IC.
## Se incluye la exclusion conjunta de Cartagena del Chaira y San Jose del
## Guaviare (la del 50).
##
## Validacion: sin excluir a nadie debe reproducir REF_ATT_DR.
## Salida:  outputs/tablas/74_leave_one_out_ic.csv
## USO (desde tesis_gfc/):  Rscript scripts/74_loo_ic.R      (~20-40 min)
## =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(did) })
source("scripts/00_config_did.R")

muestra <- cargar_muestra_principal(verificar = TRUE)
EXCLUSION_CONJUNTA <- c("18150", "95001")

estimar <- function(d) {
  set.seed(SEMILLA)
  out <- tryCatch(suppressWarnings(
    att_gt(yname = OUTCOME, tname = "year", idname = "id_num", gname = "G_dep",
           xformla = COVARIABLES_XFORMLA, data = d, control_group = CONTROL_PRINCIPAL,
           est_method = "dr", bstrap = TRUE, biters = BITERS, cband = FALSE, clustervars = "id_num")),
    error = function(e) NULL)
  if (is.null(out)) return(c(att = NA_real_, ee = NA_real_))
  s <- aggte(out, type = "simple", na.rm = TRUE)
  c(att = s$overall.att, ee = s$overall.se)
}

titulo("VALIDACION")
v <- estimar(muestra)
cat(sprintf("Sin exclusiones: %.2f (EE %.2f) | referencia %.2f\n", v["att"], v["ee"], REF_ATT_DR))
if (abs(v["att"] - REF_ATT_DR) > 1) stop("No reproduce la principal.")

titulo("LEAVE-ONE-OUT CON IC")
trat <- muestra %>% filter(G_dep > 0) %>% distinct(COD_DANE, G_dep, .keep_all = TRUE)
nom_col <- intersect(c("NOMBRE_MPI", "municipio"), names(trat))[1]
exclusiones <- c(as.list(trat$COD_DANE), list(EXCLUSION_CONJUNTA))
t0 <- Sys.time()
res <- do.call(rbind, lapply(seq_along(exclusiones), function(k) {
  cods <- exclusiones[[k]]
  e <- estimar(muestra[!muestra$COD_DANE %in% cods, ])
  if (k %% 5 == 0) cat(sprintf("  %d/%d (%.1f min)\n", k, length(exclusiones),
                               as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  data.frame(excluido = paste(cods, collapse = "+"),
             NOMBRE_MPI = if (length(cods) == 1 && !is.na(nom_col)) trat[[nom_col]][trat$COD_DANE == cods] else NA,
             cohorte = paste(unique(trat$G_dep[trat$COD_DANE %in% cods]), collapse = "+"),
             att = e["att"], ee = e["ee"])
}))
res <- res %>% mutate(ic_inf = att - 1.96 * ee, ic_sup = att + 1.96 * ee,
                      excluye_cero = ic_inf > 0 | ic_sup < 0,
                      cambio = att - REF_ATT_DR, cambio_en_ee = cambio / REF_EE_DR) %>%
  arrange(att)
write.csv(res, file.path(DIR_TAB, "74_leave_one_out_ic.csv"), row.names = FALSE)

titulo("RESUMEN")
print(res %>% mutate(across(c(att, ee, ic_inf, ic_sup, cambio, cambio_en_ee), ~ round(.x, 1))), row.names = FALSE)
cat(sprintf("\nExclusiones cuyo IC excluye el cero: %d de %d\n", sum(res$excluye_cero, na.rm = TRUE), nrow(res)))
cat(sprintf("Rango del ATT: %.1f a %.1f\n", min(res$att, na.rm = TRUE), max(res$att, na.rm = TRUE)))
cat("Guardado: outputs/tablas/74_leave_one_out_ic.csv\n")
