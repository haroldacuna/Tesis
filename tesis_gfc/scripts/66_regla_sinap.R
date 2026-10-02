## =============================================================================
## 66_regla_sinap.R  (decision D4, se aplica en la corrida final)
##
## Regla fijada antes de estimar con SINAP: la proporcion del municipio en el
## SINAP publico a 2012 (pct_sinap_pub_base, script 62) entra al modelo
## doblemente robusto SOLO si no aumenta el numero de pares cohorte-anio
## perdidos (NA en att_gt) frente a la especificacion sin ella.
##
## Corre la principal con y sin la variable sobre la misma muestra y:
##   - si entra y no estaba: la agrega a COVARIABLES_XFORMLA en 00_config_did.R
##   - si no entra y estaba: la quita
## Sale con codigo 10 si cambio la configuracion (run_all vuelve a correr el 40),
## 0 si no la cambio. Deja la decision en outputs/tablas/66_regla_sinap.csv.
## =============================================================================
source("scripts/00_config_did.R")
suppressPackageStartupMessages({ library(dplyr); library(did) })

m <- cargar_muestra_principal()
cb <- readRDS(file.path(DIR_OUT, "covariables_base_municipio.rds"))
cb$COD_DANE <- pad5(cb$COD_DANE)
if (!"pct_sinap_pub_base" %in% names(cb)) stop("covariables_base_municipio.rds no trae pct_sinap_pub_base: corre 62 y 01.")
m <- m %>% select(-any_of("pct_sinap_pub_base")) %>%
  left_join(cb %>% select(COD_DANE, pct_sinap_pub_base), by = "COD_DANE") %>%
  mutate(id_num = as.integer(id_num), G_dep = as.integer(G_dep))

ya <- "pct_sinap_pub_base" %in% all.vars(COVARIABLES_XFORMLA)
x_sin <- if (ya) update(COVARIABLES_XFORMLA, ~ . - pct_sinap_pub_base) else COVARIABLES_XFORMLA
x_con <- update(x_sin, ~ . + pct_sinap_pub_base)

correr <- function(xf, etq) {
  set.seed(SEMILLA)
  out <- suppressWarnings(att_gt(yname = OUTCOME, tname = "year", idname = "id_num", gname = "G_dep",
    xformla = xf, data = m, control_group = "notyettreated", est_method = "dr",
    bstrap = TRUE, biters = BITERS, cband = TRUE, clustervars = "id_num"))
  s <- aggte(out, type = "simple", na.rm = TRUE)
  r <- data.frame(especificacion = etq, pares_na = sum(is.na(out$att)), pares_total = length(out$att),
                  att = s$overall.att, ee = s$overall.se)
  cat(sprintf("  %-28s NA %d/%d | ATT %.2f (EE %.2f)\n", etq, r$pares_na, r$pares_total, r$att, r$ee))
  r
}
titulo("REGLA D4: ¿pct_sinap_pub_base ENTRA AL MODELO DOBLEMENTE ROBUSTO?")
r_sin <- correr(x_sin, "Sin SINAP (10 covariables)")
r_con <- correr(x_con, "Con SINAP (11 covariables)")
entra <- r_con$pares_na <= r_sin$pares_na
cat(sprintf("\nDecision: SINAP %s al modelo DR (pares NA: %d con vs %d sin).\n",
            if (entra) "ENTRA" else "NO ENTRA", r_con$pares_na, r_sin$pares_na))
if (!entra) cat("Segun D4 va solo al emparejamiento: revisar si 02_matching_psm.R debe incluirla.\n")
write.csv(rbind(r_sin, r_con) %>% mutate(decision = if (entra) "entra" else "no entra"),
          file.path(DIR_TAB, "66_regla_sinap.csv"), row.names = FALSE)

cambio <- FALSE
ruta_cfg <- file.path(RAIZ_PROYECTO, "scripts/00_config_did.R")
txt <- readLines(ruta_cfg, warn = FALSE)
i <- grep("log_defor_pre_media \\+ defor_pre_tendencia", txt)
if (length(i) != 1) stop("No encuentro la ultima linea de COVARIABLES_XFORMLA en 00_config_did.R")
if (entra && !ya) {
  txt[i] <- sub("defor_pre_tendencia\\s*$", "defor_pre_tendencia + pct_sinap_pub_base", txt[i]); cambio <- TRUE
} else if (!entra && ya) {
  txt[i] <- sub("\\s*\\+\\s*pct_sinap_pub_base", "", txt[i]); cambio <- TRUE
}
if (cambio) {
  writeLines(txt, ruta_cfg)
  cat("00_config_did.R actualizado: ", trimws(txt[i]), "\n", sep = "")
}
quit(status = if (cambio) 10 else 0)
