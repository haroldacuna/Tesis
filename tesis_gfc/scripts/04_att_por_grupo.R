## =============================================================================
## 04_att_por_grupo.R
##
## Extrae y grafica el ATT por cohorte de tratamiento (agg_grupo, ya calculado
## dentro de 03_did_callaway_santanna.R pero no exportado en detalle) — para
## ver si el efecto agregado (ATT simple) esconde heterogeneidad entre
## cohortes, o si el patrón es parejo en todas.
##
## Requiere haber corrido 03_did_callaway_santanna.R antes (usa
## output/resultados_did_completos.rds).
##
## USO
## ---
##   Rscript 04_att_por_grupo.R
## =============================================================================

library(readr)
library(dplyr)
library(did)
library(ggplot2)

resultados <- readRDS("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/resultados_did_completos.rds")
panel <- readRDS("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/panel_analisis_did.rds")
dir.create("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/tablas", showWarnings = FALSE, recursive = TRUE)
dir.create("C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/figuras", showWarnings = FALSE, recursive = TRUE)

## =============================================================================
## Tamaño de cada cohorte (cuántos municipios), para dar contexto a la
## precisión de cada estimado — cohortes chicas van a tener IC más anchos,
## y es importante distinguir "efecto genuinamente distinto" de "efecto
## impreciso por poca muestra".
## =============================================================================

tamanos_cohorte_alta <- panel %>%
  filter(year == min(year), first_treat_alta > 0) %>%
  count(first_treat_alta, name = "n_municipios") %>%
  rename(cohorte = first_treat_alta)

tamanos_cohorte_todas <- panel %>%
  filter(year == min(year), first_treat_todas > 0) %>%
  count(first_treat_todas, name = "n_municipios") %>%
  rename(cohorte = first_treat_todas)

## =============================================================================
## Función: arma tabla limpia a partir del objeto aggte(type="group")
## =============================================================================

tabla_por_grupo <- function(resultado, tamanos_cohorte) {
  if (is.null(resultado)) return(NULL)
  g <- resultado$grupo
  crit <- g$crit.val.egt

  tibble(
    cohorte = g$egt,
    att = g$att.egt,
    se = g$se.egt,
    ic_inf = att - crit * se,
    ic_sup = att + crit * se,
    significativo = (ic_inf > 0) | (ic_sup < 0)
  ) %>%
    left_join(tamanos_cohorte, by = "cohorte") %>%
    relocate(n_municipios, .after = cohorte)
}

## =============================================================================
## Construir tabla para las especificaciones "doblemente robusto" (la
## recomendada como principal) en ambas definiciones de tratamiento
## =============================================================================

tabla_alta <- tabla_por_grupo(resultados$alta_dr, tamanos_cohorte_alta)
tabla_todas <- tabla_por_grupo(resultados$todas_dr, tamanos_cohorte_todas)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("ATT POR COHORTE — Confianza alta (doblemente robusto)\n")
cat(strrep("=", 70), "\n")
print(tabla_alta, n = Inf)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("ATT POR COHORTE — Todas las fuentes (doblemente robusto)\n")
cat(strrep("=", 70), "\n")
print(tabla_todas, n = Inf)

n_sig_alta <- sum(tabla_alta$significativo, na.rm = TRUE)
n_sig_todas <- sum(tabla_todas$significativo, na.rm = TRUE)
cat("\nCohortes con ATT individualmente significativo (banda simultánea 95%):\n")
cat("  Confianza alta:", n_sig_alta, "de", nrow(tabla_alta), "\n")
cat("  Todas las fuentes:", n_sig_todas, "de", nrow(tabla_todas), "\n")
if (n_sig_alta > 0) {
  cat("  Cohortes significativas (alta confianza):", tabla_alta$cohorte[tabla_alta$significativo], "\n")
}
if (n_sig_todas > 0) {
  cat("  Cohortes significativas (todas las fuentes):", tabla_todas$cohorte[tabla_todas$significativo], "\n")
}

write_csv(tabla_alta, "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/tablas/att_por_cohorte_alta_confianza.csv")
write_csv(tabla_todas, "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/tablas/att_por_cohorte_todas_fuentes.csv")
cat("\nGuardado: output/tablas/att_por_cohorte_alta_confianza.csv\n")
cat("Guardado: output/tablas/att_por_cohorte_todas_fuentes.csv\n")

## =============================================================================
## Graficos: ATT por cohorte, con barra de error. Se marca en otro color la(s)
## cohorte(s) individualmente significativa(s), si hay alguna.
## =============================================================================

graficar_por_grupo <- function(tabla, etiqueta, ruta) {
  tabla <- tabla %>% mutate(cohorte_lbl = factor(cohorte))

  p <- ggplot(tabla, aes(x = cohorte_lbl, y = att, color = significativo)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup), size = 0.7) +
    geom_text(aes(label = paste0("n=", n_municipios)), vjust = -1.3, size = 3, color = "grey30") +
    scale_color_manual(values = c(`TRUE` = "#d62728", `FALSE` = "#1f77b4"), guide = "none") +
    labs(
      title = paste("ATT por cohorte de tratamiento -", etiqueta),
      subtitle = "Especificacion doblemente robusta. Rojo = significativo al 95% (banda simultanea). n = municipios en la cohorte.",
      x = "Año de inicio del tratamiento (cohorte)",
      y = "ATT (efecto sobre hectareas deforestadas)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(ruta, p, width = 10, height = 6, dpi = 150)
  cat("Grafico guardado:", ruta, "\n")
}

graficar_por_grupo(tabla_alta, "Confianza alta", "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/figuras/att_por_cohorte_alta.png")
graficar_por_grupo(tabla_todas, "Todas las fuentes", "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/outputs/figuras/att_por_cohorte_todas.png")

cat(
  "\n*** Interpretacion sugerida: si el ATT simple (agregado) no es",
  "significativo pero aqui ves 1-2 cohortes con estimados grandes y",
  "opuestos entre si (unas positivas, otras negativas), el promedio simple",
  "se puede estar cancelando -- vale la pena reportar la heterogeneidad por",
  "cohorte en vez de (o ademas de) el ATT agregado. Si en cambio todas las",
  "cohortes apuntan en la misma direccion pero ninguna es individualmente",
  "significativa, el problema es mas de potencia estadistica (muestra chica",
  "por cohorte) que de heterogeneidad real. ***\n"
)
