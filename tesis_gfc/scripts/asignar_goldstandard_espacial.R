## =============================================================================
## asignar_goldstandard_espacial.R
##
## Reconstruye la asignacion municipal de los proyectos de Gold Standard
## desde el archivo crudo (28 proyectos, con coordenadas propias), en vez
## de depender de archivos intermedios incompletos. Replica el mismo
## metodo ya usado para Verra: valida que las coordenadas caigan dentro
## del rango geografico de Colombia, corrige errores sistematicos de signo
## e intercambio de latitud/longitud cuando sea necesario, y asigna
## municipio mediante punto en poligono contra el shapefile real del DANE.
##
## USO
## ---
##   Rscript asignar_goldstandard_espacial.R
## =============================================================================

library(dplyr)
library(sf)
library(readr)

DATA_ROOT <- "C:/Users/USUARIO/Documents/Maestria/Tesis/tesis_gfc/data"
RUTA_GS_CRUDO <- file.path(DATA_ROOT, "interim/goldstandard_projects_colombia.csv")
RUTA_SHAPEFILE <- file.path(DATA_ROOT, "interim/municipios_clean.gpkg")

# Rango geografico continental de Colombia (mismo usado para Verra)
LAT_MIN <- -4.5; LAT_MAX <- 13.6
LON_MIN <- -82.0; LON_MAX <- -66.5

en_rango_colombia <- function(lat, lon) {
  !is.na(lat) & !is.na(lon) & lat >= LAT_MIN & lat <= LAT_MAX & lon >= LON_MIN & lon <= LON_MAX
}

gs <- read_csv(RUTA_GS_CRUDO, show_col_types = FALSE)
cat("Proyectos Gold Standard cargados:", nrow(gs), "\n")
cat("Con coordenadas no nulas:", sum(!is.na(gs$latitude) & !is.na(gs$longitude)), "\n\n")

## =============================================================================
## 1. Validar y corregir coordenadas (mismo procedimiento que Verra: probar
##    las 4 combinaciones posibles de signo/intercambio y aceptar la unica
##    que cae dentro del rango de Colombia)
## =============================================================================

corregir_fila <- function(lat, lon) {
  if (is.na(lat) || is.na(lon)) return(list(lat = NA, lon = NA, metodo = "sin_coordenadas"))

  candidatos <- list(
    original         = c(lat,  lon),
    lon_negado       = c(lat, -abs(lon)),
    intercambiado    = c(lon,  lat),
    intercambiado_neg = c(lon, -abs(lat))
  )

  for (metodo in names(candidatos)) {
    cand <- candidatos[[metodo]]
    if (en_rango_colombia(cand[1], cand[2])) {
      return(list(lat = cand[1], lon = cand[2], metodo = metodo))
    }
  }
  list(lat = NA, lon = NA, metodo = "fuera_de_rango_en_las_4_combinaciones")
}

resultado_correccion <- mapply(corregir_fila, gs$latitude, gs$longitude, SIMPLIFY = FALSE)
gs$lat_corregida <- sapply(resultado_correccion, function(x) x$lat)
gs$lon_corregida <- sapply(resultado_correccion, function(x) x$lon)
gs$metodo_correccion <- sapply(resultado_correccion, function(x) x$metodo)

cat("Resultado de validación de coordenadas:\n")
print(table(gs$metodo_correccion))

## =============================================================================
## 2. Punto en poligono contra el shapefile real de municipios
## =============================================================================

municipios_sf <- st_read(RUTA_SHAPEFILE, quiet = TRUE)
if (any(!st_is_valid(municipios_sf))) municipios_sf <- st_make_valid(municipios_sf)

gs_validos <- gs %>% filter(!is.na(lat_corregida) & !is.na(lon_corregida))
puntos <- st_as_sf(gs_validos, coords = c("lon_corregida", "lat_corregida"), crs = st_crs(municipios_sf), remove = FALSE)

union_espacial <- st_join(puntos, municipios_sf, join = st_intersects)

n_asignados <- sum(!is.na(union_espacial$COD_DANE))
cat("\nProyectos con municipio asignado por geometría:", n_asignados, "de", nrow(gs), "\n")

if (n_asignados < nrow(gs_validos)) {
  cat("Aviso:", nrow(gs_validos) - n_asignados, "proyecto(s) con coordenadas válidas pero fuera de cualquier polígono municipal",
      "(posible error residual de coordenadas, o punto justo en una frontera/costa).\n")
}

## =============================================================================
## 3. Guardar resultado consolidado
## =============================================================================

resultado <- union_espacial %>%
  st_drop_geometry() %>%
  select(id, name, latitude, longitude, lat_corregida, lon_corregida, metodo_correccion,
         crediting_period_start_date, crediting_period_end_date, COD_DANE, NOMBRE_MPI)

write_csv(resultado, file.path(DATA_ROOT, "final/goldstandard_con_municipio_reconstruido.csv"))
cat("\nGuardado: goldstandard_con_municipio_reconstruido.csv\n")
print(resultado %>% select(name, COD_DANE, NOMBRE_MPI, metodo_correccion), n = Inf)
