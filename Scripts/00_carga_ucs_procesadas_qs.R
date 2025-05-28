# ==========================================================================
# Script: carga_ucs_armonizadas_qs
# Descripción:
# Descarga un archivo .qs desde Zenodo, lo carga con qs::qread() y reporta el CRS.
# ==========================================================================

# Cargar paquetes necesarios
if (!requireNamespace("qs", quietly = TRUE)) stop("El paquete 'qs' es requerido.")
if (!requireNamespace("sf", quietly = TRUE)) stop("El paquete 'sf' es requerido.")
library(qs)
library(sf)

# ------------------------
# 1. FUNCIÓN DE DESCARGA
# ------------------------

# Descarga archivo .qs desde Zenodo
descargar_qs_curl <- function(zenodo_url) {
  nombre_archivo <- sub("\\?.*$", "", basename(zenodo_url))
  ruta_local <- file.path(tempdir(), nombre_archivo)
  
  options(timeout = max(600, getOption("timeout")))
  
  message("Descargando archivo desde Zenodo con método 'curl'...")
  utils::download.file(
    url = zenodo_url,
    destfile = ruta_local,
    mode = "wb",
    method = "curl"
  )
  
  tamaño <- suppressWarnings(file.info(ruta_local)$size)
  if (!is.na(tamaño)) {
    message("Tamaño del archivo descargado: ", round(tamaño / 1e6, 1), " MB")
  }
  
  return(ruta_local)
}

# ------------------------
# 2. ENLACE DE DESCARGA
# ------------------------

zenodo_url <- "https://zenodo.org/records/15521253/files/OUTPUT_rao_piloto_sf.qs?download=1"

# ------------------------
# 3. DESCARGA Y LECTURA
# ------------------------

ruta_qs <- descargar_qs_curl(zenodo_url)

# Leer el objeto sf desde archivo .qs
ucs_rao_sf <- qs::qread(ruta_qs)

# Verifica si es un objeto sf válido
if (!inherits(ucs_rao_sf, "sf")) stop("El archivo cargado no es un objeto 'sf'.")

# Reportar CRS
crs_obj <- sf::st_crs(ucs_rao_sf)
message("CRS detectado: ", crs_obj$input, " (EPSG:", crs_obj$epsg, ")")

# ------------------------
# 4. LIMPIA EL ESPACIO
# ------------------------

rm(list = setdiff(ls(), "ucs_rao_sf"))
