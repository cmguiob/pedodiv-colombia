# ==========================================================================
# Script: carga_ucs_armonizadas
# Insumos: 
# Previamente se subió el archivo a Zenodo. Se obtiene el enlace copiando la 
# url del botón de descarga: https://zenodo.org/records/15391254
# Descripción:
# Este script descarga un archivo .gpkg desde Zenodo, detecta la única capa,
# la carga como objeto `sf`, reporta el CRS y verifica los datos visualmente.
# ==========================================================================


# Cargar paquetes necesarios (de forma segura)
if (!requireNamespace("sf", quietly = TRUE)) stop("El paquete 'sf' es requerido.")
library(sf)

#' Descargar un archivo GeoPackage (.gpkg) desde Zenodo con método 'curl'
#' y reportar su tamaño y nombre de capa
#'
#' param: zenodo_url URL directa con '?download=1' al final
#' return: lista con elementos: `ruta` y `capa`
descargar_gpkg_curl <- function(zenodo_url) {
  # Eliminar parámetros como ?download=1 del nombre del archivo
  nombre_archivo <- sub("\\?.*$", "", basename(zenodo_url))
  ruta_local <- file.path(tempdir(), nombre_archivo)
  
  # Aumentar tiempo de espera para descargas grandes
  options(timeout = max(600, getOption("timeout")))
  
  # Descargar el archivo usando 'curl'
  message("⬇️ Descargando archivo desde Zenodo con método 'curl'...")
  utils::download.file(
    url = zenodo_url,
    destfile = ruta_local,
    mode = "wb",
    method = "curl"
  )
  
  # Reportar tamaño si es posible
  tamaño <- suppressWarnings(file.info(ruta_local)$size)
  if (!is.na(tamaño)) {
    message("📦 Tamaño del archivo descargado: ", round(tamaño / 1e6, 1), " MB")
  } else {
    message("⚠️ No se pudo verificar el tamaño, pero la descarga fue completada.")
  }
  
  # Detectar el nombre de la primera capa disponible
  capa_nombre <- sf::st_layers(ruta_local)$name[1]
  message("📚 Nombre de la capa encontrada: ", capa_nombre)
  
  message("✅ Archivo descargado y guardado en: ", ruta_local)
  
  return(list(
    ruta = ruta_local,
    capa = capa_nombre
  ))
  

}


# ------------------------
# 2. PARÁMETRO DE ENTRADA
# ------------------------

# URL Zenodo con '?download=1'
zenodo_url <- "https://zenodo.org/records/15391254/files/Mapa_Geopedologico_11092014.gpkg?download=1"

# ------------------------
# 3. DESCARGA Y DETECCIÓN DE CAPA
# ------------------------

# Ejecutar la función de descarga
resultado <- descargar_gpkg_curl(zenodo_url)

# Extraer ruta y nombre de capa
ruta_gpkg <- resultado$ruta
nombre_capa <- resultado$capa

# ------------------------
# 4. CARGA DEL OBJETO ESPACIAL
# ------------------------

# Leer el objeto sf
ucs_sf <- sf::st_read(ruta_gpkg, layer = nombre_capa, quiet = TRUE)

# Reportar CRS
crs_obj <- sf::st_crs(ucs_sf)
message("🌐 CRS detectado: ", crs_obj$input, " (EPSG:", crs_obj$epsg, ")")


# ------------------------
# 5. LIMPIA EL ESPACIO
# ------------------------

#Limpia todo menos el producto espacial
rm(list = setdiff(ls(), "ucs_sf"))
