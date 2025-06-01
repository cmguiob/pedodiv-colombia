# ==========================================================================
# Script: funcion_carga_soilgrids
# Autor: Carlos Guío
# Insumos: 
# El ISRIC pone SoilGrids disponible de forma gratuita, info en los enlaces:
# https://www.isric.org/explore/soilgrids
# https://www.isric.org/explore/soilgrids/soilgrids-access
# Descripción:
# Este script contiene una función para descargar de forma masiva varias 
# capas de SoilGrids, correspondientes a distintas variables, profundidades y
# estadísticas. Descarga por default el área de Colombia como archivo VRT
# ==========================================================================

#' @title Descarga de capas de SoilGrids para un área definida
#' 
#' @description
#' Esta función permite descargar automáticamente capas de propiedades del suelo
#' desde el repositorio oficial de SoilGrids (ISRIC), recortadas a un área definida 
#' y a una resolución espacial específica. Los archivos se acceden directamente 
#' desde el servidor mediante VSI y se devuelven como un objeto `SpatRaster` de `terra`.
#'
#' Las coordenadas de entrada se expresan en metros sobre el sistema de referencia 
#' IGH (proyección homosínclica mundial, EPSG:54052), utilizado por SoilGrids.
#' 
#' @param vars Vector de variables del suelo a descargar. Ejemplos comunes: 
#' `"sand"`, `"silt"`, `"soc"`, `"cec"`, `"phh2o"`. Verifica disponibilidad en: 
#' https://files.isric.org/soilgrids/latest/data/
#'
#' @param depths Vector de profundidades. Valores válidos: 
#' `"0-5cm"`, `"5-15cm"`, `"15-30cm"`, `"30-60cm"`, `"60-100cm"`, `"100-200cm"`.
#'
#' @param stats Vector de estadísticas por variable. Valores válidos: 
#' `"mean"`, `"uncertainty"`, `"Q0.05"`, `"Q0.5"`, `"Q0.95"`.
#'
#' @param resolucion Resolución espacial en metros, como vector numérico de dos elementos: 
#' ancho y alto del píxel. Por ejemplo: `c(250, 250)` para 250 m o `c(1000, 1000)` para 1 km.
#'
#' @param bb Cuadro delimitador (`bounding box`) del área de interés en formato:
#' `c(xmin, ymax, xmax, ymin)`, expresado en el CRS IGH. Por defecto corresponde a Colombia.
#'
#' @return Un objeto `SpatRaster` con todas las capas solicitadas apiladas.
#'
#' @details
#' El CRS utilizado por defecto es IGH (EPSG:54052). Se recomienda reproyectar
#' el resultado a EPSG:4326 o EPSG:9377 para análisis en sistemas geográficos convencionales.
#'
#' @examples
#' \dontrun{
#' stack <- descargar_soilgrids_stack(
#'   vars = c("sand", "phh2o"),
#'   depths = c("0-5cm", "30-60cm"),
#'   stats = c("mean"),
#'   resolucion = c(1000, 1000)
#' )
#' plot(stack)
#' }

descargar_soilgrids_stack <- function(vars = c("sand", "bdod"),
                                      depths = c("0-5cm", "15-30cm"),
                                      stats = c("mean"),
                                      resolucion = c(1000, 1000)) {
  
  
  # -------------------------------------------
  # 0. Carga o instala los paquetes necesarios
  # -------------------------------------------
  if (!"pacman" %in% installed.packages()[, "Package"]) install.packages("pacman")
  pacman::p_load("terra", "gdalUtilities")
  
  # ----------------------------
  # 1. Validación de parámetros
  # ----------------------------
  
  # Lista de variables, produndidades y estadísticas validas en SoilGrids 
  vars_validas <- c("bdod", "cec", "cfvo", "clay", "silt", "sand", 
                    "nitrogen", "ocd", "ocs", "soc", "phh2o")
  depths_validas <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
  stats_validos  <- c("mean", "uncertainty", "Q0.05", "Q0.5", "Q0.95")
  
  # ⚠️ Advertencia si las variables no están en la lista de referencia
  if (!all(vars %in% vars_validas)) {
    warning("Una o más variables no coinciden con la lista de referencia común de SoilGrids.",
            "\nVerifica manualmente su disponibilidad en:",
            "\nhttps://files.isric.org/soilgrids/latest/data/")
  }
  
  if (!all(depths %in% depths_validas)) {
    stop("Profundidades inválidas. Usa: ", paste(depths_validas, collapse = ", "))
  }
  
  if (!all(stats %in% stats_validos)) {
    stop("Valores estadísticos inválidos. Usa: ", paste(stats_validos, collapse = ", "))
  }
  
  
  # --------------------------------------------
  # 2. Define área de recorte y VRT base
  # --------------------------------------------
  
  # Bounding box de Colombia en proyección IGH
  bb <- c(-8800000.000, 1400000.000, -7400000.000, -500000.000)
  igh <- "+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs"
  
  warning("El sistema de referencia de coordenadas (CRS) por defecto es Homosínclico Mundial (EPSG:54052).",
          "\nEl bounding box proporcionado está en metros sobre este CRS.",
          "\nSi se desea trabajar en EPSG:4326 u otro CRS geográfico, reproyectar después con `terra::project()`.")
  
  # URL base de SoilGrids (usando acceso vía /vsicurl)
  sg_url <- "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"
  
  # Lista donde se acumularán los raster
  capas <- list()
  
  for (v in vars) {
    for (d in depths) {
      for (s in stats) {
        
        
        # Construye nombre de capa, ruta remota y ruta temporal local
        capa <- paste0(v, "_", d, "_", s)
        vrt_remote <- paste0(sg_url, v, "/", capa, ".vrt")
        vrt_local  <- file.path(tempdir(), paste0(capa, ".vrt"))
        
        message("Procesando: ", capa)
        
        # ----------------------------
        # 4. Crea archivo VRT recortado
        # ----------------------------
        
        try({
          gdal_translate(
            src_dataset = vrt_remote,
            dst_dataset = vrt_local,
            of = "VRT",
            tr = resolucion,
            projwin = bb,
            projwin_srs = igh
          )
        }, silent = TRUE)
        
        # ----------------------------
        # 5. Lee raster resultante y lo añade al stack
        # ----------------------------
        r <- try(terra::rast(vrt_local), silent = TRUE)
        
        if (!inherits(r, "try-error")) {
          names(r) <- capa
          capas[[capa]] <- r
        } else {
          warning("No se pudo leer la capa: ", capa)
        }
      }
    }
  }
  
  # -------------------------------------
  # 6. Devuelve el stack de rasteres (SpatRaster)
  # -------------------------------------
  if (length(capas) == 0) {
    stop("No se pudo cargar ninguna capa válida.")
  }
  
  return(terra::rast(capas))
}




