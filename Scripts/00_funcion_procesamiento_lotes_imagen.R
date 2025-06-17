#' @title Procesamiento por lotes de polígonos en Google Earth Engine
#'
#' @description
#' Extrae estadísticas zonales (media y desviación estándar) sobre una imagen de GEE,
#' dividiendo un conjunto de polígonos (`sf`) en lotes para respetar el límite de payload (~10MB).
#' Exporta los resultados como archivos CSV a Google Drive.
#'
#' @param sf_data Objeto `sf` con geometría tipo POLYGON/MULTIPOLYGON. Debe incluir `id_creado`, `UCSuelo` y `AREA_HA`.
#' @param image Objeto `ee$Image` con una sola banda ya seleccionada y renombrada.
#' @param variable_name Nombre base para los archivos exportados (ej: "slope", "lst").
#' @param scale Resolución espacial (en metros) para la reducción zonal (ej: 30, 50).
#' @param export_folder Carpeta en Google Drive para guardar los CSV. Default: "GEE_exports".
#' @param simplify_tolerance Tolerancia para simplificar geometrías (en grados). Default: 0.001 (~100 m).
#' @param max_payload_mb Tamaño máximo del lote (en MB). Default: 9.9 (límite práctico de GEE).
#' @param start_idx Índice inicial de procesamiento. Útil para reanudar. Default: 1.
#' @param max_index Índice máximo de procesamiento. Default: `nrow(sf_data)`.
#' @param pause_on_fail Tiempo de espera (en segundos) tras un fallo. Default: 60.
#'
#' @return `data.frame` con el log del procesamiento: índices, tamaño, estado, nombre de tarea, timestamp, banda procesada.

procesamiento_lotes_imagen <- function(sf_data,
                                       image,
                                       variable_name,
                                       scale,
                                       export_folder = "GEE_exports",
                                       simplify_tolerance = 0.001,
                                       max_payload_mb = 9.9,
                                       start_idx = 1,
                                       max_index = nrow(sf_data),
                                       pause_on_fail = 60) {
  
  log_list <- list()
  
  reducer <- ee$Reducer$mean()$combine(
    reducer2 = ee$Reducer$stdDev(),
    sharedInputs = TRUE
  )
  
  current_idx <- start_idx
  
  while (current_idx <= max_index) {
    batch_size <- 300
    retry_batch_size <- batch_size
    start_success <- FALSE
    
    while (!start_success && retry_batch_size >= 1) {
      end_idx <- min(current_idx + retry_batch_size - 1, nrow(sf_data), max_index)
      batch_range <- current_idx:end_idx
      
      batch_sf <- sf_data[batch_range, ]
      batch_sf_simple <- st_simplify(batch_sf, dTolerance = simplify_tolerance)
      
      geojson_text <- sf_geojson(batch_sf_simple, simplify = FALSE)
      size_mb <- nchar(geojson_text, type = "bytes") / (1024^2)
      
      if (size_mb <= max_payload_mb) {
        bb <- st_bbox(batch_sf_simple)
        ee_bbox <- ee$Geometry$Rectangle(
          coords = list(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]),
          geodesic = FALSE
        )
        image_clip <- image$clip(ee_bbox)
        ucs_ee <- sf_as_ee(batch_sf_simple)
        result <- image_clip$reduceRegions(
          collection = ucs_ee,
          reducer = reducer,
          scale = scale
        )
        desc <- paste0(variable_name, "_", current_idx, "_", end_idx)
        task <- ee$batch$Export$table$toDrive(
          collection = result,
          description = desc,
          folder = export_folder,
          fileNamePrefix = desc,
          fileFormat = "CSV"
        )
        
        tryCatch({
          # Intenta iniciar la tarea de exportación a Google Drive
          task$start()
          
          # Si tiene éxito, marca el batch como exitoso
          start_success <- TRUE
          message(sprintf("✅ Enviando batch: %d–%d (%.2f MB)", current_idx, end_idx, size_mb))
          
          # Guarda información del batch en el log
          log_list[[length(log_list) + 1]] <- data.frame(
            start_idx = current_idx,
            end_idx = end_idx,
            payload_mb = round(size_mb, 2),
            simplify_tolerance = simplify_tolerance,
            status = "Enviado",
            task_description = desc,
            timestamp = Sys.time(),
            image_name = image$bandNames()$getInfo()[[1]]
          )
          
          # Avanza al siguiente índice después de un envío exitoso
          current_idx <- end_idx + 1
        }, error = function(e) {
          # Si ocurre un error al iniciar el task, se captura aquí
          
          message(sprintf("❌ Error inesperado al enviar batch: %d–%d (%.2f MB). Intentando con menos polígonos...", current_idx, end_idx, size_mb))
          
          # Pausa antes de volver a intentar, para no saturar GEE
          Sys.sleep(pause_on_fail)
          
          # Reduce el tamaño del batch a la mitad para reintentar con un grupo más pequeño
          retry_batch_size <- floor(retry_batch_size / 2)
          
          # NOTA: no se usa 'break', lo que permite seguir en el bucle con batch más chico
        })
        
        
      } else {
        retry_batch_size <- floor(retry_batch_size / 2)
        if (retry_batch_size < 1) break
      }
    }
    
    if (!start_success) {
      message(sprintf("❌ Batch desde %d supera límite %.1fMB incluso con 1 polígono. Saltando.", current_idx, max_payload_mb))
      log_list[[length(log_list) + 1]] <- data.frame(
        start_idx = current_idx,
        end_idx = end_idx,
        payload_mb = round(size_mb, 2),
        simplify_tolerance = simplify_tolerance,
        status = "Falló",
        task_description = NA,
        timestamp = Sys.time(),
        image_name = image$bandNames()$getInfo()[[1]]
      )
      Sys.sleep(pause_on_fail)
      current_idx <- end_idx + 1
    }
  }
  
  log_df <- do.call(rbind, log_list)
  return(log_df)
}
