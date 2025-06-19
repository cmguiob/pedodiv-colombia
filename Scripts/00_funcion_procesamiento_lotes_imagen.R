#' procesamiento_lotes_imagen
#'
#' Esta función divide un conjunto de polígonos (objeto `sf`) en lotes que se procesan
#' secuencialmente en Google Earth Engine (GEE), extrayendo estadísticas zonales
#' (media y desviación estándar) a partir de una imagen `ee$Image`.
#'
#' Utiliza una lógica adaptativa para reducir el tamaño del lote si GEE rechaza el envío
#' (ej., por exceder el límite de payload o geometría), y luego lo vuelve a expandir 
#' gradualmente (×1.5) si los lotes pequeños se envían con éxito.
#'
#' @param sf_data Objeto `sf` con geometría POLYGON/MULTIPOLYGON. Debe tener columna `id_creado`.
#' @param image Objeto `ee$Image` con una sola banda seleccionada.
#' @param variable_name Nombre base para exportar (se usará en las tareas).
#' @param scale Resolución (en metros) para el análisis espacial.
#' @param export_folder Nombre de la carpeta en Google Drive para guardar los CSV.
#' @param simplify_tolerance Valor (en grados) para simplificar geometría. Default: 0.001 (~100m).
#' @param start_idx Índice inicial del conjunto de polígonos (por si se desea reanudar). Default: 1.
#' @param max_index Último índice a procesar. Default: nrow(sf_data).
#' @param pause_on_fail Tiempo de espera (segundos) tras un fallo antes de reintentar. Default: 60.
#'
#' @return Un data.frame con el log de procesamiento (rango de índices, tamaño del batch, estado, etc.)

procesamiento_lotes_imagen <- function(sf_data,
                                       image,
                                       batch_s = 400,
                                       reduce_batch_by = 4,
                                       variable_name,
                                       scale,
                                       export_folder = "GEE_exports",
                                       simplify_tolerance = 0.001,
                                       start_idx = 1,
                                       max_index = nrow(sf_data),
                                       pause_on_fail = 20) {
  
  log_list <- list()
  
  # Combina media y desviación estándar
  reducer <- ee$Reducer$mean()$combine(
    reducer2 = ee$Reducer$stdDev(),
    sharedInputs = TRUE
  )
  
  current_idx <- start_idx
  batch_size <- batch_s  # Tamaño máximo permitido
  retry_batch_size <- batch_size  # Se ajusta dinámicamente según éxito/fallo
  
  while (current_idx <= max_index) {
    start_success <- FALSE
    
    while (!start_success && retry_batch_size >= 1) {
      # Se reconstruye el lote completo en cada intento
      end_idx <- min(current_idx + retry_batch_size - 1, nrow(sf_data), max_index)
      batch_range <- current_idx:end_idx
      
      message("────────────────────────────")
      message(sprintf("🔄 Intentando batch %d–%d con retry_batch_size = %d", current_idx, end_idx, retry_batch_size))
      
      batch_sf <- sf_data[batch_range, ]
      batch_sf_simple <- st_simplify(batch_sf, dTolerance = simplify_tolerance)
      
      # Define el rectángulo para hacer clip en la imagen
      bb <- st_bbox(batch_sf_simple)
      ee_bbox <- ee$Geometry$Rectangle(
        coords = list(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]),
        geodesic = FALSE
      )
      image_clip <- image$clip(ee_bbox)
      
      # Convierte a ee$FeatureCollection
      ucs_ee <- tryCatch({
        sf_as_ee(batch_sf_simple)
      }, error = function(e) {
        message("❌ Error en sf_as_ee. Reduciendo tamaño.")
        return(NULL)
      })
      if (is.null(ucs_ee)) {
        retry_batch_size <<- floor(retry_batch_size / reduce_batch_by)
        next
      }
      
      # Reduce regiones sobre la imagen
      result <- tryCatch({
        image_clip$reduceRegions(
          collection = ucs_ee,
          reducer = reducer,
          scale = scale
        )
      }, error = function(e) {
        message("❌ Error en reduceRegions. Reduciendo tamaño.")
        return(NULL)
      })
      if (is.null(result)) {
        retry_batch_size <<- floor(retry_batch_size / reduce_batch_by)
        next
      }
      
      # Define nombre de la tarea y archivo
      desc <- paste0(variable_name, "_", current_idx, "_", end_idx)
      task <- tryCatch({
        ee$batch$Export$table$toDrive(
          collection = result,
          description = desc,
          folder = export_folder,
          fileNamePrefix = desc,
          fileFormat = "CSV"
        )
      }, error = function(e) {
        message("❌ Error creando task. Reduciendo tamaño.")
        return(NULL)
      })
      if (is.null(task)) {
        retry_batch_size <<- floor(retry_batch_size / reduce_batch_by)
        next
      }
      
      # Intenta enviar el lote
      tryCatch({
        task$start()
        start_success <- TRUE
        message(sprintf("✅ Enviado: %d–%d con %d polígonos", current_idx, end_idx, retry_batch_size))
        
        # Registra en el log
        log_list[[length(log_list) + 1]] <- data.frame(
          start_idx = current_idx,
          end_idx = end_idx,
          retry_batch_size = retry_batch_size,
          simplify_tolerance = simplify_tolerance,
          status = "Enviado",
          task_description = desc,
          timestamp = Sys.time(),
          image_name = image$bandNames()$getInfo()[[1]]
        )
        
        current_idx <- end_idx + 1
        
        # ✅ Aumenta tamaño usando crecimiento progresivo x1.5, incluso si era 1
        retry_batch_size <- if (retry_batch_size == 1) 2 else min(floor(retry_batch_size * 1.5), batch_size)
        
      }, error = function(e) {
        message(sprintf("❌ task$start() falló para batch %d–%d. Reduciendo tamaño...", current_idx, end_idx))
        Sys.sleep(pause_on_fail)
        retry_batch_size <<- floor(retry_batch_size / reduce_batch_by)
      })
    }
    
    if (!start_success) {
      message(sprintf("❌ Lote %d–%d falló incluso con 1 polígono. Se registra como fallido.", current_idx, current_idx))
      log_list[[length(log_list) + 1]] <- data.frame(
        start_idx = current_idx,
        end_idx = end_idx,
        retry_batch_size = retry_batch_size,
        simplify_tolerance = simplify_tolerance,
        status = "Falló",
        task_description = NA,
        timestamp = Sys.time(),
        image_name = image$bandNames()$getInfo()[[1]]
      )
      
      current_idx <- end_idx + 1
      retry_batch_size <- batch_size  # Reinicia después de fallo irrecuperable
    }
  }
  
  log_df <- do.call(rbind, log_list)
  return(log_df)
}

