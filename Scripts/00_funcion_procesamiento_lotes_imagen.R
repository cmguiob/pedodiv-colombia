#' procesamiento_lotes_imagen
#'
#' Esta función ejecuta extracción por lotes de estadísticas zonales (media y desviación estándar)
#' desde una imagen de Google Earth Engine (GEE) sobre una colección de polígonos (`sf`).
#'
#' Dado que GEE impone restricciones en el tamaño de la geometría enviada (payload), el procesamiento
#' se realiza por lotes adaptativos que se reducen dinámicamente si fallan, y se expanden si tienen éxito.
#'
#' Cada lote se simplifica geométricamente (tolerancia ajustable), y se validan valores numéricos
#' para evitar errores de codificación JSON como NaN o Inf.
#'
#' @param sf_data Objeto `sf` con geometría POLYGON o MULTIPOLYGON y atributos auxiliares.
#' @param image Objeto `ee$Image` con una sola banda seleccionada y renombrada.
#' @param batch_s Tamaño inicial del lote (en número de polígonos). Default: 400.
#' @param reduce_batch_by Factor por el cual reducir el tamaño del lote tras fallar. Default: 4.
#' @param variable_name Nombre de la variable, se usará para nombrar los archivos exportados.
#' @param scale Resolución en metros para aplicar reduceRegions().
#' @param export_folder Carpeta en Google Drive donde se exportarán los resultados. Default: "GEE_exports".
#' @param simplify_tolerance Tolerancia de simplificación para geometrías, en grados. Default: 0.001 (~100m).
#' @param start_idx Índice de inicio para comenzar a procesar (permite reanudar). Default: 1.
#' @param max_index Último índice a procesar. Default: `nrow(sf_data)`.
#' @param pause_on_fail Tiempo de espera en segundos después de un fallo. Default: 20.
#'
#' @return Data frame con el log de procesamiento: índices, tamaño del batch, estado y descripción.

procesamiento_lotes_imagen <- function(sf_data,
                                       image,
                                       batch_s = 400,
                                       reduce_batch_by = 4,
                                       variable_name,
                                       scale,
                                       export_folder = "GEE_exports",
                                       simplify_tolerance = 0.001,
                                       start_idx = 1,
                                       export_log_csv = TRUE,
                                       max_index = nrow(sf_data),
                                       pause_on_fail = 20) {
  
  log_list <- list()  # Registro acumulado de lotes procesados
  
  # Combinador de reducción: media + desviación estándar
  reducer <- ee$Reducer$mean()$combine(
    reducer2 = ee$Reducer$stdDev(),
    sharedInputs = TRUE
  )
  
  current_idx <- start_idx
  batch_size <- batch_s # Tamaño máximo permitido
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
      
      # Diagnóstico: estima el tamaño del GeoJSON
      geojson_text <- sf_geojson(batch_sf_simple, simplify = FALSE)
      size_mb <- nchar(geojson_text, type = "bytes") / (1024^2)
      message(glue::glue("📦 Payload estimado: {round(size_mb, 2)} MB (GeoJSON crudo)"))
      
      #Reemplaza NaN/Inf por NA para evitar errores de codificación JSON en EE
      batch_sf_simple <- batch_sf_simple |>
        mutate(across(
          where(is.numeric),
          ~ ifelse(is.nan(.) | is.infinite(.), NA, .)
        ))
      
      # Definir bounding box para hacer clip de la imagen
      bb <- st_bbox(batch_sf_simple)
      ee_bbox <- ee$Geometry$Rectangle(
        coords = list(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]),
        geodesic = FALSE
      )
      image_clip <- image$clip(ee_bbox)
      
      # Convertir a ee$FeatureCollection
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
      
      # Aplicar reduceRegions con media y desviación estándar
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
      
      # Crear tarea de exportación a Google Drive
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
      
      # Intenta iniciar la tarea y captura errores reales de GEE
      tryCatch({
        task$start()
        start_success <- TRUE
        message(sprintf("✅ Enviado: %d–%d con %d polígonos", current_idx, end_idx, retry_batch_size))
        
        # Registro en log
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
        
        # Avanza al siguiente lote
        current_idx <- end_idx + 1
        
        # Si se envió correctamente, aumenta el tamaño del batch x1.5, incluso si era 1 (progresivamente hasta batch_size)
        retry_batch_size <- if (retry_batch_size == 1) 2 else min(floor(retry_batch_size * 1.5), batch_size)
        
      }, error = function(e) {
        message(sprintf("❌ task$start() falló para batch %d–%d.", current_idx, end_idx))
        message("ℹ️  Detalle del error:", e$message)
        Sys.sleep(pause_on_fail)
        retry_batch_size <<- floor(retry_batch_size / reduce_batch_by)
      })
    }
    
    # Si falla incluso con un solo polígono, registra el fallo y avanza
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
      retry_batch_size <- batch_size
    }
  }
  
  # Consolidar log
  log_df <- do.call(rbind, log_list)
  
  # Exportar como CSV a Google Drive (opcional)
  if (export_log_csv) {
    nombre_log <- paste0("LOG_", variable_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    ruta_temporal <- file.path(tempdir(), nombre_log)
    readr::write_csv(log_df, ruta_temporal)
    
    googledrive::drive_upload(
      media = ruta_temporal,
      path = googledrive::as_id(export_folder),
      name = nombre_log,
      overwrite = TRUE
    )
    
    message(glue::glue("📤 Log exportado como '{nombre_log}' en carpeta '{export_folder}'"))
  }
  
  return(log_df)
}