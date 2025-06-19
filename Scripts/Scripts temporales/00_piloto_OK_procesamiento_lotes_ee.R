procesamiento_lotes_imagen <- function(sf_data,
                                       image,
                                       variable_name,
                                       scale,
                                       export_folder = "GEE_exports",
                                       simplify_tolerance = 0.001,
                                       max_payload_mb = 9.9,  # ya no usado
                                       start_idx = 1,
                                       max_index = nrow(sf_data),
                                       pause_on_fail = 60) {
  
  log_list <- list()
  reducer <- ee$Reducer$mean()$combine(
    reducer2 = ee$Reducer$stdDev(),
    sharedInputs = TRUE
  )
  
  current_idx <- start_idx
  batch_size <- 300
  retry_batch_size <- batch_size
  
  while (current_idx <= max_index) {
    start_success <- FALSE
    
    while (!start_success && retry_batch_size >= 1) {
      end_idx <- min(current_idx + retry_batch_size - 1, nrow(sf_data), max_index)
      batch_range <- current_idx:end_idx
      
      message("────────────────────────────")
      message(sprintf("🔄 Intentando batch %d–%d con retry_batch_size = %d", current_idx, end_idx, retry_batch_size))
      
      batch_sf <- sf_data[batch_range, ]
      batch_sf_simple <- st_simplify(batch_sf, dTolerance = simplify_tolerance)
      
      bb <- st_bbox(batch_sf_simple)
      ee_bbox <- ee$Geometry$Rectangle(
        coords = list(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]),
        geodesic = FALSE
      )
      image_clip <- image$clip(ee_bbox)
      
      ucs_ee <- tryCatch({
        sf_as_ee(batch_sf_simple)
      }, error = function(e) {
        message("❌ Error en sf_as_ee. Reduciendo tamaño.")
        return(NULL)
      })
      
      if (is.null(ucs_ee)) {
        retry_batch_size <<- floor(retry_batch_size / 2)
        next
      }
      
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
        retry_batch_size <<- floor(retry_batch_size / 2)
        next
      }
      
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
        retry_batch_size <<- floor(retry_batch_size / 2)
        next
      }
      
      tryCatch({
        task$start()
        start_success <- TRUE
        message(sprintf("✅ Enviado: %d–%d con %d polígonos", current_idx, end_idx, retry_batch_size))
        
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
        
        # ✅ crecimiento progresivo x1.5 tras éxito
        retry_batch_size <- min(floor(retry_batch_size * 1.5), batch_size)
        
      }, error = function(e) {
        message(sprintf("❌ task$start() falló para batch %d–%d. Reduciendo tamaño...", current_idx, end_idx))
        Sys.sleep(pause_on_fail)
        retry_batch_size <<- floor(retry_batch_size / 2)
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
      retry_batch_size <- batch_size  # reinicia tras fallo irrecuperable
    }
  }
  
  log_df <- do.call(rbind, log_list)
  return(log_df)
}
