corregir_porcentajes <- function(p_string, n_taxones) {
  # Si el string está vacío, es NA o contiene "No aplica" (sin importar mayúsculas)
  # asigna porcentajes iguales entre todos los taxones
  if (is.na(p_string) || p_string == "" || str_detect(p_string, regex("no aplica", ignore_case = TRUE))) {
    p_num <- rep(100 / n_taxones, n_taxones)
    es_asignacion_igual <- TRUE
  } else {
    # Separa el string por comas y limpia espacios
    p <- str_trim(str_split(p_string, ",\\s*")[[1]])
    # Convierte a numérico suprimiendo advertencias (por textos no numéricos)
    p_num <- suppressWarnings(as.numeric(p))
    
    # Si todos los valores son NA después de convertir, asigna valores iguales
    if (all(is.na(p_num))) {
      p_num <- rep(100 / n_taxones, n_taxones)
      es_asignacion_igual <- TRUE
    } else {
      es_asignacion_igual <- FALSE
      
      # Si hay un porcentaje de más, elimina el último
      if (length(p_num) == n_taxones + 1) {
        p_num <- p_num[1:n_taxones]
        # Si hay uno de menos, calcula el faltante para completar 100 y lo agrega
      } else if (length(p_num) == n_taxones - 1) {
        faltante <- 100 - sum(p_num, na.rm = TRUE)
        p_num <- c(p_num, faltante)
      }
      
      # Si aún no coincide la longitud, devuelve NAs y advierte
      if (length(p_num) != n_taxones) {
        warning("Número de porcentajes no coincide con número de taxones")
        return(list(porcentajes = rep(NA_real_, n_taxones), es_asignacion_igual = es_asignacion_igual))
      }
      
      # Escala los porcentajes para que sumen exactamente 100 (preserva proporciones)
      suma_total <- sum(p_num, na.rm = TRUE)
      if (!is.na(suma_total) && suma_total != 100 && suma_total > 0) {
        p_num <- 100 * p_num / suma_total
      }
    }
  }
  
  # Devuelve lista con los porcentajes corregidos y flag de asignación automática
  return(list(porcentajes = p_num, es_asignacion_igual = es_asignacion_igual))
}
