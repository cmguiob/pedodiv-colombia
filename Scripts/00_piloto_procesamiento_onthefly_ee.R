#Para trabajar con pocos polígonos, se puede usar lo siguiente, que no permite monitorear el proceso. 
#R envía un ee$Reducer a GEE y recupera al instante un sf con las estadísticas. 
#extrae_cv() combina media, sd y calcula CV en R.


# Define la función para extraer media, desviación y CV por polígono UCS
extrae_cv <- function(img, name, scale) {
  # Extraer media de píxeles dentro de cada UCS
  m <- ee_extract(
    x     = img,
    y     = ucs_ee,
    fun   = ee$Reducer$mean(),
    scale = scale,
    sf    = TRUE
  )
  # Extrae desviación estándar de píxeles dentro de cada UCS
  s <- ee_extract(
    x     = img,
    y     = ucs_ee,
    fun   = ee$Reducer$stdDev(),
    scale = scale,
    sf    = TRUE
  )
  # Renombra columnas de media y sd
  names(m)[ncol(m)] <- paste0(name, "_mean")
  names(s)[ncol(s)] <- paste0(name, "_sd")
  # Unir medias y sd, calcular CV y devolver solo id + cv
  m %>%
    left_join(s, by = "id") %>%
    mutate(!!paste0(name, "_cv") := .data[[paste0(name, "_sd")]] /
             .data[[paste0(name, "_mean")]] ) %>%
    select(id, ends_with("_cv"))
}

# Ejecuta la función para cada covariable con su resolución nativa
ucs_dem_cv <- extrae_cv(dem,   "dem", 30)   # pendiente en grados
ucs_slope_cv <- extrae_cv(slope_deg,   "slope", 30)   # pendiente en grados
ucs_horiz_cv <- extrae_cv(hc_img,      "horiz", 30)   # curvatura horizontal


ucs_vert_cv  <- extrae_cv(vc_img,      "vert",  30)   # curvatura vertical
ucs_lst_cv   <- extrae_cv(lst_img_clipped,  "lst",  100) # LST ~100 m
ucs_vhvv_cv  <- extrae_cv(vhvv_clipped,   "vhvv", 30)  # VH/VV a 30 m

# Combina todos los CV en una sola tabla por id de UCS
library(purrr)
df_cv_all <- list(
  ucs_slope_cv,
  ucs_horiz_cv,
  ucs_vert_cv,
  ucs_lst_cv,
  ucs_vhvv_cv
) %>%
  reduce(left_join, by = "id")

# 4. Verifica resultados: primeras filas y estadísticos básicos
head(df_cv_all)
summary(df_cv_all)