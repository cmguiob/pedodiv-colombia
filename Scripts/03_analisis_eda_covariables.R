## ----configuracion-------------------------------------------------------------------------------------------

#Para exportar como .R plano
# knitr::purl('05_analisis_glm_hotspots.qmd')

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(
  here,        # rutas del proyecto
  remotes,     # instalar desde GitHub
  sf,          # vectores espaciales
  geojsonio,   # GeoJSON ↔ objetos R
  geojsonsf,   # GeoJSON ↔ sf rápido
  dplyr,       # manipulación tabular
  tidyr,
  purrr,       # funciones map*
  broom,
  readr,       # leer CSV rápido
  ggdist,
  scales,
  ggplot2,     # gráficos
  ggpubr,
  patchwork,   # unir gráficos
  wesanderson, # paletas de color
  qs           # guardar objetos rápido
)

#Paleta de colores
pal <- wes_palette("Zissou1", 100, type = "continuous")

# Ajusta tamaño de letra para todo e lscript
theme(base_size = 14)

# Selección entorno ya existente antes de cualquier llamado que use Python
reticulate::use_condaenv("rgee_py", required = TRUE)

## Librerías que usan Python 
library(reticulate)
library(rgee)
library(googledrive)

# ==== Autenticación y backend Python ====
ee_clean_user_credentials()      # Limpia credenciales de GEE
ee_clean_pyenv()           # Limpia variables de entorno de reticulate
reticulate::py_run_string("import ee; ee.Authenticate()")
#reticulate::py_run_string("import ee; ee.Initialize(project='even-electron-461718-g2')") #cuenta gmail propia
reticulate::py_run_string("import ee; ee.Initialize(project='optimal-signer-459113-i1')") #cuenta UNAL

# === Autenticación Google Drive ===
googledrive::drive_auth()


## ----verifica_configuracion----------------------------------------------------------------------------------

# Se consultan datos de DEM
img <- ee$Image("USGS/SRTMGL1_003")

#Consulta que propiedades están disponibles
img$propertyNames()$getInfo()

# Consultar una propiedad específica, e.g. keywords
img$get("keywords")$getInfo()


## ----carga_pedodiversidad------------------------------------------------------------------------------------

# Corre script externo para cargar
source(here::here("Scripts", "00_funcion_carga_ucs_procesadas_qs.R"), encoding = "UTF-8")

# Asignación de id único para cada polígono
ucs_rao_sf <- ucs_rao_sf |> 
  sf::st_make_valid() |> #valida geometrias problemáticas
  dplyr::select(id_creado, UCSuelo, AREA_HA, Q) 

ucs_rao_sf <- ucs_rao_sf[!st_is_empty(ucs_rao_sf), ]

#Se verifica visulalmente
ggplot(data = ucs_rao_sf) +
  geom_sf(aes(fill = UCSuelo), color = NA) +  
  theme_void() +                             
  theme(legend.position = "none") 


## ----transforma_crs------------------------------------------------------------------------------------------

# Transforma a crs 4326 antes de pasarlo a GEE
ucs_sf_4326 <- st_transform(ucs_rao_sf, 4326)


# #Extrae bounding boxdel área del subconjunto
bb_sf_4326 <- st_bbox(ucs_sf_4326)



## ----tansform_ee---------------------------------------------------------------------------------------------

# Convertir a rectángulo de Earth Engine
bbox_ee <- ee$Geometry$Rectangle(
  coords = list(
    bb_sf_4326["xmin"], 
    bb_sf_4326["ymin"], 
    bb_sf_4326["xmax"], 
    bb_sf_4326["ymax"]
    ),
  geodesic = FALSE
)



## ----extraccion_dem------------------------------------------------------------------------------------------

# Carga y suavizado del DEM SRTM 30 m
dem_clip <- ee$Image("USGS/SRTMGL1_003")$clip(bbox_ee)

# Visualiza en el visor antes de exportar (verifica recorte)
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  dem_clip,
  visParams = list(min = 0, max = 3000,
                   palette = viridis::viridis(10)), 
  name = "DEM SRTM (nativo 30m)"
  )



## ----extraccion_pendiente------------------------------------------------------------------------------------

# Procesamiento de pendiente (slope) a partir del SRTM
slope_clip <- ee$Terrain$
  slope(ee$Image("USGS/SRTMGL1_003"))$
  clip(bbox_ee)$
  rename("slope_deg")  # valores ya están en grados

# Visualiza en el visor antes de exportar (verifica recorte)
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  slope_clip,
  visParams = list(min = 0, max = 50,
                   palette = viridis::viridis(10)), 
  name = "Pendiente SRTM (nativo 30m)"
  )


## ------------------------------------------------------------------------------------------------------------

# Bloque único de setup (solo la primera vez)
if (!py_module_available("tagee")) {
  py_install(c("tagee","ee_extra","regex","jsbeautifier"),
             envname="rgee_py", pip=TRUE)
}
tagee <- import("tagee",    convert = FALSE)
eeextra <- import("ee_extra", convert = FALSE)

reticulate::py_run_string("import ee; ee.Initialize(project='even-electron-461718-g2')")

# Ejecuta el análisis de terreno (devuelve un ee$Image con múltiples bandas)
dem_attr <- tagee$terrainAnalysis(dem)

# Extrae la banda de Curvatura Vertical
vc_clip <- dem_attr$select("VerticalCurvature")$clip(bbox_ee)


Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  vc_clip,
  visParams = list(
    min     = -0.00005,
    max     = +0.00005,
    palette = viridis::viridis(5)
  ),
  name = "Curvatura vertical (±0.00005)"
)



## ------------------------------------------------------------------------------------------------------------

# Temperatura superficial (LST)
lst_media <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
  filterBounds(bbox_ee)$
  filterDate("2013-01-01", "2023-01-01")$
  map(function(img) {
    img$select("ST_B10")$multiply(0.00341802)$add(149)
  })$
  median()$
  rename("lst")

# Visualización
Map$setCenter(-74, 4, 5)
Map$addLayer(
  lst_media,
  visParams = list(min = 270, max = 320, palette = c("blue","white","red")),
  name  = "LST mediana"
)



## ------------------------------------------------------------------------------------------------------------

vhvv_media <- ee$ImageCollection("COPERNICUS/S1_GRD")$
  filterBounds(bbox_ee)$
  filterDate("2015-01-01", "2024-01-01")$
  filter(ee$Filter$listContains("transmitterReceiverPolarisation", "VV"))$
  filter(ee$Filter$listContains("transmitterReceiverPolarisation", "VH"))$
  map(function(img) {
    img$select("VH")$
      divide(img$select("VV"))$
      rename("vhvv")
  })$
  median()$
  rename("vhvv")

# Visualización en el visor de rgee
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  vhvv_media,
  visParams = list(min = 0, max = 2, palette = c("brown", "white", "blue")),
  name = "VH/VV mediana"
)



## ------------------------------------------------------------------------------------------------------------

source(here::here("Scripts", "00_funcion_procesamiento_lotes_imagen.R"), encoding = "UTF-8")

registro_dem <- procesamiento_lotes_imagen(ucs_sf_4326, image = dem_clip, start_idx = 1, max_index = 43384, variable_name = "DEM", scale = 50)

registro_slope <- procesamiento_lotes_imagen(ucs_sf_4326, image = slope_clip, start_idx = 1, max_index = 43384, batch_s = 300, reduce_batch_by = 2, variable_name = "SLOPE", scale = 50)

registro_lst <- procesamiento_lotes_imagen(ucs_sf_4326, image = lst_media, start_idx = 531, max_index = 800, batch_s = 200, reduce_batch_by = 3, variable_name = "LST_media", scale = 50)

registro_vvvh <- procesamiento_lotes_imagen(ucs_sf_4326, image = vhvv_media, start_idx = 1, max_index = 43384, batch_s = 300, reduce_batch_by = 3, variable_name = "VHVV_media", scale = 50)





## ------------------------------------------------------------------------------------------------------------

verificar_tasks_enviados <- function(log_df) { library(dplyr) library(rgee)

# Extraer todas las tareas activas/pendientes/finalizadas tasks <- ee_manage_task_list() |> as.data.frame() |> arrange(description, state, update_time) |> group_by(description) |> slice_tail(n = 1) |> # conserva solo la última tarea por nombre ungroup() |> select(description, state)

# Unir con el log original según descripción log_df_verificado <- log_df |> left_join(tasks, by = c("task_description" = "description")) |> rename(status_real = state)

return(log_df_verificado) }


## ----reubicar_googledrive------------------------------------------------------------------------------------

combinar_y_subir_csv <- function(propiedad,
                                 carpeta_drive_id_origen = "17yxwhlpgL4EG8inI5u8Nwi08wOrnhJiM",  # GEE_exports
                                 carpeta_drive_id_destino = "1qJ5S25TZaFWzueNhx1M4Gr3P8JYWKGeU",  # Proyecto
                                 carpeta_temporal = "tmp_csv") {

  # Crea carpeta temporal local si no existe
  if (!dir.exists(carpeta_temporal)) {
    dir.create(carpeta_temporal)
  }

  # Listar archivos en Google Drive (solo .csv con prefijo exacto)
  archivos_drive <- googledrive::drive_ls(
    path = as_id(carpeta_drive_id_origen),
    pattern = glue::glue("^{propiedad}_.*\\.csv$")
  ) |>
    dplyr::filter(stringr::str_ends(name, ".csv"))

  if (nrow(archivos_drive) == 0) {
    stop(glue::glue("No se encontraron archivos CSV para la propiedad '{propiedad}' en GEE_exports."))
  }

  message(glue::glue("📥 Descargando {nrow(archivos_drive)} archivos CSV para '{propiedad}'..."))

  # Descargar archivos al directorio temporal
  purrr::walk2(
    archivos_drive$name,
    archivos_drive$id,
    ~ googledrive::drive_download(
      file = as_id(.y),
      path = file.path(carpeta_temporal, .x),
      overwrite = TRUE
    )
  )

  # Leer y combinar
  archivos_locales <- list.files(path = carpeta_temporal,
                                 pattern = paste0("^", propiedad, "_.*\\.csv$"),
                                 full.names = TRUE)

  combinado <- purrr::map_dfr(archivos_locales, readr::read_csv, show_col_types = FALSE)

  # Escribir archivo combinado
  nombre_salida <- paste0("OUT_", propiedad, "_combinado.csv")
  ruta_salida <- file.path(carpeta_temporal, nombre_salida)
  readr::write_csv(combinado, ruta_salida)

  # Subir a carpeta final de proyecto en Drive
  archivo_subido <- googledrive::drive_upload(
    media = ruta_salida,
    path = as_id(carpeta_drive_id_destino),
    name = nombre_salida,
    overwrite = TRUE
  )

  message(glue::glue("✅ Archivo combinado subido: {archivo_subido$name} (ID: {archivo_subido$id})"))

  # Limpieza automática
  unlink(carpeta_temporal, recursive = TRUE)
  message("🧹 Archivos temporales eliminados.")
}



## ------------------------------------------------------------------------------------------------------------

combinar_y_subir_csv("DEM")

combinar_y_subir_csv("SLOPE")




## ------------------------------------------------------------------------------------------------------------


# Ruta al CSV combinado
dem_cv <- read_csv(here::here("Data", "OUT_covars_csv","OUT_DEM_combinado.csv" ))

# Convierte geometría desde .geo (GeoJSON como texto) a objeto sf
dem_cv_sf <- st_as_sf(
  data.frame(dem_cv, geometry = geojson_sf(dem_cv$.geo)), #convierte a sf
  crs = 4326) |>
  select(-.geo) |> #elimina columna de geometria obsoleta
  mutate(
      dem_cv = stdDev / mean, #calcula coeficiente de variación
      log_dem_cv = log(dem_cv + 1), #calcula log de coeficiente de variación
      dem_cv_dens = dem_cv/AREA_HA, #calcula la densidad del coeficiente de variación
      log_dem_cv_dens = log(dem_cv_dens + 1) 
  ) |>
rename(dem_mean = mean, dem_stdDev = stdDev)


# Ruta al CSV combinado
slope_cv <- read_csv(here::here("Data", "OUT_covars_csv","OUT_SLOPE_combinado.csv" ))

# Convierte geometría desde .geo (GeoJSON como texto) a objeto sf
slope_cv_sf <- st_as_sf(
  data.frame(slope_cv, geometry = geojson_sf(slope_cv$.geo)), #convierte a sf
  crs = 4326) |>
  select(-.geo) |> #elimina columna de geometria obsoleta
  mutate(mean = mean / (180 / pi), #correccion temporal, mientras se descarga lote
         stdDev = stdDev / (180 / pi)) |>
  mutate(
      slope_cv = stdDev / mean, #calcula coeficiente de variación
      log_slope_cv = log(slope_cv + 1), #calcula log de coeficiente de variación
      slope_cv_dens = slope_cv/AREA_HA, #calcula la densidad del coeficiente de variación
      log_slope_cv_dens = log(slope_cv_dens + 1) 
  ) |>
rename(slope_mean = mean, slope_stdDev = stdDev)




## ------------------------------------------------------------------------------------------------------------

# Extraer solo columnas útiles de dem y slope
dem_df <- dem_cv_sf |> 
  st_drop_geometry() |>
  select(-system.index)

slope_df <- slope_cv_sf |> 
  st_drop_geometry() |>
  select(-system.index)

# Unir a la tabla base
modelo_df <- ucs_rao_sf |>
  mutate(
    #Q = if_else(Q == 0, 1e-3, Q),               # evitar log(0)
    Qdens = Q / AREA_HA,                        # diversidad por unidad de área
    log_Qdens = log(Qdens + 1),                     # log-transformación
    log_Qdens01 = (log_Qdens - min(log_Qdens)) / 
                  (max(log_Qdens) - min(log_Qdens)) # escalado entre 0–1
  ) |>
  left_join(
    dem_df,
    by = c("id_creado", "AREA_HA", "UCSuelo")
  ) |>
  left_join(
    slope_df,
    by = c("id_creado", "AREA_HA", "UCSuelo")
  ) 



## ------------------------------------------------------------------------------------------------------------
p_slope_mean <- ggplot(modelo_df, aes(slope_mean)) +
  ggdist::stat_histinterval(fill = "#56B4E9", color = "black", .width = c(0.5, 0.9)) +
  geom_density(aes(y = after_stat(scaled)), color = "#FFD700", size = 0.8) +
  labs(title = "Distribución de media de SLOPE", y = "Frecuencia", x = "Media de DEM")

p_slope_mean


p_log_slope_cv <- ggplot(modelo_df, aes(log_slope_cv)) +
  ggdist::stat_histinterval(fill = "#56B4E9", color = "black", .width = c(0.5, 0.9)) +
  geom_density(aes(y = after_stat(scaled)), color = "#FFD700", size = 0.8) +
  labs(title = "Distribución de log CV de SLOPE", y = "Frecuencia", x = "Log CV de SLOPE")

p_log_slope_cv

p_log_slope_cv_dens <- ggplot(modelo_df, aes(log_slope_cv_dens)) +
  ggdist::stat_histinterval(fill = "#56B4E9", color = "black", .width = c(0.5, 0.9)) +
  geom_density(aes(y = after_stat(scaled)), color = "#FFD700", size = 0.8) +
  labs(title = "Distribución de log densidad CV de SLOPE", y = "Frecuencia", x = "Log densidad de CV de SLOPE")

p_log_slope_cv_dens

p_mosaico_distr_slope <- p_slope_mean + p_log_slope_cv + p_log_slope_cv_dens + plot_layout(ncol = 3, widths = c(1, 1))


## ------------------------------------------------------------------------------------------------------------
p_dem_mean <- ggplot(modelo_df, aes(dem_mean)) +
  ggdist::stat_histinterval(fill = "#56B4E9", color = "black", .width = c(0.5, 0.9)) +
  geom_density(aes(y = after_stat(scaled)), color = "#FFD700", size = 0.8) +
  labs(title = "Distribución de media de DEM", y = "Frecuencia", x = "Media de DEM")

p_dem_mean


p_log_dem_cv <- ggplot(modelo_df, aes(log_dem_cv)) +
  ggdist::stat_histinterval(fill = "#56B4E9", color = "black", .width = c(0.5, 0.9)) +
  geom_density(aes(y = after_stat(scaled)), color = "#FFD700", size = 0.8) +
  labs(title = "Distribución de log CV de DEM", y = "Frecuencia", x = "Log media de DEM")

p_log_dem_cv

p_log_dem_cv_dens <- ggplot(modelo_df, aes(log_dem_cv_dens)) +
  ggdist::stat_histinterval(fill = "#56B4E9", color = "black", .width = c(0.5, 0.9)) +
  geom_density(aes(y = after_stat(scaled)), color = "#FFD700", size = 0.8) +
  labs(title = "Distribución de log densidad CV de DEM", y = "Frecuencia", x = "Log media de DEM")

p_log_dem_cv_dens

p_mosaico_distr_DEM <- p_dem_mean + p_log_dem_cv + p_log_dem_cv_dens + plot_layout(ncol = 3, widths = c(1, 1))


## ------------------------------------------------------------------------------------------------------------
p_mapa_dem_mean <- ggplot(modelo_df |> filter(dem_mean >=0)) +
  geom_sf(aes(fill = dem_mean), color = NA) +
  scale_fill_gradientn(colours = pal, na.value = "white") + 
  labs(title = "Mapa de media de DEM", fill = "Media") +
  theme_minimal()

p_mapa_dem_mean

p_mapa_slope_mean <- ggplot(modelo_df) +
  geom_sf(aes(fill = slope_mean), color = NA) +
  scale_fill_gradientn(colours = pal, na.value = "white") + 
  labs(title = "Mapa de media de SLOPE", fill = "Media") +
  theme_minimal()

p_mapa_slope_mean

p_mapa_log_dem_cv <- ggplot(modelo_df |> filter(log_dem_cv >=0)) +
  geom_sf(aes(fill = dem_cv), color = NA) +
  scale_fill_gradientn(
    colours = pal,
    na.value = "white",
    trans = "log10",
    breaks = scales::trans_breaks("log10", function(x) 10^x)
  ) +
  labs(title = "Mapa de CV de DEM - escalado log", fill = "CV") +
  theme_minimal()

p_mapa_log_dem_cv

p_mapa_log_slope_cv <- ggplot(modelo_df) +
  geom_sf(aes(fill = slope_cv), color = NA) +
  scale_fill_gradientn(
    colours = pal,
    na.value = "white",
    trans = "log10",
    breaks = scales::trans_breaks("log10", function(x) 10^x)
  ) +
  labs(title = "Mapa de log(CV de SLOPE)", fill = "CV") +
  theme_minimal()

p_mapa_log_slope_cv

p_mapa_log_dem_cv_dens <- ggplot(modelo_df) +
  geom_sf(aes(fill = dem_cv_dens), color = NA) +
  scale_fill_gradientn(
    colours = pal,
    na.value = "white",
    trans = "log10",
    breaks = scales::trans_breaks("log10", function(x) 10^x)
  ) +
  labs(title = "Mapa de log(densidad de CV de DEM)", fill = "Densidad CV") +
  theme_minimal()

p_mapa_log_dem_cv_dens

p_mapa_log_slope_cv_dens <- ggplot(modelo_df) +
  geom_sf(aes(fill = slope_cv_dens), color = NA) +
  scale_fill_gradientn(
    colours = pal,
    na.value = "white",
    trans = "log10",
    breaks = scales::trans_breaks("log10", function(x) 10^x)
  ) +
  labs(title = "Mapa de log(densidad de CV de SLOPE)", fill = "Densidad CV") +
  theme_minimal()

p_mapa_log_slope_cv_dens

p_mapas_covs <- p_mapa_dem_mean + p_mapa_slope_mean + p_mapa_log_dem_cv + p_mapa_log_slope_cv + p_mapa_log_dem_cv_dens + p_mapa_log_slope_cv_dens+ plot_layout(ncol = 2, widths = c(1, 1))


#guarda el último gráfico generado
ggsave(here("Figures", "mosaico_mapa_covariables_DEM.png"),
       plot = p_mapas_covs,
       width = 8,
       height = 10,
       dpi = 350)


