## ----configuracion-------------------------------------------------------------------------------------------

#Para exportar como .R plano
# knitr::purl('05_analisis_glm_hotspots.qmd')

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(here, remotes, sf, geojsonio, geojsonsf, dplyr, ggplot2,
               patchwork, wesanderson, qs)

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
reticulate::py_run_string("import ee; ee.Initialize(project='even-electron-461718-g2')")




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
  dplyr::select(id_creado, UCSuelo, AREA_HA)

ucs_rao_sf <- ucs_rao_sf[!st_is_empty(ucs_rao_sf), ]

#Se verifica visulalmente
ggplot(data = ucs_rao_sf) +
  geom_sf(aes(fill = UCSuelo), color = NA) +  
  theme_void() +                             
  theme(legend.position = "none") 


## ----transforma_crs------------------------------------------------------------------------------------------

# Transforma a crs 4326 antes de pasarlo a GEE
ucs_sf_4326 <- st_transform(ucs_rao_sf, 4326)

ucs_sf_4326_test <- ucs_sf_4326[1:400,]



## ----tansform_ee---------------------------------------------------------------------------------------------

# Convierte a objetos GEE
ucs_ee_4326     <- rgee::sf_as_ee(ucs_sf_4326_test)

#Extrae bounding boxdel área de estudio
ee_4326_bbox <- ucs_ee_4326$bounds()

target_crs <- "EPSG:4326"    # Sistema soportado por GEE para exportación universal
target_scale <- 50           # Resolución de pixel objetivo (50 m)
export_folder <- "GEE_exports" # Carpeta destino en Google Drive "My Drive"



## ----dem_object----------------------------------------------------------------------------------------------

# Carga y suavizado del DEM SRTM 30 m
dem_clip   <- ee$Image("USGS/SRTMGL1_003")$rename("DEM")$clip(ee_4326_bbox)

# Visualiza en el visor antes de exportar (verifica recorte)
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  dem_clip,
  visParams = list(min = 0, max = 3000,
                   palette = viridis::viridis(10)), 
  name = "DEM SRTM (nativo 30m)"
  )



## ----dem_mean_task-------------------------------------------------------------------------------------------

# Calcula media del DEM por polígono
dem_mean <- dem_clip$reduceRegions(
  collection = ucs_ee_4326, #se trata como una FeatureCollection de polígonos
  reducer    = ee$Reducer$mean(),
  scale      = target_scale
)

# Exporta resultados a Google Drive
task_mean <- ee$batch$Export$table$toDrive(
  collection     = dem_mean,
  description    = "DEM_mean_by_polygon_400",
  folder         = export_folder,
  fileNamePrefix = "DEM_mean_by_polygon_400",
  fileFormat     = "CSV"
)

# Inicia la tarea
task_mean$start()

# Chequea estado
print(task_mean$status())


## ----dem_stdev_task------------------------------------------------------------------------------------------

# Calcular desviación estándar del DEM por polígono
dem_std <- dem_clip$reduceRegions(
  collection = ucs_ee_4326,
  reducer    = ee$Reducer$stdDev(),
  scale      = target_scale # por ejemplo, 50
)

# Exportar como tabla CSV
task_std <- ee$batch$Export$table$toDrive(
  collection     = dem_std,
  description    = "DEM_stdDev_by_polygon_400",
  folder         = "GEE_exports",
  fileNamePrefix = "DEM_stdDev_by_polygon_400",
  fileFormat     = "CSV"
)

# Inicia la tarea
task_std$start()

# Chequea estado
print(task_mean$status())



## ----reubicar_googledrive------------------------------------------------------------------------------------

#Falta código



## ----geojson_a_sf_dem----------------------------------------------------------------------------------------

# Leer archivos CSV exportados desde GEE
dem_mean_400 <- read.csv(here("Data", "DEM_mean_by_polygon_400.csv"), stringsAsFactors = FALSE)
dem_stdev_400 <- read.csv(here("Data", "DEM_stdDev_by_polygon_400.csv"), stringsAsFactors = FALSE)

# Unir ambos data frames por varias columnas clave
dem_cv_400 <- left_join(
  dem_mean_400,
  dem_stdev_400,
  by = c("system.index", "id_creado", "UCSuelo", "AREA_HA", ".geo")
)

# Convertir geometría desde .geo (GeoJSON como texto) a objeto sf
dem_cv_400_sf <- st_as_sf(
  data.frame(dem_cv_400, geometry = geojson_sf(dem_cv_400$.geo)), #convierte a sf
  crs = 4326) |>
  select(-.geo) |> #elimina columna de geometria obsoleta
  mutate(cv = stdDev / mean) #calcula coeficiente de variación

# Visualización rápida
ggplot(dem_cv_400_sf) +
  geom_sf(aes(fill = cv)) +
  scale_fill_viridis_c() +
  theme_minimal()


