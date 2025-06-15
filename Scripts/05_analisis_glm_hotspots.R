## ----configuracion-------------------------------------------------------------------------------------------

#Para exportar como .R plano
# knitr::purl('05_analisis_glm_hotspots.qmd')

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(here, remotes, sf, geojsonio, dplyr, ggplot2,
               patchwork, wesanderson, qs)

Sys.setenv(EARTHENGINE_DISABLE_PROMPTS = "TRUE")

reticulate::use_condaenv("rgee_py", required = TRUE)

## Librerías que usan Python 
library(reticulate)
library(rgee)
library(googledrive)

## Autenticación y backend python
#ee_clean_user_credentials()      # Limpia credenciales de GEE
ee_clean_pyenv()           # Limpia variables de entorno de reticulate
#reticulate::py_run_string("import ee; ee.Authenticate(); ee.Initialize(project='even-electron-461718-g2')")

# 1) Autenticación & inicialización EE con scope de Drive
reticulate::py_run_string("
import ee
ee.Authenticate(scopes=[
  'https://www.googleapis.com/auth/earthengine',
  'https://www.googleapis.com/auth/drive'
])
ee.Initialize(project='even-electron-461718-g2')
")

# 2) Importa el módulo EE
ee <- reticulate::import("ee", convert = FALSE)



## ----verifica_configuracion----------------------------------------------------------------------------------

# Se consultan datos de DEM
img <- ee$Image("USGS/SRTMGL1_003")

#Consulta que propiedades están disponibles
img$propertyNames()$getInfo()


## ----carga_pedodiversidad------------------------------------------------------------------------------------

# Corre script externo para cargar
source(here::here("Scripts", "00_funcion_carga_ucs_procesadas_qs.R"), encoding = "UTF-8")

# Asignación de id único para cada polígono
ucs_rao_sf <- ucs_rao_sf |> dplyr::mutate(id = row_number())

head(ucs_rao_sf)


## ----carga_poligono------------------------------------------------------------------------------------------

# Define ruta y nombre de capa de geopackage de departamentos
deptos_ruta <- here::here("Data", "INP_departamentos_IGAC_Abril_2025.gpkg")
capa_nombre_deptos <- sf::st_layers(deptos_ruta)$name[1]

# Carga geopackage de dpartamentos
departamentos_sf <- sf::st_read(
  deptos_ruta,
  layer = capa_nombre_deptos,
  quiet = TRUE
  ) |>
  # Se seleccionan 21 departamentos de la zona Andina, Caribe y Pacífica
  dplyr::filter(DeNombre %in% c(
    "Antioquia", 
    "Atlántico",
    "Bolívar",
    "Boyacá",
    "Caldas",
    "Cauca",
    "Cesar",
    "Chocó", 
    "Córdoba",
    "Cundinamarca",
    "Huila",
    "La Guajira",
    "Magdalena",
    "Nariño",
    "Norte de Santander",
    "Quindío",
    "Risaralda",
    "Santander", 
    "Sucre",
    "Tolima",
    "Valle del Cauca")
    ) |>
  tidyr::drop_na()
  

# departamento_1_sf pasa de "SHAPE" a "geometry"
names(departamentos_sf)[names(departamentos_sf) == "SHAPE"] <- "geometry"
departamentos_sf <- sf::st_as_sf(as.data.frame(departamentos_sf), sf_column_name = "geometry")

# Aseguramos que ambos datasets tengan la misma proyección
departamentos_sf <- sf::st_transform(departamentos_sf, st_crs(ucs_rao_sf))

# Se unen los polígonos en uno solo
limite_poly <- sf::st_union(departamentos_sf) 



## ------------------------------------------------------------------------------------------------------------

ucs_ee     <- rgee::sf_as_ee(ucs_rao_sf)
estudio_ee <- rgee::sf_as_ee(limite_poly)



## ------------------------------------------------------------------------------------------------------------

# Carga y suavizado del DEM SRTM 30 m
dem   <- ee$Image("USGS/SRTMGL1_003")$rename("DEM")$clip(estudio_ee)



## ------------------------------------------------------------------------------------------------------------

# Centra el visor en tu área de interés
Map$setCenter(lon = -74, lat = 4, zoom = 5)
# 2. Añadir el DEM original (sin suavizar)
Map$addLayer(
  dem,
  visParams = list(min = 0, max = 3000,
                    palette = viridis::viridis(10)),
  name = "DEM original"
)



## ------------------------------------------------------------------------------------------------------------

slope_deg  <- ee$
  Terrain$
  slope(dem)$
  multiply(180/pi)$
  rename("slope_deg")



## ------------------------------------------------------------------------------------------------------------

# Centrar el visor en tu área de interés
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  slope_deg,
  visParams = list(min = 0, max = 60,
                   palette = viridis::viridis(10)),
  name = "Pendiente (°)"
)



## ------------------------------------------------------------------------------------------------------------

# Bloque único de setup (solo la primera vez)
if (!py_module_available("tagee")) {
  py_install(c("tagee","ee_extra","regex","jsbeautifier"),
             envname="rgee_py", pip=TRUE)
}
tagee <- import("tagee",    convert = FALSE)
eeextra <- import("ee_extra", convert = FALSE)


# Ejecuta el análisis de terreno (devuelve un ee$Image con múltiples bandas)
dem_attr <- tagee$terrainAnalysis(dem)

dem_attr$bandNames()$getInfo()



## ------------------------------------------------------------------------------------------------------------

# Extrae la banda de Curvatura Vertical
vc_img <- dem_attr$select("VerticalCurvature")



## ------------------------------------------------------------------------------------------------------------
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  vc_img,
  visParams = list(
    min     = -0.00005,
    max     = +0.00005,
    palette = viridis::viridis(5)
  ),
  name = "Curvatura vertical (±0.00005)"
)



## ------------------------------------------------------------------------------------------------------------

# Paso A: mediana global (sin clip)
lst_med <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
  filterBounds(estudio_ee)$
  filterDate("2013-01-01","2023-01-01")$
  map(function(img) img$select("ST_B10")$
                   multiply(0.00341802)$
                   add(149))$
  median()$
  rename("lst")

# Paso B: clip único de la imagen final
lst_clipped <- lst_med$clip(estudio_ee)



## ------------------------------------------------------------------------------------------------------------

# Visualización con escala explícita para reducir memoria
Map$setCenter(-74, 4, 5)
Map$addLayer(
  lst_clipped,
  visParams = list(min = 270, max = 320, palette = c("blue","white","red")),
  name  = "LST mediana (único clip)"
)



## ------------------------------------------------------------------------------------------------------------

# Paso A: mediana global de VH/VV (sin clip)
vhvv_med <- ee$ImageCollection("COPERNICUS/S1_GRD")$
  filterBounds(estudio_ee)$
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

# clip único de la imagen final al área de estudio
vhvv_clipped <- vhvv_med$clip(estudio_ee)





## ------------------------------------------------------------------------------------------------------------

# Visualización en el visor de rgee
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  vhvv_clipped,
  visParams = list(min = 0, max = 2, palette = c("brown", "white", "blue")),
  name = "VH/VV mediana (clip único)"
)



## ------------------------------------------------------------------------------------------------------------

# Se vuelve a correr esto, en caso de que alguna de las funciones previas haya cambiado la configuración
ee_clean_pyenv()
reticulate::py_run_string("
import ee
ee.Authenticate()
ee.Initialize(project='even-electron-461718-g2')
")


## ------------------------------------------------------------------------------------------------------------

export_folder <- "GEE_exports" # Carpeta destino en Google Drive "My Drive"

# ===== Chunk 1: Definir el reductor (rápido) =====
# Define un reductor combinado: media + desviación estándar (sd)
# - ee$Reducer$mean()   → media de los píxeles en cada UCS
# - ee$Reducer$stdDev() → desviación estándar de los mismos píxeles
# - sharedInputs = TRUE  → sd usa los mismos píxeles que mean
reducer_cv <- ee$Reducer$mean()$combine(
  reducer2     = ee$Reducer$stdDev(),
  sharedInputs = TRUE
)


# ===== Chunk 2: Construir reduceRegions (puede atascarse) =====
# Aplicar reduceRegions para calcular media y sd en las UCS
#    Ejemplo para pendiente (slope_deg), escala 30 m
t0 <- Sys.time()
slope_stats <- slope_deg$reduceRegions(
  collection = ucs_ee,
  reducer    = reducer_cv,
  scale      = 30
)
message("Tiempo reduceRegions: ", round(as.numeric(Sys.time() - t0),1), " seg")

# ===== Chunk 3: Mapear CV (rápido) =====
# Mapear sobre cada elemento para calcular el CV y quedarnos solo con id + slope_cv
slope_cv_fc <- slope_stats$map(function(feature) {
  feat     <- ee$Feature(feature)
  mean_val <- ee$Number(feat$get("mean"))
  sd_val   <- ee$Number(feat$get("stdDev"))
  # Añade una nueva propiedad "slope_cv" = sd / mean, y selecciona id + slope_cv
  feat$
    set("slope_cv", sd_val$divide(mean_val))$
    select(c("id", "slope_cv"))
})


# ===== Chunk 4: Crear y arrancar la tarea =====
# Exportar la tabla de CV a Google Drive para poder monitorear en EE Task Manager
task <- ee$batch$Export$table$toDrive(
  collection     = slope_cv_fc,
  description    = "CV_slope_UCS",
  folder         = "GEE_exports",
  fileNamePrefix = "CV_slope_UCS",
  fileFormat     = "CSV"
)
task$start()



## ------------------------------------------------------------------------------------------------------------

# 4) Construye tu reductor combinado para CV
reducer_cv <- ee$Reducer$mean()$combine(
  reducer2     = ee$Reducer$stdDev(),
  sharedInputs = TRUE
)

# 5) Aplica reduceRegions (esto sí puede tardar, pero no se colgará)
slope_stats <- slope_deg$reduceRegions(
  collection = ucs_ee,
  reducer    = reducer_cv,
  scale      = 30
)

# 6) Añade el campo slope_cv
slope_cv_fc <- slope_stats$map(function(f) {
  f <- ee$Feature(f)
  m <- ee$Number(f$get("mean"))
  s <- ee$Number(f$get("stdDev"))
  f$set("slope_cv", s$divide(m))$select(c("id", "slope_cv"))
})

# 7) Exporta la tabla a Google Drive
task <- ee$batch$Export$table$toDrive(
  collection     = slope_cv_fc,
  description    = "CV_slope_UCS",
  folder         = "GEE_exports",
  fileNamePrefix = "CV_slope_UCS",
  fileFormat     = "CSV"
)
task$start()


## ------------------------------------------------------------------------------------------------------------


# ——————————————————————————————————————————————————————————————————————
# Repite los pasos 2–4 para cada variable:
#   • Curvatura horizontal: use hc_img, escala = 30
#   • Curvatura vertical:   use vc_img, escala = 30
#   • LST median (lst_med): use lst_clipped, escala = 100
#   • VH/VV median:         use vhvv_clipped, escala = 



## ------------------------------------------------------------------------------------------------------------

df_glm <- ucs_rao_sf %>%
  left_join(df_cv_all, by="id") %>%
  filter(as.logical(st_intersects(geometry, limite_poly))) %>%
  mutate(hot = if_else(Q >= quantile(Q, 0.95), 1, 0))

mod <- glm(
  hot ~ slope_cv + horiz_cv + vert_cv + lst_cv + vhvv_cv,
  data    = df_glm,
  family  = binomial(),
  weights = 1/AREA_HA
)
summary(mod)
car::vif(mod)

