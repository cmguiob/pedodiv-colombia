## ----configuracion, include = FALSE--------------------------------------------------------------------

#Para exportar como .R plano
# knitr::purl('03_analisis_eda_covariables.qmd')

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(
  here,        # rutas relativas del proyecto
  sf,          # lectura y manipulación de objetos espaciales vectoriales
  httr,        # peticiones HTTP REST (consulta servicio Mapa Geológico)
  geojsonsf,   # GeoJSON ↔ sf rápido (geojson_sf)
  dplyr,       # gramática de datos: mutate, select, joins
  tidyr,       # ✗ (no usado; pivot, unnest…)
  stringr,     # utilidades de texto y regex
  purrr,       # programación funcional: map*, walk*
  readr,       # lectura/escritura rápida de CSV
  ggdist,      # distribuciones y ‘raincloud’/histinterval
  scales,      # transformaciones y breaks de ejes
  ggplot2,     # visualización de datos
  paletteer,    # paletas de colores
  patchwork   # combinación de gráficos ggplot
)

# Ajusta tamaño de letra para todo e lscript
theme(base_size = 14)

#Utilidades gráficas
# Paletas de magnitud  (10 pasos → se interpolan en la escala) 
pal_dem   <- paletteer::paletteer_c("grDevices::terrain.colors", n = 10)
pal_slope <- paletteer::paletteer_c("grDevices::Temps",          n = 10)
pal_lst   <- paletteer::paletteer_c("ggthemes::Red-Gold",        n = 10)
pal_prec  <- paletteer::paletteer_c("grDevices::Earth",          n = 10) # "grDevices::Roma"


# Selección entorno ya existente antes de cualquier llamado que use Python
reticulate::use_condaenv("rgee_py", required = TRUE)

## Librerías que usan Python 
library(reticulate)
library(rgee)
library(googledrive)

# ==== Autenticación y backend Python ====
# Se fuerzan credenciales limpias para evitar colisiones de proyectos GEE entre sesiones
ee_clean_user_credentials()      # Limpia credenciales de GEE
ee_clean_pyenv()           # Limpia variables de entorno de reticulate
reticulate::py_run_string("import ee; ee.Authenticate()")
reticulate::py_run_string("import ee; ee.Initialize(project='even-electron-461718-g2')") #cuenta gmail propia
#reticulate::py_run_string("import ee; ee.Initialize(project='optimal-signer-459113-i1')") #cuenta UNAL

# === Autenticación Google Drive ===
googledrive::drive_auth()


## ----verifica_configuracion----------------------------------------------------------------------------

# Se consultan datos de DEM
img <- ee$Image("USGS/SRTMGL1_003")

#Consulta que propiedades están disponibles
img$propertyNames()$getInfo()

# Consultar una propiedad específica, e.g. keywords
img$get("keywords")$getInfo()


## ----carga_pedodiversidad------------------------------------------------------------------------------

# Corre script externo para cargar
source(here::here("Scripts", "00_funcion_carga_ucs_procesadas_qs.R"), encoding = "UTF-8")

# Asignación de id único para cada polígono
ucs_sf_4326 <- ucs_rao_sf |> 
  sf::st_make_valid() |> #valida geometrias problemáticas
  sf::st_transform(4326) |> # Transforma a crs 4326 
  dplyr::select(id_creado, UCSuelo, AREA_HA) 

ucs_sf_4326 <- ucs_sf_4326[!st_is_empty(ucs_sf_4326), ]

#Se verifica visulalmente
ggplot(data = ucs_sf_4326) +
  geom_sf(aes(fill = UCSuelo), color = NA) +  
  theme_void() +                             
  theme(legend.position = "none") 


## ----geologia_descarga_reclasificacion-----------------------------------------------------------------

# Se desagrega la url básica en sus componentes
url <- httr::parse_url("https://srvags.sgc.gov.co/arcgis/rest/services/Mapa_Geologico_Colombia/Mapa_Geologico_Colombia_V2023/MapServer")

layer_id <- "733"

url$path <- paste(url$path, layer_id, "query", sep = "/")

# Se agregan componentes a la URL para solicitud de información
url$query <- list(where = "1=1", # para recuperar todos los features
                  outFields = "*", #para recuperar todos los campos
                  returnGeometry = "true", #retorna geometrias
                  f = "geojson") #retorna formato geojson

# Se construye la url
url_solicitud <- httr::build_url(url)

# Para recuperar los datos espaciales se usa la librería sf
respuesta <- httr::GET(url_solicitud)

#Se examina la respuesta
print(respuesta)

# Descarga del mapa geológico 1 : 500 000 y clasificación de unidades por era (más joven)
geo_sf_wgs84 <- sf::st_read(url_solicitud) |>
  sf::st_make_valid() |>
  mutate(
    # LIMPIEZA -----------------------------------------------------------------
    edad_limpia = Edad %>% 
      str_replace_all("\\?", "") |>            # quita “?”
      str_trim() |>                              # quita espacios extremos
      str_to_lower() |>                         # pasa a minúsculas
      stringi::stri_trans_general("Latin-ASCII") |> # quita acentos
      str_replace_all(pattern = "-", replacement = " ") |> 
      str_squish(),                               # colapsa espacios múltiples
      
    
    # RECLASIFICACIÓN POR ERA (la MÁS JOVEN domina) ----------------------------
    era_geo = case_when(
      # 1) CENOZOICO  ----------------------------------------------------------
      str_detect(edad_limpia, regex(
        "paleoceno|eoceno|oligoceno|mioceno|plioceno|pleistoceno|holoceno|
         aquitaniano|burdigaliano|langhiano|serravaliano|tortoniano|messiniano|
         zancliano|rupeliano|thanetiano|lutetiano|bartoniano|priaboniano|
         selandiano|daniense|chattiano|cuaternario|mesiniano",
        ignore_case = TRUE)
      ) ~ "Cz",
      
      # 2) MESOZOICO  ----------------------------------------------------------
      str_detect(edad_limpia, regex(
        "triasico|jurasico|cretacico|berriasiano|valanginiano|barremiano|
         aptiano|albiano|cenomaniano|turoniano|coniaciano|santoniano|
         campaniano|maastrichtiano",
        ignore_case = TRUE)
      ) ~ "Mz",
      
      # 3) PALEOZOICO ----------------------------------------------------------
      str_detect(edad_limpia, regex(
      "\\bcambrico\\b|\\bordovicico\\b|\\bsilurico\\b|\\bdevonico\\b|
       \\bmississipiano\\b|\\bpridoliano\\b|\\bcarbonifero\\b|
       \\bpennsylvaniano\\b|\\bpermico\\b|\\bpaleozoico\\b",
      ignore_case = TRUE
      )) ~ "Pz",
      
      # 4) PROTEROZOICO --------------------------------------------------------
      str_detect(edad_limpia, regex(
        "sideriano|rhyaciano|orosiriano|statheriano|calymmiano|ectasiano|
         steniano|toniano|criogenico|ediacariano|mesoproterozoico|
         neoproterozoico|proterozoico",
        ignore_case = TRUE)
      ) ~ "Ptz",
      
      # 5) SIN DATO ------------------------------------------------------------
      TRUE ~ "NA"
    )
  ) |>
  select(descripcion_geo = Descripcion, era_geo, edad_limpia)



#Se visualizan datos geográficamente (edad, sin leyenda)
p_mapa_geo <- ggplot2::ggplot(geo_sf_wgs84) +
  geom_sf(aes(fill = era_geo), color = NA) +
  scale_fill_manual(
    breaks = c("Cz", "Mz", "Pz", "Ptz", "NA"),
    values = c(
      Cz    = "#f8ea1e",
      Mz    = "#5bc5ea",
      Pz   = "#a9c6a8",
      Ptz = "#ea5173",
      'NA'     = "gray95"
    ),
    drop = FALSE
  ) +
  guides(fill = guide_legend(label.position = "top")) +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(from = -82, to = -66, by = 4)) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.title = element_blank(),
    legend.position = "top",                  # parte superior
    legend.justification = "center",          # centrado
    legend.direction = "horizontal",          # orientación horizontal
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),           # in título (refuerzo)
    panel.background = element_rect(fill = "gray95", color = NA),
    panel.border = element_rect(color = "gray95", fill = NA, linewidth = 0.5)
  )

p_mapa_geo


## ----tansforma_ee--------------------------------------------------------------------------------------

# #Extrae bounding boxdel área del subconjunto
bb_sf_4326 <- st_bbox(ucs_sf_4326)

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



## ----extraccion_dem------------------------------------------------------------------------------------

# Carga y suavizado del DEM SRTM 30 m
dem_clip <- ee$Image("USGS/SRTMGL1_003")$clip(bbox_ee)

# Visualiza en el visor antes de exportar (verifica recorte)
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  dem_clip,
  visParams = list(min = 0, max = 3000,
                   palette = pal_dem), 
  name = "DEM SRTM (nativo 30m)"
  )



## ----extraccion_pendiente------------------------------------------------------------------------------

# Procesamiento de pendiente (slope) a partir del SRTM
slope_clip <- ee$Terrain$
  slope(ee$Image("USGS/SRTMGL1_003"))$
  clip(bbox_ee)$
  rename("slope_deg")  # valores ya están en grados

# Visualiza en el visor antes de exportar (verifica recorte)
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  slope_clip,
  visParams = list(min = 0, max = 45,
                   palette = pal_slope), 
  name = "Pendiente SRTM (nativo 30m)"
  )


## ----extraccion_curvatura------------------------------------------------------------------------------

# Bloque único de setup (solo la primera vez)
if (!py_module_available("tagee")) {
  py_install(c("tagee","ee_extra","regex","jsbeautifier"),
             envname="rgee_py", pip=TRUE)
}
tagee <- import("tagee",    convert = FALSE)
eeextra <- import("ee_extra", convert = FALSE)

reticulate::py_run_string("import ee; ee.Initialize(project='even-electron-461718-g2')")

# Ejecuta el análisis de terreno (devuelve un ee$Image con múltiples bandas)
dem_attr <- tagee$terrainAnalysis(dem_clip)

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



## ----extraccion_st_media-------------------------------------------------------------------------------

# Colección MODIS MOD11A2 (8-días, 1 km) convertida a °C
modis_collection <- ee$ImageCollection("MODIS/061/MOD11A2")$
  filterBounds(bbox_ee)$              # descarta las imágenes cuyo footprint no toca tu bbox_ee
  filterDate("2013-01-01", "2023-01-01")$
  map(function(img) {
    img$select("LST_Day_1km")$        # banda de temperatura diurna
      multiply(0.02)$                 # factor de escala
      subtract(273.15)$               # K → °C
      rename("lst_celsius")$
      clip(bbox_ee)                   # recorte geométrico
  })

# ── MEDIA ESPACIAL POR PÍXEL ────────────────────────────────────────────────
modis_media_espacial <- modis_collection$mean()$rename("modis_media_espacial")

# Verificación visual en el visor de Earth Engine
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  modis_media_espacial, 
  list(min = 10, max = 45, palette = pal_lst),
  "MODIS LST media (°C)"
)





## ----extraccion_lst_cv---------------------------------------------------------------------------------

# =============================================================================
# Colección MODIS MOD11A2 (8-días, 1 km) convertida a °C
# =============================================================================
modis_collection <- ee$ImageCollection("MODIS/061/MOD11A2")$
  filterBounds(bbox_ee)$              # descarta las imágenes cuyo footprint no toca tu bbox_ee
  filterDate("2013-01-01", "2023-01-01")$
  map(function(img) {
    img$select("LST_Day_1km")$        # banda de temperatura diurna
      multiply(0.02)$                 # factor de escala
      subtract(273.15)$               # K → °C
      rename("lst_celsius")$
      clip(bbox_ee)                   # recorte geométrico
  })

# ── CV TEMPORAL POR PÍXEL ────────────────────────────────────────────────
modis_temp_mean <- modis_collection$mean()$rename("modis_temp_mean")
modis_temp_sd   <- modis_collection$reduce(ee$Reducer$stdDev())$rename("modis_temp_sd")
modis_temp_cv   <- modis_temp_sd$divide(modis_temp_mean)$rename("modis_temp_cv")

# Verificación visual en el visor de Earth Engine
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  modis_temp_cv, 
  list(min = 0, max = 0.15, palette = pal_lst),
  "MODIS LST CV temporal"
)



## ----extraccion_precip_media---------------------------------------------------------------------------

# (1) Colección diaria recortada al bbox
chirps_daily <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$
  filterBounds(bbox_ee)$
  filterDate("2013-01-01", "2023-01-01")

# (2) Agregación mensual  (mm por mes)
chirps_monthly_ic <- ee$ImageCollection(
  ee$List$sequence(2013, 2023)$map(ee_utils_pyfunc(function(y) {
    ee$List$sequence(1, 12)$map(ee_utils_pyfunc(function(m) {
      start <- ee$Date$fromYMD(y, m, 1)
      end   <- start$advance(1, "month")
      chirps_daily$
        filterDate(start, end)$
        sum()$                      # mm del mes
        clip(bbox_ee)$
        set("system:time_start", start)
    }))
  }))$flatten()
)

# (3) Media mensual (mm · mes⁻¹) por píxel en 2013–2023
precip_media_espacial <- chirps_monthly_ic$
  mean()$
  rename("precip_media_espacial")$
  clip(bbox_ee)

# (4) Vista rápida en el visor de Earth Engine
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  precip_media_espacial,
  list(min = 0, max = 800, palette = pal_prec),
  "CHIRPS · media mensual (mm/mes)"
)



## ----extraccion_precip_cv------------------------------------------------------------------------------

# Colección diaria recortada
chirps_daily <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$
  filterBounds(bbox_ee)$  #descarta las imágenes cuyo footprint no toca tu bbox_ee
  filterDate("2013-01-01", "2023-01-01")

# ── Agregación mensual (mm por mes) ──────────────────────────────────────────
chirps_monthly_ic <- ee$ImageCollection(
  ee$List$sequence(2013, 2023)$map(ee_utils_pyfunc(function(y) {
    ee$List$sequence(1, 12)$map(ee_utils_pyfunc(function(m) {
      start <- ee$Date$fromYMD(y, m, 1)
      end   <- start$advance(1, "month")
      chirps_daily$
        filterDate(start, end)$
        sum()$                         # mm del mes
        clip(bbox_ee)$
        set("system:time_start", start)
    }))
  }))$flatten()
)

# ── (1) CV TEMPORAL POR PÍXEL (mensual) ─────────────────────────────────────
precip_mean_temp <- chirps_monthly_ic$mean()$rename("precip_mean_temp")
precip_sd_temp   <- chirps_monthly_ic$reduce(ee$Reducer$stdDev())$rename("precip_sd_temp")
precip_cv_temp   <- precip_sd_temp$divide(precip_mean_temp)$rename("precip_cv_temp")

# Verificación visual en el visor de Earth Engine
Map$setCenter(lon = -74, lat = 4, zoom = 5)
Map$addLayer(
  precip_cv_temp,
  list(min = 0, max = 1, palette = pal_prec),
  "Precip CV temporal (mensual)"
)



## ----exporta_covariables_poligonos---------------------------------------------------------------------

#Esto solo se corre una vez

# Extrae estadísticas (mean / sd) por UCS y las envía a Drive en lotes adaptativos
#source(here::here("Scripts", "00_funcion_procesamiento_lotes_imagen.R"), encoding = "UTF-8")

#registro_dem <- procesamiento_lotes_imagen(ucs_sf_4326, image = dem_clip, start_idx = 1, max_index = 43384, variable_name = "DEM", scale = 50)

#registro_slope <- procesamiento_lotes_imagen(ucs_sf_4326, image = slope_clip, start_idx = 1, max_index = 43384, batch_s = 400, reduce_batch_by = 4, variable_name = "SLOPE", scale = 50)

#registro_lst_esp <- procesamiento_lotes_imagen(ucs_sf_4326, image = modis_media_espacial, start_idx = 1, max_index = 43384, batch_s = 300, reduce_batch_by = 3, variable_name = "LST_media_espacial", scale = 500)

#registro_lst_temp <- procesamiento_lotes_imagen(ucs_sf_4326, image = modis_temp_cv, start_idx = 1, max_index = 43384, batch_s = 300, reduce_batch_by = 3, variable_name = "LST_cv_temporal", scale = 500)

#registro_precip_cv_temporal <- procesamiento_lotes_imagen(ucs_sf_4326, image = precip_cv_temp, start_idx = 1, max_index = nrow(ucs_sf_4326), batch_s = 300, reduce_batch_by = 3, variable_name  = "PRECIP_cv_temporal", scale = 500)

#registro_precip_esp <- procesamiento_lotes_imagen(ucs_sf_4326, image = precip_media_espacial, start_idx = 1, max_index = 43384, batch_s = 300,   reduce_batch_by = 3, variable_name  = "PRECIP_media_espacial", scale = 500)




## ----reubicar_googledrive------------------------------------------------------------------------------

#Esto solo se corre una vez

# Copia local de archivos combinados para reproducibilidad offline
# combinar_y_subir_csv <- function(propiedad,
#                                  carpeta_drive_id_origen = "17yxwhlpgL4EG8inI5u8Nwi08wOrnhJiM",  # GEE_exports
#                                  carpeta_drive_id_destino = "1qJ5S25TZaFWzueNhx1M4Gr3P8JYWKGeU",  # Proyecto
#                                  carpeta_temporal = "tmp_csv") {
# 
#   # Crea carpeta temporal local si no existe
#   if (!dir.exists(carpeta_temporal)) {
#     dir.create(carpeta_temporal)
#   }
# 
#   # Listar archivos en Google Drive (solo .csv con prefijo exacto)
#   archivos_drive <- googledrive::drive_ls(
#     path = as_id(carpeta_drive_id_origen),
#     pattern = glue::glue("^{propiedad}_.*\\.csv$")
#   ) |>
#     dplyr::filter(stringr::str_ends(name, ".csv"))
# 
#   if (nrow(archivos_drive) == 0) {
#     stop(glue::glue("No se encontraron archivos CSV para la propiedad '{propiedad}' en GEE_exports."))
#   }
# 
#   message(glue::glue("📥 Descargando {nrow(archivos_drive)} archivos CSV para '{propiedad}'..."))
# 
#   # Descargar archivos al directorio temporal
#   purrr::walk2(
#     archivos_drive$name,
#     archivos_drive$id,
#     ~ googledrive::drive_download(
#       file = as_id(.y),
#       path = file.path(carpeta_temporal, .x),
#       overwrite = TRUE
#     )
#   )
# 
#   # Leer y combinar
#   archivos_locales <- list.files(path = carpeta_temporal,
#                                  pattern = paste0("^", propiedad, "_.*\\.csv$"),
#                                  full.names = TRUE)
# 
#   combinado <- purrr::map_dfr(archivos_locales, readr::read_csv, show_col_types = FALSE)
# 
#   # Escribir archivo combinado
#   nombre_salida <- paste0("OUT_", propiedad, "_combinado.csv")
#   ruta_salida <- file.path(carpeta_temporal, nombre_salida)
#   readr::write_csv(combinado, ruta_salida)
# 
#   # Subir a carpeta final de proyecto en Drive
#   archivo_subido <- googledrive::drive_upload(
#     media = ruta_salida,
#     path = as_id(carpeta_drive_id_destino),
#     name = nombre_salida,
#     overwrite = TRUE
#   )
# 
#   message(glue::glue("✅ Archivo combinado subido: {archivo_subido$name} (ID: {archivo_subido$id})"))
# 
#   # Limpieza automática
#   unlink(carpeta_temporal, recursive = TRUE)
#   message("🧹 Archivos temporales eliminados.")
# }



## ----aplica_funcion_combinacion------------------------------------------------------------------------

#combinar_y_subir_csv("DEM")
#combinar_y_subir_csv("SLOPE")
#combinar_y_subir_csv("LST_cv_temporal")
#combinar_y_subir_csv("LST_media_espacial") 
#combinar_y_subir_csv("PRECIP_cv_temporal")
#combinar_y_subir_csv("PRECIP_media_espacial") 



## ----carga_covariables---------------------------------------------------------------------------------

# ─────────────────────────────────────────────────────────────────────────────
# 0 · Ajuste de precisión para divisiones seguras
# ─────────────────────────────────────────────────────────────────────────────
eps <- 1e-6                         # ε evita CV = σ/0
safe_log10 <- function(x) log10(pmax(x, eps))

# ─────────────────────────────────────────────────────────────────────────────
# 1 · DEM  · media, CV y derivados (todas las variables en español)
# ─────────────────────────────────────────────────────────────────────────────
dem_csv <- readr::read_csv(           # <- nombre lógico del objeto CSV
  here::here("Data/OUT_covars_csv/OUT_DEM_combinado.csv"),
  show_col_types = FALSE
)

covars_dem <- st_as_sf(
  data.frame(dem_csv, geometry = geojson_sf(dem_csv$.geo)), crs = 4326
) |>
  # «media» en lugar de «mean»
  dplyr::rename(dem_media = mean) |>
  dplyr::mutate(
    # Coef. de variación espacial de la elevación
    dem_cv              = stdDev / pmax(abs(dem_media), eps),
    log_dem_cv          = safe_log10(dem_cv),

    # Densidad de CV por unidad de superficie
    dem_cv_densidad     = dem_cv / AREA_HA,
    log_dem_cv_densidad = safe_log10(dem_cv_densidad),

    # Versión estandarizada (Z-score) de cada métrica
    across(c(dem_media, dem_cv, log_dem_cv,
             dem_cv_densidad, log_dem_cv_densidad),
           ~ scale(.x)[,1], .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)

# ─────────────────────────────────────────────────────────────────────────────
# 2 · SLOPE (media & CV espacial)
# ─────────────────────────────────────────────────────────────────────────────
slope_cv <- readr::read_csv(
  here::here("Data/OUT_covars_csv/OUT_SLOPE_combinado.csv"),
  show_col_types = FALSE
)

covars_pendiente <- st_as_sf(
  data.frame(slope_cv, geometry = geojson_sf(slope_cv$.geo)), crs = 4326
) |>
  dplyr::rename(pendiente_media = mean) |>
  dplyr::mutate(
    pendiente_cv          = stdDev / pmax(abs(pendiente_media), eps),
    log_pendiente_cv      = safe_log10(pendiente_cv),
    pendiente_cv_densidad = pendiente_cv / AREA_HA,
    log_pendiente_cv_densidad = safe_log10(pendiente_cv_densidad),
    across(c(pendiente_cv, log_pendiente_cv,
             pendiente_cv_densidad, log_pendiente_cv_densidad,
             pendiente_media),
           ~ scale(.x)[,1], .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)

# ─────────────────────────────────────────────────────────────────────────────
# 3 · LST media espacial (2013-2023)
# ─────────────────────────────────────────────────────────────────────────────
lst_media_esp <- readr::read_csv(
  here::here("Data/OUT_covars_csv/OUT_LST_media_espacial_combinado.csv"),
  show_col_types = FALSE
)

covars_lst_media <- st_as_sf(
  data.frame(lst_media_esp, geometry = geojson_sf(lst_media_esp$.geo)), crs = 4326
) |>
  dplyr::rename(lst_media = mean) |>
  dplyr::mutate(
    log_lst_media          = safe_log10(lst_media),
    lst_media_densidad     = lst_media / AREA_HA,
    log_lst_media_densidad = safe_log10(lst_media_densidad),
    across(c(lst_media, log_lst_media,
             lst_media_densidad, log_lst_media_densidad),
           ~ scale(.x)[,1], .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)  

# ─────────────────────────────────────────────────────────────────────────────
# 4 · LST CV temporal (2013-2023)
# ─────────────────────────────────────────────────────────────────────────────

lst_cv_temp <- readr::read_csv(
  here::here("Data/OUT_covars_csv/OUT_LST_cv_temporal_combinado.csv"),
  show_col_types = FALSE
)

covars_lst_cv_temp <- st_as_sf(
  data.frame(lst_cv_temp, geometry = geojson_sf(lst_cv_temp$.geo)), crs = 4326
) |>
  dplyr::rename(lst_cv_temporal = mean) |>
  dplyr::mutate(
    log_lst_cv_temporal          = safe_log10(lst_cv_temporal),
    lst_cv_temporal_densidad     = lst_cv_temporal / AREA_HA,
    log_lst_cv_temporal_densidad = safe_log10(lst_cv_temporal_densidad),
    across(c(lst_cv_temporal, log_lst_cv_temporal,
             lst_cv_temporal_densidad, log_lst_cv_temporal_densidad),
           ~ scale(.x)[,1], .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)

# ─────────────────────────────────────────────────────────────────────────────
# 5 · PRECIP CV temporal  &  6 · PRECIP media espacial (idéntica lógica)
# ─────────────────────────────────────────────────────────────────────────────

# Precip cv temporal
precip_cv_temp <- readr::read_csv(
  here::here("Data/OUT_covars_csv/OUT_PRECIP_cv_temporal_combinado.csv"),
  show_col_types = FALSE
)

covars_precip_cv_temp <- st_as_sf(
  data.frame(precip_cv_temp, geometry = geojson_sf(precip_cv_temp$.geo)), crs = 4326
) |>
  dplyr::rename(precip_cv_temporal = mean) |>
  dplyr::mutate(
    log_precip_cv_temporal          = safe_log10(precip_cv_temporal),
    precip_cv_temporal_densidad     = precip_cv_temporal / AREA_HA,
    log_precip_cv_temporal_densidad = safe_log10(precip_cv_temporal_densidad),
    across(c(precip_cv_temporal, log_precip_cv_temporal,
             precip_cv_temporal_densidad, log_precip_cv_temporal_densidad),
           ~ scale(.x)[,1], .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)   


# Precip media espacial
precip_media_esp <- readr::read_csv(
  here::here("Data/OUT_covars_csv/OUT_PRECIP_media_espacial_combinado.csv"),
  show_col_types = FALSE
)

covars_precip_media <- st_as_sf(
  data.frame(precip_media_esp, geometry = geojson_sf(precip_media_esp$.geo)), crs = 4326
) |>
  dplyr::rename(precip_media = mean) |>
  dplyr::mutate(
    log_precip_media          = safe_log10(precip_media),
    precip_media_densidad     = precip_media / AREA_HA,
    log_precip_media_densidad = safe_log10(precip_media_densidad),
    across(c(precip_media, log_precip_media,
             precip_media_densidad, log_precip_media_densidad),
           ~ scale(.x)[,1], .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)





## ----union_variables-----------------------------------------------------------------------------------

# Unir a la tabla base
modelo_df <- ucs_sf_4326 |>
  left_join(covars_dem            |> st_drop_geometry(),
            by = c("id_creado","AREA_HA","UCSuelo")) |>
  left_join(covars_pendiente      |> st_drop_geometry(),
            by = c("id_creado","AREA_HA","UCSuelo")) |>
  left_join(covars_lst_cv_temp    |> st_drop_geometry(),
            by = c("id_creado","AREA_HA","UCSuelo")) |>
  left_join(covars_lst_media      |> st_drop_geometry(),
            by = c("id_creado","AREA_HA","UCSuelo")) |>
  left_join(covars_precip_cv_temp |> st_drop_geometry(),
            by = c("id_creado","AREA_HA","UCSuelo")) |>
  left_join(covars_precip_media   |> st_drop_geometry(),
            by = c("id_creado","AREA_HA","UCSuelo"))




## ----visualiza_distribuciones--------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# FUNCIÓN utilitaria · histograma + densidad
#   data  : tabla con todas las variables
#   var   : columna a graficar (tidy-eval)
#   title : título del panel
#   xlab  : texto del eje-X
# ---------------------------------------------------------------------------
common_hist <- function(data, var, title, xlab){
  ggplot(dplyr::filter(data, is.finite({{ var }})), aes({{ var }})) +
    # Histograma con intervalos de credibilidad (ggdist)
    ggdist::stat_histinterval(fill = "#56B4E9", colour = "black") +
    # Curva de densidad (área = 1) re-escalada a la altura del histo
    geom_density(aes(y = after_stat(scaled)),
                 colour = "#FFD700", linewidth = 0.8) +
    labs(title = title, x = xlab, y = "Frecuencia") +
    theme_minimal(base_size = 12)
}

# ── MAGNITUDES ─────────────────────────────────────────────────────────────
p_dem_media      <- common_hist(modelo_df, dem_media,
                                "Elevación media", "m")

p_pend_media     <- common_hist(modelo_df, pendiente_media,
                                "Pendiente media", "°")

p_lst_media      <- common_hist(modelo_df, lst_media,
                                "Tº superficie (LST)", "°C")

p_prec_media     <- common_hist(modelo_df, precip_media,
                                "Precip. media mensual", "mm·mes⁻¹")

# ── log-CV ─────────────────────────────────────────────────────────────────
p_log_dem_cv     <- common_hist(modelo_df, log_dem_cv,
                                expression(log[10]*"CV DEM"),
                                expression(log[10]*"CV"))

p_log_pend_cv    <- common_hist(modelo_df, log_pendiente_cv,
                                expression(log[10]*"CV pendiente"),
                                expression(log[10]*"CV"))

p_log_lst_cv     <- common_hist(modelo_df, log_lst_cv_temporal,
                                expression(log[10]*"CV LST"),
                                expression(log[10]*"CV"))

p_log_prec_cv    <- common_hist(modelo_df, log_precip_cv_temporal,
                                expression(log[10]*"CV precipitación"),
                                expression(log[10]*"CV"))

# ── log-CV densidad ───────────────────────────────────────────────────────
p_log_dem_cvd    <- common_hist(modelo_df, log_dem_cv_densidad,
                                expression(log[10]*"CV dens. DEM"),
                                expression(log[10]*"CV dens."))

p_log_pend_cvd   <- common_hist(modelo_df, log_pendiente_cv_densidad,
                                expression(log[10]*"CV dens. pendiente"),
                                expression(log[10]*"CV dens."))

p_log_lst_cvd    <- common_hist(modelo_df, log_lst_cv_temporal_densidad,
                                expression(log[10]*"CV dens. LST"),
                                expression(log[10]*"CV dens."))

p_log_prec_cvd   <- common_hist(modelo_df, log_precip_cv_temporal_densidad,
                                expression(log[10]*"CV dens. precipitación"),
                                expression(log[10]*"CV dens."))

# ── Mosaico 4 × 3 ──────────────────────────────────────────────────────────
p_mosaico_hist <- (
  p_dem_media | p_pend_media | p_lst_media | p_prec_media) /
  (p_log_dem_cv | p_log_pend_cv | p_log_lst_cv | p_log_prec_cv) /
  (p_log_dem_cvd| p_log_pend_cvd| p_log_lst_cvd| p_log_prec_cvd)

# Vista rápida en el visor
p_mosaico_hist

# Exportar (opcional)
ggsave(
  here::here("Figures", "mosaico_hist_covariables.png"),
  plot   = p_mosaico_hist,
  width  = 12, height = 9, dpi = 350
)


## ----visualiza_mapas-----------------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# GRADIENTES monocromáticos para los mapas de CV
#   – blancos para valores bajos
#   – color saturado para valores altos
# ---------------------------------------------------------------------------
mono_pal <- function(col_hex, n = 100)
  colorRampPalette(c("#ffffff", col_hex))(n)

pal_dem_cv        <- mono_pal(head(pal_dem,   1))
pal_pendiente_cv  <- mono_pal(tail(pal_slope, 1))
pal_lst_cv        <- mono_pal(tail(pal_lst,   1))
pal_prec_cv       <- mono_pal(tail(pal_prec,  1))

# ---------------------------------------------------------------------------
# FUNCIÓN genérica para mapas de magnitud
# ---------------------------------------------------------------------------
map_mag <- function(data, var, pal, titulo, leyenda){
  ggplot(data) +
    geom_sf(aes(fill = {{ var }}), colour = NA) +
    scale_fill_gradientn(
      colours = pal, na.value = "white",
      guide   = guide_colourbar(barwidth = unit(0.25,"cm"),
                                barheight = unit(2.5,"cm"),
                                direction = "vertical",
                                title.position = "top"),
      name = leyenda
    ) +
    labs(title = titulo) +
    coord_sf(expand = FALSE) +
    scale_x_continuous(breaks = seq(-79, -70, by = 3)) +
    theme_minimal() +
    theme(legend.justification = c("right","top"),
          plot.title = element_text(size = 10, hjust = .5))
}

# ---------------------------------------------------------------------------
# FUNCIÓN genérica para mapas de variabilidad (CV, escala log)
# ---------------------------------------------------------------------------
map_cv <- function(data, var, pal, titulo){
  ggplot(dplyr::filter(data, {{ var }} > 0)) +   # evita log(0)
    geom_sf(aes(fill = {{ var }}), colour = NA) +
    scale_fill_gradientn(
      colours = pal,
      trans   = "log10",
      breaks  = scales::trans_breaks("log10", function(x) 10^x),
      labels  = scales::label_scientific(digits = 1),
      na.value = "white",
      guide   = guide_colourbar(barwidth = unit(0.25,"cm"),
                                barheight = unit(2.5,"cm"),
                                direction = "vertical",
                                title.position = "top"),
      name    = "CV"
    ) +
    labs(title = titulo) +
    coord_sf(expand = FALSE) +
    scale_x_continuous(breaks = seq(-79, -70, by = 3)) +
    theme_minimal() +
    theme(legend.justification = c("right","top"),
          plot.title = element_text(size = 10, hjust = .5))
}

# ---------------------------------------------------------------------------
# MAPAS DE MAGNITUD
# ---------------------------------------------------------------------------
p_dem_media      <- map_mag(modelo_df, dem_media,       pal_dem,
                            "Media de DEM (m)",               "m")

p_pend_media     <- map_mag(modelo_df, pendiente_media,  pal_slope,
                            "Media de pendiente (°)",         "°")

p_lst_media      <- map_mag(modelo_df, lst_media,        pal_lst,
                            "Media de LST (°C)",              "°C")

p_prec_media     <- map_mag(modelo_df, precip_media,     pal_prec,
                            "Media de precipitación (mm·mes⁻¹)", "mm")

# ---------------------------------------------------------------------------
# MAPAS DE VARIABILIDAD (CV)
# ---------------------------------------------------------------------------
p_dem_cv         <- map_cv(modelo_df, dem_cv,            pal_dem_cv,
                            "CV de DEM")

p_pend_cv        <- map_cv(modelo_df, pendiente_cv,      pal_pendiente_cv,
                            "CV de pendiente")

p_lst_cv         <- map_cv(modelo_df, lst_cv_temporal,   pal_lst_cv,
                            "CV de LST")

p_prec_cv        <- map_cv(modelo_df, precip_cv_temporal,pal_prec_cv,
                            "CV de precipitación")

# ---------------------------------------------------------------------------
# MOSAICO 4 × 2  (arriba magnitud · abajo CV)
# ---------------------------------------------------------------------------
layout <- area(
  t = c(1,1,1,1,1, 2,2,2,2,2),
  l = c(1,2,3,4,5, 1,2,3,4,5),
  b = c(1,1,1,1,1, 2,2,2,2,2),
  r = c(1,2,3,4,5, 1,2,3,4,5)
)

espacio_vacio <- ggplot() + theme_void()

p_mosaico <- p_dem_media + p_pend_media + p_lst_media + p_prec_media + p_mapa_geo +
             p_dem_cv    + p_pend_cv    + p_lst_cv    + p_prec_cv    + espacio_vacio +
             plot_layout(design = layout)

# Exportar la figura
ggsave(
  here::here("Figures", "mosaico_mapa_covariables.png"),
  plot   = p_mosaico,
  width  = 12, height = 6, dpi = 350
)

