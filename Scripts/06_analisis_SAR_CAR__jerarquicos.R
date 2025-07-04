## ----configuracion-------------------------------------------------------------------------------------

#Para exportar como .R plano
# knitr::purl('06_analisis_SAR_CAR_jerarquicos.qmd')

if (!"pacman" %in% rownames(installed.packages())) install.packages("pacman")
pacman::p_load(
  here,         # manejo de rutas relativas al proyecto
  remotes,      # instalar paquetes desde GitHub (si llegamos a necesitarlos)
  sf,           # lectura y manipulación de objetos espaciales vectoriales
  dplyr,        # verbos de manipulación de data frames (filter, mutate, joins)
  tidyr,        # pivoteo y desanidado de datos (pivot_longer, unnest, etc.)
  readr,        # lectura/escritura rápida de archivos CSV
  geojsonsf,    # GeoJSON ↔ sf rápido (geojson_sf)
  rmapshaper,   # Simplificación de geometrías
  stringr,      # Manipulación d textos
  ggplot2,      # sistema de gráficos
  ggdist,       # distribuciones y visuales tipo half-eye / histinterval
  patchwork,    # combinación de gráficos ggplot (p1 + p2)
  paletteer,    # acceso a múltiples paletas (viridis, wesanderson, etc.)
  scales,       # helpers de ejes y transformaciones (log10, percentiles)
  grid,         # utilidades de bajo nivel para gráficos (layouts, grobs)
  GGally,       # correlogramas y extensiones ggplot
  sjPlot,       # visualización de modelos (coeficientes, efectos marginales)
  spdep,        # estructuras de vecinos y pruebas de autocorrelación
  spatialreg,   # modelos SAR / SEM / SDM / SDEM / SARAR
  performance,  # métricas de ajuste (pseudo-R², VIF, etc.)
  classInt,     # cortes de intervalos (quantile, jenks, etc.)
  INLA,         # modelos Bayesianos rápidos (CAR/BYM/Leroux)
  lme4,         # modelos mixtos jerárquicos (GLMM)
  MuMIn,        # R² marginal/condicional para modelos mixtos
  googledrive,  # autenticación y manejo de archivos en Google Drive
  qs            # serialización rápida de objetos (.qs)
)

theme_set(theme_minimal(base_size = 13))



## ----carga_ucs-----------------------------------------------------------------------------------------

#  Cargar UCS (limpia el entorno y deja ucs_rao_sf en memoria)

source(here::here("Scripts", "00_funcion_carga_ucs_procesadas_qs.R"),
       encoding = "UTF-8")

ucs_rao_sf <- ucs_rao_sf |>
  sf::st_make_valid() |>
  sf::st_transform(4326) |>
  dplyr::select(id_creado, UCSuelo, AREA_HA, Q)



## ----utilidades----------------------------------------------------------------------------------------

# Utilidades numéricas y paletas
eps        <- 1e-6                                # evita divisiones por 0
safe_log10 <- function(x) log10(pmax(x, eps))     # log10 con piso

pal_continuo <- paletteer::paletteer_c("grDevices::Zissou 1", 100)
pal_qualitat <- paletteer::paletteer_d("wesanderson::Zissou1Continuous", 5)


## ----carga_covariables---------------------------------------------------------------------------------

# ─────────────────────────────────────────────────────────────────────────────
# 2 · Directorio + lectura rápida de CSV
# ─────────────────────────────────────────────────────────────────────────────
dir_cov  <- here::here("Data", "OUT_covars_csv")
leer_cov <- function(f) readr::read_csv(file.path(dir_cov, f),
                                        show_col_types = FALSE)

# ---- DEM (media + CV) -------------------------------------------------------
dem_csv <- leer_cov("OUT_DEM_combinado.csv")
covars_dem <- st_as_sf(
  data.frame(dem_csv, geometry = geojsonsf::geojson_sf(dem_csv$.geo)), crs = 4326
) |>
  dplyr::rename(dem_mean = mean) |>
  dplyr::mutate(
    dem_cv          = stdDev / pmax(abs(dem_mean), eps),
    log_dem_cv      = safe_log10(dem_cv),
    dem_cv_dens     = dem_cv / AREA_HA,
    log_dem_cv_dens = safe_log10(dem_cv_dens),
    dplyr::across(c(dem_mean, dem_cv, log_dem_cv,
                    dem_cv_dens, log_dem_cv_dens),
                  ~ as.numeric(scale(.x)), .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)

# ---- SLOPE (media + CV) -----------------------------------------------------
slope_csv <- leer_cov("OUT_SLOPE_combinado.csv")
covars_slope <- st_as_sf(
  data.frame(slope_csv, geometry = geojsonsf::geojson_sf(slope_csv$.geo)), crs = 4326
) |>
  dplyr::rename(slope_mean = mean) |>
  dplyr::mutate(
    slope_cv          = stdDev / pmax(abs(slope_mean), eps),
    log_slope_cv      = safe_log10(slope_cv),
    slope_cv_dens     = slope_cv / AREA_HA,
    log_slope_cv_dens = safe_log10(slope_cv_dens),
    dplyr::across(c(slope_mean, slope_cv, log_slope_cv,
                    slope_cv_dens, log_slope_cv_dens),
                  ~ as.numeric(scale(.x)), .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)

# ---- LST media espacial -----------------------------------------------------
lst_mean_csv <- leer_cov("OUT_LST_media_espacial_combinado.csv")
covars_lst_mean <- st_as_sf(
  data.frame(lst_mean_csv, geometry = geojsonsf::geojson_sf(lst_mean_csv$.geo)), crs = 4326
) |>
  dplyr::rename(lst_mean = mean) |>
  dplyr::mutate(
    log_lst_mean      = safe_log10(lst_mean),
    lst_mean_dens     = lst_mean / AREA_HA,
    log_lst_mean_dens = safe_log10(lst_mean_dens),
    dplyr::across(c(lst_mean, log_lst_mean, lst_mean_dens, log_lst_mean_dens),
                  ~ as.numeric(scale(.x)), .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)

# ---- LST CV temporal --------------------------------------------------------
lst_cv_csv <- leer_cov("OUT_LST_cv_temporal_combinado.csv")
covars_lst_cv <- st_as_sf(
  data.frame(lst_cv_csv, geometry = geojsonsf::geojson_sf(lst_cv_csv$.geo)), crs = 4326
) |>
  dplyr::rename(lst_cv_temp = mean) |>
  dplyr::mutate(
    log_lst_cv_temp      = safe_log10(lst_cv_temp),
    lst_cv_temp_dens     = lst_cv_temp / AREA_HA,
    log_lst_cv_temp_dens = safe_log10(lst_cv_temp_dens),
    dplyr::across(c(lst_cv_temp, log_lst_cv_temp,
                    lst_cv_temp_dens, log_lst_cv_temp_dens),
                  ~ as.numeric(scale(.x)), .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)

# ---- PRECIP CV temporal -----------------------------------------------------
prcv_csv <- leer_cov("OUT_PRECIP_cv_temporal_combinado.csv")
covars_prcv <- st_as_sf(
  data.frame(prcv_csv, geometry = geojsonsf::geojson_sf(prcv_csv$.geo)), crs = 4326
) |>
  dplyr::rename(precip_cv_temp = mean) |>
  dplyr::mutate(
    log_precip_cv_temp      = safe_log10(precip_cv_temp),
    precip_cv_temp_dens     = precip_cv_temp / AREA_HA,
    log_precip_cv_temp_dens = safe_log10(precip_cv_temp_dens),
    dplyr::across(c(precip_cv_temp, log_precip_cv_temp,
                    precip_cv_temp_dens, log_precip_cv_temp_dens),
                  ~ as.numeric(scale(.x)), .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)

# ---- PRECIP media espacial --------------------------------------------------
prmn_csv <- leer_cov("OUT_PRECIP_media_espacial_combinado.csv")
covars_prmn <- st_as_sf(
  data.frame(prmn_csv, geometry = geojsonsf::geojson_sf(prmn_csv$.geo)), crs = 4326
) |>
  dplyr::rename(precip_mean = mean) |>
  dplyr::mutate(
    log_precip_mean      = safe_log10(precip_mean),
    precip_mean_dens     = precip_mean / AREA_HA,
    log_precip_mean_dens = safe_log10(precip_mean_dens),
    dplyr::across(c(precip_mean, log_precip_mean,
                    precip_mean_dens, log_precip_mean_dens),
                  ~ as.numeric(scale(.x)), .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)



## ------------------------------------------------------------------------------------------------------

url_base   <- "https://srvags.sgc.gov.co/arcgis/rest/services/Mapa_Geologico_Colombia/Mapa_Geologico_Colombia_V2023/MapServer/733/query"
url_geo    <- httr::modify_url(
  url_base,
  query = list(where = "1=1", outFields = "*",
               returnGeometry = "true", f = "geojson")
)

geo_sf_wgs84 <- sf::st_read(url_geo, quiet = TRUE) |>
  sf::st_make_valid() |>
  mutate(
    # LIMPIEZA -----------------------------------------------------------------
    edad_limpia = Edad |>
      str_replace_all("\\?", "") |>
      str_trim() |>
      str_to_lower() |>
      stringi::stri_trans_general("Latin-ASCII") |>
      str_replace_all("-", " ") |>
      str_squish(),
    
    # RECLASIFICACIÓN POR ERA --------------------------------------------------
    era_geo = case_when(
      # 1) CENOZOICO -----------------------------------------------------------
      str_detect(edad_limpia, regex(
        "paleoceno|eoceno|oligoceno|mioceno|plioceno|pleistoceno|holoceno|
         aquitaniano|burdigaliano|langhiano|serravaliano|tortoniano|messiniano|
         zancliano|rupeliano|thanetiano|lutetiano|bartoniano|priaboniano|
         selandiano|daniense|chattiano|cuaternario|mesiniano",
        ignore_case = TRUE)) ~ "Cz",
      
      # 2) MESOZOICO -----------------------------------------------------------
      str_detect(edad_limpia, regex(
        "triasico|jurasico|cretacico|berriasiano|valanginiano|barremiano|
         aptiano|albiano|cenomaniano|turoniano|coniaciano|santoniano|
         campaniano|maastrichtiano",
        ignore_case = TRUE)) ~ "Mz",
      
      # 3) PALEOZOICO ----------------------------------------------------------
      str_detect(edad_limpia, regex(
        "\\bcambrico\\b|\\bordovicico\\b|\\bsilurico\\b|\\bdevonico\\b|
         \\bmississipiano\\b|\\bpridoliano\\b|\\bcarbonifero\\b|
         \\bpennsylvaniano\\b|\\bpermico\\b|\\bpaleozoico\\b",
        ignore_case = TRUE)) ~ "Pz",
      
      # 4) PROTEROZOICO --------------------------------------------------------
      str_detect(edad_limpia, regex(
        "sideriano|rhyaciano|orosiriano|statheriano|calymmiano|ectasiano|
         steniano|toniano|criogenico|ediacariano|mesoproterozoico|
         neoproterozoico|proterozoico",
        ignore_case = TRUE)) ~ "Ptz",
      
      # 5) SIN DATO ------------------------------------------------------------
      TRUE ~ "NA"
    )
  ) |>
  st_transform(4326) |>
  select(descripcion_geo = Descripcion, era_geo, edad_limpia)


## ----procesamiento_datos-------------------------------------------------------------------------------

## Era geológica dominante por intersección ---------------------------
ucs_geo_df <- sf::st_join(
  ucs_rao_sf |> dplyr::select(id_creado),
  geo_sf_wgs84,
  join = sf::st_intersects,
  left = TRUE
) |>
  sf::st_drop_geometry() |>
  dplyr::distinct(id_creado, .keep_all = TRUE)   # si toca >1 era → primera

## Ensamblar modelo_sf (respuesta + covariables + geología) -----------------
modelo_sf <- ucs_rao_sf |>
  # respuesta
  dplyr::mutate(
    Q      = dplyr::if_else(Q == 0, 1e-3, Q),   # evita log(0)
    Qdens  = Q / AREA_HA,
    log_Qdens = log10(Qdens)
  ) |>
  #geologia
  dplyr::left_join(ucs_geo_df,                      by = "id_creado") |>
  #covariables
  dplyr::left_join(sf::st_drop_geometry(covars_dem),     by = c("id_creado","UCSuelo","AREA_HA")) |>
  dplyr::left_join(sf::st_drop_geometry(covars_slope),   by = c("id_creado","UCSuelo","AREA_HA")) |>
  dplyr::left_join(sf::st_drop_geometry(covars_lst_mean),by = c("id_creado","UCSuelo","AREA_HA")) |>
  dplyr::left_join(sf::st_drop_geometry(covars_lst_cv),  by = c("id_creado","UCSuelo","AREA_HA")) |>
  dplyr::left_join(sf::st_drop_geometry(covars_prmn),    by = c("id_creado","UCSuelo","AREA_HA")) |>
  dplyr::left_join(sf::st_drop_geometry(covars_prcv),    by = c("id_creado","UCSuelo","AREA_HA"))

## Vista rápida -----------------------------------------------------------
glimpse(modelo_sf, width = 80)
modelo_df <- sf::st_drop_geometry(modelo_sf)




## ----matriz_vecindad-----------------------------------------------------------------------------------

# desactiva s2 para que spdep use geometría planar
sf::sf_use_s2(FALSE)

# 1 · Simplificar solo vértices, manteniendo la forma general --------------
modelo_sf_s <- sf::st_simplify(
  modelo_sf,
  dTolerance       = 0.0005,   # ≈ 55 m; ajusta si ves artefactos
  preserveTopology = TRUE
)

# 2 · Validar (por si acaso) ------------------------------------------------
modelo_sf_s <- sf::st_make_valid(modelo_sf_s)


# Matriz de vecindario queen  ------------------------------------------------
nb_queen <- spdep::poly2nb(modelo_sf_s, queen = TRUE,
                           snap = 1e-06,    # ajusta si hay errores por vértices
                           useC = TRUE)

# Matriz de pesos -------------------------------------------------------------
lw_queen <- spdep::nb2listw(nb_queen, style = "W", zero.policy = TRUE)



## ------------------------------------------------------------------------------------------------------

# Falta código



## ----moranI_Qdens--------------------------------------------------------------------------------------

# 1. Vector respuesta y vecinos válidos ------------------------------------
y_raw     <- modelo_sf$log_Qdens
valid_idx <- which(!is.na(y_raw) & spdep::card(nb_queen) > 0)

## 2. Vector lógico del mismo largo que nb_queen ----------------------------
keep <- seq_along(nb_queen) %in% valid_idx     # TRUE si la posición es válida

## 3. Sub-lista de pesos coherente ------------------------------------------
lw_sub <- spdep::subset.listw(lw_queen, keep, zero.policy = TRUE)

## 4. Moran I global sin NA --------------------------------------------------
moran_raw <- spdep::moran.test(
  y_raw[keep],
  listw       = lw_sub,
  zero.policy = TRUE
)
moran_raw


## ----LISA----------------------------------------------------------------------------------------------

# LISA
lisa_raw <- spdep::localmoran(y_raw[valid_idx], lw_sub)

# Añade columnas al mismo objeto
modelo_sf_s <- modelo_sf_s |>
  dplyr::mutate(
    Ii_raw   = NA_real_,
    lag_raw  = NA_real_,
    p_Ii_raw = NA_real_,
    cluster_raw = NA_character_
  )
modelo_sf_s$Ii_raw  [valid_idx] <- lisa_raw[, 1]
modelo_sf_s$p_Ii_raw[valid_idx] <- lisa_raw[, 5]
modelo_sf_s$lag_raw[valid_idx]  <- spdep::lag.listw(lw_sub, y_raw[valid_idx])

modelo_sf_s <- modelo_sf_s |>
  dplyr::mutate(
    cluster_raw = dplyr::case_when(
      log_Qdens > mean(log_Qdens, na.rm = TRUE) &
        lag_raw  > mean(lag_raw,  na.rm = TRUE) ~ "High-High",
      log_Qdens < mean(log_Qdens, na.rm = TRUE) &
        lag_raw  < mean(lag_raw,  na.rm = TRUE) ~ "Low-Low",
      log_Qdens > mean(log_Qdens, na.rm = TRUE) &
        lag_raw  < mean(lag_raw,  na.rm = TRUE) ~ "High-Low",
      log_Qdens < mean(log_Qdens, na.rm = TRUE) &
        lag_raw  > mean(lag_raw,  na.rm = TRUE) ~ "Low-High",
      TRUE ~ "No sig."
    ),
    cluster_raw = ifelse(p_Ii_raw > 0.05, "No sig.", cluster_raw)
  )




## ----visualizacion_LISA--------------------------------------------------------------------------------

# ─────────────────────────────────────────────────────────────────────────────
# 0 · Paletas coherentes con el cuaderno
# ─────────────────────────────────────────────────────────────────────────────
pal_continuo <- paletteer::paletteer_c("grDevices::Zissou 1", 100)
pal_qualitat <- paletteer::paletteer_d("wesanderson::Zissou1Continuous", 5)

col_lisa <- c(
  "High-High" = pal_continuo[90],   # rojo
  "Low-Low"   = pal_continuo[10],   # azul
  "High-Low"  = pal_continuo[70],   # naranja
  "Low-High"  = pal_continuo[30],   # celeste
  "No sig."   = "grey90"
)

## ── 1. Calcula las medias (NO se modifica el objeto) ──────────────────────
x0 <- mean(modelo_sf_s$log_Qdens, na.rm = TRUE)
y0 <- mean(modelo_sf_s$lag_raw ,  na.rm = TRUE)

## ── 2. Scatterplot Moran local coherente con la clasificación ─────────────
p_scatter_lisa <- ggplot(modelo_sf_s,
                         aes(x = log_Qdens,
                             y = lag_raw,
                             colour = cluster_raw)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,
              colour = "black", linewidth = 0.5) +
  geom_hline(yintercept = y0, linetype = "dashed", colour = "grey60") +
  geom_vline(xintercept = x0, linetype = "dashed", colour = "grey60") +
  scale_colour_manual(values = col_lisa, name = "Clúster LISA") +
  labs(x = "log₁₀(Q / Área)",
       y = "Lag espacial",
       title = "Diagrama de dispersión Moran local") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

## ── 3. Mapa de clústeres y mapa de Ii (igual que antes) ───────────────────
p_moran_lisa <- ggplot(modelo_sf_s) +
  geom_sf(aes(fill = cluster_raw), colour = NA) +
  scale_fill_manual(values = col_lisa, na.value = "white",
                    name = "Clúster LISA") +
  labs(title = "Mapa: clústeres LISA de log_Qdens") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

p_ii_mapa <- ggplot(modelo_sf_s) +
  geom_sf(aes(fill = Ii_raw), colour = NA) +
  scale_fill_viridis_c(name = "Ii", option = "magma",
                       direction = -1, na.value = "white") +
  labs(title = "Mapa: valor local del estadístico Ii") +
  theme_minimal(base_size = 11)

## ── 4. Tripanel sin leyenda duplicada ─────────────────────────────────────
p_lisa_tripanel <- p_scatter_lisa + p_moran_lisa + p_ii_mapa +
  patchwork::plot_layout(ncol = 3, guides = "auto")

ggplot2::ggsave(
  filename = here::here("Figures", "moran_local_tripanel.png"),
  plot     = p_lisa_tripanel,
  width    = 15, height = 5, dpi = 300
)


