## ----configuracion-------------------------------------------------------------------------------------------
#Para exportar como .R plano
# knitr::purl('05_analisis_glm_logit_hotspots.qmd')

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(
  here,         # manejo de rutas relativas al proyecto
  remotes,      # instalar paquetes desde GitHub
  sf,           # manejo de objetos espaciales vectoriales
  dplyr,        # manipulación de data frames (verbos como filter, mutate, join)
  tidyr,        # transformación de datos (pivot_longer, pivot_wider, etc.)
  readr,        # lectura rápida de archivos CSV
  performance,  # métricas de modelos (R2 de Tjur, VIF, etc.)
  ggdist,       # distribución visual de predicciones (stat_halfeye, intervalos)
  scales,       # transformación de escalas (log10, percentiles, breaks)
  ggplot2,      # sistema de gráficos base
  sjPlot,       # visualización de modelos y efectos marginales
  spdep,        # Análisis de dependencia espacial
  scales,       # Transformación de escalas
  paletteer,    # acceso a múltiples paletas de colores (ej. viridis, wesanderson)
  googledrive,  # autenticación y manipulación de archivos en Google Drive
  patchwork,    # combinación de gráficos ggplot (p1 + p2)
  qs            # guardado y carga rápida de objetos R
)

#Paleta de colores
pal <- wes_palette("Zissou1", 100, type = "continuous")

col_hot  <- pal[90] 

# Ajusta tamaño base de letra para todos los gráficos
theme(base_size = 14)

# === Autenticación Google Drive ===
# googledrive::drive_auth()



## ----carga_pedodiversidad------------------------------------------------------------------------------------

# Carga UCS ya procesadas y armonizadas (ver script externo)
source(here::here("Scripts", "00_funcion_carga_ucs_procesadas_qs.R"), encoding = "UTF-8")

# Asegura geometrías válidas y selecciona columnas clave
ucs_rao_sf <- ucs_rao_sf |> 
  st_make_valid() |> 
  select(id_creado, UCSuelo, AREA_HA, Q)

# Elimina geometrías vacías (requerido para procesar con sf_as_ee)
ucs_rao_sf <- ucs_rao_sf[!st_is_empty(ucs_rao_sf), ]

# Transforma a WGS84 (EPSG:4326), requerido por GEE
ucs_sf_4326 <- st_transform(ucs_rao_sf, 4326)



## ----carga_covariables---------------------------------------------------------------------------------------

# ─────────────────────────────────────────────────────────────────────────────
# 0 · Ajuste de precisión para divisiones seguras
# ─────────────────────────────────────────────────────────────────────────────
eps <- 1e-6                         # ε evita CV = σ/0
safe_log10 <- function(x) log10(pmax(x, eps))

# ─────────────────────────────────────────────────────────────────────────────
# 1 · DEM (media & CV espacial)
# ─────────────────────────────────────────────────────────────────────────────
dem_cv <- readr::read_csv(
  here::here("Data/OUT_covars_csv/OUT_DEM_combinado.csv"),
  show_col_types = FALSE
)

covars_dem <- st_as_sf(
  data.frame(dem_cv, geometry = geojson_sf(dem_cv$.geo)), crs = 4326
) |>
  dplyr::rename(dem_mean = mean) |>
  dplyr::mutate(
    dem_cv          = stdDev / pmax(abs(dem_mean), eps),
    log_dem_cv      = safe_log10(dem_cv),
    dem_cv_dens     = dem_cv / AREA_HA,
    log_dem_cv_dens = safe_log10(dem_cv_dens),
    across(c(dem_cv, log_dem_cv, dem_cv_dens, log_dem_cv_dens, dem_mean),
           ~ scale(.x)[, 1], .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)   

# ─────────────────────────────────────────────────────────────────────────────
# 2 · SLOPE (media & CV espacial)
# ─────────────────────────────────────────────────────────────────────────────
slope_cv <- readr::read_csv(
  here::here("Data/OUT_covars_csv/OUT_SLOPE_combinado.csv"),
  show_col_types = FALSE
)

covars_slope <- st_as_sf(
  data.frame(slope_cv, geometry = geojson_sf(slope_cv$.geo)), crs = 4326
) |>
  dplyr::rename(slope_mean = mean) |>
  dplyr::mutate(
    slope_cv          = stdDev / pmax(abs(slope_mean), eps),
    log_slope_cv      = safe_log10(slope_cv),
    slope_cv_dens     = slope_cv / AREA_HA,
    log_slope_cv_dens = safe_log10(slope_cv_dens),
    across(c(slope_cv, log_slope_cv, slope_cv_dens, log_slope_cv_dens, slope_mean),
           ~ scale(.x)[, 1], .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index, -Q)   

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
  dplyr::rename(lst_mean = mean) |>
  dplyr::mutate(
    log_lst_mean        = safe_log10(lst_mean),
    lst_mean_dens       = lst_mean / AREA_HA,
    log_lst_mean_dens   = safe_log10(lst_mean_dens),
    across(c(lst_mean, log_lst_mean, lst_mean_dens, log_lst_mean_dens),
           ~ scale(.x)[, 1], .names = "{.col}_z")
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
  dplyr::rename(lst_cv_temp = mean) |>
  dplyr::mutate(
    log_lst_cv_temp        = safe_log10(lst_cv_temp),
    lst_cv_temp_dens       = lst_cv_temp / AREA_HA,
    log_lst_cv_temp_dens   = safe_log10(lst_cv_temp_dens),
    across(c(lst_cv_temp, log_lst_cv_temp, lst_cv_temp_dens, log_lst_cv_temp_dens),
           ~ scale(.x)[, 1], .names = "{.col}_z")
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
  dplyr::rename(precip_cv_temp = mean) |>
  dplyr::mutate(
    log_precip_cv_temp        = safe_log10(precip_cv_temp),
    precip_cv_temp_dens       = precip_cv_temp / AREA_HA,
    log_precip_cv_temp_dens   = safe_log10(precip_cv_temp_dens),
    across(c(precip_cv_temp, log_precip_cv_temp, precip_cv_temp_dens, log_precip_cv_temp_dens),
           ~ scale(.x)[, 1], .names = "{.col}_z")
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
  dplyr::rename(precip_mean = mean) |>
  dplyr::mutate(
    log_precip_mean        = safe_log10(precip_mean),
    precip_mean_dens       = precip_mean / AREA_HA,
    log_precip_mean_dens   = safe_log10(precip_mean_dens),
    across(c(precip_mean, log_precip_mean, precip_mean_dens, log_precip_mean_dens),
           ~ scale(.x)[, 1], .names = "{.col}_z")
  ) |>
  dplyr::select(-.geo, -stdDev, -system.index)   




## ----join_covars---------------------------------------------------------------------------------------------

# Une por id_creado, AREA_HA y UCSuelo
modelo_df <- ucs_rao_sf |>
  tidyr::drop_na(Q) |>
  mutate(
    Q = if_else(Q == 0, 1e-3, Q),              # Evita log(0)
    Qdens = Q / AREA_HA,                       # Densidad de diversidad
    log_Qdens = log10(Qdens)                   # Transformación log10
  ) |>
  left_join(covars_dem |> st_drop_geometry(), by = c("id_creado", "AREA_HA", "UCSuelo")) |>
  left_join(covars_slope |> st_drop_geometry(), by = c("id_creado", "AREA_HA", "UCSuelo")) |>
  left_join(covars_lst_cv_temp  |> st_drop_geometry(), by = c("id_creado", "AREA_HA", "UCSuelo")) |>
  left_join(covars_lst_media |> st_drop_geometry(), by = c("id_creado", "AREA_HA", "UCSuelo")) |>
  left_join(covars_precip_cv_temp |> st_drop_geometry(), by = c("id_creado", "AREA_HA", "UCSuelo")) |>
  left_join(covars_precip_media |> st_drop_geometry(), by = c("id_creado", "AREA_HA", "UCSuelo"))

# Verifica que la unión fue exitosa
glimpse(modelo_df)


## ----preparacion_modelado_logit------------------------------------------------------------------------------

# Filtra UCS con covariables completas (sin NA en las variables seleccionadas)
modelo_df_completo <- modelo_df |>
  drop_na(
    log_Qdens,                                # Diversidad logarítmica densificada
    dem_mean, slope_mean,                     # Elevación y pendiente media
    lst_mea, precip_mean,                     # Media de lst y precipitación
    dem_cv_dens, log_dem_cv_dens,             # Densidad y log de CV de elevación
    slope_cv_dens, log_slope_cv_dens,         # Densidad y log de CV de pendiente
    lst_cv_temp, precip_cv_tem,               # CV temporal de LST y precipitación
    log_lst_cv_temp, log_precip_cv_temp,      # log de CV temporal
    lst_cv_temp_dens, precip_cv_temp_dens,    # Densidad y log de CV temporal de LST
    log_lst_cv_temp_dens, log_precip_cv_temp_dens
  )


# Calcula el percentil 95 de diversidad (logarítmica densificada)
umbral_hot95 <- quantile(modelo_df_completo$log_Qdens, 0.95, na.rm = TRUE)

# Crea variable binaria de hotspot (1 si es hotspot, 0 si no)
modelo_df_completo <- modelo_df_completo |>
  mutate(log_Qdens_hot95 = as.integer(log_Qdens >= umbral_hot95))


## ----seleccion_subconjuntos_balanceados----------------------------------------------------------------------

# Cuántos hotspots hay en total
n_hot <- modelo_df_completo |> filter(log_Qdens_hot95 == 1) |> nrow()

# Selección aleatoria de no-hotspots
# ontiene hotspots (1) y un subconjunto aleatorio balanceado de no-hotspots (0)
# Número total de hotspots (clase positiva)
n_hot <- modelo_df_completo |> filter(log_Qdens_hot95 == 1) |> nrow()

# Número de no-hotspots deseado (proporción 1:3)
n_nohot <- n_hot * 4

# Semilla para reproducibilidad
set.seed(123)

# Selección de muestra con proporción 1:3
modelo_df_balanceado <- modelo_df_completo |>
  filter(
    log_Qdens_hot95 == 1 | 
      row_number() %in% sample(which(log_Qdens_hot95 == 0), n_nohot)
  )





## ----ajuste_modelos_logit_B----------------------------------------------------------------------------------

# === Modelo B: Hotspots vs muestra aleatoria (balanceado) ===
glm_hot_bal <- glm(
  formula = log_Qdens_hot95 ~ 
  dem_mean_z + slope_mean_z + lst_mean_z + precip_mean_z +
  log_dem_cv_z + log_slope_cv_z + log_lst_cv_temp_z + log_precip_cv_temp_z +
  log_dem_cv_dens_z + log_slope_cv_dens_z + log_lst_cv_temp_dens_z + precip_cv_temp_dens_z,
  data = modelo_df_balanceado,
  family = binomial()
)

# Agrega predicción y residuales a modelo_df_balanceado
modelo_df_balanceado <- modelo_df_balanceado |>
  mutate(
    prob_hot_bal = predict(glm_hot_bal, type = "response"),
    resid_hot_bal = residuals(glm_hot_bal, type = "response")
  )

# Reporta ajuste
summary(glm_hot_bal)
performance::r2_tjur(glm_hot_bal)



## ------------------------------------------------------------------------------------------------------------

p_coeficientes <- plot_model(glm_hot_bal, 
                             type = "est", 
                             vline.color = "red", 
                             show.values = TRUE, 
                             transform = NULL,
                             value.offset = .3) +
  labs(title = "Coeficientes estimados del modelo") +
  theme_sjplot2() +
  scale_color_sjplot("blambus")

p_coeficientes

# Guarda la figura
ggsave(here("Figures", "coeficientes_logit_b.png"),
       plot = p_coeficientes,
       width = 6,
       height = 3,
       dpi = 350)



## ------------------------------------------------------------------------------------------------------------

# log_dem_cv_dens_z: log(CV de elevación densificado por área), estandarizado
p_margin_log_dem_cv_dens_z <- plot_model(glm_hot_bal, type = "pred", terms = "log_dem_cv_dens_z [all]", title = NULL) +
  labs(x = "log(CV de DEM, densificado y estándar)",
       y = "Probabilidad predicha de hotspot") +
  theme_minimal()

# log_slope_cv_dens_z: log(CV de pendiente densificado por área), estandarizado
p_margin_log_slope_cv_dens_z <- plot_model(glm_hot_bal, type = "pred", terms = "log_slope_cv_dens_z [all]", title = NULL) +
  labs(x = "log(CV de SLOPE, densificado y estándar)",
       y = "Probabilidad predicha de hotspot") +
  theme_minimal()

# log_lst_cv_temp_dens_z: log(CV de temperatura superficial densificado por área), estandarizado
p_margin_log_lst_cv_dens_z <- plot_model(glm_hot_bal, type = "pred", terms = "log_lst_cv_temp_dens_z [all]", title = NULL) +
  labs(x = "log(CV temporal de LST, densificado y estándar)",
       y = "Probabilidad predicha de hotspot") +
  theme_minimal()

# Mosaico de curvas marginales
p_curvas_marginales_logit_b <- p_margin_log_dem_cv_dens_z + p_margin_log_slope_cv_dens_z + p_margin_log_lst_cv_dens_z

# Visualiza las curvas
p_curvas_marginales_logit_b

# Guarda la figura
ggsave(here("Figures", "curvas_marginales_modelo_logit_b.png"),
       plot = p_curvas_marginales_logit_b,
       width = 9,
       height = 4,
       dpi = 350)




## ------------------------------------------------------------------------------------------------------------

p_resid_log_dem_cv_dens_z <- ggplot(modelo_df_balanceado, aes(x = log_dem_cv_dens_z, y = resid_hot_bal)) +
  geom_point(alpha = 0.3, color = "#56B4E9") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Residuos", x = "log(CV de elevación, densificado y estándar)") +
  theme_minimal()

p_resid_log_slope_cv_dens_z <- ggplot(modelo_df_balanceado, aes(x = log_slope_cv_dens_z, y = resid_hot_bal)) +
  geom_point(alpha = 0.3, color = "#56B4E9") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Residuos", x = "log(CV de pendiente, densificado y estándar)") +
  theme_minimal()

p_resid_log_lst_cv_temp_dens_z <- ggplot(modelo_df_balanceado, aes(x = log_lst_cv_temp_dens_z, y = resid_hot_bal)) +
  geom_point(alpha = 0.3, color = "#56B4E9") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Residuos", x = "log(CV temporal de temperatura superficial, densificado y estándar)") +
  theme_minimal()


p_resid_balanceado_logit <- p_resid_log_dem_cv_dens_z + p_resid_log_slope_cv_dens_z + p_resid_log_lst_cv_temp_dens_z

ggsave(here("Figures", "mosaico_residuales_logit.png"),
       plot = p_resid_balanceado_logit,
       width = 12,
       height = 5,
       dpi = 350)


## ------------------------------------------------------------------------------------------------------------

# Extrae los hotspots observados como puntos
hotspots_obs_sf <- modelo_df_balanceado |> 
  filter(log_Qdens_hot95 == 1)

# Mapa 1: solo hotspots observados como puntos, UCS en fondo
p_mapa_obs <- ggplot() +
  geom_sf(data = modelo_df_balanceado, fill = NA, color = "grey90", size = 0.1) +
  geom_sf(data = hotspots_obs_sf, color = col_hot, size = 2) +
  labs(title = "Hotspots observados (puntos)") +
  theme_minimal()

# Mapa 2: probabilidad predicha
p_mapa_pred <- ggplot(modelo_df_balanceado) +
  geom_sf(aes(fill = prob_hot_bal), color = NA) +
  scale_fill_gradientn(
  colours = rev(viridis::plasma(100)),  # ← rev() invierte los colores
  limits = c(0, 0.6),
  oob = scales::squish,
  name = "Probabilidad\npredicha"
  ) +
  labs(title = "Probabilidad predicha de hotspot") +
  theme_minimal()

# Mapa 3: residuales
p_mapa_resid <- ggplot(modelo_df_balanceado) +
  geom_sf(aes(fill = resid_hot_bal), color = NA) +
  scale_fill_gradientn(
    colours = viridis::viridis(100),
    limits = c(-0.5, 0.5),
    name = "Residuo"
  ) +
  labs(title = "Residuales del modelo") +
  theme_minimal()

# Mosaico con patchwork
p_mapas_logit <- p_mapa_obs + p_mapa_pred + p_mapa_resid + plot_layout(ncol = 3)

# Guardar la figura
ggsave(
  filename = here("Figures", "mosaico_mapa_prediccion_logit.png"),
  plot = p_mapas_logit,
  width = 12,
  height = 5.5,
  dpi = 350
)



## ----analisis_moran_I----------------------------------------------------------------------------------------

# Usamos los centroides de los UCS para definir relaciones espaciales
modelo_centroides <- modelo_df_balanceado |> 
  st_centroid(of_largest_polygon = TRUE)

# Extraemos coordenadas de los centroides
coords <- st_coordinates(modelo_centroides)

# Construye vecinos por k más cercanos (k=6 es común en análisis ecológicos y espaciales)
k_vecinos <- knearneigh(coords, k = 6)
vecindario <- knn2nb(k_vecinos)

# Matriz de pesos espacial estandarizada (filas suman 1)
pesos <- nb2listw(vecindario, style = "W", zero.policy = TRUE)

# Moran I global sobre residuales del modelo logit
moran_global <- moran.test(modelo_df_balanceado$resid_hot_bal, listw = pesos, zero.policy = TRUE)

print(moran_global)




## ------------------------------------------------------------------------------------------------------------

# Moran local (LISA)
moran_local <- localmoran(modelo_df_balanceado$resid_hot_bal, listw = pesos, zero.policy = TRUE)

# Calcula el lag espacial de los residuos (para el scatterplot)
modelo_df_balanceado <- modelo_df_balanceado |>
  mutate(
    local_moran_I = moran_local[, 1],
    p_valor_local = moran_local[, 5],
    lag_resid = lag.listw(pesos, resid_hot_bal)
  )

# Clasifica tipo de agrupamiento espacial
modelo_df_balanceado <- modelo_df_balanceado |>
  mutate(
    cluster_tipo = case_when(
      resid_hot_bal > 0 & lag_resid > 0 ~ "High-High",
      resid_hot_bal < 0 & lag_resid < 0 ~ "Low-Low",
      resid_hot_bal > 0 & lag_resid < 0 ~ "High-Low",
      resid_hot_bal < 0 & lag_resid > 0 ~ "Low-High",
      TRUE ~ "No significativo"
    ),
    cluster_tipo = ifelse(p_valor_local > 0.05, "No significativo", cluster_tipo)
  )



## ------------------------------------------------------------------------------------------------------------

# Define colores personalizados desde paleta Zissou1 (viridis-like)
pal <- wes_palette("Zissou1", 100, type = "continuous")
colores_lisa <- c(
  "High-High"        = pal[90],   # rojo intenso
  "Low-Low"          = pal[10],   # azul oscuro
  "High-Low"         = pal[70],   # naranja
  "Low-High"         = pal[30],   # celeste
  "No significativo" = "grey90"   # gris neutro
)

# Gráfico 1: Scatterplot de Moran local coloreado por tipo de agrupamiento
p_scatter_lisa <- ggplot(modelo_df_balanceado, aes(x = resid_hot_bal, y = lag_resid)) +
  geom_point(aes(color = cluster_tipo), alpha = 0.8, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  scale_color_manual(values = colores_lisa) +
  labs(
    title = "Diagrama de dispersión Moran local",
    x = "Residuo del modelo",
    y = "Lag espacial",
    color = "Tipo de agrupamiento"
  ) +
  theme_minimal()

# Gráfico 2: Mapa LISA de agrupamientos espaciales
p_moran_lisa <- ggplot(modelo_df_balanceado) +
  geom_sf(aes(fill = cluster_tipo), color = NA) +
  scale_fill_manual(values = colores_lisa, na.value = "white") +
  labs(title = "Mapa: agrupamientos LISA (residuales)",
       fill = "Tipo de agrupamiento") +
  theme_minimal()

# Gráfico 3: Mapa de valores Ii (magnitude de autocorrelación local)
p_ii_mapa <- ggplot(modelo_df_balanceado) +
  geom_sf(aes(fill = local_moran_I), color = NA) +
  scale_fill_viridis_c(name = "Ii", option = "magma", direction = -1) +
  labs(title = "Mapa: valor del estadístico local (Ii)") +
  theme_minimal()

# Composición de los tres gráficos
p_lisa_tripanel <- p_scatter_lisa + p_moran_lisa + p_ii_mapa + 
  plot_layout(ncol = 3)

# Mostrar y guardar
print(p_lisa_tripanel)

ggsave(
  here::here("Figures", "moran_local_composicion_logit.png"),
  plot = p_lisa_tripanel,
  width = 15,
  height = 5,
  dpi = 300
)


