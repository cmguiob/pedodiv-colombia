## ----configuracion-------------------------------------------------------------------------------------
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
  grid,
  GGally,       # mapa de correlación
  sjPlot,       # visualización de modelos y efectos marginales
  spdep,        # Análisis de dependencia espacial
  scales,       # Transformación de escalas
  classInt,     # cortes con cuantiles 
  paletteer,    # acceso a múltiples paletas de colores (ej. viridis, wesanderson)
  googledrive,  # autenticación y manipulación de archivos en Google Drive
  patchwork,    # combinación de gráficos ggplot (p1 + p2)
  qs            # guardado y carga rápida de objetos R
)


# Ajusta tamaño base de letra para todos los gráficos
theme(base_size = 14)

# === Autenticación Google Drive ===
# googledrive::drive_auth()



## ----carga_pedodiversidad------------------------------------------------------------------------------

# Carga UCS ya procesadas y armonizadas (ver script externo)
source(here::here("Scripts", "00_funcion_carga_ucs_procesadas_qs.R"), encoding = "UTF-8")

# Asegura geometrías válidas y selecciona columnas clave
ucs_rao_sf <- ucs_rao_sf |> 
  st_make_valid() |> 
  select(id_creado, UCSuelo, AREA_HA, Q)

# Elimina geometrías vacías (requerido para procesar con sf_as_ee)
ucs_rao_sf <- ucs_rao_sf[!st_is_empty(ucs_rao_sf), ]

# Transforma a WGS84 (EPSG:4326), requerido por GEE
ucs_sf_4326 <- st_transform(ucs_rao_sf, 9377)



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




## ----unir_variables------------------------------------------------------------------------------------

# Une por id_creado, AREA_HA y UCSuelo
modelo_df <- ucs_rao_sf |>
  tidyr::drop_na(Q) |>
  mutate(
    Q = if_else(Q == 0, 1e-3, Q),              # Evita log(0)
    Qdens = Q / AREA_HA,                       # Densidad de diversidad
    log_Qdens = log10(Qdens)                   # Transformación log10
  ) |>
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

# Verifica que la unión fue exitosa
glimpse(modelo_df)


## ----preparacion_modelado------------------------------------------------------------------------------

# Filtra UCS con covariables completas (sin NA en las variables seleccionadas)
modelo_df_completo <- modelo_df |>
  drop_na(
    log_Qdens,                                # Diversidad logarítmica densificada
    dem_mean, slope_mean,                     # Elevación y pendiente media
    lst_mean, precip_mean,                     # Media de lst y precipitación
    dem_cv_dens, log_dem_cv_dens,             # Densidad y log de CV de elevación
    slope_cv_dens, log_slope_cv_dens,         # Densidad y log de CV de pendiente
    lst_cv_temp, precip_cv_temp,               # CV temporal de LST y precipitación
    log_lst_cv_temp, log_precip_cv_temp,      # log de CV temporal
    lst_cv_temp_dens, precip_cv_temp_dens,    # Densidad y log de CV temporal de LST
    log_lst_cv_temp_dens, log_precip_cv_temp_dens
  )


# Calcula el percentil 95 de diversidad (logarítmica densificada)
umbral_hot95 <- quantile(modelo_df_completo$log_Qdens, 0.95, na.rm = TRUE)

# Crea variable binaria de hotspot (1 si es hotspot, 0 si no)
modelo_df_completo <- modelo_df_completo |>
  mutate(log_Qdens_hot95 = as.integer(log_Qdens >= umbral_hot95))


## ----subconjuntos_balanceados--------------------------------------------------------------------------

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





## ------------------------------------------------------------------------------------------------------

# Umbral de colinealidad (Pearson)
r_thr <- 0.70      


# CANDIDATOS: define la fórmula *antes* de ajustar el modelo ------------------------------

form_candidatos <- 
  log_Qdens_hot95 ~ dem_mean_z + slope_mean_z + lst_mean_z + precip_mean_z +
  log_dem_cv_z + log_slope_cv_z + log_lst_cv_temp_z + log_precip_cv_temp_z +
  log_dem_cv_dens_z + log_slope_cv_dens_z + 
  log_lst_cv_temp_dens_z + precip_cv_temp_dens_z

vars_cand <- all.vars(form_candidatos)[-1]   # excluye la respuesta


# Matriz de correlaciones ------------------------------------------------------------------

mat_cor <- 
  modelo_df_balanceado |>
  st_drop_geometry()  |>                      # geometría no aporta a la cor
  select(all_of(vars_cand)) |>
  cor(use = "pairwise.complete.obs")


# Visualización de correlograma -------------------------------------------------------------

# 1 · Colores -------------------------------------------------------------------------------

col_low  <- "#56B4E9"
col_mid  <- "#F8F0C3FF"
col_high <- "#FFD700"

# Heat-map ----------------------------------------------------------------------------------
p_corr_covs <- ggcorr(mat_cor,
       low        = col_low,
       mid        = col_mid,
       high       = col_high,
       midpoint   = 0,
       hjust      = 0.9,
       size       = 4,
       color      = "grey60",
       label      = TRUE,
       label_alpha = TRUE,
       label_size  = 3,
       label_round = 2) +
  ggtitle("Matriz de correlaciones entre predictores (Pearson)") +
  theme_minimal(base_size = 12) +
  coord_cartesian(clip = "off") +                # ← deja dibujar fuera del panel
  theme(
    panel.clip  = "off",                         # idem (≥ ggplot2 3.4)
    plot.margin = unit(c(5, 5, 5, 90), "pt")    # más espacio a la izquierda
  )


# Guarda la figura
ggsave(here("Figures", "correlacion_covariables.png"),
       plot = p_corr_covs,
       width = 6,
       height = 4,
       dpi = 350)



## ----tabla_correlacion---------------------------------------------------------------------------------

## Tabla de pares con |r| > r_thr
pares_altos <- 
  as.data.frame(as.table(mat_cor)) |>                  # pasa a long
  rename(var1 = Var1, var2 = Var2, r = Freq) |>
  filter(var1 != var2, abs(r) >= r_thr) |>             # quita diagonal
  mutate(across(c(var1, var2), as.character)) |>
  rowwise() |>                                         # ordena alfabéticamente
  mutate(pair_id = paste(sort(c(var1, var2)), collapse = "_")) |> 
  ungroup() |>
  distinct(pair_id, .keep_all = TRUE) |>               # evita duplicados A~B / B~A
  arrange(desc(abs(r)))

print(pares_altos, n = Inf)


## ----ajuste_modelo-------------------------------------------------------------------------------------

# Selecciona explícitamente las variables que intervendrán
vars_glm <- c(
  "log_Qdens_hot95",
  "dem_mean_z",  "precip_mean_z",
  "log_slope_cv_dens_z",
  "log_lst_cv_temp_dens_z", "precip_cv_temp_dens_z"
)

# Deja sólo esas columnas + geometría  y elimina cualquier NA
modelo_df_glm <- modelo_df_balanceado |>
  dplyr::select(all_of(vars_glm), geometry) |>
  tidyr::drop_na()

#  Ajusta el logit;  na.action = na.exclude  →  predict() devuelve NAs
glm_hot_bal <- glm(
  log_Qdens_hot95 ~ .,
  data      = st_drop_geometry(modelo_df_glm),  # glm no necesita geometría
  family    = binomial(),
  na.action = na.exclude            # conserva longitud original
)

# Añade predicciones y residuos (longitud coincide con modelo_df_glm)
modelo_df_glm <- modelo_df_glm |>
  mutate(
    prob_hot_bal  = predict(glm_hot_bal, type = "response"),
    resid_hot_bal = residuals(glm_hot_bal, type = "response")
  )

# 5.  Breve diagnóstico
summary(glm_hot_bal)
print(performance::r2_tjur(glm_hot_bal))



## ----visualizacion_coeficientes------------------------------------------------------------------------

p_coeficientes <- plot_model(glm_hot_bal, 
                             type = "est", 
                             vline.color = "black", 
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



## ----visualizacion_marginales--------------------------------------------------------------------------

# ─────────────────────────────────────────────────────────────────────────────
# 1 · Curvas marginales (efectos predichos) para los 5 predictores del modelo
# ─────────────────────────────────────────────────────────────────────────────


# Helper ─ evita repetir theme & estilo de eje Y
marg_plot <- function(var, xlbl){
  plot_model(glm_hot_bal,
             type  = "pred",
             terms = sprintf("%s [all]", var),
             title = NULL) +                     # sin título interno
    labs(x = xlbl, y = "P(hotspot)") +
    theme_minimal(base_size = 11)
}

# ── Generar los cinco gráficos ──────────────────────────────────────────────
p_dem_mean_z            <- marg_plot("dem_mean_z",
                                     "Elevación media (z)")
p_precip_mean_z         <- marg_plot("precip_mean_z",
                                     "Precipitación media (z)")
p_log_slope_cv_dens_z   <- marg_plot("log_slope_cv_dens_z",
                                     "log₁₀ CV pend/Área (z)")
p_log_lst_cv_temp_dens_z<- marg_plot("log_lst_cv_temp_dens_z",
                                     "log₁₀ CV LST temp/Área (z)")
p_precip_cv_temp_dens_z <- marg_plot("precip_cv_temp_dens_z",
                                     "CV precip temp/Área (z)")

# ─────────────────────────────────────────────────────────────────────────────
# 2 · Mosaico 3×2  (la última celda queda vacía para mantener proporción)
# ─────────────────────────────────────────────────────────────────────────────
p_curvas_marginales <- p_dem_mean_z + p_precip_mean_z + p_log_slope_cv_dens_z + p_log_lst_cv_temp_dens_z + p_precip_cv_temp_dens_z + plot_layout(ncol = 5) 

p_curvas_marginales

# Guardar si se desea
ggsave(
  here::here("Figures", "p_curvas_marginales.png"),
  plot   = p_curvas_marginales,
  width  = 13, height = 3.2, dpi = 350
)



## ----visualizaicon_residuales--------------------------------------------------------------------------

resid_plot <- function(var, xlbl){
  ggplot(modelo_df_glm,
         aes(x = .data[[var]], y = resid_hot_bal)) +
    geom_point(alpha = .25, colour = "#56B4E9", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = xlbl, y = "Residuo") +
    theme_minimal(base_size = 11)
}

p_residuos_1fila <-
  resid_plot("dem_mean_z",              "Elevación media (z)")          +
  resid_plot("precip_mean_z",           "Precip. media (z)")            +
  resid_plot("log_slope_cv_dens_z",     "log₁₀ CV pend/Área (z)")       +
  resid_plot("log_lst_cv_temp_dens_z",  "log₁₀ CV LST temp/Área (z)")   +
  resid_plot("precip_cv_temp_dens_z",   "CV precip temp/Área (z)")      +
  plot_layout(ncol = 5)


p_residuos_1fila

ggsave(
  here::here("Figures", "mosaico_residuales_logit.png"),
  plot   = p_residuos_1fila,
  width  = 13, height = 3.2, dpi = 350
)


## ----visualizacion_mapas-------------------------------------------------------------------------------

pal_zissou <- paletteer_c("grDevices::Zissou 1", 100)
col_hot  <- pal_zissou[90] 

# Extrae los hotspots observados como puntos
hotspots_obs_sf <- modelo_df_glm |> 
  filter(log_Qdens_hot95 == 1)

# Mapa 1: solo hotspots observados como puntos, UCS en fondo
p_mapa_obs <- ggplot() +
  geom_sf(data = modelo_df_glm, fill = NA, color = "grey90", size = 0.1) +
  geom_sf(data = hotspots_obs_sf, color = col_hot, size = 2) +
  labs(title = "Hotspots observados (puntos)") +
  theme_minimal()

# Mapa 2: probabilidad predicha
# Genera 5 cortes (quintiles) sobre la columna de probabilidad
cortes <- classInt::classIntervals(modelo_df_glm$prob_hot_bal,
                                   n = 5, style = "quantile")$brks

# Mapa con los 5 colores
p_mapa_pred <- ggplot(modelo_df_glm) +
  geom_sf(aes(fill = cut(prob_hot_bal, cortes,
                         include.lowest = TRUE, dig.lab = 3)),
          colour = NA) +
  scale_fill_manual(
    values = rev(viridis::plasma(5)),
    name   = "Cuantiles de p"
  ) +
  labs(title = "Probabilidad predicha de hotspot (quintiles)") +
  theme_minimal()

# Mapa 3: residuales
p_mapa_resid <- ggplot(modelo_df_glm) +
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



## ----analisis_moran_I----------------------------------------------------------------------------------

# 1 · Centroides en CRS 9377 (metros) ----------------------------------------
centros_9377 <- modelo_df_glm |>
  st_centroid(of_largest_polygon = TRUE) |>
  st_transform(9377)
xy <- st_coordinates(centros_9377)

# 2 · Vecindad única hasta el radio máximo -----------------------------------
r_max  <- 300e3                     # 300 km → m
nb_max <- dnearneigh(xy, 0, r_max)  # índices de vecinos
d_max  <- nbdists(nb_max, xy)       # distancias (m)

# 3 · Función protegida contra “sin-vecinos” ---------------------------------
get_MI_fast <- function(r_km){

  r_m <- r_km * 1000                # km → m

  # 3·1  Copiamos y recortamos la vecindad
  nb_i <- nb_max
  nb_i$neighbours <- Map(\(v, d) v[d <= r_m],
                         nb_max$neighbours,
                         d_max)

  # 3·2  ¿algún polígono quedó sin vecinos?
  if (any(lengths(nb_i$neighbours) == 0)) {
    return(tibble(dist_km = r_km,
                  I       = NA_real_,
                  p.value = NA_real_))
  }

  # 3·3  Pesos y Morán I
  lw_i <- nb2listw(nb_i, style = "W", zero.policy = TRUE)
  mi   <- moran.test(modelo_df_glm$resid_hot_bal, lw_i,
                     zero.policy = TRUE)

  tibble(dist_km = r_km,
         I       = mi$estimate["Moran I"],
         p.value = mi$p.value)
}

# 4 · Ejecutamos para radios 25-300 km ---------------------------------------
radios_km <- seq(25, 300, by = 25)
moran_tab <- map_dfr(radios_km, get_MI_fast)

# 5 · Gráfico  I  vs  distancia  (p < 0.05 en rojo) --------------------------
pal_sig <- c(`TRUE`  = "#d73027",   # rojo   = significativo
             `FALSE` = "grey60")    # gris   = no sig.

g_moran <- ggplot(moran_tab,
                  aes(dist_km, I, colour = p.value < 0.05)) +
  geom_line(na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_colour_manual(values = pal_sig,
                      labels = c("No", "Sí"),
                      name   = "p < 0.05") +
  labs(x = "Radio (km)",
       y = "Moran I global",
       title = "Autocorrelación espacial de los residuos según distancia") +
  theme_minimal()

print(g_moran)




## ----analisis_LISA-------------------------------------------------------------------------------------

# Moran local (LISA)
moran_local <- localmoran(modelo_df_glm$resid_hot_bal, listw = pesos, zero.policy = TRUE)

# Calcula el lag espacial de los residuos (para el scatterplot)
modelo_df_glm <- modelo_df_glm |>
  mutate(
    local_moran_I = moran_local[, 1],
    p_valor_local = moran_local[, 5],
    lag_resid = lag.listw(pesos, resid_hot_bal)
  )

# Clasifica tipo de agrupamiento espacial
modelo_df_glm <- modelo_df_glm |>
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



## ----visualizacion_LISA--------------------------------------------------------------------------------

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
p_scatter_lisa <- ggplot(modelo_df_glm, aes(x = resid_hot_bal, y = lag_resid)) +
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
p_moran_lisa <- ggplot(modelo_df_glm) +
  geom_sf(aes(fill = cluster_tipo), color = NA) +
  scale_fill_manual(values = colores_lisa, na.value = "white") +
  labs(title = "Mapa: agrupamientos LISA (residuales)",
       fill = "Tipo de agrupamiento") +
  theme_minimal()

# Gráfico 3: Mapa de valores Ii (magnitude de autocorrelación local)
p_ii_mapa <- ggplot(modelo_df_glm) +
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


