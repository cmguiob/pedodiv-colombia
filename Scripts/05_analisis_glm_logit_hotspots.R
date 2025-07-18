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
ucs_sf_4326  <- ucs_rao_sf |> 
  st_make_valid() |> 
  select(id_creado, UCSuelo, AREA_HA, Q) |>
  st_transform(4326)

# Elimina geometrías vacías (requerido para procesar con sf_as_ee)
ucs_sf_4326 <- ucs_sf_4326[!st_is_empty(ucs_sf_4326), ]




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
modelo_df <- ucs_sf_4326 |>
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

## Filtra UCS con covariables completas y crea la variable hotspot ------------

modelo_df_completo <- modelo_df |>
  drop_na(
    # diversidad
    log_Qdens,
    # magnitudes
    dem_media, pendiente_media, lst_media, precip_media,
    # CV densificado + sus log-10
    dem_cv_densidad,  log_dem_cv_densidad,
    pendiente_cv_densidad,  log_pendiente_cv_densidad,
    lst_cv_temporal_densidad,  log_lst_cv_temporal_densidad,
    precip_cv_temporal_densidad, log_precip_cv_temporal_densidad,
    # CV “puro” (temporal) + log-10
    lst_cv_temporal,  log_lst_cv_temporal,
    precip_cv_temporal, log_precip_cv_temporal
  )

# Umbral P95 para definir hotspot de diversidad logarítmica-densificada
umbral_hot95 <- quantile(modelo_df_completo$log_Qdens, 0.95, na.rm = TRUE)

modelo_df_completo <- modelo_df_completo |>
  mutate(hotspot_95 = as.integer(log_Qdens >= umbral_hot95))


## ----subconjuntos_balanceados--------------------------------------------------------------------------


# contiene hotspots (1) y un subconjunto aleatorio balanceado de no-hotspots (0)
# Número total de hotspots (clase positiva)
n_hot   <- modelo_df_completo |> filter(hotspot_95 == 1) |> nrow()

# Número de no-hotspots deseado (proporción 1:3)
n_nohot <- n_hot * 4   

# Semilla para reproducibilidad
set.seed(123)

# Selección de muestra con proporción 1:3
modelo_df_balanceado <- modelo_df_completo |>
  mutate(fila = row_number()) |>
  filter(
    hotspot_95 == 1 |
      fila %in% sample(which(hotspot_95 == 0), n_nohot)
  ) |>
  select(-fila)



## ----analisis_correlacion------------------------------------------------------------------------------

# Umbral de colinealidad (Pearson)
r_thr <- 0.70   # umbral |r|    


# Define la fórmula *antes* de ajustar el modelo -----------------------------------------

form_candidatos <-
  hotspot_95 ~ dem_media_z + pendiente_media_z + lst_media_z + precip_media_z +
  log_dem_cv_z + log_pendiente_cv_z + log_lst_cv_temporal_z + log_precip_cv_temporal_z +
  log_dem_cv_densidad_z + log_pendiente_cv_densidad_z +
  log_lst_cv_temporal_densidad_z + precip_cv_temporal_densidad_z

vars_cand <- all.vars(form_candidatos)[-1]   # excluye la respuesta


# Matriz de correlaciones ------------------------------------------------------------------

mat_cor <- modelo_df_balanceado |>
  st_drop_geometry() |>
  select(all_of(vars_cand)) |>
  cor(use = "pairwise.complete.obs")


# Visualización de correlograma -------------------------------------------------------------

#Define paleta de colores
pal_low  <- "#56B4E9"; pal_mid <- "#F8F0C3FF"; pal_high <- "#FFD700"

# Crea mapa de calor de correlaciones Pearson
p_corr_covs <- ggcorr(
  mat_cor,
  low         = pal_low,
  mid         = pal_mid,
  high        = pal_high,
  midpoint    = 0,
  hjust       = 0.9,
  size        = 4,
  color       = "grey60",
  label       = TRUE,
  label_alpha = TRUE,
  label_size  = 3,
  label_round = 2
) +
  ggtitle("Matriz de correlaciones entre predictores (Pearson)") +
  theme_minimal(base_size = 12) +
  coord_cartesian(clip = "off") +
  theme(panel.clip = "off",
        plot.margin = unit(c(5, 5, 5, 90), "pt"))


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

# columnas finales del modelo
vars_glm <- c(
  "hotspot_95",
  "dem_media_z",  "precip_media_z",
  "log_pendiente_cv_densidad_z",
  "log_lst_cv_temporal_densidad_z", "precip_cv_temporal_densidad_z"
)

# Deja sólo esas columnas + geometría  y elimina cualquier NA
modelo_df_glm <- modelo_df_balanceado |>
  dplyr::select(all_of(vars_glm), geometry) |>
  tidyr::drop_na()

#  Ajusta el logit;  na.action = na.exclude  →  predict() devuelve NAs
glm_hot_bal <- glm(
  hotspot_95 ~ .,
  data      = st_drop_geometry(modelo_df_glm), #glm no necesita geometría
  family    = binomial(),
  na.action = na.exclude
)


# Añade predicciones y residuos (longitud coincide con modelo_df_glm)
modelo_df_glm <- modelo_df_glm |>
  mutate(
    prob_hot_bal  = predict(glm_hot_bal, type = "response"),
    resid_hot_bal = residuals(glm_hot_bal, type = "response")
  )

# Breve diagnóstico
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


# Helper ─ evita repetir theme & estilo de eje Y -------------------------------------------
marg_plot <- function(var, xlbl){
  plot_model(glm_hot_bal,
             type  = "pred",
             terms = sprintf("%s [all]", var),
             title = NULL) +                     # sin título interno
    labs(x = xlbl, y = "P(hotspot)") +
    theme_minimal(base_size = 11)
}

# Generar los cinco gráficos --------------------------------------------------------------
p_dem_media_z            <- marg_plot("dem_media_z", "Elevación media (z)")
p_precip_media_z         <- marg_plot("precip_media_z", "Precipitación media (z)")
p_log_slope_cv_dens_z   <- marg_plot("log_pendiente_cv_densidad_z","log₁₀ CV pend/Área (z)")
p_log_lst_cv_temp_dens_z<- marg_plot("log_lst_cv_temporal_densidad_z",
                                     "log₁₀ CV LST temp/Área (z)")
p_precip_cv_temp_dens_z <- marg_plot("precip_cv_temporal_densidad_z",
                                     "CV precip temp/Área (z)")


# Mosaico 3×2  (la última celda queda vacía para mantener proporción) ----------------------

p_curvas_marginales <- p_dem_media_z + p_precip_media_z + p_log_slope_cv_dens_z + p_log_lst_cv_temp_dens_z + p_precip_cv_temp_dens_z + plot_layout(ncol = 5) 

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

p_residuos <- 
  resid_plot("dem_media_z",                 "Elevación media (z)")        +
  resid_plot("precip_media_z",              "Precip. media (z)")          +
  resid_plot("log_pendiente_cv_densidad_z", "log₁₀ CV pend/Área (z)")     +
  resid_plot("log_lst_cv_temporal_densidad_z","log₁₀ CV LST temp/Área (z)")+
  resid_plot("precip_cv_temporal_densidad_z","CV precip temp/Área (z)")   +
  plot_layout(ncol = 5)


ggsave(
  here::here("Figures", "mosaico_residuales_logit.png"),
  plot   = p_residuos,
  width  = 13, height = 3.2, dpi = 350
)


## ----visualizacion_mapas-------------------------------------------------------------------------------

# Paleta “Zissou 1” (100 tonos); tomo un rojo intenso para marcar hotspots
pal_zissou <- paletteer::paletteer_c("grDevices::Zissou 1", 100)
col_hot    <- pal_zissou[90]     # rojo-naranja fuerte

# ── 1 · Hotspots observados -------------------------------------------------
# (puntos) sobre el contorno de las UCS del data-set usado en el modelo
hotspots_obs_sf <- modelo_df_glm |>
  filter(hotspot_95 == 1)        # ← antes era log_Qdens_hot95

p_mapa_obs <- ggplot() +
  geom_sf(data = modelo_df_glm, fill = NA, colour = "grey90", linewidth = .1) +
  geom_sf(data = hotspots_obs_sf, colour = col_hot, size = 2) +
  labs(title = "Hotspots observados (puntos)") +
  theme_minimal(base_size = 11)

# ── 2 · Probabilidad predicha (quintiles) -----------------------------------
cortes <- classInt::classIntervals(modelo_df_glm$prob_hot_bal,
                                   n = 5, style = "quantile")$brks

p_mapa_pred <- ggplot(modelo_df_glm) +
  geom_sf(aes(fill = cut(prob_hot_bal, cortes,
                         include.lowest = TRUE, dig.lab = 3)),
          colour = NA) +
  scale_fill_manual(
    values = rev(viridis::plasma(5)),
    name   = "Cuantiles de p"
  ) +
  labs(title = "Probabilidad predicha de hotspot (quintiles)") +
  theme_minimal(base_size = 11)

# ── 3 · Residuos del modelo --------------------------------------------------
p_mapa_resid <- ggplot(modelo_df_glm) +
  geom_sf(aes(fill = resid_hot_bal), colour = NA) +
  scale_fill_gradientn(
    colours = viridis::viridis(100),
    limits  = c(-0.5, 0.5),
    name    = "Residuo"
  ) +
  labs(title = "Residuales del modelo") +
  theme_minimal(base_size = 11)

# ── Mosaico 3×1 --------------------------------------------------------------
p_mapas_logit <- p_mapa_obs + p_mapa_pred + p_mapa_resid +
                 patchwork::plot_layout(ncol = 3)

ggsave(
  filename = here::here("Figures", "mosaico_mapa_prediccion_logit.png"),
  plot     = p_mapas_logit,
  width    = 12,
  height   = 5.5,
  dpi      = 350
)



## ----analisis_moran_I----------------------------------------------------------------------------------

## --- Hiperparámetros a modificar----------------------------------------------

dist_max_km <- 400     # distancia máxima (km)
step_km     <- 15      # ancho banda (km)  → anillo = (d , d+step]
nsim_mc     <- 200     # permutaciones Monte-Carlo

## --- Centroides en CRS métrico (EPSG 9377) ----------------------------------

centros_9377 <- modelo_df_glm |>          # objeto del logit
  sf::st_point_on_surface() |>            # centroide interno
  sf::st_transform(9377)

xy <- sf::st_coordinates(centros_9377)    # matriz (x , y)

## --- Secuencia de anillos dnearneigh() ---------------------------------------

breaks_m <- seq(0, dist_max_km*1000, by = step_km*1000)     # bordes anillos
mid_km   <- head(breaks_m, -1)/1000 + step_km/2             # centro banda km

## --- Bucle secuencial: calcula Moran I por banda -----------------------------

corr_tab <- lapply(seq_along(mid_km), \(i){

  nb_i <- spdep::dnearneigh(xy, d1 = breaks_m[i], d2 = breaks_m[i+1])

  # Si algún polígono queda sin vecinos → NA
  if (any(spdep::card(nb_i) == 0)) {
    return(tibble(dist_km = mid_km[i], I = NA, p = NA))
  }

  lw_i <- spdep::nb2listw(nb_i, style = "W", zero.policy = TRUE)
  
# --- Monte-Carlo -------------------------------------------------------------
  #   • Mantiene W fija
  #   • Permuta TODA la serie de residuos entre polígonos
  #   • Calcula I para cada permutación -> dist. nula
# -----------------------------------------------------------------------------
  mc <- spdep::moran.mc(modelo_df_glm$resid_hot_bal, lw_i,
                        nsim = nsim_mc, zero.policy = TRUE)

  tibble(dist_km = mid_km[i], I = mc$statistic, p = mc$p.value)
}) |> dplyr::bind_rows()

## --- Preparación para el gráfico --------------------------------------------
corr_tab <- corr_tab |>
  mutate(p_sym = ifelse(p > .5, 1 - p, p),                # bilateral
         sig   = factor(ifelse(is.na(I), "NA",
                          ifelse(p_sym < .05, "Si", "No")),
                        levels = c("Si", "No", "NA")))

colores <- c("Si"="#56B4E9", "No"="#FFD700", "NA"="gray90")

## ---· Correlograma ggplot ---------------------------------------------------

g_moran_corr <- ggplot(corr_tab, aes(dist_km, I)) +
  geom_line(colour = "black") +
  geom_point(aes(colour = sig), size = 3) +
  scale_colour_manual(values = colores, name = "p < 0.05") +
  geom_hline(yintercept = -1/(nrow(modelo_df_glm)-1),
             linetype = "dashed", colour = "grey40") +
  labs(title = "Correlograma de Moran I Monte-Carlo",
       x = "Distancia banda central (km)",
       y = "Moran I de los residuos") +
  theme_minimal(base_size = 12)

# Supongamos que tu ggplot se llama g_correlograma
ggsave(
  filename = here::here("Figures", "correlograma_moran_mc.png"), 
  plot     = g_moran_corr,
  width    = 8,      # pulgadas
  height   = 4,      
  dpi      = 350
)



## ----analisis_LISA-------------------------------------------------------------------------------------

# 1) Encuentra la distancia óptima (km) con mayor I y p < 0.05
r_opt_km <- corr_tab %>%
  filter(!is.na(p), p < 0.05) %>%
  slice_max(I, n = 1) %>%
  pull(dist_km)

r_opt_m <- r_opt_km * 1000  # pasa a metros

# 2) Crea la vecindad dnearneigh y lista de pesos
nb_opt <- dnearneigh(xy, d1 = 0, d2 = r_opt_m)
pesos  <- nb2listw(nb_opt, style = "W", zero.policy = TRUE)

# 3) Calcula Moran local (LISA)
moran_local <- localmoran(
  modelo_df_glm$resid_hot_bal,
  listw       = pesos,
  zero.policy = TRUE
)

modelo_df_glm <- modelo_df_glm %>%
  mutate(
    local_moran_I = moran_local[, "Ii"],    # estadístico local
    p_valor_local = moran_local[, 5],       # 5ª columna → p-valor
    lag_resid     = lag.listw(pesos, resid_hot_bal)
  ) %>%
  mutate(
    cluster_tipo = case_when(
      resid_hot_bal >  0 & lag_resid >  0 ~ "High-High",
      resid_hot_bal <  0 & lag_resid <  0 ~ "Low-Low",
      resid_hot_bal >  0 & lag_resid <  0 ~ "High-Low",
      resid_hot_bal <  0 & lag_resid >  0 ~ "Low-High",
      TRUE                                 ~ "No significativo"
    ),
    cluster_tipo = if_else(p_valor_local > 0.001, "No significativo", cluster_tipo)
  )



## ----visualizacion_LISA--------------------------------------------------------------------------------

# 1. Cargamos la paleta Zissou1 (100 tonos continuos)  
pal_zissou <- paletteer::paletteer_c("grDevices::Zissou 1", 100)

# 2. Asignamos los colores según tus clusters
colores_lisa <- c(
  "High-High"        = pal_zissou[90],  # rojo intenso Zissou
  "Low-Low"          = pal_zissou[10],  # azul Zissou
  "High-Low"         = pal_zissou[70],  # naranja Zissou
  "Low-High"         = pal_zissou[30],  # celeste Zissou
  "No significativo" = "gray90"         # gris neutro
)

# 3. Extraemos el valor máximo significativo de Moran I y su distancia
moran_max <- corr_tab %>%
  filter(!is.na(p), p < 0.05) %>%
  slice_max(I, n = 1)

# 4. Texto a mostrar en el scatterplot
moran_text <- sprintf("Moran I global = %.3f\nDistancia = %d km",
                      moran_max$I, round(moran_max$dist_km))

# 5. Scatterplot Moran local con anotación
p_scatter_lisa <- ggplot(modelo_df_glm, aes(resid_hot_bal, lag_resid, color = cluster_tipo)) +
  geom_point(alpha = .8, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = .5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  scale_color_manual(values = colores_lisa) +
  annotate("text", 
           x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5, 
           label = moran_text, 
           size = 4, fontface = "italic", color = "black") +
  labs(
    title = "Diagrama de dispersión Moran local",
    x     = "Residuo del modelo",
    y     = "Lag espacial",
    color = "Tipo de cluster"
  ) +
  theme_minimal()

# 6. Mapa de clusters LISA
p_moran_lisa <- ggplot(modelo_df_glm) +
  geom_sf(aes(fill = cluster_tipo), color = NA) +
  scale_fill_manual(values = colores_lisa, na.value = "white") +
  labs(
    title = "Mapa: agrupamientos LISA (residuales)",
    fill  = "Tipo de cluster"
  ) +
  theme_minimal()

# 7. Mapa de magnitud Ii
p_ii_mapa <- ggplot(modelo_df_glm) +
  geom_sf(aes(fill = local_moran_I), color = NA) +
  scale_fill_viridis_c(
    name      = "Ii",
    option    = "magma",
    direction = -1,
    na.value  = "white"
  ) +
  labs(title = "Mapa: valor local del estadístico Ii") +
  theme_minimal()

# 8. Composición tripanel
p_lisa_tripanel <- p_scatter_lisa + p_moran_lisa + p_ii_mapa +
  plot_layout(ncol = 3)

# 9. Exportación final
ggsave(
  here::here("Figures", "moran_local_composicion_logit.png"),
  plot   = p_lisa_tripanel,
  width  = 15,
  height = 5,
  dpi    = 350
)

