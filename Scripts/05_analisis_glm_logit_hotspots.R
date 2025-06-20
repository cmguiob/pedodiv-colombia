## ----configuracion------------------------------------------------------------------------------------------------
#Para exportar como .R plano
# knitr::purl('05_analisis_glm_logit_hotspots.qmd')

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(
  here,        # rutas del proyecto
  remotes,     # instalar desde GitHub
  sf,          # vectores espaciales
  dplyr,       # manipulación tabular
  tidyr,
  readr,       # leer CSV rápido
  performance,
  ggdist,
  scales,      #transformaciones y escalas gráficas
  ggplot2,     # gráficos
  sjPlot,      # gráficos de modelos
  paletteer,   #paletas de colores
  googledrive, #importar y exportar a Google Drive
  patchwork,   # unir gráficos
  qs           # guardar objetos rápido
)

# Ajusta tamaño de letra para todo e lscript
theme(base_size = 14)

# === Autenticación Google Drive ===
#googledrive::drive_auth()


## ----carga_pedodiversidad-----------------------------------------------------------------------------------------

# Corre script externo para cargar
source(here::here("Scripts", "00_funcion_carga_ucs_procesadas_qs.R"), encoding = "UTF-8")

# Asignación de id único para cada polígono
ucs_rao_sf <- ucs_rao_sf |> 
  sf::st_make_valid() |> #valida geometrias problemáticas
  dplyr::select(id_creado, UCSuelo, AREA_HA, Q) 

ucs_rao_sf <- ucs_rao_sf[!st_is_empty(ucs_rao_sf), ]


# Transforma a crs 4326 antes de pasarlo a GEE
ucs_sf_4326 <- st_transform(ucs_rao_sf, 4326)



## -----------------------------------------------------------------------------------------------------------------

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

# Ruta al CSV combinado
lst_cv_temp <- read_csv(here::here("Data", "OUT_covars_csv","OUT_LST_cv_temporal_combinado.csv" ))

# Convierte geometría desde .geo (GeoJSON como texto) a objeto sf
lst_cv_temporal_sf <- st_as_sf(
  data.frame(lst_cv_temp, geometry = geojson_sf(lst_cv_temp$.geo)), #convierte a sf
  crs = 4326) |>
  select(-.geo) |> #elimina columna de geometria obsoleta
  mutate(
    log_lst_mean_temporal_cv = log(mean + 1),
    lst_mean_temporal_cv_dens = mean/AREA_HA, #densidad de media de CV 
    log_lst_mean_temporal_cv_dens = log(lst_mean_temporal_cv_dens + 1)
    )|>
rename(lst_mean_temporal_cv = mean, lst_stdDev_temporal_cv = stdDev)



## ----join_covars--------------------------------------------------------------------------------------------------

# Extraer solo columnas útiles de dem y slope
dem_df <- dem_cv_sf |> 
  st_drop_geometry() |>
  select(-system.index)

slope_df <- slope_cv_sf |> 
  st_drop_geometry() |>
  select(-system.index)

lst_temporal_df <- lst_cv_temporal_sf |> 
  st_drop_geometry() |>
  select(-system.index)

# Unir a la tabla base
modelo_df <- ucs_rao_sf |>
  mutate(
    Q = if_else(Q == 0, 1e-3, Q),               # evitar log(0)
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
  ) |>
  left_join(
    lst_temporal_df,
    by = c("id_creado", "AREA_HA", "UCSuelo")
  )



## -----------------------------------------------------------------------------------------------------------------

# Subconjunto sin NA: usado para modelado
modelo_df_completo <- modelo_df |> 
drop_na(log_Qdens, dem_mean, slope_mean, lst_mean_temporal_cv, log_dem_cv_dens, log_slope_cv_dens, log_lst_mean_temporal_cv_dens )

# Subconjunto con al menos un NA
modelo_df_NA <- modelo_df |> 
  filter(if_any(c(log_Qdens, dem_cv, slope_cv, lst_mean_temporal_cv, log_dem_cv_dens, log_slope_cv_dens, log_lst_mean_temporal_cv_dens), is.na))

#Verifico set de datos
  ggplot(modelo_df_NA) +
  geom_sf(aes(), fill = "grey90", color = NA) +
  labs(title = "Mapa de NAs") +
  theme_minimal()


## -----------------------------------------------------------------------------------------------------------------

umbral95 <- quantile(modelo_df_completo$log_Qdens, 0.95, na.rm = TRUE)


modelo_df_completo <- modelo_df_completo |>
  # Agregar variable binaria a modelo_df_completo
  mutate(log_Qdens_hot95 = as.integer(log_Qdens >= umbral95)) |>
  mutate(across(
    c(dem_mean, slope_mean, lst_mean_temporal_cv,
      log_dem_cv_dens, log_slope_cv_dens, log_lst_mean_temporal_cv_dens),
    ~ scale(.)[,1],
    .names =  "{.col}_z" 
  ))



## -----------------------------------------------------------------------------------------------------------------

# Ajusta modelo directamente sobre modelo_df_completo
glm_hot <- glm(
    log_Qdens_hot95 ~ dem_mean_z + log_slope_cv_dens_z + log_lst_mean_temporal_cv_dens_z, 
    data = modelo_df_completo,
    family = binomial())

# Calcula predicciones y residuos en la misma tabla
modelo_df_completo <- modelo_df_completo |>
  mutate(
    prob_hot95 = predict(glm_hot, newdata = modelo_df_completo, type = "response"),
    resid_hot95 = residuals(glm_hot, type = "response")
  )

#Resumen estadístico
summary(glm_hot)
performance::r2_tjur(glm_hot)




## -----------------------------------------------------------------------------------------------------------------

p_margin_dem_mean_z <- plot_model(glm_hot, type = "pred", terms = "dem_mean_z [all]", title = NULL) + theme_minimal()

p_margin_log_lst_temporal_cv_dens_z <- plot_model(glm_hot, type = "pred", terms = "log_lst_mean_temporal_cv_dens_z [all]", title = NULL) + theme_minimal()

p_margin_log_slope_cv_dens_z <- plot_model(glm_hot, type = "pred", terms = "log_slope_cv_dens_z [all]", title = NULL) + theme_minimal()

p_curvas_marginales_logit <- (p_margin_dem_mean_z + p_margin_log_slope_cv_dens_z + p_margin_log_lst_temporal_cv_dens_z)

p_curvas_marginales_logit

ggsave(here("Figures", "mosaico_curvas_marginales_logit.png"),
       plot = p_curvas_marginales_logit,
       width = 9,
       height = 6,
       dpi = 350)



## -----------------------------------------------------------------------------------------------------------------

plot_model(glm_hot, vline.color = "red", show.values = TRUE, value.offset = .3)


## -----------------------------------------------------------------------------------------------------------------

p_resid_dem_mean <- ggplot(modelo_df_completo, aes(x = dem_mean_z, y = resid_hot95)) +
  geom_point(alpha = 0.3, color = "#56B4E9") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Residuos", x = "Elevación media") +
  theme_minimal()

p_resid_log_slope_cv_dens_logit <- ggplot(modelo_df_completo, aes(x = log_slope_cv_dens_z, y = resid_hot95)) +
  geom_point(alpha = 0.3, color = "#56B4E9") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Residuos", x = "log(slope CV densificado)") +
  theme_minimal()

p_resid_log_lst_temp_cv_dens <- ggplot(modelo_df_completo, aes(x = log_lst_mean_temporal_cv_dens_z, y = resid_hot95)) +
  geom_point(alpha = 0.3, color = "#56B4E9") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Residuos", x = "log(LST CV medio densificado)") +
  theme_minimal()


p_resid_dem_mean + p_resid_log_slope_cv_dens_logit + p_resid_log_lst_temp_cv_dens

ggsave(here("Figures", "mosaico_residuales_logit.png"),
       plot = p_curvas_marginales_logit,
       width = 9,
       height = 6,
       dpi = 350)


## -----------------------------------------------------------------------------------------------------------------

p1 <- ggplot(modelo_df_completo) +
  geom_sf(aes(fill = prob_hot95), color = NA) +
  scale_fill_viridis_c(option = "viridis", na.value = "white") +
  labs(title = "Probabilidad predicha de hotspot", fill = "Prob") +
  theme_minimal()

p2 <- ggplot(modelo_df_completo) +
  geom_sf(aes(fill = resid_hot95), color = NA) +
  scale_fill_viridis_c(option = "viridis", na.value = "white") +
  labs(title = "Residuo del modelo binomial", fill = "Residuo") +
  theme_minimal()

p1 + p2  # con patchwork


#guarda el último gráfico generado
ggsave(here("Figures", "mosaico_mapa_prediccion_logit.png"),
       plot = p_map_predi_logit,
       width = 10,
       height = 6,
       dpi = 350)

