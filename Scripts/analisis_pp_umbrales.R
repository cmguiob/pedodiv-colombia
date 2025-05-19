Approach 1: Thresholded Point Pattern of “Unusual” Polygons
This strategy involves filtering your polygons to retain only those that are outliers in terms of diversity, then using point pattern analysis on their centroids.

Step-by-step Explanation

Step 1: Compute Residuals from Diversity ~ Area
Fit a model like:
  
  r
Copiar
Editar
model <- lm(rao_q ~ log(area), data = df)
df$residuals <- resid(model)
Step 2: Flag Outliers
You define "unusual" polygons as those with residuals in the top or bottom quantiles:
  
  r
Copiar
Editar
q95 <- quantile(df$residuals, 0.95)
q05 <- quantile(df$residuals, 0.05)
df$group <- dplyr::case_when(
  df$residuals > q95 ~ "HighDiversity",
  df$residuals < q05 ~ "LowDiversity",
  TRUE ~ NA_character_
)
Step 3: Extract Centroids of Only Those Polygons
r
Copiar
Editar
library(sf)
centroids_sf <- st_centroid(polygons_sf)
marked_points <- centroids_sf[!is.na(df$group), ]
Step 4: Convert to ppp Object for Analysis
r
Copiar
Editar
library(spatstat.geom)
ppp_obj <- as.ppp(st_coordinates(marked_points), W = your_study_window)
marks(ppp_obj) <- df$group[!is.na(df$group)]
Step 5: Analyze Spatial Pattern of “Unusual” Points
Example: Are unusually diverse polygons clustered?
  
  r
Copiar
Editar
library(spatstat.core)
Kmarked(ppp_obj, i = "HighDiversity", j = "HighDiversity")

What You Gain
Removes the area-based spatial autocorrelation problem

Focuses on interpretable, meaningful subsets

You can separately test:
  
  Whether unusually diverse MUs cluster

Whether unusually uniform MUs form a spatial pattern