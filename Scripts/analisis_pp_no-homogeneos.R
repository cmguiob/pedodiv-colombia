Approach 2: Inhomogeneous Point Process Model (IPP) or Area-Weighted PPA
This approach assumes you don’t want to discard any polygons, but you adjust your analysis to account for the fact that point locations and marks are influenced by area.

Concept
Instead of assuming a uniform background process, model the point pattern as intensity varying in space (e.g., based on polygon size, topography, etc.).

Step-by-Step for IPP
Step 1: Compute intensity offset (e.g., area)
You can treat polygon area as an offset or covariate

Build a spatial raster or function from it:
  
  r
Copiar
Editar
library(spatstat.geom)
area_values <- df$area_km2
locations <- st_coordinates(st_centroid(polygons_sf))
area_im <- im(area_values, xcol = unique(locations[,1]), yrow = unique(locations[,2]))
Step 2: Create inhomogeneous point pattern model
r
Copiar
Editar
library(spatstat.model)
ppm_model <- ppm(ppp_obj, ~ area, covariates = list(area = area_im))
summary(ppm_model)
Step 3: Test interaction or mark dependence
Use tools like Kinhom() or Kcross.inhom() with area as a spatial intensity adjuster

Or model marks (diversity) as a function of spatial covariates using ppm() with marks

Alternative: Area-Weighted Mark Correlation
If you're interested in mark relationships (e.g., spatial clustering of high Rao Q values), weight the analysis:

e.g., in marked K-function, normalize expected intensity using area weights.

r
Copiar
Editar
Kinhom(ppp_obj, lambda = area_weights)
