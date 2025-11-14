###############################################################
# Script: 15_reducer_scale.R
#
# Purpose: 
#   Demonstrate NDVI extraction from Sentinel-2 imagery using 
#   Google Earth Engine (GEE) via rgee, including:
#     - Interactive ROI selection
#     - Cloud Score+ (CSP) cloud filtering
#     - NDVI computation and visualization
#     - Effect of spatial scale on NDVI using reduceRegion()
#     - Reprojection of NDVI from native 10 m to 200 m
#
# Workflow:
#   1. Draw region of interest (ROI) interactively
#   2. Convert drawn ROI to Earth Engine FeatureCollection
#   3. Load Sentinel-2 SR Harmonized collection
#   4. Attach Cloud Score+ (CS+) and mask cloudy pixels
#   5. Compute NDVI and take monthly median
#   6. Visualize NDVI results on the map
#   7. Extract mean NDVI at multiple spatial scales (10–200 m)
#   8. Plot NDVI sensitivity to scale using ggplot2
#   9. Reproject NDVI to 200 m and compare with native 10 m
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology
# (Like, share, subscribe, comment)
# Date: 2025-11-15
###############################################################

# ---------------------------------------------------------------------------
# Load required libraries
# ---------------------------------------------------------------------------
library(rgee)       # Version 1.1.8.9000 Earth Engine client for R
library(mapedit)    # Version 0.7.0 Interactive ROI drawing
library(ggplot2)    # Version 4.0.0 Plotting NDVI scale effects

# Initialize Earth Engine 
ee_Initialize()

# ---------------------------------------------------------------------------
# 1. Draw Region of Interest (ROI)
# ---------------------------------------------------------------------------
roi <- drawFeatures() |> 
  sf_as_ee()        # convert drawn polygon to Earth Engine FeatureCollection

# ---------------------------------------------------------------------------
# 2. Load Sentinel-2 and Cloud Score+ Collections
# ---------------------------------------------------------------------------
cs_plus <- ee$ImageCollection$
  Dataset$
  GOOGLE_CLOUD_SCORE_PLUS_V1_S2_HARMONIZED

# Filter Sentinel-2 imagery by ROI and date
s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterBounds(roi)$
  filterDate("2025-01-01", "2025-01-31")$
  
  # Attach Cloud Score+ quality band
  linkCollection(cs_plus, "cs")$
  
  # Cloud masking + NDVI computation
  map(function(img) {
    qa  <- img$select("cs")
    img <- img$updateMask(qa$gt(0.6))           # keep only high-quality pixels
    ndvi <- img$normalizedDifference(c("B8", "B4"))$rename("ndvi")
    img$addBands(ndvi)
  })$
  
  # Monthly median NDVI
  median()$
  clip(roi)$
  select("ndvi")

# ---------------------------------------------------------------------------
# 3. Visualize NDVI in Earth Engine Map
# ---------------------------------------------------------------------------
Map$centerObject(eeObject = roi, zoom = 15)

Map$addLayer(
  eeObject  = s2,
  visParams = list(min = 0, max = 1, palette = c("red", "yellow", "green")),
  name      = "NDVI (January 2025, cloud-masked)"
)

# ---------------------------------------------------------------------------
# 4. NDVI Sensitivity to Spatial Scale
# ---------------------------------------------------------------------------
scales <- seq(10, 200, 10)  # scales in meters
ndvi_values <- numeric(length(scales))

# Extract mean NDVI at each scale using reduceRegion()
for (i in seq_along(scales)) {
  result <- s2$reduceRegion(
    reducer  = ee$Reducer$mean(),
    geometry = roi,
    scale    = scales[i]
  )$getInfo()
  
  ndvi_values[i] <- result$ndvi
}

# Put scales and the ndvi values into one dataframe
df <- data.frame(scale = scales, 
                 ndvi = ndvi_values)

# Plot scale–NDVI relationship
ggplot(df, aes(x = scale, y = ndvi)) +
  geom_line() +
  geom_point() +
  labs(
    x     = "Scale (m)",
    y     = "Mean NDVI",
    title = "Effect of pixel scale on NDVI extraction"
  ) +
  theme_minimal(base_size = 14)

# ---------------------------------------------------------------------------
# 5. Native 10 m NDVI vs Reprojected 200 m NDVI
# ---------------------------------------------------------------------------
proj_10m   <- s2$projection()   # native Sentinel-2 projection
ndvi_10m   <- s2$clip(roi)      # NDVI at native resolution

# Reproject NDVI to 200 m resolution
ndvi_200m <- ndvi_10m$reproject(
  crs   = proj_10m,
  scale = 200
)

# Compare visually
Map$addLayer(
  ndvi_10m,
  list(min = 0, max = 1, palette = c("red", "yellow", "green")),
  "NDVI 10 m"
) |
  Map$addLayer(
    ndvi_200m,
    list(min = 0, max = 1, palette = c("red", "yellow", "green")),
    "NDVI 200 m"
  )

# Cite all data sources and packages you use accordingly please :)

# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------
