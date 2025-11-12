###############################################################
# Script: 12_neighborhood_reducer.R
# Purpose: Extract and visualize elevation and terrain texture 
#          (standard deviation of elevation) for Kericho County, 
#          Kenya, using the SRTM Digital Elevation Model 
#          (USGS_SRTMGL1_003) via Google Earth Engine (rgee).
#
# Workflow:
#   1. Initialize rgee and load global admin boundaries
#   2. Filter FAO GAUL data set to extract Kericho District polygon
#   3. Load SRTM 30m elevation data set
#   4. Clip DEM to Kericho boundary
#   5. Visualize elevation map
#   6. Compute local elevation variability (texture) using 
#      a neighborhood standard deviation reducer
#   7. Overlay both elevation and texture layers on interactive map
#
# Author: Wyclife Agumba Oluoch
# GitHub: https://github.com/Wycology/rgee_30_days_challenge
# YouTube: https://www.youtube.com/@wycology
# Date: 2025-11-12
###############################################################

# ---------------------------------------------------------------------------
# 1. Load required packages and initialize Google Earth Engine
# ---------------------------------------------------------------------------
library(rgee)     # Version 1.1.8.9000
library(mapedit)  # Version 0.7.0

# Initialize Earth Engine
ee_Initialize()

# ---------------------------------------------------------------------------
# 2. Load FAO GAUL 2015 administrative boundaries and filter Kericho District
# ---------------------------------------------------------------------------
world <- ee$FeatureCollection$Dataset$FAO_GAUL_2015_level2

# Filter for Kericho District (Kenya)
kericho <- world$filter(ee$Filter$eq("ADM2_NAME", "Kericho"))

# ---------------------------------------------------------------------------
# 3. Load SRTM 30 m elevation data set
# ---------------------------------------------------------------------------
elev <- ee$Image$Dataset$USGS_SRTMGL1_003

# ---------------------------------------------------------------------------
# 4. Clip the DEM to Kericho boundary
# ---------------------------------------------------------------------------
kericho_elev <- elev$clip(kericho)

# ---------------------------------------------------------------------------
# 5. Visualize elevation (m) over Kericho
# ---------------------------------------------------------------------------
Map$centerObject(eeObject = kericho, zoom = 9)

Map$addLayer(
  eeObject = kericho_elev,
  visParams = list(
    min = 1200,
    max = 2800,
    palette = c("blue", "green", "yellow", "orange", "red")
  ),
  name = "Kericho Elevation (m)"
)

# ---------------------------------------------------------------------------
# 6. Compute terrain texture (local elevation variability)
#     using neighborhood standard deviation reducer
# ---------------------------------------------------------------------------
texture <- kericho_elev$reduceNeighborhood(
  reducer = ee$Reducer$stdDev(),
  kernel = ee$Kernel$circle(7)  # 7-pixel radius (~210 m neighborhood)
)

# ---------------------------------------------------------------------------
# 7. Display elevation and texture layers on the interactive map
# ---------------------------------------------------------------------------
Map$addLayer(
  eeObject = kericho_elev,
  visParams = list(
    min = 1200,
    max = 2800,
    palette = c("blue", "darkgreen", "orange", "brown", "red")
  ),
  name = "Elevation (m)"
) |
  Map$addLayer(
    eeObject = texture,
    visParams = list(
      min = 1.6,
      max = 120,
      palette = c("white", "green", "yellow", "brown")
    ),
    name = "Terrain Texture (SD of Elevation)"
  )
# Cite the data sources and the packages you use accordingly, :)
# ---------------------------------------------------------------------------
# End of Script
# ---------------------------------------------------------------------------