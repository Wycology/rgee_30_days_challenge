###############################################################
# Script: 10_lake_area.R
# Purpose: Detect open water using Sentinel-2 (S2_SR_HARMONIZED)
#          with Cloud Score Plus masking, compute NDWI median
#          for a user-drawn region of interest, generate a water mask,
#          and calculate total water area in square kilometers.
#
# Workflow:
#   1. Initialize rgee
#   2. Draw ROI interactively (mapedit)
#   3. Load Sentinel-2 SR Harmonized + Cloud Score Plus
#   4. Filter images by date & roi
#   5. Mask clouds using Cloud Score Plus (CS > 0.6)
#   6. Compute NDWI for each image (B3, B8)
#   7. Create temporal median NDWI image
#   8. Threshold NDWI > 0 to get water mask
#   9. Calculate water area (km²) using pixelArea()
#  10. Visualize results on the map
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# (Like, share, subscribe, comment)
# Date: 2025-11-10
###############################################################

# ---------------------------------------------------------------------------
# Load libraries
# ---------------------------------------------------------------------------
library(rgee)     # Version 1.1.8 Interface to Google Earth Engine
library(mapedit)  # Version 0.7.0 For drawing polygons interactively

# ---------------------------------------------------------------------------
# Initialize Google Earth Engine
# ---------------------------------------------------------------------------
ee_Initialize()

# ---------------------------------------------------------------------------
# 1. Draw ROI interactively
# ---------------------------------------------------------------------------
roi <- drawFeatures() |> sf_as_ee()

# Center map on ROI
Map$centerObject(eeObject = roi, zoom = 6)

# ---------------------------------------------------------------------------
# 2. Load Sentinel-2 SR Harmonized + Cloud Score Plus
# ---------------------------------------------------------------------------
s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
cs_plus <- ee$ImageCollection$Dataset$GOOGLE_CLOUD_SCORE_PLUS_V1_S2_HARMONIZED

# ---------------------------------------------------------------------------
# 3. Define time window for analysis
# ---------------------------------------------------------------------------
start_date <- "2024"
end_date   <- "2025"

# ---------------------------------------------------------------------------
# 4. Filter, cloud-mask, compute NDWI, and take median composite
# ---------------------------------------------------------------------------
s2 <- s2$
  filterBounds(roi)$
  filterDate(start_date, end_date)$
  linkCollection(cs_plus, "cs")$         # Attach Cloud Score Plus to S2 images
  map(ee_utils_pyfunc(function(img) {
    # Cloud mask
    qa <- img$select("cs")
    img <- img$updateMask(qa$gt(0.6))    # Keep pixels with CS > 0.6
    
    # Keep timestamp metadata
    img$copyProperties(img, ee$List(list("system:time_start")))
  }))$
  map(ee_utils_pyfunc(function(img) {
    # Select bands for NDWI
    bands <- img$select(c("B3", "B8"))$multiply(0.0001)
    
    # NDWI = (B3 – B8) / (B3 + B8)
    ndwi <- bands$normalizedDifference(c("B3", "B8"))$rename("ndwi")
    
    # Carry over timestamp
    ndwi$copyProperties(img, ee$List(list("system:time_start")))
  }))$
  median()      # Temporal median NDWI

# Print metadata for inspection
ee_print(s2)

# ---------------------------------------------------------------------------
# 5. Create water mask (NDWI > 0)
# ---------------------------------------------------------------------------
thr <- s2$gt(0)             # Above 0 is water
s2_mask <- thr$selfMask()   # Mask non-water pixels

# ---------------------------------------------------------------------------
# 6. Visualize water mask
# ---------------------------------------------------------------------------
Map$addLayer(
  s2_mask$clip(roi),
  visParams = list(palette = "blue"),
  name = "Water mask (NDWI > 0)"
)

# ---------------------------------------------------------------------------
# 10. Compute water area in km²
# ---------------------------------------------------------------------------
sen_lake_area <- s2_mask$multiply(
  ee$Image$pixelArea()$divide(1e6)       # m² → km²
)

lake_area <- sen_lake_area$reduceRegion(
  reducer  = ee$Reducer$sum(),
  geometry = roi,
  scale    = 1000, # Change depending on how large your area is to enable computation
  bestEffort = TRUE
)$values()$get(0)

# Print area
print(lake_area$getInfo())

# Cite data sources and packages used accordingly, :)
# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------