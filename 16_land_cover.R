###############################################################
# Script: 16_land_cover.R
#
# Purpose:
#   Visualize ESA WorldCover (v100) land-cover classification 
#   for a region of interest using Google Earth Engine (GEE) via rgee.
#
#   Demonstrated steps:
#     - Initialize Earth Engine session
#     - Load GAUL level-2 administrative boundaries
#     - Filter to the region of interest administrative unit
#     - Load and clip ESA WorldCover 2020 data to region of interest
#     - Extract class palettes and metadata from image properties
#     - Visualize WorldCover classification with correct colors
#
# Workflow:
#   1. Initialize rgee and authenticate GEE
#   2. Load FAO/GAUL admin boundaries
#   3. Filter boundary to "Kericho" (ADM2 level)
#   4. Load ESA WorldCover 2020 dataset
#   5. Extract class values and palettes from metadata
#   6. Visualize results in interactive Earth Engine map
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology
# Date: 2025-11-16
###############################################################

# ---------------------------------------------------------------------------
# Load required libraries
# ---------------------------------------------------------------------------
library(rgee)   # Version 1.1.8.9000 Earth Engine client for R (rgee)
ee_Initialize() # Version 0.7.0 Authenticate and start EE session

# ---------------------------------------------------------------------------
# 1. Load Administrative Boundary (GAUL 2015)
# ---------------------------------------------------------------------------
world <- ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level2")

# Filter to Kericho District (ADM2 level)
geometry <- world$filter(ee$Filter$eq("ADM2_NAME", "Kericho"))

# ---------------------------------------------------------------------------
# 2. Load ESA WorldCover (2020) and Clip to Kericho
# ---------------------------------------------------------------------------
dataset <- ee$ImageCollection("ESA/WorldCover/v100")$first()

# Select the classification band ("Map") and clip to Kericho
classification <- dataset$select("Map")$clip(geometry)

# ---------------------------------------------------------------------------
# 3. Extract Class Metadata (Values + Palette) from Image Properties
# ---------------------------------------------------------------------------
props <- classification$propertyNames()
info  <- classification$getInfo()

# Visualization parameters obtained from the dataset metadata
vis <- list(
  min     = info$properties$`Map_class_values`[[1]],   # smallest class value
  max     = info$properties$`Map_class_values`[[11]],  # largest class value
  palette = info$properties$`Map_class_palette`        # corresponding colors
)

# ---------------------------------------------------------------------------
# 4. Visualize WorldCover Classification
# ---------------------------------------------------------------------------
Map$setCenter(lon = 35, lat = -0.2, zoom = 9)

# Add WorldCover layer
Map$addLayer(
  eeObject  = classification,
  visParams = vis,
  name      = "WorldCover 2020"
) +
  
  # Add boundary outline
  Map$addLayer(
    eeObject = geometry$style(
      color     = "magenta",
      fillColor = "#ffffff00",
      width     = 4
    ),
    name = "Kericho Boundary"
  )

# Cite data sources and packages you use for your project accordingly :)

# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------