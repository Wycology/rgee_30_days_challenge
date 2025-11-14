###############################################################
# Script: terrain_suitability_rgee.R
# Purpose: Identify and visualize terrain-suitable areas based on
#          elevation, slope, and aspect thresholds using 
#          Google Earth Engine (GEE) via rgee.
#
# Workflow:
#   1. Draw a region of interest (ROI) interactively
#   2. Convert the drawn ROI to Earth Engine FeatureCollection
#   3. Load SRTM (USGS SRTMGL1_003) elevation data
#   4. Derive slope and aspect layers using ee$Terrain
#   5. Create suitability masks:
#        - Elevation >= 2000 m
#        - Slope <= 30 degrees
#        - Aspect <= 90 degrees
#   6. Combine masks to identify suitable terrain pixels
#   7. Apply mask to elevation raster
#   8. Visualize suitable areas on the interactive map
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology
# (Like, share, subscribe, comment)
# Date: 2025-11-14
###############################################################

# ---------------------------------------------------------------------------
# Load required libraries
# ---------------------------------------------------------------------------
library(rgee)     # Version 1.1.8.900 GEE client for R
library(mapedit)  # Version 0.7.0 Interactive ROI drawing
library(dplyr)    # Version 1.1.4 Data wrangling
library(sf)       # Version 1.0.22 Spatial operations

# Initialize Earth Engine
ee_Initialize()

# ---------------------------------------------------------------------------
# 1. Draw Region of Interest (ROI)
# ---------------------------------------------------------------------------
roi <- drawFeatures() |>          # user draws polygon interactively
  mutate(name = "Nandi") |>       # assign a descriptive name
  select(name) |>                 # keep only 'name' column
  sf_as_ee()                      # convert to Earth Engine object

# ---------------------------------------------------------------------------
# 2. Load Elevation Data (SRTM 30m)
# ---------------------------------------------------------------------------
elev <- ee$Image$Dataset$USGS_SRTMGL1_003$clip(roi)

# Terrain derivatives
slope  <- ee$Terrain$slope(elev)
aspect <- ee$Terrain$aspect(elev)

# ---------------------------------------------------------------------------
# 3. Create suitability masks
# ---------------------------------------------------------------------------
mask_elev   <- elev$gte(2000)   # Elevation >= 2000 m
mask_slope  <- slope$lte(30)    # Slope <= 30°
mask_aspect <- aspect$lte(180)   # Aspect <= 180° (e.g., East-facing)

# ---------------------------------------------------------------------------
# 4. Combine masks to obtain final suitability surface
# ---------------------------------------------------------------------------
suitable_mask <- mask_elev$
  And(mask_slope)$
  And(mask_aspect)

# Apply mask to elevation raster (or alternatively slope/aspect)
suitable_area <- elev$updateMask(suitable_mask)

# ---------------------------------------------------------------------------
# 5. Visualize results in Earth Engine Map
# ---------------------------------------------------------------------------
Map$centerObject(roi, zoom = 12)

Map$addLayer(
  suitable_area,
  list(palette = "green"),
  name = "Suitable terrain (elev>=2000, slope<=30, aspect<=90)"
) +
  Map$addLayer(
    eeObject = roi$style(
      color     = "red",
      width     = 4,
      fillColor = "#ffffff00"    # transparent fill
    )
  )

# This shows topographically suitable areas for particular activity, say tea planting.
# It can be further changed due to other factors

# Cite all data and packages used in your work :)

# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------