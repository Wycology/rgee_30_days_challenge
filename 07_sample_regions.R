###############################################################
# Script: sampling_regions_rgee.R
# Purpose: Interactively draw points, extract cloud-filtered 
#          Sentinel-2 surface reflectance (harmonized) using 
#          Cloud Score+ (GOOGLE_CLOUD_SCORE_PLUS), compute a 
#          median composite, visualize RGB, and extract band 
#          values at the drawn points.
#
# Workflow:
#   1. Initialize rgee
#   2. Draw points interactively (mapedit)
#   3. Convert points → Earth Engine FeatureCollection
#   4. Load Sentinel-2 SR Harmonized
#   5. Load Cloud Score+ for S2
#   6. Filter by region bounds and date
#   7. Mask using Cloud Score+ (retain high-quality pixels)
#   8. Compute median composite
#   9. Visualize RGB composite + points
#  10. Extract pixel values at drawn points
#  11. Convert to sf → dataframe (R)
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# (Like, Share, Subscribe, Comment)
# Date: 2025-11-07
###############################################################

# ---------------------------------------------------------------------------
# Load libraries
# ---------------------------------------------------------------------------
library(rgee)       # Version 1.1.8:  Google Earth Engine for R
library(mapedit)    # Version 0.7.0:  Interactive drawing tools
library(dplyr)      # Version 1.1.4:  Data manipulation
library(sf)         # Version 1.0.21: Spatial vector handling

# ---------------------------------------------------------------------------
# 1. Initialize Google Earth Engine
# ---------------------------------------------------------------------------
ee_Initialize()

# ---------------------------------------------------------------------------
# 2. Draw points interactively and assign IDs
# ---------------------------------------------------------------------------
# User draws points on a popup map
points <- drawFeatures()

# Assign unique IDs (letters)
points <- points |>
  mutate(id = letters) |> 
  select(id)

# Convert to Earth Engine FeatureCollection
points <- sf_as_ee(points)

# Add points to map
Map$addLayer(points)

# ---------------------------------------------------------------------------
# 3. Load Sentinel-2 collections (SR + CloudScore+)
# ---------------------------------------------------------------------------
s2 <- ee$ImageCollection$Dataset$COPERNICUS_S2_SR_HARMONIZED

# Sentinel-2 spectral bands of interest
bands <- c(
  "B2","B3","B4","B5","B6","B7",
  "B8","B8A","B9","B11","B12"
)

# CloudScore+ for Sentinel-2 cloud masking
cs_plus <- ee$ImageCollection$Dataset$GOOGLE_CLOUD_SCORE_PLUS_V1_S2_HARMONIZED

# ---------------------------------------------------------------------------
# 4. Date range for filtering
# ---------------------------------------------------------------------------
start_date <- "2024-01-01"
end_date   <- "2025-01-01"

# ---------------------------------------------------------------------------
# 5. Build S2 median composite with CloudScore+ masking
# ---------------------------------------------------------------------------
s2_image <- s2$
  filterBounds(points)$
  filterDate(start_date, end_date)$
  linkCollection(cs_plus, "cs")$
  map(function(img) {
    qa <- img$select("cs")
    img$updateMask(qa$gt(0.6))
  })$
  median()$
  clip(points$bounds()$buffer(2000))$
  select(bands)

# ---------------------------------------------------------------------------
# 6. Visualize RGB composite + points
# ---------------------------------------------------------------------------
Map$addLayer(
  eeObject = s2_image,
  visParams = list(
    bands = c("B4", "B3", "B2"),  # True color
    min = 100,
    max = 1000
  )
) +
  Map$addLayer(
    eeObject = points,
    visParams = list(
      color = "blue",
      width = 12
    )
  )

# ---------------------------------------------------------------------------
# 7. Extract pixel values at point locations
# ---------------------------------------------------------------------------
stats <- s2_image$sampleRegions(
  collection = points,
  scale = 10   # Sentinel-2 native resolution
)

stats_df <- ee_as_sf(stats) |> 
  st_drop_geometry()

stats_df

# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------
