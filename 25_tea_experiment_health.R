###############################################################
# Script: 25_tea_field_health.R
# Purpose: Compute and rank the health of multiple tea fields 
#          using the latest cloud-free Sentinel-2 NDVI.
#
# Workflow:
#   1. Draw multiple tea fields interactively and assign IDs
#   2. Load Sentinel-2 SR Harmonized + Cloud Score Plus
#   3. Filter images by ROI & date, mask clouds, compute NDVI
#   4. Aggregate to median NDVI for the latest period
#   5. Reduce NDVI by tea field to get mean NDVI per field
#   6. Rank fields based on NDVI
#   7. Visualize NDVI with color coding and field boundaries
#   8. Retrieve NDVI rankings to R
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# Date: 2025-11-25
###############################################################

# ---------------------------------------------------------------------------
# Load libraries
# ---------------------------------------------------------------------------
library(rgee)     # Version 1.1.8.9000 Interface to Google Earth Engine
library(mapedit)  # Version 0.7.0 For interactive polygon drawing
library(dplyr)    # Version 1.1.4 Data Wrangling
library(ggplot2)  # Version 4.0.1 Visulaization

# ---------------------------------------------------------------------------
# Initialize Google Earth Engine
# ---------------------------------------------------------------------------
ee_Initialize()

# ---------------------------------------------------------------------------
# 1. Draw multiple tea fields and assign IDs
# ---------------------------------------------------------------------------
tea_fields <- drawFeatures() |> 
  mutate(field_id = c("Field_1", "Field_2", "Field_3", "Field_4")) |>
  select(field_id) |> 
  sf_as_ee()

# ---------------------------------------------------------------------------
# 2. Load Sentinel-2 and Cloud Score Plus collection
# ---------------------------------------------------------------------------
s2 <- ee$ImageCollection$Dataset$COPERNICUS_S2_SR_HARMONIZED
cs_plus <- ee$ImageCollection$Dataset$GOOGLE_CLOUD_SCORE_PLUS_V1_S2_HARMONIZED

# ---------------------------------------------------------------------------
# 3. Filter Sentinel-2 images, mask clouds, compute NDVI
# ---------------------------------------------------------------------------
oct_ndvi <- s2$
  filterBounds(tea_fields)$
  filterDate("2024-10-01", "2024-11-01")$       # Latest period
  linkCollection(cs_plus, "cs")$               # Attach cloud score
  map(function(img) {
    qa <- img$select('cs')
    img <- img$updateMask(qa$gt(0.6))         # Keep pixels with CS > 0.6
    img$normalizedDifference(c("B8", "B4"))$rename("ndvi")  # NDVI = (NIR - Red)/(NIR + Red)
  })$
  median()                                     # Aggregate to median NDVI

# ---------------------------------------------------------------------------
# 4. Visualize NDVI and tea field boundaries
# ---------------------------------------------------------------------------
Map$centerObject(tea_fields, zoom = 14)
Map$addLayer(
  oct_ndvi$clip(tea_fields), 
  list(min = 0, max = 1, palette = c("red","yellow","green")), 
  "Tea Health"
) +
  Map$addLayer(
    tea_fields$style(
      color = "magenta", 
      fillColor = "#ffffff00", 
      width = 2
    )
  )

# ---------------------------------------------------------------------------
# 5. Compute mean NDVI per tea field
# ---------------------------------------------------------------------------
field_health <- oct_ndvi$reduceRegions(
  collection = tea_fields,
  reducer    = ee$Reducer$mean(),  # Mean NDVI per field
  scale      = 10
)

# ---------------------------------------------------------------------------
# 6. Retrieve NDVI rankings to R
# ---------------------------------------------------------------------------
tea_field_rankings <- ee_as_sf(field_health) |> 
  sf::st_drop_geometry() |> 
  select(field_id, ndvi = mean) |> 
  arrange(desc(ndvi))
  
print(tea_field_rankings)

# Cite all data sources and packages accordingly :)

# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------