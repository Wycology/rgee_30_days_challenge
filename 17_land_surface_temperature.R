###############################################################
# Script: 17_land_surface_temperature.R
#
# Purpose:
#   Demonstrate interactive extraction and visualization of 
#   Land Surface Temperature (LST) from Landsat 8 (Collection 2)
#   using Google Earth Engine (GEE) via rgee, including:
#     - Interactive drawing of a city boundary/ROI
#     - Loading Landsat 8 Level-2 SR thermal bands
#     - Converting thermal radiance to surface temperature (°C)
#     - Creating a temperature heat map
#     - Extracting minimum and maximum LST statistics
#
# Workflow:
#   1. Draw a city boundary using mapedit
#   2. Convert drawn polygon into an Earth Engine object
#   3. Load Landsat 8 Tier 1 Level-2 image collection
#   4. Filter by date and spatial extent
#   5. Convert ST_B10 thermal band to Celsius
#   6. Visualize LST on the map
#   7. Compute basic LST statistics using reduceRegion()
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# (Like, share, subscribe, comment)
# Date: 2025-11-17
###############################################################

# ---------------------------------------------------------------------------
# Load required libraries
# ---------------------------------------------------------------------------
library(rgee)       # Version 1.1.8.9000 Earth Engine API for R
library(mapedit)    # Version 0.7.0 Interactive drawing 

# Initialize Earth Engine 
ee_Authenticate()
ee_Initialize(drive = TRUE)

# ---------------------------------------------------------------------------
# 1. Draw City Boundary / Region of Interest (ROI)
# ---------------------------------------------------------------------------
city <- drawFeatures() |>
  sf_as_ee()         # Convert drawn feature to Earth Engine FeatureCollection

# Center the interactive map on ROI
Map$centerObject(city, zoom = 10)

# ---------------------------------------------------------------------------
# 2. Load and Filter Landsat 8 L2 Imagery
# ---------------------------------------------------------------------------
landsat <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
  filterBounds(city)$                          # spatial filter
  filterDate("2024-01-01", "2024-12-31")$      # year 2024 (example)
  median()                                      # take median composite

# ---------------------------------------------------------------------------
# 3. Convert Thermal Band (ST_B10) from Kelvin to Celsius
# ---------------------------------------------------------------------------
# Landsat Collection 2 scale factor: 0.00341802
# Add 149.0 offset (radiometric calibration)
# Convert Kelvin → Celsius by subtracting 273.15

lst_celsius <- landsat$select("ST_B10")$
  multiply(0.00341802)$
  add(149)$
  subtract(273.15)$
  rename("LST")$
  clip(city)            # limit output to city boundary

# ---------------------------------------------------------------------------
# 4. Visualize Surface Temperature Map
# ---------------------------------------------------------------------------
Map$addLayer(
  lst_celsius,
  list(
    min     = 18,                       # lower bound for visualization
    max     = 36,                       # upper bound for visualization
    palette = c("blue", "green", "yellow", "red")  # cool → hot
  ),
  "Surface Temperature (°C)"
)

# ---------------------------------------------------------------------------
# 5. Compute Basic Temperature Statistics
# ---------------------------------------------------------------------------
max_temp <- lst_celsius$reduceRegion(
  reducer  = ee$Reducer$max(),
  geometry = city,
  scale    = 30
)$get("LST")

min_temp <- lst_celsius$reduceRegion(
  reducer  = ee$Reducer$min(),
  geometry = city,
  scale    = 30
)$get("LST")

# Print results
print(paste("Maximum LST (°C):", max_temp$getInfo()))
print(paste("Minimum LST (°C):", min_temp$getInfo()))

# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------
