####################################################################
# Script: 21_export_feature_collection.R
#
# Purpose:
#   Demonstrate how to load national boundaries from the
#   U.S. Department of State (LSIB 2017) dataset in Google Earth Engine
#   via the rgee, visualize the boundary of Kenya,
#   and export the boundary shapefile to Google Drive.
#
#   Demonstrated steps:
#     - Initialize Google Earth Engine in R (rgee)
#     - Load international simplified boundaries (LSIB 2017)
#     - Filter the dataset to Kenya only
#     - Visualize the country boundary interactively
#     - Export the FeatureCollection to Google Drive as a shapefile
#
# Workflow:
#   1. Initialize rgee and authenticate GEE
#   2. Load LSIB global boundary dataset (2017)
#   3. Filter boundaries to Kenya
#   4. Visualize boundary on the GEE Map viewer
#   5. Export shapefile to Google Drive
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology
# Date: 2025-11-21
####################################################################

# ------------------------------------------------------------------
# Load required libraries and initialize Earth Engine
# ------------------------------------------------------------------
library(rgee)       # Version 1.1.8.9000 R interface for Google Earth Engine
ee_Initialize()     # Authenticate and start an Earth Engine session

# ------------------------------------------------------------------
# 1. Load and Filter Boundary Dataset (LSIB Simple 2017)
# ------------------------------------------------------------------
# Load U.S. Department of State – International Boundaries (simplified)
fc <- ee$FeatureCollection("USDOS/LSIB_SIMPLE/2017") %>%
  ee$FeatureCollection$filter(ee$Filter$eq("country_na", "Kenya")) # Keep Kenya only

# Convert FeatureCollection to sf for local inspection (optional)
ken_sf <- ee_as_sf(fc) # This ken_sf you can write directly to directory
plot(sf::st_geometry(ken_sf))
# ------------------------------------------------------------------
# 2. Visualize Kenya Boundary
# ------------------------------------------------------------------
Map$centerObject(eeObject = fc$geometry(), zoom = 6)
Map$addLayer(
  eeObject = fc$style(color = "magenta", fillColor = "#ffffff00", width = 4),
  name     = "Kenya Boundary"
)

# ------------------------------------------------------------------
# 3. Export Kenya Boundary to Google Drive
# ------------------------------------------------------------------
task_drive <- ee_table_to_drive(
  collection   = fc,
  description  = "kenya",
  folder       = "GEE_Exports",   # Optional Drive destination folder
  fileFormat   = "SHP"            # Supported: SHP, CSV, KML, GEOJSON
)

task_drive$start()       # Begin export task
ee_monitoring(task_drive) # Track export progress

# ------------------------------------------------------------------
# Citation:
# Please cite data sources and tools appropriately in your project.
# ------------------------------------------------------------------

# End of script #####################################################
