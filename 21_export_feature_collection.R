####################################################################
# Script: 21_export_feature_collection.R
#
# Purpose:
#   Demonstrate how to load national boundaries from the
#   GAUL dataset in Google Earth Engine
#   via the rgee, visualize the boundary of your roi,
#   and export the boundary shapefile to Google Drive.
#
#   Demonstrated steps:
#     - Initialize Google Earth Engine in R (rgee)
#     - Load GAUL dataset
#     - Filter the dataset to roi only
#     - Visualize the roi boundary interactively
#     - Export the FeatureCollection to Google Drive as a GEOJSON
#
# Workflow:
#   1. Initialize rgee 
#   2. Load GAUL dataset
#   3. Filter boundaries to roi
#   4. Visualize boundary on the GEE Map viewer
#   5. Export GEOJSON to Google Drive
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology
# Date: 2025-11-21
####################################################################

# ------------------------------------------------------------------
# Load required libraries and initialize Earth Engine
# ------------------------------------------------------------------
library(rgee)       # Version 1.1.8.9000 R interface for Google Earth Engine
ee_Initialize()     # Initialize an Earth Engine session

# ------------------------------------------------------------------
# 1. Load and Filter Boundary Dataset (GAUL)
# ------------------------------------------------------------------
world <- ee$FeatureCollection$Dataset$FAO_GAUL_2015_level2
roi <- world$filter(ee$Filter$eq("ADM2_NAME", "Kericho"))

# Convert FeatureCollection to sf for direct use in R
roi_sf <- ee_as_sf(roi)
plot(sf::st_geometry(roi_sf))
# ------------------------------------------------------------------
# 2. Visualize roi boundary
# ------------------------------------------------------------------
Map$centerObject(eeObject = roi, zoom = 9)
Map$addLayer(roi$style(color = "magenta", fillColor = "#ffffff00", width = 4))

# ------------------------------------------------------------------
# 3. Export roi boundary to Google Drive
# ------------------------------------------------------------------
task_roi <- ee_table_to_drive(
  collection = roi,
  description = 'kericho_district',
  folder = "kericho_folder",
  fileFormat = "GEOJSON"
)

task_roi$start()       # Begin export task
ee_monitoring(task_roi) # Track export progress

# ------------------------------------------------------------------
# Citation:
# Please cite data sources and tools appropriately in your project.
# ------------------------------------------------------------------

# End of script #####################################################
