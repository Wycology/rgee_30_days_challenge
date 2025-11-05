###############################################################
# Script: ndvi_composites_rgee.R
# Purpose: Draw an ROI interactively, compute NDVI composites 
#          for 2018 and 2025 using Sentinel-2 and Cloud Score Plus, 
#          visualize layers, and export NDVI as GeoTIFF to Google Drive.
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology (Like, share, subscribe, comment)
# Date: 2025-11-01
###############################################################

# In case you are running rgee for the first time, consider these:
# install.packages("rgee)
# library(rgee)
# ee_clean_pyenv()
# ee_install(py_env = "rgee_env")
# ee_install_upgrade(earthengine_env = "rgee_env")
# ee_install_set_pyenv(py_path = NULL, py_env = "rgee_env")
# ee_Authenticate(user = "your user name", earthengine = TRUE, drive = TRUE)
# If you are using Windows, you may need to close R Studio severally

# Load required libraries ----------------------------------------------------
library(rgee)     # Earth Engine API for R
library(mapedit)  # For interactively drawing an ROI

# Initialize Earth Engine ----------------------------------------------------
# drive = TRUE allows exporting results to Google Drive
ee_Initialize(drive = TRUE)

# ---------------------------------------------------------------------------
# 1. Define Region of Interest (ROI)
# ---------------------------------------------------------------------------

# Interactively draw polygon(s) on a leaflet map
roi <- drawFeatures()

# Convert the drawn ROI (sf object) to an Earth Engine geometry
roi <- sf_as_ee(roi)

# ---------------------------------------------------------------------------
# 2. Load image collections
# ---------------------------------------------------------------------------

# Sentinel-2 Level-2A Harmonized collection
s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")

# Cloud Score Plus for Sentinel-2: provides per-pixel cloud probability
cs_plus <- ee$ImageCollection("GOOGLE/CLOUD_SCORE_PLUS/V1/S2_HARMONIZED")

# ---------------------------------------------------------------------------
# 3. Function to compute NDVI composite for a given year
# ---------------------------------------------------------------------------
compute_ndvi_composite <- function(ic, year_start, year_end, roi, cs_plus) {
  ic$
    filterDate(year_start, year_end)$       # Select time range
    filterBounds(roi)$                      # Clip to ROI
    select(c("B4", "B8"))$                  # Red and NIR bands
    linkCollection(cs_plus, "cs")$          # Attach Cloud Score Plus band
    map(function(img) {
      qa <- img$select("cs")                # Cloud score band
      img <- img$updateMask(qa$gt(0.6))
      ndvi <- img$normalizedDifference(c("B8", "B4"))$rename("ndvi")
    })$
    median()$                               # Composite using per-pixel median
    clip(roi)                               # Clip final image to ROI
}

# ---------------------------------------------------------------------------
# 4. Compute NDVI composites
# ---------------------------------------------------------------------------

ndvi_2025 <- compute_ndvi_composite(
  ic = s2,
  year_start = "2025-01-01",
  year_end   = "2026-01-01",
  roi = roi,
  cs = cs_plus
)

ndvi_2018 <- compute_ndvi_composite(
  ic = s2,
  year_start = "2018-01-01",
  year_end   = "2019-01-01",
  roi = roi,
  cs = cs_plus
)

# ---------------------------------------------------------------------------
# 5. Visualize the NDVI results
# ---------------------------------------------------------------------------

Map$centerObject(roi, zoom = 13)

# Visualization parameters
vis <- list(
  min = 0.1,
  max = 0.8,
  palette = c("red", "yellow", "green")   # Low → high NDVI
)

# Add layers to Earth Engine map
map_2025 <- Map$addLayer(
  eeObject = ndvi_2025,
  visParams = vis,
  name = "NDVI 2025"
)

map_2018 <- Map$addLayer(
  eeObject = ndvi_2018,
  visParams = vis,
  name = "NDVI 2018"
)

# Display both maps
map_2018 | map_2025
map_2018 + map_2025

# ---------------------------------------------------------------------------
# 6. Export NDVI 2025 composite to Google Drive
# ---------------------------------------------------------------------------

task_img <- ee_image_to_drive(
  image = ndvi_2025,
  scale = 10,                                # Sentinel-2 resolution
  folder = "ndvi_2025",                      # Folder in Google Drive
  fileFormat = "GEO_TIFF",
  region = roi$geometry(),
  fileNamePrefix = "ndvi_2025"
)

task_img$start()         # Start export task
ee_monitoring(task_img)  # Monitor task in console
#END#