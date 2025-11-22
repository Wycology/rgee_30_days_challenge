###############################################################
# Script: 22_export_image.R
# Purpose: Compute monthly NDVI composites using Sentinel-2 
#          SR Harmonized imagery with Cloud Score Plus masking 
#          for a user-drawn region of interest (ROI). 
#          Generates a multiband NDVI image and allows direct 
#          download or Google Drive export.
#
# Workflow:
#   1. Initialize rgee
#   2. Draw ROI interactively using mapedit
#   3. Load Sentinel-2 SR Harmonized + Cloud Score Plus
#   4. Filter images by date & ROI
#   5. Mask clouds using Cloud Score Plus (CS > 0.6)
#   6. Compute NDVI for each image (B8, B5)
#   7. Aggregate images to monthly median NDVI composites
#   8. Rename bands and convert to a multiband image
#   9. Visualize NDVI for a selected month
#  10. Export NDVI as raster or to Google Drive
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# Date: 2025-11-22
###############################################################

# ---------------------------------------------------------------------------
# Load libraries
# ---------------------------------------------------------------------------
library(rgee)     # Version 1.1.8.9000 Interface to Google Earth Engine
library(mapedit)  # Version 0.7.0 For interactive polygon drawing

# ---------------------------------------------------------------------------
# Initialize Google Earth Engine
# ---------------------------------------------------------------------------
ee_Initialize()

# ---------------------------------------------------------------------------
# 1. Draw ROI interactively
# ---------------------------------------------------------------------------
roi <- drawFeatures() |> sf_as_ee()

# ---------------------------------------------------------------------------
# 2. Load Sentinel-2 SR Harmonized + Cloud Score Plus collections
# ---------------------------------------------------------------------------
s2 <- ee$ImageCollection$Dataset$COPERNICUS_S2_SR_HARMONIZED
cs_plus <- ee$ImageCollection$Dataset$GOOGLE_CLOUD_SCORE_PLUS_V1_S2_HARMONIZED

# ---------------------------------------------------------------------------
# 3. Define analysis period
# ---------------------------------------------------------------------------
start_date <- "2024-01-01"
end_date   <- "2025-01-01"

# ---------------------------------------------------------------------------
# 4. Filter images by ROI and date, link Cloud Score, mask clouds, compute NDVI
# ---------------------------------------------------------------------------
ndvi <- s2$
  filterBounds(roi)$
  filterDate(start_date, end_date)$
  linkCollection(cs_plus, "cs")$             # Attach Cloud Score Plus
  map(function(img) {
    qa <- img$select("cs")
    img <- img$updateMask(qa$gt(0.6))       # Keep pixels with CS > 0.6
    ndvi <- img$normalizedDifference(c("B8", "B5"))$rename("ndvi")
    ndvi$copyProperties(img, ee$List(list("system:time_start")))
  })

# ---------------------------------------------------------------------------
# 5. Compute monthly median NDVI composites
# ---------------------------------------------------------------------------
months <- ee$List$sequence(1, 12)

monthly_imgs <- months$map(
  ee_utils_pyfunc(function(m) {
    m <- ee$Number(m)
    img <- ndvi$
      filter(ee$Filter$calendarRange(m, m, "month"))$
      median()
    band_name <- ee$String("ndvi_")$cat(m$format("%d"))
    img$select("ndvi")$rename(band_name)$set("month", m)
  })
)

monthly_ic <- ee$ImageCollection$fromImages(monthly_imgs)
monthly_bands <- monthly_ic$toBands()

# Clean band names
old_names <- monthly_bands$bandNames()
clean_names <- old_names$map(
  ee_utils_pyfunc(function(nm) {
    ee$String(nm)$replace("^[0-9]+_", "")
  })
)
monthly_bands_clean <- monthly_bands$rename(clean_names)$clip(roi)

# ---------------------------------------------------------------------------
# 6. Visualize NDVI for a selected month (e.g., April)
# ---------------------------------------------------------------------------
Map$centerObject(roi, zoom = 15)
Map$addLayer(
  monthly_bands_clean$select("ndvi_4"),
  list(min = 0, max = 1, palette = c("brown", "yellow", "green")),
  "April NDVI"
) +
  Map$addLayer(roi$style(color = "red", fillColor = "#ffffff00"))

# ---------------------------------------------------------------------------
# 7. Export NDVI as raster to local disk
# ---------------------------------------------------------------------------
img_down <- ee_as_rast(
  image  = monthly_bands_clean,
  region = roi$geometry(),
  scale  = 10,
  dsn    = 'rgee_export.tif'
)

# ---------------------------------------------------------------------------
# 8. Export NDVI as multiband GeoTIFF to Google Drive
# ---------------------------------------------------------------------------
task_drive <- ee_image_to_drive(
  image       = monthly_bands_clean,
  description = "monthly_ndvi_2024",
  folder      = "GEE_Exports",
  fileFormat  = "GeoTIFF",
  region      = roi,
  scale       = 250
)
task_drive$start()
ee_monitoring(task_drive)

# Cite all data sources and packages you use accordingly :)

# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------
