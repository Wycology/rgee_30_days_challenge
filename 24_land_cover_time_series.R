###############################################################
# Script: 24_landcover_roi.R
# Purpose: Extract the dominant land cover class for a  
#          region of interest (ROI) from the GLC-FCS30D annual 
#          global land cover dataset for years 2000–2022.
#
# Workflow:
#   1. Initialize rgee
#   2. Draw ROI interactively using mapedit
#   3. Load GLC-FCS30D annual land cover collection
#   4. Mosaic the collection into a single image
#   5. Rename bands to "lc_YEAR" (e.g., lc_2000, lc_2001, ...)
#   6. Reduce the image over the ROI to extract the mode (most 
#      frequent land cover class) per year
#   7. Retrieve results to R using getInfo()
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# Date: 2025-11-24
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
# 2. Load annual global land cover dataset (GLC-FCS30D)
# ---------------------------------------------------------------------------
land_cover <- ee$ImageCollection("projects/sat-io/open-datasets/GLC-FCS30D/annual")$
  mosaic()   # Combine collection into a single image

# ---------------------------------------------------------------------------
# 3. Rename bands for clarity: "lc_2000", "lc_2001", ..., "lc_2022"
# ---------------------------------------------------------------------------
clean_names <- ee$List$sequence(2000, 2022)$
  map(ee_utils_pyfunc(function(y) {
    ee$String("lc_")$cat(ee$Number(y)$format("%d"))
  }))
land_cover <- land_cover$rename(clean_names)

# ---------------------------------------------------------------------------
# 4. Reduce image over ROI to extract dominant land cover class per year
# ---------------------------------------------------------------------------
lc <- land_cover$reduceRegion(
  reducer  = ee$Reducer$mode(),  # Take the most frequent class
  geometry = roi,
  scale    = 30
)
Map$addLayer(land_cover)
# ---------------------------------------------------------------------------
# 5. Retrieve results to R
# ---------------------------------------------------------------------------
lc_info <- lc$getInfo()
print(lc_info)
as.data.frame(unlist(lc_info))

# Cite data sources and packages you use in your project accordingly, :)

# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------
