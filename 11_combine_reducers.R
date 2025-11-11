###############################################################
# Script: 11_combine_reducers.R
# Purpose: Extract elevation values and descriptive statistics
#          from the SRTM Digital Elevation Model (USGS_SRTMGL1_003)
#          for a user-drawn region using Google Earth Engine via rgee.
#
# Workflow:
#   1. Initialize rgee
#   2. Draw region of interest (ROI) interactively
#   3. Load SRTM 30m elevation data set
#   4. Compute mean elevation for ROI
#   5. Visualize clipped elevation on map
#   6. Build a combined reducer (mean, sd, min, max, median, mode, variance)
#   7. Compute all descriptive statistics for ROI
#   8. Print results to console
#
# Author: Wyclife Agumba Oluoch
# GitHub: https://github.com/Wycology/rgee_30_days_challenge
# YouTube: https://www.youtube.com/@wycology
# Date: 2025-11-11
###############################################################

# ---------------------------------------------------------------------------
# Load rgee package and initialize GEE
# ---------------------------------------------------------------------------
library(rgee)     # Version 1.1.8.9000
library(mapedit)  # Version 0.7.0
ee_Initialize()

# ---------------------------------------------------------------------------
# 1. Load SRTM elevation data set (30 m global DEM)
# ---------------------------------------------------------------------------
elev <- ee$Image$Dataset$USGS_SRTMGL1_003

# ---------------------------------------------------------------------------
# 2. Draw region of interest interactively
# ---------------------------------------------------------------------------
roi <- drawFeatures() |> 
  sf_as_ee()

# ---------------------------------------------------------------------------
# 3. Compute mean elevation within the ROI
# ---------------------------------------------------------------------------
mean_elev <- elev$reduceRegion(
  reducer  = ee$Reducer$mean(),
  geometry = roi,
  scale    = 30
)

# Print the mean elevation (meters)
mean_elev$getInfo()

# ---------------------------------------------------------------------------
# 4. Visualize elevation clipped to the ROI
# ---------------------------------------------------------------------------
Map$centerObject(eeObject = roi, zoom = 12)

Map$addLayer(
  eeObject = elev$clip(roi),
  visParams = list(
    min = 1700,
    max = 2300,          # Adjust to your terrain
    palette = c(
      "blue", "green", "yellow", "orange", "red"
    )
  ),
  name = "SRTM Elevation (m)"
)

# ---------------------------------------------------------------------------
# 5. Create a list of reducers for summary statistics
# ---------------------------------------------------------------------------
rlist <- list(
  ee$Reducer$mean(),
  ee$Reducer$stdDev(),
  ee$Reducer$min(),
  ee$Reducer$max(),
  ee$Reducer$median(),
  ee$Reducer$mode(),
  ee$Reducer$variance()
)

# Combine reducers into one
reducers <- rlist[[1]]
for (i in 2:length(rlist)) {
  reducers <- reducers$combine(
    reducer2     = rlist[[i]],
    sharedInputs = TRUE
  )
}

# ---------------------------------------------------------------------------
# 6. Compute all statistics over ROI
# ---------------------------------------------------------------------------
stats <- elev$reduceRegion(
  reducer    = reducers,
  geometry   = roi,
  bestEffort = TRUE    # Allows coarse scale if needed
)

# Print dictionary of all statistics
stats$getInfo()

# Cite data sources and packages you use accordingly
# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------
