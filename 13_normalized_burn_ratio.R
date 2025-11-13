###############################################################
# Script: burned_area_rgee.R
# Purpose: Draw an ROI interactively, compute NBR composites 
#          before and after a fire using Sentinel-2 + Cloud Score Plus, 
#          derive dNBR (burn severity), visualize results, 
#          and display burned area mask.
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# (Like, share, subscribe, comment)
# Date: 2025-11-13
###############################################################

# In case you are running rgee for the first time, check README.md file 
# in my github repository https://github.com/Wycology/rgee_30_days_challenge/

# Load required libraries ----------------------------------------------------
library(rgee)     # Version 1.1.8.9000 Earth Engine API for R
library(mapedit)  # Version 0.7.0 For interactively drawing an ROI

# Initialize Earth Engine ----------------------------------------------------
ee_Initialize()

# ---------------------------------------------------------------------------
# 1. Define Region of Interest (ROI)
# ---------------------------------------------------------------------------

# Interactively draw polygon(s) on a leaflet map
roi <- drawFeatures() |> sf_as_ee()

# ---------------------------------------------------------------------------
# 2. Load image collections
# ---------------------------------------------------------------------------

# Sentinel-2 Level-2A Harmonized
s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")

# Cloud Score Plus for Sentinel-2 (per-pixel cloud probability)
cs_plus <- ee$ImageCollection("GOOGLE/CLOUD_SCORE_PLUS/V1/S2_HARMONIZED")

# ---------------------------------------------------------------------------
# 3. Function to compute a clean NBR composite for any time window
# ---------------------------------------------------------------------------

# NBR = (NIR - SWIR2) / (NIR + SWIR2)
compute_nbr_composite <- function(start, end, roi) {
  s2$
    filterDate(start, end)$
    filterBounds(roi)$
    select(c("B8", "B12"))$                # B8 = NIR, B12 = SWIR2
    linkCollection(cs_plus, "cs")$         # Attach Cloud Score Plus
    map(function(img) {
      qa <- img$select("cs")               # Cloud score
      img <- img$updateMask(qa$gt(0.6))    # Cloud masking
      img$normalizedDifference(c("B8", "B12"))$rename("NBR")
    })$
    median()$
    clip(roi)
}

# ---------------------------------------------------------------------------
# 4. Compute NBR before and after the fire
# ---------------------------------------------------------------------------

nbr_before <- compute_nbr_composite(
  start = "2023-01-01",
  end   = "2023-06-01",
  roi   = roi
)

nbr_after <- compute_nbr_composite(
  start = "2023-06-02",
  end   = "2023-12-31",
  roi   = roi
)

# ---------------------------------------------------------------------------
# 5. Compute dNBR and burned area mask
# ---------------------------------------------------------------------------

# dNBR = NBR_before - NBR_after
dnbr <- nbr_before$subtract(nbr_after)$rename("dNBR")

# Simple burned area threshold (USGS low severity threshold ~0.1)
burned_area <- dnbr$gt(0.1)$selfMask()

# ---------------------------------------------------------------------------
# 6. Visualize results
# ---------------------------------------------------------------------------

Map$centerObject(roi, 12)

# Visualization for dNBR
vis_dnbr <- list(
  min = 0,
  max = 0.6,
  palette = c("white", "yellow", "red")
)

Map$addLayer(
  eeObject = dnbr,
  visParams = vis_dnbr,
  name = "dNBR (Burn Severity)"
) |

Map$addLayer(
  eeObject = burned_area,
  visParams = list(palette = "red"),
  name = "Burned Area Mask"
)
# Cite data sources and packages you use accordingly, :)

# ---------------------------------------------------------------------------
# End of Script
# ---------------------------------------------------------------------------