###############################################################
# Script: s2_ndvi_tidyrgee.R
# Purpose: Demonstrate tidyrgee as a user-friendly approach for 
#          Google Earth Engine analysis in R
#
# Key Benefits of tidyrgee:
# 1. Familiar dplyr-style syntax for Earth Engine operations
# 2. Seamless integration with tidyverse workflows
# 3. Simplified temporal filtering and grouping
# 4. Easy conversion between EE objects and tidy data frames
#
# Workflow:
# 1. Initialize GEE and load Sentinel-2 data
# 2. Calculate NDVI using standard rgee approach
# 3. Convert to tidyrgee for intuitive data manipulation
# 4. Demonstrate tidyverse-style filtering and aggregation
# 5. Compare code complexity: base rgee vs. tidyrgee
#
# Author: Wyclife Agumba Oluoch
# Date: 2025-11-28
###############################################################

# ---------------------------------------------------------------------------
# Load libraries
# ---------------------------------------------------------------------------
library(rgee)        # Base Google Earth Engine interface
library(tidyrgee)    # Tidy approach to Earth Engine - SIMPLIFIES code!
library(dplyr)       # Consistent data manipulation syntax

# ---------------------------------------------------------------------------
# Initialize Google Earth Engine
# ---------------------------------------------------------------------------
ee_Initialize()

# ---------------------------------------------------------------------------
# 1. Define region of interest
# ---------------------------------------------------------------------------

roi <- ee$Geometry$Point(c(36.0, -0.5))$buffer(20000)

# ---------------------------------------------------------------------------
# 2. Standard rgee approach (more complex)
# ---------------------------------------------------------------------------

s2 <- ee$ImageCollection$Dataset$COPERNICUS_S2_SR_HARMONIZED
cs_plus <- ee$ImageCollection$Dataset$GOOGLE_CLOUD_SCORE_PLUS_V1_S2_HARMONIZED

# Traditional rgee requires chained EE methods and custom functions
ndvi <- s2$
  filterBounds(roi)$
  filterDate("2022-01-01", "2025-01-01")$
  linkCollection(cs_plus, "cs")$
  map(function(img) {
    qa <- img$select("cs")
    img <- img$updateMask(qa$gt(0.6))
    ndvi <- img$normalizedDifference(list("B8", "B4"))$rename("NDVI")
    ndvi$copyProperties(img, ee$List(list("system:time_start")))
  })$
  select("NDVI")

ndvi$size()$getInfo()
Map$centerObject(eeObject = roi, zoom = 10)
Map$addLayer(eeObject = ndvi$first()$clip(roi),
             visParams = list(min = 0.1,
                              max = 0.8,
                              palette = c(
                                "red",
                                "yellow",
                                "green"
                              )))
# ---------------------------------------------------------------------------
# 3. Convert to tidyrgee for SIMPLIFIED data manipulation
# ---------------------------------------------------------------------------
ndvi_tidy <- as_tidyee(ndvi)

# ---------------------------------------------------------------------------
# 4. Temporal filtering with familiar syntax
# ---------------------------------------------------------------------------

# tidyrgee approach (INTUITIVE)
recent_ndvi <- ndvi_tidy |> 
  filter(date >= "2024-06-01")
recent_ndvi

seasonal_ndvi <- ndvi_tidy |> 
  filter(month %in% c(6, 7, 8))  # Preferred months
seasonal_ndvi

# ---------------------------------------------------------------------------
# 5. Temporal aggregation made easy
# ---------------------------------------------------------------------------

monthly_stats <- ndvi_tidy |>
  filter(year %in% 2023:2024) |>
  group_by(year, month) |>
  summarise(stat = list("mean", "sd", "min"))

monthly_stats

# ---------------------------------------------------------------------------
# 6. Multiple statistics in one operation
# ---------------------------------------------------------------------------

multi_stats <- ndvi_tidy |>
  filter(year == 2024) |>
  group_by(month) |>
  summarise(stat = list("mean", "median", "sd", "min", "max"))
multi_stats

# ---------------------------------------------------------------------------
# 7. Seamless back to ee conversion
# ---------------------------------------------------------------------------

monthly_ee <- monthly_stats |> as_ee()
ee_print(monthly_ee)

# Cite data sources and packages you use accordingly :)

# ---------------------------------------------------------------------------
# End of script - Try tidyrgee for your next GEE project!
# ---------------------------------------------------------------------------