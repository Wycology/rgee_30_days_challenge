################################################################################################
# Script: gimms_ndvi_consolidated_rgee.R
# Purpose: Load the PKU GIMMS AVHRR-MODIS consolidated NDVI dataset (1982–2022), apply quality
#          control (QC) filtering, compute annual mean NDVI, summarize at your region of interest
#          , and plot NDVI trends in R.
#
# Dataset:
#   - PKU-GIMMS NDVI Consolidated (AVHRR + MODIS blending)
#   - DOI & Resources:
#       https://essd.copernicus.org/articles/15/4181/2023/essd-15-4181-2023.html
#       https://gee-community-catalog.org/projects/gimms_ndvi/
#       https://zenodo.org/records/8253971
#
# Author: Wyclife Agumba Oluoch
# Date: 2025-11-26
################################################################################################

# -----------------------------------------------------------------------------------------------
# 0. Libraries and Earth Engine initialization
# -----------------------------------------------------------------------------------------------
library(rgee)       # Version 1.1.8.9000 Google Earth Engine API for R
library(tidyverse)  # Version 2.0.0 Data manipulation and visualization

ee_Initialize()

# -----------------------------------------------------------------------------------------------
# 1. Load the PKU-GIMMS Consolidated NDVI Image Collection
#    Raw collection contains two bands:
#        b1 = NDVI (scaled by 1000)
#        b2 = QC code encoding AVHRR and MODIS quality flags
# -----------------------------------------------------------------------------------------------

conso <- ee$ImageCollection("projects/sat-io/open-datasets/PKU-GIMMS-NDVI/AVHRR_MODIS_CONSOLIDATED")

# Quick test sample at a point in Mau Forest of Kenya
img <- conso$first()
test_sample <- img$sample(
  region = ee$Geometry$Point(35.37218, -0.47255),
  scale = 10000
)
ee_as_sf(test_sample) # b1 = 857 (Suggests ndvi), b2 = 209 (suggests QC)

# -----------------------------------------------------------------------------------------------
# 2. Quality Control (QC) Mask Function
#
# QC Encoding:
#   - The QC band encodes two digits:
#       d2 (tens): AVHRR NDVI quality     (0=good, 9=not applicable)
#       d3 (units): MODIS NDVI quality    (0=good, 9=not applicable)
#
# Filtering:
#   - Keep pixels where AVHRR and MODIS QC values are 0 (good) or 9 (not applicable)
#   - Remove sparse vegetation by enforcing NDVI >= 0.1 (100 in scaled units)
# -----------------------------------------------------------------------------------------------

maskQC <- function(img) {
  ndvi <- img$select("b1")
  qc   <- img$select("b2")
  
  # Decode QC digits
  d2 <- qc$divide(10)$floor()$mod(10)   # AVHRR QC (tens digit)
  d3 <- qc$mod(10)                      # MODIS QC (units digit)
  
  # Keep only good or N/A
  good_avhrr <- d2$eq(0)$Or(d2$eq(9))
  good_modis <- d3$eq(0)$Or(d3$eq(9))
  good_qc    <- good_avhrr$And(good_modis)
  
  # Minimum vegetation threshold (scaled NDVI >= 0.1)
  veg_mask <- ndvi$gte(100)
  
  img$select("b1")$
    updateMask(good_qc)$
    updateMask(veg_mask)
}

# Apply QC mask across entire collection, keeping only NDVI band
conso_clean <- conso$map(maskQC)$select("b1")

# -----------------------------------------------------------------------------------------------
# 3. Generate Annual NDVI Means (1982–2022)
# -----------------------------------------------------------------------------------------------

years <- ee$List$sequence(1982, 2022)

annual_imgs <- years$map(
  ee_utils_pyfunc(function(year) {
    year <- ee$Number(year)
    start <- ee$Date$fromYMD(year, 1, 1)
    end   <- start$advance(1, "year")
    
    img <- conso_clean$
      filterDate(start, end)$
      mean()
    
    band_name <- ee$String("ndvi_")$cat(year$format("%04d"))
    img$rename(band_name)$set("year", year)
  })
)

# Combine into single multi-band image, apply scale factor
annual <- ee$ImageCollection$fromImages(annual_imgs)$toBands()$multiply(0.001)

# Clean up band names (remove numeric prefixes)
old_names <- annual$bandNames()
new_names <- old_names$map(
  ee_utils_pyfunc(function(n) ee$String(n)$replace("[0-9]+_", "")) # keep ndvi_YYYY
)
annual_rename <- annual$rename(new_names)

# Visualization example (1982 NDVI)
Map$addLayer(
  eeObject = annual_rename$select("ndvi_1982"),
  visParams = list(
    min = 0.1, max = 1,
    palette = c("red","orange","yellow","palegreen","green","darkgreen")
  ),
  name = "NDVI 1982"
)

# -----------------------------------------------------------------------------------------------
# 4. Regional Aggregation: Compute NDVI Trend for Kericho (Kenya)
#    Using FAO GAUL Level-2 boundaries
# -----------------------------------------------------------------------------------------------

world <- ee$FeatureCollection$Dataset$FAO_GAUL_2015_level2
roi <- world$filter(ee$Filter$eq("ADM2_NAME", "Kericho"))

stats <- annual_rename$reduceRegions(
  collection = roi,
  reducer = ee$Reducer$mean(),
  scale = 10000
)

# Convert to sf and format for ggplot trend visualization
stats_sf <- ee_as_sf(stats)

stats_sf |>
  sf::st_drop_geometry() |>
  select(starts_with("ndvi")) |>
  pivot_longer(cols = everything(), names_to = "year", values_to = "ndvi") |>
  separate(year, into = c("prefix", "year"), sep = "_") |>
  mutate(year = as.integer(year)) |>
  ggplot(aes(x = year, y = ndvi)) +
  geom_line() +
  geom_point() +
  theme_minimal(base_size = 16)

# -----------------------------------------------------------------------------------------------
# 5. Optional: Inspect Raw QC Values Within Kericho (for validation / troubleshooting)
# -----------------------------------------------------------------------------------------------
# Like how good quality are pixels in my roi?
raw_qc <- conso$
  first()$
  select("b2")$
  clip(roi)$
  sample(region = roi) |>
  ee_as_sf() |>
  sf::st_drop_geometry() |>
  select(b2)

table(raw_qc$b2) # Yeeey! Only one had 519 (bad)

# Cite data sets and packages you use accordingly, :)

# END OF SCRIPT
################################################################################################
