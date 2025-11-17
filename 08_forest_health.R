###############################################################
# Script: forest_health_ndvi_timeseries.R
#
# Purpose:
#   - Interactively draw a polygon region of interest (ROI)
#   - Load & cloud-mask Landsat 7 SR (Collection 2, Tier 1, L2)
#   - Compute annual mean NDVI composites (2000–2024)
#   - Reduce NDVI for the ROI (median statistic)
#   - Convert results to sf → dataframe
#   - Reshape data into long format
#   - Plot NDVI time series
#   - Export CSV for further analysis
#   - Visualize example NDVI layers on the map
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology
# (Like, Share, Subscribe, Comment)
# Date: 2025-11-08
###############################################################

# ---------------------------------------------------------------------------
# 1. Load libraries
# ---------------------------------------------------------------------------
library(rgee)       # Version 1.1.8   ==> Google Earth Engine for R
library(sf)         # Version 1.0.21  ==> Spatial vector data handling
library(dplyr)      # Version 1.1.4   ==> Data manipulation
library(tidyr)      # Version 1.3.1   ==> Reshaping long/wide tables
library(ggplot2)    # Version 4.0.0   ==> Plotting data
library(mapedit)    # Version 0.7.0   ==> Interactive drawing tools

# ---------------------------------------------------------------------------
# 2. Initialize Earth Engine
# ---------------------------------------------------------------------------
rgee::ee_Initialize()

# ---------------------------------------------------------------------------
# 3. Draw polygon ROI interactively
# ---------------------------------------------------------------------------
# Popup map allows user to draw any polygon
roi <- drawFeatures() |> 
  mutate(name = "Arabuko") |> 
  select(name) |> 
  sf_as_ee()

# Add polygon to map viewer
Map$centerObject(eeObject = roi, zoom = 11)
Map$addLayer(eeObject = roi$style(color = 'yellow', 
                                  fillColor = "#ffffff00",
                                  width = 4))

# ---------------------------------------------------------------------------
# 4. Define a date range for Landsat data
# ---------------------------------------------------------------------------
start_date <- "2000-01-01"
end_date   <- "2025-01-01"

# ---------------------------------------------------------------------------
# 5. Load Landsat 7 L2 + Cloud/Saturation Mask + NDVI
# ---------------------------------------------------------------------------
l7 <- ee$ImageCollection("LANDSAT/LE07/C02/T1_L2")$
  filterBounds(roi)$
  filterDate(start_date, end_date)$
  map(function(img) {
    
    # --- Cloud & saturation masking -----------------------------------------
    # QA_PIXEL bits (0-4) encode Fill, Dilated Cloud, Unused, Cloud, Cloud Shadow
    qa <- img$select("QA_PIXEL")$bitwiseAnd(31)$eq(0)
    
    # Radiometric saturation mask
    sat <- img$select("QA_RADSAT")$eq(0)
    
    # Rescale optical SR bands using USGS scale factors
    opticalBands <- img$select("SR_B.")$multiply(0.0000275)$add(-0.2)
    
    # Apply masks + add corrected bands
    img <- img$
      addBands(opticalBands, NULL, TRUE)$
      updateMask(qa)$
      updateMask(sat)
    
    # --- Compute NDVI --------------------------------------------------------
    ndvi <- img$normalizedDifference(c("SR_B4", "SR_B3"))$rename("ndvi")
    ndvi$copyProperties(img, ee$List(list("system:time_start")))
  })

# ---------------------------------------------------------------------------
# 6. Create a list of years
# ---------------------------------------------------------------------------
years <- ee$List$sequence(2000, 2024)

# ---------------------------------------------------------------------------
# 7. Build an annual mean NDVI image for each year
# ---------------------------------------------------------------------------
annual_ndvi <- years$map(
  ee_utils_pyfunc(function(year) {
    
    year  <- ee$Number(year)
    start <- ee$Date$fromYMD(year, 1, 1)
    end   <- start$advance(1, "year")
    
    # Mean NDVI composite for the given year
    annual_img <- l7$
      filterDate(start, end)$
      mean()$
      rename(ee$String("ndvi_")$cat(year$format("%d")))
    
    annual_img$set("year", year)
  })
)

# Convert yearly images → multiband stacked image
annual_bands <- ee$ImageCollection$fromImages(annual_ndvi)$toBands()
old_names <- annual_bands$bandNames()

clean_names <- old_names$map(ee_utils_pyfunc(function(n) {
  ee$String(n)$replace('^[0-9]+_', '')
}))

annual_bands_clean <- annual_bands$rename(clean_names)
# Optional: print metadata to console
ee_print(annual_bands_clean)

# ---------------------------------------------------------------------------
# 8. Reduce annual NDVI bands over the ROI
# ---------------------------------------------------------------------------
stats <- annual_bands_clean$reduceRegions(
  collection = roi,
  reducer   = ee$Reducer$median(),
  scale     = 100   # NDVI at 30 m, aggregation fine at 100 m
)

# Convert result → sf → dataframe
stats_sf <- ee_as_sf(stats)
stats_df <- stats_sf |> st_drop_geometry()

# ---------------------------------------------------------------------------
# 9. Clean + reshape dataframe for plotting
# ---------------------------------------------------------------------------

stats_df <- stats_df |> 
  pivot_longer(
    cols = -name,
    names_to = "year_metric",
    values_to = "ndvi",
  ) |> 
  separate(year_metric, into = c("prefix", "year"), sep = "_ndvi_") |> 
  mutate(year = as.integer(year)) |> 
  select(name, year, ndvi) |> 
  arrange(name, year)


# ---------------------------------------------------------------------------
# 10. Plot NDVI time series
# ---------------------------------------------------------------------------
ggplot(stats_df, aes(x = year, y = ndvi, group = name, colour = name)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "NDVI",
    title = "Annual NDVI (Landsat 7) — 2000 to 2024"
  ) +
  theme_minimal(base_size = 14)

# ---------------------------------------------------------------------------
# 11. Export CSV file
# ---------------------------------------------------------------------------
write.csv(
  stats_df,
  'arabuko_ndvi_timeseries.csv',
  row.names = FALSE
)

# ---------------------------------------------------------------------------
# 12. Visualize NDVI layers (example: year 2000 & 2024)
# ---------------------------------------------------------------------------
Map$addLayer(
  eeObject = annual_bands_clean$select("ndvi_2000")$clip(roi),
  visParams = list(
    min = 0, max = 1,
    palette = c("red", "yellow", "darkgreen")
  )
) |
  Map$addLayer(
    eeObject = annual_bands_clean$select("ndvi_2020")$clip(roi),
    visParams = list(
      min = 0, max = 1,
      palette = c("red", "yellow", "darkgreen")
    )
  )

# Cite all data sources and packages accordingly

# ---------------------------------------------------------------------------
# End of Script
# ---------------------------------------------------------------------------


