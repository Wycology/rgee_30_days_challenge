###############################################################
# Script: annual_vpd_rgee.R
# Purpose: Prepare annual Vapor Pressure Deficit (VPD) time series
#          for Kericho (Kenya) using rgee and visualize results.
#          - Load GAUL admin boundaries
#          - Load TerraClimate VPD image collection
#          - Aggregate annual VPD (mean)
#          - Convert to multi-band image, clean band names
#          - Reduce over Kericho and plot time series with ggplot2
#          - Visualize example years on the interactive map
#
# Author: Wyclife Agumba Oluoch 
# YouTube: https://www.youtube.com/@wycology 
# (Like, share, subscribe, comment)
# Date: 2025-11-05
###############################################################

# ---------------------------------------------------------------------------
# Notes / setup
# - This script uses rgee and expects a working Earth Engine authentication.
# - If running for the first time in a new R environment, follow the
#   installation notes in the repository README.md
# ---------------------------------------------------------------------------

# Load libraries -------------------------------------------------------------
library(rgee)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)

# Initialize Earth Engine (use drive = TRUE if you plan to export to Drive)
ee_Initialize(drive = TRUE)

# ---------------------------------------------------------------------------
# 1. Load administrative boundary (GAUL) and isolate Kericho (Kenya)
# ---------------------------------------------------------------------------
# GAUL level2 contains second-level administrative units (ADM2_NAME)
gaul <- ee$FeatureCollection("FAO/GAUL/2015/level2")
kericho <- gaul$filter(ee$Filter$eq("ADM2_NAME", "Kericho"))

# Optional: check number of features (should be 1 for Kericho)
# print(kericho$size()$getInfo())

# ---------------------------------------------------------------------------
# 2. Load TerraClimate VPD collection and filter by date
# ---------------------------------------------------------------------------
vpd_coll <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")

# Analysis date range (annual composites up to 2024 inclusive)
start_date <- "1958-01-01"
end_date   <- "2024-12-31"

vpdCollection <- vpd_coll$
  filterDate(start_date, end_date)$
  select('vpd')

# ---------------------------------------------------------------------------
# 3. Build annual mean VPD images (1958 - 2024)
# ---------------------------------------------------------------------------
years <- ee$List$sequence(1958, 2024)

annualVPD <- years$map(ee_utils_pyfunc(function(year) {
  year <- ee$Number(year)
  start <- ee$Date$fromYMD(year, 1, 1)
  end <- start$advance(1, 'year')
  
  annualImg <- vpdCollection$
    filterDate(start, end)$
    mean()$                           # mean VPD across the year
    rename(ee$String('vpd_mean_')$cat(year$format('%d')))
  
  # Attach year as metadata
  annualImg$set('year', year)
}))

# Convert list-of-images → ImageCollection
vpd_ic <- ee$ImageCollection$fromImages(annualVPD)

# Convert to one multi-band image (each band = one year)
vpd_bands <- vpd_ic$toBands()$multiply(0.01)

# ---------------------------------------------------------------------------
# 4. Clean band names (remove numeric prefixes added by toBands())
# ---------------------------------------------------------------------------
old_names <- vpd_bands$bandNames()

clean_names <- old_names$map(ee_utils_pyfunc(function(n) {
  # Remove leading digits + underscore that toBands() creates
  ee$String(n)$replace('^[0-9]+_', '')
}))

vpd_bands_clean <- vpd_bands$rename(clean_names)

# ---------------------------------------------------------------------------
# 5. Reduce (mean) VPD inside Kericho and bring results to R
# ---------------------------------------------------------------------------
# Use reduceRegions over the Kericho feature collection. Using mean() here.
stats <- vpd_bands_clean$reduceRegions(
  reducer = ee$Reducer$mean(),
  collection = kericho,
  scale = 4638.3
)

# Convert to sf in R
stats_sf <- ee_as_sf(stats)

# Drop geometry and prepare for reshaping
stats_df <- stats_sf %>% sf::st_drop_geometry() |> 
  select(starts_with("vpd"))

# ---------------------------------------------------------------------------
# 6. Reshape results to long format for plotting
# ---------------------------------------------------------------------------

vpd_cols <- grep("vpd", names(stats_df), value = TRUE)

df <- stats_df %>%
  select(all_of(vpd_cols)) %>%
  pivot_longer(cols = everything(),
               names_to = "band",
               values_to = "vpd") %>%
  mutate(
    year = as.integer(gsub('.*_(\\d{4})$', '\\1', band))
  ) %>%
  arrange(year)

# ---------------------------------------------------------------------------
# 7. Plot VPD time series
# ---------------------------------------------------------------------------
ggplot(df, aes(x = year, y = vpd)) +
  geom_line() +
  geom_point() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Annual Vapor Pressure Deficit (VPD) in Kericho",
    x = "Year",
    y = "VPD"
  )

# ---------------------------------------------------------------------------
# 8. Visualize example years on the interactive map (Map from rgee)
# ---------------------------------------------------------------------------
# Center map on Kericho
Map$centerObject(kericho, zoom = 9)

# Add layer to map 
Map$addLayer(vpd_bands_clean$select("vpd_mean_2007")$clip(kericho), 
             list(min = 0.75, 
                  max = 1.1, 
                  palette = c("green", "yellow", "red")), 
             "VPD 2018" ) | 
  Map$addLayer(vpd_bands_clean$select("vpd_mean_2019")$clip(kericho), 
               list(min = 0.75, 
                    max = 1.1, 
                    palette = c("green", "yellow", "red")), 
               "VPD 2019" )

# Kindly cite the datasources and packages in your research works.

# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------
