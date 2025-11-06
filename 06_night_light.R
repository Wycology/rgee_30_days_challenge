###############################################################
# Script: annual_viirs_nightlights_rgee.R
# Purpose: Extract and visualize annual VIIRS nighttime light 
#          intensity (NOAA VIIRS DNB) for drawn regions 
#          (Tea zone & Sugar cane zone) using Google Earth Engine via rgee.
#
# Workflow:
#   1. Initialize rgee
#   2. Draw polygons interactively (mapedit)
#   3. Assign zone names (Tea zone, Sugarcane zone)
#   4. Load VIIRS Nightlights ImageCollection
#   5. Create annual mean composites (2013–2024)
#   6. Convert list-of-images → multi-band image (toBands)
#   7. Reduce annual values over regions (mean)
#   8. Convert to R sf → dataframe → tidy format
#   9. Plot annual time series for both regions using ggplot2
#  10. Visualize interesting years interactively
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# (Like, share, subscribe, comment)
# Date: 2025-11-06
###############################################################

# ---------------------------------------------------------------------------
# Load libraries
# ---------------------------------------------------------------------------
library(rgee)       # Version 1.1.8
library(mapedit)    # Version 0.7.0
library(dplyr)      # Version 1.1.4
library(tidyr)      # Version 1.3.1
library(ggplot2)    # Version 4.0.0
library(sf)         # Version 1.0.21

# ---------------------------------------------------------------------------
# Initialize Google Earth Engine
# ---------------------------------------------------------------------------
ee_Initialize()

# ---------------------------------------------------------------------------
# 1. Draw regions interactively and assign names
# ---------------------------------------------------------------------------
zones <- drawFeatures()

zones <- zones |> 
  mutate(zone = c("Tea zone", "Sugarcane zone")) |>   # assign region names manually
  select(zone)

# Convert drawn regions to Earth Engine format
zones <- zones |> sf_as_ee()

# ---------------------------------------------------------------------------
# 2. Load VIIRS Nighttime Lights (Monthly VCMCFG)
# ---------------------------------------------------------------------------
night_light <- ee$ImageCollection("NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG")
               ee$ImageCollection("NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG")
night_light <- night_light$select("avg_rad")
# ---------------------------------------------------------------------------
# 3. Define year range and compute annual mean composites
# ---------------------------------------------------------------------------
years <- ee$List$sequence(2013, 2024)

annual_imgs <- years$map(
  ee_utils_pyfunc(function(year) {
    year  <- ee$Number(year)
    start <- ee$Date$fromYMD(year, 1, 1)
    end   <- start$advance(1, "year")
    
    # Mean nighttime light intensity over the year
    annual_img <- night_light$
      filterDate(start, end)$
      mean()$
      rename(ee$String("light_")$cat(year$format("%d")))
    
    # Attach year metadata
    annual_img$set("year", year)
  })
)

# Convert list of images to an ImageCollection, then to a multi-band image
annual_imgs <- ee$ImageCollection$fromImages(annual_imgs)$toBands()

# ---------------------------------------------------------------------------
# 4. Reduce (mean) annual values over drawn regions
# ---------------------------------------------------------------------------
stat <- annual_imgs$reduceRegions(
  collection = zones,
  reducer    = ee$Reducer$mean(),
  scale      = 463.83
)

# ---------------------------------------------------------------------------
# 5. Convert Earth Engine results to R sf → dataframe
# ---------------------------------------------------------------------------
stat_sf <- ee_as_sf(stat)
stat_df <- stat_sf |> st_drop_geometry()

# Identify bands representing nighttime lights
light_cols <- grep("_light_", names(stat_df), value = TRUE)

# ---------------------------------------------------------------------------
# 6. Reshape results to tidy long format for plotting
# ---------------------------------------------------------------------------
df <- stat_df %>%
  pivot_longer(
    cols      = all_of(light_cols),
    names_to  = "band",
    values_to = "light"
  ) %>%
  mutate(
    # Extract year from band name: e.g., "X3_light_2016" → 2016
    year = as.integer(gsub(".*_(\\d{4})$", "\\1", band))
  ) %>%
  arrange(zone, year)

# ---------------------------------------------------------------------------
# 7. Plot annual nighttime light series for each region
# ---------------------------------------------------------------------------
ggplot(df, aes(
  x     = as.integer(year),
  y     = light,
  group = zone,
  color = zone
)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = unique(df$year)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Annual Nighttime Light Intensity (VIIRS)",
    x     = "Year",
    y     = "Mean Light (nW/sr/cm²)",
    color = "Zone"
  )

Map$centerObject(eeObject = zones, zoom = 10)
Map$addLayer(eeObject = annual_imgs$select("6_light_2019")$clip(zones),
             visParams = list(
               min = 0.25,
               max = 0.55
             )) 
# Cite data sources and packages used accordingly 

# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------