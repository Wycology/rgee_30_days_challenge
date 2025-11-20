###############################################################
# Script: 20_conservation_sites_evaluation.R
#
# Purpose:
#   Demonstrate how to interactively select conservation sites
#   and analyze their NDVI (Normalized Difference Vegetation Index) trends
#   over time using Sentinel-2 data in Google Earth Engine (GEE) via rgee.
#
# Workflow:
#   1. Initialize Earth Engine
#   2. Draw treatment and control sites interactively
#   3. Extract Sentinel-2 imagery and cloud scores
#   4. Compute NDVI for each image, masking clouds
#   5. Aggregate NDVI to annual median per site
#   6. Convert results to an R dataframe for analysis
#   7. Plot NDVI time series to visualize treatment effects
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology
# Date: 2025-11-20
###############################################################

# ---------------------------------------------------------------------------
# Load required libraries
# ---------------------------------------------------------------------------
library(rgee)       # Version 1.1.8.9000 Earth Engine API for R
library(dplyr)      # Version 1.1.4 Data manipulation
library(tidyr)      # Version 1.3.1 Data reshaping
library(sf)         # Version 1.0.22 Spatial vector data handling
library(ggplot2)    # Version 4.0.1 Visualization of time series NDVI
library(mapedit)    # Version 0.7.0 Interactive map editing (drawing polygons)

# Initialize Earth Engine
ee_Initialize()

# ---------------------------------------------------------------------------
# 1. Draw conservation treatment and control sites interactively
# ---------------------------------------------------------------------------
# Use mapedit to draw points for 20 treated and 20 control sites
# Normally these are polygons, so change accordingly
treat <- drawFeatures() |> 
  mutate(label = c(rep("treated", 20), rep("control", 20))) |> 
  select(label) |> 
  sf_as_ee()   # Convert drawn features to Earth Engine objects

trt <- treat$filter(ee$Filter$eq("label", "treated"))
cont <- treat$filter(ee$Filter$eq("label", "control"))

# Visualize sites on the map
Map$addLayer(treat)
Map$centerObject(treat, zoom = 12)
Map$addLayer(trt$style(color = "magenta", width = 8)) +
Map$addLayer(cont$style(color = "yellow", width = 8))

# ---------------------------------------------------------------------------
# 2. Load Sentinel-2 imagery and cloud scores
# ---------------------------------------------------------------------------
s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")   # Sentinel-2 surface reflectance
cs <- ee$ImageCollection$Dataset$GOOGLE_CLOUD_SCORE_PLUS_V1_S2_HARMONIZED  # Cloud score+ dataset

# ---------------------------------------------------------------------------
# 3. Compute NDVI and mask clouds
# ---------------------------------------------------------------------------
ndvi_ic <- s2$
  filterBounds(treat$bounds()$buffer(200))$              # Keep images near conservation sites
  filterDate("2018-01-01","2024-12-31")$                # Restrict to 2018-2024
  linkCollection(cs, "cs")$                              # Attach cloud score band
  map(function(img) {
    qa <- img$select("cs")                               # Select cloud score band
    img$updateMask(qa$gt(0.6))$                         # Mask cloudy pixels (score <= 0.6)
      normalizedDifference(c("B8","B4"))$rename("NDVI")$ # Compute NDVI
      copyProperties(img, ee$List(list("system:time_start"))) # Preserve timestamp
  })

# ---------------------------------------------------------------------------
# 4. Aggregate NDVI to annual median per site
# ---------------------------------------------------------------------------
years <- ee$List$sequence(2018, 2024)   # Years of interest

annual_imgs <- years$map(
  ee_utils_pyfunc(function(y) {
    y <- ee$Number(y)
    img <- ndvi_ic$
      filter(ee$Filter$calendarRange(y, y, "year"))$  # Filter images by year
      median()                                        # Compute annual median NDVI
    band_name <- ee$String("ndvi_")$cat(y$format("%d")) # Rename band as ndvi_YEAR
    img$select("NDVI")$rename(band_name)$set("year", y)
  })
)

# Convert annual images to a single multi-band image
ndvi_img <- ee$ImageCollection$fromImages(annual_imgs)$toBands()

# Clean band names by removing numeric prefixes
old_names <- ndvi_img$bandNames()
clean_names <- old_names$map(ee_utils_pyfunc(function(n) {
  ee$String(n)$replace("[0-9]+_", "")
}))
ndvi_img <- ndvi_img$rename(clean_names)

# ---------------------------------------------------------------------------
# 5. Extract mean NDVI per site
# ---------------------------------------------------------------------------
stats <- ndvi_img$reduceRegions(
  collection = treat,
  reducer = ee$Reducer$mean(),   # Compute mean NDVI within each polygon
  scale = 10                     # 10m resolution
)

# Convert results to R dataframe and reshape for plotting
ndvi_df <- ee_as_sf(stats) |> 
  st_drop_geometry() |> 
  pivot_longer(cols = starts_with("ndvi"), 
               names_to = "year_metric", 
               values_to = "NDVI") |>
  separate(year_metric, 
           into = c("metric", "year"), 
           sep = "_") |> 
  mutate(year = as.integer(year))

# ---------------------------------------------------------------------------
# 6. Plot NDVI time series for treated vs control
# ---------------------------------------------------------------------------
ggplot(ndvi_df %>% 
         group_by(label, year) %>% 
         summarise(NDVI = mean(NDVI)), 
       aes(x = year, y = NDVI, color = label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = c(
    "treated" = "magenta",
    "control" = "yellow"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    text = element_text(colour = "white", size = 24, face = 'bold'),
    axis.text = element_text(colour = "white", size = 22, face = 'bold')
  ) +
  labs(x = "Year", 
       y = "NDVI", 
       color = "Treatment")

ggsave("ndvi_plot.png", 
       bg = "transparent", 
       width = 8, height = 5, 
       dpi = 300)

# Always cite data sources and packages you use in your work accordingly, :)

# ---------------------------------------------------------------------------
# End of Script
# ---------------------------------------------------------------------------
