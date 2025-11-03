###############################################################
# Script: monthly_ndvi_rgee.R
# Purpose: Interactive workflow to:
#   ✓ Draw experimental plots on a map using mapedit
#   ✓ Load and mask Sentinel-2 data with Cloud Score+
#   ✓ Compute NDVI for one year
#   ✓ Build monthly median composites
#   ✓ Extract monthly NDVI per polygon
#   ✓ Visualize NDVI time-series using ggplot2
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# (Like, share, subscribe, comment)
# Date: 2025-11-03
###############################################################

# ---------------------------------------------------------------------------
# Installation notes (for beginners)
# Uncomment these if using rgee for the first time:
# install.packages("rgee")
# library(rgee)
# ee_clean_pyenv()
# ee_install(py_env = "rgee_env")
# ee_install_set_pyenv(py_env = "rgee_env")
# ee_Authenticate(user = "your username", drive = TRUE, earthengine = TRUE)
# ---------------------------------------------------------------------------

# Load required libraries ----------------------------------------------------
library(rgee)      # Google Earth Engine package for R
library(mapedit)   # Interactive drawing of polygons
library(dplyr)     # Data manipulation
library(tidyr)     # Reshaping data
library(ggplot2)   # Visualization

# Initialize Earth Engine ---------------------------------------------------
ee_Initialize(drive = TRUE)

# ---------------------------------------------------------------------------
# 1. Draw experimental plots interactively
# ---------------------------------------------------------------------------
# Draw FOUR polygons representing experimental plots.
# drawFeatures() returns an sf object → converted to ee$FeatureCollection.

plots <- drawFeatures()   # Draw 4 polygons manually

# Add simple plot labels: A, B, C, D
plts <- plots |> 
  mutate(plot_id = c("A", "B", "C", "D")) |> 
  select(plot_id)

# Convert to Earth Engine
plts_ee <- plts |> sf_as_ee()

# ---------------------------------------------------------------------------
# 2. Visualize drawn polygons on Earth Engine Map
# ---------------------------------------------------------------------------

plts_styled <- ee$FeatureCollection(plts_ee)$style(
  color = "#ffff00",
  fillColor = "#ffffff00",
  width = 4
)

Map$centerObject(plts_ee, zoom = 15)
Map$addLayer(plts_styled)

# ---------------------------------------------------------------------------
# 3. Load Sentinel-2 and apply Cloud Score+ masking
# ---------------------------------------------------------------------------

# Date range
start_date <- "2024-01-01"
end_date   <- "2025-01-01"

# Cloud Score+ data set
cs_plus <- ee$ImageCollection$Dataset$GOOGLE_CLOUD_SCORE_PLUS_V1_S2_HARMONIZED

# Sentinel-2 SR Harmonized
s2 <- ee$ImageCollection$Dataset$COPERNICUS_S2_SR_HARMONIZED$
  filterBounds(plts_ee)$
  filterDate(start_date, end_date)$
  # Attach the Cloud Score+ collection
  linkCollection(cs_plus, "cs")$
  # Mask clouds using CS+ probability > 0.5
  map(function(img) {
    qa <- img$select("cs")
    img$updateMask(qa$gt(0.5))
  })

# Create NDVI calculating function
calc_ndvi <- function(img) {
  ndvi <- img$normalizedDifference(c("B8", "B4"))$rename("NDVI")
  ndvi$copyProperties(img, ee$List(list("system:time_start")))
}

# Apply NDVI function
s2_ndvi <- s2$map(addNDVI)

# ---------------------------------------------------------------------------
# 4. Build monthly median NDVI composites
# ---------------------------------------------------------------------------

months <- ee$List$sequence(1, 12)

monthly_imgs <- months$map(
  ee_utils_pyfunc(function(m) {
    m <- ee$Number(m)
    img <- s2_ndvi$
      filter(ee$Filter$calendarRange(m, m, "month"))$
      median()
    # Rename band to e.g., ndvi_1, ndvi_2, ...
    band_name <- ee$String("ndvi_")$cat(m$format("%d"))
    img$select("NDVI")$rename(band_name)$set("month", m)
  })
)

monthly_ic <- ee$ImageCollection$fromImages(monthly_imgs)

# Convert into a multi-band image to enable region reduction
monthly_bands <- monthly_ic$toBands()

# Clean band names (remove 0_, 1_, 2_ prefixes)
band_names <- monthly_bands$bandNames()
clean_names <- band_names$map(
  ee_utils_pyfunc(function(nm) {
    ee$String(nm)$replace("^[0-9]+_", "")
  })
)

# Rename bands
monthly_bands_clean <- monthly_bands$rename(clean_names)

# ---------------------------------------------------------------------------
# 5. Extract mean monthly NDVI per experimental plot
# ---------------------------------------------------------------------------

monthly_ndvi <- monthly_bands_clean$reduceRegions(
  collection = plts_ee,
  reducer = ee$Reducer$mean(),
  scale = 10
)

# ---------------------------------------------------------------------------
# 6. Convert Earth Engine results to R sf → tibble
# ---------------------------------------------------------------------------

monthly_sf <- ee_as_sf(monthly_ndvi)
monthly_df <- monthly_sf %>% sf::st_drop_geometry()

# ---------------------------------------------------------------------------
# 7. Prepare data for ggplot2 visualization
# ---------------------------------------------------------------------------

df <- monthly_df %>%
  pivot_longer(
    cols = starts_with("ndvi_"),
    names_to = "month",
    values_to = "ndvi"
  ) %>%
  mutate(month = as.integer(sub("ndvi_", "", month))) %>%
  arrange(plot_id, month)

# Plot NDVI time series
ggplot(df, aes(
  x = month,
  y = ndvi,
  color = plot_id,
  group = plot_id
)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    title = "Monthly NDVI Trend for Experimental Plots",
    x = "Month",
    y = "Mean NDVI",
    color = "Plot"
  ) +
  theme_minimal(base_size = 14)
# ---------------------------------------------------------------------------
# END OF SCRIPT
# ---------------------------------------------------------------------------