###############################################################
# Script: monthly_wvp_rgee.R
# Purpose: Interactive workflow to:
#   ✓ Draw a field polygon on a map using mapedit
#   ✓ Load and mask Sentinel-2 data with Cloud Score+
#   ✓ Extract Water Vapor Pressure (WVP) for one year
#   ✓ Build monthly median WVP composites
#   ✓ Extract monthly WVP per polygon
#   ✓ Visualize WVP time-series using ggplot2
#   ✓ Visualize WVP interactively on map
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# (Like, share, subscribe, comment)
# Date: 2025-11-04
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
library(rgee)      # Google Earth Engine interface for R
library(mapedit)   # Interactive drawing of polygons
library(dplyr)     # Data manipulation
library(tidyr)     # Data reshaping
library(ggplot2)   # Visualization

# Initialize Earth Engine ---------------------------------------------------
ee_Initialize(drive = TRUE)

# ---------------------------------------------------------------------------
# 1. Draw field interactively
# ---------------------------------------------------------------------------
# The user draws ONE field polygon using a pop-up interactive map.
# drawFeatures() returns an sf object containing the drawn geometry.

site <- drawFeatures() |> 
  mutate(name = "Field_1") |>       # Give the field a name
  select(name)                      # Keep only the name column

# Convert sf polygon → Earth Engine FeatureCollection
site_ee <- sf_as_ee(site)

# ---------------------------------------------------------------------------
# 2. Load Sentinel-2 and mask clouds using Cloud Score+
# ---------------------------------------------------------------------------

# Cloud Score+ collection
cs_plus <- ee$ImageCollection$Dataset$GOOGLE_CLOUD_SCORE_PLUS_V1_S2_HARMONIZED

# Sentinel-2 Surface Reflectance Harmonized collection
s2 <- ee$ImageCollection$Dataset$COPERNICUS_S2_SR_HARMONIZED

# One-year analysis range
start_date <- "2024-01-01"
end_date   <- "2025-01-01"

# Filter Sentinel-2 by location and date
# Attach Cloud Score+ to each image
# Mask clouds using CS+ > 0.6
# Keep only the Water Vapor Pressure band (WVP)
s2 <- s2$
  filterBounds(site_ee)$
  filterDate(start_date, end_date)$
  linkCollection(cs_plus, "cs")$
  map(function(img) {
    qa <- img$select("cs")           # Cloud Score+ probability image
    img <- img$updateMask(qa$gt(0.6))# Mask where cloud score < 0.6
    img$copyProperties(              # Keep original timestamp
      img, ee$List(list("system:time_start"))
    )
  })$
  select("WVP")

# ---------------------------------------------------------------------------
# 3. Build monthly median WVP composites
# ---------------------------------------------------------------------------

# Month numbers 1–12
months <- ee$List$sequence(1, 12)

# For each month:
#   - Filter images belonging to that month
#   - Compute monthly median WVP
#   - Convert WVP from m to cm (× 0.001)
#   - Rename band to wvp_1 ... wvp_12
monthly_wvp <- months$map(
  ee_utils_pyfunc(function(m) {
    m <- ee$Number(m)
    img <- s2$
      filter(ee$Filter$calendarRange(m, m, "month"))$
      median()$
      multiply(0.001)               # Convert WVP to cm
    
    band_name <- ee$String("wvp_")$cat(m$format("%d"))
    img$select("WVP")$
      rename(band_name)$
      set("month", m)
  })
)

# Convert the list of monthly images → ImageCollection
monthly_ic <- ee$ImageCollection$fromImages(monthly_wvp)

# Convert ImageCollection → one multi-band EE image
monthly_bands <- monthly_ic$toBands()

# ---------------------------------------------------------------------------
# 4. Extract monthly WVP per polygon
# ---------------------------------------------------------------------------

# Reduce each band inside the polygon
# WVP dataset is often smooth → min() is used here, but mean() also works.
stat <- monthly_bands$reduceRegions(
  reducer = ee$Reducer$min(),
  collection = site_ee,
  scale = 10
)

# Convert result to sf → R data frame
wvp_sf <- ee_as_sf(stat)
wvp_df <- wvp_sf |> sf::st_drop_geometry()

# ---------------------------------------------------------------------------
# 5. Prepare data for ggplot2 visualization
# ---------------------------------------------------------------------------

# Convert band_xxx columns into long format for plotting
df_long <- wvp_df %>%
  pivot_longer(
    cols = starts_with("X"),       # rgee often prefixes band names with X
    names_to = "month",
    values_to = "wvp"
  ) %>%
  mutate(
    month = as.integer(sub(".*_wvp_(\\d+)", "\\1", month)), # Extract month number
    month_label = factor(month.abb[month], levels = month.abb)
  )

# ---------------------------------------------------------------------------
# 6. Plot WVP time series
# ---------------------------------------------------------------------------

ggplot(df_long, aes(x = month_label, y = wvp, group = name, color = name)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Monthly Water Vapor Pressure (WVP) for Field_1",
    x = "Month",
    y = "WVP (cm)",
    color = "Field"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ---------------------------------------------------------------------------
# 7. Add map layers for example months
# ---------------------------------------------------------------------------

# Center the map on the drawn field
Map$centerObject(site_ee, zoom = 17)

# Visualize two sample months on the interactive GEE map
# Bands "2_wvp_3" and "3_wvp_4" correspond to internal naming after toBands()
Map$addLayer(
  monthly_bands$select("2_wvp_3")$clip(site_ee),
  visParams = list(min = 1, max = 3, palette = c("brown", "yellow", "darkgreen"))
) |
  Map$addLayer(
    monthly_bands$select("3_wvp_4")$clip(site_ee),
    visParams = list(min = 1, max = 3, palette = c("brown", "yellow", "darkgreen"))
  )

# ---------------------------------------------------------------------------
# END OF SCRIPT
# ---------------------------------------------------------------------------
