###############################################################
# Script: 28_soil_ph.R
# Purpose: Extract soil pH values for multiple sites 
#          from the ISDASOIL Africa dataset and visualize pH 
#          by depth.
#
# Workflow:
#   1. Draw multiple sites interactively
#   2. Load ISDASOIL soil pH dataset
#   3. Extract mean pH values for two depth layers (0–20 cm, 20–50 cm)
#   4. Convert results to an R data frame and reshape
#   5. Adjust units and order, then create a bar plot of pH
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# Date: 2025-11-28
###############################################################

# ---------------------------------------------------------------------------
# Load libraries
# ---------------------------------------------------------------------------
library(rgee)     # Version 1.1.8.9000 Interface to Google Earth Engine
library(ggplot2)  # Version 4.0.1 For plotting
library(dplyr)    # Version 1.1.4 For data manipulation
library(tidyr)    # Version 1.3.1 For reshaping data
library(sf)       # Version 1.0.22 For handling spatial data
library(mapedit)  # Version 0.7.0 Interactive drawing of polygons
# ---------------------------------------------------------------------------
# Initialize Google Earth Engine
# ---------------------------------------------------------------------------
ee_Initialize()

# ---------------------------------------------------------------------------
# 1. Draw multiple experimental tea field sites
# ---------------------------------------------------------------------------
sites <- drawFeatures() |> 
  mutate(site_id = c("Sugarcane", "Tea", "Rice", "Maize")) |> 
  select(site_id) |> 
  sf_as_ee()

# ---------------------------------------------------------------------------
# 2. Load soil pH data set (ISDASOIL Africa)
# ---------------------------------------------------------------------------
soil_ph <- ee$Image$Dataset$ISDASOIL_Africa_v1_ph
ee_utils_dataset_display(soil_ph)

vis = list(
  min = 35,
  max = 105,
  palette = c(
    "#cc0000", 
    "#ffaa00", 
    "#aaff55",
    "#2bffd5", 
    "#0055ff" 
  )
)

Map$addLayer(eeObject = soil_ph$select("mean_0_20"),
             visParams = vis) |
  Map$addLayer(eeObject = soil_ph$select("mean_20_50"),
               visParams = vis)

# ---------------------------------------------------------------------------
# 3. Extract mean pH values for each site and depth
# ---------------------------------------------------------------------------
ph_data <- soil_ph$select(c("mean_0_20", "mean_20_50"))$
  reduceRegions(
    collection = sites,
    reducer = ee$Reducer$mean(),  # Mean pH per site
    scale = 30                    # Spatial resolution in meters
  )

# ---------------------------------------------------------------------------
# 4. Convert results to R and reshape
# ---------------------------------------------------------------------------
ph_sf <- ee_as_sf(ph_data)
ph_df <- ph_sf %>% st_drop_geometry()

# Reshape and clean pH data
df <- ph_df %>%
  pivot_longer(
    cols = c("mean_0_20", "mean_20_50"),
    names_to = "depth",
    values_to = "ph"
  ) %>%
  mutate(
    ph = ph / 10  # Convert to correct pH units
  ) %>%
  arrange(site_id, depth)

# ---------------------------------------------------------------------------
# 5. Visualize soil pH by site and depth
# ---------------------------------------------------------------------------
ggplot(df, aes(x = site_id, y = ph, fill = depth)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Soil pH in Tea Fields",
    y = "pH in H2O",
    x = "Site"
  ) +
  theme_minimal()

# Cite all data sources and packages used accordingly :)

# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------
