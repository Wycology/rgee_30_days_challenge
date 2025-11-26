###############################################################
# Script: 27_tea_field_soc.R
# Purpose: Extract Soil Organic Carbon (SOC) values for multiple 
#          tea field sites from the OpenLandMap dataset and 
#          visualize SOC by depth.
#
# Workflow:
#   1. Draw multiple experimental tea field sites interactively
#   2. Load OpenLandMap SOC dataset
#   3. Reduce SOC image over sites to compute mean SOC per site
#   4. Convert results to an R data frame and reshape by soil depth
#   5. Adjust units and order, then create a bar plot of SOC
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# Date: 2025-11-27
###############################################################

# ---------------------------------------------------------------------------
# Load libraries
# ---------------------------------------------------------------------------
library(rgee)     # Interface to Google Earth Engine
library(mapedit)  # For interactive polygon drawing
library(tidyverse) # For data manipulation and plotting
library(sf)        # For handling spatial data

# ---------------------------------------------------------------------------
# Initialize Google Earth Engine
# ---------------------------------------------------------------------------
ee_Initialize()

# ---------------------------------------------------------------------------
# 1. Draw multiple experimental tea field sites
# ---------------------------------------------------------------------------
sites <- drawFeatures() |> 
  mutate(site_id = c("North", "South", "East", "West")) |>
  select(site_id) |> 
  sf_as_ee()

# ---------------------------------------------------------------------------
# 2. Load Soil Organic Carbon (SOC) dataset from OpenLandMap
# ---------------------------------------------------------------------------
soc <- ee$Image("OpenLandMap/SOL/SOL_ORGANIC-CARBON_USDA-6A1C_M/v02")

Map$addLayer(eeObject = soc,
             visParams = list(
               bands = "b0",
               min = 0.0,
               max = 10.0,
               palette = c('#ffffa0','#f7fcb9','#d9f0a3',
                           '#addd8e','#78c679','#41ab5d',
                           '#238443','#005b29','#004b29',
                           '#012b13','#00120b')
             )) |
  Map$addLayer(eeObject = soc,
               visParams = list(
                 bands = "b200",
                 min = 0.0,
                 max = 10.0,
                 palette = c('#ffffa0','#f7fcb9','#d9f0a3',
                             '#addd8e','#78c679','#41ab5d',
                             '#238443','#005b29','#004b29',
                             '#012b13','#00120b')
               )) 
# ---------------------------------------------------------------------------
# 3. Extract mean SOC values for each site
# ---------------------------------------------------------------------------
soc_data <- soc$reduceRegions(
  collection = sites,
  reducer = ee$Reducer$mean(),  # Compute mean SOC per polygon
  scale = 250                   # Spatial resolution in meters
)

# ---------------------------------------------------------------------------
# 4. Convert results to dataframe and reshape
# ---------------------------------------------------------------------------
soc_sf <- ee_as_sf(soc_data)      # Convert to sf object
soc_df <- soc_sf %>% st_drop_geometry()  # Drop geometry for tidy data

# Reshape and clean SOC data
df <- soc_df %>%
  pivot_longer(
    cols = starts_with("b"),     # SOC bands correspond to depths
    names_to = "depth",
    values_to = "soc"
  ) %>%
  mutate(
    depth = case_when(
      depth == "b0" ~ "0cm",
      depth == "b10" ~ "10cm", 
      depth == "b30" ~ "30cm",
      depth == "b60" ~ "60cm",
      depth == "b100" ~ "100cm",
      depth == "b200" ~ "200cm"
    ),
    soc = soc / 5  # Convert to correct units (g/kg)
  ) %>%
  arrange(site_id, depth)

# ---------------------------------------------------------------------------
# 5. Visualize SOC by site and depth
# ---------------------------------------------------------------------------
ggplot(df, aes(x = site_id, y = soc, fill = depth)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Soil Organic Carbon in Tea Fields",
    y = "SOC (g/kg)",
    x = "Site"
  ) +
  theme_minimal()

# Cite all data sets and packages you use accordingly :)

# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------

