###############################################################
# Script: 18_color_by_attribute.R
#
# Purpose:
#   Demonstrate interactive calculation and visualization of 
#   feature areas using Google Earth Engine (GEE) via rgee, including:
#     - Loading country boundaries
#     - Filtering to Africa region
#     - Calculating country area (km²) and storing as property
#     - Calculating country perimeter (km) and storing as property
#     - Visualizing country areas and perimeters as a choropleth map
#
# Workflow:
#   1. Load global administrative boundaries (USDOS/LSIB)
#   2. Filter the collection to Africa
#   3. Compute the area of each country polygon
#   4. Paint the country areas onto an empty image
#   5. Visualize the painted image with a color palette based on area
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology 
# Date: 2025-11-18
###############################################################

# ---------------------------------------------------------------------------
# Load the required libraries
# ---------------------------------------------------------------------------
library(rgee)        # Version 1.1.8.9000 Earth Engine API for R
ee_Initialize()      # Initialize Earth Engine session

# ---------------------------------------------------------------------------
# 1. Load Global Administrative Boundaries and Filter to Africa
# ---------------------------------------------------------------------------

# Simplified LSIB boundaries
wld <- ee$FeatureCollection("USDOS/LSIB_SIMPLE/2017")

# Filter countries to African region
afr <- wld$filter(ee$Filter$eq("wld_rgn", "Africa"))

# Add Africa layer to map
Map$addLayer(afr)
print(afr$first()$getInfo())  # inspect first feature

# ---------------------------------------------------------------------------
# 2. Compute Area of Each African Country
# ---------------------------------------------------------------------------
afr <- afr$map(function(f) {
  area <- f$geometry()$area()               # calculate area in square meters
  perimeter <- f$geometry()$perimeter()     # calculate perimeter in meters
  f$set("area", area$divide(1e6))           # add area as property im km2
  f$set("perimeter", perimeter$divide(1e3)) # add perimeter as property in km
})

print(afr$first()$getInfo())        # inspect first feature with area & perimeter

# ---------------------------------------------------------------------------
# 3. Paint FeatureCollection onto an Empty Image
# ---------------------------------------------------------------------------
# Create an empty image to paint features
empty <- ee$Image()$byte()

# Define color palette for visualization
palette <- c('green', 'yellow', 'orange', 'red')

# Paint country polygons using 'area' property
africa <- empty$paint(
  featureCollection = afr,
  color = 'area'
)

# Paint country polygons using 'perimeter' property
africa_p <- empty$paint(
  featureCollection = afr,
  color = 'perimeter'
)
# ---------------------------------------------------------------------------
# 4. Visualize the Painted Image
# ---------------------------------------------------------------------------
# Center map on Africa
Map$setCenter(22, 4, 3)

# Add painted image to map with visualization parameters
Map$addLayer(
  eeObject = africa,
  visParams = list(
    min = 7.797876,             # minimum area for palette scaling
    max = 2339448,              # maximum area for palette scaling
    palette = palette
  ),
  name = 'African Countries by Area'
) +
Map$addLayer(
  eeObject = africa_p,
  visParams = list(
    min = 14,             # minimum perimeter for palette scaling
    max = 11060,          # maximum perimeter for palette scaling
    palette = palette
  ),
  name = 'African Countries by Perimeter'
) +
  Map$addLayer(afr$style(color = "white", fillColor = "#ffffff00")) +
  Map$addLayer(afr$style(color = "magenta", fillColor = "#ffffff00")) 

# Cite all data sources and packages you use accordingly in your work :)
# ---------------------------------------------------------------------------
# End of script
# ---------------------------------------------------------------------------
