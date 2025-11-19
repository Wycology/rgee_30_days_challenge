###############################################################
# Script: 19_buildings_data.R
#
# Purpose:
#   Demonstrate how to interactively identify and visualize 
#   Open Buildings tiles in Google Earth Engine (GEE) using rgee, including:
#     - Loading the global Open Buildings grid
#     - Selecting the tile that contains a user-defined point
#     - Retrieving the tile's quadkey ID
#     - Loading and visualizing building footprints for that tile
#     - Digitizing a region of interest (ROI) interactively
#
# Workflow:
#   1. Initialize Earth Engine and load the Open Buildings grid
#   2. Define a point in Kenya (Kericho region)
#   3. Identify which grid tile the point falls into
#   4. Load building footprints for that tile using its quadkey
#   5. Digitize an ROI with mapedit and visualize the buildings within it
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology
# Date: 2025-11-19
###############################################################

# ---------------------------------------------------------------------------
# Load required libraries
# ---------------------------------------------------------------------------
library(rgee)       # Version 1.1.8.9000 Earth Engine API for R
library(mapedit)    # Version 0.7.0 Interactive spatial editing tools
ee_Initialize()     # Authenticate & initialize Earth Engine

# ---------------------------------------------------------------------------
# 1. Load the global Open Buildings grid
# ---------------------------------------------------------------------------
# The grid contains all tile definitions (indexed by quadkey)
grid <- ee$FeatureCollection(
  'projects/sat-io/open-datasets/OPEN-BUILDING-MAPS/open_buildings_grid'
)

# ---------------------------------------------------------------------------
# 2. Define a reference point in Kenya (Kericho region)
# ---------------------------------------------------------------------------
pt <- ee$Geometry$Point(35.35, -0.36)   # Lon, Lat
Map$addLayer(pt, list(color = 'red'), 'Reference Point')

# ---------------------------------------------------------------------------
# 3. Identify which Open Buildings tile contains the point
# ---------------------------------------------------------------------------
tile <- grid$filterBounds(pt)$first()  # filter grid to tiles intersecting pt
tile_id <- tile$get('quadkey')         # retrieve quadkey for matching tile
print(tile_id$getInfo())               # display quadkey

# ---------------------------------------------------------------------------
# 4. Load building footprints for the selected tile
# ---------------------------------------------------------------------------

buildings <- ee$FeatureCollection(
  'projects/sat-io/open-datasets/OPEN-BUILDING-MAPS/tiles/building_300110'
)

# ---------------------------------------------------------------------------
# 5. Digitize a region of interest (ROI) interactively
# ---------------------------------------------------------------------------
roi <- drawFeatures() |> sf_as_ee()

# Center and visualize buildings within the region of interest
Map$centerObject(roi, 14)

Map$addLayer(
  buildings$filterBounds(roi)$style(color = 'red', 
                                    fillColor = "#ffffff00"),
  list(),
  'Buildings within ROI'
) +
  Map$addLayer(roi$style(color = 'blue', 
                         fillColor = "#ffffff00"))

# Cite data sources and packages you use accordingly :)

# ---------------------------------------------------------------------------
# End of Script
# ---------------------------------------------------------------------------
