#######################################################################
# Script: filter_feature_collections.R
# Purpose: Demonstrate how to filter Google Earth Engine 
#          FeatureCollections in R (rgee), including:
#             • Spatial filtering (filterBounds)
#             • Attribute filtering (eq, neq, inList)
#             • Numeric filtering (rangeContains)
#             • String filtering (contains, starts/ends with)
#
# Workflow:
#   1. Initialize rgee
#   2. Draw a boundary to subset the world map to Africa
#   3. Filter by attribute equality (e.g., ADM0_NAME == "Kenya")
#   4. Filter by membership (multiple countries using inList)
#   5. Exclude a country (ADM0_NAME != "Uganda")
#   6. Filter by numeric range (Shape_Area limits)
#   7. String-based filters (contains, starts with, ends with)
#   8. Visualize results on the interactive map
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology
# (Like, Share, Subscribe, Comment)
# Date: 2025-11-09
#######################################################################

# ---------------------------------------------------------------------
# Load libraries
# ---------------------------------------------------------------------
library(rgee)     # Version 1.1.8
library(mapedit)  # Version 0.7.0

# ---------------------------------------------------------------------
# Initialize Earth Engine
# ---------------------------------------------------------------------
ee_Initialize()

# ---------------------------------------------------------------------
# 1. Load global countries and filter by drawn region
# ---------------------------------------------------------------------
world  <- ee$FeatureCollection("FAO/GAUL/2015/level0")

# Draw a polygon interactively → convert to EE object
bounds <- drawFeatures() |> sf_as_ee()

# Filter world countries to only those intersecting your drawn region
africa <- world$filterBounds(bounds)

Map$centerObject(africa, zoom = 2)
Map$addLayer(africa, name = "Africa (filtered by bounds)")

# ---------------------------------------------------------------------
# 2. Filter by attribute equality: ADM0_NAME == "Kenya"
# ---------------------------------------------------------------------
filtered <- africa$filter(
  ee$Filter$eq("ADM0_NAME", "Kenya")
)

Map$centerObject(filtered, zoom = 6)
Map$addLayer(filtered, name = "Kenya")

# ---------------------------------------------------------------------
# 3. Filter by membership: multiple countries (inList)
# ---------------------------------------------------------------------
filtered <- africa$filter(
  ee$Filter$inList(
    "ADM0_NAME",
    list("Kenya", "Somalia", "Morocco", "Lesotho")
  )
)

Map$centerObject(filtered, zoom = 2)
Map$addLayer(filtered, name = "Selected Countries")

# ---------------------------------------------------------------------
# 4. Exclude a country: ADM0_NAME != "Uganda"
# ---------------------------------------------------------------------
filtered <- africa$filter(
  ee$Filter$neq("ADM0_NAME", "Uganda")
)

Map$centerObject(filtered, zoom = 1)
Map$addLayer(filtered, name = "Africa Without Uganda")

# ---------------------------------------------------------------------
# 5. Numeric filtering: Shape_Area between two limits
#     NOTE: Shape_Area is in square degrees (°²)
# ---------------------------------------------------------------------
filtered <- africa$filter(
  ee$Filter$rangeContains(
    "Shape_Area",
    100,   # lower bound (squared degrees)
    212    # upper bound (squared degrees)
  )
)

Map$centerObject(filtered, zoom = 2)
Map$addLayer(filtered, name = "Area Filter: 100–212 deg²")

# ---------------------------------------------------------------------
# 6. String filter: NAME contains substring ("ia")
# ---------------------------------------------------------------------
filtered <- africa$filter(
  ee$Filter$stringContains(
    "ADM0_NAME", 
    "ia")
)

Map$centerObject(filtered, zoom = 2)
Map$addLayer(filtered, name = "Countries Containing 'ia'")

# ---------------------------------------------------------------------
# 7. String filter: NAME ends with ("go")
# ---------------------------------------------------------------------
filtered <- africa$filter(
  ee$Filter$stringEndsWith("ADM0_NAME", "go")
)

Map$centerObject(filtered, zoom = 2)
Map$addLayer(filtered, name = "Ends With 'go'")

# ---------------------------------------------------------------------
# 8. String filter: NAME starts with ("E")
# ---------------------------------------------------------------------
filtered <- africa$filter(
  ee$Filter$stringStartsWith("ADM0_NAME", "E")
)

Map$centerObject(filtered, zoom = 2)
Map$addLayer(filtered, name = "Starts With 'E'")

# Cite all data sources and packages you use accordingly :)

#######################################################################
# End of script
#######################################################################
