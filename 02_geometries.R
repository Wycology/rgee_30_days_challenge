###############################################################
# Script: geometries_rgee.R
# Purpose: Demonstrate creation, visualization, and styling of
#          geometries in Google Earth Engine (GEE) using rgee,
#          draw features interactively with mapedit, import 
#          vector data, and explore the FAO GAUL 2015 dataset.
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology (Like, share, subscribe, comment)
# Date: 2025-11-02
###############################################################

# In case you are running rgee for the first time, consider these:
# install.packages("rgee")
# library(rgee)
# ee_clean_pyenv()
# ee_install(py_env = "rgee_env")
# ee_install_upgrade(earthengine_env = "rgee_env")
# ee_install_set_pyenv(py_path = NULL, py_env = "rgee_env")
# ee_Authenticate(user = "your username", earthengine = TRUE, drive = TRUE)
# If using Windows, you may need to restart RStudio several times

# Load required libraries ----------------------------------------------------
library(rgee)      # R interface to Google Earth Engine
library(mapedit)   # For interactive feature drawing
library(leaflet)   # Interactive mapping
library(sf)        # For handling spatial vector data

# Initialize Earth Engine ----------------------------------------------------
# drive = TRUE allows export and access via Google Drive
ee_Initialize(drive = TRUE)

# ---------------------------------------------------------------------------
# 1. Set map center and zoom level
# ---------------------------------------------------------------------------
Map$setCenter(
  lon = 35.1330,   # Longitude
  lat = -0.4468,   # Latitude
  zoom = 15        # Zoom level (1=World, 20=Street-level)
)

# ---------------------------------------------------------------------------
# 2. Create geometries (Point, Line, Rectangle, Polygon)
# ---------------------------------------------------------------------------

# Point geometry -------------------------------------------------------------
point <- ee$Geometry$Point(35.1330, -0.4468)

# Line geometry --------------------------------------------------------------
line <- ee$Geometry$LineString(
  c(35.133, -0.4468, 
    35.135, -0.4368,
    35.136, -0.445)
)

# Rectangle geometry ---------------------------------------------------------
rect <- ee$Geometry$Rectangle(
  c(35.13, -0.45, 35.14, -0.435)
)

# Polygon geometry -----------------------------------------------------------
poly <- ee$Geometry$Polygon(
  c(
    35.12709, -0.45349,
    35.12159, -0.43976,
    35.12709, -0.42827,
    35.14177, -0.42827,
    35.14599, -0.43976,
    35.14083, -0.45349,
    35.12709, -0.45349
  )
)

# ---------------------------------------------------------------------------
# 3. Style and visualize geometries
# ---------------------------------------------------------------------------

# Style rectangle (pink outline, transparent fill)
rect_styled <- ee$FeatureCollection(rect)$style(
  color = "#ff00ff",        # Magenta border
  fillColor = "#ffffff00",  # Transparent fill
  width = 4
)

# Style polygon (yellow outline, transparent fill)
poly_styled <- ee$FeatureCollection(poly)$style(
  color = "#ffff00",
  fillColor = "#ffffff00",
  width = 4
)

# Add layers to Earth Engine map --------------------------------------------
Map$addLayer(point, visParams = list(color = "red", width = 16)) +
  Map$addLayer(line, visParams = list(color = "blue")) +
  Map$addLayer(rect_styled) +
  Map$addLayer(poly_styled)

# ---------------------------------------------------------------------------
# 4. Draw geometries interactively with mapedit
# ---------------------------------------------------------------------------

# Create base leaflet map using Esri World Imagery
m <- leaflet() %>% addProviderTiles(providers$Esri.WorldImagery)

# Draw geometries interactively on map
m_point  <- drawFeatures() |> sf_as_ee()       # Point
m_line   <- drawFeatures() |> sf_as_ee()       # Line
m_rect   <- drawFeatures() |> sf_as_ee()       # Rectangle
m_circle <- drawFeatures(map = m) |> sf_as_ee()# Circle
m_poly   <- drawFeatures(map = m) |> sf_as_ee()# Polygon

# Add drawn geometries to Earth Engine map ----------------------------------
Map$addLayer(m_poly) +
  Map$addLayer(m_rect) +
  Map$addLayer(m_circle) +
  Map$addLayer(m_line) +
  Map$addLayer(m_point)

# ---------------------------------------------------------------------------
# 5. Import and display local vector data
# ---------------------------------------------------------------------------

# Replace "path to your file" with the actual path to your shapefile or GeoPackage
# Example: "data/my_boundary.shp" or "data/buffer_area.gpkg"
my_sf <- st_read("path to your file") |> sf_as_ee()

# Add vector layer to map
Map$addLayer(my_sf)

# ---------------------------------------------------------------------------
# 6. Explore GEE Data Catalog: FAO GAUL 2015 Administrative Boundaries
# ---------------------------------------------------------------------------

# Load Level-1 administrative boundaries
gaul1 <- ee$FeatureCollection$Dataset$FAO_GAUL_2015_level1

# Add all Level-1 boundaries to map
Map$addLayer(eeObject = gaul1)

# Filter example: display Nyanza region
nyanza <- gaul1$filter(ee$Filter$eq("ADM1_NAME", "Nyanza"))
Map$addLayer(nyanza)

# Spatial filter example: find region containing a point in Malawi
pt <- ee$Geometry$Point(37.1061, -12.74735)
malawi_poly <- gaul1$filterBounds(pt)
Map$addLayer(malawi_poly)

# ---------------------------------------------------------------------------
# END OF SCRIPT
# ---------------------------------------------------------------------------
