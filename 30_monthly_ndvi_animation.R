###############################################################
# Script: 30_monthly_ndvi_animation.R
#
# Purpose:
#   - Define a point of interest
#   - Create a 2 km roi buffer
#   - Load Sentinel-2 SR Harmonized + CloudScore+ (CS+) dataset
#   - Apply cloud masking using CloudScore+ (threshold 0.6)
#   - Compute monthly NDVI median composites (2020–2025)
#   - Generate forward + reverse sequence for smooth animation
#   - Render GIF frames using rgeeExtra
#   - Annotate GIF with title, timestamps, credits
#   - Save final animation as .gif
#
# Author: Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology
# (Like, Share, Subscribe, Comment)
# Date: 2025-11-30
###############################################################

# ---------------------------------------------------------------------------
# 1. Load libraries
# ---------------------------------------------------------------------------
library(rgee)        # Version 1.1.8.9000 Google Earth Engine for R
library(rgeeExtra)   # Version 0.1.0 GIF creation, annotations, helpers

extra_Initialize()
ee_Initialize()

# ---------------------------------------------------------------------------
# 2. Define date range & point of interest
# ---------------------------------------------------------------------------
start_date <- "2020-01-01"
end_date   <- "2025-01-01"

# Example point: Kericho, Kenya (replace coordinates if needed)
pt <- ee$Geometry$Point(35.28653, -0.46916)

# Create 2 km buffer roi, adjust as needed or use your own region
roi <- pt$buffer(2000)

# Visualize roi
Map$centerObject(eeObject = roi, zoom = 14)
Map$addLayer(
  ee$FeatureCollection(roi)$style(
    color = "red",
    fillColor = "#ffffff00",
    width = 4
  )
)

# ---------------------------------------------------------------------------
# 3. Load Sentinel-2 SR & CloudScore+ harmonized dataset
# ---------------------------------------------------------------------------
s2       <- ee$ImageCollection$Dataset$COPERNICUS_S2_SR_HARMONIZED
cs_plus  <- ee$ImageCollection$Dataset$GOOGLE_CLOUD_SCORE_PLUS_V1_S2_HARMONIZED

# ---------------------------------------------------------------------------
# 4. Apply cloud masking + compute NDVI
# ---------------------------------------------------------------------------
ndvi <- s2$
  filterBounds(roi)$
  filterDate(start_date, end_date)$
  linkCollection(cs_plus, "cs")$        # Attach CS+ scores
  map(function(img) {
    
    # CloudScore+ masking
    qa <- img$select("cs")
    img <- img$updateMask(qa$gt(0.6))    # Keep pixels with quality > 0.6
    
    # NDVI = (B8 - B4) / (B8 + B4)
    ndvi <- img$normalizedDifference(c("B8", "B4"))$rename("ndvi")
    
    # Carry over timestamp
    ndvi$copyProperties(img, ee$List(list("system:time_start")))
  })$
  select("ndvi")

# ---------------------------------------------------------------------------
# 5. Create sequence of months between start and end dates
# ---------------------------------------------------------------------------
months <- ee$List$sequence(
  0,
  ee$Date(end_date)$difference(ee$Date(start_date), "month")$subtract(1)
)

# ---------------------------------------------------------------------------
# 6. Build monthly median NDVI composites
# ---------------------------------------------------------------------------
monthly_ic <- ee$ImageCollection(
  months$map(
    ee_utils_pyfunc(function(m) {
      
      start <- ee$Date(start_date)$advance(m, "month")
      end   <- start$advance(1, "month")
      
      composite <- ndvi$
        filterDate(start, end)$
        median()$
        clip(roi)$
        set("label", start$format("YYYY-MM"))
      
      # Visualization for animation frames
      composite$
        select("ndvi")$
        visualize(
          min = 0.1, max = 0.8,
          palette = c("red", "yellow", "#006400")
        )$
        set("label", composite$get("label"))
    })
  )
)

# ---------------------------------------------------------------------------
# 7. Create smooth forward + reverse animation sequence
# ---------------------------------------------------------------------------
appendReverse <- function(col) col$merge(col$sort("label", FALSE))
monthly_anim <- appendReverse(monthly_ic)

# ---------------------------------------------------------------------------
# 8. Generate GIF using rgeeExtra
# ---------------------------------------------------------------------------
videoArgs <- list(
  dimensions = 450,
  region = roi,
  framesPerSecond = 5
)

animation <- ee_utils_gif_creator(
  monthly_anim,
  videoArgs,
  mode = "wb"
)

# Extract label timestamps
labels <- monthly_anim$aggregate_array("label")$getInfo()

# ---------------------------------------------------------------------------
# 9. Annotate GIF (title, dates, credits, watermark)
# ---------------------------------------------------------------------------
animation_annotated <- animation %>%
  ee_utils_gif_annotate(
    "Field_A",
    size = 24, weight = 700, color = 'red'
  ) %>%
  ee_utils_gif_annotate(
    labels,
    size = 23, weight = 700,
    location = "+00+30",
    boxcolor = "#FFFFFF"
  ) %>%
  ee_utils_gif_annotate(
    "magick + rgee",
    size = 16,
    boxcolor = 'white',
    location = "+340+00"
  ) %>%
  ee_utils_gif_annotate(
    "@Wycology",
    size = 24,
    color = 'magenta',
    weight = 700,
    location = "+00+420"
  ) %>%
  ee_utils_gif_annotate(
    "rgeeExtra",
    size = 24,
    color = 'yellow',
    weight = 700,
    location = "+330+420"
  )

animation_annotated

# ---------------------------------------------------------------------------
# 10. Save GIF
# ---------------------------------------------------------------------------
gc(reset = TRUE)

ee_utils_gif_save(
  animation_annotated,
  path = "gg.gif"
)

# Cite data and packages you use accordingly :)

# ---------------------------------------------------------------------------
# End of Script
# ---------------------------------------------------------------------------

