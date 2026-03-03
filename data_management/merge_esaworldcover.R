library(terra)

path <- commandArgs(trailingOnly = TRUE)[1]

tif_folder <- "raw_tif"
tif_folder <- file.path(path, tif_folder)
tif_list <- list.files(tif_folder, pattern = ".tif", full.names = TRUE)

tiles <- lapply(tif_list, terra::rast)

tiles <- terra::sprc(tiles)
mosaic <- terra::mosaic(tiles)

# Next two lines set NA values (water farther than 30 km from the coasts)
# to 80 (full water bodies)
mosaic <- terra::classify(mosaic, cbind(NA, 80L))
terra::writeRaster(mosaic, "ESA_WorldCover_10m_2021_v200_waterfilled.tif")
# terra::writeRaster(mosaic, "ESA_WorldCover_10m_2021_v200.tif")
