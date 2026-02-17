library(terra)

folder <- commandArgs(trailingOnly = TRUE)[1]
to_join <- file.path(folder, "data", "dem")

raster_name <- file.path(folder, "data", "DEM1_SAR_DTE_30_europe_L93.tif")

files <- list.files(to_join,
  pattern = ".dt2$",
  full.names = TRUE,
  recursive = TRUE
)


rasters <- lapply(files, terra::rast)
sprc <- terra::sprc(rasters)
mosaique <- terra::merge(sprc)

mosaique[is.na(mosaique)] <- 0
mosaique <- project(mosaique, "EPSG:2154")

writeRaster(mosaique, raster_name, overwrite = TRUE)

print("done")
