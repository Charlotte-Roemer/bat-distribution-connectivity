library(terra)

# set location of your downloaded GRIP data :
loc <- "/home/tsevere/Documents/mnhn/data/routes/"
raster_tp1_name <- "grip4_tp1_dens_m_km2.asc"
raster_total_name <- "grip4_total_dens_m_km2.asc"

# get ROI file

roi_file <- "/home/tsevere/Documents/mnhn/data/regions.gpkg"

roi <- terra::vect(
  roi_file,
  layer = "europe"
)

raster_total <- rast(file.path(loc, raster_total_name))
raster_tp1 <- rast(file.path(loc, raster_tp1_name))


cropped_tp1 <- crop(raster_tp1, ext(roi))
cropped_tp1 <- classify(cropped_tp1, cbind(NA, 0))

cropped_total <- crop(raster_total, ext(roi))
cropped_total <- classify(cropped_total, cbind(NA, 0))

cropped_autres <- cropped_total - cropped_tp1


writeRaster(cropped_tp1, filename = file.path(loc, "grip4_tp1_dens_m_km2_sea_filled.tif"))
writeRaster(cropped_autres, filename = file.path(loc, "grip4_tpothers_dens_m_km2_sea_filled.tif"))
