# Script to calculate distance from coast using line vector (coast) and a
# raster(points for which to calculate distance)

library(terra)
library(sf)

sf::sf_use_s2(FALSE)
coastlines_file <- "/home/tsevere/Documents/mnhn/projet_git_BMRE/data/GIS/coastlines.gpkg"
coastlines <- st_read(coastlines_file, layer = "lines")
coastlines <- st_transform(coastlines, 2154)

points_raster_file <- "/home/tsevere/Documents/mnhn/projet_git_BMRE/data/GIS/europe_1000m_L93.tif"

points_raster <- rast(points_raster_file)

d <- distance(points_raster, coastlines, rasterize = TRUE)

writeRaster(d, "/home/tsevere/Documents/mnhn/projet_git_BMRE/data/GIS/europe_1000m_coastline_dist.tif")
