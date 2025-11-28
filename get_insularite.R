library(sf)
library(terra)
source("variables.R")



# insularité


coastlines_file <- file.path(data_path, "GIS", "coastlines.gpkg")
coastlines <- st_read(coastlines_file, layer = "eurasie")

france_file <- file.path(data_path, "GIS", "regions.gpkg")
france <- st_read(france_file, layer = "france_met_uniques")

st_crs(france)
france <- st_transform(france, st_crs(coastlines))
st_crs(coastlines)


nearest <- try(sf::st_nearest_feature(france, coastlines))

insul_dist <- st_distance(france,
  coastlines[nearest, ],
  by_element = TRUE
)

france$insularite <- insul_dist

st_write(france, france_file, layer = "france_met_insularite")
