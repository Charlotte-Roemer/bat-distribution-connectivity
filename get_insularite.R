library(terra)
source("variables.R")



# insularité


coastlines_file <- file.path(data_path, "GIS", "coastlines.gpkg")
coastlines <- vect(coastlines_file, layer = "eurasie")

france_file <- file.path(data_path, "GIS", "regions.gpkg")
france <- vect(france_file, layer = "france_met_uniques")
france <- project(france, crs(coastlines))

insul_dist <- terra::distance(france, coastlines)

france$insularite <- insul_dist

writeVector(france, france_file, layer = "france_met_insularite")

# nearest <- try(sf::st_nearest_feature(france, coastlines))
#
# insul_dist <- st_distance(france,
#   coastlines[nearest, ],
#   by_element = TRUE
# )

# france$insularite <- insul_dist

# st_write(france, france_file, layer = "france_met_insularite")
