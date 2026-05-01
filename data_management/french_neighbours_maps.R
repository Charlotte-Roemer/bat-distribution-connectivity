# SHAPE OF FRENCH NEIGHBOUR COUNTRIES

library(tidyverse)
library(sf)

# Load French contour
France_area <- st_read(dsn = "/home/charlotte/Bureau/regions.gpkg", layer = "france_met") %>%
  st_transform(3035)

# Load Europe contour
Europe_area <- st_read(dsn = "/home/charlotte/Bureau/regions.gpkg", layer = "europe_large") %>%
  st_transform(3035)

# Buffer around France
sf_use_s2(TRUE)
buffer_200km = st_buffer(France_area, dist = 200000) %>% # 200 km in meters
  st_make_valid()

# Crop Europe in buffer
Neighbours = Europe_area %>% 
  st_intersection(buffer_200km)

ggplot() +
  geom_sf(data=Neighbours) + 
  geom_sf(data=France_area, fill=NA, color="blue")

  # Save
st_write(Neighbours, dsn="/home/charlotte/Bureau/regions.gpkg", layer='french_neighbours', delete_layer=T)


