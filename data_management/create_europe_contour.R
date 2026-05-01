
# CREATE CONTOUR OF EUROPE

library(tidyverse)
library(sf)
library(giscoR)

# Load WORLD GIS limits and crop Europe
sf_use_s2(FALSE)
world_poly <- gisco_get_countries(resolution = 01, epsg = 3035, spatialtype = "RG") %>% 
  filter(!NAME_ENGL %in% c("Algeria", "Morocco", "Tunisia", "Iceland")) %>% 
  st_crop(xmin= 2500000, ymin= 1300000, xmax= 6500000, ymax= 5400000) %>% 
  st_union() 

print("europe limits charged")

ggplot() +
  geom_sf(data=world_poly) 

# Save
st_write(world_poly, dsn="/home/charlotte/Bureau/regions.gpkg", layer='europe_large')

