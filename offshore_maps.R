
# CREATE MAPS OFFSHORE USING EXPERT ASSESSMENT OF BAT ACTIVITY (based on Migratlane data)

library(tidyverse)
library(sf)
library(giscoR)
library(terra)

# Load expert assessment of bat activity
Activity_offshore = read_delim("/sps/mnhn/croemer/data/GIS/Offshore/Offshore_activity.csv")

# Load WORLD GIS limits and crop Europe
sf_use_s2(FALSE)
world_poly <- gisco_get_countries(resolution = 01, epsg = 3035, spatialtype = "RG") %>% 
  st_crop(xmin= 2500000, ymin= 1000000, xmax= 6800000, ymax= 5500000) %>% 
  st_union()

# ggplot(world_poly) +
#   geom_sf() 

# Buffer the coastlines
sf_use_s2(TRUE)
coastline_buffer_n2km = st_buffer(world_poly, dist = -2000) %>% # -2 km in meters
  st_make_valid()
coastline_buffer_1km = st_buffer(world_poly, dist = 1000) %>% # 1 km in meters
  st_make_valid()
coastline_buffer_2km = st_buffer(world_poly, dist = 2000) %>% # 2 km in meters
  st_make_valid() 
coastline_buffer_100km = st_buffer(world_poly, dist = 100000) %>% # 100 km in meters
  st_make_valid() 

# Crop and add bat activity
coastline_buffer_100km_crop = coastline_buffer_100km %>% 
  st_difference(coastline_buffer_2km) %>% 
  st_as_sf() %>% 
  mutate(km_from_coast = 3) %>% 
  left_join(Activity_offshore)
coastline_buffer_2km_crop = coastline_buffer_2km %>% 
  st_difference(coastline_buffer_1km) %>% 
  st_as_sf() %>% 
  mutate(km_from_coast = 2) %>% 
  left_join(Activity_offshore)
coastline_buffer_1km_crop = coastline_buffer_1km %>% 
  st_difference(coastline_buffer_n2km) %>% # need buffer 2 km inland to avoid holes
  st_as_sf() %>% 
  mutate(km_from_coast = 1) %>% 
  left_join(Activity_offshore)

# ggplot() +
#   geom_sf(data = coastline_buffer_1km_crop , fill = "lightblue")

# Join three buffers
Offshore_buffers = bind_rows(coastline_buffer_1km_crop, 
                                  coastline_buffer_2km_crop, 
                                  coastline_buffer_100km_crop) %>% 
  st_crop(xmin= 2500000, ymin= 1000000, xmax= 6800000, ymax= 5500000) %>% 
  st_make_valid() %>% 
  st_collection_extract("POLYGON")

# ggplot(Offshore_buffers) +
#   geom_sf(aes(fill = km_from_coast))

# Save sf
write_sf(Offshore_buffers, "/sps/mnhn/croemer/data/GIS/Offshore/Offshore_buffers.shp")

# Convert to raster and save
template = rast(vect(Offshore_buffers),res=1000)
for (i in 1:3){
  Sp = names(table(Offshore_buffers$Species))[i]
  Offshore_buffers_i = Offshore_buffers %>% 
    filter(Species == Sp)
  Offshore_buffers_raster_i = rasterize(vect(Offshore_buffers_i), template, "Activity_expert")
  writeRaster(Offshore_buffers_raster_i, 
              paste0("/sps/mnhn/croemer/data/GIS/Offshore/", Sp, "_Offshore_buffers.tif"), 
              overwrite=TRUE)
}

