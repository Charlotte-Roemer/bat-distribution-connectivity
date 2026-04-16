library(tidyverse)
library(sf)
source("variables.R")

# Load GIS limit of Corsica
emprise <- file.path(data_path, "GIS", "regions.gpkg")
GIS_limits <- st_read(dsn = emprise, layer = "corse") %>%
  st_as_sf()

# Load data_train
data_train_path <- file.path(data_path, "observations", "obs_vars", "data_train_france_met.csv")
data_train = read_delim(data_train_path)

# Load data_pred
data_pred_path <- file.path(data_path, "observations", "pred_vars", "data_pred_france_met.csv")
data_pred = read_delim(data_pred_path)

# Crop Corsica
data_train_sf = st_as_sf(data_train, # convert to sf
    coords = c("X", "Y"), crs=4326, remove=FALSE)
data_pred_sf = st_as_sf(data_pred, # convert to sf
    coords = c("X", "Y"), crs=4326, remove=FALSE)
data_train_corsica_sf <- st_intersection(data_train_sf, GIS_limits)
data_pred_corsica_sf <- st_intersection(data_pred_sf, GIS_limits)
data_train_corsica = st_drop_geometry(data_train_corsica_sf)
data_pred_corsica = st_drop_geometry(data_pred_corsica_sf)

# Save
write_csv(data_train_corsica, paste0(data_path, "/observations/obs_vars/data_train_corsica.csv"))
write_csv(data_pred_corsica, paste0(data_path, "/observations/pred_vars/data_pred_corsica.csv"))