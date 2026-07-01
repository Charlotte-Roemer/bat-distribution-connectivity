
library(tidyverse)
library(terra)

# Load transition layer
List_Transition = list.files("/home/charlotte/Bureau/SDM/French_neighbours/Connectivity/VC90_all_acticlass_None_season", pattern = ".tif$")
List_List_Transition_raster <- lapply(List_Transition, function(x) raster(x))

# Load raster offshore
List_offshore = list.files("/home/charlotte/Bureau/SDM/French_neighbours/Offshore", pattern = ".tif$")
List_offshore_raster <- lapply(List_offshore, function(x) raster(x))

# For each species merge



