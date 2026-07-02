
library(tidyverse)
library(terra)
library(gdistance)

# List transition layer
List_Transition = list.files("/home/charlotte/Bureau/SDM/French_neighbours/Connectivity/VC90_all_acticlass_None_season", pattern = ".rds$", full.names = T)

# List raster offshore
List_offshore = list.files("/home/charlotte/Bureau/SDM/French_neighbours/Offshore", pattern = ".tif$", full.names = T)

# For each species merge (but keep transition layer above because offshore maps go a little bit in the lands)
list_species = sub("_Offshore_buffers\\.tif$", "", basename(List_offshore))
for (i in length(list_species)){
  Sp = list_species[i]
  print(Sp)
  
  # Read offshore raster
  offshore = subset(List_offshore, grepl(Sp, List_offshore))
  offshore_raster <- rast(offshore)
  print(crs(offshore_raster))
  
  # Read transition layer
  transition = subset(List_Transition, grepl(Sp, List_Transition))
  transition_obj = readRDS(transition)
  transition_raster <- rast(raster(transition_obj))
  crs(transition_raster) <- "EPSG:2154"
  print(crs(transition_raster))
  
  # Align offshore raster according to transition layer
  offshore_align <- project( 
    offshore_raster,
    transition_raster,
    method = "bilinear"
  )
  
  # Check alignment before fusion : if TRUE it's OK
  compareGeom(transition_raster, offshore_align) 
  
  # Fusion
  r_final <- cover(transition_raster, offshore_align) 
  
  # Plot
  par(mfrow = c(1,3))
  plot(transition_raster)
  plot(offshore_align)
  plot(r_final)
  
  # Save
  
  
}

