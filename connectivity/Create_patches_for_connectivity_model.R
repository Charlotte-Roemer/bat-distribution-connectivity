
# Create patches for movement model

library(data.table)
library(tidyverse)
library(raster)
library(sf)
#library(landscapemetrics)
library(gdistance)
library(beepr)
library(terra)

# Load acoustic predictions
Name =  "RFspat_VC90_2026-05" #"weighted_2022-08-15"
Directory <- "/home/charlotte/Bureau/SDM/IDF_k4/Season/" # repertory with outputs from Predict_act
Species = "Pipnat"  # "Myocap"

NewDir = paste0(Directory, "/Connectivity_", Name)
dir.create(NewDir)

START=Sys.time()

# Functions needed

Q80_function <- function(x_raster)
{
  # Patches are areas with value > Q80
  Data_Q80 = x_raster
  Data_Q80[Data_Q80<quantile(Data_Q80, 0.8)] = 0
  Data_Q80[Data_Q80>quantile(Data_Q80, 0.8)] = 1
  
  return(Data_Q80)
}

Clump_function <- function(Raster_sub)
{
  Raster_patches <- clump(Raster_sub, directions=8, gaps=F)
  y <- patches(rast(Raster_patches))
  rz <- zonal(cellSize(y, unit="ha"), y, sum, as.raster=TRUE)
  s <- ifel(rz < 100, NA, y)
  s2 = st_as_sf(as.polygons(s))
  patches_perimeter = as.data.frame(st_coordinates(s2))
  colnames(patches_perimeter)[colnames(patches_perimeter) == "X"] <- "x"
  colnames(patches_perimeter)[colnames(patches_perimeter) == "Y"] <- "y"
  patches_perimeter$id = rownames(patches_perimeter)
  
  return(patches_perimeter)
}

# # Species names
# List_species_names = c("Myomyo", "Myobly", "Barbar", "Eptser", "Hypsav", "Minsch", 
#                        "Myodau", "Nyclei", "Nycnoc", "Pipkuh", "Pipnat", "Pippip", 
#                        "Pippyg", "Rhifer", "Rhihip", "Tadten", "Rhieur", "Myocap", 
#                        "Myobec", "Myobra", "Myomys", "Myonus", "Myocry", "Myonat", 
#                        "Myodas", "Myopun", "Pleaus", "Pleaur", "Plemac", "Eptnil", 
#                        "Vesmur", "Myoalc", "Myoema", "Nyclas", "Plesp", "MyoGT")

# Load files
if(Species == "All"){
  list_file <- list.files(Directory, recursive=FALSE, pattern="*.tif$")
}else{
  list_file <- list.files(Directory, recursive=FALSE, pattern=paste0("^", Name, ".*.", Species, ".*.tif$"))
}

ls2 = paste0(Directory,list_file, sep="")
ld <- lapply(ls2, function(x) raster(x))

list_species = unique(tstrsplit(list_file,split="_")[[8]])

if(Species != "All"){
  list_species = Species
}

for(i in 1:length(list_species)){
  Sp = list_species[i]
  
  print(Sp)
  
  # Define origin and goal data
  dataa_ALLSPRING = subset(ld, grepl(Sp, ld) & grepl("spring", ld))
  dataa_ALLSUMMER = subset(ld, grepl(Sp, ld) & grepl("summer", ld))
  dataa_ALLAUTUMN = subset(ld, grepl(Sp, ld) & grepl("autumn", ld))
  s <- stack(ld)
  if(dim(s)[3]!=3){
    error(paste0("error: there are ", dim(s)[3], " layers for ", Sp))
  }
  
  print("Transition layer")
  
  # Create transition layer by selecting the highest value of each pixel across the year
  Raster_TRANSITION_YEAR = max(s)
  
  # Save
  Raster_extent= extent(Raster_TRANSITION_YEAR)
  Raster_TRANSITION_YEAR_wtNA = Raster_TRANSITION_YEAR
  Raster_TRANSITION_YEAR_wtNA[is.na(Raster_TRANSITION_YEAR_wtNA)] <- 0 # replace NA by 0 because passage function does not like NA
  land_cost_sub_YEAR <- crop(Raster_TRANSITION_YEAR_wtNA, Raster_extent)
  land_cost_sub_YEAR <- transition(land_cost_sub_YEAR, transitionFunction = mean, 8)
  land_cost_sub_YEAR <- geoCorrection(land_cost_sub_YEAR, type = "r", scl = T) # "r" because we anticipate low theta values and randomised shortest path method
  saveRDS(land_cost_sub_YEAR, paste0(NewDir, "/", Sp, "_", "Year_", "Transition", ".rds"))
  
  # Get highest values for patches
  print("Get highest values for patches")
  Raster_SPRING= Q80_function(dataa_ALLSPRING[[1]])
  Raster_AUTUMN= Q80_function(dataa_ALLAUTUMN[[1]])
  
  # Save result
  #writeRaster(Raster_MID, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/Pipnat_MID.tif"), overwrite=TRUE)
  
  # plot(Raster_0315)
  
  # Crop data in box (optional)
  print("Crop")
  Raster_SPRING_sub <- crop(Raster_SPRING, Raster_extent)
  Raster_AUTUMN_sub <- crop(Raster_AUTUMN, Raster_extent)
  
  # Clump pixels and remove small patches
  print("Clump")
  SPRING_patches_perimeter = Clump_function(Raster_SPRING_sub)
  AUTUMN_patches_perimeter = Clump_function(Raster_AUTUMN_sub)
  
  ListPatches = list(SPRING_patches_perimeter, AUTUMN_patches_perimeter)
  
  ListTimes = c("SPRING", "AUTUMN")
  
  # Save patches
  fwrite(SPRING_patches_perimeter, paste0(NewDir, "/", Sp, "_", "SPRING", ".csv"))
  fwrite(AUTUMN_patches_perimeter, paste0(NewDir, "/", Sp, "_", "AUTUMN", ".csv"))
}


END=Sys.time()
TIMEDIFF=END-START
TIMEDIFF

beep(2)

