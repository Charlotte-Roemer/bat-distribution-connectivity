
# Create patches for movement model

source("../variables.R")
library(data.table)
library(tidyverse)
library(raster)
library(sf)
#library(landscapemetrics)
library(gdistance)
library(terra)

## Script is called with Rscript and options :
option_list <- list(
  optparse::make_option(c("-s", "--species"),
                        type = "character", default = "Pippip",
                        help = 'Set modelling species as a 6 character species code (e.g. "Pippip")'
  ),
  optparse::make_option(c("-r", "--region"),
                        type = "character", default = "france_met",
                        help = 'Set region of interest between "france_met" (default),
                        "europe", "idf", "french_neighbours'
  ),
  optparse::make_option(c("-t", "--threshold"),
                        type = "character", default = "50",
                        help = 'Set sorting threshold between values : "0", "50", "90" and "weighted'
  ),
  optparse::make_option(c("-v", "--variableselection"),
                        type = "character", default = "None",
                        help = 'Choose which variable selection you want to make between values : "None", "VSURF", "indisp", "PCA", "PCAdecomp".'
  ),
  optparse::make_option(c("-d", "--date"),
                        type = "character",
                        help = "Necessary : pass date when script is run with $(date +%Y-%m-%d)"
  ),
  optparse::make_option(c("--data_sel"),
                        type = "character", default = "all",
                        help = "Do you reduce data by the pixel (no = all, yes = median)"
  ),
  optparse::make_option(c("--acti"),
                        type = "character", default = "nb_contacts",
                        help = "Which value do you want to predict ? nbcontacts, acticlass"
  ),
  optparse::make_option(c("-p", "--period"),
    type = "character", default = "year",
    help = "Which activity are you modelling year, spring, summer or autumn"
  )
)

# Parse options to opt object
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)

# Load acoustic predictions
Name = paste0("RFspat_VC", opt$threshold, "_",  opt$date, ".*.", "_noSpace_", opt$data_sel, "_", opt$acti, "_", opt$variableselection) # RFspat_VC90_2026-05-04_noSpace_all_acticlass_None
print(Name)
#Directory <- "/home/charlotte/Bureau/SDM/IDF_k4/Season/" # repertory with outputs from Predict_act
season_year = ifelse(opt$period == "year", "year", "season")
Directory = file.path(data_path, "ModPred", opt$acti, paste0("VC", opt$threshold, "_", opt$data_sel, "_", opt$acti, "_", opt$variableselection, "_", season_year))
print(Directory)
#NewDir = file.path(data_path, "Connectivity", Name)
NewDir = file.path(paste0(data_path, "/Connectivity/", basename(Directory)))
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

# Load files
print("Load files")
print(paste0("^", Name, ".*.", opt$species, ".*.", opt$region, ".*.", "predictions.tif$"))
list_file <- list.files(Directory, recursive=TRUE, pattern=paste0("^", Name, ".*.", opt$species, ".*.", opt$region, ".*.", "predictions.tif$"))

ls2 = paste(Directory, list_file, sep="/")
ld <- lapply(ls2, function(x) raster(x))

Sp = opt$species
print(Sp)

# Define origin and goal data
dataa_ALLSPRING = subset(ld, grepl(Sp, ld) & grepl("spring", ld))
dataa_ALLSUMMER = subset(ld, grepl(Sp, ld) & grepl("summer", ld))
dataa_ALLAUTUMN = subset(ld, grepl(Sp, ld) & grepl("autumn", ld))
s <- stack(ld)
if(dim(s)[3]!=3){
  stop(paste0("error: there are ", dim(s)[3], " layers for ", Sp))
}

print("Transition layer")

# Create transition layer by selecting the highest value of each pixel across the year
Raster_TRANSITION_YEAR = max(s)

# Create transition layer
Raster_extent= extent(Raster_TRANSITION_YEAR)
Raster_TRANSITION_YEAR_wtNA = Raster_TRANSITION_YEAR
Raster_TRANSITION_YEAR_wtNA[is.na(Raster_TRANSITION_YEAR_wtNA)] <- 0 # replace NA by 0 because passage function does not like NA
land_cost_sub_YEAR <- crop(Raster_TRANSITION_YEAR_wtNA, Raster_extent)

print("Merge with offshore layer")
# Read offshore raster
List_offshore = list.files("/sps/mnhn/croemer/data/GIS/Offshore", pattern = ".tif$", full.names = T)
offshore = subset(List_offshore, grepl(Sp, List_offshore))
offshore_raster <- rast(offshore)

# Align offshore raster according to transition layer
offshore_align <- project( 
  offshore_raster,
  land_cost_sub_YEAR,
  method = "bilinear"
)

# Check alignment before fusion : if TRUE it's OK
print("is alignment OK?")
print(compareGeom(land_cost_sub_YEAR, offshore_align))

# Fusion
land_cost_sub_YEAR_offshore <- cover(land_cost_sub_YEAR, offshore_align) 

# Convert to transition object
land_cost_sub_YEAR_final <- transition(land_cost_sub_YEAR_offshore, transitionFunction = mean, 8)
land_cost_sub_YEAR_final <- geoCorrection(land_cost_sub_YEAR_final, type = "c", scl = T) # "r" if we anticipate low theta values and randomised shortest path method or "c" else

print("Save transition object")
saveRDS(land_cost_sub_YEAR_final, paste0(NewDir, "/", Sp, "_", "Year_", "Transition", ".rds"))

# Get highest values for patches
print("Get highest values for patches")
Raster_SPRING= Q80_function(dataa_ALLSPRING[[1]])
Raster_AUTUMN= Q80_function(dataa_ALLAUTUMN[[1]])

# Crop data in box (optional)
print("Crop")
Raster_SPRING_sub <- crop(Raster_SPRING, Raster_extent)
Raster_AUTUMN_sub <- crop(Raster_AUTUMN, Raster_extent)

# Clump pixels and remove small patches
print("Clump")
SPRING_patches_perimeter = Clump_function(Raster_SPRING_sub)
AUTUMN_patches_perimeter = Clump_function(Raster_AUTUMN_sub)

ListPatches = list(SPRING_patches_perimeter, AUTUMN_patches_perimeter)

# Save patches
fwrite(SPRING_patches_perimeter, paste0(NewDir, "/", Sp, "_", "SPRING", ".csv"))
fwrite(AUTUMN_patches_perimeter, paste0(NewDir, "/", Sp, "_", "AUTUMN", ".csv"))

END=Sys.time()
TIMEDIFF=END-START
print(TIMEDIFF)

print(paste0("Patches selected for ", Sp))


