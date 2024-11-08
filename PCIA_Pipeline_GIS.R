# install.packages("exactextractr")
library(exactextractr)
library(svDialogs)
library(optparse)
library(dplyr)
library(sf)
source("variables.R")


# Setup project folder (must contain folders scripts, data, outputs):
setwd(project_path) # dossier dans lequel

option_list <- list(
  optparse::make_option(c("-m", "--mode"), type = "character", default = NULL,
    help = "Set train or predict mode"), 
  optparse::make_option(c("-r", "--region"), type = "character", default = "france_met",
    help = 'Set region of interest between "france_met" (default), "europe", "idf"'),
  optparse::make_option(c("-d", "--date"), type = "character", default = NULL,
    help = 'Set fortnight date for predict grid extraction.'),
  optparse::make_option(c("-s", "--size"), type = "integer", default = 500,
    help = 'Set grid size in meters.'),
  optparse::make_option(c("-f", "--force-all"), type = "logical", default = FALSE,
    help = 'Force recreating all variables (TRUE/FALSE)')
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

if (is.null(opt$region)){
  print_help(opt_parser)
  stop("At least one region must be supplied from france_met, europe, idf with argument --region")
}

data_folder  <- data_path
zone_file <- file.path(data_folder, "GIS", "regions.gpkg")

zone <- st_read(zone_file, layer = "france_met")

# Localisation des fonctions d’extraction (relative à loc) :
folderfun <- file.path(project_path, "scripts", "f_Coord")


# FCoord varie selon qu’on soit sur les observations ou de la préparation
# de données de prédiction

#valid <- FALSE
#train <- TRUE  #Si on veut entrainer le modèle, sinon predict

# replace date setting with options
# to be replaced
if (opt$mode == "predict") {
  # date_pred should be in fortnightnumber_year format fn_yyyy. Fortnight scales
 # from 1 to 24 as there are two every month (ex : 2_2023)
  date_pred <- opt$date

}
# end of replacement


#Pipeline <- "exists"

if (opt$mode == "train") {
  nuits_obs_file <- file.path(data_folder,"observations",  "parti_unique_non_confi.csv")
  nuits_obs <- read.csv2(nuits_obs_file)
  nuits_obs$X <- nuits_obs$longitude
  nuits_obs$Y <- nuits_obs$latitude
  locs <- sf::st_as_sf(nuits_obs, coords = c("X", "Y"), crs = 4326)
  locs <- sf::st_intersection(locs$geometry, zone)
  locs$FID <- 1:nrow(locs)
  # setting the fortnight number (1-24) :
  locs$fortnight <- ifelse(as.integer(format(as.Date(locs$Nuit), "%d")) <= 15, 
    as.integer(format(as.Date(locs$Nuit), "%m")) * 2 - 1,
    as.integer(format(as.Date(locs$Nuit), "%m")) *2)
  locs$fortnight_year <- paste0(locs$fortnight, "_", format(as.Date(locs$Nuit), "%Y"))

  locs_l93 <- locs %>% sf::st_transform(2154)
  dates <- unique(locs_l93$Nuit)
  codes <- list()

#  # If we want to group local observations by fortnight rather than night :
  #dates <- unique(locs_l93$fortnight_year) # or locs_l93$fortnight to lose year effect
  #codes <- list()

  #for (date in dates){
    #subset_fortnight <- locs_l93[locs_l93$fortnight_year == date, ]
    #buffer <- subset_fortnight %>% sf::st_buffer(250) %>% sf::st_union() %>% sf::st_cast('POLYGON')
    #buffer <- sf::st_as_sf(buffer)
    #buffer$code <- paste0(date,"_", 1:nrow(buffer))
    #subset_fortnight <- subset_fortnight %>% sf::st_join(buffer, left = TRUE)
    #codes <- rlist::list.append(codes, subset_fortnight)
  #}


  for (date in dates){
    subset_nuit <- locs_l93[locs_l93$Nuit == date, ]
    sbuffer <- subset_nuit %>% sf::st_buffer(250) %>% sf::st_union() %>% sf::st_cast('POLYGON')s
    buffer <- sf::st_as_sf(buffer)
    buffer$code <- paste0(date, 1:nrow(buffer))
    subset_nuit <- subset_nuit %>% sf::st_join(buffer, left = TRUE)
    codes <- rlist::list.append(codes, subset_nuit)
  }

  code_bind <- do.call("rbind", codes)
  codes <- code_bind %>% dplyr::select(FID, code)
  codes <- codes %>% sf::st_drop_geometry()
  locs <- locs %>% dplyr::left_join(codes, by = "FID")
  locs  <- locs %>% dplyr::select(-FID) %>% sf::st_drop_geometry()
  locs$X <- locs$longitude
  locs$Y <- locs$latitude

  FCoord <- file.path(data_path, "variables", paste0("loc_train_", opt$region))
  readr::write_delim(locs, paste0(FCoord, ".csv"), delim = ",")
  GridName <- basename(FCoord)

} else {
  FCoord <- file.path(data_vc, paste0("SysGrid_500m_de_cote_", opt$region))
  GridName <- basename(FCoord)
}

Coord_Headers <- c("X", "Y", "Nuit") # long and lat

# buffers distances :
BS <- 50
BM <- 500
BL <- 5000

# Couches à extraire :
#Layer_ALAN <- "/mnt/beegfs/ybas/SIG/SVDNB_npp_20160101-20161231_75N060W_vcm-orm-ntl_v10_c201807311200.avg_rade9.tif"
folder_alan <- file.path(data_folder, "GIS", "ALAN")
folder_vcf <- file.path(data_folder, "GIS", "VCF")
# Layer_Alti <- file.path(loc, "data", "prep_data", "BDALTI")
layer_alti <- file.path(data_folder, "GIS", "BDALTI")
Layer_Carthage_P <- file.path(data_folder, "GIS", "BD_TOPAGE_2023-shp", "SurfaceElementaire_FXX.shp")
Layer_Carthage_C <- file.path(data_folder, "GIS", "BD_TOPAGE_2023-shp", "TronconHydrographique_FXX.shp")
folder_CLC <- file.path(data_folder, "GIS", "CLC")
folder_OCS <- file.path(data_folder, "GIS", "OCS_OSO")
folder_route <- file.path(data_folder, "GIS", "ROUTE500_3-0__SHP_LAMB93_FXX_2021-11-03")
clim_norm_folder <- file.path(data_folder, "GIS", "CLIM_NORM")

ListLayer <- c(
  "ALAN", "Alti", "Carthage", "CLCraster", "OCS2018bis",
  "Transports", "Reseau", "Meteo", "VCF"
)


listfun <- list.files(folderfun, full.names = T, pattern = ".R$")

for (i in 1:length(listfun))
{
  source(listfun[i])
}

### ALAN ###
print("ALAN")
Coord_ALAN(
  points = FCoord,
  names_coord = Coord_Headers,
  bm = BM,
  bl = BL,
  layers = folder_alan
)

### VCF ###
print("VCF")
Coord_VCF(
  points = FCoord,
  names_coord = Coord_Headers,
  bs = BS,
  bm = BM,
  bl = BL,
  layers = folder_vcf
)

### ALTI ####
print("Altitude & slope")
Coord_Alti(
  points = FCoord,
  names_coord = Coord_Headers,
  bs = BS,
  bm = BM,
  bl = BL,
  layer = layer_alti
)
Sys.time()


#### CARTHAGE (eau) ####
print("Water")
Coord_Carthage(
  points = FCoord,
  names_coord = Coord_Headers,
  bs = BS,
  bm = BM,
  bl = BL,
  carthagep = Layer_Carthage_P,
  carthagec = Layer_Carthage_C
)


### CLC Corine Land Cover (Habitat) ####
print("CLC")
Coord_CLCraster(
  points = FCoord,
  names_coord = Coord_Headers,
  bm = BM,
  bl = BL,
  layer = folder_CLC
)


### CESBIO (Habitat) ####
print("OCS OSO")
Coord_OCS_OSO(
  points = FCoord,
  names_coord = Coord_Headers,
  bs = BS,
  bm = BM
  # Buffer Large is not done because was too long in Pipeline V1, and
  # at this scale, Corine Land Cover is sufficient anyway
  , layer = Layer_OCS
)


#### ROADS and TRAINS ####
print("Roads and trains")
Coord_Route(
  points = FCoord,
  names_coord = Coord_Headers,
  bs = BS,
  bm = BM,
  bl = BL,
  folder = folder_route
)

print("Meteo")
Coord_Meteo(
  points = FCoord
)

loc_data <- file.path(loc, "data")


if (exists("date_pred")){
  commande <- paste0("python group_obs.py --mode predict --date ", date_pred, " --loc ", loc_data)
} else if (valid){
  espece <- as.character(svDialogs::dlgInput("Pour quelle espèces voulez-vous préparer les données ? (Barbar, Pippip...): ")$res)
  commande <- paste0("python group_obs.py --mode valid --sp ", espece, " --loc ", loc_data)
}else{
  espece <- as.character(svDialogs::dlgInput("Pour quelle espèces voulez-vous préparer les données ? (Barbar, Pippip...): ")$res)
  commande <- paste0("python group_obs.py --mode train --sp ", espece, " --loc ", loc_data)
}


if (getwd() != file.path(loc, "prep_data_chiros", "scripts")){
  setwd("./prep_data_chiros/scripts")
}


system(commande)

# remplacer combine par appel à group_obs.py avec arguments (date, mode, espèce)
#combineGIS_FR(
  #points = FCoord,
  #names_coord = Coord_Headers,
  #layerlist = ListLayer
#)
