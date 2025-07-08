library(exactextractr)
library(optparse)
library(dplyr)
library(stringr)
library(sf)
source("variables.R")


# Setup project folder (must contain folders scripts, data, outputs):
setwd(data_path) # dossier dans lequel

## Script is called with Rscript and options :
option_list <- list(
  optparse::make_option(c("-m", "--mode"),
    type = "character", default = NULL,
    help = "Set train or predict mode"
  ),
  optparse::make_option(c("-r", "--region"),
    type = "character", default = "france_met",
    help = 'Set region of interest between "france_met" (default),
 "europe", "idf"'
  ),
  optparse::make_option(c("-d", "--date"),
    type = "character", default = NULL,
    help = "Set fortnight date for predict grid extraction."
  ),
  optparse::make_option(c("-s", "--size"),
    type = "integer", default = 500,
    help = "Set grid size in meters."
  ),
  optparse::make_option(c("-c", "--csv"),
    type = "character", default = NULL,
    help = "filename without extension"
  ),
  optparse::make_option(c("-f", "--force-all"),
    type = "logical", default = FALSE,
    help = "Force recreating all variables (TRUE/FALSE)"
  )
)

# Parse options to opt object
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

## Setting variables from variables.R

data_folder <- data_path
zone_file <- file.path(data_folder, "GIS", "regions.gpkg")

## creating folders to receive the extracted variables if they don’t exist
dir.create(file.path(data_folder, "observations", "obs_vars"), showWarnings = FALSE)
dir.create(file.path(data_folder, "observations", "pred_vars"), showWarnings = FALSE)

obs_vars_folder <- file.path(data_folder, "observations", "obs_vars")
pred_vars_folder <- file.path(data_folder, "observations", "pred_vars")

# I keep the loop for later (meteo) no need for folder but useful as filenaming
# for (i in seq(3, 11)){
# for (ii in seq(1, 2)){
# folder_name <- paste0(sprintf("%02d", i), "-", ii)
# dir.create(file.path(pred_vars_folder, folder_name), showWarnings = FALSE)
# }
# }

## loading study area
# change to zone <− sf::st_read(zone_file, layer = opt$region) in production
print("Loading study area...")
zone <- sf::st_read(zone_file, layer = opt$region)
print("Study area loaded.")


## Function folder :
folderfun <- file.path(project_path, "f_Coord_autres")


# for testing purposes

## opt <- NA
## opt$region <- "france_met"
## opt$mode <- "train"


# FCoord varie selon qu’on soit sur les observations ou de la préparation
# de données de prédiction

# valid <- FALSE
# train <- TRUE  #Si on veut entrainer le modèle, sinon predict
# end of replacement

print(paste0("MODE : ", opt$mode))

print("autre")
print(opt$csv)
loc_train_exists <- file.exists(file.path(obs_vars_folder, paste0(opt$csv, ".csv")))
print(loc_train_exists)

## To extract predictors on observation points :

FCoord <- file.path(obs_vars_folder, opt$csv)
print(paste0("FCoord = ", FCoord))
GridName <- basename(FCoord)


Coord_Headers <- c("X", "Y") # long and lat

# buffers distances :
BS <- 100
BM <- 1000
BL <- 5000

print("Setting layers")
if (opt$region %in% c("idf", "france_met")) {
  #  GIS Layers locations :
  folder_alan <- file.path(data_folder, "GIS", "ALAN")
  folder_vcf <- file.path(data_folder, "GIS", "VCF")
  layer_alti <- file.path(data_folder, "GIS", "BDALTI")
  layer_Carthage_P <- file.path(data_folder, "GIS", "BD_TOPAGE_2023-shp", "SurfaceElementaire_FXX.shp")
  layer_Carthage_C <- file.path(data_folder, "GIS", "BD_TOPAGE_2023-shp", "TronconHydrographique_FXX.shp")
  folder_CLC <- file.path(data_folder, "GIS", "CLC")
  folder_OCS <- file.path(data_folder, "GIS", "OCS_OSO")
  folder_route <- file.path(data_folder, "GIS", "ROUTE500_3-0__SHP_LAMB93_FXX_2021-11-03")
  clim_norm_folder <- file.path(data_folder, "GIS", "CLIM_NORM")
  layer_wind_turbines <- file.path(data_folder, "GIS", "wind_turbines", "Mats_service_TOTAL.shp")
  # bioclim_folder <- file.path(data_folder, "GIS", "worldclim")
  bioclim_folder <- file.path(data_folder, "GIS", "chelsav2_bio")
  layer_ecoline_low <- file.path(data_folder, "GIS", "ecoline", "ecoline_vb_2017.shp")
  layer_ecoline_high <- file.path(data_folder, "GIS", "ecoline", "ecoline_vh_2017.shp")

  # layer_bioclim_gross <- file.path(data_folder, "GIS", "BioclimGross", "GrossV.shp")
  layer_wind <- file.path(data_folder, "GIS", "WIND", "gwa3_250_windspeed_10m_europe.tif")
  layer_precip <- file.path(data_folder, "GIS", "CLIM_NORM", "chelsea_eur11_pr_norm_1981-2005_v1_1.tif")
  layer_temp <- file.path(data_folder, "GIS", "CLIM_NORM", "chelsea_eur11_tas_norm_1981-2005_v1_1.tif")
  layer_grotto <- file.path(data_folder, "GIS", "grottocenter.gpkg")
}

ListLayer <- c(
  "ALAN", "Alti", "Carthage", "CLCraster", "OCS2018bis",
  "Transports", "Reseau", "Meteo", "VCF", "grotto"
)


print("Listing function files")
listfun <- list.files(folderfun, full.names = TRUE, pattern = ".R$")


print("Loading function files")
for (i in 1:length(listfun))
{
  source(listfun[i])
}

### Bioclim ###
# print("Bioclim")
# Coord_BioclimLocal(
#   points = FCoord,
#   names_coord = Coord_Headers,
#   layer_folder = bioclim_folder
# )
#
#
# ###  ALAN ###
# print("ALAN")
# Coord_ALAN(
#   points = FCoord,
#   names_coord = c(Coord_Headers, "Nuit"),
#   bm = BM,
#   bl = BL,
#   layers = folder_alan
# )
#
# # Grotto ###
# print("Grotto")
# Coord_Grotto(
#   points = FCoord,
#   names_coord = Coord_Headers,
#   bs = BS,
#   bm = BM,
#   bl = BL,
#   layer = layer_grotto
# )
#
# #  VCF ###
# print("VCF")
# Coord_VCF(
#   points = FCoord,
#   names_coord = c(Coord_Headers, "Nuit"),
#   bs = BS,
#   bm = BM,
#   bl = BL,
#   layers = folder_vcf
# )
#
# ## ALTI ####
# print("Altitude & slope")
# Coord_Alti(
#   points = FCoord,
#   names_coord = Coord_Headers,
#   bs = BS,
#   bm = BM,
#   bl = BL,
#   layer = layer_alti
# )
#
##  Wind Turbines ###
print("Wind Turbines")
Coord_eol(
  points = FCoord,
  names_coord = Coord_Headers,
  bs = BS,
  bm = BM,
  bl = BL,
  layer = layer_wind_turbines
)
#
# ##  CARTHAGE (eau) ####
# print("Water")
#
# Coord_Eau(
#   points = FCoord,
#   names_coord = Coord_Headers,
#   # bs = BS,
#   # bm = BM,
#   # bl = BL,
#   carthagep = layer_Carthage_P,
#   carthagec = layer_Carthage_C
# )
#
#
# ## Ecoline (idf)
# Coord_Ecoline(
#   points = FCoord,
#   names_coord = Coord_Headers,
#   ecoline_vh = layer_ecoline_high,
#   ecoline_vb = layer_ecoline_low,
#   buffer = BM
# )
#
#
#
## CLC Corine Land Cover (Habitat) ####
# print("CLC")
# Coord_CLCraster(
#   points = FCoord,
#   names_coord = c(Coord_Headers, "Nuit"),
#   bm = BM,
#   bl = BL,
#   layer = folder_CLC
# )

#
# ##  CESBIO (Habitat) ####
# print("OCS OSO")
# Coord_OCS_OSO(
#   points = FCoord,
#   names_coord = c(Coord_Headers, "Nuit"),
#   bs = BS,
#   bm = BM
#   # Buffer Large is not done because was too long in Pipeline V1, and
#   # at this scale, Corine Land Cover is sufficient anyway
#   , layer = Layer_OCS
# )
#
#
# ## ROADS and TRAINS ####
print("Roads and trains")
Coord_Route(
  points = FCoord,
  names_coord = Coord_Headers,
  bs = BS,
  bm = BM,
  bl = BL,
  folder = folder_route
)
#
print("Meteo")
Coord_Meteo(
  points = FCoord,
  temp = layer_temp,
  prec = layer_precip,
  wind = layer_wind
)
#
## loc_data <- file.path(loc, "data")


## if (exists("date_pred")) {
##   commande <- paste0("python group_obs.py --mode predict --date ", date_pred, " --loc ", loc_data)
## } else if (valid) {
##   espece <- as.character(svDialogs::dlgInput("Pour quelle espèces voulez-vous préparer les données ? (Barbar, Pippip...): ")$res)
##   commande <- paste0("python group_obs.py --mode valid --sp ", espece, " --loc ", loc_data)
## } else {
##   espece <- as.character(svDialogs::dlgInput("Pour quelle espèces voulez-vous préparer les données ? (Barbar, Pippip...): ")$res)
##   commande <- paste0("python group_obs.py --mode train --sp ", espece, " --loc ", loc_data)
## }


## if (getwd() != file.path(loc, "prep_data_chiros", "scripts")) {
##   setwd("./prep_data_chiros/scripts")
## }


## system(commande)

# remplacer combine par appel à group_obs.py avec arguments (date, mode, espèce)
# combineGIS_FR(
# points = FCoord,
# names_coord = Coord_Headers,
# layerlist = ListLayer
# )
