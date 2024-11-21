library(exactextractr)
library(svDialogs)
library(optparse)
library(dplyr)
library(stringr)
library(sf)
source("variables.R")


# Setup project folder (must contain folders scripts, data, outputs):
setwd(project_path) # dossier dans lequel

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
zone <- sf::st_read(zone_file, layer = "france_met")


## Function folder :
folderfun <- file.path(project_path, "f_Coord")


# for testing purposes

opt <- NA
opt$region <- "france_met"
opt$mode <- "train"


# FCoord varie selon qu’on soit sur les observations ou de la préparation
# de données de prédiction

# valid <- FALSE
# train <- TRUE  #Si on veut entrainer le modèle, sinon predict

# replace date setting with options
# to be replaced
if (opt$mode == "predict") {
  # date_pred should be in fortnightnumber_year format fn_yyyy. Fortnight scales
  # from 1 to 24 as there are two every month (ex : 2_2023)
  date_pred <- opt$date
}
# end of replacement


## To extract predictors on observation points :
if (opt$mode == "train") {
  
  ## Load file with "latitude" "longitude" and "Nuit" (date) columns
  ## CRS must be 4326
  nuits_obs_file <- file.path(data_folder,
                              "observations",
                              "parti_unique_non_confi.csv")
  nuits_obs <- read.csv2(nuits_obs_file)
  nuits_obs$X <- nuits_obs$longitude
  nuits_obs$Y <- nuits_obs$latitude

  locs <- sf::st_as_sf(nuits_obs, coords = c("X", "Y"), remove = FALSE, crs = 4326)
  ## rapide

  locs <- locs[zone, ]
  ## long
  ## locs_intersects <- sf::st_intersection(locs, zone)  

  locs <- locs %>% dplyr::select(X, Y, Nuit)
  locs$FID <- 1:nrow(locs)
  # setting the fortnight number (1-24) :
  locs$fortnight <-
    ifelse(as.integer(format(as.Date(locs$Nuit), "%d")) <= 15,
    as.integer(format(as.Date(locs$Nuit), "%m")) * 2 - 1,
    as.integer(format(as.Date(locs$Nuit), "%m")) * 2
  )
  locs$fortnight_year <- paste0(locs$fortnight,
                                "_",
                                format(as.Date(locs$Nuit), "%Y")
                                )

  locs_etrs89 <- locs %>% sf::st_transform(3035)

  grid_file <- file.path(data_folder,
                         "GIS",
                         paste0("SysGrid_500m_de_cote_",
                                opt$region,
                                ".csv"
                                )
                         )
  study_area <- st_read(zone_file, layer = opt$region)
  # buffer size is to be adapted depending on the study region 
  study_area_m <- st_transform(study_area, 3035)
  study_area_m_buf <- st_buffer(study_area_m, 250)

  xmin <- st_bbox(study_area_m_buf)["xmin"] - 250
  ymin <- st_bbox(study_area_m_buf)["ymin"] - 250
  xmax <- st_bbox(study_area_m_buf)["xmax"] + 250
  ymax <- st_bbox(study_area_m_buf)["ymax"] + 250


  cols <- (xmax - xmin) / 500
  rows <- (ymax - ymin) / 500

  grid_polyg <- st_make_grid(study_area_m_buf,
    cellsize = c(500, 500),
    offset = c(xmin, ymin),
    n = c(cols, rows)
  ) %>% st_as_sf()

  grid_polyg$grid_id <- seq(1, nrow(grid_polyg))

  locs_etrs89 <- st_join(locs_etrs89, grid_polyg, join = st_covered_by)
  width <- nchar(as.character(max(locs_etrs89$grid_id)))
  locs_etrs89$code <- paste0(
    str_pad(locs_etrs89$grid_id, width = width, pad = "0"),
    "-",
    locs_etrs89$fortnight
  )


  codes <- locs_etrs89 %>% dplyr::select(FID, code)
  codes <- codes %>% sf::st_drop_geometry()
  locs <- locs %>% dplyr::left_join(codes, by = "FID")
  locs <- locs %>%
    dplyr::select(-FID) %>%
    sf::st_drop_geometry()
  ## locs$X <- locs$longitude
  ## locs$Y <- locs$latitude

  FCoord <- file.path(obs_vars_folder, paste0("loc_train_", opt$region))
  readr::write_delim(locs, paste0(FCoord, ".csv"), delim = ",")
  GridName <- basename(FCoord)
  rm(grid_polyg)
} else {
  FCoord <- file.path(pred_vars_folder, paste0("SysGrid_500m_de_cote_", opt$region))
  GridName <- basename(FCoord)
}

Coord_Headers <- c("X", "Y", "Nuit") # long and lat

# buffers distances :
BS <- 50
BM <- 500
BL <- 5000

if (opt$region == "france_met") {
  #  GIS Layers locations :
  folder_alan <- file.path(data_folder, "GIS", "ALAN")
  folder_vcf <- file.path(data_folder, "GIS", "VCF")
  layer_alti <- file.path(data_folder, "GIS", "BDALTI")
  Layer_Carthage_P <- file.path(data_folder, "GIS", "BD_TOPAGE_2023-shp", "SurfaceElementaire_FXX.shp")
  Layer_Carthage_C <- file.path(data_folder, "GIS", "BD_TOPAGE_2023-shp", "TronconHydrographique_FXX.shp")
  folder_CLC <- file.path(data_folder, "GIS", "CLC")
  folder_OCS <- file.path(data_folder, "GIS", "OCS_OSO")
  folder_route <- file.path(data_folder, "GIS", "ROUTE500_3-0__SHP_LAMB93_FXX_2021-11-03")
  clim_norm_folder <- file.path(data_folder, "GIS", "CLIM_NORM")
  layer_wind_turbines <- file.path(data_folder, "GIS", "wind_turbines", "Mats_service_TOTAL.shp")
  bioclim_folder <- file.path(data_folder, "GIS", "worldclim")
  layer_bioclim_gross <- file.path(data_folder, "GIS", "BioclimGross", "GrossV.shp")
}

ListLayer <- c(
  "ALAN", "Alti", "Carthage", "CLCraster", "OCS2018bis",
  "Transports", "Reseau", "Meteo", "VCF"
)



listfun <- list.files(folderfun, full.names = TRUE, pattern = ".R$")


for (i in 1:length(listfun))
{
  source(listfun[i])
}

### Bioclim ###
print("Bioclim")
Coord_BioclimLocal(
  points = FCoord,
  names_coord = Coord_Headers,
  layer_folder = bioclim_folder,
  layCorr = layer_bioclim_gross
)

###  ALAN ###
print("ALAN")
Coord_ALAN(
  points = FCoord,
  names_coord = Coord_Headers,
  bm = BM,
  bl = BL,
  layers = folder_alan
)

###  VCF ###
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

### Wind Turbines ###
print("Wind Turbines")
Coord_eol(points = FCoord,
          names_coord = Coord_Headers,
          bs = BS,
          bm = BM,
          bl = BL,
          layer = layer_wind_turbines
          )

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


if (exists("date_pred")) {
  commande <- paste0("python group_obs.py --mode predict --date ", date_pred, " --loc ", loc_data)
} else if (valid) {
  espece <- as.character(svDialogs::dlgInput("Pour quelle espèces voulez-vous préparer les données ? (Barbar, Pippip...): ")$res)
  commande <- paste0("python group_obs.py --mode valid --sp ", espece, " --loc ", loc_data)
} else {
  espece <- as.character(svDialogs::dlgInput("Pour quelle espèces voulez-vous préparer les données ? (Barbar, Pippip...): ")$res)
  commande <- paste0("python group_obs.py --mode train --sp ", espece, " --loc ", loc_data)
}


if (getwd() != file.path(loc, "prep_data_chiros", "scripts")) {
  setwd("./prep_data_chiros/scripts")
}


system(commande)

# remplacer combine par appel à group_obs.py avec arguments (date, mode, espèce)
# combineGIS_FR(
# points = FCoord,
# names_coord = Coord_Headers,
# layerlist = ListLayer
# )
