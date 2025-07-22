library(caret)
library(randomForest)
library(tidyr)
library(sf)
library(optparse)
source("variables.R")

option_list <- list(
  optparse::make_option(c("-t", "--threshold"),
    type = "character", default = "50",
    help = 'Choose sorting threshold between values : "0", "50", "90" and "weighted'
  ),
  optparse::make_option(c("-s", "--species"),
    type = "character", default = "Minsch",
    help = "Choose for which species you want to make predictions"
  ),
  optparse::make_option(c("-d", "--date_trained"),
    type = "character", default = "2025-03-10",
    help = "when was the model you want to use trained ?"
  ),
  optparse::make_option(c("-p", "--date_to_predict"),
    type = "character", default = "20240608",
    help = "when was the model you want to use trained ?"
  ),
  optparse::make_option(c("-m", "--method"),
    type = "character", default = "EDF",
    help = "Which spatialization method you want to use ? (EDF, LatLong, noSpace)"
  ),
  optparse::make_option(c("-r", "--region"),
    type = "character", default = "france_met",
    help = "Which area do you want to predict on ?"
  ),
  optparse::make_option(c("-g", "--grid"),
    type = "character", default = "500",
    help = "Which size is your grid in meters (500, 200, 100) ?"
  )
)

# Parse options to opt object
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)


# model <- "/media/tsevere/BBK/VC50_2025-02-28/RFspat_BarbarVC50_2025-02-28_EDF_Barbar.rds" # rds file
model_location <- file.path(data_path, "ModPred", paste0("VC", opt$threshold, "_", opt$date_trained))
model <- file.path(
  model_location,
  paste0(
    "RFspat_",
    opt$species,
    "VC",
    opt$threshold,
    "_",
    opt$date_trained,
    "_",
    opt$method,
    "_",
    opt$species,
    ".rds"
  )
)

# for france only right now more to come in opt$
empty_raster_file <- file.path(data_path, "GIS", paste0(opt$region, "_", opt$grid, "m_L93.tif"))
empty_raster <- terra::rast(empty_raster_file)

model <- readRDS(model)

str(model)

pred_data_file <- file.path(
  data_path,
  "observations",
  "pred_vars",
  paste0("data_pred", "_", opt$region, "_", opt$date_to_predict, ".csv")
)

pred_data <- data.table::fread(pred_data_file)

pred_data$SpRecorder <- "SM4"
# ajouter colonnes nécessaires : spGite, spcosdaysinday

train_names <- colnames(model$trainingData)
pred_names <- colnames(pred_data)

setdiff(train_names, pred_names) # pour connaitre les colonnes à ajouter
# [1] "SpGite"      "SpCDate"     "SpSDate"     "SpYear"      "SpEDF1"      "SpEDF2"
# [7] "SpEDF3"      "SpEDF4"      "SpEDF5"      "Splatitude"  "Splongitude" "SpRecorder"

pred_data$SpGite <- 0L
nuits <- as.Date(pred_data$Nuit)
SpFDate <- lubridate::yday(nuits)
pred_data$SpCDate <- cos(SpFDate / 365L * 2L * pi) # to create a circular variable for date
pred_data$SpSDate <- sin(SpFDate / 365 * 2L * pi) # to create a circular variable for date
pred_data$SpYear <- lubridate::year(pred_data$Nuit)
pred_data$Splongitude <- pred_data$X
pred_data$Splatitude <- pred_data$Y

pred_data_sf <- st_as_sf(pred_data, coords = c(x = "X", y = "Y"), crs = 4326L) |>
  st_transform(2154)

coords <- as.data.frame(st_coordinates(pred_data_sf))

# sf object with 5 points: the bounding box of the grid of points + the center
# EDF <- rbind(
#   st_sf(geom = st_sfc(st_point(c(min(coords$X), min(coords$Y))))),
#   st_sf(geom = st_sfc(st_point(c(min(coords$X), max(coords$Y))))),
#   st_sf(geom = st_sfc(st_point(c(max(coords$X), min(coords$Y))))),
#   st_sf(geom = st_sfc(st_point(c(max(coords$X), max(coords$Y))))),
#   st_sf(geom = st_sfc(st_point(c(median(coords$X), median(coords$Y)))))
# )
# EDF <- st_set_crs(EDF, st_crs(pred_data_sf))
# EDF <- st_distance(pred_data_sf, EDF) / 1000 # calculate distance between the point and each of these 5 points
# EDF <- units::drop_units(EDF)
# EDF <- as.data.frame(EDF)
# names(EDF) <- paste0("EDF", 1:5)
# pred_data_sf$SpEDF1 <- EDF$EDF1
# pred_data_sf$SpEDF2 <- EDF$EDF2
# pred_data_sf$SpEDF3 <- EDF$EDF3
# pred_data_sf$SpEDF4 <- EDF$EDF4
# pred_data_sf$SpEDF5 <- EDF$EDF5
# pred_data_sf$SpRecorder <- "SM2BAT+"
#
X_pred <- pred_data_sf |>
  dplyr::select(dplyr::starts_with("Sp")) |>
  sf::st_drop_geometry()
X_pred[is.na(X_pred)] <- 0

y_pred <- predict(model, X_pred)

x_predict_map <- cbind(pred_data_sf, y_pred)

map <- terra::rasterize(
  x = x_predict_map, y = empty_raster,
  field = "y_pred",
  fun = "mean"
)

terra::writeRaster(
  x = map,
  overwrite = TRUE,
  filename = file.path(model_location, paste0(
    "RFspat_",
    "VC",
    opt$threshold,
    "_",
    opt$date_trained,
    "_",
    opt$method,
    "_",
    opt$species,
    "_",
    opt$region,
    ".tif"
  ))
)
