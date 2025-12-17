library(caret)
library(randomForest)
library(stats)
library(FactoMineR)
library(factoextra)
library(PCDimension)
library(tidyr)
library(dplyr)
library(sf)
library(optparse)
source("RF_prepare_data.R")
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
  optparse::make_option(c("-m", "--method"),
    type = "character", default = "noSpace",
    help = "Which spatialization method you want to use ? (EDF, LatLong, noSpace)"
  ),
  optparse::make_option(c("-r", "--region"),
    type = "character", default = "france_met",
    help = "Which area do you want to predict on ?"
  ),
  optparse::make_option(c("-g", "--grid"),
    type = "character", default = "500",
    help = "Which size is your grid in meters (500, 200, 100) ?"
  ),
  optparse::make_option(c("-p", "--period"),
    type = "character", default = "year",
    help = "Is it a yearly model or a seasonal one ? (spring, summer, autumn)"
  ),
  optparse::make_option(c("--predict_period"),
    type = "character", default = NULL,
    help = "Is it a yearly model or a seasonal one ? (spring, summer, autumn)"
  ),
  optparse::make_option(c("--acti"),
    type = "character", default = NULL,
    help = "Variable to predict (nb_contacts or acti_int_class)"
  ),
  optparse::make_option(c("--data_sel"),
    type = "character", default = NULL,
    help = "How is the activity selected inside the pixels (all or median)"
  ),
  optparse::make_option(c("--variableselection"),
    type = "character", default = NULL,
    help = 'How the training variables were selected ("None", "VSURF", "indisp", "PCA", "PCAdecomp")'
  )
)

# Parse options to opt object
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)

period <- opt$period
activite <- opt$acti
data_sel <- opt$data_sel
selection <- opt$variableselection

# model <- "/media/tsevere/BBK/VC50_2025-02-28/RFspat_BarbarVC50_2025-02-28_EDF_Barbar.rds" # rds file
model_location <- file.path(
  data_path,
  "ModPred",
  activite,
  paste0(
    "VC",
    opt$threshold,
    "_",
    data_sel,
    "_",
    activite,
    "_",
    selection,
    "_",
    period,
    "_",
    opt$date_trained
  )
)
model <- file.path(
  model_location,
  paste0(
    "RFspat_",
    opt$species,
    "_",
    "VC",
    opt$threshold,
    "_",
    opt$date_trained,
    "_",
    period,
    "_",
    opt$region,
    "_",
    opt$method,
    "_",
    opt$species,
    ".rds"
  )
)

acps <- list.files(model_location, pattern = "acp")
acps <- grep(opt$species, acps, value = TRUE)
acps <- grep(period, acps, value = TRUE)

if (length(acps) > 0) {
  acp_names <- unique(t(as.data.frame(strsplit(acps, "_", fixed = TRUE)))[, 2L])
} else {
  acp_names <- c()
}


# for france only right now more to come in opt$
empty_raster_file <- file.path(data_path, "GIS", paste0(opt$region, "_", opt$grid, "m_L93.tif"))
empty_raster <- terra::rast(empty_raster_file)

model <- readRDS(model)


pred_data_file <- file.path(
  data_path,
  "observations",
  "pred_vars",
  paste0("data_pred", "_", opt$region, ".csv")
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
pred_data$Spprecipitations <- 0
pred_data$Sptemp <- 0
pred_data$Spwind <- 0
pred_data$SpRo_dist <- 0

if (!is.null(opt$predict_period)) {
  pred_data$SpSaison <- opt$predict_period
}

pred_data <- pred_data |>
  dplyr::mutate(SpSpring = if_else(SpSaison == "spring", 1, 0)) |>
  dplyr::mutate(SpSummer = if_else(SpSaison == "summer", 1, 0)) |>
  dplyr::mutate(SpAutumn = if_else(SpSaison == "autumn", 1, 0))



cat("Aggregating roads :", fill = TRUE)
# pred_data$SpRoAddM <- pred_data$SpRo1M + pred_data$SpRo2M +
#   pred_data$SpRo3M + pred_data$SpRo4M
#
cat("Roads aggregated :", fill = TRUE)

for (acp in acp_names) {
  cat(paste("ACP name : ", acp), fill = TRUE)
  train_data_file <-
    file.path(
      model_location,
      paste0(
        opt$species, "_", period, "_", opt$region, "_datatrain.csv"
      )
    )

  cat("Getting number of components :", fill = TRUE)

  comp_nb <- get_comp_nb(train_data_file, acp, model_location)

  cat(paste("Components : ", comp_nb), fill = TRUE)

  pca_file <- file.path(
    model_location,
    paste0(
      "acp_", acp, "_", opt$species, "_", period, ".rds"
    )
  )
  cat("Reading PCA file", fill = TRUE)
  pca <- readRDS(pca_file)
  pred_data <- as.data.frame(pred_data)
  print(names(pred_data))

  print(rownames(pca$var$coord))
  comp <- predict(pca, pred_data)
  comp <- as.data.frame(comp$coord)
  comp <- comp[, 1L:comp_nb]
  names(comp) <- paste0(names(comp), "_", acp)
  pred_data <- c(pred_data, comp)
}
pred_data <- as.data.frame(pred_data)
pred_names <- names(pred_data)
missing_vars <- setdiff(train_names, pred_names) # pour connaitre les colonnes à ajouter
cat(paste("missing vars : ", missing_vars))

# for predicting over paris with france data
for (variable in missing_vars) {
  pred_data[[variable]] <- 0
}

cat("data ready", fill = TRUE)

pred_data_sf <- st_as_sf(pred_data, coords = c(x = "X", y = "Y"), crs = 4326L) |>
  st_transform(2154L)

cat("data_sf ok", fill = TRUE)

# coords <- as.data.frame(st_coordinates(pred_data_sf))

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
  # dplyr::select(dplyr::starts_with("Sp")) |>
  sf::st_drop_geometry()
X_pred[is.na(X_pred)] <- 0

print("X pred done")
y_pred <- predict(model, X_pred)

print("prediction done!")
x_predict_map <- cbind(pred_data_sf, y_pred)

map <- terra::rasterize(
  x = x_predict_map, y = empty_raster,
  field = "y_pred",
  fun = "mean"
)
print("raster ready")

print("path")
period <- opt$predict_period

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
    data_sel,
    "_",
    activite,
    "_",
    selection,
    "_",
    opt$species,
    "_",
    opt$region,
    "_",
    period,
    ".tif"
  ))
)

print("raster written")
