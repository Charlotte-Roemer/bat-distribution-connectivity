#library(rlang, lib.loc = "~/altRlibs")
library(rlang)
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
source("../variables.R")

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

print(opt$species)

period <- opt$period
activite <- opt$acti
data_sel <- opt$data_sel
selection <- opt$variableselection

if (period == "year") {
  period_mod <- "year"
} else {
  period_mod <- "season"
}

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
    period_mod
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
missing_vars <- setdiff(train_names, c(pred_names, ".outcome")) # pour connaitre les colonnes à ajouter
cat(paste("missing vars : ", missing_vars), fill = TRUE)

# for predicting over paris with france data
for (variable in missing_vars) {
  pred_data[[variable]] <- 0
}

cat("data ready", fill = TRUE)

pred_data_sf <- st_as_sf(pred_data, coords = c(x = "X", y = "Y"), crs = 4326L) |>
  st_transform(2154L)

cat("data_sf ok", fill = TRUE)

X_pred <- pred_data_sf |>
  # dplyr::select(dplyr::starts_with("Sp")) |>
  sf::st_drop_geometry()
X_pred[is.na(X_pred)] <- 0

print("X pred done")

chunk_size <- 10000
n <- nrow(X_pred)

# y_pred <- numeric(n)
# y_sd   <- numeric(n)  # incertitude (écart-type)

# start_idx <- seq(1, n, by = chunk_size)

# for (i in start_idx) {
#   idx <- i:min(i + chunk_size - 1, n)
  
#   pred <- predict(
#     model$finalModel,
#     X_pred[idx, model$finalModel$xNames],
#     predict.all = TRUE
#   )
  
#   # moyenne
#   y_pred[idx] <- pred$aggregate
  
#   # incertitude = sd des arbres
#   y_sd[idx] <- apply(pred$individual, 1, sd)
# }

# ----------------------------------------------------------
# FINAL PREDICTION
# using full model
# ----------------------------------------------------------

y_pred <- numeric(n)

start_idx <- seq(1, n, by = chunk_size)

cat(
  "predicting final model",
  fill = TRUE
)

for(i in start_idx) {

  idx <- i:min(i + chunk_size - 1, n)

  y_pred[idx] <- predict(
    model$spatmod$finalModel,
    #model$finalModel,
    X_pred[idx, model$spatmod$finalModel$xNames]
    #X_pred[idx, model$finalModel$xNames]
    #X_pred[idx, model$covariates]
  )
}

# ----------------------------------------------------------
# UNCERTAINTY
# using spatial folds
# ----------------------------------------------------------

if(!is.null(model$fold_models)) {

  print("Check model$fold_models")
  print(names(model))
  print(length(model$fold_models))

  nfolds <- length(model$fold_models)

  all_fold_preds <- matrix(
    NA,
    nrow = n,
    ncol = nfolds
  )

  for(f in seq_len(nfolds)) {

    cat(
      paste("predicting fold", f),
      fill = TRUE
    )

    fold_model <- model$fold_models[[f]]
    print(attr(fold_model, "xNames"))

    fold_pred <- numeric(n)

    for(i in start_idx) {

      idx <- i:min(i + chunk_size - 1, n)
      xnames <- attr(fold_model, "xNames")

      fold_pred[idx] <- predict(
        fold_model,
        #X_pred[idx, fold_model$xNames]
        X_pred[idx, xnames, drop = FALSE]
      )
    }

    all_fold_preds[, f] <- fold_pred
  }

  # SD across folds
  y_sd <- apply(
    all_fold_preds,
    1,
    sd,
    na.rm = TRUE
  )

  # coefficient of variation
  #y_cv <- y_sd / abs(y_pred)
  y_cv <- y_sd / pmax(abs(y_pred), 1e-6)

  print("check all_fold_preds")
  print(dim(all_fold_preds))
  print(summary(all_fold_preds))
  print(sum(is.na(all_fold_preds)))

} else {

  y_sd <- rep(NA, n)
  y_cv <- rep(NA, n)

  print("WARNING: model$fold_models is NULL")

}

print("prediction done!")

#x_predict_map <- cbind(pred_data_sf, y_pred)
#stat <- y_sd # calculates the uncertainty of the predictions
#x_predict_map <- cbind(x_predict_map, stat)
x_predict_map <- cbind(
  pred_data_sf,
  y_pred,
  y_sd,
  y_cv
)
print(colnames(x_predict_map))

# Map predictions
map <- terra::rasterize(
  x = x_predict_map, y = empty_raster,
  field = "y_pred",
  fun = "mean"
)

print("raster ready")

# Map uncertainty (standard deviation between folds)
map_stats <- terra::rasterize(
  x = x_predict_map, y = empty_raster,
  field = "y_sd",
  fun = "mean"
)

# Map Cross-validation: useful to compare species with different activity levels
map_cv <- terra::rasterize(
  x = x_predict_map,
  y = empty_raster,
  field = "y_cv",
  fun = "mean"
)

print("raster stats ready")

print("Writing rasters")
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
    "_predictions.tif"
  ))
)


terra::writeRaster(
  x = map_stats,
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
    "_incertitude.tif"
  ))
)

terra::writeRaster(
  x = map_cv,
  overwrite = TRUE,
  filename = file.path(
    model_location,
    paste0(
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
      "_cv.tif"
    )
  )
)

print("raster written")
