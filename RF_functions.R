#------------------------------------------------------------------------------#
#                      Function to fit case study models                       #
#------------------------------------------------------------------------------#

# covariates <- Prednames
# spatial_ctrl <- sctrl
# traindf <- DataSaison
# n_tree = NTREE

fitvalpred_rf <- function(covariates,
                          spatial_ctrl,
                          traindf) {
  # # 1. Tune (find the best mtry)
  print("tuning model")
  mtrys <- c(17, 75, 100) # removed 200
  tune_ctrl <- caret::trainControl(method = "oob")
  cl <- parallel::makeCluster(10, type = "MPI")
  doParallel::registerDoParallel(cl)
  ntree <- c(800, 1500, 3000) # (150, 500, 1500, 6000)
  print("starting RF tuning")
  error <- list()
  params <- list()
  R2 <- list()
  ntrees <- list()
  A <- Sys.time()

  print("variable names :")
  print(colnames(traindf))

  for (tree in ntree) {
    for (mtry in mtrys) {
      tune_mod <- caret::train(
        x = as.data.frame(traindf)[, covariates],
        y = as.data.frame(traindf)[, "nb_contacts"],
        method = "rf",
        importance = TRUE,
        trControl = tune_ctrl,
        ntree = tree,
        tuneGrid = data.frame(mtry = mtry)
      )
      error <- append(error, tune_mod$results$RMSE)
      R2 <- append(R2, tune_mod$results$Rsquared)
      params <- append(params, tune_mod$results$mtry)
      ntrees <- append(ntrees, tree)
    }
  }

  print("model tuned")
  parallel::stopCluster(cl)
  B <- Sys.time()
  print(B - A)

  results <- data.frame(
    R2 = unlist(R2),
    RMSE = unlist(error),
    mtry = as.factor(unlist(params)),
    ntrees = as.factor(unlist(ntrees))
  )

  best_mtry <- results[results$R2 == max(results$R2), ]$mtry
  best_mtry <- as.numeric(as.character((best_mtry)))
  cat("Best tuning mtry", best_mtry, fill = TRUE)

  best_ntrees <- results[results$R2 == max(results$R2), ]$ntrees
  best_ntrees <- as.numeric(as.character((best_ntrees)))
  cat("Best tuning ntree", best_ntrees, fill = TRUE)

  cat("Best tuning r2", max(results$R2), fill = TRUE)
  cat("Best tuning rmse", min(results$RMSE), fill = TRUE)
  results <- results %>%
    dplyr::arrange(ntrees)

  # best_params_graph <- ggplot2::ggplot(
  #   data = results,
  #   aes(x = ntrees, y = mtry, fill = R2)
  # ) +
  #   geom_tile() +
  #   coord_equal() +
  #   scale_fill_viridis_c()
  #
  # 2. # build model and calculate RMSE and R² using the kNNDM cross-validation method
  spatial_grid <- data.frame(mtry = best_mtry)
  # spatial_grid <- data.frame(mtry = round(length(covariates)*2/3))
  print("building model")
  A <- Sys.time()
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)

  spatial_mod <- caret::train(
    x = as.data.frame(traindf)[, covariates], # train model
    y = as.data.frame(traindf)[, "nb_contacts"],
    method = "rf",
    importance = TRUE,
    trControl = spatial_ctrl,
    ntree = best_ntrees,
    tuneGrid = spatial_grid
  )

  B <- Sys.time()
  print(B - A)
  print("model built, calculating RMSE and R²")
  parallel::stopCluster(cl)
  spatial_stats <- global_validation(spatial_mod)[c("RMSE", "Rsquared")]
  names(spatial_stats) <- paste0("kNNDM_", names(spatial_stats))

  # 3. Surface predictions
  # preds <- predict(rstack, spatial_mod, na.rm=TRUE)

  # 4. Variable importance
  impfeat <- randomForest::importance(spatial_mod$finalModel, type = 2)
  impfeat <- sum(impfeat[row.names(impfeat) %in% covariates, 1]) / sum(impfeat[, 1]) * 100
  names(impfeat) <- "impfeat"

  # Tidy and return results
  tabres <- as.data.frame(t(c( # random_stats,
    spatial_stats,
    # AOA,
    impfeat
  )))
  # names(preds) <- c("prediction")
  list(
    tab = tabres,
    # preds = preds,
    ## tunemod = tune_mod,
    spatmod = spatial_mod,
    graphmod = results
  )
}

#------------------------------------------------------------------------------#
#             Function to fit case study models   (classification)             #
#------------------------------------------------------------------------------#


fitvalpred_rf_cat <- function(covariates,
                              spatial_ctrl,
                              traindf,
                              samp_sizes) {
  # # 1. Tune (find the best mtry)
  print("tuning model")
  mtrys <- c(17, 75, 100) # removed 200
  tune_ctrl <- caret::trainControl(method = "oob")
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)
  ntree <- c(800, 1500, 3000) # (150, 500, 1500, 6000)
  print("starting RF tuning")
  params <- list()
  Accuracy <- list()
  ntrees <- list()
  A <- Sys.time()


  print("covariates :")
  print(covariates)

  for (tree in ntree) {
    for (mtry in mtrys) {
      tune_mod <- caret::train(
        x = as.data.frame(traindf)[, covariates],
        y = as.data.frame(traindf)[, "acti_class"],
        method = "rf",
        importance = TRUE,
        trControl = tune_ctrl,
        ntree = tree,
        strata = traindf$acti_class,
        sampsize = samp_sizes,
        tuneGrid = data.frame(mtry = mtry)
      )
      print("r2")
      print(tune_mod)
      Accuracy <- append(Accuracy, tune_mod$results$Accuracy)
      params <- append(params, tune_mod$results$mtry)
      ntrees <- append(ntrees, tree)
    }
  }

  print("model tuned")
  parallel::stopCluster(cl)
  B <- Sys.time()
  print(B - A)
  print("Accuracy")
  print(Accuracy)

  results <- data.frame(
    Accuracy = unlist(Accuracy),
    mtry = as.factor(unlist(params)),
    ntrees = as.factor(unlist(ntrees))
  )

  best_mtry <- results[results$Accuracy == max(results$Accuracy), ]$mtry[1]
  print(results)
  best_mtry <- as.numeric(as.character((best_mtry)))
  cat("Best tuning mtry", best_mtry, fill = TRUE)

  best_ntrees <- results[results$Accuracy == max(results$Accuracy), ]$ntrees[1]
  best_ntrees <- as.numeric(as.character((best_ntrees)))
  cat("Best tuning ntree", best_ntrees, fill = TRUE)

  cat("Best tuning Accuracy", max(results$Accuracy), fill = TRUE)
  results <- results %>%
    dplyr::arrange(ntrees)

  # best_params_graph <- ggplot2::ggplot(
  #   data = results,
  #   aes(x = ntrees, y = mtry, fill = R2)
  # ) +
  #   geom_tile() +
  #   coord_equal() +
  #   scale_fill_viridis_c()
  #
  # 2. # build model and calculate RMSE and R² using the kNNDM cross-validation method
  spatial_grid <- data.frame(mtry = best_mtry)
  # spatial_grid <- data.frame(mtry = round(length(covariates)*2/3))
  print("building model")
  A <- Sys.time()
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)

  spatial_mod <- caret::train(
    x = as.data.frame(traindf)[, covariates], # train model
    y = as.data.frame(traindf)[, "acti_class"],
    method = "rf",
    importance = TRUE,
    trControl = spatial_ctrl,
    ntree = best_ntrees,
    tuneGrid = spatial_grid
  )

  B <- Sys.time()
  print(B - A)
  print("model built, calculating RMSE and R²")
  parallel::stopCluster(cl)
  spatial_stats <- CAST::global_validation(spatial_mod)[c("Accuracy")]
  names(spatial_stats) <- paste0("kNNDM_", names(spatial_stats))

  # 3. Surface predictions
  # preds <- predict(rstack, spatial_mod, na.rm=TRUE)

  # 4. Variable importance
  impfeat <- randomForest::importance(spatial_mod$finalModel, type = 2)
  impfeat <- sum(impfeat[row.names(impfeat) %in% covariates, 1]) / sum(impfeat[, 1]) * 100
  names(impfeat) <- "impfeat"

  # Tidy and return results
  tabres <- as.data.frame(t(c( # random_stats,
    spatial_stats,
    # AOA,
    impfeat
  )))
  # names(preds) <- c("prediction")
  list(
    tab = tabres,
    # preds = preds,
    ## tunemod = tune_mod,
    spatmod = spatial_mod,
    graphmod = results
  )
}


#------------------------------------------------------------------------------#
#       Function to calculate Moran’s I for a variable on a dataset            #
#------------------------------------------------------------------------------#

# in_data = sf_dataframe
# tested_variable = variable name from in_data you want to test spatial correlation


check_moran <- function(in_data, tested_variable) {
  print(head(in_data[, ..tested_variable]))
  # moran won’t work with no in_data in tested variable
  # in_data <- subset(in_data, !is.na(in_data[, ..tested_variable]))


  print(paste("Variable testée :", tested_variable))
  in_data <- sf::st_as_sf(in_data, coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_transform(2154)

  in_data$x_l93 <- sf::st_coordinates(in_data)[, 1]
  in_data$y_l93 <- sf::st_coordinates(in_data)[, 2]

  in_data <- sf::st_drop_geometry(in_data)

  # in order to mesure moran we need to avoid duplicates in locations
  # by adding more or less 1m to the longitude randomly we won’t have such
  #  problem without changing the in_data at our scale.
  rand_value <- runif(nrow(in_data), min = -1, max = 1)

  # summary(in_data$x_l93) # used to check longitude before adding random value

  in_data$x_l93 <- in_data$x_l93 + rand_value
  print("Data ready for Moran’s I")

  #  summary(in_data_l93$x_l93) # results are very close

  # we make a new sf object with modified longitude :
  in_data <- sf::st_as_sf(in_data, coords = c("x_l93", "y_l93"), crs = 2154)

  # See sfdep documentation for more on moran’s I
  # https://sfdep.josiahparry.com/reference/

  geo <- sf::st_geometry(in_data)

  print("Running knn...")

  nb <- sfdep::st_knn(geo, k = 150)

  print("Running Inverse distance...")
  wt <- sfdep::st_inverse_distance(nb, geo, 500) # we work at 500m distance (is it pertinent ?)

  dest <- in_data$acti_int_class # j’aurais préféré mettre tested_variable, mais trop de bugs

  print(paste("dest : ", length(dest))) # prints in case of length pb
  print(paste("nb : ", length(nb)))
  print(paste("wt : ", length(wt)))

  global_moran <- sfdep::global_moran(dest, nb, wt)

  print(paste("Global Moran : ", global_moran))

  return(global_moran$I)
}
