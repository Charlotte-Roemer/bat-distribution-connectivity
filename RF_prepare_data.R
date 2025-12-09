#------------------------------------------------------------------------------#
#                      Function to prepare data                                #
#------------------------------------------------------------------------------#

prepare_data <- function(args, fpar, fsl) {
  library(dplyr)
  # Read bat activity data
  cat("Reading data", fill = TRUE)
  data_cpl2 <- data.table::fread(paste0(args[1L], ".csv")) # SpNuit2
  data_cpl2$Nuit <- as.Date(data_cpl2$Nuit)

  data_cpl3 <- dplyr::filter(data_cpl2, Nuit < args[11L])
  rm(data_cpl2)

  parti_nuit <- unique(
    dplyr::select(
      data_cpl3,
      c("participation", "Nuit")
    )
  )

  # Read predictor table :
  coord_sig <- data.table::fread(paste0(args[2L], ".csv")) # GI_FR_sites_loc (variables)

  coord_sig <- rename(
    coord_sig,
    longitude = args[12L],
    latitude = args[13L]
  )

  # cleaning data in case duplicated columns remains :
  coord_sig <- coord_sig |>
    rename_all(~ str_replace_all(., "\\.x", ""))
  coord_sig <- coord_sig |>
    select(-contains(".y"))

  # Read participation and locality data
  cat("Reading participations...", fill = TRUE)
  particip <- readr::read_delim(fpar, delim = ";") # p_export.csv
  cat("Reading locations...", fill = TRUE)
  site_loc <- data.table::fread(fsl) # sites_localites.txt

  # Identifies sites recorded near bat roosts !!! Remove these sites ???
  cat("Identifying shelters", fill = TRUE)
  Gite <- mapply(
    function(x, y) {
      ((grepl(paste0(y, "="), x)) | (grepl(paste0(y, " ="), x)))
    },
    site_loc$commentaire,
    site_loc$localite
  )
  site_loc$SpGite <- as.numeric(Gite)
  site_loc <- site_loc |>
    mutate_at(
      .vars = c("longitude", "latitude"),
      .fun = function(x) as.numeric(gsub(",", "\\.", x, fixed = TRUE))
    )

  # List coordinates existing in bat activity data to help add 0 in nb_contacts later
  cat("Listing unique locations", fill = TRUE) # Là il faut peut-être discuter
  list_par <- levels(as.factor(data_cpl3$participation))
  sel_par <- subset(particip, particip$participation %in% list_par)
  sel_par <- unique(sel_par)
  site_loc <- unique(site_loc)
  sel_par <- merge(sel_par, parti_nuit, by = "participation", all.x = TRUE)
  cat("Merging Site locations and participations", fill = TRUE)
  sel_par_sl <- merge(site_loc, sel_par, by.x = c("site", "nom"), by.y = c("site", "point"))

  #  TEST
  # write.csv(sel_par_sl, file.path(
  #   Output,
  #   paste0(
  #     "test_dat", "_selparsl.txt"
  #   )
  # ))
  cat("Merge done", fill = TRUE)
  coord_par <- aggregate(sel_par_sl$participation,
    by = c(
      list("longitude" = sel_par_sl$longitude),
      list("latitude" = sel_par_sl$latitude),
      list("Nuit" = sel_par_sl$Nuit),
      list("participation" = sel_par_sl$participation)
    ),
    FUN = length
  )
  coord_par$x <- NULL

  # Merge list of coordinates with environmental variables
  cat("Merging Coordinates", fill = TRUE)
  cat("coord_par", fill = TRUE)
  cat("coord_sig", fill = TRUE)
  coord_ps <- merge(
    coord_par,
    coord_sig,
    by = c("longitude", "latitude", "Nuit")
  )
  # TEST
  # write.csv(coord_ps, file.path(
  #   Output,
  #   paste0(
  #     "test_dat", "_coordps.txt"
  #   )
  # ))
  cat("Merged", fill = TRUE)
  coord_ps[is.na(coord_ps)] <- 0
  testPar <- grepl(args[6L], names(coord_ps))
  cat("Subseting data", fill = TRUE)
  numPar <- subset(c(1L:length(testPar)), testPar)
  cat("data subseted", fill = TRUE)
  cat(numPar[1L], fill = TRUE)

  # TEST

  # write.csv(numPar, file.path(
  #   Output,
  #   paste0(
  #     "test_dat", "_numpar.txt"
  #   )
  # ))
  coord_ps$participation <- as.data.frame(coord_ps)[, numPar[1L]]

  cat("Done... ready to return data", fill = TRUE)
  list(
    coord_ps, # environmental variables
    data_cpl3, # bat activity (without absence data)
    sel_par_sl # list of sampling sessions to know when to add absence data
  )
}

#------------------------------------------------------------------------------#
#                     Function to classify activity                            #
#------------------------------------------------------------------------------#

def_classes <- function(data) {
  data_no_zero <- data[data$nb_contacts > 0, ]
  quant <- quantile(
    x = unlist(data_no_zero$nb_contacts),
    c(0.25, 0.50, 0.75),
    na.rm = TRUE
  )
  data$acti_class[data$nb_contacts <= quant[1]] <- "Faible"
  data$acti_class[data$nb_contacts == 0] <- "NoAct"
  data$acti_class[data$nb_contacts > quant[1] & data$nb_contacts <= quant[2]] <- "Moyen"
  data$acti_class[data$nb_contacts > quant[2] & data$nb_contacts <= quant[3]] <- "Fort"
  data$acti_class[data$nb_contacts > quant[3]] <- "TresFort"
  data$acti_class <- factor(data$acti_class, levels = c("NoAct", "Faible", "Moyen", "Fort", "TresFort"))
  data$acti_class
}


#------------------------------------------------------------------------------#
#           Function to produce equilibrate sample size vector                 #
#------------------------------------------------------------------------------#

def_sample_vector <- function(data, column, proportion) {
  prop <- ceiling(proportion * min(summary(data[[column]])))
  rep(prop, length(summary(data[[column]])))
}

#------------------------------------------------------------------------------#
#                     Function to classify activity                            #
#------------------------------------------------------------------------------#


def_int_classes <- function(data) {
  data_no_zero <- data[data$nb_contacts > 0, ]
  quant <- quantile(
    x = unlist(data_no_zero$nb_contacts),
    c(0.25, 0.50, 0.75),
    na.rm = TRUE
  )
  data$acti_int_class[data$nb_contacts <= quant[1]] <- 1
  data$acti_int_class[data$nb_contacts == 0] <- 0
  data$acti_int_class[data$nb_contacts > quant[1] & data$nb_contacts <= quant[2]] <- 2
  data$acti_int_class[data$nb_contacts > quant[2] & data$nb_contacts < quant[3]] <- 3
  data$acti_int_class[data$nb_contacts >= quant[3]] <- 4
  data$acti_int_class
}


#------------------------------------------------------------------------------#
#            Function to select best predictors with VSURF                     #
#------------------------------------------------------------------------------#

get_prednames <- function(df, prednames, response_var, samp_vector = NULL) {
  predictors <- df |>
    dplyr::select(all_of(prednames))

  if (response_var == "acti_class") {
    vsurf <- VSURF::VSURF(predictors,
      df$acti_class,
      parallel = TRUE,
      nmin = 0.5,
      clusterType = "MPI",
      sampsize = samp_vector
    )
  } else {
    response <- as.vector(df$nb_contacts)

    vsurf <- VSURF::VSURF(predictors,
      response,
      parallel = TRUE,
      clusterType = "MPI"
    )
  }
  vsurf$varselect.pred
}


#------------------------------------------------------------------------------#
#              Function to select best predictors with PCA                     #
#------------------------------------------------------------------------------#

get_components <- function(predictors, prefix) {
  res <- FactoMineR::PCA(predictors, graph = FALSE)

  # get number of components for brokenstick
  total_comp <- nrow(res$eig)

  res <- FactoMineR::PCA(predictors, ncp = total_comp, graph = FALSE)
  pcent_variance <- res$eig[, 2L]

  broken_stick <- PCDimension::brokenStick(1L:total_comp, total_comp)

  nb_comp <- sum(pcent_variance > broken_stick * 100L)
  print(nb_comp)

  components <- data.frame(res$ind$coord[, 1L:nb_comp])
  names(components) <- paste0(names(components), "_", prefix)

  ret <- list()
  ret[[1]] <- components
  ret[[2]] <- res
  names(ret) <- c("components", "acp")
  ret
}

#------------------------------------------------------------------------------#
#              Function to select highest activity by grid                     #
#------------------------------------------------------------------------------#

filter_by_max_grid <- function(data, region) {
  grid_file <- intersect(
    list.files(file.path(data_path, "observations"),
      pattern = "SysGrid",
      full.names = TRUE
    ),
    list.files(file.path(data_path, "observations"),
      pattern = region,
      full.names = TRUE
    )
  )
  data <- st_transform(data, 4326)
  grid_d <- read.csv(grid_file)
  grid_sf <- sf::st_as_sf(grid_d, coords = c("X", "Y"), crs = 4326)
  data <- sf::st_join(data, grid_sf, join = st_nearest_feature)
  data <- dplyr::slice_max(data, nb_contacts, by = "ID")
  data <- st_transform(data, 2154)
  data
}

#------------------------------------------------------------------------------#
#            Function to get row which value is closest to  median             #
#------------------------------------------------------------------------------#

slice_median <- function(x, column) {
  x[which.min(abs(x[[column]] - median(x[[column]]))), ]
}

#------------------------------------------------------------------------------#
#    Function to select median activity by grid grid cell for each season      #
#------------------------------------------------------------------------------#


filter_by_median_season_grid <- function(data, region) {
  grid_file <- intersect(
    list.files(file.path(data_path, "observations"),
      pattern = "SysGrid",
      full.names = TRUE
    ),
    list.files(file.path(data_path, "observations"),
      pattern = region,
      full.names = TRUE
    )
  )

  data <- st_transform(data, 4326)

  grid_d <- read.csv(grid_file)

  grid_sf <- sf::st_as_sf(grid_d,
    coords = c("X", "Y"),
    crs = 4326
  )

  data <- sf::st_join(data, grid_sf, join = st_nearest_feature)

  data <- data |> arrange(desc(nb_contacts))

  data <- data |>
    dplyr::group_by(ID, SpSaison) |>
    dplyr::group_modify(~ slice_median(., "nb_contacts"), .groups = "keep") |>
    dplyr::ungroup()

  # data <- st_transform(data, 2154)
}


#------------------------------------------------------------------------------#
#            Function to get number of components selected in data             #
#------------------------------------------------------------------------------#


get_comp_nb <- function(data, name, data_path) {
  train_data <- read.csv(data)
  colonnes <- names(train_data)

  search_pattern <- paste0("^Dim.*", name, "$")
  sum(grepl(search_pattern, colonnes))
}
