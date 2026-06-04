print("load europe land cover")

Coord_Land_Cover <- function(points, names_coord, bs, bm, bl, layer) {
  print("esaworldcover")
  print(points)
  library(data.table)
  library(dplyr)
  library(sf)
  library(exactextractr)
  library(rlist)

  ## pour tests
  # points <- FCoord
  # names_coord <- Coord_Headers
  # bm <- BM
  # bs <- BS
  # fin variable test
  FOccSL <- points

  if (opt$mode == "predict") {
    OccSL <- read.csv(paste0(points, ".csv")) %>%
      dplyr::select(c("X", "Y"))

    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL$Nuit <- date_pred

    OccSL_L3035 <- OccSL %>%
      sf::st_transform(3035)

    CoordH <- names_coord
  } else {
    OccSL <- read.csv(paste0(points, ".csv")) |>
      dplyr::select(c("X", "Y", "Nuit"))
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L3035 <- OccSL %>%
      sf::st_transform(3035)
  }

  OccSL_L3035$year <- sapply(strsplit(OccSL_L3035$Nuit, "-"), "[", 1)
  unique_years <- unique(OccSL_L3035$year)

  CoordH <- names_coord

  #BuffersSmall <- bs
  #BufferMedium <- bm

  ocs_file <- list.files(folder_OCS,
    recursive = TRUE,
    pattern = "tif$",
    full.names = TRUE
  )

  nuits_uniques <- unique(OccSL_L3035$Nuit)
  tableaux_m <- list()
  tableaux_s <- list()

  # removed ocs_annees TODO: remove comment

  options(dplyr.summarise.inform = FALSE) # to quiet the message produced by the sumarize function below

  ## nuit <- as.character(nuit)
  ## print(nuit)
  print("Europe)")
  print(paste0("Treating year : ")) # When more years will be avaialble, need to replace by the same for (year in unique_years) as in f_Coord_CLCraster.R
  print(unique_years)


  OCS <- terra::rast(ocs_file)
  print("Raster loaded")

  #OCS <- terra::project(OCS, "epsg:3035")
  #print("Raster reprojected")

  # For benchmark test
  n_test <- min(10000, nrow(OccSL_L3035))
  OccSL_L3035 <- OccSL_L3035[1:n_test, ]

  # Operation to crop ESA worldcover to correspond to the zone of points 
  # before reprojecting ESA to epsg:3035 because to lead to OOM killing
  #zone_3035 <- sf::st_transform(zone, 3035)
  zone_vect <- terra::vect(zone)
  zone_vect <- terra::project(zone_vect, terra::crs(OCS))
  print("Zone reprojected")
  OCS_crop <- terra::crop(OCS, zone_vect) # crop
  #OCS_crop <- terra::mask(OCS_crop, zone_vect) # puts values to NA if they are not in the polygon formed by BL
  OCS_crop_3035 <- terra::project( # reprojection to epsg:3035
  OCS_crop,
  "epsg:3035",
  method = "near"
  )

  #rm(OCS, OCS_crop)

  #print("Raster reprojected")

  print("Creating 100m raster") # to have realistic extraction time
  t_agg <- system.time({
  OCS_100m <- terra::aggregate(
    OCS_crop,
    fact = 10,
    fun = modal,
    na.rm = TRUE
  )
  })
  print(t_agg)
  rm(OCS_crop)

  # create a buffer around the points
  tableau_BM <- sf::st_buffer(OccSL_L3035, bm)
  tableau_BL <- sf::st_buffer(OccSL_L3035, bl)
  print(object.size(tableau_BM), units = "GB")
  print(object.size(tableau_BL), units = "GB")

  rm(OccSL_L3035)

  print("Buffers created")

  # reproject buffers to raster's CRS 
  tableau_BM <- sf::st_transform(
    tableau_BM,
    terra::crs(OCS)
  )

  tableau_BL <- sf::st_transform(
    tableau_BL,
    terra::crs(OCS)
  )

  cat("Buffers reprojected\n")

  # Extract values in medium buffer
  print("Medium buffer")
  t_medium <- system.time({
  landcov_fracs_Medium <- exactextractr::exact_extract(OCS, tableau_BM, function(df) {
    df %>%
      dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
      dplyr::group_by(FID, value) %>%
      dplyr::summarize(freq = sum(frac_total))
  }, summarize_df = TRUE, include_cols = "FID", progress = FALSE)
  })
  print(t_medium)

  rm(tableau_BM)

  # Extract values in large buffer
  print("Large buffer 10m")
  t_large10 <- system.time({
  landcov_fracs_Large <- exactextractr::exact_extract(OCS_100m, tableau_BL, function(df) {
    df %>%
      dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
      dplyr::group_by(FID, value) %>%
      dplyr::summarize(freq = sum(frac_total))
  }, summarize_df = TRUE, include_cols = "FID", progress = FALSE)
  })
  print(t_large10)

  # print("Large buffer extraction 100m")

  # t_large100 <- system.time({

  # landcov_fracs_Large_100m <- exactextractr::exact_extract(
  #   OCS_100m,
  #   tableau_BL,
  #   function(df) {
  #     df %>%
  #       dplyr::mutate(
  #         frac_total = coverage_fraction / sum(coverage_fraction)
  #       ) %>%
  #       dplyr::group_by(FID, value) %>%
  #       dplyr::summarize(
  #         freq = sum(frac_total),
  #         .groups = "drop"
  #       )
  #   },
  #   summarize_df = TRUE,
  #   include_cols = "FID",
  #   progress = FALSE
  # )

  # })

  # print(t_large100)

  # Append large buffer list
  rm(OCS, tableau_BL)

  # Pivot tibbles and rename columns
  landcov_fracs_Medium_pivot <- landcov_fracs_Medium %>%
    tidyr::pivot_wider(names_from = "value", values_from = "freq") %>% # pivot to use CLC values as column names
    dplyr::rename_with(~ paste0("SpHOCS", ., "M"), -FID) %>%
    replace(is.na(.), 0)

  rm(landcov_fracs_Medium)

  landcov_fracs_Large_pivot <- landcov_fracs_Large %>%
    tidyr::pivot_wider(names_from = "value", values_from = "freq") %>% # pivot to use CLC values as column names
    dplyr::rename_with(~ paste0("SpHOCS", ., "L"), -FID) %>%
    replace(is.na(.), 0)

  rm(landcov_fracs_Large)

  # landcov_fracs_Large100_pivot <- landcov_fracs_Large_100m %>%
  # tidyr::pivot_wider(
  #   names_from = value,
  #   values_from = freq
  # ) %>%
  # replace(is.na(.), 0)

  # rm(landcov_fracs_Large_100m)

  # print("COMPARAISON")

  # comp <- dplyr::inner_join(
  # landcov_fracs_Large10_pivot,
  # landcov_fracs_Large100_pivot,
  # by = "FID",
  # suffix = c("_10m", "_100m")
  # )

  # classes <- setdiff(
  # names(landcov_fracs_Large10_pivot),
  # "FID"
  # )

  # for(cl in classes){

  # cat("\n", cl, "\n")

  # print(
  #   cor(
  #     comp[[paste0(cl, "_10m")]],
  #     comp[[paste0(cl, "_100m")]],
  #     use = "complete.obs"
  #   )
  # )
  # }

  # stop("FIN COMPARAISON")

  HabufPropT_Tot <- dplyr::inner_join(landcov_fracs_Medium_pivot,
    landcov_fracs_Large_pivot,
    by = c("FID")
  ) %>%
    as.data.frame()

  OccSL_ARajouter <- dplyr::left_join(OccSL, HabufPropT_Tot) %>%
    as.data.frame() %>%
    dplyr::select(!c(FID, geometry))

  rm(OccSL, landcov_fracs_Medium_pivot, HabufPropT_Tot, landcov_fracs_Large_pivot)


  # colnames(OccSL_ARajouter)[colnames(OccSL_ARajouter) == 'latitude'] = "Y"
  # colnames(OccSL_ARajouter)[colnames(OccSL_ARajouter) == 'longitude'] = "X"

  if (opt$mode == "predict") {
    year <- substr(date_pred, 1, 4)
    data.table::fwrite(OccSL_ARajouter, paste0(FOccSL, "_", year, "_OCSraster.csv"))
  } else {
    data.table::fwrite(OccSL_ARajouter, paste0(FOccSL, "_OCSraster.csv"))
  }

  rm(OccSL_ARajouter)

}
