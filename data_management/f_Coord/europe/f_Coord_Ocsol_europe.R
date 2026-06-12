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

  ocs_file <- list.files(folder_OCS,
    recursive = TRUE,
    pattern = "tif$",
    full.names = TRUE
  )

  nuits_uniques <- unique(OccSL_L3035$Nuit)
  # tableaux_m <- list()
  # tableaux_s <- list()

  # removed ocs_annees TODO: remove comment

  options(dplyr.summarise.inform = FALSE) # to quiet the message produced by the sumarize function below

  print("Europe)")
  print(paste0("Treating year : ")) # When more years will be avaialble, need to replace by the same for (year in unique_years) as in f_Coord_CLCraster.R
  print(unique_years)


  OCS <- terra::rast(ocs_file)
  print("Raster loaded")

  #OCS <- terra::project(OCS, "epsg:3035")
  #print("Raster reprojected")

  # Operation to crop ESA worldcover to correspond to the zone of points 
  # before reprojecting ESA to epsg:3035 to avoid OOM killing
  #zone_3035 <- sf::st_transform(zone, 3035)
  zone_vect <- terra::vect(zone)
  zone_vect <- terra::project(zone_vect, terra::crs(OCS))
  print("Zone reprojected")
  print(terra::tmpFiles())
  OCS_crop <- terra::crop(OCS, zone_vect) # crop

  print("Crop done")
  print(terra::tmpFiles())

  terraOptions(memfrac = 0.5)

  print("Creating 100m raster")

  terraOptions(
  tempdir = "/sps/mnhn/croemer/Temp",
  memfrac = 0.3
)

  t_agg <- system.time({
  print(OCS_crop)
  OCS_100m <- terra::aggregate(
    OCS_crop,
    fact = 10,
    fun = modal,
    na.rm = TRUE,
    filename = "/sps/mnhn/croemer/Temp/OCS_100m.tif",
    overwrite = TRUE
  )
  })

  print(t_agg)
  print("OCS_100m")
  print(OCS_100m)
  rm(OCS_crop)

  print("100m raster created")
  print(gc())
  print(terra::tmpFiles())

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

  HabufPropT_Tot <- dplyr::inner_join(landcov_fracs_Medium_pivot,
    landcov_fracs_Large_pivot,
    by = c("FID")
  ) %>%
    as.data.frame()

  OccSL_ARajouter <- dplyr::left_join(OccSL, HabufPropT_Tot) %>%
    as.data.frame() %>%
    dplyr::select(!c(FID, geometry))

  rm(OccSL, landcov_fracs_Medium_pivot, HabufPropT_Tot, landcov_fracs_Large_pivot)


  # Save
  if (opt$mode == "predict") {
    year <- substr(date_pred, 1, 4)
    data.table::fwrite(OccSL_ARajouter, paste0(FOccSL, "_", year, "_OCSraster.csv"))
  } else {
    data.table::fwrite(OccSL_ARajouter, paste0(FOccSL, "_OCSraster.csv"))
  }

  rm(OccSL_ARajouter)

}
