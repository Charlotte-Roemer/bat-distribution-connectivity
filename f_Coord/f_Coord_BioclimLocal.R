Coord_BioclimLocal <- function(points, names_coord, layer_folder) {
  library(data.table)
  library(terra)
  library(tidyverse)
  library(sf)

  ## for testing purposes :
  ## points <- "/home/bbk9/Documents/mnhn/data/observations/obs_vars/loc_train_france_met"
  ## names_coord <- c("X", "Y")
  ## layer_folder <- "/home/bbk9/Documents/mnhn/data/GIS/worldclim"
  ## layCorr <- "/home/bbk9/Documents/mnhn/data/GIS/BioclimGross/GrossV.shp"

  sf_use_s2(FALSE) # important to avoid geometry error during st_intersection(OccSL_NA[,1:3],GrossBioclim)

  FOccSL <- points

  # Load grid points
  print(paste("names_coord =", names_coord))

  OccSL <- read.csv(paste0(points, ".csv")) %>%
    select(names_coord)

  OccSL$FID <- c(1L:nrow(OccSL))

  OccSL <- OccSL %>%
    sf::st_as_sf(coords = names_coord, crs = 4326L, remove = FALSE)

  # List bioclim rasters
  CoordH <- names_coord
  asc_files <- list.files(layer_folder, pattern = ".tif$", full.names = TRUE)


  print("extracting bioclim")
  # Extract all 19 Bioclim values for each grid point
  for (i in 1L:length(asc_files)) {
    rasti <- terra::rast(asc_files[i])
    Sys.time()
    SpBioci <- terra::extract(rasti, OccSL)
    OccSL$SpBioci <- SpBioci[, 2L]

    NumBioci <- data.table::tstrsplit(basename(asc_files[i]), split = "_")[[2]]
    NumBioci <- gsub("bio", "", NumBioci)
    print(paste0("extracting : SpBioC", NumBioci))

    names(OccSL)[ncol(OccSL)] <- paste0("SpBioC", NumBioci)
  }

  # multiplier par 10 : 1-2 4-11
  # for back-compatibility with old-fashioned Bioclim in 1e-10C
  OccSL$SpBioC1 <- OccSL$SpBioC1 * 10L
  OccSL$SpBioC2 <- OccSL$SpBioC2 * 10L
  OccSL$SpBioC4 <- OccSL$SpBioC4 * 10L
  OccSL$SpBioC5 <- OccSL$SpBioC5 * 10L
  OccSL$SpBioC6 <- OccSL$SpBioC6 * 10L
  OccSL$SpBioC7 <- OccSL$SpBioC7 * 10L
  OccSL$SpBioC8 <- OccSL$SpBioC8 * 10L
  OccSL$SpBioC9 <- OccSL$SpBioC9 * 10L
  OccSL$SpBioC10 <- OccSL$SpBioC10 * 10L
  OccSL$SpBioC11 <- OccSL$SpBioC11 * 10L

  OccSL_NA <- subset(OccSL, is.na(OccSL$SpBioC1))

  OccSL_A <- subset(OccSL, !is.na(OccSL$SpBioC1))

  print(nrow(OccSL_NA))

  OccSL_All <- as.data.frame(OccSL_A)

  OccSL_All <- OccSL_All %>%
    dplyr::select(!c(FID, geometry))

  colnames(OccSL_All)[colnames(OccSL_All) == "latitude"] <- "Y"
  colnames(OccSL_All)[colnames(OccSL_All) == "longitude"] <- "X"

  print(paste0("Writing file ", FOccSL, "_Bioclim.csv"))
  fwrite(OccSL_All, paste0(FOccSL, "_Bioclim.csv"))
}
