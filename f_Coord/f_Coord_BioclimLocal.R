Coord_BioclimLocal <- function(points, names_coord, layer_folder, layCorr) {
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
  OccSL <- read_delim(paste0(points, ".csv")) %>%
    select(names_coord)

  OccSL$FID <- c(1L:nrow(OccSL))

  OccSL <- OccSL %>%
    sf::st_as_sf(coords = names_coord, crs = 4326L, remove = FALSE)

  # List bioclim rasters
  CoordH <- names_coord
  asc_files <- list.files(layer_folder, pattern = ".tif$", full.names = TRUE)

  # Load Bioclim values extended at sea
  GrossBioclim <- read_sf(layCorr)

  print("extracting bioclim")
  # Extract all 19 Bioclim values for each grid point
  for (i in 1L:length(asc_files)) {
    rasti <- terra::rast(asc_files[i])
    Sys.time()
    SpBioci <- terra::extract(rasti, OccSL)
    OccSL$SpBioci <- SpBioci[, 2L]

    NumBioci <- data.table::tstrsplit(basename(asc_files[i]), split = "_")[[4]]
    NumBioci <- gsub(".tif", "", NumBioci)
    print(paste0("extracting : SpBioC", NumBioci))

    names(OccSL)[ncol(OccSL)] <- paste0("SpBioC", NumBioci)
  }

  #multiplier par 10 : 1-2 4-11
  #for back-compatibility with old-fashioned Bioclim in 1e-1�C
  OccSL$SpBioC1 <- OccSL$SpBioC1*10
  OccSL$SpBioC2 <- OccSL$SpBioC2*10
  OccSL$SpBioC4 <- OccSL$SpBioC4*10
  OccSL$SpBioC5 <- OccSL$SpBioC5*10
  OccSL$SpBioC6 <- OccSL$SpBioC6*10
  OccSL$SpBioC7 <- OccSL$SpBioC7*10
  OccSL$SpBioC8 <- OccSL$SpBioC8*10
  OccSL$SpBioC9 <- OccSL$SpBioC9*10
  OccSL$SpBioC10 <- OccSL$SpBioC10*10
  OccSL$SpBioC11 <- OccSL$SpBioC11*10

  OccSL_NA <- subset(OccSL, is.na(OccSL$SpBioC1))

  OccSL_A <- subset(OccSL, !is.na(OccSL$SpBioC1))

  print(nrow(OccSL_NA))

  # For points which had no correspondance with Bioblim, use the layer Bioclim Gross (extension at sea)
  if (nrow(OccSL_NA) > 0) {
    
    GrossBioclim = GrossBioclim   %>%
      st_transform(st_crs(OccSL_NA))

    # Extract GrossBioclim variables
    print("extracting bioclim for points at sea")

    ## print(names(OccSL_NA))

    OccSL_NA_pour_inter <- OccSL_NA %>%
      select(-starts_with("SpBio"))

    OccSL_Add <- st_intersection(OccSL_NA_pour_inter, GrossBioclim)
    names(OccSL_Add) <- gsub("bio", "SpBioC", names(OccSL_Add))

    # Paste coordinates and Bioclim Gross values
    OccSL_NAdd <- OccSL_Add %>%
      select(!ID) %>%
      as.data.frame()

    names(OccSL_NAdd)[names(OccSL_NAdd) == 'num.site'] <- "num site"

    print("drop geometry a")
    OccSL_A <- sf::st_drop_geometry(OccSL_A)
    ## OccSL_A <- OccSL_A %>%
    ##     select(-geometry) %>%
    ##     as.data.frame()

    print("drop geometry nadd")

    OccSL_NAdd <- sf::st_drop_geometry(OccSL_NAdd)
    OccSL_NAdd <- OccSL_NAdd %>%
      select(-geometry) %>%
      as.data.frame()


    OccSL_All <- rbind(OccSL_A, OccSL_NAdd)

  } else {
    OccSL_All <- as.data.frame(OccSL_A)
  }

  OccSL_All <- OccSL_All %>%
    select(!c(FID))

  colnames(OccSL_All)[colnames(OccSL_All) == 'latitude'] <- "Y"
  colnames(OccSL_All)[colnames(OccSL_All) == 'longitude'] <- "X"

  print(paste0("Writing file ", FOccSL, "_Bioclim.csv"))
  fwrite(OccSL_All, paste0(FOccSL, "_Bioclim.csv"))
}
