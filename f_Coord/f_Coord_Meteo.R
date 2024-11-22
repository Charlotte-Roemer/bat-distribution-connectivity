print("load Meteo")
# points = FCoord
library(dplyr)
library(terra)
library(sf)
Coord_Meteo <- function(points) {
  if (opt$mode == "predict") {
    mode <- "predict"
    command <- paste0("python3 get_meteo.py --mode ", mode, " --date ", opt$date, " --file ", points, ".csv")
  } else if (opt$mode == "train"){
    command <- paste0("python3 get_meteo.py  --file ", points, ".csv")
  }

  setwd(project_path) # optional ?

  system(command)

  setwd(project_path)

  file_name <- paste0(points, "_meteo.csv")

  # now i need to calculate difference to normal:
  # - load raster normals
  # - get value at point
  # - calculate difference (train, keep normals for predict)
  # - remove other columns (or not)
  if (exists("date_pred")) {
    OccSL <- readr::read_delim(paste0(points, ".csv"), delim = ",") %>%
      select(c("X", "Y"))
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)
    OccSL$Nuit <- date_pred

    OccSL_L93 <- OccSL %>%
      sf::st_transform(2154)

    CoordH <- names_coord
  } else {
    print("loading observations:")
    OccSL <- read.csv(paste0(points, ".csv")) %>%
      select(names_coord)
    print("observations loaded")
    # OccSL <- points
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL %>%
      sf::st_transform(2154)
    CoordH <- names_coord
  }

  nuits_uniques <- unique(OccSL_L93$Nuit)
  tableaux <- list()

  for (nuit in nuits_uniques) {
    nuit <- as.character(nuit)
    print(nuit)
    tableau_nuit <- OccSL_L93[OccSL_L93$Nuit == nuit, ]
    annee <- strsplit(nuit, "-")[[1]][1]
    pattern <- paste0("A", annee)
    raster <- list.files(layers, pattern = pattern, full.names = TRUE)

    VCF <- terra::rast(raster)
    # create a buffer around the points
    tableau_BS <- sf::st_buffer(tableau_nuit, bs) %>%
      sf::st_transform(4326)
    tableau_BM <- sf::st_buffer(tableau_nuit, bm) %>%
      sf::st_transform(4326)
    tableau_BL <- sf::st_buffer(tableau_nuit, bl) %>%
      sf::st_transform(4326)

    print("Buffer Small")

    SpVCF_S_tab <- exactextractr::exact_extract(VCF, tableau_BS, "mean")
    tableau_nuit$SpVCF_S <- SpVCF_S_tab

    print("Buffer Medium")

    SpVCF_M_tab <- exactextractr::exact_extract(VCF, tableau_BM, "mean")
    tableau_nuit$SpVCF_M <- SpVCF_M_tab

    print("Buffer Large")

    SpVCF_L_tab <- exactextractr::exact_extract(VCF, tableau_BL, "mean")
    tableau_nuit$SpVCF_L <- SpVCF_L_tab
    tableaux <- rlist::list.append(tableaux, tableau_nuit)
  }

  tab <- do.call("rbind", tableaux)


  VCF <- data.frame(cbind(tab$Nuit, tab$X, tab$Y, tab$SpVCF_S, tab$SpVCF_M, tab$SpVCF_L))
  colnames(VCF) <- c("Nuit", "X", "Y", "SpVCF_S", "SpVCF_M", "SpVCF_L")
  fwrite(VCF, paste0(FOccSL, "_VCF.csv"))
  if (exists("date_pred")) {
    OccSL <- readr::read_delim(paste0(points, ".csv"), delim = ",") %>%
      dplyr::select(c("X", "Y"))
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_2000_meteo <- readr::read_delim(paste0(points_2000, "_meteo.csv"), delim = ",")
    colnames(OccSL_2000_meteo)[1] <- "FID"

    OccSL_2000_meteo$FID <- OccSL_2000_meteo + 1
    OccSL_2000_meteo <- OccSL_2000_meteo %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_500_meteo <- sf::st_join(OccSL, OccSL_2000_meteo, st_nearest_feature,
      left = TRUE, suffix = c("", ".y")
    ) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(-c(ends_with(".y"), FID))

    data.table::fwrite(OccSL_500_meteo, paste0(points, "_meteo.csv"))
  }
}
