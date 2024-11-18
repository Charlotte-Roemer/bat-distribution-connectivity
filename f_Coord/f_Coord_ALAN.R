print("load ALAN")

Coord_ALAN <- function(points, names_coord, bm, bl, layers){
  print("ALAN")
  library(data.table)
  library(terra)
  library(sf)
  library(exactextractr)
  library(tidyverse)
  library(rlist)
  library(lubridate)


  FOccSL <- points
  # OccSL=fread(paste0(FOccSL,".csv"))

  if (opt$mode == "predict") {
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
      dplyr::select(names_coord)
    print("observations loaded")
    # OccSL <- points
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL %>%
      sf::st_transform(2154)
    CoordH <- names_coord
  }

  unique_years <- unique(strsplit(OccSL_L93$Nuit, "-"[[1]][1]))
  tables <- list()
  # Get all the .tif files in the folder
  alan <- list.files(folder_alan,
    recursive = TRUE,
    pattern = "tif$",
    full.names = TRUE
  )

  grep()
  for (year in unique_years) {
    year <- as.character(year)
    print(year)
    table_year <- OccSL_L93[strsplit(OccSL_L93$Nuit, "-")[[1]][1] == year, ]

    # match the year alan raster
    year_index <- grep(pattern = paste0("_", year, "_"), x = alan)
    year_file <− alan[year_index]
    ALAN <- terra::rast(year_file)
    # create a buffer around the points
    table_BM <- sf::st_buffer(table_year, bm) %>%
      sf::st_transform(4326)
    table_BL <- sf::st_buffer(table_year, bl) %>%
      sf::st_transform(4326)

    print("Buffer Medium")

    SpALAN_M_tab <- exactextractr::exact_extract(ALAN, table_BM, "mean")
    table_year$SpALAN_M <- SpALAN_M_tab

    print("Buffer Large")

    SpALAN_L_tab <- exactextractr::exact_extract(ALAN, table_BL, "mean")
    table_year$SpALAN_L <- SpALAN_L_tab
    tables <- rlist::list.append(tables, table_year)
  }

  tab <- do.call("rbind", tables)

  # ALAN = raster(layer)

  print("alanL")
  ALAN <- data.frame(cbind(tab$Nuit, tab$X, tab$Y, tab$SpALAN_M, tab$SpALAN_L))
  colnames(ALAN) <- c("Nuit", "X", "Y", "SpALAN_M", "SpALAN_L")
  data.table::fwrite(ALAN, paste0(FOccSL, "_ALAN.csv"))
}
