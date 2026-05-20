cat("load water europe", fill = TRUE)

Coord_Water <- function(points, names_coord, water_polyg, water_lines) {
  library(sf)
  library(data.table)
  library(tidyverse)

  #  # pour tests
  # points = FCoord
  # names_coord  <- Coord_Headers
  # bm <- BM
  # bl <- BL
  ## fin variable test


  if (opt$mode == "predict") {
    OccSL <- read.csv(paste0(points, ".csv")) |>
      dplyr::select(c("X", "Y"))

    OccSL$FID <- c(1L:nrow(OccSL))
    OccSL <- OccSL |>
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L3035 <- OccSL |>
      sf::st_transform(3035)
    OccSL_L3035$Nuit <- date_pred

    CoordH <- names_coord
  } else {
    OccSL <- read.csv(paste0(points, ".csv")) |>
      dplyr::select(names_coord)
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL |>
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L3035 <- OccSL |>
      sf::st_transform(3035)
    CoordH <- names_coord
  }

  rm(OccSL)

  # recuperation des donnees Carthage (eau)
  EauPolyg <- sf::read_sf(
    water_polyg
  )
  EauPolyg <- sf::st_transform(EauPolyg, 3035)

  EauPolyg$surf <- st_area(EauPolyg)

  EauLines <- sf::read_sf(
    water_lines
  ) # All "En service"
  EauLines <- sf::st_transform(EauLines, 3035)

  # Split result before saving?
  # Split <- FALSE
  # Start=10001
  # End=20000
  # Start <- 270001L
  # End <- 280194L
  # if (Split) {
  # OccSL <- OccSL[Start:(min(End, nrow(OccSL))), ]
  # }

  ##########################################
  ##########################################
  ########    Water  Distance     ##########
  ##########################################
  ##########################################

  nearest_p <- try(sf::st_nearest_feature(OccSL_L3035, EauPolyg))
  water_dist_polyg <- st_distance(OccSL_L3035,
    EauPolyg[nearest_p, ],
    by_element = TRUE
  )
  OccSL_L3035$water_dist_polyg <- water_dist_polyg

  nearest_l <- try(sf::st_nearest_feature(OccSL_L3035, EauLines))
  water_dist_line <- st_distance(OccSL_L3035,
    EauLines[nearest_l, ],
    by_element = TRUE
  )
  OccSL_L3035$water_dist_line <- water_dist_line
  OccSL_L3035$SpWD <- with(
    OccSL_L3035,
    pmin(water_dist_line, water_dist_polyg)
  )

  rm(water_dist_polyg, nearest_l, water_dist_line)

  ##########################################
  ########         Larges         ##########
  ##########################################

  EauLineslarge <- EauLines[EauLines$ORD_FLOW <= 6, ]
  EauPolyg$surf <- units::drop_units(EauPolyg$surf)
  EauPolyglarge <- EauPolyg[EauPolyg$surf >= 10000L, ]

  rm(EauPolyg, EauLines)

  nearest_pp <- try(sf::st_nearest_feature(OccSL_L3035, EauPolyglarge))
  water_dist_polyg_large <- st_distance(OccSL_L3035,
    EauPolyglarge[nearest_pp, ],
    by_element = TRUE
  )
  OccSL_L3035$water_dist_polyg_large <- water_dist_polyg_large

  nearest_lp <- try(sf::st_nearest_feature(OccSL_L3035, EauLineslarge))
  water_dist_line_large <- st_distance(OccSL_L3035,
    EauLineslarge[nearest_lp, ],
    by_element = TRUE
  )
  OccSL_L3035$water_dist_line_large <- water_dist_line_large

  OccSL_L3035$SpWDlarge <- with(
    OccSL_L3035,
    pmin(water_dist_line_large, water_dist_polyg_large)
  )

  rm(EauLineslarge, EauPolyglarge, nearest_lp, water_dist_line_large, water_dist_polyg_large)

  ######################################################################
  #################### Write ###########################################
  ######################################################################

  OccSL_WGS84 <- OccSL_L3035 |>
    st_transform(4326L) # back transform to WGS84

  OccSL_ARajouter <- subset(OccSL_L3035,
    select = grepl("Sp", names(OccSL_L3035), fixed = TRUE)
  )

  rm(OccSL_L3035)

  Carthage <- data.frame(cbind(
    st_coordinates(OccSL_WGS84),
    as.data.frame(OccSL_ARajouter)
  ))

  rm(OccSL_WGS84, OccSL_ARajouter)

  Carthage <- Carthage |>
    st_drop_geometry() |>
    select(!geometry)

  NewName <- paste0(points, "_Carthage.csv")
  head(Carthage)
  cat(NewName, fill = TRUE)

  fwrite(Carthage, NewName)
  rm(Carthage)
}
