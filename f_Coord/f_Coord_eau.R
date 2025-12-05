cat("load eau", fill = TRUE)
Coord_Eau <- function(points, names_coord, carthagep, carthagec) {
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

    OccSL_L93 <- OccSL |>
      sf::st_transform(2154)
    OccSL_L93$Nuit <- date_pred

    CoordH <- names_coord
  } else {
    OccSL <- read.csv(paste0(points, ".csv")) |>
      dplyr::select(names_coord)
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL |>
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL |>
      sf::st_transform(2154)
    CoordH <- names_coord
  }


  # recuperation des donnees Carthage (eau)
  CarthageP <- sf::read_sf(
    dsn = dirname(carthagep),
    layer = basename(tools::file_path_sans_ext(carthagep))
  )
  CarthageP$surf <- st_area(CarthageP)

  CarthageC <- sf::read_sf(
    dsn = dirname(carthagec),
    layer = basename(tools::file_path_sans_ext(carthagec))
  ) # All "En service"

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

  nearest_p <- try(sf::st_nearest_feature(OccSL_L93, CarthageP))
  water_dist_polyg <- st_distance(OccSL_L93,
    CarthageP[nearest_p, ],
    by_element = TRUE
  )
  OccSL_L93$water_dist_polyg <- water_dist_polyg

  nearest_l <- try(sf::st_nearest_feature(OccSL_L93, CarthageC))
  water_dist_line <- st_distance(OccSL_L93,
    CarthageC[nearest_l, ],
    by_element = TRUE
  )
  OccSL_L93$water_dist_line <- water_dist_line
  OccSL_L93$SpWD <- with(
    OccSL_L93,
    pmin(water_dist_line, water_dist_polyg)
  )

  ##########################################
  ########       Temporary        ##########
  ##########################################


  CarthageCinterm <- CarthageC[CarthageC$Persistanc == "intermittent", ]
  CarthagePinterm <- CarthageP[CarthageP$Persistanc == "intermittent", ]

  nearest_pp <- try(sf::st_nearest_feature(OccSL_L93, CarthagePinterm))
  water_dist_polyg_interm <- st_distance(OccSL_L93,
    CarthagePinterm[nearest_pp, ],
    by_element = TRUE
  )
  OccSL_L93$water_dist_polyg_interm <- water_dist_polyg_interm

  nearest_lp <- try(sf::st_nearest_feature(OccSL_L93, CarthageCinterm))
  water_dist_line_interm <- st_distance(OccSL_L93,
    CarthageCinterm[nearest_lp, ],
    by_element = TRUE
  )
  OccSL_L93$water_dist_line_interm <- water_dist_line_interm

  OccSL_L93$SpWDinterm <- with(
    OccSL_L93,
    pmin(water_dist_line_interm, water_dist_polyg_interm)
  )

  ##########################################
  ########        Salted          ##########
  ##########################################

  CarthageCsalted <- CarthageC[CarthageC$SaliniteTH == 1L, ]
  CarthagePsalted <- CarthageP[CarthageP$SaliniteSE == 1L, ]

  nearest_pp <- try(sf::st_nearest_feature(OccSL_L93, CarthagePsalted))
  water_dist_polyg_salted <- st_distance(OccSL_L93,
    CarthagePsalted[nearest_pp, ],
    by_element = TRUE
  )
  OccSL_L93$water_dist_polyg_salted <- water_dist_polyg_salted

  nearest_lp <- try(sf::st_nearest_feature(OccSL_L93, CarthageCsalted))
  water_dist_line_salted <- st_distance(OccSL_L93,
    CarthageCsalted[nearest_lp, ],
    by_element = TRUE
  )
  OccSL_L93$water_dist_line_salted <- water_dist_line_salted

  OccSL_L93$SpWDsalted <- with(
    OccSL_L93,
    pmin(water_dist_line_salted, water_dist_polyg_salted)
  )

  ##########################################
  ########         Larges         ##########
  ##########################################

  CarthageClarge <- CarthageC[CarthageC$ClasseLarg %in% c("15_50", "50_250", "250_1250", "1250"), ]
  CarthageP$surf <- units::drop_units(CarthageP$surf)
  CarthagePlarge <- CarthageP[CarthageP$surf >= 10000L, ]

  nearest_pp <- try(sf::st_nearest_feature(OccSL_L93, CarthagePlarge))
  water_dist_polyg_large <- st_distance(OccSL_L93,
    CarthagePlarge[nearest_pp, ],
    by_element = TRUE
  )
  OccSL_L93$water_dist_polyg_large <- water_dist_polyg_large

  nearest_lp <- try(sf::st_nearest_feature(OccSL_L93, CarthageClarge))
  water_dist_line_large <- st_distance(OccSL_L93,
    CarthageClarge[nearest_lp, ],
    by_element = TRUE
  )
  OccSL_L93$water_dist_line_large <- water_dist_line_large

  OccSL_L93$SpWDlarge <- with(
    OccSL_L93,
    pmin(water_dist_line_large, water_dist_polyg_large)
  )

  ######################################################################
  #################### Write ###########################################
  ######################################################################

  OccSL_WGS84 <- OccSL_L93 |>
    st_transform(4326L) # back transform to WGS84

  OccSL_ARajouter <- subset(OccSL_L93,
    select = grepl("Sp", names(OccSL_L93), fixed = TRUE)
  )

  Carthage <- data.frame(cbind(
    st_coordinates(OccSL_WGS84),
    as.data.frame(OccSL_ARajouter)
  ))

  Carthage <- Carthage |>
    st_drop_geometry() |>
    select(!geometry)

  NewName <- paste0(points, "_Carthage.csv")

  fwrite(Carthage, NewName)
}
