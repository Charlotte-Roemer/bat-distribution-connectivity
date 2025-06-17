cat("load ecoline", fill = TRUE)

get_edge_length <- function(points, polyg, buffer) {
  if (sf::st_is_longlat(points)) {
    cli::cli_abort(message = "points layer should be in a cartographic projection")
  }
  if (st_crs(points) != st_crs(polyg)) {
    cat("Reprojecting polyg to same CRS as points.", fill = TRUE)
    polyg <- st_transform(polyg, st_crs(points))
    cli::cli_alert_success("Reprojecting polyg to same CRS as points.")
  }
  # setting "edge_id" to recover values later
  points$edge_id <- base::seq_len(base::nrow(points))
  # joining polygons to gather continuity

  cat("Combining polygons.", fill = TRUE)
  aggregated_polyg <- polyg |>
    sf::st_union() |>
    sf::st_cast("MULTILINESTRING")
  cli::cli_alert_success("Combining polygons.")
  # apply desired buffer to points
  cat("Applying buffer.", fill = TRUE)
  buffer <- sf::st_buffer(points, buffer)
  cli::cli_alert_success("Applying buffer.")
  # identifying which buffer intersect with polygs
  cat("Identifying intersections.", fill = TRUE)
  points_haies <- buffer[sf::st_intersects(buffer, aggregated_polyg, sparse = FALSE), ]
  cli::cli_alert_success("Identifying intersections.")
  # calculating edge lengths

  cat("Mesuring edges.", fill = TRUE)
  points_haies$edge_length <- points_haies |>
    sf::st_intersection(aggregated_polyg) |>
    sf::st_length() |>
    units::drop_units()
  cli::cli_alert_success("Mesuring edges.")
  # getting only id and edge_length in a dataframe
  values_with_index <- points_haies |>
    dplyr::select(c("edge_id", "edge_length")) |>
    sf::st_drop_geometry()

  # joining new data to original dataframe
  points <- dplyr::left_join(points, values_with_index, by = "edge_id")

  # setting 0s to the other features
  points[is.na(points$edge_length), ]$edge_length <- 0L
  points <- points |>
    dplyr::select(c("edge_id", "edge_length")) |>
    sf::st_drop_geometry()
  # returning desired vector
  cli::cli_alert_success("Edges mesured.")
  points
}


Coord_Ecoline <- function(points, names_coord, ecoline_vh, ecoline_vb, buffer) {
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

    OccSL$FID <- seq_len(nrow(OccSL))
    OccSL <- OccSL |>
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326L, remove = FALSE)

    OccSL_L93 <- OccSL |>
      sf::st_transform(2154L)
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


  # recuperation des donnees ecoline
  Ecoline_VH <- sf::read_sf(
    ecoline_vh
  )
  Ecoline_VB <- sf::read_sf(
    ecoline_vb
  ) # All "En service"

  ecoline_H <- get_edge_length(OccSL_L93, Ecoline_VH, buffer)
  ecoline_B <- get_edge_length(OccSL_L93, Ecoline_VB, buffer)
  OccSL_L93$Sp_ecolineh <- ecoline_H$edge_length
  OccSL_L93$Sp_ecolineb <- ecoline_B$edge_length

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

  NewName <- paste0(points, "_ecoline.csv")

  fwrite(Carthage, NewName)
}
