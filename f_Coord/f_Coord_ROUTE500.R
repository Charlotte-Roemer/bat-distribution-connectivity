print("load routes500")

Coord_Route <- function(points, names_coord, bm, bl, folder) {
  library(sf)
  library(data.table)
  library(tidyverse)

  Sys.time()
  route_file <- list.files(folder,
    pattern = "TRONCON_ROUTE.shp",
    recursive = TRUE, full.names = TRUE
  )
  ROUTE <- st_read(route_file, options = "ENCODING=WINDOWS-1252")

  fer_file <- list.files(folder,
    pattern = "TRONCON_VOIE_FERREE.shp",
    recursive = TRUE, full.names = TRUE
  )
  FER <- st_read(fer_file, options = "ENCODING=WINDOWS-1252")

  Sys.time()

  #  # pour tests
  # points <- FCoord

  # names_coord  <- Coord_Headers
  # bs <- BS
  # bm <- BM
  # bl <- BL
  # fin variable test

  FOccSL <- points
  if (opt$mode == "predict") {
    OccSL <- read.csv(paste0(points, ".csv")) |>
      dplyr::select(c("X", "Y"))

    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL |>
      st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL |>
      st_transform(2154)
    OccSL_L93$Nuit <- date_pred

    CoordH <- names_coord
  } else {
    OccSL <- read.csv(paste0(points, ".csv")) |>
      select(names_coord)
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL |>
      st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL |>
      st_transform(2154)
    CoordH <- names_coord
  }

  BufferMedium <- bm
  BufferLarge <- bl


  ##########################################
  ##########################################
  #############  Routes    #################
  ##########################################
  ##########################################

  nearest_r <- try(sf::st_nearest_feature(OccSL_L93, ROUTE))
  road_dist <- st_distance(OccSL_L93,
    ROUTE[nearest_r, ],
    by_element = TRUE
  )

  # Write dictionary
  ClassP <- unique(ROUTE$VOCATION)
  ClassP <- ClassP[order(ClassP)]
  CPd <- data.frame(ClassP, Code = c(1:length(ClassP)))
  fwrite(CPd, "ROUTE500_dictionary.csv", sep = ";")

  OccSL_L93Re <- OccSL_L93

  for (h in 1L:length(ClassP)) {
    ROUTEP <- ROUTE[ROUTE$VOCATION == ClassP[h], ]
    print(ClassP[h])
    print(names(OccSL_L93Re))
    print(h)

    ########
    # Buffer M
    ########

    BufferM <- st_buffer(OccSL_L93, dist = BufferMedium) |>
      st_transform(st_crs(ROUTE))

    Sys.time()
    BufferM$Route_count <- st_intersects(BufferM, ROUTEP) |>
      lengths()
    Sys.time()

    # library(viridis)
    # BufferM |>
    #   st_crop(xmin=161290, xmax=211290 , ymin=6046796 , ymax=7109796) |> #zoom in some area
    #   ggplot( aes(fill=Route_count)) +
    #   geom_sf() +
    #   scale_fill_gradientn(colours=rev(magma(6)))

    SpRoute <- BufferM

    if (is.null(BufferM$Route_count)) {
      OccSL_L93Re$SpRo_M <- 0
    } else {
      PC_50 <- aggregate(SpRoute$Route_count, by = list(SpRoute$FID), FUN = sum)
      names(PC_50)[ncol(PC_50)] <- "SpRo_M"
      OccSL_L93Re <- merge(OccSL_L93Re, PC_50, by.x = "FID", by.y = "Group.1", all.x = T)
      OccSL_L93Re$SpRo_M[is.na(OccSL_L93Re$SpRo_M)] <- 0
      # spplot(OccSL_L93Re,zcol="SpRo_S",col="transparent")
    }
    names(OccSL_L93Re)[names(OccSL_L93Re) == "SpRo_M"] <- paste0("SpRo", h, "M")


    ########
    # Buffer L
    ########

    BufferL <- st_buffer(OccSL_L93, dist = BufferLarge) |>
      st_transform(st_crs(ROUTE))

    Sys.time()
    BufferL$Route_count <- st_intersects(BufferL, ROUTEP) |>
      lengths()
    Sys.time()

    # library(viridis)
    # BufferL |>
    #   st_crop(xmin=161290, xmax=211290 , ymin=6046796 , ymax=7109796) |> #zoom in some area
    #   ggplot( aes(fill=Route_count)) +
    #   geom_sf() +
    #   scale_fill_gradientn(colours=rev(magma(6)))

    SpRoute <- BufferL

    if (is.null(BufferL$Route_count)) {
      OccSL_L93Re$SpRo_L <- 0
    } else {
      PC_50 <- aggregate(SpRoute$Route_count, by = list(SpRoute$FID), FUN = sum)
      names(PC_50)[ncol(PC_50)] <- "SpRo_L"
      OccSL_L93Re <- merge(OccSL_L93Re, PC_50, by.x = "FID", by.y = "Group.1", all.x = T)
      OccSL_L93Re$SpRo_L[is.na(OccSL_L93Re$SpRo_L)] <- 0
      # spplot(OccSL_L93Re,zcol="SpRo_S",col="transparent")
    }
    names(OccSL_L93Re)[names(OccSL_L93Re) == "SpRo_L"] <- paste0("SpRo", h, "L")
  }

  cat("Roads extracted", fill = TRUE)


  road_cols <- grep("SpRo[1-4]", names(OccSL_L93Re), value = TRUE)
  road_colsL <- grep("L", road_cols, value = TRUE)
  road_colsM <- grep("M", road_cols, value = TRUE)


  cat("combining roads", fill = TRUE)

  OccSL_L93Re$SpRoadsM <- OccSL_L93Re |>
    dplyr::select(dplyr::all_of(road_colsM)) |>
    purrr::reduce(`+`)

  OccSL_L93Re$SpRoadsL <- OccSL_L93Re |>
    dplyr::select(dplyr::all_of(road_colsL)) |>
    purrr::reduce(`+`)

  cat("Roads combined", fill = TRUE)

  OccSL_L93Re <- OccSL_L93Re |>
    dplyr::select(-road_cols)

  OccSL_L93Re$SpRo_dist <- road_dist


  ##########################################
  ##########################################
  ########## Voies ferrées  #################
  ##########################################
  ##########################################


  ########
  # Buffer M
  ########

  print("FerM")

  BufferM <- st_buffer(OccSL_L93, dist = BufferMedium) |>
    st_transform(st_crs(FER))

  Sys.time()
  BufferM$Fer_count <- st_intersects(BufferM, FER) |>
    lengths()
  Sys.time()

  # library(viridis)
  # BufferM |>
  #   st_crop(xmin=161290, xmax=211290 , ymin=6046796 , ymax=7109796) |> #zoom in some area
  #   ggplot( aes(fill=Fer_count)) +
  #   geom_sf() +
  #   scale_fill_gradientn(colours=rev(magma(6)))

  SpFer <- BufferM

  if (is.null(BufferM$Fer_count)) {
    OccSL_L93Re$SpFe_M <- 0
  } else {
    PC_50 <- aggregate(SpFer$Fer_count, by = list(SpFer$FID), FUN = sum)
    names(PC_50)[ncol(PC_50)] <- "SpFe_M"
    OccSL_L93Re <- merge(OccSL_L93Re, PC_50, by.x = "FID", by.y = "Group.1", all.x = T)
    OccSL_L93Re$SpFe_M[is.na(OccSL_L93Re$SpFe_M)] <- 0
    # spplot(OccSL_L93Re,zcol="SpFe_M",col="transparent")
  }

  ########
  # Buffer L
  ########

  print("FerL")

  BufferL <- st_buffer(OccSL_L93, dist = BufferLarge) |>
    st_transform(st_crs(FER))

  Sys.time()
  BufferL$Fer_count <- st_intersects(BufferL, FER) |>
    lengths()
  Sys.time()

  # library(viridis)
  # BufferL |>
  #   st_crop(xmin=161290, xmax=211290 , ymin=6046796 , ymax=7109796) |> #zoom in some area
  #   ggplot( aes(fill=Fer_count)) +
  #   geom_sf() +
  #   scale_fill_gradientn(colours=rev(magma(6)))

  SpFer <- BufferL

  if (is.null(BufferL$Fer_count)) {
    OccSL_L93Re$SpFe_L <- 0
  } else {
    PC_50 <- aggregate(SpFer$Fer_count, by = list(SpFer$FID), FUN = sum)
    names(PC_50)[ncol(PC_50)] <- "SpFe_L"
    OccSL_L93Re <- merge(OccSL_L93Re, PC_50, by.x = "FID", by.y = "Group.1", all.x = T)
    OccSL_L93Re$SpFe_L[is.na(OccSL_L93Re$SpFe_L)] <- 0
    # spplot(OccSL_L93Re,zcol="SpFe_L",col="transparent")
  }

  ##########################################
  ##########################################
  #############  Reseaux   #################
  ##########################################
  ##########################################

  #
  OccSL_WGS84 <- OccSL_L93 |>
    st_transform(4326) # back transform to WGS84

  OccSL_ARajouter <- subset(OccSL_L93Re, select = grepl("Sp", names(OccSL_L93Re)))

  Reseau <- data.frame(cbind(
    st_coordinates(OccSL_WGS84),
    as.data.frame(OccSL_ARajouter)
  ))

  if (opt$mode == "predict") {
    Reseau$SpRo_dist <- 0
  }

  Reseau <- Reseau |>
    st_drop_geometry() |>
    select(!geometry)


  NewName <- paste0(FOccSL, "_Transports.csv")
  fwrite(Reseau, NewName)

  # coordinates(Reseau) <- CoordH

  # SelCol=sample(names(OccSL_ARajouter),1)
  # spplot(Reseau,zcol=SelCol,main=SelCol)
  # class(Reseau)
}
