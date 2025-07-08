print("load Carthage")
Coord_Carthage <- function(points, names_coord, bs, bm, bl, carthagep, carthagec) {
  library(sf)
  library(data.table)
  library(tidyverse)

  #  # pour tests
  # points = FCoord
  # names_coord  <- Coord_Headers
  # bm <- BM
  # bl <- BL
  ## fin variable test

  FOccSL <- points

  if (opt$mode == "predict") {
    OccSL <- read.csv(paste0(points, ".csv")) %>%
      dplyr::select(c("X", "Y"))

    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL %>%
      sf::st_transform(2154)
    OccSL_L93$Nuit <- date_pred

    CoordH <- names_coord
  } else {
    OccSL <- read.csv(paste0(points, ".csv")) %>%
      dplyr::select(names_coord)
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL %>%
      sf::st_transform(2154)
    CoordH <- names_coord
  }


  BufferSmall <- bs
  BufferMedium <- bm
  BufferLarge <- bl

  # recuperation des donnees Carthage (eau)
  CarthageP <- sf::read_sf(dsn = dirname(Layer_Carthage_P), layer = basename(tools::file_path_sans_ext(Layer_Carthage_P)))
  CarthageC <- sf::read_sf(dsn = dirname(Layer_Carthage_C), layer = basename(tools::file_path_sans_ext(Layer_Carthage_C))) # All "En service"

  # Split result before saving?
  Split <- F
  # Start=10001
  # End=20000
  Start <- 270001
  End <- 280194
  if (Split) {
    OccSL <- OccSL[Start:(min(End, nrow(OccSL))), ]
  }

  # Write dictionary
  ClassP <- unique(CarthageP$NatureSE)
  ClassP <- ClassP[order(ClassP)]
  CPd <- data.frame(ClassP, Code = c(1:length(ClassP)))
  data.table::fwrite(CPd, "CarthageP_dictionary.csv", sep = ";")

  OccSL_L93PP <- OccSL_L93
  for (h in 1:length(ClassP))
  {
    CarthagePP <- CarthageP[CarthageP$NatureSE == ClassP[h], ]
    print(ClassP[h])

    ##########################################
    ##########################################
    ########## water surface #################
    ##########################################
    ##########################################

    ########
    # Buffer S
    ########

    print("Buffer Small")

    BufferS <- sf::st_buffer(OccSL_L93, dist = BufferSmall) %>%
      sf::st_transform(sf::st_crs(CarthageP))

    Sys.time()
    BufferS$Water_surface_count <- sf::st_intersects(BufferS, CarthagePP) %>%
      lengths()
    Sys.time()

    SpCarthagePP <- BufferS

    if (is.null(BufferS$Water_surface_count)) {
      OccSL_L93PP$SpWS_S <- 0
    } else {
      PC_50 <- aggregate(SpCarthagePP$Water_surface_count, by = list(SpCarthagePP$FID), FUN = sum)
      print("ok")
      names(PC_50)[ncol(PC_50)] <- "SpWS_S"
      OccSL_L93PP <- merge(OccSL_L93PP, PC_50, by.x = "FID", by.y = "Group.1", all.x = T)
      OccSL_L93PP$SpWS_S[is.na(OccSL_L93PP$SpWS_S)] <- 0
    }
    names(OccSL_L93PP)[names(OccSL_L93PP) == "SpWS_S"] <- paste0("SpWS", h, "S")

    ########
    # Buffer M
    ########

    print("Buffer Medium")

    BufferM <- sf::st_buffer(OccSL_L93, dist = BufferMedium) %>%
      sf::st_transform(st_crs(CarthageP))

    Sys.time()
    BufferM$Water_surface_count <- sf::st_intersects(BufferM, CarthagePP) %>%
      lengths()
    Sys.time()

    SpCarthagePP <- BufferM

    if (is.null(BufferM$Water_surface_count)) {
      OccSL_L93PP$SpWS_M <- 0
    } else {
      PC_50 <- aggregate(SpCarthagePP$Water_surface_count, by = list(SpCarthagePP$FID), FUN = sum)
      names(PC_50)[ncol(PC_50)] <- "SpWS_M"
      OccSL_L93PP <- merge(OccSL_L93PP, PC_50, by.x = "FID", by.y = "Group.1", all.x = T)
      OccSL_L93PP$SpWS_M[is.na(OccSL_L93PP$SpWS_M)] <- 0
    }
    names(OccSL_L93PP)[names(OccSL_L93PP) == "SpWS_M"] <- paste0("SpWS", h, "M")

    ########
    # Buffer L
    ########

    print("Buffer Large")

    BufferL <- sf::st_buffer(OccSL_L93, dist = BufferLarge) %>%
      sf::st_transform(sf::st_crs(CarthageP))

    Sys.time()
    BufferL$Water_surface_count <- sf::st_intersects(BufferL, CarthagePP) %>%
      lengths()
    Sys.time()

    SpCarthagePP <- BufferL

    if (is.null(BufferL$Water_surface_count)) {
      OccSL_L93PP$SpWS_L <- 0
    } else {
      PC_50 <- aggregate(SpCarthagePP$Water_surface_count, by = list(SpCarthagePP$FID), FUN = sum)
      names(PC_50)[ncol(PC_50)] <- "SpWS_L"
      OccSL_L93PP <- merge(OccSL_L93PP, PC_50, by.x = "FID", by.y = "Group.1", all.x = T)
      OccSL_L93PP$SpWS_L[is.na(OccSL_L93PP$SpWS_L)] <- 0
    }
    names(OccSL_L93PP)[names(OccSL_L93PP) == "SpWS_L"] <- paste0("SpWS", h, "L")
  }

  ##########################################
  ##########################################
  ########## water courses #################
  ##########################################
  ##########################################

  ClassC <- unique(CarthageC$Persistanc)
  ClassC <- ClassC[order(ClassC)]
  CCd <- data.frame(ClassC, Code = c(1:length(ClassC)))
  fwrite(CCd, "CarthageC_dictionary.csv", sep = ";")

  for (h in 1:length(ClassC))
  {
    CarthageCP <- CarthageC[CarthageC$Persistanc == ClassC[h], ]

    print(ClassC[h])

    ########
    # Buffer S
    ########

    print("Buffer Small")

    BufferS <- st_buffer(OccSL_L93, dist = BufferSmall) %>%
      st_transform(st_crs(CarthageCP))

    Sys.time()
    BufferS$Water_course_count <- st_intersects(BufferS, CarthageCP) %>%
      lengths()
    Sys.time()

    SpCarthagePC <- BufferS

    if (is.null(BufferS$Water_course_count)) {
      OccSL_L93PP$SpWC_S <- 0
    } else {
      PC_50 <- aggregate(SpCarthagePC$Water_course_count, by = list(SpCarthagePP$FID), FUN = sum)
      names(PC_50)[ncol(PC_50)] <- "SpWC_S"
      OccSL_L93PP <- merge(OccSL_L93PP, PC_50, by.x = "FID", by.y = "Group.1", all.x = T)
      OccSL_L93PP$SpWC_S[is.na(OccSL_L93PP$SpWC_S)] <- 0
    }
    names(OccSL_L93PP)[names(OccSL_L93PP) == "SpWC_S"] <- paste0("SpCC", h, "S")

    ########
    # Buffer M
    ########

    print("Buffer Medium")

    BufferM <- st_buffer(OccSL_L93, dist = BufferMedium) %>%
      st_transform(st_crs(CarthageCP))

    Sys.time()
    BufferM$Water_course_count <- st_intersects(BufferM, CarthageCP) %>%
      lengths()
    Sys.time()

    SpCarthagePC <- BufferM

    if (is.null(BufferM$Water_course_count)) {
      OccSL_L93PP$SpWC_M <- 0
    } else {
      PC_50 <- aggregate(SpCarthagePC$Water_course_count, by = list(SpCarthagePP$FID), FUN = sum)
      names(PC_50)[ncol(PC_50)] <- "SpWC_M"
      OccSL_L93PP <- merge(OccSL_L93PP, PC_50, by.x = "FID", by.y = "Group.1", all.x = T)
      OccSL_L93PP$SpWC_M[is.na(OccSL_L93PP$SpWC_M)] <- 0
    }
    names(OccSL_L93PP)[names(OccSL_L93PP) == "SpWC_M"] <- paste0("SpCC", h, "M")


    ########
    # Buffer L
    ########

    print("Buffer Large")

    BufferL <- st_buffer(OccSL_L93, dist = BufferLarge) %>%
      st_transform(st_crs(CarthageCP))

    Sys.time()
    BufferL$Water_course_count <- st_intersects(BufferL, CarthageCP) %>%
      lengths()
    Sys.time()

    SpCarthagePC <- BufferL

    if (is.null(BufferL$Water_course_count)) {
      OccSL_L93PP$SpWC_L <- 0
    } else {
      PC_50 <- aggregate(SpCarthagePC$Water_course_count, by = list(SpCarthagePP$FID), FUN = sum)
      names(PC_50)[ncol(PC_50)] <- "SpWC_L"
      OccSL_L93PP <- merge(OccSL_L93PP, PC_50, by.x = "FID", by.y = "Group.1", all.x = T)
      OccSL_L93PP$SpWC_L[is.na(OccSL_L93PP$SpWC_L)] <- 0
    }
    names(OccSL_L93PP)[names(OccSL_L93PP) == "SpWC_L"] <- paste0("SpCC", h, "L")
  }


  ######################################################################
  #################### Write ###########################################
  ######################################################################

  OccSL_WGS84 <- OccSL_L93 %>%
    st_transform(4326) # back transform to WGS84

  OccSL_ARajouter <- subset(OccSL_L93PP, select = grepl("Sp", names(OccSL_L93PP)))

  Carthage <- data.frame(cbind(
    st_coordinates(OccSL_WGS84),
    as.data.frame(OccSL_ARajouter)
  ))

  Carthage <- Carthage %>%
    st_drop_geometry() %>%
    select(!geometry)

  if (Split) {
    NewName <- paste0(FOccSL, "_Carthage_", Start, "_", End, ".csv")
  } else {
    NewName <- paste0(FOccSL, "_Carthage.csv")
  }

  fwrite(Carthage, NewName)
}
