# Consideration of altitude and slope

########## INPUTS################
# points = the name of csv, with its path -> randomized (RandXXX) or non-randomized (SysSampleXXX) sampling points OR participation points (CoordWGS84)

print("load Alti")

Coord_Alti <- function(points, names_coord, bs, bm, bl, layer) {
  library(data.table)
  # library(raster)
  library(tidyverse)
  library(sf)
  library(terra)
  library(exactextractr)

  if (opt$mode == "predict") {
    OccSL <- read.csv(paste0(points, ".csv")) %>%
      dplyr::select(c("X", "Y"))

    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL %>%
      st_transform(2154)
    OccSL_L93$Nuit <- date_pred

    CoordH <- names_coord
  } else {
    OccSL <- read.csv(paste0(points, ".csv")) %>%
      dplyr::select(names_coord)
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL %>%
      st_transform(2154)
    CoordH <- names_coord
  }

  FOccSL <- points
  CoordH <- names_coord
  BufferSmall <- bs
  BufferMedium <- bm
  BufferLarge <- bl

  asc_files <- list.files(layer,
    pattern = ".asc$", full.names = TRUE
  )

  # extraction des donnees alti
  rast.list <- list()

  print("Raster list OK")
  for (i in 1:length(asc_files)) {
    rast.list[i] <- rast(asc_files[i])
  }
  Sys.time()
  rast.list <- sprc(rast.list)
  AltiTot <- terra::mosaic(rast.list, fun = mean) # 8 min
  Sys.time()
  # plot(AltiTot)
  Sys.time()

  ####################################################
  ####################################################
  ############# Extraction altitude####################
  ####################################################
  ####################################################
  # extraction des altitudes ponctuelles

  # create a buffer around the points
  Points_BS <- st_buffer(OccSL_L93, bs) # %>%
  # st_transform(4326)
  Points_BM <- st_buffer(OccSL_L93, bm) # %>%
  # st_transform(4326)
  Points_BL <- st_buffer(OccSL_L93, bl) # %>%
  # st_transform(4326)

  #######
  # Buffer S
  #######

  Sys.time()
  SpAltiS <- exact_extract(AltiTot, Points_BS, "mean")
  SpAltiS[is.na(SpAltiS)] <- 0
  OccSL$SpAltiS <- SpAltiS

  print("BS a")
  print(summary(OccSL$SpAltiS))


  #######
  # Buffer M
  #######

  Sys.time()
  SpAltiM <- exact_extract(AltiTot, Points_BM, "mean")
  SpAltiM[is.na(SpAltiM)] <- 0
  OccSL$SpAltiM <- SpAltiM


  #######
  # Buffer L
  #######

  Sys.time()
  SpAltiL <- exact_extract(AltiTot, Points_BL, "mean")
  SpAltiL[is.na(SpAltiL)] <- 0
  OccSL$SpAltiL <- SpAltiL


  ####################################################
  ####################################################
  ############# Calcul de la SpPen#####################
  ####################################################
  ####################################################

  print("SpPen")
  # ajout de 8 points cardinaux a 75m de chaques points (N,S,E,O,NO,NE,SE,SO)


  Coord <- as.data.frame(OccSL_L93) # extraire les colonnes x, Group1 et Group2
  Coord$Group.1 <- st_coordinates(OccSL_L93)[, 1]
  Coord$Group.2 <- st_coordinates(OccSL_L93)[, 2]

  print("a")

  ListePointCard <- data.frame()
  for (k in 1:nrow(Coord)) {
    x <- c(0, 0, 0, 0, 0, 0, 0, 0)
    Group.1 <- c(Coord$Group.1[k] + 75, Coord$Group.1[k] - 75, Coord$Group.1[k] + 75, Coord$Group.1[k], Coord$Group.1[k] - 75, Coord$Group.1[k], Coord$Group.1[k] - 75, Coord$Group.1[k] + 75)
    Group.2 <- c(Coord$Group.2[k] + 75, Coord$Group.2[k] - 75, Coord$Group.2[k], Coord$Group.2[k] + 75, Coord$Group.2[k], Coord$Group.2[k] - 75, Coord$Group.2[k] + 75, Coord$Group.2[k] - 75)

    PointsCard <- data.frame(x, Group.1, Group.2)
    ListePointCard <- rbind(ListePointCard, PointsCard)
  }

  print("b")

  ListePointCard <- st_as_sf(ListePointCard, coords = c("Group.1", "Group.2"), crs = 2154, remove = FALSE)


  #######
  # Buffer S
  #######
  print("Buffer S")

  AltiListePointCard <- terra::extract(AltiTot, ListePointCard) # liste altitudes des points Cardinaux

  print("BS a")
  print(summary(AltiListePointCard))

  AltiListePointCard[is.na(AltiListePointCard)] <- 0

  print(head(AltiListePointCard))
  print(AltiListePointCard[(k - 1) * 8 + 1, 2])
  # calcul de la SpPen maximale (en degrï¿½)
  SpPenS <- vector()
  SpNorS <- vector()
  SpEasS <- vector()
  SpSumS <- vector()
  for (k in 1:(nrow(Coord))) {
    SpPenk <- max(
      atan(abs(AltiListePointCard[(k - 1) * 8 + 1, 2] - SpAltiS[k]) / 75),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 2, 2] - SpAltiS[k]) / 75),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 3, 2] - SpAltiS[k]) / 75),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 4, 2] - SpAltiS[k]) / 75),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 5, 2] - SpAltiS[k]) / 75),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 6, 2] - SpAltiS[k]) / 75),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 7, 2] - SpAltiS[k]) / 75),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 8, 2] - SpAltiS[k]) / 75)
    )
    SpNork <- atan((AltiListePointCard[(k - 1) * 8 + 4, 2] - AltiListePointCard[(k - 1) * 8 + 6, 2]) / 75 * 2)
    SpEask <- atan((AltiListePointCard[(k - 1) * 8 + 3, 2] - AltiListePointCard[(k - 1) * 8 + 5, 2]) / 75 * 2)
    SpSumk <- SpAltiS[k] - mean(AltiListePointCard[((k - 1) * 8 + 1):((k - 1) * 8 + 8), 2])
    SpPenS <- c(SpPenS, SpPenk)
    SpNorS <- c(SpNorS, SpNork)
    SpEasS <- c(SpEasS, SpEask)
    SpSumS <- c(SpSumS, SpSumk)
  }

  print("BS b")
  print(summary(SpPenS))

  OccSL$SpPenS <- SpPenS
  OccSL$SpNorS <- SpNorS
  OccSL$SpEasS <- SpEasS
  OccSL$SpSumS <- SpSumS

  #######
  # Buffer M
  #######
  print("Buffer M")

  ListePointCard <- data.frame()
  for (k in 1:nrow(Coord)) {
    x <- c(0, 0, 0, 0, 0, 0, 0, 0)
    Group.1 <- c(Coord$Group.1[k] + BufferMedium, Coord$Group.1[k] - BufferMedium, Coord$Group.1[k] + BufferMedium, Coord$Group.1[k], Coord$Group.1[k] - BufferMedium, Coord$Group.1[k], Coord$Group.1[k] - BufferMedium, Coord$Group.1[k] + BufferMedium)
    Group.2 <- c(Coord$Group.2[k] + BufferMedium, Coord$Group.2[k] - BufferMedium, Coord$Group.2[k], Coord$Group.2[k] + BufferMedium, Coord$Group.2[k], Coord$Group.2[k] - BufferMedium, Coord$Group.2[k] + BufferMedium, Coord$Group.2[k] - BufferMedium)

    PointsCard <- data.frame(x, Group.1, Group.2)
    ListePointCard <- rbind(ListePointCard, PointsCard)
  }

  ListePointCard <- st_as_sf(ListePointCard, coords = c("Group.1", "Group.2"), crs = 2154, remove = FALSE)



  AltiListePointCard <- terra::extract(AltiTot, ListePointCard)

  for (z in 1:length(AltiListePointCard))
  {
    if (is.na(AltiListePointCard[z, 2])) {
      AltiListePointCard[z, 2] <- SpAltiS[floor(z / 8) + 1]
    }
  }
  AltiListePointCard[is.na(AltiListePointCard)] <- 0

  # calcul de la SpPen maximale (en degrï¿½)
  SpPenM <- vector()
  SpNorM <- vector()
  SpEasM <- vector()
  SpSumM <- vector()

  for (k in 1:(nrow(Coord))) {
    SpPenk <- max(
      atan(abs(AltiListePointCard[(k - 1) * 8 + 1, 2] - SpAltiS[k]) / BufferMedium),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 2, 2] - SpAltiS[k]) / BufferMedium),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 3, 2] - SpAltiS[k]) / BufferMedium),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 4, 2] - SpAltiS[k]) / BufferMedium),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 5, 2] - SpAltiS[k]) / BufferMedium),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 6, 2] - SpAltiS[k]) / BufferMedium),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 7, 2] - SpAltiS[k]) / BufferMedium),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 8, 2] - SpAltiS[k]) / BufferMedium)
    )
    SpNork <- atan((AltiListePointCard[(k - 1) * 8 + 4, 2] - AltiListePointCard[(k - 1) * 8 + 6, 2]) / BufferMedium * 2)
    SpEask <- atan((AltiListePointCard[(k - 1) * 8 + 3, 2] - AltiListePointCard[(k - 1) * 8 + 5, 2]) / BufferMedium * 2)
    SpSumk <- SpAltiS[k] - mean(AltiListePointCard[((k - 1) * 8 + 1):((k - 1) * 8 + 8), 2])
    SpPenM <- c(SpPenM, SpPenk)
    SpNorM <- c(SpNorM, SpNork)
    SpEasM <- c(SpEasM, SpEask)
    SpSumM <- c(SpSumM, SpSumk)
  }
  OccSL$SpPenM <- SpPenM
  OccSL$SpNorM <- SpNorM
  OccSL$SpEasM <- SpEasM
  OccSL$SpSumM <- SpSumM


  #######
  # Buffer L
  #######
  print("Buffer L")

  ListePointCard <- data.frame()
  for (k in 1:nrow(Coord)) {
    x <- c(0, 0, 0, 0, 0, 0, 0, 0)
    Group.1 <- c(Coord$Group.1[k] + BufferLarge, Coord$Group.1[k] - BufferLarge, Coord$Group.1[k] + BufferLarge, Coord$Group.1[k], Coord$Group.1[k] - BufferLarge, Coord$Group.1[k], Coord$Group.1[k] - BufferLarge, Coord$Group.1[k] + BufferLarge)
    Group.2 <- c(Coord$Group.2[k] + BufferLarge, Coord$Group.2[k] - BufferLarge, Coord$Group.2[k], Coord$Group.2[k] + BufferLarge, Coord$Group.2[k], Coord$Group.2[k] - BufferLarge, Coord$Group.2[k] + BufferLarge, Coord$Group.2[k] - BufferLarge)

    PointsCard <- data.frame(x, Group.1, Group.2)
    ListePointCard <- rbind(ListePointCard, PointsCard)
  }

  ListePointCard <- st_as_sf(ListePointCard, coords = c("Group.1", "Group.2"), crs = 2154, remove = FALSE)


  AltiListePointCard <- terra::extract(AltiTot, ListePointCard)

  for (z in 1:length(AltiListePointCard))
  {
    if (is.na(AltiListePointCard[z, 2])) {
      AltiListePointCard[z, 2] <- SpAltiS[floor(z / 8) + 1]
    }
  }
  AltiListePointCard[is.na(AltiListePointCard)] <- 0


  # calcul de la SpPen maximale (en degrï¿½)
  SpPenL <- vector()
  SpNorL <- vector()
  SpEasL <- vector()
  SpSumL <- vector()
  for (k in 1:(nrow(Coord))) {
    SpPenk <- max(
      atan(abs(AltiListePointCard[(k - 1) * 8 + 1, 2] - SpAltiS[k]) / BufferLarge),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 2, 2] - SpAltiS[k]) / BufferLarge),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 3, 2] - SpAltiS[k]) / BufferLarge),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 4, 2] - SpAltiS[k]) / BufferLarge),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 5, 2] - SpAltiS[k]) / BufferLarge),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 6, 2] - SpAltiS[k]) / BufferLarge),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 7, 2] - SpAltiS[k]) / BufferLarge),
      atan(abs(AltiListePointCard[(k - 1) * 8 + 8, 2] - SpAltiS[k]) / BufferLarge)
    )
    SpNork <- atan((AltiListePointCard[(k - 1) * 8 + 4, 2] - AltiListePointCard[(k - 1) * 8 + 6, 2]) / BufferLarge * 2)
    SpEask <- atan((AltiListePointCard[(k - 1) * 8 + 3, 2] - AltiListePointCard[(k - 1) * 8 + 5, 2]) / BufferLarge * 2)
    SpSumk <- SpAltiS[k] - mean(AltiListePointCard[((k - 1) * 8 + 1):((k - 1) * 8 + 8), 2])
    SpPenL <- c(SpPenL, SpPenk)
    SpNorL <- c(SpNorL, SpNork)
    SpEasL <- c(SpEasL, SpEask)
    SpSumL <- c(SpSumL, SpSumk)
  }
  OccSL$SpPenL <- SpPenL
  OccSL$SpNorL <- SpNorL
  OccSL$SpEasL <- SpEasL
  OccSL$SpSumL <- SpSumL


  ################## SUITE#######################

  print("c")

  Alti <- data.frame(cbind(
    st_coordinates(OccSL), SpAltiS, SpAltiM, SpAltiL,
    SpPenS, SpPenM, SpPenL, SpNorS, SpNorM, SpNorL,
    SpEasS, SpEasM, SpEasL, SpSumS, SpSumM, SpSumL
  ))

  # test=subset(Alti,(Alti$SpPenL>1.24)&(Alti$SpAltiS<50))


  fwrite(Alti, paste0(FOccSL, "_Alti.csv"))
  print("written")
}
