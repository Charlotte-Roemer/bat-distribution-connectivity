library(sf)
library(data.table)
library(tidyverse)

# #Test
# points = FCoord
# names_coord = Coord_Headers
# bs = BS
# bm = BM
# bl = BL
# layer1 = Layer_eoliennes


########## INPUTS################
# layers : made by Charlotte ROEMER
# points = the name of csv, with its path -> randomized (RandXXX) or non-randomized (SysSampleXXX) sampling points OR participation points (CoordWGS84)
# bs,bm,bl = buffers in meters

"extraction of data"

Coord_eol <- function(points, names_coord, bm, bl, layer) {
  print("a")
  FOccSL <- points # grid points
  OccSL <- read_delim(paste0(FOccSL, ".csv")) %>%
    select(names_coord)
  OccSL$ID <- c(1:nrow(OccSL))

  OccSL <- OccSL %>%
    st_as_sf(coords = names_coord, crs = 4326, remove = FALSE) %>%
    st_transform(2154)

  BufferMedium <- bm
  BufferLarge <- bl

  print("b")

  # Read wind turbines layer
  Sys.time()
  R1 <- read_sf(layer) %>%
    st_transform(2154) # convert to L93 to intersect with grid points

  ##########################################
  ##########################################
  #############  Buffer #  #################
  ##########################################
  ##########################################

  ########
  # Buffer M
  ########

  print("d")

  BufferM <- st_buffer(OccSL, dist = BufferMedium) %>%
    st_transform(st_crs(OccSL))

  BufferM$pt_count <- st_intersects(BufferM, R1) %>%
    lengths()

  # library(viridis)
  # BufferM %>%
  #   st_crop(xmin=161290, xmax=181290 , ymin=5046796 , ymax=7109796) %>% #zoom in some area
  #   ggplot( aes(fill=pt_count)) +
  #   geom_sf() +
  #   scale_fill_gradientn(colours=rev(magma(6)))

  SpEol <- BufferM

  if (length(BufferM$pt_count) > 0) {
    Sys.time()
    PC_50 <- aggregate(SpEol$pt_count, by = list(SpEol$ID), FUN = sum)
    names(PC_50)[ncol(PC_50)] <- "SpEol_M"
    OccSL_Re <- merge(OccSL_Re, PC_50, by.x = "ID", by.y = "Group.1", all.x = TRUE)
    OccSL_Re$SpEol_M[is.na(OccSL_Re$SpEol_M)] <- 0
  } else {
    OccSL_Re$SpEol_M <- 0
  }

  ########
  # Buffer L
  ########

  print("e")

  BufferL <- st_buffer(OccSL, dist = BufferLarge) %>%
    st_transform(st_crs(R1))

  BufferL$pt_count <- st_intersects(BufferL, R1) %>%
    lengths()

  # BufferL %>%
  #   st_crop(xmin=161290, xmax=181290 , ymin=5046796 , ymax=7109796) %>% #zoom in some area
  #   ggplot( aes(fill=pt_count)) +
  #   geom_sf() +
  #   scale_fill_gradientn(colours=rev(magma(6)))

  SpEol <- BufferL

  if (length(BufferL$pt_count) > 0) {
    PC_50 <- aggregate(SpEol$pt_count, by = list(SpEol$ID), FUN = sum)
    names(PC_50)[ncol(PC_50)] <- "Sp_L"
    OccSL_Re <- merge(OccSL_Re, PC_50, by.x = "ID", by.y = "Group.1", all.x = TRUE)
    OccSL_Re$SpEol_L[is.na(OccSL_Re$SpEol_L)] <- 0
  } else {
    OccSL_Re$SpEol_L <- 0
  }

  ##########################################
  ##########################################
  #############  Reseaux   #################
  ##########################################
  ##########################################

  print("f")

  OccSL_Re_WGS84 <- OccSL_Re %>%
    st_transform(4326) # back transform to WGS84

  print("g")

  OccSL_WGS84 <- OccSL %>%
    st_transform(4326) # back transform to WGS84

  OccSL_ARajouter <- subset(OccSL_Re_WGS84,
    select = grepl(
      "Sp",
      names(OccSL_Re_WGS84)
    )
  )

  Reseau <- data.frame(cbind(
    st_coordinates(OccSL_WGS84),
    as.data.frame(OccSL_ARajouter)
  ))


  Reseau <- Reseau %>%
    dplyr::select(!c(geometry))

  NewName <- paste0(FOccSL, "_Reseau.csv")

  fwrite(Reseau, NewName)

  # coordinates(Reseau) <- CoordH

  # SelCol=sample(names(OccSL_ARajouter),1)
  # spplot(Reseau,zcol=SelCol,main=SelCol)
  # class(Reseau)
}
