print("load VCF")

Coord_VCF <- function(points, names_coord, bs, bm, bl, layers) {
  print("VCF")
  library(data.table)
  library(terra)
  library(sf)
  library(exactextractr)
  library(tidyverse)
  library(rlist)
  library(lubridate)


  FOccSL <- points

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
    OccSL <- read.csv(paste0(points, ".csv")) 
    print("observations loaded")
    #OccSL <- points
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL %>%
      sf::st_transform(2154)
    CoordH <- names_coord
   
  }

  OccSL_L93$year <- sapply(strsplit(OccSL_L93$Nuit, "-"), "[", 1)
  unique_years <- unique(OccSL_L93$year)

  tableaux <- list()
  vcf_files <- list.files(folder_vcf,
                          recursive = TRUE,
                          pattern = "tif$",
                          full.names = TRUE)
  vcf_years <- as.vector(
    as.integer(
      substring(
        sapply(
          strsplit(
            basename(vcf_files),
            "A"
          ),
          "[", 2
        ), 1, 4
      )
    )
  )

  for (year in unique_years){
    print(paste0("Treating year : ", year))
    tableau_year <- OccSL_L93[OccSL_L93$year == year, ]
    
    raster <- vcf_files[which.min(abs(vcf_years - as.integer(year)))]

    VCF <- terra::rast(raster)
    # create a buffer around the points
    tableau_BS <- sf::st_buffer(tableau_year, bs) %>%
      sf::st_transform(4326)
    tableau_BM <- sf::st_buffer(tableau_year, bm) %>%
      sf::st_transform(4326)
    tableau_BL <- sf::st_buffer(tableau_year, bl) %>%
      sf::st_transform(4326)

    print("Buffer Small")

    SpVCF_S_tab <- exactextractr::exact_extract(VCF, tableau_BS, "mean")
    tableau_year$SpVCF_S <- SpVCF_S_tab

    print("Buffer Medium")

    SpVCF_M_tab <- exactextractr::exact_extract(VCF, tableau_BM, "mean")
    tableau_year$SpVCF_M <- SpVCF_M_tab

    print("Buffer Large")

    SpVCF_L_tab <- exactextractr::exact_extract(VCF, tableau_BL, "mean")
    tableau_year$SpVCF_L <- SpVCF_L_tab
    tableaux <- rlist::list.append(tableaux, tableau_year)
  }

  tab <- do.call("rbind", tableaux)


  VCF <- data.frame(cbind(tab$Nuit, tab$X, tab$Y,tab$SpVCF_S, tab$SpVCF_M, tab$SpVCF_L))
  colnames(VCF) <- c('Nuit', 'X', 'Y', 'SpVCF_S', 'SpVCF_M', 'SpVCF_L')
  fwrite(VCF, paste0(FOccSL, "_VCF.csv"))

  # coordinates(ALAN) <- CoordH

  # SelCol=sample(c("SpALAN_M","SpALAN_L"),1)
  # spplot(ALAN,zcol=SelCol,main=SelCol)
}

