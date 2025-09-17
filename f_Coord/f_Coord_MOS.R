print("load mosraster")

Coord_MOSraster <- function(points, names_coord, bs, bm, bl, layer) {
  print("mosraster")
  print(points)
  library(data.table)
  library(dplyr)
  library(sf)
  library(exactextractr)
  library(rlist)

  #  # pour tests
  #  points = FCoord
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
    OccSL$Nuit <- date_pred
    OccSL_L93 <- OccSL %>%
      sf::st_transform(2154)

    CoordH <- names_coord
  } else {
    OccSL <- read.csv(paste0(points, ".csv"))
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL %>%
      sf::st_transform(2154)
    CoordH <- names_coord
  }

  OccSL_L93$year <- sapply(strsplit(OccSL_L93$Nuit, "-"), "[", 1)
  unique_years <- unique(OccSL_L93$year)

  # début ajout extraction nuit (à adapter)
  ## nuits_uniques <- unique(OccSL_L93$Nuit)
  tableaux_s <- list()
  tableaux_m <- list()
  tableaux_l <- list()
  mos_files <- list.files(layer,
    recursive = TRUE,
    pattern = "tif$",
    full.names = TRUE
  )


  mos_annees <- as.vector(
    as.integer(
      substring(
        sapply(
          strsplit(
            tools::file_path_sans_ext(
              basename(mos_files)
            ), "_"
          ), "[", 1
        ), 4, 7
      )
    )
  )

  print(mos_annees)
  stop()


  # extract mos habitats
  options(dplyr.summarise.inform = FALSE) # to quiet the message produced by the sumarize function below

  # test
  # OccSL_L93 <- OccSL_L93[1:50, ]

  for (year in unique_years) {
    ## nuit <- as.character(nuit)
    ## print(nuit)
    print(paste0("Treating year : ", year))
    tableau_year <- OccSL_L93[OccSL_L93$year == year, ]
    ## annee <- as.integer(strsplit(nuit, "-")[[1]][1])

    # Determine which mos year is closest
    mos_file <- mos_files[which.min(abs(mos_annees - as.integer(year)))]

    mos <- terra::rast(mos_file)
    # create a buffer around the points

    tableau_BS <- st_buffer(tableau_year, bs)
    tableau_BM <- st_buffer(tableau_year, bm)
    tableau_BL <- st_buffer(tableau_year, bl)

    # Extract values in small buffer
    landcov_fracs_Small <- exact_extract(mos, tableau_BS, function(df) {
      df %>%
        mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
        group_by(FID, value) %>%
        summarize(freq = sum(frac_total))
    }, summarize_df = TRUE, include_cols = "FID", progress = FALSE)

    # Append small buffer list
    tableaux_s <- rlist::list.append(tableaux_s, landcov_fracs_Small)


    # Extract values in medium buffer
    landcov_fracs_Medium <- exact_extract(mos, tableau_BM, function(df) {
      df %>%
        mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
        group_by(FID, value) %>%
        summarize(freq = sum(frac_total))
    }, summarize_df = TRUE, include_cols = "FID", progress = FALSE)

    # Append medium buffer list
    tableaux_m <- rlist::list.append(tableaux_m, landcov_fracs_Medium)

    # Extract values in large buffer
    landcov_fracs_Large <- exact_extract(mos, tableau_BL, function(df) {
      df %>%
        mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
        group_by(FID, value) %>%
        summarize(freq = sum(frac_total))
    }, summarize_df = TRUE, include_cols = "FID", progress = FALSE)

    # Append large buffer list
    tableaux_l <- rlist::list.append(tableaux_l, landcov_fracs_Large)
  }
  # Concatenate lists of tibbles
  tableaux_s_bind <- do.call("rbind", tableaux_s)
  tableaux_m_bind <- do.call("rbind", tableaux_m)
  tableaux_l_bind <- do.call("rbind", tableaux_l)

  landcov_fracs_Small_pivot <- tableaux_s_bind %>%
    tidyr::pivot_wider(names_from = "value", values_from = "freq") %>% # pivot to use mos values as column names
    dplyr::rename_with(~ paste0("SpMOS", ., "S"), -FID) %>%
    replace(is.na(.), 0)

  # Pivot tibbles and rename columns
  landcov_fracs_Medium_pivot <- tableaux_m_bind %>%
    tidyr::pivot_wider(names_from = "value", values_from = "freq") %>% # pivot to use mos values as column names
    dplyr::rename_with(~ paste0("SpMOS", ., "M"), -FID) %>%
    replace(is.na(.), 0)

  landcov_fracs_Large_pivot <- tableaux_l_bind %>%
    tidyr::pivot_wider(names_from = "value", values_from = "freq") %>% # pivot to use mos values as column names
    dplyr::rename_with(~ paste0("SpMOS", ., "L"), -FID) %>%
    replace(is.na(.), 0)

  HabufPropT_Tot <- dplyr::inner_join(landcov_fracs_Medium_pivot,
    landcov_fracs_Large_pivot,
    by = c("FID")
  ) %>%
    as.data.frame()

  OccSL_ARajouter <- left_join(OccSL, HabufPropT_Tot, by = c("FID")) %>%
    as.data.frame() %>%
    dplyr::select(!c(FID, geometry))


  # colnames(OccSL_ARajouter)[colnames(OccSL_ARajouter) == 'latitude'] = "Y"
  # colnames(OccSL_ARajouter)[colnames(OccSL_ARajouter) == 'longitude'] = "X"

  if (opt$mode == "predict") {
    year <- substr(date_pred, 1, 4)
    fwrite(OccSL_ARajouter, paste0(FOccSL, "_", year, "_mosraster.csv"))
  } else {
    fwrite(OccSL_ARajouter, paste0(FOccSL, "_mosraster.csv"))
  }
}
