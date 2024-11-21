print("load CLCraster")

Coord_CLCraster <- function(points, names_coord, bm, bl, layer) {
  print("CLCraster")
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


  BufferMedium <- bm
  BufferLarge <- bl

  # début ajout extraction nuit (à adapter)
  ## nuits_uniques <- unique(OccSL_L93$Nuit)
  tableaux_m <- list()
  tableaux_l <- list()
  clc_files <- list.files(folder_CLC,
    recursive = TRUE,
    pattern = "tif$",
    full.names = TRUE
  )


clc_annees <- as.vector(
  as.integer(
    substring(
      sapply(
        strsplit(
          tools::file_path_sans_ext(
            basename(clc_files)
          ), "_"
        ), "[", 2
      ), 4, 7
    )
  )
)


  # extract CLC habitats
  options(dplyr.summarise.inform = FALSE) # to quiet the message produced by the sumarize function below

  # test
  # OccSL_L93 <- OccSL_L93[1:50, ]

  for (year in unique_years) {
    ## nuit <- as.character(nuit)
    ## print(nuit)
    print(paste0("Treating year : ", year))
    tableau_year <- OccSL_L93[OccSL_L93$year == year, ]
    ## annee <- as.integer(strsplit(nuit, "-")[[1]][1])

    # Determine which clc year is closest
    clc_file <- clc_files[which.min(abs(clc_annees - as.integer(year)))]

    CLC <- terra::rast(clc_file)
    # create a buffer around the points
    tableau_BM <- st_buffer(tableau_year, bm) %>%
      st_transform(3035)
    tableau_BL <- st_buffer(tableau_year, bl) %>%
      st_transform(3035)

    # Extract values in medium buffer
    landcov_fracs_Medium <- exact_extract(CLC, tableau_BM, function(df) {
      df %>%
        mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
        group_by(FID, value) %>%
        summarize(freq = sum(frac_total))
    }, summarize_df = TRUE, include_cols = "FID", progress = FALSE)

    # Append medium buffer list
    tableaux_m <- rlist::list.append(tableaux_m, landcov_fracs_Medium)

    # Extract values in large buffer
    landcov_fracs_Large <- exact_extract(CLC, tableau_BL, function(df) {
      df %>%
        mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
        group_by(FID, value) %>%
        summarize(freq = sum(frac_total))
    }, summarize_df = TRUE, include_cols = "FID", progress = FALSE)

    # Append large buffer list
    tableaux_l <- rlist::list.append(tableaux_l, landcov_fracs_Large)
  }
  # Concatenate lists of tibbles
  tableaux_m_bind <- do.call("rbind", tableaux_m)
  tableaux_l_bind <- do.call("rbind", tableaux_l)
  tableaux_m_nv2 <- tableaux_m_bind
  tableaux_m_nv3 <- tableaux_m_bind
  tableaux_l_nv2 <- tableaux_l_bind
  tableaux_l_nv3 <- tableaux_l_bind

  tableaux_m_nv2$valuenv2 <- round(tableaux_m_nv2$value / 10, 0)
  nv2m <- tableaux_m_nv2 %>%
    dplyr::select(c("FID", "valuenv2", "freq")) %>%
    group_by(FID, valuenv2) %>%
    summarize(freq = sum(freq))

  tableaux_m_nv3$valuenv3 <- round(tableaux_m_nv3$value / 100, 0)
  nv3m <- tableaux_m_nv3 %>%
    dplyr::select(c("FID", "valuenv3", "freq")) %>%
    group_by(FID, valuenv3) %>%
    summarize(freq = sum(freq))

  tableaux_l_nv2$valuenv2 <- round(tableaux_l_nv2$value / 10, 0)
  nv2l <- tableaux_l_nv2 %>%
    dplyr::select(c("FID", "valuenv2", "freq")) %>%
    group_by(FID, valuenv2) %>%
    summarize(freq = sum(freq))

  tableaux_l_nv3$valuenv3 <- round(tableaux_l_nv3$value / 100, 0)
  nv3l <- tableaux_l_nv3 %>%
    dplyr::select(c("FID", "valuenv3", "freq")) %>%
    group_by(FID, valuenv3) %>%
    summarize(freq = sum(freq))



  #  Pivot tibbles and rename columns
  landcov_fracs_Medium_pivot <- tableaux_m_bind %>%
    tidyr::pivot_wider(names_from = "value", values_from = "freq") %>% # pivot to use CLC values as column names
    dplyr::rename_with(~ paste0("SpHC", ., "M"), -FID) %>%
    replace(is.na(.), 0)

  landcov_fracs_Large_pivot <- tableaux_l_bind %>%
    tidyr::pivot_wider(names_from = "value", values_from = "freq") %>% # pivot to use CLC values as column names
    dplyr::rename_with(~ paste0("SpHC", ., "L"), -FID) %>%
    replace(is.na(.), 0)

  landcov_fracs_Medium_nv2_pivot <- nv2m %>%
    tidyr::pivot_wider(names_from = "valuenv2", values_from = "freq") %>% # pivot to use CLC values as column names
    dplyr::rename_with(~ paste0("SpHC", ., "M"), -FID) %>%
    replace(is.na(.), 0)

  landcov_fracs_Large_nv2_pivot <- nv2l %>%
    tidyr::pivot_wider(names_from = "valuenv2", values_from = "freq") %>% # pivot to use CLC values as column names
    dplyr::rename_with(~ paste0("SpHC", ., "L"), -FID) %>%
    replace(is.na(.), 0)

  landcov_fracs_Medium_nv3_pivot <- nv3m %>%
    tidyr::pivot_wider(names_from = "valuenv3", values_from = "freq") %>% # pivot to use CLC values as column names
    dplyr::rename_with(~ paste0("SpHC", ., "M"), -FID) %>%
    replace(is.na(.), 0)

  landcov_fracs_Large_nv3_pivot <- nv3l %>%
    tidyr::pivot_wider(names_from = "valuenv3", values_from = "freq") %>% # pivot to use CLC values as column names
    dplyr::rename_with(~ paste0("SpHC", ., "L"), -FID) %>%
    replace(is.na(.), 0)

  HabufPropT_Tot <- dplyr::inner_join(landcov_fracs_Medium_pivot,
    landcov_fracs_Medium_nv2_pivot,
    by = c("FID")
  ) %>%
    as.data.frame()


  HabufPropT_Tot <- dplyr::inner_join(HabufPropT_Tot,
    landcov_fracs_Medium_nv3_pivot,
    by = c("FID")
  ) %>%
    as.data.frame()

  HabufPropT_Tot <- dplyr::inner_join(HabufPropT_Tot,
    landcov_fracs_Large_pivot,
    by = c("FID")
  ) %>%
    as.data.frame()

  HabufPropT_Tot <- dplyr::inner_join(HabufPropT_Tot,
    landcov_fracs_Large_nv2_pivot,
    by = c("FID")
  ) %>%
    as.data.frame()

  HabufPropT_Tot <- dplyr::inner_join(HabufPropT_Tot,
    landcov_fracs_Large_nv3_pivot,
    by = c("FID")
  ) %>%
    as.data.frame()

  OccSL_ARajouter <- left_join(OccSL, HabufPropT_Tot, by = c("FID")) %>%
    as.data.frame() %>%
    dplyr::select(!c(FID, geometry))


  # colnames(OccSL_ARajouter)[colnames(OccSL_ARajouter) == 'latitude'] = "Y"
  # colnames(OccSL_ARajouter)[colnames(OccSL_ARajouter) == 'longitude'] = "X"

  fwrite(OccSL_ARajouter, paste0(FOccSL, "_CLCraster.csv"))
}
