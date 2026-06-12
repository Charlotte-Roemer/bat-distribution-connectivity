print("load france Land Cover")

Coord_Land_Cover <- function(points, names_coord, bm, bl, layer) {
  print("OCS OSO")
  print(points)
  library(data.table)
  library(dplyr)
  library(sf)
  library(exactextractr)
  library(rlist)

  ## pour tests
  # points <- FCoord
  # names_coord <- Coord_Headers
  # bm <- BM
  # bs <- BS
  # fin variable test
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
    OccSL <- read.csv(paste0(points, ".csv")) |>
      dplyr::select(c("X", "Y", "Nuit"))
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL %>%
      sf::st_transform(2154)
  }

  OccSL_L93$year <- sapply(strsplit(OccSL_L93$Nuit, "-"), "[", 1)
  unique_years <- unique(OccSL_L93$year)

  CoordH <- names_coord

  ocs_files <- list.files(folder_OCS,
    recursive = TRUE,
    pattern = "tif$",
    full.names = TRUE
  )

  nuits_uniques <- unique(OccSL_L93$Nuit)
  tableaux_m <- list()
  tableaux_l <- list()

  ocs_annees <- as.vector(
    as.integer(
      sapply(
        strsplit(
          tools::file_path_sans_ext(
            basename(ocs_files)
          ), "_"
        ), "[", 2
      )
    )
  )

  options(dplyr.summarise.inform = FALSE) # to quiet the message produced by the sumarize function below

  for (year in unique_years) {
    print("France)")
    print(paste0("Treating year : ", year))
    tableau_year <- OccSL_L93[OccSL_L93$year == year, ]
    ## annee <- as.integer(strsplit(nuit, "-")[[1]][1])

    # Determine which clc year is closest
    ocs_file <- ocs_files[which.min(abs(ocs_annees - as.integer(year)))]
    OCS <- terra::rast(ocs_file) # OCS OSO is already in L93

    # create a buffer around the points
    #tableau_BS <- sf::st_buffer(tableau_year, bs) %>%
    #  sf::st_transform(2154)
    tableau_BM <- sf::st_buffer(tableau_year, bm) %>%
      sf::st_transform(2154)
    tableau_BL <- sf::st_buffer(tableau_year, bl) %>%
      sf::st_transform(2154)

    rm(tableau_year)

    # Extract values in medium buffer
    landcov_fracs_Medium <- exactextractr::exact_extract(OCS, tableau_BM, function(df) {
      df %>%
        dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
        dplyr::group_by(FID, value) %>%
        dplyr::summarize(freq = sum(frac_total))
    }, summarize_df = TRUE, include_cols = "FID", progress = FALSE)

    rm(tableau_BM)

    # Append medium buffer list
    tableaux_m <- rlist::list.append(tableaux_m, landcov_fracs_Medium)
    rm(landcov_fracs_Medium)

    # Extract values in large buffer
    landcov_fracs_Large <- exactextractr::exact_extract(OCS, tableau_BL, function(df) {
      df %>%
        dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
        dplyr::group_by(FID, value) %>%
        dplyr::summarize(freq = sum(frac_total))
    }, summarize_df = TRUE, include_cols = "FID", progress = FALSE)

    rm(tableau_BL)

    # Append large buffer list
    tableaux_l <- rlist::list.append(tableaux_l, landcov_fracs_Large)
    rm(OCS, landcov_fracs_Large)
  }
  # Concatenate lists of tibbles
  tableaux_m_bind <- do.call("rbind", tableaux_m)
  tableaux_l_bind <- do.call("rbind", tableaux_l)

  rm(tableaux_m, tableaux_l)

  #  Pivot tibbles and rename columns
  landcov_fracs_Medium_pivot <- tableaux_m_bind %>%
    tidyr::pivot_wider(names_from = "value", values_from = "freq") %>% # pivot to use CLC values as column names
    dplyr::rename_with(~ paste0("SpHOCS", ., "M"), -FID) %>%
    replace(is.na(.), 0)

  rm(tableaux_m_bind)

  landcov_fracs_Large_pivot <- tableaux_l_bind %>%
    tidyr::pivot_wider(names_from = "value", values_from = "freq") %>% # pivot to use CLC values as column names
    dplyr::rename_with(~ paste0("SpHOCS", ., "S"), -FID) %>%
    replace(is.na(.), 0)

  rm(tableaux_l_bind)

  HabufPropT_Tot <- dplyr::inner_join(landcov_fracs_Medium_pivot,
    landcov_fracs_Large_pivot,
    by = c("FID")
  ) %>%
    as.data.frame()

  rm(landcov_fracs_Medium_pivot, landcov_fracs_Large_pivot)

  OccSL_ARajouter <- dplyr::left_join(OccSL, HabufPropT_Tot) %>%
    as.data.frame() %>%
    dplyr::select(!c(FID, geometry))

  rm(OccSL, HabufPropT_Tot)

  if (opt$mode == "predict") {
    year <- substr(date_pred, 1, 4)
    data.table::fwrite(OccSL_ARajouter, paste0(FOccSL, "_", year, "_OCSraster.csv"))
  } else {
    data.table::fwrite(OccSL_ARajouter, paste0(FOccSL, "_OCSraster.csv"))
  }
  rm(OccSL_ARajouter)
}
