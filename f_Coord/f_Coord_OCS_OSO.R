print("load OCS OSO")

Coord_OCS_OSO <- function(points, names_coord, bs, bm, layer) {
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
    OccSL <- read.csv(paste0(points, ".csv"))
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_L93 <- OccSL %>%
      sf::st_transform(2154)
  }

  OccSL_L93$year <- sapply(strsplit(OccSL_L93$Nuit, "-"), "[", 1)
  unique_years <- unique(OccSL_L93$year)

  CoordH <- names_coord

  BuffersSmall <- bs
  BufferMedium <- bm

  ocs_files <- list.files(folder_OCS,
    recursive = TRUE,
    pattern = "tif$",
    full.names = TRUE
  )

  nuits_uniques <- unique(OccSL_L93$Nuit)
  tableaux_m <- list()
  tableaux_s <- list()

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

  options(dplyr..summarise.inform = FALSE) # to quiet the message produced by the sumarize function below

  for (year in unique_years) {
    ## nuit <- as.character(nuit)
    ## print(nuit)
    print(paste0("Treating year : ", year))
    tableau_year <- OccSL_L93[OccSL_L93$year == year, ]
    ## annee <- as.integer(strsplit(nuit, "-")[[1]][1])

    # Determine which clc year is closest
    ocs_file <- ocs_files[which.min(abs(ocs_annees - as.integer(year)))]
    OCS <- terra::rast(ocs_file)

    # create a buffer around the points
    tableau_BM <- sf::st_buffer(tableau_year, bm) %>%
      sf::st_transform(2154)
    tableau_BS <- sf::st_buffer(tableau_year, bs) %>%
      sf::st_transform(2154)

    # Extract values in medium buffer
    landcov_fracs_Medium <- exactextractr::exact_extract(OCS, tableau_BM, function(df) {
      df %>%
        dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
        dplyr::group_by(FID, value) %>%
        dplyr::summarize(freq = sum(frac_total))
    }, summarize_df = TRUE, include_cols = "FID", progress = FALSE)

    # Append medium buffer list
    tableaux_m <- rlist::list.append(tableaux_m, landcov_fracs_Medium)

    # Extract values in large buffer
    landcov_fracs_Small <- exactextractr::exact_extract(OCS, tableau_BS, function(df) {
      df %>%
        dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
        dplyr::group_by(FID, value) %>%
        dplyr::summarize(freq = sum(frac_total))
    }, summarize_df = TRUE, include_cols = "FID", progress = FALSE)

    # Append large buffer list
    tableaux_s <- rlist::list.append(tableaux_s, landcov_fracs_Small)
  }
  # Concatenate lists of tibbles
  tableaux_m_bind <- do.call("rbind", tableaux_m)
  tableaux_s_bind <- do.call("rbind", tableaux_s)

  #  Pivot tibbles and rename columns
  landcov_fracs_Medium_pivot <- tableaux_m_bind %>%
    tidyr::pivot_wider(names_from = "value", values_from = "freq") %>% # pivot to use CLC values as column names
    dplyr::rename_with(~ paste0("SpHOCS", ., "M"), -FID) %>%
    replace(is.na(.), 0)

  landcov_fracs_Small_pivot <- tableaux_s_bind %>%
    tidyr::pivot_wider(names_from = "value", values_from = "freq") %>% # pivot to use CLC values as column names
    dplyr::rename_with(~ paste0("SpHOCS", ., "S"), -FID) %>%
    replace(is.na(.), 0)

  HabufPropT_Tot <- dplyr::inner_join(landcov_fracs_Medium_pivot,
    landcov_fracs_Small_pivot,
    by = c("FID")
  ) %>%
    as.data.frame()

  OccSL_ARajouter <- dplyr::left_join(OccSL, HabufPropT_Tot) %>%
    as.data.frame() %>%
    dplyr::select(!c(FID, geometry))


  # colnames(OccSL_ARajouter)[colnames(OccSL_ARajouter) == 'latitude'] = "Y"
  # colnames(OccSL_ARajouter)[colnames(OccSL_ARajouter) == 'longitude'] = "X"

  if (opt$mode == "predict") {
    year <- substr(date_pred, 1, 4)
    data.table::fwrite(OccSL_ARajouter, paste0(FOccSL, "_", year, "_OCSraster.csv"))
  } else {
    data.table::fwrite(OccSL_ARajouter, paste0(FOccSL, "_OCSraster.csv"))
  }
}
