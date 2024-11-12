print("load Meteo")
# points = FCoord
library(dplyr)
library(sf)
Coord_Meteo <- function(points) {
  if (opt$mode == "predict") {
    mode <- "predict"
    command <- paste0("python3 get_meteo.py --mode ", mode, " --date ", opt$date, " --file ", points, ".csv")
  } else {
    mode <- "train"
    command <- paste0("python3 get_meteo.py --mode ", mode, " --file ", points, ".csv")
  }

  setwd("./prep_data_chiros/scripts")

  system(command)

  setwd(loc)

  if (exists("date_pred")) {
    OccSL <- readr::read_delim(paste0(points, ".csv"), delim = ",") %>%
      dplyr::select(c("X", "Y"))
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_2000_meteo <- readr::read_delim(paste0(points_2000, "_meteo.csv"), delim = ",")
    colnames(OccSL_2000_meteo)[1] <- "FID"

    OccSL_2000_meteo$FID <- OccSL_2000_meteo + 1
    OccSL_2000_meteo <- OccSL_2000_meteo %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

    OccSL_500_meteo <- sf::st_join(OccSL, OccSL_2000_meteo, st_nearest_feature,
      left = TRUE, suffix = c("", ".y")
    ) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(-c(ends_with(".y"), FID))

    data.table::fwrite(OccSL_500_meteo, paste0(points, "_meteo.csv"))
  }
}
