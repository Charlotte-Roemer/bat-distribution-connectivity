print("load Meteo")
# points = FCoord
library(dplyr)
library(terra)
library(sf)
Coord_Meteo <- function(points, temp, prec, wind) {
  if (opt$mode == "predict") {
    # Put 0 as value for deviation from monthly mean 
    OccSL <- readr::read_delim(paste0(points, ".csv"), delim = ",") %>%
      select(c("X", "Y"))
    # OccSL$Nuit <- opt$date # no need for date since it’s all 0s
    OccSL$Spwind <- 0
    OccSL$Sptemp <- 0
    OccSL$Spprecipitations <- 0

    tab <- OccSL
  } else if (opt$mode == "train") {
    # Load nightly weather values, calculate deviation from monthly mean, add new columns and overwrite file

    # This part was done by hand via a srun (see wiki)
    # command <- paste0("python3", file.path(
    #   project_path,
    #   "data_management",
    #   "get_meteo.py" # asks weather database online
    # ), "--file", points, ".csv")
    # system(command)

    # Read table created by get_meteo.py
    OccSL <- readr::read_delim(paste0(points, "_meteo.csv"), delim = ",") %>%
      select(c("X", "Y", "Nuit", "mean_temp", "mean_wind", "total_precipitations"))
    OccSL$FID <- c(1:nrow(OccSL))

    # Transform to sf object
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)
    OccSL$Nuit <- as.character(OccSL$Nuit)

    # Get month
    OccSL$month <- sapply(strsplit(OccSL$Nuit, "-"), "[", 2)
    unique_months <- unique(OccSL$month)

    monthly_tables <- list()

    # Loading climate norms rasters
    print("Loading climate norms rasters")
    print(temp)
    temperature_norms <- terra::rast(temp)
    print(prec)
    precipitation_norms <- terra::rast(prec)
    print(wind)
    wind_norms <- terra::rast(wind)

    # Extract monthly means
    print("Extracting wind norms")
    print(class(OccSL))
    head(OccSL)
    print(wind_norms)
    print(OccSL)

    wind_norm <- terra::extract(wind_norms, OccSL, ID = FALSE)
    OccSL$wind_norm <- wind_norm[, 1]

    for (month in unique_months) {
      print(paste0("Extracting temperature and precipition means for month : ", month))
      tableau_month <- OccSL[OccSL$month == month, ]
      index <- as.integer(month)
      temp_norm <- terra::extract(temperature_norms[[index]], tableau_month, ID = FALSE)
      tableau_month$temp_norm <- temp_norm[, 1]
      precip_norm <- terra::extract(precipitation_norms[[index]], tableau_month, ID = FALSE)
      tableau_month$precip_norm <- precip_norm[, 1]
      monthly_tables <- rlist::list.append(monthly_tables, tableau_month)
    }
    tab <- do.call("rbind", monthly_tables)

    # Calculate deviation from the monthly mean (temperature, wind and precipitations)
    tab$Spprecipitations <- tab$precip_norm - tab$total_precipitations
    tab$Sptemp <- tab$temp_norm - tab$mean_temp
    tab$Spwind <- tab$wind_norm - (tab$mean_wind * 0.27777778) # converting kmh to ms

    rm(temperature_norms, precipitation_norms, wind_norms, wind_norm, tableau_month, temp_norm, precip_norm)
  }
  print(tab, n = 10, width = Inf)
  data.table::fwrite(tab, paste0(points, "_meteo.csv"))

  rm(OccSL, tab)

}
