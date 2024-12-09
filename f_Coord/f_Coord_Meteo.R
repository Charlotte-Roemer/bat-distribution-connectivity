print("load Meteo")
# points = FCoord
library(dplyr)
library(terra)
library(sf)
Coord_Meteo <- function(points, temp, prec, wind) {
  if (opt$mode == "predict") {
    OccSL <- readr::read_delim(paste0(points, ".csv"), delim = ",") %>%
      select(c("X", "Y", "Nuit"))
    OccSL$Spwind <- 0
    OccSL$Sptemp <- 0
    OccSL$Spprecipitations <- 0

    tab <- OccSL
  } else if (opt$mode == "train") {
    # command <- paste0("python3 get_meteo.py  --file ", points, ".csv")
    # system(command)
    OccSL <- readr::read_delim(paste0(points, "_meteo.csv"), delim = ",") %>%
      select(c("X", "Y", "Nuit", "mean_temp", "mean_wind", "total_precipitations"))
    OccSL$FID <- c(1:nrow(OccSL))
    OccSL <- OccSL %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)
    OccSL$Nuit <- as.character(OccSL$Nuit)

    OccSL$month <- sapply(strsplit(OccSL$Nuit, "-"), "[", 2)
    unique_months <- unique(OccSL$month)

    monthly_tables <- list()

    ## Loading climate norms rasters
    print("Loading climate norms rasters")
    print(temp)
    temperature_norms <- terra::rast(temp)
    print(prec)
    precipitation_norms <- terra::rast(prec)
    print(wind)
    wind_norms <- terra::rast(wind)
    print("Extracting wind norms")
    print(class(OccSL))
    head(OccSL)
    print(wind_norms)
    print(OccSL)

    wind_norm <- terra::extract(wind_norms, OccSL, ID = FALSE)
    OccSL$wind_norm <- wind_norm[, 1]
    

    for (month in unique_months) {
      print(paste0("Extracting climate norms for month : ", month))
      tableau_month <- OccSL[OccSL$month == month, ]
      index <- as.integer(month)

      temp_norm <- terra::extract(temperature_norms[[index]], tableau_month, ID = FALSE)
      tableau_month$temp_norm <- temp_norm[,1]
      precip_norm <- terra::extract(precipitation_norms[[index]], tableau_month, ID = FALSE)
      tableau_month$precip_norm <- precip_norm[, 1]
      monthly_tables <- rlist::list.append(monthly_tables, tableau_month)
    }
    tab <- do.call("rbind", monthly_tables)
    tab$Spprecipitations <- tab$temp_norm - tab$total_precipitations
    tab$Sptemp <- tab$temp_norm - tab$mean_temp
    tab$Spwind <- tab$wind_norm - tab$mean_wind
  }
  print(tab, n = 10, width = Inf)
  data.table::fwrite(tab, paste0(points, "_meteo.csv"))
}
