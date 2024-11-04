library(blackmaRble)
library(sf)
library(terra)
library(dplyr)

setwd("/home/bbk9/Documents/asellia/Barba_2024")
vector_data <- "/home/bbk9/Nextcloud/tsevere/projet_git_BMRE/data/GIS/regions.gpkg"
region <- st_read(vector_data,
  layer = "europe"
)
source("_vars.R") # contains username, password for earthdata

credentials <- c(username, password)
bm_initialize(username, password)

get_viirs <- function(date_start, date_end, delta, data_product, variable_name, chemin,
                      custom_shape = NULL) {
 if {
    custom_shape <- if (is.character(custom_shape) == TRUE) {
      read_sf(custom_shape)
    } else {
        custom_shape
      }
  }
  if (!file.exists(chemin)) {
    dir.create(chemin)
  }
  if (!file.exists(file.path(chemin, "temp"))) {
    dir.create(file.path(chemin, "temp"))
  }
  assign("date_start", date_start, envir = .GlobalEnv)
  assign("date_end", date_end, envir = .GlobalEnv)
  assign("delta", delta, envir = .GlobalEnv)
  assign("data_product", data_product, envir = .GlobalEnv)
  assign("variable_name", variable_name, envir = .GlobalEnv)
  assign("custom_shape", custom_shape, envir = .GlobalEnv)
  tiles <- st_filter(tiles, custom_shape, .predicate = st_intersects)
  tile_index <- unique(tiles$TileID)
  httr::timeout(20)
  options(timeout = 20)
  # log <- odr_login(credentials = c(username, password), source = "earthdata")
  date_range <- seq(as.Date(date_start), as.Date(date_end),
    by = delta
  )
  year <- lubridate::year(date_range)
  month <- lubridate::month(date_range)
  day <- lubridate::day(date_range)
  yday <- lubridate::yday(date_range)
  df <- data.frame(date_range, year, month, day, yday)
  for (i in 1:nrow(df)) {
    tryCatch(
      {
        print(paste0("Downloading file ", i, " of ", nrow(df) *
          length(tile_index)))
        requete <- paste0(
          "https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/",
          data_product, "/", df$year[i], "/", ifelse(nchar(df$yday[i]) ==
            1, paste0("00", df$yday[i]), ifelse(nchar(df$yday[i]) ==
              2, paste0("0", df$yday[i]), df$yday[i])), ".csv"
        )
        csv <- read_csv(
          requete
          ,
          col_types = cols()
        )
        file <- as.character(csv$name[grep(paste(tile_index,
          collapse = "|"
        ), csv$name)])
        for (j in 1:length(file)) {
          link <- as.character(paste0(
            "https://ladsweb.modaps.eosdis.nasa.gov/opendap/RemoteResources/laads/allData/5000/",
            data_product, "/", as.character(df$year[i]),
            "/", as.character(ifelse(nchar(df$yday[i]) ==
              1, paste0("00", df$yday[i]), ifelse(nchar(df$yday[i]) ==
                2, paste0("0", df$yday[i]), df$yday[i]))),
            "/", file[j], ".nc4"
          ))
          dl_func(
            link, file.path(chemin, "temp", paste0(file[j])),
            username, password
          )
        }
      },
      error = function(e) {
        print(link)
      }
    )
  }
  lista <- list.files(path = file.path(chemin, "temp"), pattern = "VNP46A2", full.names = T)
  lista <- gtools::mixedsort(lista)
  tiles <- st_read(vector_data,
    layer = "MODIStiles"
  )

  tiles <- read_sf(system.file("extdata", "BlackMarbleTiles.shp",
    package = "blackmaRble"))

  lista <- lista[sapply(lista, file.size) > 1e+06]

  unique_dates <- list()
  for (i in 1:length(lista)) {
    date_id <- qdapRegex::ex_between(lista[i], ".", ".")[[1]][1]
    unique_dates <- append(unique_dates, unlist(date_id))
  }
  unique_dates <- unique(unique_dates)

  for (i in 1:length(unique_dates)){
    print(unique_dates[i])
    listb <- list.files(path=file.path(chemin, "temp"), pattern = unique_dates[[i]], full.names = T)
    vars <- lapply(listb, function(X) {
      rast(X, subds = variable_name)
    })
    for (j in 1:length(vars)) {
      tile_id <- qdapRegex::ex_between(sources(vars[[j]]), ".", ".")[[1]][2]
      print(tile_id)
      terra::ext(vars[[j]]) <- terra::ext(dplyr::filter(tiles, TileID == tile_id))
      terra::crs(vars[[j]]) <- "epsg:4326"
      name <- gsub(paste0(tile_id, "."), "", listb[j])
      name <- basename(name)
    }
    vars <- terra::sprc(vars)
    vars <- terra::mosaic(vars)
    terra::writeRaster(vars, file.path(chemin, paste0(name, ".tif")), overwrite = TRUE)
  }
  unlink(file.path(chemin, "temp"), recursive = TRUE)
  # lista <- lista[sapply(lista, file.size) > 1e+06]
  # vars <- lapply(lista, function(X) {
  # raster(X, varname = variable_name)
  # })
  # tiles <- read_sf(system.file("extdata", "BlackMarbleTiles.shp",
  # package = "blackmaRble"
  # ))
  # for (i in 1:length(vars)) {
  # print(i)
  # tile_id <- ex_between(lista[i], ".", ".")[[1]][2]
  # vars[[i]] <- crop(vars[[i]], extent(filter(tiles, TileID ==
  # tile_id)))
  # }
  # list_extents <- lapply(vars, extent)
  # list_extents <- lapply(list_extents, paste0)
  # list_extents <- unlist(list_extents)
  # s <- split(vars, list_extents)
  # vars_stack <- lapply(s, stack)
  # if (length(vars_stack) > 1) {
  # names(vars_stack) <- NULL
  # vars_stack <- do.call(raster::merge, vars_stack)
  # vars_stack <- stack(vars_stack)
  # } else {
  # vars_stack <- stack(vars_stack)
  # }
  # vars_stack <- stack(crop(vars_stack, extent(custom_shape)))
  # names(vars_stack) <- date_range
  # return(vars_stack)
}

chemin <- "/home/bbk9/Bureau/barba/data/viirs"

output <- get_viirs(date_start = "2014-01-01",
  date_end = "2014-01-03",
  delta = "days",
  data_product = "VNP46A2",
  variable_name = "//_HDFEOS_GRIDS_VNP_Grid_DNB_Data_Fields_Gap_Filled_DNB_BRDF-Corrected_NTL", 
  chemin, 
  custom_shape = region)
output



basename(name)

unlist(vars)
var <- list()

test <- for (i in 1:length(vars)){
  var <- base::append(var, vars[[i]])
}

length(unique_dates)

length(vars)

for (date in unique_dates){
  rm(date_rasters)
  for(i in 1:length(vars)){
    date_id <- qdapRegex::ex_between(lista[i], ".", ".")[[1]][1]
    if (date_id == date){
      if(exists("date_rasters")){
        add(date_rasters) <- vars[[i]]
      }else{
      date_rasters <- c(vars[[i]])
      }
    }
  }
  mosaique <- mosaic(date_rasters)

}

sources(vars[[2]])

class(vars)

for (i in 1:length(vars)) {
  print(sources(vars[[i]]))
}


vars

filter(tiles, TileID == "h17v04")
  # list_extents <- lapply(vars, extent)
  # list_extents <- lapply(list_extents, paste0)
  # list_extents <- unlist(list_extents)
  # s <- split(vars, list_extents)
  # vars_stack <- lapply(s, stack)
  # if (length(vars_stack) > 1) {
  # names(vars_stack) <- NULL
  # vars_stack <- do.call(raster::merge, vars_stack)
  # vars_stack <- stack(vars_stack)
  # } else {
  # vars_stack <- stack(vars_stack)
  # }
  # vars_stack <- stack(crop(vars_stack, extent(custom_shape)))
  # names(vars_stack) <- date_range
  # return(vars_stack)

