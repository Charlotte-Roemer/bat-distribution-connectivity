# library(blackmaRble)
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(rvest)

vector_data <- "/home/tsevere/Documents/mnhn/projet_git_BMRE/data/GIS/regions.gpkg"
region <- st_read(vector_data,
  layer = "europe"
)
source("/home/tsevere/Documents/mnhn/projet_git_BMRE/bat-distribution-connectivity/_vars.R") # contains username, password for earthdata


get_vcf <- function(year_start, year_end, chemin, variable_name,
                    custom_shape = NULL) {
  custom_shape <- if (is.character(custom_shape) == TRUE) {
    read_sf(custom_shape)
  } else {
    custom_shape
  }
  temp_folder <- file.path(chemin, "temp")
  if (!file.exists(chemin)) {
    dir.create(chemin)
  }
  if (!file.exists(temp_folder)) {
    dir.create(temp_folder)
  }
  assign("year_start", year_start, envir = .GlobalEnv)
  assign("year_end", year_end, envir = .GlobalEnv)
  assign("custom_shape", custom_shape, envir = .GlobalEnv)
  assign("vcf_link", "https://e4ftl01.cr.usgs.gov/MOLT/MOD44B.061/", envir = .GlobalEnv)
  tiles <- st_read(vector_data,
    layer = "MODIStiles"
  )
  tiles <- st_filter(tiles, custom_shape, .predicate = st_intersects)
  tile_index <- unique(tiles$TileID)
  date_range <- seq(year_start, year_end)
  total <- length(tile_index) * length(date_range)
  httr::timeout(20)
  options(timeout = 20)
  folders_page <- rvest::read_html(vcf_link)
  folder_links <- html_nodes(folders_page, "a")
  for (year in date_range) {
    print(paste("Année :", year))
    folder_link <- grep(as.character(year), folder_links)
    folder <- strsplit(as.character(folder_links[folder_link]), '\\"')[[1]][2]
    files_page_link <- paste0(vcf_link, folder)
    files_page <- rvest::read_html(files_page_link)
    files_page_links <- html_nodes(files_page, "a")

    df <- as.data.frame(as.character(files_page_links))
    colnames(df) <- "links"
    df <- df %>% separate_wider_delim(links, delim = '\"', names = c("balise1", "name", "balise2"))
    df <- subset(df, grepl("hdf$", df$name), )
    for (index in tile_index) {
      filename <- subset(df, grepl(index, df$name), )$name
      dl_link <- paste0(files_page_link, filename)
      print(dl_link)
      command <- paste0(
        "wget ", "--http-user=", username, " --http-password='", password,
        "' -N ", dl_link, " -P ", temp_folder, " -nv"
      )
      # setwd(file.path(chemin, "temp"))
      system(command)

      # tryCatch(
      # dl_func(dl_link, file.path(chemin, "temp", filename),
      # username, password)

      # )
    }
  }

  lista <- list.files(path = temp_folder, full.names = T)
  lista <- gtools::mixedsort(lista)
  lista <- lista[sapply(lista, file.size) > 1e+06]

  unique_dates <- list()
  for (i in 1:length(lista)) {
    date_id <- qdapRegex::ex_between(lista[i], ".", ".")[[1]][1]
    unique_dates <- append(unique_dates, unlist(date_id))
  }
  unique_dates <- unique(unique_dates)


  for (i in 1:length(unique_dates)) {
    print(unique_dates[i])
    listb <- list.files(path = file.path(chemin, "temp"), pattern = unique_dates[[i]], full.names = T)
    vars <- lapply(listb, function(X) {
      rast(X, subds = variable_name)
    })

    for (j in 1:length(vars)) {
      tile_id <- qdapRegex::ex_between(sources(vars[[j]]), ".", ".")[[1]][2]
      # terra::ext(vars[[j]]) <- terra::ext(dplyr::filter(tiles, TileID == tile_id))
      # set correct projection to modis rasters (sinusoidal, not 4326)
      terra::crs(vars[[j]]) <- "+proj=sinu +a=6371007.181 +b=6371007.181 +units=m +type=crs"
      name <- gsub(paste0(tile_id, "."), "", listb[j])
      name <- basename(name)
    }
    vars <- terra::sprc(vars)
    vars <- terra::mosaic(vars)
    vars <- terra::classify(vars, cbind(200, 0))
    vars <- terra::project(vars, "EPSG:4326")
    terra::writeRaster(vars, file.path(chemin, paste0(name, ".tif")), overwrite = TRUE)
  }
  unlink(temp_folder, recursive = TRUE)
}

chemin <- "/home/tsevere/Documents/mnhn/data/vcftest"

output <- get_vcf(
  year_start = "2015",
  year_end = "2023",
  chemin,
  custom_shape = region,
  variable_name = "Percent_Tree_Cover"
)
