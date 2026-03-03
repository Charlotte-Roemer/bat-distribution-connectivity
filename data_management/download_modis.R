library("modisfast")
library("sf")

# récupérer les identifiants EOSDIS dans _vars.R username et password
source("/home/tsevere/Documents/mnhn/projet_git_BMRE/bat-distribution-connectivity/_vars.R")

# dossier dans lequel on veut obtenir les images finales
path <- "/home/tsevere/Documents/mnhn/data/VCF"

# définition de la région d’intérêt et du fichier où la trouver
region <- "europe"

roi <- "/home/tsevere/Documents/mnhn/projet_git_BMRE/data/GIS/regions.gpkg"

# création d’un dossier temporaire
dl_path <- file.path(path, "temp")

if (dir.exists(dl_path)) {
  cat("Download path already exists", fill = TRUE)
} else {
  dir.create(dl_path)
}

# connection au service
log <- mf_login(credentials = c(username, password))
roi <- st_read(roi,
  layer = region
)

roi <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(roi)))
roi$id <- 1


start <- "2014-01-01"
end <- "2023-12-31"

time_range <- as.Date(c(start, end))

collection <- "MOD44B.061"
# to get possible variables execute :  mf_list_variables(collection)

variables <- "Percent_Tree_Cover"

urls <- mf_get_url(
  collection = collection,
  variables = variables,
  roi = roi,
  time_range = time_range
)

downloads <- mf_download_data(
  df_to_dl = urls,
  path = dl_path,
  parallel = TRUE
)

r <- mf_import_data(
  path = dirname(downloads$destfile[1]),
  collection = collection,
  parallel = TRUE,
  proj_epsg = 4326
)


times <- terra::time(r)
years <- substr(times, 1, 4)
r <- subst(r, NA, 0)

for (i in 1:length(years)) {
  name <- paste0(collection, ".", years[i], ".", region, ".tif")
  print(name)
  chemin_raster <- file.path(path, name)
  terra::writeRaster(r[[i]], chemin_raster, overwrite = TRUE)
}

a <- readline("Do you want to remove temp downloaded files ?\n1:yes\n2:no\n")

if (!(a %in% c(1, 2))) {
  cli::cli_inform("please answer 1 or 2")
} else if (a == 1) {
  unlink(dl_path, recursive = TRUE)
} else if (a == 2) {
  cli::cli_inform(paste0("Folder ", dl_path, " and its content will stay on disk"))
}
