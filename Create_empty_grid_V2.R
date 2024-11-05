library(optparse)
library(sf)
source("variables.R")

option_list <- list(
  optparse::make_option(c("-r", "--region"),
    type = "character", default = "france_met",
    help = "Set region you want to make a grid on (france_met, europe, idf)"
  ),
  optparse::make_option(c("-s", "--size"),
    type = "integer", default = 500,
    help = "Set regular grid size in meters"
  )
)
# This script is called from the terminal with :
# Rscript Create_emtpy_grid_V2.R -r france_met -s 500


opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)

data_loc <- data_path # dossier dans lequel
# se trouvent data(dependances_fixes, prep_data, observations), scripts

size_square <- opt$size # in meters
GridName <- paste0("SysGrid_500m_de_cote_", opt$region)
# Pathname = "C:/Users/croemer01/Documents/Donnees vigie-chiro/"

emprise <- file.path(data_loc, "GIS", "regions.gpkg")

# Load country GIS limits
# GIS_limits <- map_data("world", region = countries) # attention, these limits are not very precise
GIS_limits <- sf::st_read(dsn = emprise, layer = opt$region) %>%
  sf::st_as_sf() %>%
  sf::st_transform(2154)


# Make grid
g <- GIS_limits %>%
  sf::st_make_grid(cellsize = size_square, what = "centers", crs = 2154) %>%
  sf::st_intersection(GIS_limits)

g <- sf::st_transform(g, 4326)


coords <- g %>% sf::st_coordinates()
coords <- as.data.frame(coords)
coords$ID <- 1:nrow(coords)

# Envisager de mettre tout ça dans un gpkg :
# for (i in seq_along(names1)) {
st_write(
  coords,
  file.path(data_loc, "GIS", paste0(GridName, ".csv"))
)
# }
