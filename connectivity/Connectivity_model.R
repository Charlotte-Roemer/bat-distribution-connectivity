source("../variables.R")
library(data.table)
library(tidyverse)
library(raster)
library(sf)
library(gdistance)
# library(geosphere)

# library(future)
# library(future.apply)

# plan(multicore, workers = 16)
# ou :
# plan(multisession, workers = 16)

## Script is called with Rscript and options :
option_list <- list(
  optparse::make_option(c("-s", "--species"),
    type = "character", default = "Pippip",
    help = 'Set modelling species as a 6 character species code (e.g. "Pippip")'
  ),
  optparse::make_option(c("-r", "--region"),
    type = "character", default = "france_met",
    help = 'Set region of interest between "france_met" (default),
                        "europe", "idf", "french_neighbours'
  ),
  optparse::make_option(c("-t", "--threshold"),
    type = "character", default = "50",
    help = 'Set sorting threshold between values : "0", "50", "90" and "weighted'
  ),
  optparse::make_option(c("-v", "--variableselection"),
    type = "character", default = "None",
    help = 'Choose which variable selection you want to make between values : "None", "VSURF", "indisp", "PCA", "PCAdecomp".'
  ),
  optparse::make_option(c("-d", "--date"),
    type = "character",
    help = "Necessary : pass date when script is run with $(date +%Y-%m-%d)"
  ),
  optparse::make_option(c("--data_sel"),
    type = "character", default = "all",
    help = "Do you reduce data by the pixel (no = all, yes = median)"
  ),
  optparse::make_option(c("--acti"),
    type = "character", default = "nb_contacts",
    help = "Which value do you want to predict ? nbcontacts, acticlass"
  ),
  optparse::make_option(c("--theta"),
    type = "numeric", default = "15",
    help = "Which value for theta ? Between 0 and 20"
  ),
  optparse::make_option(c("--season"),
    type = "numeric", default = "spring",
    help = "Which season to process ? spring or autumn"
  )
)

# THETA = 15
N_paths <- 500 # number of pathways to calculate --> all paths do not suceed so choose much more than the target

# Parse options to opt object
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)

# Directory
# Name = paste0("RFspat_VC", opt$threshold, "_",  opt$date, ".*", "_noSpace_", opt$data_sel, "_", opt$acti, "_", opt$variableselection) # RFspat_VC90_2026-05-04_noSpace_all_acticlass_None
BaseDir <- paste0("VC", opt$threshold, "_", opt$data_sel, "_", opt$acti, "_", opt$variableselection, "_season")
Directory <- file.path(data_path, "Connectivity", BaseDir)

# Directory <- "/home/charlotte/Bureau/SDM/IDF_k4/Season/Connectivity_RFspat_VC90_2026-05" # repertory with outputs from Predict_act


START <- Sys.time()

LongMigrant <- c("Nyclei", "Nycnoc", "Pipnat", "Nyclas", "Vesmur")

# Table_Dist = read_delim("/home/charlotte/Documents/Donnees vigie-chiro/Maximum_distance_seasonal_movements.csv")
Table_Dist <- read_delim("/sps/mnhn/croemer/data/observations/donnees_vigie_chiro/Maximum_distance_seasonal_movements.csv")

Sp <- opt$species
print(Sp)

DistanceMaxSp <- Table_Dist$Dist_migration_max[which(Table_Dist$Species == Sp)]

Season <- str_to_upper(opt$season)
cat(Sp, Season, "\n")

# Read Patches
print("Load Patches")
Patches <- fread(paste0(Directory, "/", Sp, "_", opt$region, "_", Season, ".csv"))

# Read Transition
print("Load transition")
land_cond_sub <- readRDS(paste0(Directory, "/", Sp, "_", opt$region, "_Year_Transition", ".rds"))
crs(land_cond_sub) <- "EPSG:2154"
print(object.size(land_cond_sub))

# # -------- Cell precalculation for passage function --------

# print("Cell precalculation")
# coords_mat <- as.matrix(
#   Patches[, c("x", "y")]
# )

# r_template <- raster(land_cond_sub)

# patch_cells <- cellFromXY(
#   r_template,
#   coords_mat
# )

# -------- Tries to calculate paths for N_paths attempts --------

for (k in 1:N_paths) {
  print(k)

  repeat {
    cat("NEW TRY\n")

    # -------- Draw random points --------

    idx_origin <- sample.int(nrow(coords_mat), 1)
    idx_goal <- sample.int(nrow(coords_mat), 1)

    origin_xy <- coords_mat[idx_origin, ]
    goal_xy <- coords_mat[idx_goal, ]

    print(origin_xy)
    print(goal_xy)

    Coord_tab <- as.data.frame(rbind(origin_xy, goal_xy))

    pt_Origin <- SpatialPoints(
      matrix(origin_xy, ncol = 2),
      proj4string = CRS("EPSG:2154")
    ) # get xy for each point

    pt_Goal <- SpatialPoints(
      matrix(goal_xy, ncol = 2),
      proj4string = CRS("EPSG:2154")
    ) # get xy for each point

    # Plot
    plot(raster(land_cond_sub))
    plot(pt_Origin, add = T)
    plot(pt_Goal, add = T)

    # -------- Check max distance --------

    x_vecteur <- goal_xy[1] - origin_xy[1]
    y_vecteur <- goal_xy[2] - origin_xy[2]

    if (x_vecteur == 0 || y_vecteur == 0) {
      next
    } # does not go further because origin and goal are probably the same point

    Distance <- sqrt(x_vecteur^2 + y_vecteur^2) / 1000

    if (Distance >= DistanceMaxSp) {
      next
    } # does not go further because distance between points is too big

    # -------- Check orientation + distance --------

    # Calculate direction angle of vector between origin and goal
    # atan2() returns an angle in radians acounting for x and y signs

    orientation <- atan2(x_vecteur, y_vecteur) * 180 / pi

    # Convert angle in degrees between 0 and 360
    # atan2() can return negative values (-180 to +180)
    # we thus add 360 and apply modulo 360
    # examples :
    #   -45  becomes 315
    #   270  stays 270

    orientation <- (orientation + 360) %% 360

    # Change angle so that in Spring, North-East = 0
    if (Season == "SPRING") {
      orientation2 <-
        ifelse(
          orientation > 45,
          orientation - 45,
          -orientation - 45 + 360
        )
    }

    # Change angle so that in Autumn, South-West = 0
    if (Season == "AUTUMN") {
      orientation2 <-
        ifelse(
          orientation > 225,
          orientation - 225,
          -orientation - 225 + 360
        )
    }

    orientation3 <-
      ifelse(
        orientation2 < 180,
        orientation2,
        -orientation2 + 360
      )

    Tolerance <- 6.6

    if (orientation3 > 90) {
      AngleDist <- log10(orientation2^2 * Distance + 1)

      if (AngleDist >= Tolerance && (Sp %in% LongMigrant)) {
        next # if wrong direction and LongDistantMigrant then stop unless short distance
      }
    }

    # -------- Calculate path --------

    pasT <- passage(
      land_cond_sub,
      pt_Origin,
      pt_Goal,
      theta = opt$theta
    )

    # Save result
    print("maximum value in result:")
    print(cellStats(pasT, max))
    if (cellStats(pasT, max) > 0) { # only if the raster contains some value (passage sometimes produces empty rasters...)
      UniqueName <- paste0(format(Sys.Date(), "%Y%m%d"), "_", Sys.getpid(), "_", k, "_", sample.int(1e9, 1))
      writeRaster(pasT, paste0(Directory, "/", Sp, "_", opt$region, "_", opt$theta, "_", Season, "_", UniqueName, ".tif"), overwrite = TRUE)
      write_csv(Coord_tab, paste0(Directory, "/", Sp, "_", opt$region, "_", opt$theta, "_", Season, "_", UniqueName, ".csv"))
    }

    break
  }

  NULL
}

END <- Sys.time()
TIMEDIFF <- END - START
TIMEDIFF

print(paste0("Connectivity done for "), Sp, " at ", opt$season)

# # Plot check
# library(maps)
# countries <- c("France")
# France <- map_data("world", region = countries)
#
# France_sf = st_as_sf(
#   France, coords = c("long", "lat"), crs=4326, remove=FALSE)  %>%
#   group_by(group) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON") %>%
#   #st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#   st_transform(2154)
#
# ggplot(France_sf) +
#   geom_sf() +
#   geom_sf(data = pt_Origin_sf, col = "blue") +
#   geom_sf(data = pt_Goal_sf, col = "green")
