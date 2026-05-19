

library(data.table)
library(tidyverse)
library(raster)
library(sf)
library(gdistance)
#library(geosphere)

# library(future)
# library(future.apply)

#plan(multicore, workers = 16)
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
  )
)

#THETA = 15
N_paths = 500 # number of pathways to calculate for each time gap

# Parse options to opt object
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)

# Load acoustic predictions
Name = paste0("RFspat_VC", opt$threshold, "_",  opt$date, "_noSpace_", opt$data_sel, "_", opt$acti, "_", opt$variableselection) # RFspat_VC90_2026-05-04_noSpace_all_acticlass_None
Directory = file.path(data_path, "Connectivity", Name)

#Directory <- "/home/charlotte/Bureau/SDM/IDF_k4/Season/Connectivity_RFspat_VC90_2026-05" # repertory with outputs from Predict_act


START=Sys.time()

LongMigrant = c("Nyclei", "Nycnoc", "Pipnat", "Nyclas", "Vesmur")
ListTimes = c("SPRING", "AUTUMN")

Table_Dist = read_delim("/home/charlotte/Documents/Donnees vigie-chiro/Maximum_distance_seasonal_movements.csv")

Sp = opt$species
print(Sp)

DistanceMaxSp = Table_Dist$Dist_migration_max[which(Table_Dist$Species==Sp)]

for (j in seq_along(ListTimes)) {
  
  Season = ListTimes[j]
  cat(Sp, Season, "\n")
  
  # Read Patches
  Patches = fread(paste0(Directory, "/", Sp, "_", Season, ".csv"))
  
  # Read Transition
  land_cond_sub = readRDS(paste0(Directory, "/", Sp, "_Year_Transition", ".rds"))
  crs(land_cond_sub) <- "EPSG:2154"
  
  # -------- Cell precalculation for passage function --------
  
  coords_mat <- as.matrix(
    Patches[, c("x", "y")]
  )
  
  r_template <- raster(land_cond_sub)
  
  patch_cells <- cellFromXY(
    r_template,
    coords_mat
  )
  
  # future_lapply(
    # 1:N_paths,
    # function(k) {
  for (k in 1:N_paths) {
      
      print(k)
      
      repeat {
        
        # ID_Origin <- sample(Patches$id, size=1) #draw random points (random pairs)
        # ID_Goal   <- sample(Patches$id, size=1)
        
        idx_origin <- sample.int(nrow(coords_mat), 1)
        idx_goal   <- sample.int(nrow(coords_mat), 1)
        
        origin_xy <- coords_mat[idx_origin, ]
        goal_xy   <- coords_mat[idx_goal, ]
        
        # fromCell <- patch_cells[idx_origin]
        # toCell   <- patch_cells[idx_goal]
        
        pt_Origin <- SpatialPoints(
          matrix(origin_xy, ncol=2),
          proj4string = CRS("EPSG:2154")) # get xy for each point
        
        pt_Goal <- SpatialPoints(
          matrix(goal_xy, ncol=2),
          proj4string = CRS("EPSG:2154")) # get xy for each point
        
        # Plot
        plot(raster(land_cond_sub))
        plot(pt_Origin, add=T)
        plot(pt_Goal, add=T)
        
        #coords <- rbind(coordinates(pt_Origin), coordinates(pt_Goal))
        
        # Check that the direction of the flyway corresponds to long-distance migration
        
        #   pt_Origin_v = as.data.frame(pt_Origin)
        #   names(pt_Origin_v) = c("x", "y")
        #   pt_Goal_v = as.data.frame(pt_Goal)
        #   names(pt_Goal_v) = c("x", "y")
        #   
        #   pt_Origin_sf = st_as_sf(pt_Origin_v, coords = c("x", "y"), crs=2154, remove=FALSE)
        #   pt_Goal_sf = st_as_sf(pt_Goal_v, coords = c("x", "y"), crs=2154, remove=FALSE)
        #   
        #   my_data_points=as.data.frame(rbind(st_coordinates(pt_Origin_sf), st_coordinates(pt_Goal_sf)))
        #   Distance = dist(my_data_points)[1]/1000
        #   x_vecteur = my_data_points$X[2]-my_data_points$X[1]
        #   y_vecteur = my_data_points$Y[2]-my_data_points$Y[1]
        
        # x_vecteur = coords[2,1] - coords[1,1]
        # y_vecteur = coords[2,2] - coords[1,2]
        x_vecteur <- goal_xy[1] - origin_xy[1]
        y_vecteur <- goal_xy[2] - origin_xy[2]
        
        #   if(x_vecteur == 0 | y_vecteur == 0){
        #     # does not go further because origin and goal are probably the same point
        #   }else{
        #     
        #     orientation<-vector(length=0)
        #     if (x_vecteur>0 & y_vecteur>0) { 
        #       orientation=180*(atan(y_vecteur/-x_vecteur)/pi)+90 # ok
        #     }else {
        #       if (x_vecteur<0 & y_vecteur>0) { 
        #         orientation=(180*(atan(-x_vecteur/(-y_vecteur))/pi))+360 #ok
        #       }else {
        #         if (x_vecteur<0 & y_vecteur<0) { 
        #           orientation=(180*(atan((-y_vecteur)/(x_vecteur))/pi))+270 #ok
        #         }else {
        #           if (x_vecteur>0 & y_vecteur<0) { 
        #             orientation=(180*(atan(x_vecteur/(y_vecteur))/pi))+180 #ok
        #           }
        #         }}}
        #
        #     # Radar of angle
        #     dataT = data.frame("x" = round(orientation), "y" = 10)
        #     if(dataT$x == 360){
        #       dataT$x=0
        #     }
        #     # ggplot(dataT, aes(x, y)) +
        #     #   geom_col(aes(x, y), width = 5)  +
        #     #   scale_x_continuous(breaks = seq(0, 359, by = 10), limits = c(-10, 350)) +
        #     #   coord_polar(start = -pi/18, clip = "off")
        #     
        #     Tolerance = 6.6
        #     
        #     if(Distance<(DistanceMaxSp)){ 
        #       
        #       # Change angle so that in Spring, North-East = 0 and in Autumn, South-West = 0
        #       if(Season == "SPRING"){
        #         orientation2 = ifelse(orientation>45, orientation-45, -orientation-45+360)
        #       }
        #       if(Season == "AUTUMN"){
        #         orientation2 = ifelse(orientation>225, orientation-225, -orientation-225+360)
        #       }
        #       orientation3 = ifelse(orientation2<180, orientation2, -orientation2+360)
        #       
        #       if(orientation3>90){ # if wrong direction then stop unless short distance
        #         AngleDist = log10(orientation2^2*Distance+1)
        #         if(AngleDist<Tolerance | !(Sp %in% LongMigrant)){
        if (x_vecteur == 0 || y_vecteur == 0)
          next # does not go further because origin and goal are probably the same point
        
        Distance <- sqrt(x_vecteur^2 + y_vecteur^2) / 1000
        
        if (Distance >= DistanceMaxSp)
          next # does not go further because distance between points is too big
        
        # -------- orientation --------
        
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
          orientation2 =
            ifelse(
              orientation > 45,
              orientation - 45,
              -orientation - 45 + 360
            )
        }
        
        # Change angle so that in Autumn, South-West = 0
        if (Season == "AUTUMN") {
          orientation2 =
            ifelse(
              orientation > 225,
              orientation - 225,
              -orientation - 225 + 360
            )
        }
        
        orientation3 =
          ifelse(
            orientation2 < 180,
            orientation2,
            -orientation2 + 360
          )
        
        Tolerance = 6.6
        
        if (orientation3 > 90) {
          
          AngleDist = log10(orientation2^2 * Distance + 1)
          
          if (AngleDist >= Tolerance && (Sp %in% LongMigrant)) {
            next # if wrong direction and LongDistantMigrant then stop unless short distance
          }
        }
        
        # -------- Calculate paths --------
        
        pasT <- passage(
          land_cond_sub,
          pt_Origin,
          pt_Goal,
          theta = THETA
        )
        # pasT <- passage(
        #   land_cond_sub,
        #   fromCell,
        #   toCell,
        #   theta = THETA
        # )
        
        # Save result
        if(max(pasT)>0){ # only if the raster contains some value (passage sometimes produces empty rasters...)
          UniqueName = paste0(format(Sys.Date(), "%Y%m%d"), "_", Sys.getpid(), "_", k, "_", sample.int(1e9, 1) )
          writeRaster(pasT, paste0(Directory, "/", Sp, "_", THETA, "_", UniqueName, ".tif" ), overwrite = TRUE)
        }
        
        break
      }
      
      NULL
    }#,
  #   future.seed = TRUE
  # )
}
  
  #           # Calculate paths
  #           crs(pt_Origin) <- "EPSG:2154" # enlever ?
  #           crs(pt_Goal) <- "EPSG:2154" # enlever ?
  #           crs(land_cond_sub) <- "EPSG:2154" # enlever ?
  #           pasT <- gdistance::passage(land_cond_sub, pt_Origin, pt_Goal, theta=THETA)
  #           
  #           # Save result
  #           UniqueName = paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"), "_", round(runif(1, 1, 100000000000)))
  #           raster::writeRaster(pasT, paste0(NewDir, "/", Sp, "_", THETA, "_", UniqueName, ".tif"), overwrite=TRUE)
  #           # raster::writeRaster(pasT, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/TEST",
  #           #                                  "/", Sp, "_", Date, "_", THETA, "_", UniqueName, ".tif"), overwrite=TRUE)
  #           k=k+1
  #           
  #         }else{
  #           #warning("AngleDist > Tolerance")
  #         }
  #       }
  #       # Calculate paths
  #       crs(pt_Origin) <- "EPSG:2154" # enlever ?
  #       crs(pt_Goal) <- "EPSG:2154" # enlever ?
  #       crs(land_cond_sub) <- "EPSG:2154" # enlever ?
  #       pasT <- passage(land_cond_sub, pt_Origin, pt_Goal, theta=THETA)
  #       
  #       # Save result
  #       UniqueName = paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"), "_", round(runif(1, 1, 100000000000)))
  #       raster::writeRaster(pasT, paste0(NewDir, "/", Sp, "_", THETA, "_", UniqueName, ".tif"), overwrite=TRUE)
  #       # raster::writeRaster(pasT, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/TEST",
  #       #                                  "/", Sp, "_", Date, "_", THETA, "_", UniqueName, ".tif"), overwrite=TRUE)
  #       k=k+1
  #     }else{
  #       #warning("Distance > 525 km")
  #     }
  #   }
  # }



END=Sys.time()
TIMEDIFF=END-START
TIMEDIFF

print(paste0("Connectivity done for "), Sp)

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


