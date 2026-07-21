
library(tidyverse)
library(sf)
library(terra)
library(viridis)
library(beepr)

Sp = "Pipnat"
Season = "SPRING"

# Load connectivity
Connectivity = rast(list.files("/home/charlotte/Bureau/SDM/French_neighbours/Connectivity/VC90_all_acticlass_None_season/Stacked/", 
                                      pattern=paste0(".*", Sp, "_", Season),
                                      full.names = T))


# Load hotspots
Hotspots = read_sf(paste0("/home/charlotte/Bureau/SDM/French_neighbours/Connectivity/VC90_all_acticlass_None_season/", 
                          Sp, "_", Season, ".csv"))

# Create polygons from table for polygons
for(k in 1:length(names(table(Hotspots$L2)))){
  outer_k = matrix(c(as.numeric(Hotspots$x[which(Hotspots$L1==1 & Hotspots$L2==k)]), # exterior rings
                     as.numeric(Hotspots$y[which(Hotspots$L1==1 & Hotspots$L2==k)])), 
                   nrow = nrow(Hotspots[which(Hotspots$L1==1 & Hotspots$L2==k),]), 
                   ncol = 2) 
  if(exists("list_k")){rm(list_k)}
  for (l in 1:length(names(table(Hotspots$L1[which(Hotspots$L2==k)])))){ # different holes
    hole_l = matrix(c(as.numeric(Hotspots$x[which(Hotspots$L1==l & Hotspots$L2==k)]),
                      as.numeric(Hotspots$y[which(Hotspots$L1==l & Hotspots$L2==k)])), 
                    nrow = nrow(Hotspots[which(Hotspots$L1==l & Hotspots$L2==k),]), 
                    ncol = 2)
    if(exists("list_k")){
      list_k[[l]] = hole_l
    }else{
      list_k = list(outer_k, hole_l) # create one list (exterior ring + holes) for each L2 feature
    } 
  }
  if(exists("c_final")){
    c_final = c(c_final, st_polygon(list_k))
  }else{
    c_final = st_polygon(list_k)
  }
}

plot(c_final)

# Add CRS information to paths
crs(Connectivity) <- st_crs(2154)$wkt # a WKT string

# Add CRS information to polygons
c_final2 = st_sfc(c_final)
c_final3 = st_as_sf(c_final2, crs = 2154)
patches_vect <- vect(c_final3)

# Function to make paths as visible as patches
#fonction saturante : 1−e−x/λ
# Elle présente plusieurs avantages :
#les faibles valeurs sont fortement étalées ;
#les très fortes valeurs sont comprimées ;
#toutes les valeurs restent comprises entre 0 et 1 ;
#les patches peuvent naturellement être fixés à 1.
# elle produit un indice de connectivité borné entre 0 et 1, où les patches correspondent naturellement au maximum de connectivité.
lambda <- quantile(values(Connectivity), 0.9, na.rm = TRUE)
corridors2 <- 1 - exp(-Connectivity / lambda)

# Rasterise  patches with value = 1 (max of connectivity)
patches_rast <- rasterize(
  patches_vect,
  corridors2,
  field = 1
)

# Merge
corridors_final <- cover(patches_rast, corridors2)
plot(corridors_final)

# Save TIF
writeRaster(corridors_final,
            paste0("/home/charlotte/Bureau/SDM/French_neighbours/Connectivity/VC90_all_acticlass_None_season/Stacked/", Sp, "_", Season, "_Connectivity.tif"),  
                   overwrite = TRUE)


# Save PNG plot
{
  countries <- c(
    "Andorra", "Portugal", "Spain", "France", "Switzerland", "Germany",
    "Belgium", "UK", "Netherlands", "Monaco", "Luxembourg", "Italy"
  )
  
  Europe <- map_data("world", region = countries)
  Europe_sf = st_as_sf(
    Europe, coords = c("long", "lat"), crs=4326, remove=FALSE)  %>%
    group_by(group) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>% 
    st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
    st_transform(2154)
  
  # Crop to raster extent
  bbox <- st_bbox(Connectivity) %>% 
    st_as_sfc()
  
  isection <- st_intersection(Europe_sf, bbox)
  
  png(filename=paste0("/home/charlotte/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                      Sp, "_", DateOrigin, ".png"),width=1600,height=1000,res=300)
  ggplot() +
    tidyterra::geom_spatraster(data = corridors_final) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_fill_viridis_c(option = "D",
                         trans = scales::pseudo_log_trans(base = 10),
                         na.value=NA,
                         name = "Index of connectivity") +
    coord_sf() +
    geom_sf(data=isection, col="white", fill = NA, size = 0.2)
  
  dev.off()
}



