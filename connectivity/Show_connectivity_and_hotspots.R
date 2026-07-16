
library(tidyverse)
library(sf)
library(terra)
library(viridis)
library(beepr)

Sp = "Pipnat"
DateOrigin = "SPRING"
DateGoal = "SPRING"

# Connectivity
Connectivity = terra::rast(list.files("/home/charlotte/Bureau/SDM/French_neighbours/Connectivity/VC90_all_acticlass_None_season/Stacked/", 
                                 pattern=paste0(".*", Sp, "_", DateGoal),
                                 full.names = T))


# Hotspots Origin
Hotspots_Origin = read_sf(paste0("/home/charlotte/Bureau/SDM/French_neighbours/Connectivity/VC90_all_acticlass_None_season/", 
                                 Sp, "_", DateGoal, ".csv"))
# Hotspots_Goal = read_sf(paste0("/home/charlotte/Bureau/SDM/French_neighbours/Connectivity/VC90_all_acticlass_None_season/", 
#                                Sp, "_", DateGoal, "_Goal.csv"))

# Create polygons from table for origin
for(k in 1:length(names(table(Hotspots_Origin$L2)))){
  outer_k = matrix(c(as.numeric(Hotspots_Origin$x[which(Hotspots_Origin$L1==1 & Hotspots_Origin$L2==k)]), # exterior rings
                     as.numeric(Hotspots_Origin$y[which(Hotspots_Origin$L1==1 & Hotspots_Origin$L2==k)])), 
                   nrow = nrow(Hotspots_Origin[which(Hotspots_Origin$L1==1 & Hotspots_Origin$L2==k),]), 
                   ncol = 2) 
  if(exists("list_k")){rm(list_k)}
  for (l in 1:length(names(table(Hotspots_Origin$L1[which(Hotspots_Origin$L2==k)])))){ # different holes
    hole_l = matrix(c(as.numeric(Hotspots_Origin$x[which(Hotspots_Origin$L1==l & Hotspots_Origin$L2==k)]),
                      as.numeric(Hotspots_Origin$y[which(Hotspots_Origin$L1==l & Hotspots_Origin$L2==k)])), 
                    nrow = nrow(Hotspots_Origin[which(Hotspots_Origin$L1==l & Hotspots_Origin$L2==k),]), 
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

# # Create polygons from table for goal
# for(k in 1:length(names(table(Hotspots_Goal$L2)))){
#   outer_k = matrix(c(as.numeric(Hotspots_Goal$x[which(Hotspots_Goal$L1==1 & Hotspots_Goal$L2==k)]), # exterior rings
#                      as.numeric(Hotspots_Goal$y[which(Hotspots_Goal$L1==1 & Hotspots_Goal$L2==k)])), 
#                    nrow = nrow(Hotspots_Goal[which(Hotspots_Goal$L1==1 & Hotspots_Goal$L2==k),]), 
#                    ncol = 2) 
#   if(exists("list_k")){rm(list_k)}
#   for (l in 1:length(names(table(Hotspots_Goal$L1[which(Hotspots_Goal$L2==k)])))){ # different holes
#     hole_l = matrix(c(as.numeric(Hotspots_Goal$x[which(Hotspots_Goal$L1==l & Hotspots_Goal$L2==k)]),
#                       as.numeric(Hotspots_Goal$y[which(Hotspots_Goal$L1==l & Hotspots_Goal$L2==k)])), 
#                     nrow = nrow(Hotspots_Goal[which(Hotspots_Goal$L1==l & Hotspots_Goal$L2==k),]), 
#                     ncol = 2)
#     if(exists("list_k")){
#       list_k[[l]] = hole_l
#     }else{
#       list_k = list(outer_k, hole_l) # create one list (exterior ring + holes) for each L2 feature
#     } 
#   }
#   if(exists("c_final_goal")){
#     c_final_goal = c(c_final_goal, st_polygon(list_k))
#   }else{
#     c_final_goal = st_polygon(list_k)
#   }
# }
# 
# plot(c_final_goal)

crs(Connectivity) <- st_crs(2154)$wkt # a WKT string
temp<-as.data.frame(Connectivity, xy = T)
names(temp)[which(grepl(paste0(".*", Sp, "_", DateOrigin), names(temp)))] = "layer"

# add CRS information
c_final2 = st_sfc(c_final)
c_final3 = st_as_sf(c_final2, crs = 2154)
# c_final_goal2 = st_sfc(c_final_goal)
# c_final_goal3 = st_as_sf(c_final_goal2, crs = 4326)

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
  geom_raster(data=temp, aes(x = x, y = y, fill = layer, col=NULL)) +
  geom_sf(data=c_final3, fill="#FDE725FF", colour = NA) +
  #geom_sf(data=c_final_goal3, fill="#FDE725FF", colour = NA) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_fill_viridis_c(option = "D",
                       trans = scales::pseudo_log_trans(base = 10),
                       na.value=NA,
                       name = "Index of connectivity") +
  coord_sf() +
  geom_sf(data=isection, col="white", fill = NA, size = 1)

dev.off()


# 
# png(filename=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
#                     Sp, "_", DateOrigin, ".png"),width=1600,height=1000,res=300)
# ggplot() +
#   geom_sf(data = FRANCE_sf, fill = "black") +
#   geom_sf(data=c_final3, fill="#FDE725FF", colour = NA) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),
#         axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   coord_sf()
# 
# dev.off()
# 
# png(filename=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
#                     Sp, "_", DateGoal, ".png"),width=1600,height=1000,res=300)
# ggplot() +
#   geom_sf(data = FRANCE_sf, fill = "black") +
#   geom_sf(data=c_final_goal3, fill="#FDE725FF", colour = NA) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),
#         axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   coord_sf()
# 
# dev.off()

