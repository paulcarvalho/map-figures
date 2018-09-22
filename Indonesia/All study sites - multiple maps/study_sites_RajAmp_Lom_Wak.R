#####################################################################################################################
## Indonesia Map - with separate maps for each study site
## 
## DATE CREATED: 9/22/2018
## AUTHOR: Paul Carvalho  
##
#####################################################################################################################

rm(list=ls())


#____________________________________________________________________________________________________________________
#### Libraries ####
#____________________________________________________________________________________________________________________
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)


#____________________________________________________________________________________________________________________
#### Country Map ####
#____________________________________________________________________________________________________________________
map      <- get_map(location=c(lon=118, lat=0), zoom=4, maptype="toner-background", source="stamen")
map.plot <- ggmap(map) + labs(x="", y="")
map.plot


#____________________________________________________________________________________________________________________
#### Raja Ampat Map ####
#____________________________________________________________________________________________________________________
# Plot sites on a map
map      <- get_map(location=c(lon=130.2, lat=-0.5), zoom=9, 
                    maptype="toner-background", source="stamen", crop=FALSE)
map.plot <- ggmap(map) +
  geom_point(data=site.df, aes(x=lon, y=lat),   # plot site coordinates
             size=2, alpha=1, colour="black") + # plot site coordinates
  labs(x="Longitude", y="Latitude") +           # label axes
  geom_point(data=loc.df, aes(x=lon, y=lat),    # plot city coordinates
             size=3, colour="blue", shape=18) + # plot city coordinates
  theme(panel.border = element_rect(colour="black", fill=NA, size=2)) + # add a border around the plot
  annotate('rect', xmin=131.2, ymin=-0.8, xmax=131.5, ymax=-0.6, col="black", fill=NA) + # annotate "Sorong"
  annotate('text', x=131.2, y=-0.8, label='Sorong', colour="black", size=5) +            # annotate "Sorong"
  annotate('rect', xmin=131, ymin=-0.45, xmax=131.5, ymax=-0.6, col="black", fill=NA) +  # annotate "Waisai"
  annotate('text', x=131, y=-0.45, label='Waisai', colour="black", size=5)               # annotate "Waisai"
map.plot$layers[2][[1]]$geom_params$raster <- gsub("#000000", "#AFB0B1", map.plot$layers[2][[1]]$geom_params$raster) # change the color of the ocean in the raster


#____________________________________________________________________________________________________________________
#### Wakatobi Map ####
#____________________________________________________________________________________________________________________
# Plot sites on a map
loc      <- c(123.7, -5.65)
map      <- get_map(location=loc, zoom=10, source= "stamen", maptype="toner-background", crop=FALSE)

map.plot <- ggmap(map) +
  geom_point(data=fish.data, aes(x=long, y=lat), size=2, colour="black") +
  labs(x="Longitude", y="Latitude") +
  theme(panel.border = element_rect(colour="black", fill=NA, size=2)) # add a border around the plot
map.plot$layers[2][[1]]$geom_params$raster <- gsub("#000000", "#AFB0B1", map.plot$layers[2][[1]]$geom_params$raster) # change the hexadecimal color of the ocean in the raster
# change the hexadecimal color of the ocean in the raster
map.plot




