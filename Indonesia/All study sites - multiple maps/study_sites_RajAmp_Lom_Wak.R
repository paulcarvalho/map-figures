#####################################################################################################################
## Indonesia Map - with separate maps for each study site
## 
## DATE CREATED: 9/22/2018
## AUTHOR: Paul Carvalho  
##
#####################################################################################################################

rm(list=ls())

#____________________________________________________________________________________________________________________
#### DIRECTORIES ####
#____________________________________________________________________________________________________________________
# 
setwd("~/github/map-figures/Indonesia/All study sites - multiple maps")


#____________________________________________________________________________________________________________________
#### Libraries ####
#____________________________________________________________________________________________________________________
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)


#____________________________________________________________________________________________________________________
#### Import data ####
#____________________________________________________________________________________________________________________
coord <- read_excel("site_info.xlsx")


#____________________________________________________________________________________________________________________
#### Country Map ####
#____________________________________________________________________________________________________________________
map      <- get_map(location=c(lon=118, lat=0), zoom=4, maptype="toner-background", source="stamen")
map.plot <- ggmap(map) + labs(x="", y="")
map.plot


#____________________________________________________________________________________________________________________
#### Raja Ampat Map ####
#____________________________________________________________________________________________________________________
# Filter data
raja_ampat <- coord %>% filter(region == "Raja_Ampat")

# Plot Raja Ampat without sites
map <- get_map(location=c(lon=130.55, lat=-0.5), zoom="auto", 
               maptype="toner-background", source="stamen", crop=TRUE)
map.plot <- ggmap(map) +
  labs(x="Longitude", y="Latitude") +           # label axes
  theme(panel.border = element_rect(colour="black", fill=NA, size=2)) # add a border around the plot
map.plot

# Plot Raja Ampat with sites
map <- get_map(location=c(lon=130.55, lat=-0.5), zoom="auto", 
                    maptype="toner-background", source="stamen", crop=TRUE)
map.plot <- ggmap(map) +
  geom_point(data=raja_ampat, aes(x=lon, y=lat),   # plot site coordinates
             size=2, alpha=1, colour="red") + # plot site coordinates
  labs(x="Longitude", y="Latitude") +           # label axes
  theme(panel.border = element_rect(colour="black", fill=NA, size=2)) # add a border around the plot
# map.plot$layers[2][[1]]$geom_params$raster <- gsub("#000000", "#AFB0B1", map.plot$layers[2][[1]]$geom_params$raster) # change the color of the ocean in the raster
map.plot


#____________________________________________________________________________________________________________________
#### Wakatobi Map ####
#____________________________________________________________________________________________________________________
# Filter data
wakatobi <- coord %>% filter(region == "Wakatobi")

# Plot Wakatobi without sites
map <- get_map(location=c(123.7, -5.6), zoom="auto",
               source= "stamen", maptype="toner-background", crop=FALSE)
map.plot <- ggmap(map) +
  labs(x="Longitude", y="Latitude") +
  theme(panel.border = element_rect(colour="black", fill=NA, size=2)) # add a border around the plot
map.plot

# Plot Wakatobi with sites
map <- get_map(location=c(123.7, -5.6), zoom="auto",
               source= "stamen", maptype="toner-background", crop=FALSE)
map.plot <- ggmap(map) +
  geom_point(data=wakatobi, aes(x=lon, y=lat), size=2, colour="red") +
  labs(x="Longitude", y="Latitude") +
  theme(panel.border = element_rect(colour="black", fill=NA, size=2)) # add a border around the plot
map.plot


#____________________________________________________________________________________________________________________
#### Lombok Map ####
#____________________________________________________________________________________________________________________
# Filter data
lombok <- coord %>% filter(region == "Lombok")

# Plot Lombok without sites
map <- get_map(location=c(116.2, -8.6), zoom="auto",
               source= "stamen", maptype="toner-background", crop=FALSE)
map.plot <- ggmap(map) +
  labs(x="Longitude", y="Latitude") +
  theme(panel.border = element_rect(colour="black", fill=NA, size=2)) # add a border around the plot
map.plot

# Plot Lombok with sites
map <- get_map(location=c(116.3, -8.6), zoom="auto",
               source= "stamen", maptype="toner-background", crop=TRUE)
map.plot <- ggmap(map) +
  geom_point(data=lombok, aes(x=lon, y=lat), size=2, colour="red") +
  labs(x="Longitude", y="Latitude") +
  theme(panel.border = element_rect(colour="black", fill=NA, size=2)) # add a border around the plot
map.plot
