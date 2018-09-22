#####################################################################################################################
## Indonesia Map - with separate maps for each study site
## 
## DATE CREATED: 9/22/2018
## AUTHOR: Paul Carvalho  
##
#####################################################################################################################



#____________________________________________________________________________________________________________________
#### Libraries ####
#____________________________________________________________________________________________________________________
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)


# Create map of Indonesia
map      <- get_map(location=c(lon=118, lat=0), zoom=4, maptype="toner-background", source="stamen")
map.plot <- ggmap(map) + labs(x="", y="")
map.plot
