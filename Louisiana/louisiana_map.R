#####################################################################################################################
## Louisiana Map
## 
## DATE CREATED: 10/5/2018
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
#### Louisiana State Map ####
#____________________________________________________________________________________________________________________
# Store state data
state <- map_data("state")
la_df <- subset(state, region == "louisiana")

# Store county data
counties <- map_data("county")
la_county <- subset(counties, region == "louisiana")
terrebonne <- subset(la_county, subregion == "terrebonne")

# Create base map of Louisiana
la_base <- ggplot(data=la_df, mapping=aes(x=long, y=lat, group=group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "white")

# Plot map with counties
la_base + theme_nothing() +
  geom_polygon(data = la_county, fill = NA, color = "black") +
  geom_polygon(data = terrebonne, fill="black", color="black") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top


#____________________________________________________________________________________________________________________
#### Terrebonne Parish Map ####
#____________________________________________________________________________________________________________________
# Zoomed in map of Terrebonne Parish
terre_location <- c(left=-91.3, bottom=29.0, right=-90.3, top=29.8)
map1 <- get_stamenmap(bbox=terre_location, maptype="toner-background", crop=FALSE)
ggmap(map1)









