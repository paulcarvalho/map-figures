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

# Create base map of Louisiana
la_base <- ggplot(data=la_df, mapping=aes(x=long, y=lat)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "white")

# Plot map with counties
la_base + theme_nothing() +
  geom_polygon(data = la_county, fill = NA, color = "white") +



geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top










