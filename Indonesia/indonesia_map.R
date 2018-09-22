#####################################################################################################################
## Indonesia Map
## 
## DATE CREATED: 9/22/2018
## AUTHOR: Paul Carvalho  
##
#####################################################################################################################

#____________________________________________________________________________________________________________________
#### Directories ####
#____________________________________________________________________________________________________________________


#____________________________________________________________________________________________________________________
#### Libraries ####
#____________________________________________________________________________________________________________________
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

#____________________________________________________________________________________________________________________
#### Functions ####
#____________________________________________________________________________________________________________________
# Condense rows with same values in all columns to a single row
# Input:  dataframe
# Output: condensed data frame
comb.rows <- function(df){
  # get number of columns
  num.col <- length(names(df))
  # enter first row
  out.df  <- df[1,]
  for(i in 2:length(df[,1])){ # rows
    flag = 0
    for(j in 1:num.col){ # columns
      if(df[i,j] != df[i-1,j]){
        flag = 1
      }
    }
    if(flag == 1){
      out.df <- rbind(out.df, df[i,])  
    }
  }
  rownames(out.df) <- c()
  return(out.df)
}

#____________________________________________________________________________________________________________________
#### Import data ####
#____________________________________________________________________________________________________________________

#____________________________________________________________________________________________________________________
#### Map Raja Ampat sites ####
#____________________________________________________________________________________________________________________
# Get unique sites and coordinates
site.df <- data.frame(site.ID   = fish.data$site.ID,
                      site.name = fish.data$site.name,
                      lon       = fish.data$lon,
                      lat       = fish.data$lat)
# Condense data and remove repeated rows
site.df <- comb.rows(site.df)

# Convert coordinates to numeric values
site.df$lon <- as.numeric(levels(site.df$lon))[site.df$lon]
site.df$lat <- as.numeric(levels(site.df$lat))[site.df$lat]

# Sorong and Waisai coordinates
loc.df <- data.frame(lat = c(-0.871, -0.4),
                     lon = c(131.254, 130.821))

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
map.plot$layers[2][[1]]$geom_params$raster <- gsub("#000000", "#AFB0B1", map.plot$layers[2][[1]]$geom_params$raster) # change the hexadecimal color of the ocean in the raster
# unique(map.plot$layers[2][[1]]$geom_params$raster)

# Create seperate map of Indonesia
map      <- get_map(location=c(lon=118, lat=10), zoom=4, 
                    maptype="toner-background", source="stamen", crop=TRUE)
map.plot <- ggmap(map) +
  # geom_point(data=site.df, aes(x=lon, y=lat),
  #            size=3, alpha=1, colour="red") +
  labs(x="", y="")
# map.plot$layers[2][[1]]$geom_params$raster <- gsub("#000000", "#AFB0B1", map.plot$layers[2][[1]]$geom_params$raster)
map.plot
