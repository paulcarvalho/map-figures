#________________________________
# Raja Ampat Site Selection Map
#
# Project: USAID SHERA
# Date: May 8, 2019
#________________________________

# clean workspace
rm(list=ls())

# directories
setwd("C:/Users/pgcar/Downloads") # PC personal laptop

# libraries
library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(readxl)

# load data
main_df <- read_excel("AH site selection.xlsx", sheet=1, col_names=TRUE)
main_df$latitude <- as.numeric(main_df$latitude)
main_df$longitude <- as.numeric(main_df$longitude)

# set all mapping themes
allfiguretheme <- theme_bw() +
  theme(panel.grid.minor = element_line(colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        panel.border = element_rect(colour = "black", size=1.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(size=12, colour = "black"), # Controls size of x axis tickmark labels
        axis.title.y = element_text(size=12, colour = "black"), # Controls size of y axis tickmark labels
        legend.text = element_text(size=8),
        legend.title = element_blank(),
        legend.position = "right",
        plot.title = element_text(size=20))

outdir <- "C:/Users/pgcar/Downloads"

# Read in sf mapping files from GADM website:
indo_sf<-readRDS("gadm36_IDN_4_sf.rds")

# Change column names
names(indo_sf)[grep("^NAME_4$", names(indo_sf))]<-"Village"
names(indo_sf)[grep("^NAME_3$", names(indo_sf))]<-"Subdistrict"
names(indo_sf)[grep("^NAME_2$", names(indo_sf))]<-"District"
names(indo_sf)[grep("^NAME_1$", names(indo_sf))]<-"Region"
names(indo_sf)[grep("^NAME_0$", names(indo_sf))]<-"Country"

# AH site data
main_df_sf <- main_df
main_df_sf <- na.omit(main_df_sf)

# Set coordinates and projection model
main_df_sf <- st_as_sf(main_df_sf, coords=c("longitude","latitude"), crs="+proj=longlat +datum=WGS84")

class(indo_sf)
class(main_df_sf)

st_geometry(indo_sf)
st_geometry(main_df_sf)

indo_sf %>%
  filter(District=="Raja Ampat") %>%
  select(Region) %>%
  print(n=1)

# Plot
indo_sf %>%
  filter(Region=="Papua Barat") %>%
  select(District) %>%
  plot(graticule=TRUE, key.pos=NULL)

# Plotting Raja Ampat
ra_sf <- indo_sf[indo_sf$District=="Raja Ampat",]

p <- ggplot(data=ra_sf) +
  geom_sf() +
  allfiguretheme
print(p)

# Plot with sites
st_geometry(main_df_sf)
main_sf_utm <- st_transform(main_df_sf, "+init=epsg:23891 +units=km") # ID74 / UTM zone 51S
ra_sf_utm <- st_transform(ra_sf, "+init=epsg:23891 +units=km")

setwd(outdir)

p <- ggplot()+
  geom_sf(data=ra_sf_utm) +
  geom_sf(data=main_sf_utm)+
  allfiguretheme
print(p)

# Misool sf
misool_sf <- ra_sf[grep("Misool", ra_sf$Subdistrict),]
misool_sf_utm <- st_transform(misool_sf, "+init=epsg:23891 +units=km")
# Site info sf
main_sf_utm$Management.Rule<-as.factor(main_sf_utm$Management.Rule)
# Text sf
# Need to move some of the text to prevent overlapping text
text_df <- main_df
text_df <- na.omit(text_df)
text_df$longitude <- text_df$longitude+0.015
text_df$longitude[1] <- text_df$longitude[1] - 0.03
text_df$latitude[8] <- text_df$latitude[8] + 0.01
text_df$latitude[9] <- text_df$latitude[9] - 0.015
text_df$longitude[9] <- text_df$longitude[9] - 0.015
text_df$latitude[11] <- text_df$latitude[11] + 0.01
text_df$latitude[12] <- text_df$latitude[12] - 0.01
text_df$latitude[13] <- text_df$latitude[13] + 0.01
text_df$latitude[14] <- text_df$latitude[14] - 0.01
text_df$latitude[16] <- text_df$latitude[16] + 0.008
text_df$latitude[17] <- text_df$latitude[17] + 0.025
text_df$longitude[18] <- text_df$longitude[18] - 0.01
text_df$latitude[18] <- text_df$latitude[18] + 0.015
text_df$latitude[19] <- text_df$latitude[19] - 0.01
text_df$latitude[25] <- text_df$latitude[25] + 0.01
text_df$latitude[27] <- text_df$latitude[27] + 0.01
text_df$latitude[28] <- text_df$latitude[28] - 0.02
text_df$latitude[34] <- text_df$latitude[34] + 0.01
text_df$latitude[37] <- text_df$latitude[37] - 0.012
text_df$latitude[39] <- text_df$latitude[39] - 0.01
text_df$longitude[39] <- text_df$longitude[39] - 0.01
text_df$longitude[42] <- text_df$longitude[42] - 0.03
text_df$latitude[42] <- text_df$latitude[42] + 0.01
text_df$latitude[43] <- text_df$latitude[43] + 0.01
text_df$latitude[44] <- text_df$latitude[44] - 0.008
text_df$latitude[45] <- text_df$latitude[45] + 0.01
text_df$latitude[46] <- text_df$latitude[46] - 0.015
text_df$longitude[46] <- text_df$longitude[46] - 0.01
text_df_sf <- st_as_sf(text_df, coords=c("longitude","latitude"), crs="+proj=longlat +datum=WGS84")
text_df_utm <- st_transform(text_df_sf, "+init=epsg:23891 +units=km")
text_df_utm$Site.Number <- as.character(seq(1:50))
text_df_utm <- text_df_utm[,c(5,4)]
text_points <- sf::st_point_on_surface(text_df_utm)
text_coords <- as.data.frame(sf::st_coordinates(text_points))
text_coords$Site.Number <- text_df_utm$Site.Number 

# NOTE: sites 10 and 28 have the same coordinates
# NOTE: sites 16 and 17 have the same coordinates
# NOTE: unable to find coordinates for Wayilbatan South02
p <- ggplot()+
  geom_sf(data=misool_sf_utm) +
  geom_sf(data=main_sf_utm, aes(color=Management.Rule)) +
  geom_text(data=text_coords, aes(X, Y, label=Site.Number), colour="black", size=4) +
  allfiguretheme +
  labs(y="",x="")
print(p)


# Plot biomass and management rule
buf <- NULL
for(i in 1:length(main_sf_utm$Biomass)){
  if(main_sf_utm$Biomass[i] < 250){
    buf <- c(buf,0.5)
  } else if (main_sf_utm$Biomass[i] >= 251 & main_sf_utm$Biomass[i] < 500) {
    buf <- c(buf,1)
  } else if (main_sf_utm$Biomass[i] >= 501 & main_sf_utm$Biomass[i] < 1000) {
    buf <- c(buf,1.5)
  } else if (main_sf_utm$Biomass[i] >= 1001 & main_sf_utm$Biomass[i] < 1500) {
    buf <- c(buf,2)
  } else if (main_sf_utm$Biomass[i] >= 1501 & main_sf_utm$Biomass[i] < 2000) {
    buf <- c(buf,2.5)
  } else if (main_sf_utm$Biomass[i] >= 2001 & main_sf_utm$Biomass[i] < 2500) {
    buf <- c(buf,3)
  } else if (main_sf_utm$Biomass[i] >= 2501 & main_sf_utm$Biomass[i] < 3000) {
    buf <- c(buf,3.5)
  } else if (main_sf_utm$Biomass[i] >= 3001 & main_sf_utm$Biomass[i] < 3500) {
    buf <- c(buf,4)
  } else if (main_sf_utm$Biomass[i] > 3500) {
    buf <- c(buf,7)
  }
}
main_utm_buffer <- st_buffer(main_sf_utm, buf)
text_df <- main_df
text_df <- na.omit(text_df)
text_df$longitude[6] <- text_df$longitude[6] + 0.005
text_df$latitude[6] <- text_df$latitude[6] - 0.005
text_df$latitude[8] <- text_df$latitude[8] + 0.005
text_df$longitude[8] <- text_df$longitude[8] + 0.01
text_df$latitude[9] <- text_df$latitude[9] - 0.02
text_df$longitude[10] <- text_df$longitude[10] - 0.005
text_df$latitude[11] <- text_df$latitude[11] + 0.005
text_df$latitude[13] <- text_df$latitude[13] + 0.005
text_df$latitude[16] <- text_df$latitude[16] + 0.02
text_df$latitude[18] <- text_df$latitude[18] + 0.01
text_df$latitude[19] <- text_df$latitude[19] - 0.01
text_df$longitude[20] <- text_df$longitude[20] + 0.01
text_df$longitude[22] <- text_df$longitude[22] + 0.01
text_df$latitude[25] <- text_df$latitude[25] + 0.01
text_df$latitude[26] <- text_df$latitude[26] - 0.005
text_df$latitude[28] <- text_df$latitude[28] - 0.015
text_df$latitude[39] <- text_df$latitude[39] - 0.01
text_df$latitude[37] <- text_df$latitude[37] - 0.01
text_df$latitude[44] <- text_df$latitude[44] - 0.02
text_df$latitude[46] <- text_df$latitude[46] - 0.01
text_df$longitude[46] <- text_df$longitude[46] + 0.01
text_df$latitude[47] <- text_df$latitude[47] - 0.02
text_df$longitude[47] <- text_df$longitude[47] + 0.02
text_df_sf <- st_as_sf(text_df, coords=c("longitude","latitude"), crs="+proj=longlat +datum=WGS84")
text_df_utm <- st_transform(text_df_sf, "+init=epsg:23891 +units=km")
text_df_utm$Site.Number <- as.character(seq(1:50))
text_df_utm <- text_df_utm[,c(5,4)]
text_points <- sf::st_point_on_surface(text_df_utm)
text_coords <- as.data.frame(sf::st_coordinates(text_points))
text_coords$Site.Number <- text_df_utm$Site.Number 

p <- ggplot() +
  geom_sf(data=misool_sf_utm) +
  geom_sf(data=main_utm_buffer, aes(colour=Management.Rule), fill="transparent") +
  geom_text(data=text_coords, aes(X, Y, label=Site.Number), color="black", size=3) +
  allfiguretheme +
  labs(x="",y="")
print(p)



