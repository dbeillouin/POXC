#POXC metaanalysis based on 2.2024 POXC file
#Authors: Cécile Chéron-Bessou, Damien Beillouin, Alexis Thoumazeau, Lydie Chapuis-Lardy, Tiphaine Chevallier, Julien Demenois, Paul N Nelson

#########################################################################################
#1. World map + Whittaker biomes with POXC data points----


## 1.0 Needed librairies----

# Needed packages to upload and launch
# install.packages("ggplot2")
# install.packages("maps")
# install.packages("mapdata")

library(ggplot2)
library(maps)
library(mapdata)
library(ggnewscale)
library(raster)
library(sp)

##1.1 Creation of the world map with the number of experiments per country translated into colour classes----


# World map with country names and coordinates
world_map <- map_data("world")

# Adding the number of experiments per country based on their coordinates

# Counting the number of experiments per combinatino of x-y coordinates (more than one experiment is possible per combination)
NB_Points<-POXC_map %>% group_by(Lat_y,Long_x) %>% tally()

# Counting the number of experiments per country
NB_Country<-POXC_map %>% group_by(Country) %>% tally()
names(NB_Country)[1]<- 'region'
# on associe les 2.
world_map2<- left_join(world_map,NB_Country)


# Create a discrete variable
world_map2$cut_n<- cut (world_map2$n, breaks= c(0, 5, 20, 60, 80, 110))
brks_scale <- levels(world_map2$cut_n)


ggplot() +
  geom_map(data = world_map2, map = world_map,
           aes(x = long, y = lat, map_id = region,
               fill = cut_n), color = "#4d4d4d", linewidth = 0.2)+ # Changer les couleurs et la taille des lignes
  labs(fill = 'Number of experiments'                    # on ajuste les titres, légendes, ....
       ,color = '.'
       ,title = ''
       ,x = NULL
       ,y = NULL) +
  labs(title = "",
       x = "Longitude", y = "Latitude")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = 'grey50', size = 0.3, linetype = 3))+
  theme(legend.position="right")+
  scale_fill_manual(values=c('#FFD700','#B5CF68','#90CC9C','#6BC8D0','#446DA8'), na.value= "white")


##1.2 Whitetaker plot----

# on charge les données de Worldclim pour avoir precipitation et temp
##https://www.worldclim.org/data/worldclim21.html (19) WorldClim Bioclimatic variables for WorldClim version 2.
##They are the average for the years 1970-2000.BIO1 = Annual Mean Temperature + BIO12 = Annual Precipitation
P1 <- raster('wc2.1_30s_bio_12.tif')
T1 <- raster('wc2.1_30s_bio_1.tif')

# on selectionne les coordonnées de nos points GPS
# nom variable et tableau a adapter...
POXC_W<-POXC_whit %>% dplyr::filter(!is.na(Long_x),
                             !is.na(Lat_y))
TEST1<-sf::st_as_sf(POXC_W, coords = c( "Long_x", "Lat_y" ) )
values <- raster::extract(P1,TEST1)
DATA_P <- cbind.data.frame(POXC_W,values)
names(DATA_P)[8]<-"Precipitations"

# même chose avec température
values <- raster::extract(T1,TEST1)
DATA_T <- cbind.data.frame(POXC_W,values)
names(DATA_T)[8]<-"Temperature"

# on merge nos fichiers
DATA<-left_join(DATA_T,DATA_P)

#devtools::install_github("valentinitnelav/plotbiomes")
#library(plotbiomes)

# on va faire le Whittaker plot
install.packages("remotes")
remotes::install_github("valentinitnelav/plotbiomes")
library(plotbiomes)

whittaker_base_plot() +
  geom_point(data = DATA,         #add the temperature - precipitation data points
             aes(x = Temperature,
                 y = Precipitations/10),
             size   = 3,
             shape  = 23,
             colour = "white",
             fill   = "gray35",
             stroke = 1.5,
             alpha  = 0.8) +
  theme_bw()+
  theme(panel.grid.major = element_line(colour = 'grey50', size = 0.3, linetype = 3))

