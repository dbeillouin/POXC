#############################################################
## map world
################################################################


# Charger les bibliothèques nécessaires si elles ne sont pas déjà installées
# install.packages("ggplot2")
# install.packages("maps")
# install.packages("mapdata")

# Charger les bibliothèques
library(ggplot2)
library(maps)
library(mapdata)

# Récupérer les données des pays du monde
world_map <- map_data("world")


# Créer la carte d
library(ggplot2)

# compte le nombre de expé pour chaque combin de latiture et longitude (plusieurs données peuvent provenir de la même zone)
NB_Points<-data_Code %>% group_by(LAT_algebr,LON_algebr,NEW_treatment_type) %>% tally()

# Compte le nombre de données par pays (pour colorer le fond de carte)
NB_Country<-data_Code %>% group_by(country) %>% tally()
names(NB_Country)[1]<- 'region'
# on associe les 2.
world_map2<- left_join(world_map,NB_Country)

# on peux si on veux choisir un autre niveau d'info; par exemple ici les types d'intervention
# nom de la variable a changer, j'ai réutilisé un vieux scripts
NB_SYST<-data_Code %>% group_by(NEW_treatment_type) %>% tally() %>%
  arrange()

# on chooisi nos couleurs
couleurs_par_niveau <- c(
  "Agroforestry fallow" = "#7fc97f",
  "Alley cropping" = "#beaed4",
  "Hedgerow" = "#fdc086",
  "Multistrata system" = "#ffff99",
  "Shaded perennial" = "#386cb0",
  "Silvopasture" = "#f0027f"
)

# et on l'histogramme du nombre de données
ggplot(NB_SYST, aes(x = reorder(NEW_treatment_type, n), y = n, fill = NEW_treatment_type)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "", x = "", y = "Count") +
  scale_fill_manual(values = couleurs_par_niveau) +  # Utiliser les couleurs spécifiques pour chaque niveau
  theme_pubr() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, color = 'black')) +
  coord_flip()


## on va faire la carte
NB_Points<-left_join(NB_Points, TAB)

library(ggnewscale)
ggplot() +
  geom_map(data = world_map2, map = world_map,
           aes(x = long, y = lat, map_id = region,
               fill = n), color = "#4d4d4d", size = 0.5) +  # Changer les couleurs et la taille des lignes
  scale_fill_gradientn(colours=brewer.pal(5,"OrRd"))+
  coord_cartesian(xlim = longitude_lim, ylim = latitude_lim) +
  labs(title = "",
       x = "", y = "") +  # Ajouter des étiquettes aux axes
  theme_minimal() +  new_scale_fill() +
  geom_point(data = NB_Points,
             aes(x = LON_algebr, y = LAT_algebr, size= n,
                 fill = NEW_treatment_type),shape=21) +
  scale_fill_manual(values = couleurs_par_niveau) + # Utiliser les couleurs spécifiques pour chaque niveau
  theme(legend.position = "none",  # Supprimer la légende pour les points
        panel.grid = element_blank())  # Supprimer le quadrillage


### Whitetaker plot


### Whitetaker - this part needs updating ####

library(raster)
library(sp)

# on charge les données de Worldclim pour avoir precipitation et temp
P1 <- raster('wc2.1_10m_prec_01.tif')
T1 <- raster('wc2.1_10m_tavg_01.tif')

# on selectionne les coordonnées de nos points GPS
# nom variable et tableau a adapter...
TEST<-TEST %>% dplyr::filter(!is.na(longitude),
                             !is.na(latitude))
TEST1<-sf::st_as_sf(TEST, coords = c( "longitude", "latitude" ) )
values <- raster::extract(P1,TEST1)
DATA_P <- cbind.data.frame(TEST,values)
names(DATA_P)[4]<-"precipitation"

# même chose avec température
values <- raster::extract(T1,TEST1)
DATA_T <- cbind.data.frame(TEST,values)
names(DATA_T)[4]<-"temperature"

# on merge nos fichiers
DATA<-left_join(DATA_T,DATA_P)

#devtools::install_github("valentinitnelav/plotbiomes")
#library(plotbiomes)

# on va faire le Whitetaker plot
whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = DATA,
             aes(x = temperature,
                 y = precipitation),
             size   = 3,
             shape  = 21,
             colour = "gray95",
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  theme_bw()
