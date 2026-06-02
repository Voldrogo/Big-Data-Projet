c <- read.csv("C:/Users/carol/Documents/Cours/Projet/voiture_electrique/Big-Data-Projet/data/fichier_nettoye.csv",header = TRUE, sep = ',');

c["coordonneesXY"];

library(mapsf) #sert à afficher la carte
library(sf) #pour tout ce qui est relatif à l'affiche de carte


dep <- st_read("../data/lot46.gpkg", layer = "departement", quiet = TRUE) #Lit le fichier qui contient les données de la carte

mf_map(x = dep) # affiche la carte de la france


st_crs(dep) #permet de connaitre la norme des coordonnées de la carte (ici ; EPSG:2154 — Lambert‑93)

separate(c["coordonneesXY"], into = c("X", "Y"), sep = ", ", convert = TRUE)

for (coord in c["coordonneesXY"]) {
  print(coord[,1])
  #point_sf <- st_as_sf(c, coords = c(coord), crs = 2154)  # convertit les coord en points sf
  #points_dep <- st_join(point_sf, dep) # jointure spacial -> ajoute une colonne avec le département correspondant 
}

points_dep

