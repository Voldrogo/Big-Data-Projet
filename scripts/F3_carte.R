c <- read.csv("C:/Users/carol/Documents/Cours/Projet/voiture_electrique/Big-Data-Projet/data/fichier_nettoye.csv",header = TRUE, sep = ',');

c["coordonneesXY"];

library(mapsf)
mtq <- mf_get_mtq()
mf_map(x = mtq, var = "FR", type = "choro")

dep <- st_read("data/lot46.gpkg", layer = "departement", quiet = TRUE)
mf_map(x = mtq, var = dep, type = "choro")
mf_map(x = dep, lwd = 2, col = NA, add = TRUE) # affiche le contour des départements
