irve <- read.csv("../../IRVE2.csv",header = TRUE);

library(mapsf) #sert à afficher la carte
library(sf) #pour tout ce qui est relatif à l'affiche de carte

dep <- st_read("../data/lot46.gpkg", layer = "departement", quiet = TRUE) #Lit le fichier qui contient les données de la carte

irve_filtre <- irve[irve$prise_type_combo_ccs == TRUE, ]

num_dep <- substr(irve_filtre$code_insee_commune, 1, 2) # récupère les deux premières lettres du code INSEE


tab_charge <- table(num_dep); tab_charge; # fournit le nombre de bornes par région
charges <- c()

for (d in dep$INSEE_DEP){
  print(d)
  print(unname(tab_charge[d]))
  charges <- c(charges, unname(tab_charge[d])) #uname -> permet d'avoir uniquement le nombre de bornes sans le numéro du département associé
}

dep$charge_total <- charges #ajout du nouveau paramètre à la carte

mf_map(x = dep, var = "charge_total", type = "choro") # affiche la Carte choroplèthe en fonction du nombre de bornes par département
mf_title("nombre de bornes en fonction du département")




variables <- c("prise_type_ef", "prise_type_combo_ccs", "prise_type_2", "prise_type_chademo", "prise_type_autre")

maj_var <- c()     
ecart <- c()      

for (d in dep$INSEE_DEP) {
  
  irve_dep <- donnees[substr(donnees$code_insee_commune, 1, 2) == d, ]
  
  counts <- sapply(variables, function(v) sum(irve_dep[[v]] == TRUE, na.rm = TRUE))# Compte les TRUE pour chaque variable / sapply function() : créer une fonction et l'applique à chaque éléments de variable
  
  maj_var <- c(maj_var, names(which.max(counts))) # Trouve la majoritaire
  
  
  counts_sorted <- sort(counts, decreasing = TRUE)
  ecart <- c(ecart, counts_sorted[1] - counts_sorted[2]) # Calcule l'écart entre la 1ère et la 2ème
  
}

dep$variable_maj <- maj_var
dep$ecart <- ecart


# Carte 2 : variable majoritaire
mf_map(x = dep, var = "variable_maj", type = "typo")
mf_title("type de prise majoritaire par département")

# Carte 3 : écart
mf_map(x = dep, var = "ecart", type = "choro")
mf_title("Écart la prise type 2 et combo css")


# carte intéractive 
library(mapview)
library(leaflet)
library(mapedit)

color <- c()
color <- c(color,ifelse(toupper(irve$reservation) == TRUE, "red", "green"))


icons <- awesomeIcons( # créer les marqueurs 
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = color
)


map <- leaflet(irve) %>% addTiles() %>% setView(lng = 2, lat = 46, zoom = 5) %>% #créer la carte et met le point de vue sur la france
  addAwesomeMarkers(lng = ~consolidated_longitude,
        lat = ~consolidated_latitude,
        icon=icons,
        popup =  ~paste("Puissance nominale :", puissance_nominale,"<br>","horaire :", horaires,"<br>","condition d'accès :", condition_acces,"<br>","gratuité :", gratuit,"<br>","réservation :", reservation ),
        clusterOptions = markerClusterOptions()
  )

map;


