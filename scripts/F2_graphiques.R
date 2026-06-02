#Installer le package ggplot2 pour faire des graphiques
#Evolution du nombre de stations mises en service par année/mois, parts de marché des opérateurs
CPS1985 <- read.csv("C:/Users/manon/OneDrive/Documents/CoursIsen/A3/S6/projet_bigdata/IRVE.csv",header = TRUE)
date <- as.Date(CPS1985$date_mise_en_service)# Permet de convertir en une date qui est représenter dans un calendrier
annee_mois <- format(date, "%Y-%m")#Permet de mettre un format année-mois
evolution <- table(annee_mois)# Permet de compter le nombre de mise en service par mois
plot(evolution, type="o",col="blue", main="Évolution des mises en service",xlab="Date", ylab="Nombre de stations")
#o c'est pour avoir les points et les lignes 
hist(CPS1985$puissance_nominale, main="Histogramme de la puissance", xlab="Puissance (en kW)",ylab="Nombre de stations", col="white", border="black")
########################################################################
#nom_enseigne en fonction de code_insee_commune
library(ggplot2)
table<-table(CPS1985$nom_enseigne,CPS1985$code_insee_commune)
df <- as.data.frame(table)# creer un style de matrice avec les enseignes(Var1), les communes(Var2) et leurs fréquences d'appartions(Freq)
#geom_col() pour faire des barres verticales, aes pour faire les axes x et y, fill pour faire les couleurs en fonction de var1
ggplot(df, aes(x = Var2, y = Freq, fill = Var1)) + geom_col() + labs(
    title = "Nom de l'enseigne en fonction du code INSEE de la commune",
    x = "Code INSEE",
    y = "Nombre"
  )
#barplot(table, main="Nom de l'enseigne en fonction du code INSEE de la commune", ylab="nb fois", xlab="code insee", col=couleurs,legend=TRUE)
#nb_pdc par implementation 

boxplot(CPS1985$nbre_pdc ~ CPS1985$implantation_station, main="Nombre de points de charge par type d'implémentation", xlab="Type d'implémentation", ylab="Nombre de points de charge", col=c("lightblue", "lightgreen"))
#################################################################################"
#repartition des puissance
hist(CPS1985$puissance_nominale, main="Répartition de la puissance nominale", xlab="Puissance nominale (en kW)", ylab="Nombre de stations", col="lightblue", border="black")
##########################################################################"
#type de prise 
#force la conversion TRUE/FALSE pour les variables de type prise
CPS1985$prise_type_ef <- as.logical(CPS1985$prise_type_ef)
CPS1985$prise_type_2 <- as.logical(CPS1985$prise_type_2)
CPS1985$prise_type_combo_ccs <- as.logical(CPS1985$prise_type_combo_ccs)
CPS1985$prise_type_chademo <- as.logical(CPS1985$prise_type_chademo)
#on somme les valeurs TRUE pour chaque type de prise pour obtenir le nombre de stations qui ont ce type de prise et on les met dans un vecteur
donnees_prises <- c(
  "EF" = sum(CPS1985$prise_type_ef == TRUE, na.rm = TRUE),
  "Type 2" = sum(CPS1985$prise_type_2 == TRUE, na.rm = TRUE),
  "Combo CCS" = sum(CPS1985$prise_type_combo_ccs == TRUE, na.rm = TRUE),
  "CHAdeMO" = sum(CPS1985$prise_type_chademo == TRUE, na.rm = TRUE)
)
print(donnees_prises)
#table<-table(CPS1985$prise_type_ef,CPS1985$prise_type_2, CPS1985$prise_type_combo_ccs, CPS1985$prise_type_chademo)
barplot(donnees_prises, main="Répartition des types de prises", xlab="Type de prise", ylab="Nombre de stations", col="red")
###############################################################################"
#repartition des reservations
CPS1985$reservation <- as.logical(CPS1985$reservation)
table<-table(CPS1985$reservation)
df<-as.data.frame(table)
ggplot(df, aes(x = Var1, y = Freq)) +geom_col(fill = "lightblue") +labs(title = "Répartition des stations avec réservation",x = "Réservation",y = "Nombre de stations")
#################################################################################
#camembert de l'enseigne la plus utilisé
library(ggplot2)
table<-table(CPS1985$nom_enseigne)
df<-as.data.frame(table)
ggplot(df, aes(x = "", y = Freq, fill = Var1)) + #defini l'axe x, defini la taille et defini la couleur 
  geom_col(width = 1, color = "white") +  # color = "white" ajoute une bordure entre les parts
  coord_polar(theta = "y") + #converti une graphique en barres en un camembert  
  labs(title = "Répartition des enseignes", fill = "Enseigne") 
