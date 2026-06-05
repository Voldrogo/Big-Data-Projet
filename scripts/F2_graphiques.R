# Fonctionnalité 2

## Représentation Graphique

Évolution du déploiement : Ce graphique retrace le rythme d'installation des bornes mois par mois pour visualiser les périodes d'accélération du réseau.

```{r points}
#irve <- read.csv("IRVE.csv", header = TRUE)
date <- as.Date(donnees$date_mise_en_service)
annee <- format(date, "%Y")#Conversion en date et extraction de l'année
#Comptage par année et conversion en dataframe
evolution <- table(annee)
df_evolution <- as.data.frame(evolution)
colnames(df_evolution) <- c("Annee", "Nombre")

#conversion en nombre et suppression des années erronées
df_evolution$Annee <- as.numeric(as.character(df_evolution$Annee))
df_evolution <- df_evolution[df_evolution$Annee > 2000, ]

#Création du graphique (on utilise les colonnes de df_evolution)
plot(df_evolution$Annee, 
     df_evolution$Nombre, 
     type = "o",  #permet d'avoir une courbe avec des points
     col = "blue", 
     main = "Évolution des mises en service",
     xlab = "Année", 
     ylab = "Nombre de stations")
```

Analyse des enseignes : Ce script identifie les 15 enseignes les plus présentes dans la base de données et les affiche sous forme de graphique en secteurs pour visualiser leur répartition.

```{r camembert}
library(dplyr)
library(ggplot2)
# 1. Préparation des données
df <- as.data.frame(table(donnees$nom_enseigne))
# 2. Filtrage du top 15
top_df <- df %>%
  arrange(desc(Freq)) %>%
  head(15)
# 3. Création du graphique
ggplot(top_df, aes(x = "", y = Freq, fill = Var1)) + 
  geom_col(width = 1, color = "white") + #Dessine les parts (le width=1 les colle entre elles)
  coord_polar(theta = "y") + #Transforme les barres en cercle 
  theme_void() + 
  labs(title = "Top 15 des enseignes", fill = "Enseigne")
```

## Histogramme

Analyse des types de prises : Ce script convertit les variables binaires en valeurs logiques, comptabilise le nombre total de stations équipées pour chaque standard et génère un diagramme en barres comparatif.

```{r histogramme}

#Barplot de la prise EF en fonction de sa puissance nominale
donnees_prises <- tapply( #tapply() regroupe les lignes qui ont le même valeur de prise (TRUE/FALSE) et de puissance
  donnees$prise_type_ef,       
  donnees$puissance_nominale,           
  sum, na.rm = TRUE            # on somme les TRUE
)

barplot(donnees_prises, main="prise EF en fonction de sa puissance", xlab="puissance nominale", ylab="Nombre de prises", col="red")



#Barplot de la prise  combo CSS en fonction de sa puissance nominale
donnees_prises <- tapply( #tapply() regroupe les lignes qui ont le même valeur de prise (TRUE/FALSE) et de puissance
  donnees$prise_type_combo_ccs,       
  donnees$puissance_nominale,           
  sum, na.rm = TRUE            # on somme les TRUE
)

barplot(donnees_prises, main="prise combo CCS en fonction de sa puissance", xlab="puissance nominale", ylab="Nombre de prises", col="yellow")
```
``` {r densite}
library(ggplot2)
library(dplyr)

donnees$type <- NA
donnees$type[donnees$prise_type_ef == "True"] <- "EF"
donnees$type[donnees$prise_type_2 == "True"] <- "Type 2"
donnees$type[donnees$prise_type_combo_ccs == "True"] <- "Combo CCS"
donnees$type[donnees$prise_type_chademo == "True"] <- "CHAdeMO"

donnees_all <- subset(donnees, !is.na(type) & !is.na(date_mise_en_service))
donnees_all$date_mise_en_service <- as.Date(donnees_all$date_mise_en_service)
donnees_all$puissance_nominale <- as.numeric(donnees_all$puissance_nominale)
donnees_all <- donnees_all[donnees_all$date_mise_en_service >= as.Date("2000-01-01"), ]

# 1. Nombre cumulé de prises par type au fil du temps
donnees_count <- donnees_all %>%
  group_by(date_mise_en_service, type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(type) %>%
  arrange(date_mise_en_service) %>%
  mutate(n_cumul = cumsum(n))

ggplot(donnees_count, aes(x = date_mise_en_service, y = n_cumul, color = type)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Nombre cumulé de prises par type au fil du temps",
    x = "Date de mise en service",
    y = "Nombre de prises (cumulé)",
    color = "Type de prise"
  )

# 2. Puissance totale par type au fil du temps
donnees_pwr <- donnees_all %>%
  filter(!is.na(puissance_nominale)) %>%
  group_by(date_mise_en_service, type) %>%
  summarise(puissance_tot = sum(puissance_nominale), .groups = "drop")

ggplot(donnees_pwr, aes(x = date_mise_en_service, y = puissance_tot, color = type)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Puissance totale par type au fil du temps",
    x = "Date de mise en service",
    y = "Puissance totale (kW)",
    color = "Type de prise"
  )
```
