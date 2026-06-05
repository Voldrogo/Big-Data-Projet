#Regression Lineaire
################################################################"
irve <- read.csv("../../IRVE2.csv",header = TRUE)
library(rgl)
modele1<-library(rgl)

modele1 <- lm(puissance_nominale ~  nbre_pdc +
                prise_type_ef + prise_type_2 + prise_type_combo_ccs +
                prise_type_chademo + prise_type_autre + gratuit +
                paiement_acte + paiement_cb + paiement_autre +
                reservation + station_deux_roues +
                consolidated_longitude + consolidated_latitude,
              data = irve)
summary(modele1)
plot(modele1)


str(irve)
table(irve$tarification)


###################################################################
#Regression logistique

irve <- read.csv("../../IRVE2.csv",header = TRUE)

t <- table(irve$tarification)

trie_tarif <- t[t >= 100]              # garder uniquement les valeurs ≥ 100
trie_tarif <- sort(trie_tarif, decreasing = FALSE)   # trier par ordre croissant

valeurs_a_garder <- names(trie_tarif) # garde uniquement la valeur des variables pas le nombre d'occurence

filtre_tarif <- irve[irve$tarification %in% valeurs_a_garder, ] # garde uniquement les lignes avec les variables de tarifications stockés dans valeurs_a_garder

#table(filtre_tarif$tarification)


filtre_tarif_euro <- filtre_tarif[ grepl("[0-9]+ ?(€|cts)", filtre_tarif$tarification), ] #garde toute les lignes qui respecte le grep

#table(filtre_tarif_euro$tarification)

filtre_tarif_euro$tarification <- as.numeric(
  sub(",", ".",  # transforme les , en .
      regmatches(filtre_tarif_euro$tarification, # renvoie uniquement les parties qui satisfont l'exigence 
                           regexpr("[0-9]+([.,][0-9]+)?(?=[[:space:]]*(€|cts))", filtre_tarif_euro$tarification,perl=TRUE)))# renvoie toute les nombres qui sont suivi d'un € ou cts, =? signifie qu'on ne renvoie pas ces symboles 
)
#table(filtre_tarif_euro$tarification)

filtre_tarif_euro$tarification <- ifelse( #transforme euro en centimes
filtre_tarif_euro$tarification > 1,
filtre_tarif_euro$tarification / 100,
filtre_tarif_euro$tarification
)

#table(filtre_tarif_euro$tarification)

tarif_gratuit <- irve[irve$gratuit %in% "True", ] # garde uniquement les lignes avec les variables de tarifications stockés dans valeurs_a_garder
#table(tarif_gratuit$tarification)
tarif_gratuit$tarification <- 0; # affecte un 0 à toute les variables
filtre_tarif_final <- rbind(filtre_tarif_euro, tarif_gratuit)
table(filtre_tarif_final$tarification)

filtre_tarif_final$categorie_tarif <- cut(
  filtre_tarif_final$tarification,
  breaks = quantile(filtre_tarif_final$tarification, probs = c(0, 1/3, 2/3, 1)),
  labels = c("bas", "modéré", "élevé"),
  include.lowest = TRUE
)


library(nnet)
library(ggplot2)
library(dplyr)
library(tidyr)    # pour pivot_longer
library(broom)
library(caret)

cols_utilisees <- c("categorie_tarif","puissance_nominale", "nbre_pdc",
                    "prise_type_ef", "prise_type_2", "prise_type_combo_ccs",
                    "prise_type_chademo", "prise_type_autre",
                    "paiement_acte", "paiement_cb", "paiement_autre",
                    "reservation", "station_deux_roues",
                    "consolidated_longitude", "consolidated_latitude")


filtre_tarif_final <- na.omit(filtre_tarif_final[, cols_utilisees]) # supprime toutes les lignes avec un NA
cat(sprintf("Lignes après suppression des NA : %d\n", nrow(filtre_tarif_final)))

set.seed(42)
filtre_tarif_final <- filtre_tarif_final[sample(nrow(filtre_tarif_final)), ]# mélange les données

filtre_tarif_final[, cols_utilisees] <- lapply(filtre_tarif_final[, cols_utilisees], function(col) { # transforme tous les booléens en numérique
  if (all(unique(na.omit(col)) %in% c("True", "False"))) as.numeric(col == "True")
  else col
})

fold_size <- floor(nrow(filtre_tarif_final) / 5) # floor arrondi le nombre entier inférieur
resultats <- c()

for (i in 1:5) {
  idx_test <- ((i-1)*fold_size + 1):(i*fold_size) #crée une séquence qui se décale au fil des boucles 
  
  train <- filtre_tarif_final[-idx_test, cols_utilisees]  # cols_utilisees sert à utiliser uniquement les colonnes filtrés et pas les autres

  test  <- filtre_tarif_final[idx_test, cols_utilisees]
  
  modele <- multinom(categorie_tarif ~ nbre_pdc +
                       prise_type_ef + prise_type_2 + prise_type_combo_ccs +
                       prise_type_chademo + prise_type_autre +
                       paiement_acte + paiement_cb + paiement_autre +
                       reservation + station_deux_roues +
                       consolidated_longitude + consolidated_latitude, data = train)
  
  predictions <- predict(modele, newdata = test)
  accuracy <- mean(predictions == test$categorie_tarif)
  resultats <- c(resultats, accuracy)
  cat("Fold", i, "- Accuracy :", accuracy, "\n")
}

cat("Accuracy moyenne :", mean(resultats), "\n")


# --- 1. Odds Ratios ---
or_df <- tidy(modele, exponentiate = TRUE, conf.int = TRUE)

ggplot(or_df %>% filter(term != "(Intercept)"),
       aes(x = estimate, y = reorder(term, estimate),
           xmin = conf.low, xmax = conf.high, color = y.level)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +
  geom_errorbarh(height = 0.25, alpha = 0.5) +
  geom_point(size = 2.5) +
  facet_wrap(~y.level) +
  scale_x_log10() +
  labs(x = "Odds Ratio (log)", y = NULL, title = "Odds Ratios par catégorie vs 'bas'") +
  theme_minimal() + theme(legend.position = "none")


# --- 3. Matrice de confusion ---
preds <- predict(modele, newdata = filtre_tarif_final, type = "class")
cm    <- confusionMatrix(preds, filtre_tarif_final$categorie_tarif)

as.data.frame(cm$table) %>%
  ggplot(aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "white", high = "#3B8BD4") +
  labs(title = "Matrice de confusion", x = "Prédit", y = "Observé") +
  theme_minimal()