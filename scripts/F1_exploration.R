# F1 - Exploration & Nettoyage des données IRVE

library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

donnees <- read.csv("data/IRVE.csv", sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)
cat("Dimensions initiales :", nrow(donnees), "lignes x", ncol(donnees), "colonnes\n")

# Filtre les lignes selon une condition et affiche le nombre de lignes supprimées
# donnees   : le dataframe à filtrer
# condition : expression de filtre (ex: puissance >= 1)
# message   : texte affiché dans la console
filtrer <- function(donnees, condition, message) {
  nb_avant <- nrow(donnees)
  donnees  <- donnees %>% filter({{ condition }})
  cat(message, ":", nb_avant - nrow(donnees), "\n")
  donnees
}


# 1. DOUBLONS
nb_avant <- nrow(donnees)
donnees  <- donnees %>% distinct()
cat("Doublons supprimés :", nb_avant - nrow(donnees), "| Lignes restantes :", nrow(donnees), "\n")


# 2. VALEURS MANQUANTES
# Tableau récapitulatif des NA par colonne
nb_na_par_col <- colSums(is.na(donnees))
bilan_na <- data.frame(
  colonne     = names(donnees),
  nb_na       = nb_na_par_col,
  pourcentage = round(nb_na_par_col / nrow(donnees) * 100, 2)
) %>% arrange(desc(pourcentage))

colonnes_avec_na <- bilan_na[bilan_na$nb_na > 0, ]
if (nrow(colonnes_avec_na) > 0) {
  cat("Colonnes avec NA :\n")
  print(colonnes_avec_na, row.names = FALSE)
} else {
  cat("Aucune valeur manquante.\n")
}

# Supprimer les colonnes avec plus de 70% de NA
colonnes_a_supprimer <- bilan_na$colonne[bilan_na$pourcentage > 70]
if (length(colonnes_a_supprimer) > 0) {
  cat("Colonnes supprimées (>70% NA) :", paste(colonnes_a_supprimer, collapse = ", "), "\n")
  donnees <- donnees %>% select(-all_of(colonnes_a_supprimer))
}

# puissance_nominale : NA → médiane
mediane_puissance <- median(donnees$puissance_nominale, na.rm = TRUE)
nb_na_puissance   <- sum(is.na(donnees$puissance_nominale))
donnees$puissance_nominale <- replace(donnees$puissance_nominale,
                                      is.na(donnees$puissance_nominale), mediane_puissance)
cat("puissance_nominale :", nb_na_puissance, "NA → médiane (", mediane_puissance, "kW)\n")

# Chaînes vides → NA
donnees <- donnees %>% mutate(across(where(is.character), ~na_if(., "")))

# Coordonnées manquantes → supprimer la ligne
donnees <- filtrer(donnees,
                   !is.na(consolidated_longitude) & !is.na(consolidated_latitude),
                   "Lignes sans coordonnées supprimées")

# Normalise les valeurs booléennes texte vers "True" ou "False"
normaliser_booleens <- function(x) {
  case_when(
    tolower(x) %in% c("true",  "1", "oui", "yes") ~ "True",
    tolower(x) %in% c("false", "0", "non", "no")  ~ "False",
    TRUE ~ x
  )
}

colonnes_booleennes <- c("gratuit", "paiement_acte", "paiement_cb", "paiement_autre",
                         "prise_type_ef", "prise_type_2", "prise_type_combo_ccs",
                         "prise_type_chademo", "prise_type_autre", "reservation",
                         "cable_t2_attache", "station_deux_roues",
                         "consolidated_is_lon_lat_correct", "consolidated_is_code_insee_verified",
                         "consolidated_is_code_insee_modified")

donnees <- donnees %>% mutate(across(any_of(colonnes_booleennes), normaliser_booleens))
cat("Booléens normalisés → True / False\n")

# 3. VALEURS ABERRANTES

# puissance_nominale hors [1–500 kW]
cat("puissance_nominale — Min:", min(donnees$puissance_nominale, na.rm = TRUE),
    "| Max:", max(donnees$puissance_nominale, na.rm = TRUE), "kW\n")
donnees <- filtrer(donnees, puissance_nominale >= 1 & puissance_nominale <= 500,
                   "Lignes aberrantes puissance supprimées")

# Normalisation aux paliers standards
paliers <- c(3.7, 7.4, 11, 22, 50, 75, 100, 150, 175, 200, 250, 300, 350, 400)
donnees$puissance_nominale <- sapply(donnees$puissance_nominale,
                                     function(x) paliers[which.min(abs(paliers - x))])
cat("puissance_nominale normalisée aux paliers standards\n")

# Coordonnées hors France métropolitaine
donnees <- filtrer(donnees,
                   consolidated_longitude >= -5.5 & consolidated_longitude <= 10 &
                   consolidated_latitude  >= 41   & consolidated_latitude  <= 51.5,
                   "Lignes hors France supprimées")

# nbre_pdc aberrant
donnees <- filtrer(donnees, nbre_pdc >= 1 & nbre_pdc <= 100,
                   "Lignes aberrantes nbre_pdc supprimées")


# 4. EXPORT
cat("Dimensions finales :", nrow(donnees), "lignes x", ncol(donnees), "colonnes\n")
write.csv(donnees, "data/export_IA.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Export OK → data/export_IA.csv\n")
