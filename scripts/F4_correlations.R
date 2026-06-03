# ============================================================
# F4 - Corrélations entre variables (IRVE)
# ============================================================

library(dplyr)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
if (!dir.exists("output")) dir.create("output")

df <- read.csv("data/export_IA.csv", sep = ",", stringsAsFactors = FALSE) %>%
  
  #Convertit toutes les chaînes vides et inconnu en NA (sécurité)
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate(across(where(is.character), ~na_if(., "inconnu")))

cat("Données chargées :", nrow(df), "lignes\n")


# ── Fonction générique barplot ────────────────────────────────
barplot_moy <- function(data, groupe, valeur, titre, axe_x, couleur, fichier, top = 15, min_n = 50) {
  d <- data %>%
    #filtre les NA
    filter(!is.na(.data[[groupe]]) & !is.na(.data[[valeur]])) %>%
    #Regroupe par groupe
    group_by(g = .data[[groupe]]) %>%
    #calcule Moyenne + Observation groupe
    summarise(moy = mean(.data[[valeur]], na.rm = TRUE), nb = n(), .groups = "drop") %>%
    #filtre les petit groupe
    filter(nb >= min_n) %>%
    #trié ordre croissant 
    arrange(desc(moy)) %>%
    #on garde que les N plus grand 
    slice_head(n = top)

  #Construction Graph
  p <- ggplot(d, aes(x = reorder(g, moy), y = moy)) +
    geom_col(fill = couleur) +
    geom_text(aes(label = round(moy, 1)), hjust = -0.2, size = 3.5) +
    coord_flip() +
    labs(title = titre, x = NULL, y = axe_x) +
    theme_minimal(base_size = 12) +
    expand_limits(y = max(d$moy) * 1.12)

  ggsave(paste0("output/", fichier), p, width = 11, height = 7, dpi = 150)
  cat("→ output/", fichier, "\n")
}

cat("\n── Export des barplots ──\n")

# Opérateur Puissance moyenne
barplot_moy(df, "nom_operateur", "puissance_nominale",
            "Puissance moyenne par opérateur (top 15)",
            "Puissance moyenne (kW)", "#e63946",
            "F4_operateur_puissance.png")

# Opérateur Nombre moyen de PDC
barplot_moy(df, "nom_operateur", "nbre_pdc",
            "Nombre moyen de PDC par opérateur (top 15)",
            "Nombre moyen de PDC", "#f4a261",
            "F4_operateur_pdc.png")

# Implantation Nombre moyen de PDC
barplot_moy(df, "implantation_station", "nbre_pdc",
            "Nombre moyen de PDC par type d'implantation",
            "Nombre moyen de PDC", "#457b9d",
            "F4_implantation_pdc.png", top = 20, min_n = 10)

# Implantation Puissance moyenne
barplot_moy(df, "implantation_station", "puissance_nominale",
            "Puissance moyenne par type d'implantation",
            "Puissance moyenne (kW)", "#2a9d8f",
            "F4_implantation_puissance.png", top = 20, min_n = 10)

# Condition d'accès Puissance moyenne
barplot_moy(df, "condition_acces", "puissance_nominale",
            "Puissance moyenne par condition d'accès",
            "Puissance moyenne (kW)", "#8338ec",
            "F4_acces_puissance.png", top = 10, min_n = 10)

# Condition d'accès Nombre moyen de PDC
barplot_moy(df, "condition_acces", "nbre_pdc",
            "Nombre moyen de PDC par condition d'accès",
            "Nombre moyen de PDC", "#3a86ff",
            "F4_acces_pdc.png", top = 10, min_n = 10)
