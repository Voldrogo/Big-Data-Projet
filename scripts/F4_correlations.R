
# F4 - Corrélations & Analyses bivariées (IRVE)


library(dplyr)
library(ggplot2)
library(forcats)

# ── Chargement ───────────────────────────────────────────────
racine <- "/Users/voldrogo/Documents/Big-Data-Projet"
setwd(racine)
if (!dir.exists("output")) dir.create("output")

donnees <- read.csv("data/export_IA.csv", sep = ",", stringsAsFactors = FALSE) %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate(across(where(is.character), ~na_if(., "inconnu")))

cat("Données chargées :", nrow(donnees), "lignes\n")

# 1. LIENS ENTRE VARIABLES — Barplots (quantitatif/qualitatif)


# préparation des données pour les test 
barplot_moy <- function(data, groupe, valeur, titre, axe_x, couleur, fichier, top = 15, min_n = 50) {
  d <- data %>%
    #suprime les lignes NA 
    filter(!is.na(.data[[groupe]]) & !is.na(.data[[valeur]])) %>%
    group_by(g = .data[[groupe]]) %>%
    summarise(moy = mean(.data[[valeur]], na.rm = TRUE), nb = n(), .groups = "drop") %>%
    #élimine les groupes trop petits 
    filter(nb >= min_n) %>%
    #trie de façon croissant
    arrange(desc(moy)) %>%
    #garde les n plus grand 
    slice_head(n = top)

  p <- ggplot(d, aes(x = reorder(g, moy), y = moy)) +
    geom_col(fill = couleur) +
    #afficher la valeur en bout de barre 
    geom_text(aes(label = round(moy, 1)), hjust = -0.2, size = 3.5) +
    #barre horizontal 
    coord_flip() +
    #définie les titres x , y 
    labs(title = titre, x = NULL, y = axe_x) +
    theme_minimal(base_size = 12) +
    expand_limits(y = max(d$moy) * 1.12)

  ggsave(paste0("output/", fichier), p, width = 11, height = 7, dpi = 150)
  cat("→ output/", fichier, "\n")
}

barplot_moy(donnees, "nom_operateur", "puissance_nominale",
            "Puissance moyenne par opérateur (top 15)",
            "Puissance moyenne (kW)", "#e63946", "F4_operateur_puissance.png")

barplot_moy(donnees, "nom_operateur", "nbre_pdc",
            "Nombre moyen de PDC par opérateur (top 15)",
            "Nombre moyen de PDC", "#f4a261", "F4_operateur_pdc.png")

barplot_moy(donnees, "implantation_station", "nbre_pdc",
            "Nombre moyen de PDC par type d'implantation",
            "Nombre moyen de PDC", "#457b9d", "F4_implantation_pdc.png", top = 20, min_n = 10)

barplot_moy(donnees, "implantation_station", "puissance_nominale",
            "Puissance moyenne par type d'implantation",
            "Puissance moyenne (kW)", "#2a9d8f", "F4_implantation_puissance.png", top = 20, min_n = 10)

barplot_moy(donnees, "condition_acces", "puissance_nominale",
            "Puissance moyenne par condition d'accès",
            "Puissance moyenne (kW)", "#8338ec", "F4_acces_puissance.png", top = 10, min_n = 10)

barplot_moy(donnees, "prise_type_combo_ccs", "puissance_nominale",
            "Puissance moyenne selon présence Combo CCS",
            "Puissance moyenne (kW)", "#06d6a0", "F4_ccs_puissance.png", top = 5, min_n = 10)

barplot_moy(donnees, "condition_acces", "nbre_pdc",
            "Nombre moyen de PDC par condition d'accès",
            "Nombre moyen de PDC", "#3a86ff", "F4_acces_pdc.png", top = 10, min_n = 10)


# 2. ANALYSES BIVARIÉES — Nuages de points


nuage_points <- function(data, x, y, titre, fichier) {
  d <- data %>% filter(!is.na(.data[[x]]) & !is.na(.data[[y]]))
  #arrondie les valeur des coordonée 
  r <- round(cor(d[[x]], d[[y]], method = "pearson"), 3)

  p <- ggplot(d, aes(x = .data[[x]], y = .data[[y]])) +
    #met un décalage au point par coordonnée pour eviter les superpositions
    geom_jitter(alpha = 0.15, color = "#264653", size = 0.8, width = 0.5, height = 0.3) +
    geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 1) +
    labs(title = titre,
         subtitle = paste0("r = ", r, "  |  n = ", nrow(d), " bornes"),
         x = x, y = y) +
    theme_classic(base_size = 12)

  ggsave(paste0("output/", fichier), p, width = 9, height = 6, dpi = 150)
  cat("r =", r, "→ output/", fichier, "\n")
}



nuage_points(donnees, "nbre_pdc", "puissance_nominale",
             "Nombre de PDC vs Puissance nominale", "F4_biv_pdc_puissance.png")

nuage_points(donnees, "consolidated_latitude", "nbre_pdc",
             "Latitude vs Nombre de PDC", "F4_biv_lat_pdc.png")

nuage_points(donnees, "consolidated_latitude", "puissance_nominale",
             "Latitude vs Puissance nominale", "F4_biv_lat_puissance.png")

nuage_points(donnees, "consolidated_longitude", "nbre_pdc",
             "Longitude vs Nombre de PDC", "F4_biv_lon_pdc.png")


# 3. VARIABLES QUALITATIVES — Chi2 + Mosaicplot


# Test Chi2 + barplot 100% empilé


# top_n : nombre de modalités max à afficher (les autres → "Autre")
test_chi2_mosaic <- function(data, var1, var2, titre, fichier, top_n = 6) {
  d <- data %>%
    filter(!is.na(.data[[var1]]) & !is.na(.data[[var2]])) %>%
    mutate(
      #variable en axe X (ex: implantation_station)
      v1 = fct_lump_n(.data[[var1]], top_n),
      # variable de remplissage/couleur (ex: condition_acces)
      v2 = fct_lump_n(.data[[var2]], top_n)
    )

  # Test Chi2
  tbl <- table(d$v1, d$v2)
  chi <- chisq.test(tbl, simulate.p.value = TRUE, B = 2000)
  cat("\n──", titre, "──\n")
  cat("Chi2 =", round(chi$statistic, 2), "| p-value =", format(chi$p.value, digits = 4))
  if (chi$p.value < 0.05) cat("  → liaison significative ✓\n") else cat("  → pas de liaison\n")

  # Barplot 100% empilé
  p <- ggplot(d, aes(x = v1, fill = v2)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title    = titre,
      subtitle = paste0("Chi2 = ", round(chi$statistic, 1), "  |  p-value = ", round(chi$p.value, 4)),
      x        = var1,
      y        = "Proportion",
      fill     = var2
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          legend.position = "right")

  ggsave(paste0("output/", fichier), p, width = 11, height = 7, dpi = 150)
  cat("→ output/", fichier, "\n")
}

test_chi2_mosaic(donnees, "implantation_station", "condition_acces",
                 "Implantation ↔ Condition d'accès",
                 "F4_chi2_implantation_acces.png")

test_chi2_mosaic(donnees, "implantation_station", "prise_type_combo_ccs",
                 "Implantation ↔ Prise Combo CCS",
                 "F4_chi2_implantation_ccs.png")

test_chi2_mosaic(donnees, "condition_acces", "gratuit",
                 "Condition d'accès ↔ Gratuit",
                 "F4_chi2_acces_gratuit.png")

test_chi2_mosaic(donnees, "nom_operateur", "condition_acces",
                 "Opérateur ↔ Condition d'accès",
                 "F4_chi2_operateur_acces.png")

