
# F4 - Corrélations & Analyses bivariées (IRVE)

library(dplyr)
library(ggplot2)
library(forcats)

# ── Chargement ───────────────────────────────────────────────
racine <- "/Users/voldrogo/Documents/Big-Data-Projet"
setwd(racine)
if (!dir.exists("output")) dir.create("output")

donnees <- read.csv("data/IRVE.nettoyer.csv", sep = ",", stringsAsFactors = FALSE) %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate(across(where(is.character), ~na_if(., "inconnu")))

cat("Données chargées :", nrow(donnees), "lignes\n")

# Normalisation condition_acces et implantation_station
donnees$condition_acces <- case_when(
  grepl("libre",  tolower(donnees$condition_acces)) ~ "Accès libre",
  grepl("reserv", tolower(donnees$condition_acces)) ~ "Accès réservé",
  TRUE ~ donnees$condition_acces
)

donnees$implantation_station <- case_when(
  grepl("usage.*public|priv.*public", tolower(donnees$implantation_station)) ~ "Parking privé à usage public",
  grepl("client",                     tolower(donnees$implantation_station)) ~ "Parking privé réservé à la clientèle",
  grepl("parking public",             tolower(donnees$implantation_station)) ~ "Parking public",
  grepl("voirie",                     tolower(donnees$implantation_station)) ~ "Voirie",
  grepl("rapid|dedie|dedi",           tolower(donnees$implantation_station)) ~ "Station dédiée à la recharge rapide",
  TRUE ~ donnees$implantation_station
)


# ============================================================
# 1. CORRÉLATIONS QUANTITATIVES — Nuages de points
# ============================================================

# Calcule r de Pearson et génère un nuage de points avec droite de régression
nuage_points <- function(data, x, y, titre, fichier) {
  d <- data %>% filter(!is.na(.data[[x]]) & !is.na(.data[[y]]))
  r <- round(cor(d[[x]], d[[y]], method = "pearson"), 3)

  p <- ggplot(d, aes(x = .data[[x]], y = .data[[y]])) +
    geom_jitter(alpha = 0.15, color = "#264653", size = 0.8, width = 0.5, height = 0.3) +
    geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 1) +
    labs(title    = titre,
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


# ============================================================
# 2. CORRÉLATIONS QUALITATIVES — Chi2 + Mosaicplot
# ============================================================

# Test Chi2 + mosaicplot
# top_n : nombre de modalités max à afficher (les autres → "Other")
library(dplyr)
library(forcats)

test_chi2_mosaic <- function(data, var1, var2, titre, fichier, top_n = 6) {
  
  d <- data %>%
    filter(!is.na(.data[[var1]]), !is.na(.data[[var2]])) %>%
    mutate(
      v1 = fct_lump_n(factor(.data[[var1]]), n = top_n, other_level = "Autre"),
      v2 = fct_lump_n(factor(.data[[var2]]), n = top_n, other_level = "Autre")
    )

  tbl <- table(d$v1, d$v2)
  chi <- chisq.test(tbl, simulate.p.value = TRUE, B = 2000)
  
  cat("\n──", titre, "──\n")
  cat("Chi2 =", round(chi$statistic, 2), "| p-value =", format(chi$p.value, digits = 4))
  if (chi$p.value < 0.05) cat("  → liaison significative ✓\n") else cat("  → pas de liaison\n")
  
  # ── Résidus de Pearson ───────────────────────────────────────
  # residus[i,j] = (obs - attendu) / sqrt(attendu)
  # Valeur absolue élevée = cellule qui "tire" le Chi2 vers le haut
  residus <- chi$residuals  # matrice de même dimension que tbl
  
  # Palette : bleu (sous-représenté) → blanc (neutre) → rouge (sur-représenté)
  # On discrétise en 5 classes selon les seuils habituels des résidus
  palette_residus <- function(r) {
    ifelse(r < -4,  "#1a5276",   # très fortement sous-représenté
           ifelse(r < -2,  "#5dade2",   # fortement sous-représenté
                  ifelse(r < -0.5,"#aed6f1",  # légèrement sous-représenté
                         ifelse(r <  0.5,"#f5f5f5",  # neutre
                                ifelse(r <  2,  "#f1948a",  # légèrement sur-représenté
                                       ifelse(r <  4,  "#e74c3c",  # fortement sur-représenté
                                              "#7b241c"   # très fortement sur-représenté
                                       ))))))
  }
  
  couleurs <- palette_residus(residus)
  
  # ── Export PNG ───────────────────────────────────────────────
  png(paste0("output/", fichier), width = 1600, height = 1100, res = 130)
  
  # Marges : bas large (labels), droite large (légende)
  layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))
  
  # — Panneau 1 : mosaicplot —
  par(mar = c(14, 4, 6, 1))
  mosaicplot(
    tbl,
    main     = paste0(titre,
                      "\nChi2 = ", round(chi$statistic, 1),
                      "  |  p-value = ", format(chi$p.value, digits = 4),
                      if (chi$p.value < 0.05) "  ✓ liaison significative" else ""),
    color    = couleurs,   # matrice de couleurs alignée sur tbl
    las      = 2,
    cex.axis = 0.75,
    border   = "white"
  )
  
  # — Panneau 2 : légende des résidus —
  par(mar = c(14, 0, 6, 3))
  plot.new()
  
  legende_labels <- c("< -4", "-4 à -2", "-2 à -0.5", "-0.5 à 0.5", "0.5 à 2", "2 à 4", "> 4")
  legende_cols   <- c("#1a5276", "#5dade2", "#aed6f1", "#f5f5f5",    "#f1948a", "#e74c3c", "#7b241c")
  n <- length(legende_cols)
  
  # Rectangles colorés
  y_positions <- seq(0.85, 0.15, length.out = n)
  rect_h <- 0.09
  for (i in seq_len(n)) {
    rect(0.05, y_positions[i] - rect_h / 2,
         0.45, y_positions[i] + rect_h / 2,
         col = legende_cols[i], border = "white", lwd = 0.5)
    text(0.52, y_positions[i], legende_labels[i],
         adj = 0, cex = 0.75, col = "grey20")
  }
  
  # Titre légende
  text(0.25, 0.97, "Résidu de\nPearson",
       adj = 0.5, cex = 0.8, font = 2, col = "grey20")
  
  # Sous-titre explicatif
  text(0.25, 0.04,
       "+ = sur-représenté\n– = sous-représenté",
       adj = 0.5, cex = 0.65, col = "grey50")
  
  dev.off()
  cat("→ output/", fichier, "\n")
}

# ── Appels ───────────────────────────────────────────────────────────────────

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

cat("\n══════════════════════════════════════\n")
cat("  EXPORT TERMINÉ — output/\n")
cat("══════════════════════════════════════\n")

