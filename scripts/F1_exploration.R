# ============================================================
# F1 - Exploration & Nettoyage des données IRVE
# ============================================================

library(dplyr)
library(stringr)
library(lubridate)

# ── Chargement ───────────────────────────────────────────────
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

df <- read.csv("data/IRVE.csv", sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)
cat("Dimensions initiales :", nrow(df), "lignes x", ncol(df), "colonnes\n")


# ============================================================
# 1. DOUBLONS
# ============================================================

cat("\n══════════════════════════════════════\n")
cat("  1. DOUBLONS\n")
cat("══════════════════════════════════════\n")

avant <- nrow(df)
df <- df %>% distinct()
cat("Doublons supprimés (lignes 100% identiques) :", avant - nrow(df), "\n")
cat("Lignes après dédoublonnage :", nrow(df), "\n")


# ============================================================
# 2. VALEURS MANQUANTES
# ============================================================

cat("\n══════════════════════════════════════\n")
cat("  2. VALEURS MANQUANTES\n")
cat("══════════════════════════════════════\n")

# Audit : taux de NA par colonne
na_summary <- data.frame(
  colonne     = names(df),
  nb_na       = colSums(is.na(df)),
  taux_na_pct = round(colSums(is.na(df)) / nrow(df) * 100, 2)
) %>% arrange(desc(taux_na_pct))

cols_avec_na <- na_summary[na_summary$nb_na > 0, ]
if (nrow(cols_avec_na) > 0) {
  cat("Colonnes avec NA :\n")
  print(cols_avec_na, row.names = FALSE)
} else {
  cat("Aucune valeur manquante détectée.\n")
}

# Supprimer les colonnes avec > 70% de NA
cols_drop <- na_summary$colonne[na_summary$taux_na_pct > 70]
if (length(cols_drop) > 0) {
  cat("\nColonnes supprimées (>70% NA) :", paste(cols_drop, collapse = ", "), "\n")
  df <- df %>% select(-all_of(cols_drop))
}

# puissance_nominale : NA → médiane
if ("puissance_nominale" %in% names(df)) {
  nb_na_pui <- sum(is.na(df$puissance_nominale))
  if (nb_na_pui > 0) {
    med <- median(df$puissance_nominale, na.rm = TRUE)
    df$puissance_nominale[is.na(df$puissance_nominale)] <- med
    cat("puissance_nominale :", nb_na_pui, "NA remplacés par la médiane (", med, "kW)\n")
  }
}

# Coordonnées manquantes : supprimer la ligne
if (all(c("consolidated_longitude", "consolidated_latitude") %in% names(df))) {
  avant <- nrow(df)
  df <- df %>% filter(!is.na(consolidated_longitude) & !is.na(consolidated_latitude))
  cat("Lignes supprimées (coords manquantes) :", avant - nrow(df), "\n")
}

# Colonnes texte importantes : NA → "inconnu"
cols_texte <- c("condition_acces", "implantation_station", "nom_operateur",
                "nom_enseigne", "consolidated_commune")
for (col in intersect(cols_texte, names(df))) {
  nb <- sum(is.na(df[[col]]) | df[[col]] == "")
  if (nb > 0) {
    df[[col]][is.na(df[[col]]) | df[[col]] == ""] <- "inconnu"
    cat(col, ": ", nb, "vides → 'inconnu'\n")
  }
}

# Colonnes booléennes texte : NA → "inconnu"
bool_cols <- c("gratuit", "paiement_cb", "paiement_autre",
               "prise_type_ef", "prise_type_2", "prise_type_combo_ccs", "prise_type_chademo")
for (col in intersect(bool_cols, names(df))) {
  nb <- sum(is.na(df[[col]]))
  if (nb > 0) {
    df[[col]][is.na(df[[col]])] <- "inconnu"
    cat(col, ": ", nb, "NA → 'inconnu'\n")
  }
}


# ============================================================
# 3. VALEURS ABERRANTES
# ============================================================

cat("\n══════════════════════════════════════\n")
cat("  3. VALEURS ABERRANTES\n")
cat("══════════════════════════════════════\n")

# ── 3a. puissance_nominale ───────────────────────────────────
if ("puissance_nominale" %in% names(df)) {
  cat("puissance_nominale — Min:", min(df$puissance_nominale, na.rm = TRUE),
      "| Max:", max(df$puissance_nominale, na.rm = TRUE), "kW\n")

  avant <- nrow(df)
  df <- df %>% filter(puissance_nominale >= 1 & puissance_nominale <= 500)
  cat("Lignes aberrantes puissance (<1 ou >500 kW) supprimées :", avant - nrow(df), "\n")

  # Normalisation aux paliers standards
  paliers <- c(3.7, 7.4, 11, 22, 50, 75, 100, 150, 175, 200, 250, 300, 350, 400)
  arrondir <- function(x) paliers[which.min(abs(paliers - x))]
  df$puissance_nominale <- sapply(df$puissance_nominale, arrondir)
  cat("puissance_nominale normalisée aux paliers standards\n")
}

# ── 3b. Coordonnées GPS hors France ─────────────────────────
if (all(c("consolidated_longitude", "consolidated_latitude") %in% names(df))) {
  avant <- nrow(df)
  df <- df %>% filter(
    consolidated_longitude >= -5.5  & consolidated_longitude <= 10,
    consolidated_latitude  >= 41    & consolidated_latitude  <= 51.5
  )
  cat("Lignes hors France (GPS) supprimées :", avant - nrow(df), "\n")
}

# ── 3c. nbre_pdc aberrant ────────────────────────────────────
if ("nbre_pdc" %in% names(df)) {
  avant <- nrow(df)
  df <- df %>% filter(nbre_pdc >= 1 & nbre_pdc <= 100)
  cat("Lignes aberrantes nbre_pdc (<1 ou >100) supprimées :", avant - nrow(df), "\n")
}


# ============================================================
# 4. EXPORT
# ============================================================

cat("\n══════════════════════════════════════\n")
cat("  RÉSUMÉ FINAL\n")
cat("══════════════════════════════════════\n")
cat("Dimensions finales :", nrow(df), "lignes x", ncol(df), "colonnes\n")

write.csv(df, "data/export_IA.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Export OK → data/export_IA.csv\n")

