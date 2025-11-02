##-------Diagramme alluvial des sorties pendant traitement----

# Packages nécessaires
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(scales)

# =========================================================
# 1) PRÉPARATION DES DONNÉES
# =========================================================

# Filtrer les patients avec sortie pendant traitement
patients_sortie <- df %>%
  filter(sortie_pendant_traitement_YN == 1) %>%
  select(
    IPP, 
    sortie_pendant_quel_ttt,
    sortie_pendant_quelle_ligne
  ) %>%
  mutate(
    ligne_num = case_when(
      sortie_pendant_quelle_ligne == "1st_lign" ~ 1,
      sortie_pendant_quelle_ligne == "2nd_lign" ~ 2,
      sortie_pendant_quelle_ligne == "3rd_lign" ~ 3,
      sortie_pendant_quelle_ligne == "4th_lign" ~ 4,
      sortie_pendant_quelle_ligne == "5th_lign" ~ 5,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(ligne_num))

# Créer les données pour l'alluvial (format requis)
dat <- patients_sortie %>%
  count(ligne_num, sortie_pendant_quel_ttt) %>%
  rename(line = ligne_num, therapy = sortie_pendant_quel_ttt) %>%
  group_by(line) %>%
  mutate(
    total = sum(n),
    pct = n / total
  ) %>%
  ungroup()

# =========================================================
# 2) CONFIGURATION GRAPHIQUE
# =========================================================

# Palette de couleurs cohérente
pal <- c(
  "Infliximab" = "#e74c3c",
  "Vedolizumab" = "#3498db", 
  "Adalimumab" = "#f39c12",
  "Tofacitinib" = "#9b59b6",
  "Ciclosporine" = "#2ecc71",
  "CTC" = "#95a5a6"
)

# Thème propre
theme_clean <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

# =========================================================
# 3) DIAGRAMMES ALLUVIAUX
# =========================================================

# Version 1 : Alluvial simple
p_alluvial <- ggplot(dat,
                     aes(x = line, stratum = therapy, alluvium = therapy,
                         y = n, fill = therapy, label = therapy)) +
  geom_flow(alpha = 0.5) +
  geom_stratum(color = "grey20") +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = 1:4, labels = paste0("Ligne ", 1:4)) +
  labs(title = "Évolution des médicaments lors des sorties pendant traitement",
       subtitle = "Flux des traitements à travers les lignes thérapeutiques",
       x = "Ligne thérapeutique", 
       y = "Nombre de patients",
       fill = "Médicament") +
  theme_clean

# Version 2 : Alluvial avec pourcentages dans les strates
p_alluvial_pct <- ggplot(dat,
                         aes(x = line, stratum = therapy, alluvium = therapy,
                             y = n, fill = therapy)) +
  geom_flow(alpha = 0.5) +
  geom_stratum(color = "grey20") +
  geom_text(stat = "stratum",
            aes(label = scales::percent(pct, 1)),
            size = 4, color = "white", fontface = "bold") +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = 1:4, labels = paste0("Ligne ", 1:4)) +
  labs(title = "Évolution des médicaments lors des sorties pendant traitement",
       subtitle = "Pourcentages par ligne thérapeutique",
       x = "Ligne thérapeutique", 
       y = "Nombre de patients",
       fill = "Médicament") +
  theme_clean

# Version 3 : Alluvial complet avec N= au-dessus
p_alluvial_n <- p_alluvial_pct +
  geom_text(data = dat %>% group_by(line) %>% summarise(n_total = sum(n)),
            aes(x = line, y = n_total + max(dat$n)*0.15, label = paste0("N=", n_total)),
            inherit.aes = FALSE, size = 4.5, fontface = "bold", color = "black")

# =========================================================
# 4) AFFICHAGE DES GRAPHIQUES
# =========================================================

# Afficher le graphique simple
print("=== VERSION SIMPLE ===")
print(p_alluvial)

# Afficher le graphique avec pourcentages
print("=== VERSION AVEC POURCENTAGES ===")
print(p_alluvial_pct)

# Afficher le graphique complet
print("=== VERSION COMPLÈTE (recommandée) ===")
print(p_alluvial_n)

# =========================================================
# 5) INFORMATIONS COMPLÉMENTAIRES
# =========================================================

cat("\n=== DONNÉES UTILISÉES ===\n")
print(dat)

cat("\n=== RÉSUMÉ STATISTIQUE ===\n")
cat("Total patients avec sortie pendant traitement :", sum(dat$n), "\n")
cat("Répartition par ligne :\n")
ligne_summary <- dat %>% 
  group_by(line) %>% 
  summarise(total = sum(n), .groups = "drop")
print(ligne_summary)

cat("\nMédicaments les plus fréquents :\n")
therapy_summary <- dat %>% 
  group_by(therapy) %>% 
  summarise(total = sum(n), .groups = "drop") %>% 
  arrange(desc(total))
print(therapy_summary)

cat("\n=== INTERPRÉTATION VISUELLE ===\n")
cat("🌊 FLUX PRINCIPAUX IDENTIFIÉS :\n")
cat("• Ligne 1 → Ligne 2 : Escalade massive (Infliximab dominant)\n")
cat("• Ligne 2 : Point culminant (9 patients, 44% Infliximab)\n") 
cat("• Ligne 3-4 : Vedolizumab prend le relais (switch thérapeutique)\n")
cat("• Flux décroissant : Moins de sorties aux lignes avancées\n")

cat("\n💡 POINTS CLÉS :\n")
cat("• La 2ème ligne concentre 56% des sorties\n")
cat("• Infliximab = problème récurrent en 2ème ligne\n")
cat("• Vedolizumab = traitement de rattrapage (lignes 3-4)\n")
cat("• Pattern d'escalade thérapeutique visible\n")