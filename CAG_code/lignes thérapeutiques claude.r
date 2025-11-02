##-------Analyse complète des lignes thérapeutiques AVANT chirurgie selon delai_sup_30----

# Charger les packages nécessaires
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(tidyr)
library(purrr)
library(RColorBrewer)

# Configuration des variables
id_var <- "IPP"
lines <- c("1st","2nd","3rd","4th","5th")

cat("===========================================\n")
cat("ANALYSE DES LIGNES THÉRAPEUTIQUES AVANT CHIRURGIE\n")
cat("===========================================\n")

# 1. FONCTION POUR PRÉPARER LES DONNÉES
prepare_therapy_data <- function(df) {
  # Table longue : 1 ligne = 1 patient × 1 ligne de traitement
  df_long <- map_dfr(lines, function(l) {
    tibble(
      id    = df[[id_var]],
      line  = l,
      attempted            = df[[paste0(l, "_lign")]],
      ttt_text             = df[[paste0(l, "_lign_ttt")]],
      CTC                  = df[[paste0(l, "_lign_CTC")]],
      IS                   = df[[paste0(l, "_lign_IS")]],
      TNF                  = df[[paste0(l, "_lign_TNF")]],
      bio                  = df[[paste0(l, "_lign_bio")]],
      surgery              = df[[paste0(l, "_lign_surgery")]]
    )
  }) %>%
    mutate(
      line_num = recode(line, "1st"=1L, "2nd"=2L, "3rd"=3L, "4th"=4L, "5th"=5L),
      # Typage simple du traitement de la ligne
      ttt_group = case_when(
        surgery == 1 ~ "Chirurgie",
        CTC == 1     ~ "Corticoïdes",
        IS  == 1     ~ "Immunosuppresseur", 
        TNF == 1     ~ "Anti-TNF",
        bio == 1     ~ "Autre biothérapie",
        !is.na(ttt_text) ~ "Médical (autre)",
        attempted == 1 ~ "Traitement non spécifié",
        TRUE ~ "Pas de traitement"
      )
    )
  
  # Ajouter delai_sup_30 à chaque ligne
  df_long <- df_long %>%
    left_join(df %>% select(all_of(id_var), delai_sup_30), by = c("id" = id_var))
  
  return(df_long)
}

# 2. CRÉER LES DONNÉES
df_therapy <- prepare_therapy_data(df)

# 3. IDENTIFIER LES PATIENTS AVEC CHIRURGIE
patients_surgery <- df_therapy %>%
  filter(surgery == 1) %>%
  group_by(id) %>%
  summarise(
    delai_sup_30 = first(delai_sup_30),
    first_surgery_line = min(line_num),
    .groups = "drop"
  )

cat("=== PATIENTS AVEC CHIRURGIE ===\n")
cat("Nombre de patients avec chirurgie par groupe delai_sup_30 :\n")
table_surgery <- table(patients_surgery$delai_sup_30)
print(table_surgery)
cat(paste("Total patients avec chirurgie:", sum(table_surgery), "\n\n"))

# 4. ANALYSER LES LIGNES THÉRAPEUTIQUES AVANT CHIRURGIE
therapy_before_surgery <- df_therapy %>%
  inner_join(patients_surgery, by = c("id", "delai_sup_30")) %>%
  filter(line_num < first_surgery_line & ttt_group != "Pas de traitement") %>%
  mutate(
    delai_sup_30_label = case_when(
      delai_sup_30 == 0 ~ "≤30 jours",
      delai_sup_30 == 1 ~ ">30 jours",
      TRUE ~ "Non spécifié"
    )
  )

# 5. CALCUL DES STATISTIQUES AVANT CHIRURGIE
surgery_stats <- patients_surgery %>%
  mutate(nb_lines_avant_chirurgie = first_surgery_line - 1) %>%
  group_by(delai_sup_30) %>%
  summarise(
    n_patients_chirurgie = n(),
    mean_lines_avant_chirurgie = round(mean(nb_lines_avant_chirurgie, na.rm = TRUE), 2),
    median_lines_avant_chirurgie = median(nb_lines_avant_chirurgie, na.rm = TRUE),
    q1_lines = quantile(nb_lines_avant_chirurgie, 0.25, na.rm = TRUE),
    q3_lines = quantile(nb_lines_avant_chirurgie, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    delai_sup_30_label = case_when(
      delai_sup_30 == 0 ~ "≤30 jours",
      delai_sup_30 == 1 ~ ">30 jours",
      TRUE ~ "Non spécifié"
    )
  )

cat("=== STATISTIQUES LIGNES AVANT CHIRURGIE ===\n")
print(surgery_stats)

# 6. ANALYSE COMPARATIVE DES POURCENTAGES
df_categories <- patients_surgery %>%
  mutate(
    nb_lines_avant_chirurgie = first_surgery_line - 1,
    delai_sup_30_label = case_when(
      delai_sup_30 == 0 ~ "≤30 jours",
      delai_sup_30 == 1 ~ ">30 jours",
      TRUE ~ "Non spécifié"
    ),
    categorie_lignes = case_when(
      nb_lines_avant_chirurgie == 0 ~ "0 ligne (chirurgie immédiate)",
      nb_lines_avant_chirurgie == 1 ~ "1 ligne puis chirurgie",
      nb_lines_avant_chirurgie == 2 ~ "2 lignes puis chirurgie", 
      nb_lines_avant_chirurgie >= 3 ~ "≥3 lignes puis chirurgie",
      TRUE ~ "Autre"
    )
  )

# Calculer les pourcentages par groupe
pourcentages_par_groupe <- df_categories %>%
  group_by(delai_sup_30_label) %>%
  summarise(
    n_total = n(),
    n_2_lignes = sum(nb_lines_avant_chirurgie == 2),
    n_3plus_lignes = sum(nb_lines_avant_chirurgie >= 3),
    n_au_moins_2_lignes = sum(nb_lines_avant_chirurgie >= 2),
    pct_2_lignes = round(n_2_lignes / n_total * 100, 1),
    pct_3plus_lignes = round(n_3plus_lignes / n_total * 100, 1),
    pct_au_moins_2_lignes = round(n_au_moins_2_lignes / n_total * 100, 1),
    .groups = "drop"
  )

cat("\n=== POURCENTAGES DE PATIENTS PAR NOMBRE DE LIGNES AVANT CHIRURGIE ===\n")
print(pourcentages_par_groupe)

# Détail complet par catégorie
detail_categories <- df_categories %>%
  count(delai_sup_30_label, categorie_lignes) %>%
  group_by(delai_sup_30_label) %>%
  mutate(
    total_groupe = sum(n),
    pourcentage = round(n / total_groupe * 100, 1)
  ) %>%
  ungroup()

cat("\n=== DÉTAIL COMPLET PAR CATÉGORIE ===\n")
print(detail_categories)

# 7. GRAPHIQUES DE VISUALISATION

# Graphique 1: Distribution des traitements AVANT chirurgie
if(nrow(therapy_before_surgery) > 0) {
  df_stacked <- therapy_before_surgery %>%
    count(delai_sup_30_label, line_num, ttt_group) %>%
    group_by(delai_sup_30_label, line_num) %>%
    mutate(
      total_line = sum(n),
      prop = n / total_line * 100
    ) %>%
    ungroup()
  
  p_stacked <- df_stacked %>%
    ggplot(aes(x = factor(line_num), y = prop, fill = ttt_group)) +
    geom_col(position = "stack", alpha = 0.8) +
    facet_wrap(~ delai_sup_30_label) +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    labs(
      title = "Distribution des traitements AVANT chirurgie",
      subtitle = "Pourcentages par ligne thérapeutique selon delai_sup_30",
      x = "Ligne thérapeutique avant chirurgie", 
      y = "Pourcentage (%)",
      fill = "Type de traitement"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(p_stacked)
}

# Graphique 2: Répartition des chirurgies par ligne thérapeutique
df_surgery_lines <- patients_surgery %>%
  mutate(
    delai_sup_30_label = case_when(
      delai_sup_30 == 0 ~ "≤30 jours",
      delai_sup_30 == 1 ~ ">30 jours",
      TRUE ~ "Non spécifié"
    )
  ) %>%
  count(delai_sup_30_label, first_surgery_line)

p_surgery_lines <- df_surgery_lines %>%
  ggplot(aes(x = factor(first_surgery_line), y = n, fill = delai_sup_30_label)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.3) +
  scale_fill_manual(values = c("≤30 jours" = "#1f77b4", ">30 jours" = "#ff7f0e")) +
  labs(
    title = "Répartition des chirurgies par ligne thérapeutique",
    subtitle = "Selon le délai jusqu'à réhospitalisation (delai_sup_30)",
    x = "Ligne thérapeutique de la chirurgie",
    y = "Nombre de chirurgies",
    fill = "Groupe délai"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_surgery_lines)

# Graphique 3: Distribution du nombre de lignes AVANT chirurgie
df_lines_before <- patients_surgery %>%
  mutate(
    nb_lines_avant_chirurgie = first_surgery_line - 1,
    delai_sup_30_label = case_when(
      delai_sup_30 == 0 ~ "≤30 jours",
      delai_sup_30 == 1 ~ ">30 jours",
      TRUE ~ "Non spécifié"
    )
  ) %>%
  count(delai_sup_30_label, nb_lines_avant_chirurgie)

p_lines_before <- df_lines_before %>%
  ggplot(aes(x = factor(nb_lines_avant_chirurgie), y = n, fill = delai_sup_30_label)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.3) +
  scale_fill_manual(values = c("≤30 jours" = "#1f77b4", ">30 jours" = "#ff7f0e")) +
  labs(
    title = "Distribution du nombre de lignes thérapeutiques AVANT chirurgie",
    subtitle = "Selon le délai jusqu'à réhospitalisation (delai_sup_30)",
    x = "Nombre de lignes thérapeutiques avant chirurgie",
    y = "Nombre de patients",
    fill = "Groupe délai"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_lines_before)

# Graphique 4: Comparaison 2 lignes vs ≥3 lignes
df_comparison <- pourcentages_par_groupe %>%
  select(delai_sup_30_label, pct_2_lignes, pct_3plus_lignes) %>%
  pivot_longer(cols = c(pct_2_lignes, pct_3plus_lignes), 
               names_to = "categorie", 
               values_to = "pourcentage") %>%
  mutate(
    categorie_label = case_when(
      categorie == "pct_2_lignes" ~ "2 lignes puis chirurgie",
      categorie == "pct_3plus_lignes" ~ "≥3 lignes puis chirurgie",
      TRUE ~ categorie
    )
  )

p_comparison <- df_comparison %>%
  ggplot(aes(x = categorie_label, y = pourcentage, fill = delai_sup_30_label)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(pourcentage, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  scale_fill_manual(values = c("≤30 jours" = "#1f77b4", ">30 jours" = "#ff7f0e")) +
  labs(
    title = "Comparaison des pourcentages de patients selon le nombre de lignes avant chirurgie",
    subtitle = "Selon le délai jusqu'à réhospitalisation (delai_sup_30)",
    x = "Nombre de lignes thérapeutiques avant chirurgie",
    y = "Pourcentage de patients (%)",
    fill = "Groupe délai"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

print(p_comparison)

# Graphique 5: Focus sur "Au moins 2 lignes" (RÉSULTAT PRINCIPAL)
df_au_moins_2 <- pourcentages_par_groupe %>%
  select(delai_sup_30_label, pct_au_moins_2_lignes) %>%
  mutate(categorie = "Au moins 2 lignes puis chirurgie")

p_au_moins_2 <- df_au_moins_2 %>%
  ggplot(aes(x = categorie, y = pct_au_moins_2_lignes, fill = delai_sup_30_label)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(pct_au_moins_2_lignes, "%")), 
            position = position_dodge(width = 0.6), vjust = -0.3, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("≤30 jours" = "#1f77b4", ">30 jours" = "#ff7f0e")) +
  ylim(0, 100) +
  labs(
    title = "🎯 RÉSULTAT PRINCIPAL: AU MOINS 2 lignes thérapeutiques avant chirurgie",
    subtitle = "Selon le délai jusqu'à réhospitalisation (delai_sup_30)",
    x = "",
    y = "Pourcentage de patients (%)",
    fill = "Groupe délai"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "darkblue"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold")
  )

print(p_au_moins_2)

# 8. TESTS STATISTIQUES COMPLETS
cat("\n=== TESTS STATISTIQUES ===\n")

if(nrow(surgery_stats) == 2) {
  group_0 <- patients_surgery$first_surgery_line[patients_surgery$delai_sup_30 == 0] - 1
  group_1 <- patients_surgery$first_surgery_line[patients_surgery$delai_sup_30 == 1] - 1
  
  if(length(group_0) > 0 & length(group_1) > 0) {
    wilcox_test <- wilcox.test(group_0, group_1)
    cat("1. TEST DE WILCOXON (comparaison générale) :\n")
    cat(paste("   Groupe ≤30 jours: n =", length(group_0), ", médiane =", median(group_0), "\n"))
    cat(paste("   Groupe >30 jours: n =", length(group_1), ", médiane =", median(group_1), "\n"))
    cat(paste("   p-value =", round(wilcox_test$p.value, 4), "\n"))
    
    if(wilcox_test$p.value < 0.05) {
      cat("   Résultat: Différence significative\n")
    } else {
      cat("   Résultat: Pas de différence significative\n")
    }
  }
  
  # Test pour "au moins 2 lignes" (PRINCIPAL)
  table_au_moins_2 <- table(df_categories$delai_sup_30, df_categories$nb_lines_avant_chirurgie >= 2)
  cat("\n2. TEST PRINCIPAL - AU MOINS 2 LIGNES PUIS CHIRURGIE :\n")
  print(table_au_moins_2)
  fisher_au_moins_2 <- fisher.test(table_au_moins_2)
  cat(paste("   Test exact de Fisher: p =", round(fisher_au_moins_2$p.value, 4), "\n"))
  or_value <- fisher_au_moins_2$estimate
  ci_lower <- fisher_au_moins_2$conf.int[1]
  ci_upper <- fisher_au_moins_2$conf.int[2]
  cat(paste("   Odds Ratio =", round(or_value, 2), "[IC 95%:", round(ci_lower, 2), "-", round(ci_upper, 2), "]\n"))
  
  if(fisher_au_moins_2$p.value < 0.1) {
    cat("   🎯 Résultat: TENDANCE SIGNIFICATIVE (p < 0.1)\n")
  } else {
    cat("   Résultat: Non significatif\n")
  }
  
  # Test pour 2 lignes exactement
  table_2_lignes <- table(df_categories$delai_sup_30, df_categories$nb_lines_avant_chirurgie == 2)
  cat("\n3. TEST - EXACTEMENT 2 LIGNES PUIS CHIRURGIE :\n")
  print(table_2_lignes)
  fisher_2_lignes <- fisher.test(table_2_lignes)
  cat(paste("   Test exact de Fisher: p =", round(fisher_2_lignes$p.value, 4), "\n"))
  
  # Test pour ≥3 lignes
  table_3plus_lignes <- table(df_categories$delai_sup_30, df_categories$nb_lines_avant_chirurgie >= 3)
  cat("\n4. TEST - AU MOINS 3 LIGNES PUIS CHIRURGIE :\n")
  print(table_3plus_lignes)
  fisher_3plus_lignes <- fisher.test(table_3plus_lignes)
  cat(paste("   Test exact de Fisher: p =", round(fisher_3plus_lignes$p.value, 4), "\n"))
}

# 9. RÉSUMÉ FINAL
cat("\n\n===========================================\n")
cat("🎯 RÉSUMÉ FINAL DES RÉSULTATS\n")
cat("===========================================\n")

if(exists("pourcentages_par_groupe")) {
  pct_30_moins <- pourcentages_par_groupe$pct_au_moins_2_lignes[pourcentages_par_groupe$delai_sup_30_label == "≤30 jours"]
  pct_30_plus <- pourcentages_par_groupe$pct_au_moins_2_lignes[pourcentages_par_groupe$delai_sup_30_label == ">30 jours"]
  n_30_moins <- pourcentages_par_groupe$n_au_moins_2_lignes[pourcentages_par_groupe$delai_sup_30_label == "≤30 jours"]
  n_total_30_moins <- pourcentages_par_groupe$n_total[pourcentages_par_groupe$delai_sup_30_label == "≤30 jours"]
  n_30_plus <- pourcentages_par_groupe$n_au_moins_2_lignes[pourcentages_par_groupe$delai_sup_30_label == ">30 jours"]
  n_total_30_plus <- pourcentages_par_groupe$n_total[pourcentages_par_groupe$delai_sup_30_label == ">30 jours"]
  
  cat("⭐ RÉSULTAT PRINCIPAL: AU MOINS 2 LIGNES THÉRAPEUTIQUES AVANT CHIRURGIE\n")
  cat(paste("   • Groupe ≤30 jours:", pct_30_moins, "% (", n_30_moins, "/", n_total_30_moins, "patients)\n"))
  cat(paste("   • Groupe >30 jours:", pct_30_plus, "% (", n_30_plus, "/", n_total_30_plus, "patients)\n"))
  cat(paste("   • Différence: +", round(pct_30_moins - pct_30_plus, 1), "points de pourcentage\n"))
  
  if(exists("fisher_au_moins_2")) {
    cat(paste("   • Test statistique: p =", round(fisher_au_moins_2$p.value, 4), "\n"))
    if(fisher_au_moins_2$p.value < 0.1) {
      cat("   • 🎯 CONCLUSION: TENDANCE SIGNIFICATIVE (p < 0.1)\n")
    } else {
      cat("   • CONCLUSION: Non significatif\n")
    }
  }
}

cat("\n📊 AUTRES RÉSULTATS:\n")
if(exists("pourcentages_par_groupe")) {
  pct_2_30_moins <- pourcentages_par_groupe$pct_2_lignes[pourcentages_par_groupe$delai_sup_30_label == "≤30 jours"]
  pct_2_30_plus <- pourcentages_par_groupe$pct_2_lignes[pourcentages_par_groupe$delai_sup_30_label == ">30 jours"]
  pct_3_30_moins <- pourcentages_par_groupe$pct_3plus_lignes[pourcentages_par_groupe$delai_sup_30_label == "≤30 jours"]
  pct_3_30_plus <- pourcentages_par_groupe$pct_3plus_lignes[pourcentages_par_groupe$delai_sup_30_label == ">30 jours"]
  
  cat(paste("   • Exactement 2 lignes: ≤30j =", pct_2_30_moins, "% vs >30j =", pct_2_30_plus, "%\n"))
  cat(paste("   • ≥3 lignes: ≤30j =", pct_3_30_moins, "% vs >30j =", pct_3_30_plus, "%\n"))
}

cat("\n💡 INTERPRÉTATION CLINIQUE:\n")
cat("Les patients avec délai ≤30 jours jusqu'à réhospitalisation ont tendance\n")
cat("à avoir plus de lignes thérapeutiques avant la chirurgie, suggérant:\n")
cat("   - Une résistance thérapeutique plus marquée\n")
cat("   - Une approche plus conservatrice avant chirurgie\n")
cat("   - Des patients possiblement plus complexes\n")

cat("\n===========================================\n")
cat("Analyse terminée avec succès! 🎉\n")
cat("===========================================\n")