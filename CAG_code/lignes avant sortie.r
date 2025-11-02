##-------Stats pour traitement de la poussée actuelle----

# ID patient robuste
df <- df %>% mutate(.row_id = row_number())
id_var <- if ("IPP" %in% names(df)) "IPP" else ".row_id"
lines <- c("1st","2nd","3rd","4th","5th")

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
    surgery              = df[[paste0(l, "_lign_surgery")]],
    date                 = df[[paste0(l, "_lign_date")]],
    reponse_med          = df[[paste0(l, "_lign_reponse_ttt_med")]],
    sortie_apres_ttt_med = df[[paste0(l, "_lign_sortie_apres_ttt_med")]]
  )
}) %>%
  mutate(
    line_num = recode(line, "1st"=1L, "2nd"=2L, "3rd"=3L, "4th"=4L, "5th"=5L),
    # Ligne médicale = tout sauf chirurgie
    medical_line = surgery != 1 & (attempted == 1 | !is.na(ttt_text) | CTC==1 | IS==1 | TNF==1 | bio==1),
    # Événement "sorti pendant" (uniquement si ligne médicale)
    event_sortie_med = as.integer(medical_line & sortie_apres_ttt_med == 1),
    # Typage simple du traitement de la ligne
    ttt_group = dplyr::case_when(
      surgery == 1 ~ "Chirurgie",
      CTC == 1     ~ "Corticoïdes généraux",
      IS  == 1     ~ "Immunosuppresseur",
      TNF == 1     ~ "Anti-TNF",
      bio == 1     ~ "Autre biothérapie",
      !is.na(ttt_text) ~ "Médical (autre)",
      TRUE ~ NA_character_
    )
  )

# Caractéristiques patient (niveau patient)
df_feat_base <- df_long %>%
  group_by(id) %>%
  summarise(
    n_lignes_tentees = sum(replace_na(attempted, 0) == 1, na.rm = TRUE),
    any_CTC   = as.integer(any(CTC    == 1, na.rm = TRUE)),
    any_IS    = as.integer(any(IS     == 1, na.rm = TRUE)),
    any_TNF   = as.integer(any(TNF    == 1, na.rm = TRUE)),
    any_bio   = as.integer(any(bio    == 1, na.rm = TRUE)),
    any_surg  = as.integer(any(surgery== 1, na.rm = TRUE)),
    first_surgery_line = if_else(any(surgery == 1, na.rm = TRUE),
                                 min(line_num[surgery == 1], na.rm = TRUE), NA_integer_),
    ever_sortie_med    = as.integer(any(event_sortie_med == 1, na.rm = TRUE)),
    first_sortie_line  = if_else(any(event_sortie_med == 1, na.rm = TRUE),
                                 min(line_num[event_sortie_med == 1], na.rm = TRUE), NA_integer_)
  ) %>% ungroup()

# Type de traitement à la première "sortie pendant"
first_sortie <- df_long %>%
  filter(event_sortie_med == 1) %>%
  group_by(id) %>%
  slice_min(line_num, with_ties = FALSE) %>%
  ungroup() %>%
  select(id, ttt_at_first_sortie = ttt_group)

# Table patient : features + stratificateur + délais
# (les deux délais sont pris depuis df et convertis en numériques si besoin)
df_patient <- df_feat_base %>%
  left_join(first_sortie, by = "id") %>%
  left_join(
    df %>%
      transmute(
        id = .data[[id_var]],
        delai_sup_30,
        delai_dernier_ttt_rehospit = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(delai_dernier_ttt_rehospit)))),
        delai_admission_derniere_hospit = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(delai_admission_derniere_hospit))))
      ),
    by = "id"
  )

# Tableau patient-level (stratifié par delai_sup_30)
cols_t2_patient <- c(
  "n_lignes_tentees",
  "any_CTC", "any_IS", "any_TNF", "any_bio", "any_surg",
  "ever_sortie_med",
  "first_sortie_line",
  "ttt_at_first_sortie",
  "first_surgery_line",
  "delai_dernier_ttt_rehospit",
  "delai_admission_derniere_hospit"
)

tableau_sortie_global <- df_patient %>%
  mutate(across(c(any_CTC, any_IS, any_TNF, any_bio, any_surg, ever_sortie_med), ~ . == 1)) %>%
  tbl_summary(
    by = delai_sup_30,
    include = any_of(cols_t2_patient),
    missing = "ifany",
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ 0
    ),
    label = list(
      n_lignes_tentees                ~ "Nombre de lignes tentées",
      any_CTC                         ~ "Corticoïdes généraux (≥1 ligne)",
      any_IS                          ~ "Immunosuppresseur (≥1 ligne)",
      any_TNF                         ~ "Anti-TNF (≥1 ligne)",
      any_bio                         ~ "Autre biothérapie (≥1 ligne)",
      any_surg                        ~ "Chirurgie (≥1 ligne)",
      ever_sortie_med                 ~ "Sortie pendant un ttt médical",
      first_sortie_line               ~ "Ligne de la 1ère sortie (1–5)",
      ttt_at_first_sortie             ~ "Type de ttt à la 1ère sortie",
      first_surgery_line              ~ "Rang de la 1ère chirurgie",
      delai_dernier_ttt_rehospit      ~ "Délai: dernière ligne → réhospit (j)",
      delai_admission_derniere_hospit ~ "Délai: sortie → réhospit (j)"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

# Affichage
tableau_sortie_global

#détail des lignes de traitement de la poussée
cols_to_include_ttt_poussee <- c(
  "1st_lign", "1st_lign_ttt", "1st_lign_CTC", "1st_lign_IS", "1st_lign_TNF", "1st_lign_bio", "1st_lign_surgery",
  "2nd_lign", "2nd_lign_ttt", "2nd_lign_CTC", "2nd_lign_IS", "2nd_lign_TNF", "2nd_lign_bio", "2nd_lign_surgery",
  "3rd_lign", "3rd_lign_ttt", "3rd_lign_CTC", "3rd_lign_IS", "3rd_lign_TNF", "3rd_lign_bio", "3rd_lign_surgery",
  "4th_lign", "4th_lign_ttt", "4th_lign_CTC", "4th_lign_IS", "4th_lign_TNF", "4th_lign_bio", "4th_lign_surgery",
  "5th_lign", "5th_lign_ttt", "5th_lign_CTC", "5th_lign_IS", "5th_lign_TNF", "5th_lign_bio", "5th_lign_surgery"
)


#tableau_ttt_poussee avec by=
tableau_ttt_poussee2 <- df %>%
  tbl_summary(
    by = delai_sup_30,
    include = all_of(cols_to_include_ttt_poussee),
    missing = "ifany",
    percent = "column",
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ 0)
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**")

# Afficher dans le viewer
tableau_ttt_poussee2



##-------Analyse complète des lignes thérapeutiques AVANT chirurgie selon sortie_pendant_traitement_YN----

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
cat("SELON SORTIE PENDANT TRAITEMENT\n")
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
  
  # Ajouter sortie_pendant_traitement_YN à chaque ligne
  df_long <- df_long %>%
    left_join(df %>% select(all_of(id_var), sortie_pendant_traitement_YN), by = c("id" = id_var))
  
  return(df_long)
}

# 2. CRÉER LES DONNÉES
df_therapy <- prepare_therapy_data(df)

# 3. IDENTIFIER LES PATIENTS AVEC CHIRURGIE
patients_surgery <- df_therapy %>%
  filter(surgery == 1) %>%
  group_by(id) %>%
  summarise(
    sortie_pendant_traitement_YN = first(sortie_pendant_traitement_YN),
    first_surgery_line = min(line_num),
    .groups = "drop"
  )

cat("=== PATIENTS AVEC CHIRURGIE ===\n")
cat("Nombre de patients avec chirurgie par groupe sortie_pendant_traitement_YN :\n")
table_surgery <- table(patients_surgery$sortie_pendant_traitement_YN)
print(table_surgery)
cat(paste("Total patients avec chirurgie:", sum(table_surgery), "\n\n"))

# 4. ANALYSER LES LIGNES THÉRAPEUTIQUES AVANT CHIRURGIE
therapy_before_surgery <- df_therapy %>%
  inner_join(patients_surgery, by = c("id", "sortie_pendant_traitement_YN")) %>%
  filter(line_num < first_surgery_line & ttt_group != "Pas de traitement") %>%
  mutate(
    sortie_pendant_ttt_label = case_when(
      sortie_pendant_traitement_YN == 0 ~ "Pas de sortie pendant ttt",
      sortie_pendant_traitement_YN == 1 ~ "Sortie pendant ttt",
      TRUE ~ "Non spécifié"
    )
  )

# 5. CALCUL DES STATISTIQUES AVANT CHIRURGIE
surgery_stats <- patients_surgery %>%
  mutate(nb_lines_avant_chirurgie = first_surgery_line - 1) %>%
  group_by(sortie_pendant_traitement_YN) %>%
  summarise(
    n_patients_chirurgie = n(),
    mean_lines_avant_chirurgie = round(mean(nb_lines_avant_chirurgie, na.rm = TRUE), 2),
    median_lines_avant_chirurgie = median(nb_lines_avant_chirurgie, na.rm = TRUE),
    q1_lines = quantile(nb_lines_avant_chirurgie, 0.25, na.rm = TRUE),
    q3_lines = quantile(nb_lines_avant_chirurgie, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sortie_pendant_ttt_label = case_when(
      sortie_pendant_traitement_YN == 0 ~ "Pas de sortie pendant ttt",
      sortie_pendant_traitement_YN == 1 ~ "Sortie pendant ttt",
      TRUE ~ "Non spécifié"
    )
  )

cat("=== STATISTIQUES LIGNES AVANT CHIRURGIE ===\n")
print(surgery_stats)

# 6. ANALYSE COMPARATIVE DES POURCENTAGES
df_categories <- patients_surgery %>%
  mutate(
    nb_lines_avant_chirurgie = first_surgery_line - 1,
    sortie_pendant_ttt_label = case_when(
      sortie_pendant_traitement_YN == 0 ~ "Pas de sortie pendant ttt",
      sortie_pendant_traitement_YN == 1 ~ "Sortie pendant ttt",
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
  group_by(sortie_pendant_ttt_label) %>%
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
  count(sortie_pendant_ttt_label, categorie_lignes) %>%
  group_by(sortie_pendant_ttt_label) %>%
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
    count(sortie_pendant_ttt_label, line_num, ttt_group) %>%
    group_by(sortie_pendant_ttt_label, line_num) %>%
    mutate(
      total_line = sum(n),
      prop = n / total_line * 100
    ) %>%
    ungroup()
  
  p_stacked <- df_stacked %>%
    ggplot(aes(x = factor(line_num), y = prop, fill = ttt_group)) +
    geom_col(position = "stack", alpha = 0.8) +
    facet_wrap(~ sortie_pendant_ttt_label) +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    labs(
      title = "Distribution des traitements AVANT chirurgie",
      subtitle = "Pourcentages par ligne thérapeutique selon sortie pendant traitement",
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
    sortie_pendant_ttt_label = case_when(
      sortie_pendant_traitement_YN == 0 ~ "Pas de sortie pendant ttt",
      sortie_pendant_traitement_YN == 1 ~ "Sortie pendant ttt",
      TRUE ~ "Non spécifié"
    )
  ) %>%
  count(sortie_pendant_ttt_label, first_surgery_line)

p_surgery_lines <- df_surgery_lines %>%
  ggplot(aes(x = factor(first_surgery_line), y = n, fill = sortie_pendant_ttt_label)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.3) +
  scale_fill_manual(values = c("Pas de sortie pendant ttt" = "#2ca02c", "Sortie pendant ttt" = "#d62728")) +
  labs(
    title = "Répartition des chirurgies par ligne thérapeutique",
    subtitle = "Selon la sortie pendant traitement",
    x = "Ligne thérapeutique de la chirurgie",
    y = "Nombre de chirurgies",
    fill = "Sortie pendant traitement"
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
    sortie_pendant_ttt_label = case_when(
      sortie_pendant_traitement_YN == 0 ~ "Pas de sortie pendant ttt",
      sortie_pendant_traitement_YN == 1 ~ "Sortie pendant ttt",
      TRUE ~ "Non spécifié"
    )
  ) %>%
  count(sortie_pendant_ttt_label, nb_lines_avant_chirurgie)

p_lines_before <- df_lines_before %>%
  ggplot(aes(x = factor(nb_lines_avant_chirurgie), y = n, fill = sortie_pendant_ttt_label)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.3) +
  scale_fill_manual(values = c("Pas de sortie pendant ttt" = "#2ca02c", "Sortie pendant ttt" = "#d62728")) +
  labs(
    title = "Distribution du nombre de lignes thérapeutiques AVANT chirurgie",
    subtitle = "Selon la sortie pendant traitement",
    x = "Nombre de lignes thérapeutiques avant chirurgie",
    y = "Nombre de patients",
    fill = "Sortie pendant traitement"
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
  select(sortie_pendant_ttt_label, pct_2_lignes, pct_3plus_lignes) %>%
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
  ggplot(aes(x = categorie_label, y = pourcentage, fill = sortie_pendant_ttt_label)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(pourcentage, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  scale_fill_manual(values = c("Pas de sortie pendant ttt" = "#2ca02c", "Sortie pendant ttt" = "#d62728")) +
  labs(
    title = "Comparaison des pourcentages de patients selon le nombre de lignes avant chirurgie",
    subtitle = "Selon la sortie pendant traitement",
    x = "Nombre de lignes thérapeutiques avant chirurgie",
    y = "Pourcentage de patients (%)",
    fill = "Sortie pendant traitement"
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
  select(sortie_pendant_ttt_label, pct_au_moins_2_lignes) %>%
  mutate(categorie = "Au moins 2 lignes puis chirurgie")

p_au_moins_2 <- df_au_moins_2 %>%
  ggplot(aes(x = categorie, y = pct_au_moins_2_lignes, fill = sortie_pendant_ttt_label)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(pct_au_moins_2_lignes, "%")), 
            position = position_dodge(width = 0.6), vjust = -0.3, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("Pas de sortie pendant ttt" = "#2ca02c", "Sortie pendant ttt" = "#d62728")) +
  ylim(0, 100) +
  labs(
    title = "🎯 RÉSULTAT PRINCIPAL: AU MOINS 2 lignes thérapeutiques avant chirurgie",
    subtitle = "Selon la sortie pendant traitement",
    x = "",
    y = "Pourcentage de patients (%)",
    fill = "Sortie pendant traitement"
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
  group_0 <- patients_surgery$first_surgery_line[patients_surgery$sortie_pendant_traitement_YN == 0] - 1
  group_1 <- patients_surgery$first_surgery_line[patients_surgery$sortie_pendant_traitement_YN == 1] - 1
  
  if(length(group_0) > 0 & length(group_1) > 0) {
    wilcox_test <- wilcox.test(group_0, group_1)
    cat("1. TEST DE WILCOXON (comparaison générale) :\n")
    cat(paste("   Groupe sans sortie pendant ttt: n =", length(group_0), ", médiane =", median(group_0), "\n"))
    cat(paste("   Groupe avec sortie pendant ttt: n =", length(group_1), ", médiane =", median(group_1), "\n"))
    cat(paste("   p-value =", round(wilcox_test$p.value, 4), "\n"))
    
    if(wilcox_test$p.value < 0.05) {
      cat("   Résultat: Différence significative\n")
    } else {
      cat("   Résultat: Pas de différence significative\n")
    }
  }
  
  # Test pour "au moins 2 lignes" (PRINCIPAL)
  table_au_moins_2 <- table(df_categories$sortie_pendant_traitement_YN, df_categories$nb_lines_avant_chirurgie >= 2)
  cat("\n2. TEST PRINCIPAL - AU MOINS 2 LIGNES PUIS CHIRURGIE :\n")
  print(table_au_moins_2)
  fisher_au_moins_2 <- fisher.test(table_au_moins_2)
  cat(paste("   Test exact de Fisher: p =", round(fisher_au_moins_2$p.value, 4), "\n"))
  or_value <- fisher_au_moins_2$estimate
  ci_lower <- fisher_au_moins_2$conf.int[1]
  ci_upper <- fisher_au_moins_2$conf.int[2]
  cat(paste("   Odds Ratio =", round(or_value, 2), "[IC 95%:", round(ci_lower, 2), "-", round(ci_upper, 2), "]\n"))
  
  if(fisher_au_moins_2$p.value < 0.05) {
    cat("   🎯 Résultat: SIGNIFICATIF (p < 0.05)\n")
  } else if(fisher_au_moins_2$p.value < 0.1) {
    cat("   🎯 Résultat: TENDANCE SIGNIFICATIVE (p < 0.1)\n")
  } else {
    cat("   Résultat: Non significatif\n")
  }
  
  # Test pour 2 lignes exactement
  table_2_lignes <- table(df_categories$sortie_pendant_traitement_YN, df_categories$nb_lines_avant_chirurgie == 2)
  cat("\n3. TEST - EXACTEMENT 2 LIGNES PUIS CHIRURGIE :\n")
  print(table_2_lignes)
  fisher_2_lignes <- fisher.test(table_2_lignes)
  cat(paste("   Test exact de Fisher: p =", round(fisher_2_lignes$p.value, 4), "\n"))
  
  # Test pour ≥3 lignes
  table_3plus_lignes <- table(df_categories$sortie_pendant_traitement_YN, df_categories$nb_lines_avant_chirurgie >= 3)
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
  pct_pas_sortie <- pourcentages_par_groupe$pct_au_moins_2_lignes[pourcentages_par_groupe$sortie_pendant_ttt_label == "Pas de sortie pendant ttt"]
  pct_sortie <- pourcentages_par_groupe$pct_au_moins_2_lignes[pourcentages_par_groupe$sortie_pendant_ttt_label == "Sortie pendant ttt"]
  n_pas_sortie <- pourcentages_par_groupe$n_au_moins_2_lignes[pourcentages_par_groupe$sortie_pendant_ttt_label == "Pas de sortie pendant ttt"]
  n_total_pas_sortie <- pourcentages_par_groupe$n_total[pourcentages_par_groupe$sortie_pendant_ttt_label == "Pas de sortie pendant ttt"]
  n_sortie <- pourcentages_par_groupe$n_au_moins_2_lignes[pourcentages_par_groupe$sortie_pendant_ttt_label == "Sortie pendant ttt"]
  n_total_sortie <- pourcentages_par_groupe$n_total[pourcentages_par_groupe$sortie_pendant_ttt_label == "Sortie pendant ttt"]
  
  cat("⭐ RÉSULTAT PRINCIPAL: AU MOINS 2 LIGNES THÉRAPEUTIQUES AVANT CHIRURGIE\n")
  cat(paste("   • Pas de sortie pendant ttt:", pct_pas_sortie, "% (", n_pas_sortie, "/", n_total_pas_sortie, "patients)\n"))
  cat(paste("   • Sortie pendant ttt:", pct_sortie, "% (", n_sortie, "/", n_total_sortie, "patients)\n"))
  
  if(length(pct_pas_sortie) > 0 && length(pct_sortie) > 0) {
    cat(paste("   • Différence:", round(pct_sortie - pct_pas_sortie, 1), "points de pourcentage\n"))
  }
  
  if(exists("fisher_au_moins_2")) {
    cat(paste("   • Test statistique: p =", round(fisher_au_moins_2$p.value, 4), "\n"))
    if(fisher_au_moins_2$p.value < 0.05) {
      cat("   • 🎯 CONCLUSION: RÉSULTAT SIGNIFICATIF (p < 0.05)\n")
    } else if(fisher_au_moins_2$p.value < 0.1) {
      cat("   • 🎯 CONCLUSION: TENDANCE SIGNIFICATIVE (p < 0.1)\n")
    } else {
      cat("   • CONCLUSION: Non significatif\n")
    }
  }
}

cat("\n📊 AUTRES RÉSULTATS:\n")
if(exists("pourcentages_par_groupe")) {
  pct_2_pas_sortie <- pourcentages_par_groupe$pct_2_lignes[pourcentages_par_groupe$sortie_pendant_ttt_label == "Pas de sortie pendant ttt"]
  pct_2_sortie <- pourcentages_par_groupe$pct_2_lignes[pourcentages_par_groupe$sortie_pendant_ttt_label == "Sortie pendant ttt"]
  pct_3_pas_sortie <- pourcentages_par_groupe$pct_3plus_lignes[pourcentages_par_groupe$sortie_pendant_ttt_label == "Pas de sortie pendant ttt"]
  pct_3_sortie <- pourcentages_par_groupe$pct_3plus_lignes[pourcentages_par_groupe$sortie_pendant_ttt_label == "Sortie pendant ttt"]
  
  if(length(pct_2_pas_sortie) > 0 && length(pct_2_sortie) > 0) {
    cat(paste("   • Exactement 2 lignes: Pas de sortie =", pct_2_pas_sortie, "% vs Sortie =", pct_2_sortie, "%\n"))
  }
  if(length(pct_3_pas_sortie) > 0 && length(pct_3_sortie) > 0) {
    cat(paste("   • ≥3 lignes: Pas de sortie =", pct_3_pas_sortie, "% vs Sortie =", pct_3_sortie, "%\n"))
  }
}

cat("\n💡 INTERPRÉTATION CLINIQUE:\n")
cat("Cette analyse compare les patients selon qu'ils aient eu ou non\n")
cat("une sortie pendant le traitement de la colite aiguë grave.\n")
cat("Les résultats montrent les différences dans l'escalade thérapeutique\n")
cat("avant recours à la chirurgie selon ce critère.\n")

cat("\n===========================================\n")
cat("Analyse terminée avec succès! 🎉\n")
cat("===========================================\n")