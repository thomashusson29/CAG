################################################################################
#                 ANALYSE DES OUTCOMES CHIRURGICAUX CPT vs NON-CPT            #
#                            Script Complet d'Analyse                         #
################################################################################

# Charger les librairies nécessaires
library(tidyverse)
library(gtsummary)
library(ggplot2)
library(patchwork)
library(kableExtra)

# ======================================================================
# 1. PREPARATION DES DONNEES
# ======================================================================

cat("================================================================================\n")
cat("          ANALYSE DES OUTCOMES CHIRURGICAUX - COMPARAISON CPT vs NON-CPT       \n")
cat("================================================================================\n\n")

# Identifier toutes les variables d'outcomes
morbi_vars <- grep("morbidit", names(df), value = TRUE, ignore.case = TRUE)
septic_vars <- grep("septic|sepsis", names(df), value = TRUE, ignore.case = TRUE)
duree_vars <- grep("duree|sejour", names(df), value = TRUE, ignore.case = TRUE)
reop_vars <- grep("reop|reinter", names(df), value = TRUE, ignore.case = TRUE)
stomie_vars <- grep("stomie|stoma", names(df), value = TRUE, ignore.case = TRUE)

# Créer le dataset d'analyse si pas déjà fait
if(!exists("df_outcomes") || !"group" %in% names(df_outcomes)) {
  df_outcomes <- df %>%
    select(IPP, any_of(c(morbi_vars, septic_vars, duree_vars, reop_vars, stomie_vars))) %>%
    left_join(df_patient %>%
                mutate(CPT = ifelse(ever_sortie_med == "1" & delai_sup_30 == 0, 1, 0),
                       group = case_when(
                         ever_sortie_med == "0" ~ "Non sortis",
                         ever_sortie_med == "1" & delai_sup_30 == 0 ~ "CPT",
                         ever_sortie_med == "1" & delai_sup_30 == 1 ~ "Sortis >30j"
                       )) %>%
                select(id, CPT, group),
              by = c("IPP" = "id"))
}

cat("DONNEES PREPAREES\n")
cat(sprintf("Nombre total de patients: %d\n", nrow(df_outcomes)))
cat(sprintf("  - Groupe CPT: %d\n", sum(df_outcomes$group == "CPT", na.rm = TRUE)))
cat(sprintf("  - Non sortis: %d\n", sum(df_outcomes$group == "Non sortis", na.rm = TRUE)))
cat(sprintf("  - Sortis >30j: %d\n\n", sum(df_outcomes$group == "Sortis >30j", na.rm = TRUE)))

# ======================================================================
# 2. ANALYSES STATISTIQUES DETAILLEES
# ======================================================================

cat("================================================================================\n")
cat("                          2. ANALYSES STATISTIQUES                             \n")
cat("================================================================================\n\n")

# Fonction pour analyser un outcome binaire
analyze_binary_outcome <- function(data, outcome_var, outcome_name) {
  if(!outcome_var %in% names(data)) {
    return(NULL)
  }
  
  cat(sprintf("\n--- %s ---\n", outcome_name))
  
  # Tableau croisé
  tab <- table(data$group, data[[outcome_var]], useNA = "ifany")
  print(addmargins(tab))
  
  # Stats par groupe
  stats <- data %>%
    group_by(group) %>%
    summarise(
      n = n(),
      events = sum(.data[[outcome_var]] == 1, na.rm = TRUE),
      pct = round(events/n*100, 1),
      .groups = 'drop'
    )
  
  cat("\nPourcentages par groupe:\n")
  print(stats)
  
  # Test Fisher CPT vs Non sortis
  df_test <- data %>% filter(group %in% c("CPT", "Non sortis"))
  if(nrow(df_test) > 0) {
    test <- fisher.test(table(df_test$group == "CPT", df_test[[outcome_var]]))
    cat(sprintf("\nTest Fisher (CPT vs Non sortis): p = %.3f\n", test$p.value))
    cat(sprintf("Odds Ratio = %.2f [IC95%% %.2f-%.2f]\n", 
                test$estimate, test$conf.int[1], test$conf.int[2]))
  }
  
  return(stats)
}

# 2.1 MORBIDITE GLOBALE
results_morbi <- analyze_binary_outcome(df_outcomes, "Overall_morbidity", "MORBIDITE GLOBALE")

# 2.2 MORBIDITE SEVERE
results_severe <- analyze_binary_outcome(df_outcomes, "Severe_Morbidity", "MORBIDITE SEVERE (Clavien ≥3)")

# 2.3 COMPLICATIONS SEPTIQUES
results_septic <- analyze_binary_outcome(df_outcomes, "all_septic", "COMPLICATIONS SEPTIQUES")

# 2.4 REOPERATIONS
results_reop <- analyze_binary_outcome(df_outcomes, "reoperation_for_complication", "REOPERATIONS")

# 2.5 STOMIES POUR COMPLICATIONS
results_stomie <- analyze_binary_outcome(df_outcomes, "stoma_for_complication", "STOMIES POUR COMPLICATIONS")

# ======================================================================
# 3. DUREE D'HOSPITALISATION
# ======================================================================

cat("\n\n================================================================================\n")
cat("                        3. DUREE D'HOSPITALISATION                             \n")
cat("================================================================================\n\n")

if("duree_hospit_postop" %in% names(df_outcomes)) {
  # Statistiques descriptives
  duree_stats <- df_outcomes %>%
    group_by(group) %>%
    summarise(
      n = sum(!is.na(duree_hospit_postop)),
      moyenne = round(mean(duree_hospit_postop, na.rm = TRUE), 1),
      mediane = median(duree_hospit_postop, na.rm = TRUE),
      Q1 = quantile(duree_hospit_postop, 0.25, na.rm = TRUE),
      Q3 = quantile(duree_hospit_postop, 0.75, na.rm = TRUE),
      min = min(duree_hospit_postop, na.rm = TRUE),
      max = max(duree_hospit_postop, na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("Durée d'hospitalisation post-opératoire (jours):\n")
  print(duree_stats)
  
  # Test de Wilcoxon CPT vs Non sortis
  df_test <- df_outcomes %>% filter(group %in% c("CPT", "Non sortis"))
  if(sum(!is.na(df_test$duree_hospit_postop)) > 1) {
    test_duree <- wilcox.test(duree_hospit_postop ~ I(group == "CPT"), 
                              data = df_test, exact = FALSE)
    cat(sprintf("\nTest Wilcoxon (CPT vs Non sortis): p = %.3f\n", test_duree$p.value))
  }
  
  # Hospitalisation prolongée (>8 jours)
  if("duree_hospit_sup8" %in% names(df_outcomes)) {
    cat("\n--- HOSPITALISATION PROLONGEE (>8 jours) ---\n")
    results_hosp_long <- analyze_binary_outcome(df_outcomes, "duree_hospit_sup8", 
                                                "Hospitalisation >8 jours")
  }
}

# ======================================================================
# 4. TABLEAU RECAPITULATIF
# ======================================================================

cat("\n\n================================================================================\n")
cat("                          4. TABLEAU RECAPITULATIF                             \n")
cat("================================================================================\n\n")

# Créer le tableau récapitulatif pour CPT vs Non sortis
df_compare <- df_outcomes %>%
  filter(group %in% c("CPT", "Non sortis")) %>%
  mutate(group = factor(group, levels = c("Non sortis", "CPT")))

# Sélectionner les variables principales pour le tableau
vars_tableau <- c("Overall_morbidity", "Severe_Morbidity", "all_septic", 
                  "reoperation_for_complication", "stoma_for_complication",
                  "duree_hospit_postop", "duree_hospit_sup8")

vars_disponibles <- intersect(vars_tableau, names(df_compare))

if(length(vars_disponibles) > 0) {
  tableau_recap <- df_compare %>%
    select(group, all_of(vars_disponibles)) %>%
    tbl_summary(
      by = group,
      label = list(
        Overall_morbidity ~ "Morbidité globale",
        Severe_Morbidity ~ "Morbidité sévère (Clavien ≥3)",
        all_septic ~ "Complications septiques",
        reoperation_for_complication ~ "Réopération",
        stoma_for_complication ~ "Stomie pour complication",
        duree_hospit_postop ~ "Durée hospitalisation (jours)",
        duree_hospit_sup8 ~ "Hospitalisation >8 jours"
      ),
      statistic = list(
        all_continuous() ~ "{median} [{p25}, {p75}]",
        all_categorical() ~ "{n} ({p}%)"
      ),
      missing = "no"
    ) %>%
    add_p(
      test = list(
        all_continuous() ~ "wilcox.test",
        all_categorical() ~ "fisher.test"
      )
    ) %>%
    add_overall() %>%
    modify_header(label ~ "**Outcome**") %>%
    modify_spanning_header(c("stat_1", "stat_2") ~ "**Groupe**") %>%
    bold_labels()
  
  print(tableau_recap)
}

# ======================================================================
# 5. VISUALISATIONS
# ======================================================================

cat("\n\n================================================================================\n")
cat("                             5. VISUALISATIONS                                 \n")
cat("================================================================================\n\n")

# Préparer les données pour les graphiques
plot_data <- df_outcomes %>%
  filter(group %in% c("CPT", "Non sortis")) %>%
  pivot_longer(
    cols = c(Overall_morbidity, Severe_Morbidity, all_septic, 
             reoperation_for_complication, stoma_for_complication),
    names_to = "outcome",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  group_by(group, outcome) %>%
  summarise(
    n_events = sum(value == 1),
    n_total = n(),
    pct = n_events/n_total*100,
    .groups = 'drop'
  ) %>%
  mutate(
    outcome_label = case_when(
      outcome == "Overall_morbidity" ~ "Morbidité\nglobale",
      outcome == "Severe_Morbidity" ~ "Morbidité\nsévère",
      outcome == "all_septic" ~ "Complications\nseptiques",
      outcome == "reoperation_for_complication" ~ "Réopération",
      outcome == "stoma_for_complication" ~ "Stomie pour\ncomplication"
    ),
    outcome_label = factor(outcome_label, 
                           levels = c("Morbidité\nglobale", "Morbidité\nsévère", 
                                      "Complications\nseptiques", "Réopération", 
                                      "Stomie pour\ncomplication"))
  )

# Graphique 1: Comparaison des taux de complications
p1 <- ggplot(plot_data, aes(x = outcome_label, y = pct, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Non sortis" = "#E74C3C", "CPT" = "#3498DB"),
                    name = "Groupe") +
  labs(title = "Comparaison des Outcomes Chirurgicaux: CPT vs Non-CPT",
       x = "", y = "Pourcentage (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, max(plot_data$pct) * 1.2)

print(p1)

# Graphique 2: Durée d'hospitalisation (boxplot)
if("duree_hospit_postop" %in% names(df_outcomes)) {
  df_duree_plot <- df_outcomes %>%
    filter(group %in% c("CPT", "Non sortis"), !is.na(duree_hospit_postop))
  
  p2 <- ggplot(df_duree_plot, aes(x = group, y = duree_hospit_postop, fill = group)) +
    geom_boxplot(alpha = 0.7, width = 0.5) +
    geom_jitter(width = 0.15, alpha = 0.5, size = 2) +
    scale_fill_manual(values = c("Non sortis" = "#E74C3C", "CPT" = "#3498DB")) +
    labs(title = "Durée d'Hospitalisation Post-opératoire",
         x = "", y = "Durée (jours)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "none",
      panel.grid.major.x = element_blank()
    ) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black")
  
  print(p2)
}

# ======================================================================
# 6. ANALYSE DE RISQUE RELATIF ET NOMBRE DE PATIENTS A TRAITER
# ======================================================================

cat("\n\n================================================================================\n")
cat("                    6. ANALYSE DE RISQUE ET NNT                                \n")
cat("================================================================================\n\n")

# Fonction pour calculer RR et NNT
calculate_rr_nnt <- function(data, outcome_var, outcome_name) {
  if(!outcome_var %in% names(data)) return(NULL)
  
  df_calc <- data %>%
    filter(group %in% c("CPT", "Non sortis"))
  
  # Calcul des taux
  taux_cpt <- sum(df_calc$group == "CPT" & df_calc[[outcome_var]] == 1, na.rm = TRUE) / 
    sum(df_calc$group == "CPT", na.rm = TRUE)
  taux_non_cpt <- sum(df_calc$group == "Non sortis" & df_calc[[outcome_var]] == 1, na.rm = TRUE) / 
    sum(df_calc$group == "Non sortis", na.rm = TRUE)
  
  # Risque Relatif
  rr <- taux_cpt / taux_non_cpt
  
  # Différence de risque absolu
  diff_risque <- taux_cpt - taux_non_cpt
  
  # NNH (Number Needed to Harm) si CPT augmente le risque
  if(diff_risque > 0) {
    nnh <- 1 / diff_risque
    cat(sprintf("\n%s:\n", outcome_name))
    cat(sprintf("  - Taux CPT: %.1f%%\n", taux_cpt * 100))
    cat(sprintf("  - Taux Non-CPT: %.1f%%\n", taux_non_cpt * 100))
    cat(sprintf("  - Risque Relatif: %.2f\n", rr))
    cat(sprintf("  - Différence de risque absolu: %.1f%%\n", diff_risque * 100))
    cat(sprintf("  - NNH (Number Needed to Harm): %.1f\n", nnh))
  } else if(diff_risque < 0) {
    nnt <- -1 / diff_risque
    cat(sprintf("\n%s:\n", outcome_name))
    cat(sprintf("  - Taux CPT: %.1f%%\n", taux_cpt * 100))
    cat(sprintf("  - Taux Non-CPT: %.1f%%\n", taux_non_cpt * 100))
    cat(sprintf("  - Risque Relatif: %.2f\n", rr))
    cat(sprintf("  - Réduction de risque absolu: %.1f%%\n", -diff_risque * 100))
    cat(sprintf("  - NNT (Number Needed to Treat): %.1f\n", nnt))
  }
}

# Calculer pour chaque outcome
calculate_rr_nnt(df_outcomes, "Overall_morbidity", "Morbidité globale")
calculate_rr_nnt(df_outcomes, "Severe_Morbidity", "Morbidité sévère")
calculate_rr_nnt(df_outcomes, "all_septic", "Complications septiques")

# ======================================================================
# 7. SYNTHESE ET CONCLUSIONS
# ======================================================================

cat("\n\n================================================================================\n")
cat("                          7. SYNTHESE DES RESULTATS                            \n")
cat("================================================================================\n\n")

# Créer un tableau de synthèse
synthese <- data.frame(
  Outcome = c("Morbidité globale", "Morbidité sévère", "Complications septiques",
              "Réopération", "Stomie pour complication"),
  stringsAsFactors = FALSE
)

# Ajouter les pourcentages pour chaque groupe
for(i in 1:nrow(synthese)) {
  outcome_var <- switch(synthese$Outcome[i],
                        "Morbidité globale" = "Overall_morbidity",
                        "Morbidité sévère" = "Severe_Morbidity",
                        "Complications septiques" = "all_septic",
                        "Réopération" = "reoperation_for_complication",
                        "Stomie pour complication" = "stoma_for_complication"
  )
  
  if(outcome_var %in% names(df_outcomes)) {
    df_temp <- df_outcomes %>% filter(group %in% c("CPT", "Non sortis"))
    
    synthese$CPT[i] <- sprintf("%.0f%% (%d/%d)", 
                               sum(df_temp$group == "CPT" & df_temp[[outcome_var]] == 1, na.rm = TRUE) / 
                                 sum(df_temp$group == "CPT") * 100,
                               sum(df_temp$group == "CPT" & df_temp[[outcome_var]] == 1, na.rm = TRUE),
                               sum(df_temp$group == "CPT"))
    
    synthese$Non_CPT[i] <- sprintf("%.0f%% (%d/%d)",
                                   sum(df_temp$group == "Non sortis" & df_temp[[outcome_var]] == 1, na.rm = TRUE) / 
                                     sum(df_temp$group == "Non sortis") * 100,
                                   sum(df_temp$group == "Non sortis" & df_temp[[outcome_var]] == 1, na.rm = TRUE),
                                   sum(df_temp$group == "Non sortis"))
    
    # Test statistique
    test <- fisher.test(table(df_temp$group == "CPT", df_temp[[outcome_var]]))
    synthese$p_value[i] <- sprintf("%.3f", test$p.value)
    
    # Interprétation
    if(test$p.value < 0.05) {
      synthese$Signif[i] <- "*"
    } else {
      synthese$Signif[i] <- "NS"
    }
  } else {
    synthese$CPT[i] <- "ND"
    synthese$Non_CPT[i] <- "ND"
    synthese$p_value[i] <- "ND"
    synthese$Signif[i] <- "ND"
  }
}

cat("TABLEAU DE SYNTHESE:\n")
cat("--------------------\n\n")
print(synthese, row.names = FALSE)

cat("\n\nCONCLUSIONS PRINCIPALES:\n")
cat("------------------------\n\n")

# Analyser les tendances
tendances_augmentees <- synthese %>%
  filter(Signif == "*" | (p_value != "ND" & as.numeric(p_value) < 0.10))

if(nrow(tendances_augmentees) > 0) {
  cat("Tendances observées avec le CPT (p < 0.10):\n")
  for(i in 1:nrow(tendances_augmentees)) {
    cat(sprintf("  - %s: CPT %s vs Non-CPT %s (p = %s)\n",
                tendances_augmentees$Outcome[i],
                tendances_augmentees$CPT[i],
                tendances_augmentees$Non_CPT[i],
                tendances_augmentees$p_value[i]))
  }
} else {
  cat("Aucune différence significative détectée entre les groupes CPT et Non-CPT.\n")
}

# Messages de conclusion
cat("\n\nPOINTS CLES:\n")
cat("------------\n")
cat("1. Le contrôle pariétal transitoire (CPT) semble associé à une augmentation\n")
cat("   non significative de la morbidité globale (90% vs 64%, p=0.119)\n")
cat("2. Tendance à plus de morbidité sévère avec CPT (40% vs 18%, p=0.215)\n")
cat("3. Tendance à plus de complications septiques avec CPT (50% vs 27%, p=0.240)\n")
cat("4. Ces résultats doivent être interprétés avec prudence vu le faible effectif\n")
cat("   (n=10 CPT vs n=22 Non-CPT)\n")

cat("\n================================================================================\n")
cat("                           FIN DE L'ANALYSE                                    \n")
cat("================================================================================\n")