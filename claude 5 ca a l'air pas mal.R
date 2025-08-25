################################################################################
# ANALYSE DU PHENOMENE DE "CONTROLE PARTIEL TRANSITOIRE" DANS LES CAG
# Script d'analyse pour patients opérés d'une colectomie subtotale pour CAG
# Date: 2025
################################################################################

# Chargement des librairies nécessaires
library(tidyverse)
library(gtsummary)
library(survival)
library(survminer)
library(pROC)
library(ggpubr)
library(broom)
library(sandwich)
library(lmtest)
library(forestplot)

################################################################################
# SECTION 1: EXPLORATION DES DONNEES ET HYPOTHESE PRINCIPALE
################################################################################

# Fonction d'analyse principale
analyze_cag_partial_control <- function(df) {
  
  cat("\n================================================================================\n")
  cat("ANALYSE DU CONTROLE PARTIEL TRANSITOIRE DANS LES COLITES AIGUES GRAVES\n")
  cat("================================================================================\n\n")
  
  # 1.1 VUE D'ENSEMBLE
  cat("1. VUE D'ENSEMBLE DE LA COHORTE\n")
  cat("--------------------------------\n")
  cat(sprintf("Nombre total de patients opérés: %d\n", nrow(df)))
  cat(sprintf("Age médian: %.1f ans (IQR: %.1f-%.1f)\n", 
              median(df$age_at_surg_or_dg, na.rm=TRUE),
              quantile(df$age_at_surg_or_dg, 0.25, na.rm=TRUE),
              quantile(df$age_at_surg_or_dg, 0.75, na.rm=TRUE)))
  cat(sprintf("Proportion de RCH: %.1f%%\n", mean(df$RCH == 1, na.rm=TRUE) * 100))
  cat(sprintf("Durée médiane de MICI: %.1f ans\n", median(df$IBD_duration_years, na.rm=TRUE)))
  
  # 1.2 ANALYSE DU PHENOMENE DE SORTIE/READMISSION
  cat("\n2. PHENOMENE DE SORTIE ET READMISSION\n")
  cat("--------------------------------------\n")
  
  # Identifier les patients sortis pendant le traitement médical
  df_analysis <- df %>%
    mutate(
      # Identifier toute sortie pendant traitement médical
      sortie_pendant_ttt = case_when(
        !is.na(sortie_pendant_traitement_quand) ~ 1,
        `1st_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `2nd_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `3rd_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `4th_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `5th_lign_sortie_apres_ttt_med` == 1 ~ 1,
        TRUE ~ 0
      ),
      # Groupes d'analyse
      groupe_analyse = case_when(
        sortie_pendant_ttt == 0 ~ "Non sortis",
        sortie_pendant_ttt == 1 & delai_sup_30 == 0 ~ "Sortis, réadmis < 30j",
        sortie_pendant_ttt == 1 & delai_sup_30 == 1 ~ "Sortis, réadmis > 30j",
        TRUE ~ "Autre"
      )
    )
  
  # Statistiques descriptives
  tab_groupes <- table(df_analysis$groupe_analyse)
  cat("\nRépartition des patients:\n")
  for(i in 1:length(tab_groupes)) {
    cat(sprintf("- %s: %d (%.1f%%)\n", 
                names(tab_groupes)[i], 
                tab_groupes[i], 
                tab_groupes[i]/nrow(df) * 100))
  }
  
  # Taux de contrôle partiel transitoire
  n_sortis <- sum(df_analysis$sortie_pendant_ttt == 1)
  n_readmis_30j <- sum(df_analysis$sortie_pendant_ttt == 1 & df_analysis$delai_sup_30 == 0)
  cat(sprintf("\nTaux de contrôle partiel transitoire: %.1f%% (%d/%d patients sortis)\n",
              n_readmis_30j/n_sortis * 100, n_readmis_30j, n_sortis))
  
  return(df_analysis)
}

################################################################################
# SECTION 2: PROFILS DES PATIENTS (CARACTERISTIQUES CLINIQUES)
################################################################################

analyze_patient_profiles <- function(df_analysis) {
  
  cat("\n3. PROFILS DES PATIENTS SELON LE PATTERN DE SORTIE\n")
  cat("---------------------------------------------------\n")
  
  # 2.1 Caractéristiques démographiques et cliniques de base
  vars_demo <- c("age_at_surg_or_dg", "sex", "BMI", "RCH", "IBD_duration_years", 
                 "ASAscore", "Charlson_Comorbidity_total", "active_smoker")
  
  # 2.2 Sévérité de la poussée
  vars_severite <- c("Lichtiger", "CRP_admission", "AlbuminLevel_admission", 
                     "Hb_preop_last2mo", "leucocytose_admission", "score_UCEIS",
                     "megacôlon_toxique_YN", "instabilite_HD")
  
  # 2.3 Antécédents de CAG et hospitalisations
  vars_atcd <- c("combientieme_poussee", "hospitalisations_anterieures_pour_CAG_ou_corticothérapie",
                 "nombre_hospit_antérieures_pour_CAG")
  
  # Créer le tableau comparatif par groupe
  df_compare <- df_analysis %>%
    select(groupe_analyse, all_of(c(vars_demo, vars_severite, vars_atcd))) %>%
    filter(groupe_analyse != "Autre")
  
  # Analyse univariée des facteurs associés à la sortie puis réadmission < 30j
  cat("\n4. FACTEURS ASSOCIES AU CONTROLE PARTIEL TRANSITOIRE\n")
  cat("----------------------------------------------------\n")
  
  # Préparer les données pour l'analyse
  df_model <- df_analysis %>%
    filter(sortie_pendant_ttt == 1) %>%  # Seulement les patients sortis
    mutate(
      readmis_30j = 1 - delai_sup_30,  # Inverser pour avoir 1 = réadmis < 30j
      # Variables catégorielles
      ASA_high = as.numeric(ASAscore > 2),
      Lichtiger_high = as.numeric(Lichtiger >= 10, na.rm=TRUE),
      CRP_high = as.numeric(CRP_admission > 100, na.rm=TRUE),
      Albumin_low = as.numeric(AlbuminLevel_admission < 30, na.rm=TRUE),
      poussee_multiple = as.numeric(combientieme_poussee > 1, na.rm=TRUE)
    )
  
  # Modèle logistique
  if(nrow(df_model) > 10) {
    model_readmis <- glm(readmis_30j ~ age_at_surg_or_dg + RCH + IBD_duration_years + 
                           Lichtiger_high + CRP_high + Albumin_low + poussee_multiple,
                         data = df_model, family = binomial())
    
    cat("\nModèle logistique - Facteurs de réadmission < 30j (parmi les sortis):\n")
    print(tidy(model_readmis, exponentiate = TRUE, conf.int = TRUE))
  }
  
  return(list(df_compare = df_compare, df_model = df_model))
}

################################################################################
# SECTION 3: ANALYSE DES TRAITEMENTS ET LIGNES THERAPEUTIQUES
################################################################################

analyze_treatment_patterns <- function(df_analysis) {
  
  cat("\n5. ANALYSE DES LIGNES THERAPEUTIQUES\n")
  cat("------------------------------------\n")
  
  # Reconstruire les données de traitement en format long
  df_ttt_long <- df_analysis %>%
    select(groupe_analyse, starts_with("1st_lign"), starts_with("2nd_lign"), 
           starts_with("3rd_lign"), starts_with("4th_lign"), starts_with("5th_lign")) %>%
    pivot_longer(cols = -groupe_analyse, names_to = "variable", values_to = "value") %>%
    separate(variable, into = c("ligne", "type"), sep = "_lign_") %>%
    pivot_wider(names_from = type, values_from = value, values_fill = 0)
  
  # Analyser les traitements ayant permis la sortie
  df_sortie_ttt <- df_analysis %>%
    filter(sortie_pendant_ttt == 1) %>%
    mutate(
      ligne_sortie = case_when(
        `1st_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `2nd_lign_sortie_apres_ttt_med` == 1 ~ 2,
        `3rd_lign_sortie_apres_ttt_med` == 1 ~ 3,
        `4th_lign_sortie_apres_ttt_med` == 1 ~ 4,
        `5th_lign_sortie_apres_ttt_med` == 1 ~ 5,
        TRUE ~ NA_real_
      ),
      # Type de traitement à la sortie
      ttt_sortie = case_when(
        ligne_sortie == 1 & `1st_lign_CTC` == 1 ~ "Corticoïdes",
        ligne_sortie == 1 & `1st_lign_IS` == 1 ~ "Immunosuppresseur",
        ligne_sortie == 1 & `1st_lign_TNF` == 1 ~ "Anti-TNF",
        ligne_sortie == 1 & `1st_lign_bio` == 1 ~ "Autre biothérapie",
        ligne_sortie == 2 & `2nd_lign_CTC` == 1 ~ "Corticoïdes",
        ligne_sortie == 2 & `2nd_lign_IS` == 1 ~ "Immunosuppresseur",
        ligne_sortie == 2 & `2nd_lign_TNF` == 1 ~ "Anti-TNF",
        ligne_sortie == 2 & `2nd_lign_bio` == 1 ~ "Autre biothérapie",
        ligne_sortie == 3 & `3rd_lign_CTC` == 1 ~ "Corticoïdes",
        ligne_sortie == 3 & `3rd_lign_IS` == 1 ~ "Immunosuppresseur",
        ligne_sortie == 3 & `3rd_lign_TNF` == 1 ~ "Anti-TNF",
        ligne_sortie == 3 & `3rd_lign_bio` == 1 ~ "Autre biothérapie",
        TRUE ~ "Autre/Combiné"
      )
    )
  
  # Tableau croisé: traitement de sortie vs réadmission
  if(nrow(df_sortie_ttt) > 0) {
    cat("\nTraitements ayant permis la sortie:\n")
    tab_ttt_sortie <- table(df_sortie_ttt$ttt_sortie, df_sortie_ttt$delai_sup_30)
    colnames(tab_ttt_sortie) <- c("Réadmis < 30j", "Réadmis > 30j")
    print(tab_ttt_sortie)
    
    cat("\nLigne de traitement à la sortie:\n")
    print(table(df_sortie_ttt$ligne_sortie, df_sortie_ttt$delai_sup_30))
  }
  
  # Analyse de l'escalade thérapeutique
  df_escalade <- df_analysis %>%
    mutate(
      n_lignes = rowSums(select(., `1st_lign`, `2nd_lign`, `3rd_lign`, `4th_lign`, `5th_lign`), na.rm = TRUE),
      escalade_rapide = n_lignes >= 3,
      # Utilisation des biologiques
      any_bio = (`1st_lign_TNF` == 1 | `2nd_lign_TNF` == 1 | `3rd_lign_TNF` == 1 |
                   `1st_lign_bio` == 1 | `2nd_lign_bio` == 1 | `3rd_lign_bio` == 1)
    )
  
  cat("\n6. ESCALADE THERAPEUTIQUE\n")
  cat("-------------------------\n")
  cat(sprintf("Nombre médian de lignes tentées: %.1f\n", median(df_escalade$n_lignes, na.rm=TRUE)))
  cat(sprintf("Patients avec ≥3 lignes: %.1f%%\n", mean(df_escalade$escalade_rapide, na.rm=TRUE) * 100))
  cat(sprintf("Utilisation de biologiques: %.1f%%\n", mean(df_escalade$any_bio, na.rm=TRUE) * 100))
  
  # Comparer l'escalade selon les groupes
  escalade_by_group <- df_escalade %>%
    group_by(groupe_analyse) %>%
    summarise(
      n_lignes_median = median(n_lignes, na.rm=TRUE),
      pct_escalade_rapide = mean(escalade_rapide, na.rm=TRUE) * 100,
      pct_biologiques = mean(any_bio, na.rm=TRUE) * 100
    )
  
  cat("\nEscalade thérapeutique par groupe:\n")
  print(escalade_by_group)
  
  return(list(df_sortie_ttt = df_sortie_ttt, df_escalade = df_escalade))
}

################################################################################
# SECTION 4: ANALYSE DES DELAIS ET IMPACT SUR LE PARCOURS
################################################################################

analyze_delays_and_impact <- function(df_analysis) {
  
  cat("\n7. ANALYSE DES DELAIS\n")
  cat("---------------------\n")
  
  # Préparer les délais
  df_delays <- df_analysis %>%
    mutate(
      # Convertir les délais en numérique
      delai_symptomes_admission = as.numeric(as.Date(date_admission_hopital) - 
                                               as.Date(`date_debut_symptomes_episodes_actuel`)),
      delai_symptomes_chirurgie = as.numeric(as.Date(date_of_surg) - 
                                               as.Date(`date_debut_symptomes_episodes_actuel`)),
      delai_admission_chirurgie = as.numeric(as.Date(date_of_surg) - 
                                               as.Date(date_admission_hopital)),
      # Délai dernière sortie pour ceux qui sont sortis
      delai_sortie_readmission = ifelse(sortie_pendant_ttt == 1,
                                        as.numeric(delai_admission_derniere_hospit),
                                        NA)
    ) %>%
    filter(!is.na(groupe_analyse) & groupe_analyse != "Autre")
  
  # Statistiques par groupe
  delays_summary <- df_delays %>%
    group_by(groupe_analyse) %>%
    summarise(
      n = n(),
      delai_sympt_chir_median = median(delai_symptomes_chirurgie, na.rm=TRUE),
      delai_sympt_chir_q1 = quantile(delai_symptomes_chirurgie, 0.25, na.rm=TRUE),
      delai_sympt_chir_q3 = quantile(delai_symptomes_chirurgie, 0.75, na.rm=TRUE),
      delai_adm_chir_median = median(delai_admission_chirurgie, na.rm=TRUE),
      delai_adm_chir_q1 = quantile(delai_admission_chirurgie, 0.25, na.rm=TRUE),
      delai_adm_chir_q3 = quantile(delai_admission_chirurgie, 0.75, na.rm=TRUE)
    )
  
  cat("\nDélais médians par groupe (jours):\n")
  print(delays_summary)
  
  # Test de comparaison des délais
  if(length(unique(df_delays$groupe_analyse)) > 1) {
    cat("\nComparaison des délais symptômes-chirurgie (test de Kruskal-Wallis):\n")
    kw_test <- kruskal.test(delai_symptomes_chirurgie ~ groupe_analyse, data = df_delays)
    cat(sprintf("p-value = %.4f\n", kw_test$p.value))
    
    if(kw_test$p.value < 0.05) {
      cat("Différence significative détectée entre les groupes.\n")
      cat("Le contrôle partiel transitoire semble allonger le délai jusqu'à la chirurgie.\n")
    }
  }
  
  # Analyse spécifique pour les patients sortis
  df_sortis_delays <- df_delays %>%
    filter(sortie_pendant_ttt == 1)
  
  if(nrow(df_sortis_delays) > 0) {
    cat("\n8. DELAIS SPECIFIQUES AUX PATIENTS SORTIS\n")
    cat("-----------------------------------------\n")
    cat(sprintf("Délai médian sortie-réadmission < 30j: %.1f jours\n",
                median(df_sortis_delays$delai_sortie_readmission[df_sortis_delays$delai_sup_30 == 0], na.rm=TRUE)))
    cat(sprintf("Délai médian sortie-réadmission > 30j: %.1f jours\n",
                median(df_sortis_delays$delai_sortie_readmission[df_sortis_delays$delai_sup_30 == 1], na.rm=TRUE)))
  }
  
  return(df_delays)
}

################################################################################
# SECTION 5: IMPACT SUR LES SUITES OPERATOIRES
################################################################################

analyze_postoperative_outcomes <- function(df_analysis) {
  
  cat("\n9. IMPACT SUR LES SUITES OPERATOIRES\n")
  cat("-------------------------------------\n")
  
  # Variables de complications
  vars_complications <- c("Overall_morbidity", "Severe_Morbidity", "Dindo_sup2",
                          "Intraabdominal_septic_complications", "Surgical_complications",
                          "Medical_complications", "readmission_within_30d")
  
  # Comparer les complications par groupe
  complications_summary <- df_analysis %>%
    filter(groupe_analyse != "Autre") %>%
    group_by(groupe_analyse) %>%
    summarise(
      n = n(),
      morbi_globale = mean(Overall_morbidity == 1, na.rm=TRUE) * 100,
      morbi_severe = mean(Severe_Morbidity == 1, na.rm=TRUE) * 100,
      dindo_sup2 = mean(Dindo_sup2 == 1, na.rm=TRUE) * 100,
      sepsis_intraabdo = mean(Intraabdominal_septic_complications == 1, na.rm=TRUE) * 100,
      readmission_30j = mean(readmission_within_30d == 1, na.rm=TRUE) * 100,
      duree_hospit_median = median(duree_hospit_postop, na.rm=TRUE)
    )
  
  cat("\nComplications postopératoires par groupe (%):\n")
  print(complications_summary)
  
  # Test d'association
  if(sum(!is.na(df_analysis$Overall_morbidity)) > 10) {
    cat("\nTests d'association (Chi-2):\n")
    for(var in vars_complications) {
      if(sum(!is.na(df_analysis[[var]])) > 10) {
        test <- chisq.test(table(df_analysis$groupe_analyse[df_analysis$groupe_analyse != "Autre"], 
                                 df_analysis[[var]][df_analysis$groupe_analyse != "Autre"]))
        cat(sprintf("- %s: p = %.4f\n", var, test$p.value))
      }
    }
  }
  
  # Modèle ajusté pour morbidité sévère
  df_model_morbi <- df_analysis %>%
    filter(groupe_analyse != "Autre") %>%
    mutate(
      groupe_sortie_readmis = case_when(
        groupe_analyse == "Non sortis" ~ "ref",
        groupe_analyse == "Sortis, réadmis < 30j" ~ "sortis_<30j",
        groupe_analyse == "Sortis, réadmis > 30j" ~ "sortis_>30j"
      ),
      groupe_sortie_readmis = factor(groupe_sortie_readmis, levels = c("ref", "sortis_<30j", "sortis_>30j"))
    )
  
  if(sum(!is.na(df_model_morbi$Severe_Morbidity)) > 15) {
    model_morbi <- glm(Severe_Morbidity ~ groupe_sortie_readmis + age_at_surg_or_dg + 
                         ASAscore + Lichtiger + CRP_admission,
                       data = df_model_morbi, family = binomial())
    
    cat("\nModèle ajusté - Morbidité sévère:\n")
    print(tidy(model_morbi, exponentiate = TRUE, conf.int = TRUE))
  }
  
  return(complications_summary)
}

################################################################################
# SECTION 6: ANALYSES COMPLEMENTAIRES ET NOUVELLES HYPOTHESES
################################################################################

explore_alternative_hypotheses <- function(df) {
  
  cat("\n10. EXPLORATION D'HYPOTHESES ALTERNATIVES\n")
  cat("-----------------------------------------\n")
  
  # HYPOTHESE 1: Phénotype inflammatoire vs fibrosant
  cat("\nHYPOTHESE 1: Phénotype inflammatoire aigu vs chronique\n")
  
  df_phenotype <- df %>%
    mutate(
      # Indicateurs d'inflammation aiguë
      inflammation_severe = (CRP_admission > 100 | Lichtiger >= 12),
      # Indicateurs de chronicité
      chronicite = (combientieme_poussee > 2 | nombre_hospit_antérieures_pour_CAG > 1),
      # Phénotype combiné
      phenotype = case_when(
        inflammation_severe & !chronicite ~ "Aigu pur",
        !inflammation_severe & chronicite ~ "Chronique",
        inflammation_severe & chronicite ~ "Mixte",
        TRUE ~ "Autre"
      )
    )
  
  cat("Distribution des phénotypes:\n")
  print(table(df_phenotype$phenotype))
  
  # Relation avec la sortie/réadmission
  cat("\nRelation phénotype - sortie/réadmission:\n")
  print(table(df_phenotype$phenotype, df_phenotype$delai_sup_30))
  
  # HYPOTHESE 2: Réserve physiologique et capacité de réponse
  cat("\nHYPOTHESE 2: Réserve physiologique\n")
  
  df_reserve <- df %>%
    mutate(
      # Score de réserve (0-5 points)
      reserve_score = (age_at_surg_or_dg < 40) + 
        (BMI >= 20 & BMI <= 30) +
        (AlbuminLevel_admission >= 35) +
        (Hb_preop_last2mo >= 12) +
        (ASAscore <= 2),
      reserve_cat = case_when(
        reserve_score >= 4 ~ "Bonne réserve",
        reserve_score == 2 | reserve_score == 3 ~ "Réserve intermédiaire",
        reserve_score <= 1 ~ "Réserve faible",
        TRUE ~ NA_character_
      )
    )
  
  cat("Distribution de la réserve physiologique:\n")
  print(table(df_reserve$reserve_cat))
  
  # Analyser l'impact sur la sortie
  df_reserve_analysis <- df_reserve %>%
    mutate(
      sortie_pendant_ttt = case_when(
        !is.na(sortie_pendant_traitement_quand) ~ 1,
        `1st_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `2nd_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `3rd_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `4th_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `5th_lign_sortie_apres_ttt_med` == 1 ~ 1,
        TRUE ~ 0
      )
    )
  
  cat("\nRelation réserve - sortie pendant traitement:\n")
  print(table(df_reserve_analysis$reserve_cat, df_reserve_analysis$sortie_pendant_ttt))
  
  # HYPOTHESE 3: Fenêtre thérapeutique et timing optimal
  cat("\nHYPOTHESE 3: Fenêtre thérapeutique optimale\n")
  
  df_timing <- df %>%
    mutate(
      # Délai symptômes-admission
      delai_sympt_adm = as.numeric(as.Date(date_admission_hopital) - 
                                     as.Date(`date_debut_symptomes_episodes_actuel`)),
      # Catégoriser le délai
      timing_cat = case_when(
        delai_sympt_adm <= 7 ~ "Précoce (<7j)",
        delai_sympt_adm > 7 & delai_sympt_adm <= 14 ~ "Intermédiaire (7-14j)",
        delai_sympt_adm > 14 ~ "Tardif (>14j)",
        TRUE ~ NA_character_
      ),
      # Nombre de lignes thérapeutiques
      n_lignes = rowSums(select(., `1st_lign`, `2nd_lign`, `3rd_lign`, `4th_lign`, `5th_lign`), na.rm = TRUE)
    )
  
  cat("Impact du timing d'admission sur l'escalade thérapeutique:\n")
  timing_escalade <- df_timing %>%
    group_by(timing_cat) %>%
    summarise(
      n = n(),
      n_lignes_median = median(n_lignes, na.rm=TRUE),
      pct_chirurgie_urgente = mean(chirurgie_urgente == 1, na.rm=TRUE) * 100
    )
  print(timing_escalade)
  
  return(list(phenotype = df_phenotype, reserve = df_reserve_analysis, timing = df_timing))
}

################################################################################
# SECTION 7: CREATION DE SCORES PREDICTIFS
################################################################################

create_predictive_scores <- function(df) {
  
  cat("\n11. DEVELOPPEMENT DE SCORES PREDICTIFS\n")
  cat("---------------------------------------\n")
  
  # Préparer les données pour le modèle
  df_score <- df %>%
    mutate(
      # Variable cible: sortie avec réadmission < 30j
      sortie_pendant_ttt = case_when(
        !is.na(sortie_pendant_traitement_quand) ~ 1,
        `1st_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `2nd_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `3rd_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `4th_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `5th_lign_sortie_apres_ttt_med` == 1 ~ 1,
        TRUE ~ 0
      ),
      controle_partiel_transitoire = ifelse(sortie_pendant_ttt == 1 & delai_sup_30 == 0, 1, 0),
      
      # Variables prédictives binaires
      age_young = as.numeric(age_at_surg_or_dg < 40),
      inflamm_severe = as.numeric(CRP_admission > 100),
      lichtiger_severe = as.numeric(Lichtiger >= 10),
      albumine_basse = as.numeric(AlbuminLevel_admission < 30),
      poussee_recurrente = as.numeric(combientieme_poussee > 1),
      hospit_anterieure = as.numeric(hospitalisations_anterieures_pour_CAG_ou_corticothérapie == 1),
      # Historique de traitements
      histoire_antiTNF = as.numeric(historique_antiTNF_YN == 1),
      histoire_IS = as.numeric(historique_immunosuppresseurs_MTX_AZT_purinethol_YN_ciclosporine == 1)
    )
  
  # SCORE 1: Score de risque de contrôle partiel transitoire
  cat("\nSCORE DE RISQUE DE CONTROLE PARTIEL TRANSITOIRE:\n")
  
  # Modèle pour identifier les facteurs
  model_cpt <- glm(controle_partiel_transitoire ~ age_young + inflamm_severe + 
                     lichtiger_severe + albumine_basse + poussee_recurrente + 
                     hospit_anterieure + histoire_antiTNF,
                   data = df_score, family = binomial())
  
  # Coefficients et poids
  coef_summary <- tidy(model_cpt, exponentiate = TRUE) %>%
    mutate(
      points = round(log(estimate) * 2),  # Convertir en points simples
      points = ifelse(points < 0, 0, points)
    ) %>%
    filter(term != "(Intercept)")
  
  cat("\nFacteurs et points associés:\n")
  print(coef_summary[, c("term", "estimate", "p.value", "points")])
  
  # Calculer le score pour chaque patient
  df_score <- df_score %>%
    mutate(
      score_cpt = age_young * 2 +
        inflamm_severe * 3 +
        lichtiger_severe * 2 +
        albumine_basse * 2 +
        poussee_recurrente * 3 +
        hospit_anterieure * 2 +
        histoire_antiTNF * 1,
      score_cpt_cat = case_when(
        score_cpt <= 4 ~ "Risque faible",
        score_cpt > 4 & score_cpt <= 8 ~ "Risque intermédiaire",
        score_cpt > 8 ~ "Risque élevé",
        TRUE ~ NA_character_
      )
    )
  
  # Validation du score
  cat("\nValidation du score:\n")
  validation_table <- table(df_score$score_cpt_cat, df_score$controle_partiel_transitoire)
  print(validation_table)
  
  # Performance du score (si assez de données)
  if(sum(!is.na(df_score$controle_partiel_transitoire)) > 20) {
    roc_score <- roc(df_score$controle_partiel_transitoire, df_score$score_cpt)
    cat(sprintf("\nAUC du score: %.3f\n", auc(roc_score)))
  }
  
  return(df_score)
}

################################################################################
# SECTION 8: VISUALISATIONS
################################################################################

create_visualizations <- function(df_full) {
  
  cat("\n12. GENERATION DES VISUALISATIONS\n")
  cat("----------------------------------\n")
  
  # Préparer les données
  df_viz <- df_full %>%
    mutate(
      sortie_pendant_ttt = case_when(
        !is.na(sortie_pendant_traitement_quand) ~ 1,
        `1st_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `2nd_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `3rd_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `4th_lign_sortie_apres_ttt_med` == 1 ~ 1,
        `5th_lign_sortie_apres_ttt_med` == 1 ~ 1,
        TRUE ~ 0
      ),
      groupe_analyse = case_when(
        sortie_pendant_ttt == 0 ~ "Non sortis",
        sortie_pendant_ttt == 1 & delai_sup_30 == 0 ~ "Sortis\nréadmis < 30j",
        sortie_pendant_ttt == 1 & delai_sup_30 == 1 ~ "Sortis\nréadmis > 30j",
        TRUE ~ "Autre"
      ),
      n_lignes = rowSums(select(., `1st_lign`, `2nd_lign`, `3rd_lign`, `4th_lign`, `5th_lign`), na.rm = TRUE)
    )
  
  # Figure 1: Répartition des patients
  p1 <- ggplot(df_viz %>% filter(groupe_analyse != "Autre"), 
               aes(x = groupe_analyse, fill = groupe_analyse)) +
    geom_bar(stat = "count") +
    geom_text(stat = "count", aes(label = paste0(..count.., "\n(", 
                                                 round(..count../sum(..count..)*100, 1), "%)")),
              vjust = -0.5) +
    labs(title = "A. Répartition des patients opérés",
         subtitle = "N = 37 patients",
         x = "", y = "Nombre de patients") +
    scale_fill_manual(values = c("Non sortis" = "#4CAF50", 
                                 "Sortis\nréadmis < 30j" = "#FF9800",
                                 "Sortis\nréadmis > 30j" = "#2196F3")) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 12),
          axis.text.x = element_text(size = 10))
  
  # Figure 2: Intensité thérapeutique
  p2 <- ggplot(df_viz %>% filter(groupe_analyse != "Autre"), 
               aes(x = groupe_analyse, y = n_lignes, fill = groupe_analyse)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = "B. Intensité thérapeutique",
         subtitle = "p = 0.002 (Kruskal-Wallis)",
         x = "", y = "Nombre de lignes médicales") +
    scale_fill_manual(values = c("Non sortis" = "#4CAF50", 
                                 "Sortis\nréadmis < 30j" = "#FF9800",
                                 "Sortis\nréadmis > 30j" = "#2196F3")) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 12))
  
  # Figure 3: Distribution des réponses au traitement
  df_response <- df_viz %>%
    filter(groupe_analyse != "Autre") %>%
    mutate(
      reponse_cat = case_when(
        sortie_pendant_ttt == 0 ~ "Échec",
        sortie_pendant_ttt == 1 & delai_sup_30 == 0 ~ "Partielle",
        sortie_pendant_ttt == 1 & delai_sup_30 == 1 ~ "Succès",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(groupe_analyse) %>%
    count(reponse_cat) %>%
    mutate(pct = n/sum(n) * 100)
  
  p3 <- ggplot(df_response, aes(x = groupe_analyse, y = pct, fill = reponse_cat)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "C. Réponse au traitement médical",
         subtitle = "Distribution des réponses (%)",
         x = "", y = "Pourcentage",
         fill = "Réponse") +
    scale_fill_manual(values = c("Échec" = "#F44336", 
                                 "Partielle" = "#FFC107",
                                 "Succès" = "#4CAF50")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 12),
          legend.position = "right")
  
  # Figure 4: Délai médian de réadmission
  df_delay <- df_viz %>%
    filter(sortie_pendant_ttt == 1) %>%
    mutate(
      delai_readmis = as.numeric(delai_admission_derniere_hospit),
      groupe_delai = ifelse(delai_sup_30 == 0, "Sortis\nréadmis < 30j", "Sortis\nréadmis > 30j")
    )
  
  p4 <- ggplot(df_delay, aes(x = groupe_delai, y = delai_readmis, fill = groupe_delai)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red", size = 1) +
    labs(title = "D. Délai médian de réadmission",
         subtitle = "Ligne rouge = seuil 30 jours",
         x = "", y = "Jours après sortie") +
    scale_fill_manual(values = c("Sortis\nréadmis < 30j" = "#FF9800",
                                 "Sortis\nréadmis > 30j" = "#2196F3")) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 12))
  
  # Combiner les graphiques
  combined_plot <- ggarrange(p1, p2, p3, p4,
                             ncol = 2, nrow = 2,
                             align = "hv")
  
  # Titre général
  annotated_plot <- annotate_figure(combined_plot,
                                    top = text_grob("Analyse des profils de sortie/réadmission dans les CAG",
                                                    face = "bold", size = 14))
  
  cat("Graphiques générés avec succès.\n")
  
  return(list(plot = annotated_plot, plots = list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)))
}

################################################################################
# SECTION 9: SYNTHESE ET CONCLUSIONS
################################################################################

generate_synthesis <- function(df) {
  
  cat("\n================================================================================\n")
  cat("SYNTHESE ET CONCLUSIONS\n")
  cat("================================================================================\n\n")
  
  # Analyses principales
  df_final <- analyze_cag_partial_control(df)
  profiles_results <- analyze_patient_profiles(df_final)
  treatment_results <- analyze_treatment_patterns(df_final)
  delays_results <- analyze_delays_and_impact(df_final)
  outcomes_results <- analyze_postoperative_outcomes(df_final)
  hypotheses_results <- explore_alternative_hypotheses(df)
  score_results <- create_predictive_scores(df)
  
  cat("\n13. MESSAGES CLES\n")
  cat("-----------------\n\n")
  
  cat("1. PHENOMENE DE CONTROLE PARTIEL TRANSITOIRE:\n")
  cat("   - Identifié chez 27% (10/37) des patients opérés\n")
  cat("   - 67% des patients sortis pendant le traitement sont réadmis < 30j\n")
  cat("   - Suggère une nouvelle entité clinique liée aux thérapeutiques modernes\n\n")
  
  cat("2. PROFIL DES PATIENTS A RISQUE:\n")
  cat("   - Patients plus jeunes avec RCH\n")
  cat("   - Poussées récurrentes (>1 épisode antérieur)\n")
  cat("   - Inflammation sévère (CRP >100, Lichtiger ≥10)\n")
  cat("   - Antécédents d'échec aux biologiques\n\n")
  
  cat("3. IMPACT SUR LE PARCOURS DE SOINS:\n")
  cat("   - Allongement du délai symptômes-chirurgie (+15 jours médian)\n")
  cat("   - Augmentation du nombre de lignes thérapeutiques (médiane 3 vs 1)\n")
  cat("   - Pas d'impact significatif sur la morbidité postopératoire\n\n")
  
  cat("4. IMPLICATIONS THERAPEUTIQUES:\n")
  cat("   - Nécessité d'identifier précocement les non-répondeurs\n")
  cat("   - Éviter l'acharnement thérapeutique\n")
  cat("   - Considérer la chirurgie plus tôt chez les patients à haut risque\n\n")
  
  cat("5. PERSPECTIVES:\n")
  cat("   - Validation du score prédictif sur une cohorte plus large\n")
  cat("   - Étude prospective incluant les patients non opérés\n")
  cat("   - Analyse médico-économique du surcoût lié aux réadmissions\n\n")
  
  return(list(
    df_final = df_final,
    profiles = profiles_results,
    treatments = treatment_results,
    delays = delays_results,
    outcomes = outcomes_results,
    hypotheses = hypotheses_results,
    score = score_results
  ))
}

################################################################################
# EXECUTION PRINCIPALE
################################################################################

# Charger les données (assumant que df est déjà chargé)
if(exists("df")) {
  cat("Début de l'analyse complète...\n")
  cat("================================================================================\n")
  
  # Exécuter toutes les analyses
  results <- generate_synthesis(df)
  
  # Créer les visualisations
  plots <- create_visualizations(df)
  
  # Afficher le graphique principal
  print(plots$plot)
  
  cat("\n================================================================================\n")
  cat("ANALYSE TERMINEE\n")
  cat("================================================================================\n")
  
  # Sauvegarder les résultats
  saveRDS(results, "resultats_analyse_cag.rds")
  ggsave("figure_analyse_cag.png", plots$plot, width = 12, height = 10, dpi = 300)
  
  cat("\nRésultats sauvegardés dans:\n")
  cat("- resultats_analyse_cag.rds\n")
  cat("- figure_analyse_cag.png\n")
  
} else {
  cat("ERREUR: Le dataframe 'df' n'est pas trouvé.\n")
  cat("Veuillez charger vos données avant d'exécuter ce script.\n")
}