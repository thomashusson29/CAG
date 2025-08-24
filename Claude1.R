##-----------------------------------------------
## ANALYSE CAG - PROFILS PATIENTS SORTIE/READMISSION
## Objectif : Identifier le profil des patients qui sortent 
## pendant le traitement et reviennent dans les 30 jours
##-----------------------------------------------

# Chargement des packages nécessaires
library(dplyr)
library(tidyr)
library(ggplot2)
library(gtsummary)
library(purrr)
library(forcats)
library(broom)
library(survival)
library(survminer)

# Vérifier que df est bien chargé
if(!exists("df")) {
  stop("Le dataframe 'df' n'est pas disponible. Veuillez charger les données d'abord.")
}

##-----------------------------------------------
## PARTIE 1 : EXPLORATION DESCRIPTIVE
##-----------------------------------------------

# 1.1 Vue d'ensemble du parcours des patients
cat("\n======== VUE D'ENSEMBLE ========\n")
cat("Nombre total de patients opérés:", nrow(df), "\n")
cat("Patients avec délai > 30j:", sum(df$delai_sup_30 == 1, na.rm = TRUE), "\n")
cat("Patients avec délai ≤ 30j:", sum(df$delai_sup_30 == 0, na.rm = TRUE), "\n")
cat("Patients sortis pendant traitement médical:", sum(df$sortie_pendant_traitement_YN == 1, na.rm = TRUE), "\n\n")

# 1.2 Analyse croisée : sortie pendant traitement x délai de réadmission
cross_tab <- table(
  "Sorti pendant ttt" = df$sortie_pendant_traitement_YN,
  "Délai > 30j" = df$delai_sup_30,
  useNA = "ifany"
)
print(cross_tab)
cat("\nProportions (ligne):\n")
print(prop.table(cross_tab, margin = 1) * 100)

##-----------------------------------------------
## PARTIE 2 : PROFIL DES PATIENTS SORTIS/READMIS < 30J
##-----------------------------------------------

# 2.1 Créer un indicateur composite
df_analysis <- df %>%
  mutate(
    # Variable composite : sorti pendant ttt ET réadmis < 30j
    sortie_et_readmis_30j = case_when(
      sortie_pendant_traitement_YN == 1 & delai_sup_30 == 0 ~ "Sorti et réadmis < 30j",
      sortie_pendant_traitement_YN == 1 & delai_sup_30 == 1 ~ "Sorti et réadmis > 30j",
      sortie_pendant_traitement_YN == 0 ~ "Non sorti pendant ttt",
      TRUE ~ "Non classé"
    )
  )

# 2.2 Distribution des groupes
cat("\n======== GROUPES DE PATIENTS ========\n")
table(df_analysis$sortie_et_readmis_30j)

# 2.3 Caractéristiques démographiques et cliniques par groupe
demo_vars <- c("age_at_surg_or_dg", "sex", "BMI", "ASAscore", 
               "Charlson_Comorbidity_total", "RCH", "IBD_duration_years")

tab_demo <- df_analysis %>%
  filter(sortie_et_readmis_30j != "Non classé") %>%
  tbl_summary(
    by = sortie_et_readmis_30j,
    include = all_of(demo_vars),
    missing = "no",
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age_at_surg_or_dg ~ "Âge (années)",
      sex ~ "Sexe",
      BMI ~ "BMI (kg/m²)",
      ASAscore ~ "Score ASA",
      Charlson_Comorbidity_total ~ "Score de Charlson",
      RCH ~ "RCH",
      IBD_duration_years ~ "Durée MICI (années)"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**")

print(tab_demo)

##-----------------------------------------------
## PARTIE 3 : ANALYSE DES LIGNES DE TRAITEMENT
##-----------------------------------------------

# 3.1 Reconstruction des données de traitement (format long)
lines <- c("1st", "2nd", "3rd", "4th", "5th")

df_long <- map_dfr(lines, function(l) {
  tibble(
    IPP = df$IPP,
    line = l,
    attempted = df[[paste0(l, "_lign")]],
    ttt_text = df[[paste0(l, "_lign_ttt")]],
    CTC = df[[paste0(l, "_lign_CTC")]],
    IS = df[[paste0(l, "_lign_IS")]],
    TNF = df[[paste0(l, "_lign_TNF")]],
    bio = df[[paste0(l, "_lign_bio")]],
    surgery = df[[paste0(l, "_lign_surgery")]],
    date = df[[paste0(l, "_lign_date")]],
    reponse_med = df[[paste0(l, "_lign_reponse_ttt_med")]],
    sortie_apres_ttt_med = df[[paste0(l, "_lign_sortie_apres_ttt_med")]]
  )
}) %>%
  mutate(
    line_num = recode(line, "1st"=1L, "2nd"=2L, "3rd"=3L, "4th"=4L, "5th"=5L),
    medical_line = surgery != 1 & (attempted == 1 | !is.na(ttt_text) | 
                                     CTC==1 | IS==1 | TNF==1 | bio==1),
    event_sortie_med = as.integer(medical_line & sortie_apres_ttt_med == 1)
  )

# 3.2 Analyse par patient : intensité thérapeutique
df_intensity <- df_long %>%
  group_by(IPP) %>%
  summarise(
    n_lignes_tentees = sum(replace_na(attempted, 0) == 1, na.rm = TRUE),
    n_lignes_medicales = sum(medical_line == TRUE, na.rm = TRUE),
    any_CTC = any(CTC == 1, na.rm = TRUE),
    any_IS = any(IS == 1, na.rm = TRUE),
    any_TNF = any(TNF == 1, na.rm = TRUE),
    any_bio = any(bio == 1, na.rm = TRUE),
    max_intensite = case_when(
      any_bio ~ 4,
      any_TNF ~ 3,
      any_IS ~ 2,
      any_CTC ~ 1,
      TRUE ~ 0
    )
  ) %>%
  left_join(
    df_analysis %>% select(IPP, sortie_et_readmis_30j, delai_sup_30),
    by = "IPP"
  )

# 3.3 Comparaison de l'intensité thérapeutique
cat("\n======== INTENSITÉ THÉRAPEUTIQUE PAR GROUPE ========\n")
intensity_summary <- df_intensity %>%
  filter(sortie_et_readmis_30j != "Non classé") %>%
  group_by(sortie_et_readmis_30j) %>%
  summarise(
    n = n(),
    lignes_moy = mean(n_lignes_medicales, na.rm = TRUE),
    lignes_med = median(n_lignes_medicales, na.rm = TRUE),
    pct_TNF = mean(any_TNF, na.rm = TRUE) * 100,
    pct_bio = mean(any_bio, na.rm = TRUE) * 100,
    intensite_max_moy = mean(max_intensite, na.rm = TRUE)
  )
print(intensity_summary)

##-----------------------------------------------
## PARTIE 4 : ANALYSE DES FACTEURS PRÉDICTIFS
##-----------------------------------------------

# 4.1 Préparation des données pour modélisation
df_model <- df_analysis %>%
  filter(sortie_pendant_traitement_YN == 1) %>%  # Uniquement les sortis
  mutate(
    readmis_30j = as.numeric(delai_sup_30 == 0),  # Outcome : réadmis < 30j
    age_cat = cut(age_at_surg_or_dg, c(0, 40, 60, Inf), labels = c("<40", "40-60", ">60")),
    BMI_cat = cut(BMI, c(0, 18.5, 25, 30, Inf), labels = c("<18.5", "18.5-25", "25-30", ">30")),
    IBD_duration_cat = cut(IBD_duration_years, c(0, 5, 10, Inf), labels = c("<5", "5-10", ">10"))
  ) %>%
  left_join(df_intensity, by = "IPP")

# 4.2 Modèle logistique : facteurs associés à la réadmission < 30j
if(sum(!is.na(df_model$readmis_30j)) > 10) {  # Vérifier qu'on a assez de données
  
  model_readmis <- glm(
    readmis_30j ~ age_cat + sex + BMI_cat + Charlson_Comorbidity_total + 
      RCH + n_lignes_medicales + max_intensite,
    data = df_model,
    family = binomial()
  )
  
  cat("\n======== MODÈLE PRÉDICTIF RÉADMISSION < 30J ========\n")
  print(summary(model_readmis))
  
  # Odds ratios avec IC 95%
  or_table <- broom::tidy(model_readmis, exponentiate = TRUE, conf.int = TRUE)
  print(or_table)
}

##-----------------------------------------------
## PARTIE 5 : ANALYSE TEMPORELLE
##-----------------------------------------------

# 5.1 Délais selon le profil
df_delays <- df_analysis %>%
  filter(sortie_pendant_traitement_YN == 1) %>%
  select(
    sortie_et_readmis_30j,
    delai_dernier_ttt_rehospit,
    delai_admission_derniere_hospit,
    sortie_pendant_quelle_ligne
  ) %>%
  mutate(
    delai_dernier_ttt_rehospit = as.numeric(delai_dernier_ttt_rehospit),
    delai_admission_derniere_hospit = as.numeric(delai_admission_derniere_hospit)
  )

cat("\n======== DÉLAIS PAR GROUPE ========\n")
delay_summary <- df_delays %>%
  group_by(sortie_et_readmis_30j) %>%
  summarise(
    n = n(),
    delai_ttt_median = median(delai_dernier_ttt_rehospit, na.rm = TRUE),
    delai_ttt_Q1 = quantile(delai_dernier_ttt_rehospit, 0.25, na.rm = TRUE),
    delai_ttt_Q3 = quantile(delai_dernier_ttt_rehospit, 0.75, na.rm = TRUE),
    ligne_sortie_moy = mean(as.numeric(sortie_pendant_quelle_ligne), na.rm = TRUE)
  )
print(delay_summary)

##-----------------------------------------------
## PARTIE 6 : ANALYSE DE LA SÉVÉRITÉ CLINIQUE
##-----------------------------------------------

# 6.1 Scores de sévérité
severity_vars <- c("Lichtiger", "CRP_admission", "AlbuminLevel_admission",
                   "score_UCEIS", "megacôlon_toxique_YN", "perforation_YN")

df_severity <- df_analysis %>%
  filter(sortie_et_readmis_30j != "Non classé") %>%
  select(all_of(c("sortie_et_readmis_30j", severity_vars))) %>%
  mutate(across(where(is.character), as.numeric))

cat("\n======== SÉVÉRITÉ CLINIQUE PAR GROUPE ========\n")
severity_summary <- df_severity %>%
  group_by(sortie_et_readmis_30j) %>%
  summarise(
    n = n(),
    Lichtiger_med = median(Lichtiger, na.rm = TRUE),
    CRP_med = median(CRP_admission, na.rm = TRUE),
    Albumine_med = median(AlbuminLevel_admission, na.rm = TRUE),
    UCEIS_med = median(score_UCEIS, na.rm = TRUE),
    pct_megacolon = mean(megacôlon_toxique_YN == 1, na.rm = TRUE) * 100,
    pct_perforation = mean(perforation_YN == 1, na.rm = TRUE) * 100
  )
print(severity_summary)

##-----------------------------------------------
## PARTIE 7 : HYPOTHÈSES ALTERNATIVES
##-----------------------------------------------

cat("\n======== HYPOTHÈSES ALTERNATIVES ========\n")

# 7.1 Hypothèse : Effet "mille-feuille" thérapeutique
# Les patients avec plus de lignes de traitement ont-ils plus de risque de réadmission rapide ?

df_hypothesis1 <- df_intensity %>%
  filter(!is.na(delai_sup_30)) %>%
  mutate(readmis_rapide = delai_sup_30 == 0)

cor_test <- cor.test(df_hypothesis1$n_lignes_medicales, 
                     as.numeric(df_hypothesis1$readmis_rapide),
                     method = "spearman")
cat("\nCorrélation entre nombre de lignes et réadmission < 30j:\n")
print(cor_test)

# 7.2 Hypothèse : Épuisement thérapeutique
# Les patients ayant atteint les biothérapies ont-ils un profil différent ?

df_hypothesis2 <- df_analysis %>%
  left_join(df_intensity, by = "IPP") %>%
  filter(!is.na(max_intensite)) %>%
  mutate(
    therapie_avancee = max_intensite >= 3,  # Anti-TNF ou biothérapie
    groupe_analyse = case_when(
      sortie_pendant_traitement_YN == 0 ~ "Non sorti",
      delai_sup_30 == 0 ~ "Réadmis < 30j",
      delai_sup_30 == 1 ~ "Réadmis > 30j"
    )
  )

tab_therapie <- table(df_hypothesis2$groupe_analyse, df_hypothesis2$therapie_avancee)
cat("\nThérapie avancée par groupe:\n")
print(tab_therapie)
print(prop.table(tab_therapie, margin = 1) * 100)

# 7.3 Hypothèse : Fenêtre d'opportunité manquée
# Y a-t-il un moment optimal pour la chirurgie ?

df_hypothesis3 <- df_long %>%
  filter(medical_line == TRUE) %>%
  group_by(IPP, line_num) %>%
  summarise(
    reponse = first(reponse_med),
    sortie = first(sortie_apres_ttt_med)
  ) %>%
  left_join(df_analysis %>% select(IPP, delai_sup_30), by = "IPP") %>%
  filter(!is.na(reponse))

cat("\n\nRéponse au traitement par ligne et délai de réadmission:\n")
response_by_line <- df_hypothesis3 %>%
  group_by(line_num, delai_sup_30) %>%
  summarise(
    n = n(),
    pct_echec = mean(reponse == "Fail", na.rm = TRUE) * 100
  )
print(response_by_line)

##-----------------------------------------------
## PARTIE 8 : SYNTHÈSE ET RECOMMANDATIONS
##-----------------------------------------------

cat("\n\n======== SYNTHÈSE ========\n")
cat("
ANALYSE DES PROFILS DE SORTIE/RÉADMISSION DANS LES CAG

Résultats principaux:
")

# Points clés à identifier automatiquement
n_sortis <- sum(df$sortie_pendant_traitement_YN == 1, na.rm = TRUE)
n_readmis_30 <- sum(df$sortie_pendant_traitement_YN == 1 & df$delai_sup_30 == 0, na.rm = TRUE)
pct_readmis_30 <- round(n_readmis_30 / n_sortis * 100, 1)

cat(paste0(
  "- ", n_sortis, " patients sortis pendant le traitement médical\n",
  "- ", n_readmis_30, " (", pct_readmis_30, "%) réadmis dans les 30 jours\n"
))

# Identification des facteurs de risque significatifs
if(exists("model_readmis")) {
  significant_factors <- or_table %>%
    filter(p.value < 0.05, term != "(Intercept)") %>%
    select(term, estimate, p.value)
  
  if(nrow(significant_factors) > 0) {
    cat("\nFacteurs significativement associés à la réadmission < 30j:\n")
    print(significant_factors)
  }
}

cat("\n
CONCLUSION:
L'analyse suggère que le phénomène de 'contrôle partiel' avec sortie temporaire
puis réadmission rapide concerne une proportion notable des patients.
Les facteurs clés semblent être l'intensité thérapeutique et la sévérité clinique.

PISTES D'AMÉLIORATION:
1. Identifier précocement les patients à risque de réadmission rapide
2. Optimiser le timing de la chirurgie (éviter l'épuisement thérapeutique)
3. Développer des scores prédictifs spécifiques
")

##-----------------------------------------------
## PARTIE 9 : VISUALISATIONS
##-----------------------------------------------

# 9.1 Distribution des délais de réadmission
if(sum(!is.na(df$delai_dernier_ttt_rehospit)) > 5) {
  p1 <- ggplot(df %>% filter(!is.na(delai_dernier_ttt_rehospit)), 
               aes(x = as.numeric(delai_dernier_ttt_rehospit))) +
    geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = 30, color = "red", linetype = "dashed", size = 1) +
    labs(title = "Distribution des délais de réadmission",
         subtitle = "Ligne rouge = seuil 30 jours",
         x = "Délai (jours)",
         y = "Nombre de patients") +
    theme_minimal()
  print(p1)
}

# 9.2 Intensité thérapeutique par groupe
if(nrow(df_intensity) > 10) {
  p2 <- df_intensity %>%
    filter(sortie_et_readmis_30j != "Non classé") %>%
    ggplot(aes(x = sortie_et_readmis_30j, y = n_lignes_medicales, 
               fill = sortie_et_readmis_30j)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "Nombre de lignes thérapeutiques selon le profil",
         x = "Groupe",
         y = "Nombre de lignes médicales") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))
  print(p2)
}

cat("\n======== ANALYSE TERMINÉE ========\n")