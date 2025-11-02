##-------INSTALLATION PACKAGES DE BASE-------
if (!require("devtools")) install.packages("devtools", dependencies = TRUE)
if (!require("remotes")) install.packages("remotes", dependencies = TRUE)
#install claudeR avec force = TRUE pour écraser les anciennes versions
devtools::install_github("IMNMV/ClaudeR", force = TRUE)
library(ClaudeR)
install_clauder()
claudeAddin()


# Installation des packages nécessaires
install.packages(c(
  "cardx", "dplyr", "readxl", "openxlsx", "tidyverse", "gtsummary",
  "magrittr", "ggplot2", "lubridate", "ggpubr", "survival", "scales",
  "survminer", "summarytools", "MatchIt", "optmatch", "purr",
  "officer", "flextable", "gt", "mice", "googlesheets4", "cards",
  "RItools", "epiR", "tableone", "cobalt", "broom", "forcats", "dlstats", "pkgsearch", "pROC", "stats",
  "parameters", "broom.helpers", "forestplot", "kableExtra", "rsconnect", "pacman", "stringr", "knitr", "purr", "lubridate"
))

# ----LIBRARY ----
library(cardx)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyverse)
library(gtsummary)
library(magrittr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(survival)
library(survminer)
library(scales)
library(summarytools)
library(MatchIt)
library(optmatch)
library(purrr)
library(officer)
library(flextable)
library(gt)
library(mice)
library(googlesheets4)
library(cards)
library(RItools)
library(epiR)
library(tableone)
library(cobalt)
library(broom)
library(forcats)
library(dlstats)
library(pkgsearch)
library(pROC)
library(stats)
library(parameters)
library(broom.helpers)
library(forestplot)
library(kableExtra)
library(rsconnect)
library(pacman)
library(stringr)


#---COMMIT & PUSH SUR GITHUB----

commit_and_push_CAG <- function() {
  repo_path <- "/home/thomas-husson/Documents/R/CAG"
  
  # Demander le message uniquement en mode interactif
  if (interactive()) {
    message <- readline(prompt = "Message de commit : ")
    if (message == "") {
      message <- paste0("Mise à jour du ", format(Sys.Date(), "%d/%m"))
    }
  } else {
    message <- paste0("Mise à jour du ", format(Sys.Date(), "%d/%m"))
  }
  
  # Aller dans le bon dossier
  old_wd <- getwd()
  setwd(repo_path)
  on.exit(setwd(old_wd))
  
  # Vérifier le remote et la branche
  cat("Remote GitHub utilisé :\n")
  system("git remote -v")
  
  cat("\nBranche courante :\n")
  system("git branch --show-current")
  
  # Ajouter, commit, push
  system("git add .")
  system(paste0("git commit -m \"", message, "\""))
  system("git push origin main")
}


commit_and_push_CAG()


##-------Import de la base----

gs4_deauth()  # aucune fenêtre OAuth, accès read-only aux feuilles publiques

df_total <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1Y-BqL27PF0h6lDOmTLCJV8pXXeJ6gYbTL55NRnkOAa0/edit?gid=1505593793",
  sheet = "Feuille1"
) %>%
  filter(vrai_CAG == 1)


##-----Stats pour patients opérés uniquement (df$opere = 1)----
df <- df_total %>% 
  filter(opere == 1.00)

##-------Premières stats sans by = ----
cols_to_include_1 <- c(
  "age_at_surg_or_dg", 
  "BMI", 
  "sex", 
  "RCH", 
  "ASAscore", 
  "ASA_sup_2",
  "active_smoker", 
  "IBD_duration_years",
  "IBD_age_of_diagnosis",
  "poussee_inaugurale_Y_N",
  "combientieme_poussee",
  "hospitalisations_anterieures_pour_CAG_ou_corticothérapie",
  "hospitalisations_anterieures_pourquoi",
  "ttt_dernière_CAG_CTC_seuls",
  "ttt_dernière_CAG_CTC_et_2eligne",
  
  # Charlson comorbidities
  "CCI1_Prior_Myocardial",
  "CCI1_Congestive_HF",
  "CCI1_Peripheral_vascular",
  "CCI1_Cerebrovascular_disease",
  "CCI1_Dementia",
  "CCI1_Chronic_pulmonary_disease",
  "CCI1_Rheumatologic_disease",
  "CCI1_Peptic_ulcer_disease",
  "CCI1_Mild_liver_disease_(legere)",
  "CCI1_Diabetes",
  "CCI2_cerebrovascular_hemiplegia_event",
  "CCI2_moderate_to_severe_renal_disease_DFGsup60",
  "CCI2_diabetes_chronic_complications",
  "CCI2_cancer_without_metastases",
  "CCI2_leukemia",
  "CCI2_lymphoma_myeloma",
  "CCI3_moderate_severe_liver_disease",
  "CCI6_metastatic_solid_tumor",
  "CCI6_SIDA",
  "Charlson_Comorbidity_total",
  
  # Tabac
  "smoker",
  "active_smoker",
  
  # IBD-specific,
  "IBD_duration_days",
  "CSP_associee",
  "uveite_associee",
  "psoriasis_ou_dermato_associee",
  "aphtose_associee",
  "appendicectomy_YN",
  "SpA",
  "LAP_associees",
  
  # Montreal classification
  "MontrealClassA_A1sub16_A21740_A3sup40",
  "MontrealClassB_B1nistricturenipenetrate_B2stricturing_B3_penetrate_B4_both"
)

df <- df %>%
  dplyr::mutate(
    sex = factor(sex),
    smoker = factor(smoker),
    active_smoker = factor(active_smoker),
    Crohn_RCH = factor(Crohn_RCH)
  )

df <- df %>%
  mutate(
    
    Montreal_Age_Class = if_else(
      Crohn_RCH == "Crohn" & !is.na(IBD_age_of_diagnosis),
      as.character(cut(IBD_age_of_diagnosis, c(-Inf,16,40,Inf), labels = c("A1","A2","A3"))),
      NA_character_
    ),
    MontrealClassA_A1sub16_A21740_A3sup40 = if_else(
      Crohn_RCH == "Crohn", Montreal_Age_Class,
      as.character(MontrealClassA_A1sub16_A21740_A3sup40)
    ),
    Montreal_Age_Class_Label = dplyr::recode(
      Montreal_Age_Class, A1 = "A1 (≤16 ans)", A2 = "A2 (17–40 ans)", A3 = "A3 (>40 ans)", .default = NA_character_
    )
  )


tableau1 <- df %>%
  tbl_summary(
    include = all_of(cols_to_include_1),
    missing = "no",
    type = list(
      Charlson_Comorbidity_total ~ "continuous",
      combientieme_poussee ~ "continuous"# <-- forcée en continue
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ 0
    ),
    label = list(
      age_at_surg_or_dg ~ "Âge (années)",
      BMI ~ "BMI (kg/m²)",
      sex ~ "Sexe",
      RCH ~ "RCH (n, %)",
      ASAscore ~ "Score ASA",
      ASA_sup_2 ~ "Score ASA > 2",
      active_smoker ~ "Tabagisme actif",
      IBD_duration_years ~ "Durée MICI (années)",
      IBD_age_of_diagnosis ~ "Âge de diagnostic MICI (années)",
      Charlson_Comorbidity_total ~ "Score de Charlson"
    )
  ) %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

# Afficher dans le viewer
tableau1

##


##-------Premières stats----
cols_to_include_1 <- c(
  "age_at_surg_or_dg", 
  "BMI", 
  "sex", 
  "RCH", 
  "ASAscore", 
  "ASA_sup_2",
  "active_smoker", 
  "IBD_duration_years",
  "IBD_age_of_diagnosis",
  
  # Charlson comorbidities
  "CCI1_Prior_Myocardial",
  "CCI1_Congestive_HF",
  "CCI1_Peripheral_vascular",
  "CCI1_Cerebrovascular_disease",
  "CCI1_Dementia",
  "CCI1_Chronic_pulmonary_disease",
  "CCI1_Rheumatologic_disease",
  "CCI1_Peptic_ulcer_disease",
  "CCI1_Mild_liver_disease_(legere)",
  "CCI1_Diabetes",
  "CCI2_cerebrovascular_hemiplegia_event",
  "CCI2_moderate_to_severe_renal_disease_DFGsup60",
  "CCI2_diabetes_chronic_complications",
  "CCI2_cancer_without_metastases",
  "CCI2_leukemia",
  "CCI2_lymphoma_myeloma",
  "CCI3_moderate_severe_liver_disease",
  "CCI6_metastatic_solid_tumor",
  "CCI6_SIDA",
  "Charlson_Comorbidity_total",
  
  # Tabac
  "smoker",
  "active_smoker",
  
  # IBD-specific,
  "IBD_duration_days",
  "CSP_associee",
  "uveite_associee",
  "psoriasis_ou_dermato_associee",
  "aphtose_associee",
  "appendicectomy_YN",
  "SpA",
  "LAP_associees",
  "combientieme_poussee",
  "IBD_age_of_diagnosis",
  "poussee_inaugurale_Y_N",
  "nombre_hospit_antérieures_pour_CAG",
  "hospitalisations_anterieures_pour_CAG_ou_corticothérapie",
  "ttt_dernière_CAG_CTC_seuls",
  "ttt_dernière_CAG_CTC_et_2eligne", 
  
  
  # Montreal classification
  "MontrealClassA_A1sub16_A21740_A3sup40",
  "MontrealClassB_B1nistricturenipenetrate_B2stricturing_B3_penetrate_B4_both"
)




df <- df %>%
  dplyr::mutate(
    sex = factor(sex),
    smoker = factor(smoker),
    active_smoker = factor(active_smoker),
    Crohn_RCH = factor(Crohn_RCH)
  )

df <- df %>%
  mutate(
    
    Montreal_Age_Class = if_else(
      Crohn_RCH == "Crohn" & !is.na(IBD_age_of_diagnosis),
      as.character(cut(IBD_age_of_diagnosis, c(-Inf,16,40,Inf), labels = c("A1","A2","A3"))),
      NA_character_
    ),
    MontrealClassA_A1sub16_A21740_A3sup40 = if_else(
      Crohn_RCH == "Crohn", Montreal_Age_Class,
      as.character(MontrealClassA_A1sub16_A21740_A3sup40)
    ),
    Montreal_Age_Class_Label = dplyr::recode(
      Montreal_Age_Class, A1 = "A1 (≤16 ans)", A2 = "A2 (17–40 ans)", A3 = "A3 (>40 ans)", .default = NA_character_
    )
  )


tableau1 <- df %>%
  tbl_summary(
    by = sortie_pendant_traitement_YN,
    include = all_of(cols_to_include_1),
    missing = "no",
    type = list(
      Charlson_Comorbidity_total ~ "continuous",   # <-- forcée en continue
      combientieme_poussee ~ "continuous"# <-- forcée en continue
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ 0
    ),
    label = list(
      age_at_surg_or_dg ~ "Âge (années)",
      BMI ~ "BMI (kg/m²)",
      sex ~ "Sexe",
      RCH ~ "RCH (n, %)",
      ASAscore ~ "Score ASA",
      ASA_sup_2 ~ "Score ASA > 2",
      active_smoker ~ "Tabagisme actif",
      IBD_duration_years ~ "Durée MICI (années)",
      IBD_age_of_diagnosis ~ "Âge de diagnostic MICI (années)",
      Charlson_Comorbidity_total ~ "Score de Charlson"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

# Afficher dans le viewer
tableau1

# export du tableau en docx ou odt
tableau1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "tableau1.docx")

#export html pour essayer
tableau1 %>%
  as_gt() %>%
  gt::gtsave("tableau1.html")




#délai réhospit après dernière hospitalisation
cols_to_include_delai <- c(
  "bisdelai_dernier_ttt_rehospit"
)

df_sortie <- df %>%
  filter(df$sortie_pendant_traitement_YN == 1)

tableaudelai <- df_sortie %>%
  tbl_summary(
    by = delai_sup_30,
    include = all_of(cols_to_include_delai),
    missing = "ifany",
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})"
    ),
    digits = list(
      all_continuous() ~ 1
    ),
    label = list(
      bisdelai_dernier_ttt_rehospit ~ "Délai: sortie → réhospit (j)"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3)")

# Afficher dans le viewer
tableaudelai

#tableau délai sans by = 
tableaudelai2 <- df %>%
  tbl_summary(
    include = all_of(cols_to_include_delai),
    missing = "ifany",
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})"
    ),
    digits = list(
      all_continuous() ~ 1
    ),
    label = list(
      bisdelai_dernier_ttt_rehospit ~ "Délai: sortie → réhospit (j)"
    )
  ) %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3)")

tableaudelai2


library(ggplot2)

# On garde seulement les patients avec une réadmission
df_delai <- df_sortie %>%
  filter(!is.na(bisdelai_dernier_ttt_rehospit))

# Courbe de densité avec ligne à 30 jours
ggplot(df_delai, aes(x = bisdelai_dernier_ttt_rehospit)) +
  geom_density(fill = "skyblue", alpha = 0.4, color = "blue") +
  geom_vline(xintercept = 30, linetype = "dashed", color = "red", size = 1) +
  labs(
    x = "Délai avant réhospitalisation (jours)",
    y = "Densité",
    title = "Distribution des délais avant réhospitalisation",
    subtitle = "Ligne rouge = seuil à 30 jours"
  ) +
  theme_minimal(base_size = 13)

#export
ggsave("delai_rehospitalisation_density.png", width = 8, height = 5, dpi = 1000)

# Courbe de densité sans ligne à 30 jours
ggplot(df_delai, aes(x = bisdelai_dernier_ttt_rehospit)) +
  geom_density(fill = "skyblue", alpha = 0.4, color = "blue") +
  labs(
    x = "Délai avant réhospitalisation (jours)",
    y = "Densité",
    title = "Distribution des délais avant réhospitalisation"
  ) +
  theme_minimal(base_size = 13)

#export
ggsave("delai_rehospitalisation_density_noline.png", width = 8, height = 5, dpi = 1000)

#afficher le délai médian de réadmission et Q1 - Q3
median_delay <- median(df_delai$bisdelai_dernier_ttt_rehospit, na.rm = TRUE)
iqr_delay <- quantile(df_delai$bisdelai_dernier_ttt_rehospit, probs = c(0.25, 0.75), na.rm = TRUE)
cat("Délai médian de réadmission :", median_delay, "jours\n")
cat("IQR du délai de réadmission :", iqr_delay[1], "-", iqr_delay[2], "jours\n")

#afficher Q1 et Q3
q1_delay <- quantile(df_delai$bisdelai_dernier_ttt_rehospit, probs = 0.25, na.rm = TRUE)
q3_delay <- quantile(df_delai$bisdelai_dernier_ttt_rehospit, probs = 0.75, na.rm = TRUE)
cat("Q1 du délai de réadmission :", q1_delay, "jours\n")
cat("Q3 du délai de réadmission :", q3_delay, "jours\n")










#A LA MÊME TAILLE
library(ggplot2)

# Définir les bornes communes
x_max <- max(df_delai$delai_admission_derniere_hospit, na.rm = TRUE)
y_max <- max(density(df_delai$delai_admission_derniere_hospit, na.rm = TRUE)$y)

# Courbe de densité avec ligne à 30 jours
p1 <- ggplot(df_delai, aes(x = delai_admission_derniere_hospit)) +
  geom_density(fill = "skyblue", alpha = 0.4, color = "blue") +
  geom_vline(xintercept = 30, linetype = "dashed", color = "red", size = 1) +
  labs(
    x = "Délai avant réhospitalisation (jours)",
    y = "Densité",
    title = "Distribution des délais avant réhospitalisation"
  ) +
  coord_cartesian(xlim = c(0, x_max), ylim = c(0, y_max)) +
  theme_minimal(base_size = 13)

ggsave("delai_rehospitalisation_density.png", p1, width = 8, height = 5, dpi = 1000)

# Courbe de densité sans ligne à 30 jours
p2 <- ggplot(df_delai, aes(x = delai_admission_derniere_hospit)) +
  geom_density(fill = "skyblue", alpha = 0.4, color = "blue") +
  labs(
    x = "Délai avant réhospitalisation (jours)",
    y = "Densité",
    title = "Distribution des délais avant réhospitalisation"
  ) +
  coord_cartesian(xlim = c(0, x_max), ylim = c(0, y_max)) +
  theme_minimal(base_size = 13)

ggsave("delai_rehospitalisation_density_noline.png", p2, width = 8, height = 5, dpi = 1000)



##-------Premières stats en fonction de df$at_least_3_medical_lines----
cols_to_include_1 <- c(
  "age_at_surg_or_dg", 
  "BMI", 
  "sex", 
  "RCH", 
  "ASAscore", 
  "ASA_sup_2",
  "active_smoker", 
  "IBD_duration_years",
  "IBD_age_of_diagnosis",
  
  # Charlson comorbidities
  "CCI1_Prior_Myocardial",
  "CCI1_Congestive_HF",
  "CCI1_Peripheral_vascular",
  "CCI1_Cerebrovascular_disease",
  "CCI1_Dementia",
  "CCI1_Chronic_pulmonary_disease",
  "CCI1_Rheumatologic_disease",
  "CCI1_Peptic_ulcer_disease",
  "CCI1_Mild_liver_disease_(legere)",
  "CCI1_Diabetes",
  "CCI2_cerebrovascular_hemiplegia_event",
  "CCI2_moderate_to_severe_renal_disease_DFGsup60",
  "CCI2_diabetes_chronic_complications",
  "CCI2_cancer_without_metastases",
  "CCI2_leukemia",
  "CCI2_lymphoma_myeloma",
  "CCI3_moderate_severe_liver_disease",
  "CCI6_metastatic_solid_tumor",
  "CCI6_SIDA",
  "Charlson_Comorbidity_total",
  
  # Tabac
  "smoker",
  "active_smoker",
  
  # IBD-specific,
  "IBD_duration_days",
  "CSP_associee",
  "uveite_associee",
  "psoriasis_ou_dermato_associee",
  "aphtose_associee",
  "appendicectomy_YN",
  "SpA",
  "LAP_associees",
  "combientieme_poussee",
  "IBD_age_of_diagnosis",
  "poussee_inaugurale_Y_N",
  "nombre_hospit_antérieures_pour_CAG",
  "hospitalisations_anterieures_pour_CAG_ou_corticothérapie",
  "ttt_dernière_CAG_CTC_seuls",
  "ttt_dernière_CAG_CTC_et_2eligne", 
  
  
  # Montreal classification
  "MontrealClassA_A1sub16_A21740_A3sup40",
  "MontrealClassB_B1nistricturenipenetrate_B2stricturing_B3_penetrate_B4_both"
)




df <- df %>%
  dplyr::mutate(
    sex = factor(sex),
    smoker = factor(smoker),
    active_smoker = factor(active_smoker),
    Crohn_RCH = factor(Crohn_RCH)
  )

df <- df %>%
  mutate(
    
    Montreal_Age_Class = if_else(
      Crohn_RCH == "Crohn" & !is.na(IBD_age_of_diagnosis),
      as.character(cut(IBD_age_of_diagnosis, c(-Inf,16,40,Inf), labels = c("A1","A2","A3"))),
      NA_character_
    ),
    MontrealClassA_A1sub16_A21740_A3sup40 = if_else(
      Crohn_RCH == "Crohn", Montreal_Age_Class,
      as.character(MontrealClassA_A1sub16_A21740_A3sup40)
    ),
    Montreal_Age_Class_Label = dplyr::recode(
      Montreal_Age_Class, A1 = "A1 (≤16 ans)", A2 = "A2 (17–40 ans)", A3 = "A3 (>40 ans)", .default = NA_character_
    )
  )


tableau1 <- df %>%
  tbl_summary(
    by = at_least_3_medical_lines,
    include = all_of(cols_to_include_1),
    missing = "no",
    type = list(
      Charlson_Comorbidity_total ~ "continuous",   # <-- forcée en continue
      combientieme_poussee ~ "continuous"# <-- forcée en continue
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ 0
    ),
    label = list(
      age_at_surg_or_dg ~ "Âge (années)",
      BMI ~ "BMI (kg/m²)",
      sex ~ "Sexe",
      RCH ~ "RCH (n, %)",
      ASAscore ~ "Score ASA",
      ASA_sup_2 ~ "Score ASA > 2",
      active_smoker ~ "Tabagisme actif",
      IBD_duration_years ~ "Durée MICI (années)",
      IBD_age_of_diagnosis ~ "Âge de diagnostic MICI (années)",
      Charlson_Comorbidity_total ~ "Score de Charlson"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

# Afficher dans le viewer
tableau1

# export du tableau en docx ou odt
tableau1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "tableau1.docx")

#export html pour essayer
tableau1 %>%
  as_gt() %>%
  gt::gtsave("tableau1.html")




#délai réhospit après dernière hospitalisation
cols_to_include_delai <- c(
  "delai_admission_derniere_hospit"
)

df_sortie <- df %>%
  filter(df$sortie_pendant_traitement_YN == 1)

tableaudelai <- df_sortie %>%
  tbl_summary(
    by = delai_sup_30,
    include = all_of(cols_to_include_delai),
    missing = "ifany",
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})"
    ),
    digits = list(
      all_continuous() ~ 1
    ),
    label = list(
      delai_admission_derniere_hospit ~ "Délai: sortie → réhospit (j)"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3)")

# Afficher dans le viewer
tableaudelai

#tableau délai sans by = 
tableaudelai2 <- df %>%
  tbl_summary(
    include = all_of(cols_to_include_delai),
    missing = "ifany",
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})"
    ),
    digits = list(
      all_continuous() ~ 1
    ),
    label = list(
      delai_admission_derniere_hospit ~ "Délai: sortie → réhospit (j)"
    )
  ) %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3)")

tableaudelai2


library(ggplot2)

# On garde seulement les patients avec une réadmission
df_delai <- df_sortie %>%
  filter(!is.na(delai_admission_derniere_hospit))

# Courbe de densité avec ligne à 30 jours
ggplot(df_delai, aes(x = delai_admission_derniere_hospit)) +
  geom_density(fill = "skyblue", alpha = 0.4, color = "blue") +
  geom_vline(xintercept = 30, linetype = "dashed", color = "red", size = 1) +
  labs(
    x = "Délai avant réhospitalisation (jours)",
    y = "Densité",
    title = "Distribution des délais avant réhospitalisation",
    subtitle = "Ligne rouge = seuil à 30 jours"
  ) +
  theme_minimal(base_size = 13)

#export
ggsave("delai_rehospitalisation_density.png", width = 8, height = 5, dpi = 1000)

# Courbe de densité sans ligne à 30 jours
ggplot(df_delai, aes(x = delai_admission_derniere_hospit)) +
  geom_density(fill = "skyblue", alpha = 0.4, color = "blue") +
  labs(
    x = "Délai avant réhospitalisation (jours)",
    y = "Densité",
    title = "Distribution des délais avant réhospitalisation"
  ) +
  theme_minimal(base_size = 13)

#export
ggsave("delai_rehospitalisation_density_noline.png", width = 8, height = 5, dpi = 1000)








#A LA MÊME TAILLE
library(ggplot2)

# Définir les bornes communes
x_max <- max(df_delai$delai_admission_derniere_hospit, na.rm = TRUE)
y_max <- max(density(df_delai$delai_admission_derniere_hospit, na.rm = TRUE)$y)

# Courbe de densité avec ligne à 30 jours
p1 <- ggplot(df_delai, aes(x = delai_admission_derniere_hospit)) +
  geom_density(fill = "skyblue", alpha = 0.4, color = "blue") +
  geom_vline(xintercept = 30, linetype = "dashed", color = "red", size = 1) +
  labs(
    x = "Délai avant réhospitalisation (jours)",
    y = "Densité",
    title = "Distribution des délais avant réhospitalisation"
  ) +
  coord_cartesian(xlim = c(0, x_max), ylim = c(0, y_max)) +
  theme_minimal(base_size = 13)

ggsave("delai_rehospitalisation_density.png", p1, width = 8, height = 5, dpi = 1000)

# Courbe de densité sans ligne à 30 jours
p2 <- ggplot(df_delai, aes(x = delai_admission_derniere_hospit)) +
  geom_density(fill = "skyblue", alpha = 0.4, color = "blue") +
  labs(
    x = "Délai avant réhospitalisation (jours)",
    y = "Densité",
    title = "Distribution des délais avant réhospitalisation"
  ) +
  coord_cartesian(xlim = c(0, x_max), ylim = c(0, y_max)) +
  theme_minimal(base_size = 13)

ggsave("delai_rehospitalisation_density_noline.png", p2, width = 8, height = 5, dpi = 1000)



##-------Stats pour historique des traitements----
#ne compte pas les patients pour qui la poussée est inaugurale de la RCH (donc pas de ttt de fond)
df_t2 <- df %>%
  filter(is.na(poussee_inaugurale_Y_N) | poussee_inaugurale_Y_N != 1)

cols_to_include_2 <- c(
  # Locaux
  "historique_locaux_PENTASA_endocort_YN",
  "pentasa_YN",
  "endocort_YN",
  
  # Corticoïdes
  "historique_corticoides_generaux",
  "cortico_PO_long_cours",
  "corticodependance_impossibilité_sous_10_mg_ou_rechute_inf3mois_apres_arret",
  "cortico_PO_nombre_jours",
  "cortico_PO_decroissance_ou_arret_avant_poussee_YN",
  "cortico_PO_delai_decroissance_arret_poussee",
  
  # Immunosuppresseurs
  "historique_immunosuppresseurs_MTX_AZT_purinethol_YN_ciclosporine",
  "historique_immunosuppresseurs_MTX_AZT_purinethol_nombre",
  "Cliclosporine_fond",
  "azathioprine_YN",
  "purinethol_YN",
  "MTX_YN",
  
  # Anti-TNF
  "historique_antiTNF_YN",
  "historique_antiTNF_nombre",
  "infliximab_fond",
  "adalimumab_YN",
  
  # Autres biothérapies
  "historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_YN",
  "historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_nombre",
  "vedolizumab_fond_ou_poussee_ancienne",
  "tofacitinib_YN",
  "stelara_YN",
  "golimumab_YN"
)

# Tableau 2
tableau2 <- df_t2 %>%
  tbl_summary(
    by = sortie_pendant_traitement_YN,
    include = all_of(cols_to_include_2),
    missing = "ifany",
    percent = "column",
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ 0
    ),
    label = list(
      historique_locaux_PENTASA_endocort_YN ~ "Locaux (Pentasa, Endocort)",
      pentasa_YN ~ "Pentasa",
      endocort_YN ~ "Endocort",
      historique_corticoides_generaux ~ "Corticoïdes généraux",
      cortico_PO_long_cours ~ "Corticoïdes PO long cours",
      corticodependance_impossibilité_sous_10_mg_ou_rechute_inf3mois_apres_arret ~ "Corticodépendance",
      cortico_PO_nombre_jours ~ "Durée corticoïdes PO (jours)",
      cortico_PO_decroissance_ou_arret_avant_poussee_YN ~ "Décroissance/arrêt avant poussée",
      cortico_PO_delai_decroissance_arret_poussee ~ "Délai décroissance/arrêt avant poussée (jours)",
      historique_immunosuppresseurs_MTX_AZT_purinethol_YN_ciclosporine ~ "Immunosuppresseurs (MTX, AZT, purinethol, ciclosporine)",
      historique_immunosuppresseurs_MTX_AZT_purinethol_nombre ~ "Nombre d'immunosuppresseurs",
      Cliclosporine_fond ~ "Ciclosporine",
      azathioprine_YN ~ "Azathioprine",
      purinethol_YN ~ "Purinethol",
      MTX_YN ~ "MTX",
      historique_antiTNF_YN ~ "Anti-TNF",
      historique_antiTNF_nombre ~ "Nombre d'anti-TNF",
      infliximab_fond ~ "Infliximab",
      adalimumab_YN ~ "Adalimumab",
      historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_YN ~ "Autres biothérapies (Vedolizumab, Tofa, Stelara, Golimumab)",
      historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_nombre ~ "Nombre d'autres biothérapies",
      vedolizumab_fond_ou_poussee_ancienne ~ "Vedolizumab",
      tofacitinib_YN ~ "Tofacitinib",
      stelara_YN ~ "Stelara",
      golimumab_YN ~ "Golimumab"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

# Afficher dans le viewer
tableau2





      
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


#tableau sortie global sans by = 
tableau_sortie_global2 <- df_patient %>%
  mutate(across(c(any_CTC, any_IS, any_TNF, any_bio, any_surg, ever_sortie_med), ~ . == 1)) %>%
  tbl_summary(
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
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

tableau_sortie_global2

#détail des lignes de traitement de la poussée
cols_to_include_ttt_poussee <- c(
  "1st_lign", "1st_lign_ttt", "1st_lign_CTC", "1st_lign_IS", "1st_lign_TNF", "1st_lign_bio", "1st_lign_surgery",
  "2nd_lign", "2nd_lign_ttt", "2nd_lign_CTC", "2nd_lign_IS", "2nd_lign_TNF", "2nd_lign_bio", "2nd_lign_surgery",
  "3rd_lign", "3rd_lign_ttt", "3rd_lign_CTC", "3rd_lign_IS", "3rd_lign_TNF", "3rd_lign_bio", "3rd_lign_surgery",
  "4th_lign", "4th_lign_ttt", "4th_lign_CTC", "4th_lign_IS", "4th_lign_TNF", "4th_lign_bio", "4th_lign_surgery",
  "5th_lign", "5th_lign_ttt", "5th_lign_CTC", "5th_lign_IS", "5th_lign_TNF", "5th_lign_bio", "5th_lign_surgery"
)


tableau_ttt_poussee <- df %>%
  tbl_summary(
    include = all_of(cols_to_include_ttt_poussee),
    missing = "ifany",
    percent = "column",
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ 0)
  ) %>%
  modify_header(label ~ "**Caractéristique**")


# Afficher dans le viewer
tableau_ttt_poussee

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

  








##-------Stats-timing------

#includes ces colonnes : df$delai_admission_derniere_hospit
#df$delai_chirurgie_diagnostic
#df$delai_chirurgie_premiers_symptomes
#df$delai_dernier_ttt_rehospit
#df$nb_selles_24h
#df$nb_selles_sanglantes_24h
#df$Truelove_minor_heart_rate_sup_or_equal_90
#df$Truelove_minor_temperature_sup_or_equal_37_8
#df$Truelove_minor_Hb_inf_or_equal_10_5
#df$Truelove_minor_Albumine_inf_or_equal_35
#df$Truelove_minor_CRPsup_equal_30

cols_to_include_timing <- c(
  "delai_chirurgie_premiers_symptomes",
  "delai_admission_derniere_hospit",
  "delai_chirurgie_diagnostic",
  "delai_chirurgie_premiers_symptomes",
  "delai_dernier_ttt_rehospit",
  "nb_selles_24h",
  "nb_selles_sanglantes_24h",
  "Truelove_minor_heart_rate_sup_or_equal_90",
  "Truelove_minor_fever_sup_37_8",
  "Truelove_minor_Hb_inf_or_equal_10_5",
  "Truelove_minor_Albumine_inf_or_equal_35",
  "Truelove_minor_CRPsup_equal_30"
)


#tableau avec ca
tableau_timing <- df %>%
  tbl_summary(
    by = delai_sup_30,
    include = all_of(cols_to_include_timing),
    missing = "ifany",
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})"
    ),
    digits = list(
      all_continuous() ~ 1
    ),
    label = list(
      delai_admission_derniere_hospit ~ "Délai: sortie → réhospit (j)",
      delai_chirurgie_diagnostic      ~ "Délai: diag → chirurgie (j)",
      delai_chirurgie_premiers_symptomes ~ "Délai: premiers symptomes → chirurgie (j)",
      delai_dernier_ttt_rehospit      ~ "Délai: dernière ligne → réhospit (j)"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3)")

tableau_timing


# 1) Sous-ensemble: pas de poussée inaugurale
df_prev <- df %>%
  filter(is.na(poussee_inaugurale_Y_N) | poussee_inaugurale_Y_N != 1) %>%
  mutate(
    # Délais clés (en jours)
    delai_symptomes_admission       = as.numeric(as.Date(date_admission_hopital) - as.Date(`date_debut_symptomes_episodes_actuel`)),
    delai_derniere_sortie_admission = as.numeric(as.Date(date_admission_hopital) - as.Date(`date_sortie_derniere_hospit_pour_CAG`)),
    # Sorti pendant le traitement médical (binaire dérivé de la date d’événement)
    sortie_pendant_traitement = !is.na(`sortie_pendant_traitement_quand`),
    # Nettoyage raisons d'hospitalisation antérieures + Top 5
    hosp_pourquoi_clean = str_squish(as.character(hospitalisations_anterieures_pourquoi)),
    hosp_pourquoi_top5  = fct_lump_n(as.factor(hosp_pourquoi_clean), n = 5, other_level = "Autres/NA"),
    # Catégorisation du rang de poussée
    combientieme_poussee_cat = case_when(
      is.na(combientieme_poussee)          ~ NA_character_,
      combientieme_poussee <= 1            ~ "1",
      combientieme_poussee == 2            ~ "2",
      combientieme_poussee >= 3            ~ "≥3"
    )
  )

# 2) Tableau principal des antécédents et délais (stratifié par delai_sup_30)
vars_prev <- c(
  "combientieme_poussee_cat",
  "hospitalisations_anterieures_pour_CAG_ou_corticothérapie",
  "nombre_hospit_antérieures_pour_CAG",
  "hosp_pourquoi_top5",
  "delai_symptomes_admission",
  "delai_derniere_sortie_admission"
)

tab_antecedents <- df_prev %>%
  # Forcer les binaires en catégoriel pour n'afficher qu'une ligne utile
  tbl_summary(
    by = delai_sup_30,
    include = all_of(vars_prev),
    missing = "ifany",
    type = list(
      hospitalisations_anterieures_pour_CAG_ou_corticothérapie ~ "categorical"
    ),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_categorical() ~ 0
    ),
    label = list(
      combientieme_poussee_cat                                ~ "Nombre de poussées (cat.)",
      hospitalisations_anterieures_pour_CAG_ou_corticothérapie ~ "ATCD d'hospit pour CAG/CTC",
      `nombre_hospit_antérieures_pour_CAG`                    ~ "Nombre d'hospit antérieures (CAG)",
      hosp_pourquoi_top5                                      ~ "Motif d'hospitalisation antérieure (Top 5)",
      delai_symptomes_admission                               ~ "Délai symptômes → admission (j)",
      delai_derniere_sortie_admission                         ~ "Délai dernière sortie → admission (j)"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

tab_antecedents


# A) Cross-tab: rang de poussée vs delai_sup_30
tab_poussee_vs_delai <- tbl_cross(
  data = df_prev,
  row = combientieme_poussee_cat, col = delai_sup_30, percent = "row"
) %>% add_p()
tab_poussee_vs_delai

# B) Chez les patients sortis pendant traitement:
#    distribution des délais (médiane [IQR]) par ligne lors de la sortie
tab_sortis_detail <- df_prev %>%
  filter(sortie_pendant_traitement) %>%
  select(
    delai_derniere_sortie_admission,
    delai_symptomes_admission,
    sortie_pendant_quelle_ligne,
    delai_sup_30
  ) %>%
  tbl_summary(
    by = sortie_pendant_quelle_ligne,
    missing = "ifany",
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    digits = all_continuous() ~ 1,
    label = list(
      delai_derniere_sortie_admission ~ "Délai dernière sortie → admission (j)",
      delai_symptomes_admission       ~ "Délai symptômes → admission (j)"
    )
  ) %>%
  add_p()
tab_sortis_detail



  # features à partir des colonnes 1st..5th (toutes existent dans ton xlsx) 
df <- df %>% mutate(.row_id = dplyr::row_number())
id_var <- if ("IPP" %in% names(df)) "IPP" else ".row_id"
lines <- c("1st","2nd","3rd","4th","5th")

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
    line_num = dplyr::recode(line, "1st"=1L, "2nd"=2L, "3rd"=3L, "4th"=4L, "5th"=5L),
    medical_line = surgery != 1 & (attempted == 1 | !is.na(ttt_text) | CTC==1 | IS==1 | TNF==1 | bio==1),
    event_sortie_med = as.integer(medical_line & sortie_apres_ttt_med == 1),
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

df_feat_base <- df_long %>%
  group_by(id) %>%
  summarise(
    n_lignes_tentees = sum(replace_na(attempted, 0) == 1, na.rm = TRUE),
    any_CTC   = as.integer(any(CTC    == 1, na.rm = TRUE)),
    any_IS    = as.integer(any(IS     == 1, na.rm = TRUE)),
    any_TNF   = as.integer(any(TNF    == 1, na.rm = TRUE)),
    any_bio   = as.integer(any(bio    == 1, na.rm = TRUE)),
    any_surg  = as.integer(any(surgery== 1, na.rm = TRUE)),
    ever_sortie_med    = as.integer(any(event_sortie_med == 1, na.rm = TRUE)),
    first_sortie_line  = if_else(any(event_sortie_med == 1, na.rm = TRUE),
                                 min(line_num[event_sortie_med == 1], na.rm = TRUE), NA_integer_)
  ) %>% ungroup()

# Variante "niveau de traitement le plus avancé" (CTC<IS<TNF<Autre bio)
ttt_level_map <- c("Corticoïdes généraux"=1L, "Immunosuppresseur"=2L, "Anti-TNF"=3L, "Autre biothérapie"=4L)
df_max_stage <- df_long %>%
  filter(medical_line, ttt_group %in% names(ttt_level_map)) %>%
  mutate(stage = ttt_level_map[ttt_group]) %>%
  group_by(id) %>%
  summarise(max_stage = ifelse(any(!is.na(stage)), max(stage, na.rm = TRUE), NA)) %>%
  ungroup() %>%
  mutate(max_stage = factor(max_stage, levels = 1:4,
                            labels = c("CTC","IS","Anti-TNF","Autre bio")))

df_patient <- df_feat_base %>%
  left_join(df_max_stage, by = "id") %>%
  left_join(df %>% transmute(id = .data[[id_var]], delai_sup_30), by = "id")

#Modèle 1 : patient-level (issue = ever_sortie_med) 
df_patient_m <- df_patient %>%
  mutate(
    delai_sup_30 = forcats::fct_infreq(as.factor(delai_sup_30)),
    delai_sup_30 = forcats::fct_relevel(delai_sup_30, levels(delai_sup_30)[1])
  )

fit_patient <- glm(
  ever_sortie_med ~ any_CTC + any_IS + any_TNF + any_bio + n_lignes_tentees + delai_sup_30,
  data = df_patient_m,
  family = binomial(),
  na.action = na.exclude
)

tab_patient <- tbl_regression(
  fit_patient, exponentiate = TRUE,
  label = list(
    any_CTC ~ "CTC (≥1 ligne)",
    any_IS ~ "IS (≥1 ligne)",
    any_TNF ~ "Anti-TNF (≥1 ligne)",
    any_bio ~ "Autre biothérapie (≥1 ligne)",
    n_lignes_tentees ~ "Nb de lignes tentées",
    delai_sup_30 ~ "Délai (cat.)"
  )
)

# Variante compacte avec une seule variable "stade max"
fit_patient_stage <- glm(
  ever_sortie_med ~ max_stage + n_lignes_tentees + delai_sup_30,
  data = df_patient_m,
  family = binomial(),
  na.action = na.exclude
)
tab_patient_stage <- tbl_regression(
  fit_patient_stage, exponentiate = TRUE,
  label = list(
    max_stage ~ "Stade de ttt le plus avancé",
    n_lignes_tentees ~ "Nb de lignes tentées",
    delai_sup_30 ~ "Délai (cat.)"
  )
)

# ---------- Modèle 2 : line-level (issue = event_sortie_med), SE robustes cluster id
df_line_m <- df_long %>%
  filter(medical_line) %>%
  left_join(df %>% transmute(id = .data[[id_var]], delai_sup_30), by = "id") %>%
  mutate(
    ttt_group = fct_relevel(
      factor(ttt_group),
      "Corticoïdes généraux","Immunosuppresseur","Anti-TNF","Autre biothérapie","Médical (autre)"
    ),
    delai_sup_30 = fct_infreq(as.factor(delai_sup_30)),
    delai_sup_30 = fct_relevel(delai_sup_30, levels(delai_sup_30)[1])
  )

fit_line <- glm(
  event_sortie_med ~ ttt_group + line_num + delai_sup_30,
  data = df_line_m,
  family = binomial(),
  na.action = na.exclude
)
vcov_cl <- sandwich::vcovCL(fit_line, cluster = ~ id)
res_line <- broom::tidy(fit_line) |>
  mutate(
    se = sqrt(diag(vcov_cl)),
    z  = estimate / se,
    p_robust = 2 * pnorm(abs(z), lower.tail = FALSE),
    OR = exp(estimate),
    OR_low = exp(estimate - 1.96*se),
    OR_high= exp(estimate + 1.96*se)
  ) |>
  select(term, OR, OR_low, OR_high, p_robust)

# ---------- Affichage 
tab_patient        # modèle patient-level (any_*)
tab_patient_stage  # variante patient-level (stade max)
res_line           # tableau coef line-level (SE robustes)










#--------Stats clinique poussée sans by =----
# Colonnes à inclure (une par ligne, regroupées par blocs)
cols_to_include_3 <- c(
  # Chirurgie & hospitalisation
  "chirurgie_urgente",
  "hospit_pre_ICU_tradi",
  
  # Poids / nutrition
  "weight_loss_sup10",
  "AlbuminLevel_admission",
  "Albumin_sub35",
  "Malnutrition_BMIsub18_or_albusub35_or_weightloss",
  
  # Biologie à l'admission
  "CRP_admission",
  "CRP_admission_sup30",
  "Hb_preop_last2mo",
  "Hb_preopInf10_5_g_dL",
  "calpro_a_ladmission",
  "leucocytose_admission",
  "leucocytosis_sup_10_or_sub_4",
  "plaquettes_admission",
  "thrombopenia_sub150",
  "thrombocytosis_sup400",
  
  # Imagerie au diagnostic
  "imagerie_au_diagnostic",
  "imagerie_dg_laquelle",
  "imagerie_dg_TDM",
  "imagerie_dg_ASP",
  
  # Endoscopie / UCEIS
  "rectosig_au_diagnostic",
  "rectosig_poussee_nombre",
  "rectosig_CAG_UCEIS_numero_1",
  "rectosig_CAG_limite_sup_atteinte",
  "rectosig_CAG_derniere_UCEIS",
  "rectosig_derniere_delai",
  "score_UCEIS",
  
  # Infections / dysplasie
  "CMV_YN",
  "clostridium_YN",
  "dysplasie_YN",
  
  # Lichtiger & clinique
  "Lichtiger",
  "Lichtiger_sup_or_equal_10",
  "nb_selles_24h",
  "selles_nocturnes",
  "saignement_rectal",
  "incontinence_fecale",
  "douleurs_abdo",
  "douleurs_abdo_provoquees",
  "etat_general",
  "imodium",
  "lichtiger_diagnostic",
  "Lichtiger_chirurgie",
  
  # Complications aiguës
  "hemorragie_digestive_grave_YN",
  "perforation_YN",
  "colectasie_caecum_mm",
  "colectasie_transverse_mm",
  "colectasie_caecum_sup100mm",
  "colectasie_transversesup60mm",
  "megacôlon_toxique_YN",
  "instabilite_HD",
  
  # ATCD digestifs
  "previous_digestive_surgery",
  "previous_digestive_surgery_whole_text",
  "previous_digestive_resection",
  "previous_digestive_resection_for_MICI",
  "number_of_previous_resection",
  
  # Topographie & lésions endoscopiques
  "topographie_colite",
  "endoscopie_lesions_severes",
  "ulcerations_creusantes",
  "ulcerations_en_puits",
  "decollement_muqueux",
  "abrasion_muqueuse"
)


tableau3 <- df %>%
  tbl_summary(
    include = all_of(cols_to_include_3),
    missing = "ifany",
    type  = list(all_dichotomous() ~ "dichotomous"),
    value = list(all_dichotomous() ~ 1),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_dichotomous() ~ "{n} ({p}%)",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_dichotomous() ~ 0,
      all_categorical() ~ 0
    ),
    label = list(
      # Chirurgie & hospitalisation
      chirurgie_urgente ~ "Chirurgie urgente",
      hospit_pre_ICU_tradi ~ "Hospitalisation préalable (ICU/traditionnel)",
      
      # Poids / nutrition
      weight_loss_sup10 ~ "Perte de poids >10%",
      AlbuminLevel_admission ~ "Albuminémie à l'admission (g/L)",
      Albumin_sub35 ~ "Albumine <35 g/L",
      Malnutrition_BMIsub18_or_albusub35_or_weightloss ~ "Malnutrition (IMC<18, albumine<35 ou perte de poids)",
      
      # Biologie
      CRP_admission ~ "CRP à l'admission (mg/L)",
      CRP_admission_sup30 ~ "CRP >30 mg/L",
      Hb_preop_last2mo ~ "Hémoglobine pré-op (g/dL, 2 mois)",
      Hb_preopInf10_5_g_dL ~ "Hémoglobine <10,5 g/dL",
      calpro_a_ladmission ~ "Calprotectine à l'admission (µg/g)",
      leucocytose_admission ~ "Leucocytes à l'admission (/mm³)",
      leucocytosis_sup_10_or_sub_4 ~ "Leucocytes >10 ou <4",
      plaquettes_admission ~ "Plaquettes à l'admission (/mm³)",
      thrombopenia_sub150 ~ "Thrombopénie <150 G/L",
      thrombocytosis_sup400 ~ "Thrombocytose >400 G/L",
      
      # Imagerie
      imagerie_au_diagnostic ~ "Imagerie réalisée au diagnostic",
      imagerie_dg_laquelle ~ "Imagerie: type",
      imagerie_dg_TDM ~ "Scanner (TDM)",
      imagerie_dg_ASP ~ "ASP",
      
      # Endoscopie / UCEIS
      rectosig_au_diagnostic ~ "Rectosigmoïdoscopie au diagnostic",
      rectosig_poussee_nombre ~ "Nombre de rectosigmoïdoscopies pendant poussée",
      rectosig_CAG_UCEIS_numero_1 ~ "Score UCEIS (1er exam)",
      rectosig_CAG_limite_sup_atteinte ~ "Limite proximale atteinte",
      rectosig_CAG_derniere_UCEIS ~ "Score UCEIS (dernier exam)",
      rectosig_derniere_delai ~ "Délai dernier exam (j)",
      score_UCEIS ~ "Score UCEIS global",
      
      # Infections / dysplasie
      CMV_YN ~ "Infection CMV",
      clostridium_YN ~ "Clostridium",
      dysplasie_YN ~ "Dysplasie",
      
      # Lichtiger & clinique
      Lichtiger ~ "Score Lichtiger",
      Lichtiger_sup_or_equal_10 ~ "Score Lichtiger ≥10",
      nb_selles_24h ~ "Nb de selles/24h",
      selles_nocturnes ~ "Selles nocturnes",
      saignement_rectal ~ "Saignements rectaux",
      incontinence_fecale ~ "Incontinence fécale",
      douleurs_abdo ~ "Douleurs abdominales",
      douleurs_abdo_provoquees ~ "Douleurs abdominales provoquées",
      etat_general ~ "Altération de l'état général",
      imodium ~ "Traitement par Imodium",
      lichtiger_diagnostic ~ "Score Lichtiger au diagnostic",
      Lichtiger_chirurgie ~ "Score Lichtiger pré-chirurgie",
      
      # Complications aiguës
      hemorragie_digestive_grave_YN ~ "Hémorragie digestive grave",
      perforation_YN ~ "Perforation",
      colectasie_caecum_mm ~ "Colectasie cæcum (mm)",
      colectasie_transverse_mm ~ "Colectasie transverse (mm)",
      colectasie_caecum_sup100mm ~ "Colectasie cæcum >100 mm",
      colectasie_transversesup60mm ~ "Colectasie transverse >60 mm",
      megacôlon_toxique_YN ~ "Mégacôlon toxique",
      instabilite_HD ~ "Instabilité hémodynamique",
      
      # ATCD digestifs
      previous_digestive_surgery ~ "ATCD chirurgie digestive",
      previous_digestive_surgery_whole_text ~ "ATCD chirurgie digestive (texte)",
      previous_digestive_resection ~ "ATCD résection digestive",
      previous_digestive_resection_for_MICI ~ "ATCD résection pour MICI",
      number_of_previous_resection ~ "Nb de résections digestives antérieures",
      
      # Topographie & lésions
      topographie_colite ~ "Topographie de la colite",
      endoscopie_lesions_severes ~ "Lésions sévères à l'endoscopie",
      ulcerations_creusantes ~ "Ulcérations creusantes",
      ulcerations_en_puits ~ "Ulcérations en puits",
      decollement_muqueux ~ "Décollement muqueux",
      abrasion_muqueuse ~ "Abrasion muqueuse"
    )
  ) %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

# Afficher
tableau3

#--------Stats clinique poussée----
# Colonnes à inclure (une par ligne, regroupées par blocs)
cols_to_include_3 <- c(
  # Chirurgie & hospitalisation
  "chirurgie_urgente",
  "hospit_pre_ICU_tradi",
  
  # Poids / nutrition
  "weight_loss_sup10",
  "AlbuminLevel_admission",
  "Albumin_sub35",
  "Malnutrition_BMIsub18_or_albusub35_or_weightloss",
  
  # Biologie à l'admission
  "CRP_admission",
  "CRP_admission_sup30",
  "Hb_preop_last2mo",
  "Hb_preopInf10_5_g_dL",
  "calpro_a_ladmission",
  "leucocytose_admission",
  "leucocytosis_sup_10_or_sub_4",
  "plaquettes_admission",
  "thrombopenia_sub150",
  "thrombocytosis_sup400",
  
  # Imagerie au diagnostic
  "imagerie_au_diagnostic",
  "imagerie_dg_laquelle",
  "imagerie_dg_TDM",
  "imagerie_dg_ASP",
  
  # Endoscopie / UCEIS
  "rectosig_au_diagnostic",
  "rectosig_poussee_nombre",
  "rectosig_CAG_UCEIS_numero_1",
  "rectosig_CAG_limite_sup_atteinte",
  "rectosig_CAG_derniere_UCEIS",
  "rectosig_derniere_delai",
  "score_UCEIS",
  
  # Infections / dysplasie
  "CMV_YN",
  "clostridium_YN",
  "dysplasie_YN",
  
  # Lichtiger & clinique
  "Lichtiger",
  "Lichtiger_sup_or_equal_10",
  "nb_selles_24h",
  "selles_nocturnes",
  "saignement_rectal",
  "incontinence_fecale",
  "douleurs_abdo",
  "douleurs_abdo_provoquees",
  "etat_general",
  "imodium",
  "lichtiger_diagnostic",
  "Lichtiger_chirurgie",
  
  # Complications aiguës
  "hemorragie_digestive_grave_YN",
  "perforation_YN",
  "colectasie_caecum_mm",
  "colectasie_transverse_mm",
  "colectasie_caecum_sup100mm",
  "colectasie_transversesup60mm",
  "megacôlon_toxique_YN",
  "instabilite_HD",
  
  # ATCD digestifs
  "previous_digestive_surgery",
  "previous_digestive_surgery_whole_text",
  "previous_digestive_resection",
  "previous_digestive_resection_for_MICI",
  "number_of_previous_resection",
  
  # Topographie & lésions endoscopiques
  "topographie_colite",
  "endoscopie_lesions_severes",
  "ulcerations_creusantes",
  "ulcerations_en_puits",
  "decollement_muqueux",
  "abrasion_muqueuse"
)

df$score_UCEIS <- as.numeric(df$score_UCEIS)

tableau3 <- df %>%
  tbl_summary(
    include = all_of(cols_to_include_3),
    by = delai_sup_30,
    type = list(
      score_UCEIS ~ "continuous",
      all_dichotomous() ~ "dichotomous"
    ),
    value = list(all_dichotomous() ~ 1),
    missing = "ifany",
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_dichotomous() ~ "{n} ({p}%)",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_dichotomous() ~ 0,
      all_categorical() ~ 0
    ),
    label = list(
      # Chirurgie & hospitalisation
      chirurgie_urgente ~ "Chirurgie urgente",
      hospit_pre_ICU_tradi ~ "Hospitalisation préalable (ICU/traditionnel)",
      
      # Poids / nutrition
      weight_loss_sup10 ~ "Perte de poids >10%",
      AlbuminLevel_admission ~ "Albuminémie à l'admission (g/L)",
      Albumin_sub35 ~ "Albumine <35 g/L",
      Malnutrition_BMIsub18_or_albusub35_or_weightloss ~ "Malnutrition (IMC<18, albumine<35 ou perte de poids)",
      
      # Biologie
      CRP_admission ~ "CRP à l'admission (mg/L)",
      CRP_admission_sup30 ~ "CRP >30 mg/L",
      Hb_preop_last2mo ~ "Hémoglobine pré-op (g/dL, 2 mois)",
      Hb_preopInf10_5_g_dL ~ "Hémoglobine <10,5 g/dL",
      calpro_a_ladmission ~ "Calprotectine à l'admission (µg/g)",
      leucocytose_admission ~ "Leucocytes à l'admission (/mm³)",
      leucocytosis_sup_10_or_sub_4 ~ "Leucocytes >10 ou <4",
      plaquettes_admission ~ "Plaquettes à l'admission (/mm³)",
      thrombopenia_sub150 ~ "Thrombopénie <150 G/L",
      thrombocytosis_sup400 ~ "Thrombocytose >400 G/L",
      
      # Imagerie
      imagerie_au_diagnostic ~ "Imagerie réalisée au diagnostic",
      imagerie_dg_laquelle ~ "Imagerie: type",
      imagerie_dg_TDM ~ "Scanner (TDM)",
      imagerie_dg_ASP ~ "ASP",
      
      # Endoscopie / UCEIS
      rectosig_au_diagnostic ~ "Rectosigmoïdoscopie au diagnostic",
      rectosig_poussee_nombre ~ "Nombre de rectosigmoïdoscopies pendant poussée",
      rectosig_CAG_UCEIS_numero_1 ~ "Score UCEIS (1er exam)",
      rectosig_CAG_limite_sup_atteinte ~ "Limite proximale atteinte",
      rectosig_CAG_derniere_UCEIS ~ "Score UCEIS (dernier exam)",
      rectosig_derniere_delai ~ "Délai dernier exam (j)",
      score_UCEIS ~ "Score UCEIS global",
      
      # Infections / dysplasie
      CMV_YN ~ "Infection CMV",
      clostridium_YN ~ "Clostridium",
      dysplasie_YN ~ "Dysplasie",
      
      # Lichtiger & clinique
      Lichtiger ~ "Score Lichtiger",
      Lichtiger_sup_or_equal_10 ~ "Score Lichtiger ≥10",
      nb_selles_24h ~ "Nb de selles/24h",
      selles_nocturnes ~ "Selles nocturnes",
      saignement_rectal ~ "Saignements rectaux",
      incontinence_fecale ~ "Incontinence fécale",
      douleurs_abdo ~ "Douleurs abdominales",
      douleurs_abdo_provoquees ~ "Douleurs abdominales provoquées",
      etat_general ~ "Altération de l'état général",
      imodium ~ "Traitement par Imodium",
      lichtiger_diagnostic ~ "Score Lichtiger au diagnostic",
      Lichtiger_chirurgie ~ "Score Lichtiger pré-chirurgie",
      
      # Complications aiguës
      hemorragie_digestive_grave_YN ~ "Hémorragie digestive grave",
      perforation_YN ~ "Perforation",
      colectasie_caecum_mm ~ "Colectasie cæcum (mm)",
      colectasie_transverse_mm ~ "Colectasie transverse (mm)",
      colectasie_caecum_sup100mm ~ "Colectasie cæcum >100 mm",
      colectasie_transversesup60mm ~ "Colectasie transverse >60 mm",
      megacôlon_toxique_YN ~ "Mégacôlon toxique",
      instabilite_HD ~ "Instabilité hémodynamique",
      
      # ATCD digestifs
      previous_digestive_surgery ~ "ATCD chirurgie digestive",
      previous_digestive_surgery_whole_text ~ "ATCD chirurgie digestive (texte)",
      previous_digestive_resection ~ "ATCD résection digestive",
      previous_digestive_resection_for_MICI ~ "ATCD résection pour MICI",
      number_of_previous_resection ~ "Nb de résections digestives antérieures",
      
      # Topographie & lésions
      topographie_colite ~ "Topographie de la colite",
      endoscopie_lesions_severes ~ "Lésions sévères à l'endoscopie",
      ulcerations_creusantes ~ "Ulcérations creusantes",
      ulcerations_en_puits ~ "Ulcérations en puits",
      decollement_muqueux ~ "Décollement muqueux",
      abrasion_muqueuse ~ "Abrasion muqueuse"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

# Afficher
tableau3


#--------Stats chirurgie----
# Colonnes à inclure (une par ligne, regroupées par blocs)
cols_to_include_4 <- c(
  # voie d'abord
  "approach",
  "conversion",
  
  #gestion stomie
  "moignon_rectal_hartmann_ileosigmoidostomie_FID_sigmoïdostomie",
  
  #associé
  "drainage",
  "omentectomie",
  "curage",
  "duree_chirurgie_min",
  
  #adverse per op
  "transfusion",
  "complication_perop_all",
  "complication_perop_plaie_digestive",
  "complication_perop_plaie_vasculaire",
  "complication_perop_hemorragie",
  "complication_perop_other",
  "complication_perop_gestion",
  "complication_perop_whole_text"
)

tableau4 <- df %>%
  tbl_summary(
    by = delai_sup_30,
    include = all_of(cols_to_include_4),
    missing = "ifany",
    type  = list(all_dichotomous() ~ "dichotomous"),
    value = list(all_dichotomous() ~ 1),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_dichotomous() ~ "{n} ({p}%)",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_dichotomous() ~ 0,
      all_categorical() ~ 0
    ),
    label = list(
      approach ~ "Voie d'abord",
      conversion ~ "Conversion",
      moignon_rectal_hartmann_ileosigmoidostomie_FID_sigmoïdostomie ~ "Moignon rectal / stomie",
      drainage ~ "Drainage associé",
      omentectomie ~ "Omentectomie",
      curage ~ "Curage",
      duree_chirurgie_min ~ "Durée chirurgicale (min)",
      transfusion ~ "Transfusion",
      complication_perop_all ~ "Complication peropératoire (toutes)",
      complication_perop_plaie_digestive ~ "Plaie digestive peropératoire",
      complication_perop_plaie_vasculaire ~ "Plaie vasculaire peropératoire",
      complication_perop_hemorragie ~ "Hémorragie peropératoire",
      complication_perop_other ~ "Autre complication peropératoire",
      complication_perop_gestion ~ "Gestion de la complication",
      complication_perop_whole_text ~ "Description complication peropératoire"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

# Afficher
tableau4

#tableau4 mais sans by =
tableau4 <- df %>%
  tbl_summary(
    include = all_of(cols_to_include_4),
    missing = "ifany",
    type  = list(all_dichotomous() ~ "dichotomous"),
    value = list(all_dichotomous() ~ 1),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_dichotomous() ~ "{n} ({p}%)",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_dichotomous() ~ 0,
      all_categorical() ~ 0
    ),
    label = list(
      approach ~ "Voie d'abord",
      conversion ~ "Conversion",
      moignon_rectal_hartmann_ileosigmoidostomie_FID_sigmoïdostomie ~ "Moignon rectal / stomie",
      drainage ~ "Drainage associé",
      omentectomie ~ "Omentectomie",
      curage ~ "Curage",
      duree_chirurgie_min ~ "Durée chirurgicale (min)",
      transfusion ~ "Transfusion",
      complication_perop_all ~ "Complication peropératoire (toutes)",
      complication_perop_plaie_digestive ~ "Plaie digestive peropératoire",
      complication_perop_plaie_vasculaire ~ "Plaie vasculaire peropératoire",
      complication_perop_hemorragie ~ "Hémorragie peropératoire",
      complication_perop_other ~ "Autre complication peropératoire",
      complication_perop_gestion ~ "Gestion de la complication",
      complication_perop_whole_text ~ "Description complication peropératoire"
    )
  ) %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

tableau4

#--------Stats Postop----
# Colonnes à inclure (une par ligne, regroupées par blocs)
cols_to_include_5 <- c(
  # complications septiques/chirurgicales
  "intraabdominal_abcess_or_collection",
  "intra_abdominal_hematoma",
  "wound_complication",
  "detail_wound_complication",
  "peritonite",
  "radiological_drainage_for_complication",
  "reoperation_for_complication",
  "details_reoperation_for_complication",
  
  # iléus
  "ileus",
  "ileus_SNG_1_a_jeun_0",
  "ileus_SNG",
  "ileus_a_jeun_seul",
  
  # stomie / pariétal
  "stoma_related_complication",
  "detail_stoma_related_complication",
  
  # médicaux
  "rectal_bleeding",
  "anemia_transfusion",
  "infection_urinaire",
  "insuffisance_renale",
  "acute_urinary_retention",
  "pneumopathie",
  "MTEV",
  "catheter_infection",
  "bacteriema",
  "dehydratation_IV_fluids",
  "poor_control_of_pain",
  
  # autres & scores
  "other",
  "other_details",
  "other_sepsis",
  "ClavienDindo",
  "Dindo_sup2",
  
  # durées/sortie
  "duree_hospit_postop",
  "duree_hospit_sup8",
  
  # agrégats
  "Overall_morbidity",
  "Severe_Morbidity",
  "Intraabdominal_septic_complications",
  "Surgical_complications",
  "Medical_complications",
  "all_septic",
  "Stomial_complications",
  "All_Stomial_or_Wound_complications",
  "readmission_within_30d",
  "J_transit"
)

tableau5 <- df %>%
  tbl_summary(
    by = bissortie_pendant_traitement_YN,
    include = all_of(cols_to_include_5),
    missing = "ifany",
    type  = list(all_dichotomous() ~ "dichotomous"),
    value = list(all_dichotomous() ~ 1),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_dichotomous() ~ "{n} ({p}%)",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_dichotomous() ~ 0,
      all_categorical() ~ 0
    ),
    label = list(
      intraabdominal_abcess_or_collection ~ "Abcès/collection intra-abdominal",
      intra_abdominal_hematoma ~ "Hématome intra-abdominal",
      wound_complication ~ "Complication pariétale",
      detail_wound_complication ~ "Détail : complication pariétale",
      peritonite ~ "Péritonite",
      radiological_drainage_for_complication ~ "Drainage radiologique",
      reoperation_for_complication ~ "Réintervention",
      details_reoperation_for_complication ~ "Détail : réintervention",
      ileus ~ "Iléus",
      ileus_SNG_1_a_jeun_0 ~ "Iléus : SNG + mise à jeun",
      ileus_SNG ~ "Iléus : SNG seule",
      ileus_a_jeun_seul ~ "Iléus : mise à jeun seule",
      stoma_related_complication ~ "Complication stomiale",
      detail_stoma_related_complication ~ "Détail : complication stomiale",
      rectal_bleeding ~ "Hémorragie rectale",
      anemia_transfusion ~ "Anémie transfusée",
      infection_urinaire ~ "Infection urinaire",
      insuffisance_renale ~ "Insuffisance rénale aiguë",
      acute_urinary_retention ~ "Rétention aiguë d’urines",
      pneumopathie ~ "Pneumopathie",
      MTEV ~ "MTEV",
      catheter_infection ~ "Infection de cathéter",
      bacteriema ~ "Bactériémie",
      dehydratation_IV_fluids ~ "Déshydratation nécessitant perfusion",
      poor_control_of_pain ~ "Mauvais contrôle de la douleur",
      other ~ "Autre complication",
      other_details ~ "Autre : détails",
      other_sepsis ~ "Autre sepsis",
      ClavienDindo ~ "Clavien-Dindo",
      Dindo_sup2 ~ "Clavien-Dindo > II",
      duree_hospit_postop ~ "Durée d’hospitalisation postopératoire (jours)",
      duree_hospit_sup8 ~ "Durée d’hospitalisation > 8 jours",
      Overall_morbidity ~ "Morbimortalité globale",
      Severe_Morbidity ~ "Morbimortalité sévère",
      Intraabdominal_septic_complications ~ "Complications septiques intra-abdominales",
      Surgical_complications ~ "Complications chirurgicales",
      Medical_complications ~ "Complications médicales",
      all_septic ~ "Toutes complications septiques",
      Stomial_complications ~ "Complications stomiales",
      All_Stomial_or_Wound_complications ~ "Complications stomiales ou pariétales",
      readmission_within_30d ~ "Réadmission < 30 jours",
      J_transit ~ "Jours avant reprise du transit"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

# Afficher
tableau5


#tableau 5 mais sans by =
tableau5 <- df %>%
  tbl_summary(
    include = all_of(cols_to_include_5),
    missing = "ifany",
    type  = list(all_dichotomous() ~ "dichotomous"),
    value = list(all_dichotomous() ~ 1),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_dichotomous() ~ "{n} ({p}%)",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_dichotomous() ~ 0,
      all_categorical() ~ 0
    ),
    label = list(
      intraabdominal_abcess_or_collection ~ "Abcès/collection intra-abdominal",
      intra_abdominal_hematoma ~ "Hématome intra-abdominal",
      wound_complication ~ "Complication pariétale",
      detail_wound_complication ~ "Détail : complication pariétale",
      peritonite ~ "Péritonite",
      radiological_drainage_for_complication ~ "Drainage radiologique",
      reoperation_for_complication ~ "Réintervention",
      details_reoperation_for_complication ~ "Détail : réintervention",
      ileus ~ "Iléus",
      ileus_SNG_1_a_jeun_0 ~ "Iléus : SNG + mise à jeun",
      ileus_SNG ~ "Iléus : SNG seule",
      ileus_a_jeun_seul ~ "Iléus : mise à jeun seule",
      stoma_related_complication ~ "Complication stomiale",
      detail_stoma_related_complication ~ "Détail : complication stomiale",
      rectal_bleeding ~ "Hémorragie rectale",
      anemia_transfusion ~ "Anémie transfusée",
      infection_urinaire ~ "Infection urinaire",
      insuffisance_renale ~ "Insuffisance rénale aiguë",
      acute_urinary_retention ~ "Rétention aiguë d’urines",
      pneumopathie ~ "Pneumopathie",
      MTEV ~ "MTEV",
      catheter_infection ~ "Infection de cathéter",
      bacteriema ~ "Bactériémie",
      dehydratation_IV_fluids ~ "Déshydratation nécessitant perfusion",
      poor_control_of_pain ~ "Mauvais contrôle de la douleur",
      other ~ "Autre complication",
      other_details ~ "Autre : détails",
      other_sepsis ~ "Autre sepsis",
      ClavienDindo ~ "Clavien-Dindo",
      Dindo_sup2 ~ "Clavien-Dindo > II",
      duree_hospit_postop ~ "Durée d’hospitalisation postopératoire (jours)",
      duree_hospit_sup8 ~ "Durée d’hospitalisation > 8 jours",
      Overall_morbidity ~ "Morbimortalité globale",
      Severe_Morbidity ~ "Morbimortalité sévère",
      Intraabdominal_septic_complications ~ "Complications septiques intra-abdominales",
      Surgical_complications ~ "Complications chirurgicales",
      Medical_complications ~ "Complications médicales",
      all_septic ~ "Toutes complications septiques",
      Stomial_complications ~ "Complications stomiales",
      All_Stomial_or_Wound_complications ~ "Complications stomiales ou pariétales",
      readmission_within_30d ~ "Réadmission < 30 jours")
  ) %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")


tableau5







#--------Stats Postop en fonction de df$at_least_3_medical_lines----
# Colonnes à inclure (une par ligne, regroupées par blocs)
cols_to_include_5 <- c(
  # complications septiques/chirurgicales
  "intraabdominal_abcess_or_collection",
  "intra_abdominal_hematoma",
  "wound_complication",
  "detail_wound_complication",
  "peritonite",
  "radiological_drainage_for_complication",
  "reoperation_for_complication",
  "details_reoperation_for_complication",
  
  # iléus
  "ileus",
  "ileus_SNG_1_a_jeun_0",
  "ileus_SNG",
  "ileus_a_jeun_seul",
  
  # stomie / pariétal
  "stoma_related_complication",
  "detail_stoma_related_complication",
  
  # médicaux
  "rectal_bleeding",
  "anemia_transfusion",
  "infection_urinaire",
  "insuffisance_renale",
  "acute_urinary_retention",
  "pneumopathie",
  "MTEV",
  "catheter_infection",
  "bacteriema",
  "dehydratation_IV_fluids",
  "poor_control_of_pain",
  
  # autres & scores
  "other",
  "other_details",
  "other_sepsis",
  "ClavienDindo",
  "Dindo_sup2",
  
  # durées/sortie
  "duree_hospit_postop",
  "duree_hospit_sup8",
  
  # agrégats
  "Overall_morbidity",
  "Severe_Morbidity",
  "Intraabdominal_septic_complications",
  "Surgical_complications",
  "Medical_complications",
  "all_septic",
  "Stomial_complications",
  "All_Stomial_or_Wound_complications",
  "readmission_within_30d",
  "J_transit"
)

tableau5 <- df %>%
  tbl_summary(
    by = at_least_3_medical_lines,
    include = all_of(cols_to_include_5),
    missing = "ifany",
    type  = list(all_dichotomous() ~ "dichotomous"),
    value = list(all_dichotomous() ~ 1),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_dichotomous() ~ "{n} ({p}%)",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_dichotomous() ~ 0,
      all_categorical() ~ 0
    ),
    label = list(
      intraabdominal_abcess_or_collection ~ "Abcès/collection intra-abdominal",
      intra_abdominal_hematoma ~ "Hématome intra-abdominal",
      wound_complication ~ "Complication pariétale",
      detail_wound_complication ~ "Détail : complication pariétale",
      peritonite ~ "Péritonite",
      radiological_drainage_for_complication ~ "Drainage radiologique",
      reoperation_for_complication ~ "Réintervention",
      details_reoperation_for_complication ~ "Détail : réintervention",
      ileus ~ "Iléus",
      ileus_SNG_1_a_jeun_0 ~ "Iléus : SNG + mise à jeun",
      ileus_SNG ~ "Iléus : SNG seule",
      ileus_a_jeun_seul ~ "Iléus : mise à jeun seule",
      stoma_related_complication ~ "Complication stomiale",
      detail_stoma_related_complication ~ "Détail : complication stomiale",
      rectal_bleeding ~ "Hémorragie rectale",
      anemia_transfusion ~ "Anémie transfusée",
      infection_urinaire ~ "Infection urinaire",
      insuffisance_renale ~ "Insuffisance rénale aiguë",
      acute_urinary_retention ~ "Rétention aiguë d’urines",
      pneumopathie ~ "Pneumopathie",
      MTEV ~ "MTEV",
      catheter_infection ~ "Infection de cathéter",
      bacteriema ~ "Bactériémie",
      dehydratation_IV_fluids ~ "Déshydratation nécessitant perfusion",
      poor_control_of_pain ~ "Mauvais contrôle de la douleur",
      other ~ "Autre complication",
      other_details ~ "Autre : détails",
      other_sepsis ~ "Autre sepsis",
      ClavienDindo ~ "Clavien-Dindo",
      Dindo_sup2 ~ "Clavien-Dindo > II",
      duree_hospit_postop ~ "Durée d’hospitalisation postopératoire (jours)",
      duree_hospit_sup8 ~ "Durée d’hospitalisation > 8 jours",
      Overall_morbidity ~ "Morbimortalité globale",
      Severe_Morbidity ~ "Morbimortalité sévère",
      Intraabdominal_septic_complications ~ "Complications septiques intra-abdominales",
      Surgical_complications ~ "Complications chirurgicales",
      Medical_complications ~ "Complications médicales",
      all_septic ~ "Toutes complications septiques",
      Stomial_complications ~ "Complications stomiales",
      All_Stomial_or_Wound_complications ~ "Complications stomiales ou pariétales",
      readmission_within_30d ~ "Réadmission < 30 jours",
      J_transit ~ "Jours avant reprise du transit"
    )
  ) %>%
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

# Afficher
tableau5


#tableau 5 mais sans by =
tableau5 <- df %>%
  tbl_summary(
    include = all_of(cols_to_include_5),
    missing = "ifany",
    type  = list(all_dichotomous() ~ "dichotomous"),
    value = list(all_dichotomous() ~ 1),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_dichotomous() ~ "{n} ({p}%)",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_dichotomous() ~ 0,
      all_categorical() ~ 0
    ),
    label = list(
      intraabdominal_abcess_or_collection ~ "Abcès/collection intra-abdominal",
      intra_abdominal_hematoma ~ "Hématome intra-abdominal",
      wound_complication ~ "Complication pariétale",
      detail_wound_complication ~ "Détail : complication pariétale",
      peritonite ~ "Péritonite",
      radiological_drainage_for_complication ~ "Drainage radiologique",
      reoperation_for_complication ~ "Réintervention",
      details_reoperation_for_complication ~ "Détail : réintervention",
      ileus ~ "Iléus",
      ileus_SNG_1_a_jeun_0 ~ "Iléus : SNG + mise à jeun",
      ileus_SNG ~ "Iléus : SNG seule",
      ileus_a_jeun_seul ~ "Iléus : mise à jeun seule",
      stoma_related_complication ~ "Complication stomiale",
      detail_stoma_related_complication ~ "Détail : complication stomiale",
      rectal_bleeding ~ "Hémorragie rectale",
      anemia_transfusion ~ "Anémie transfusée",
      infection_urinaire ~ "Infection urinaire",
      insuffisance_renale ~ "Insuffisance rénale aiguë",
      acute_urinary_retention ~ "Rétention aiguë d’urines",
      pneumopathie ~ "Pneumopathie",
      MTEV ~ "MTEV",
      catheter_infection ~ "Infection de cathéter",
      bacteriema ~ "Bactériémie",
      dehydratation_IV_fluids ~ "Déshydratation nécessitant perfusion",
      poor_control_of_pain ~ "Mauvais contrôle de la douleur",
      other ~ "Autre complication",
      other_details ~ "Autre : détails",
      other_sepsis ~ "Autre sepsis",
      ClavienDindo ~ "Clavien-Dindo",
      Dindo_sup2 ~ "Clavien-Dindo > II",
      duree_hospit_postop ~ "Durée d’hospitalisation postopératoire (jours)",
      duree_hospit_sup8 ~ "Durée d’hospitalisation > 8 jours",
      Overall_morbidity ~ "Morbimortalité globale",
      Severe_Morbidity ~ "Morbimortalité sévère",
      Intraabdominal_septic_complications ~ "Complications septiques intra-abdominales",
      Surgical_complications ~ "Complications chirurgicales",
      Medical_complications ~ "Complications médicales",
      all_septic ~ "Toutes complications septiques",
      Stomial_complications ~ "Complications stomiales",
      All_Stomial_or_Wound_complications ~ "Complications stomiales ou pariétales",
      readmission_within_30d ~ "Réadmission < 30 jours")
  ) %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")


tableau5







#--------Score CPT-----

# Recalcul SCORE 1 à partir de 'df' uniquement (sans ever_sortie_med)
# CPT := 1 si delai_sup_30 == 0 ; sinon 0
# Items du score 1 : BIO + (>=2 lignes MED) + CRP>150 + Albumine<25 + RCH
# Sortie : % CPT par total (0..5) + IC95% + mapping logistique


suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(stringr) })

# -- Helpers (si absents) --
if (!exists("to_01")) {
  to_01 <- function(x){
    if (is.character(x)) x <- trimws(x)
    if (is.logical(x))   x <- as.integer(x)
    if (is.factor(x))    x <- as.character(x)
    x <- suppressWarnings(as.numeric(x))
    x[is.na(x)] <- 0; x[x != 0] <- 1; x
  }
}
if (!exists("cp_ci")) {
  cp_ci <- function(events, n, conf.level = 0.95){
    bt <- binom.test(events, n, conf.level = conf.level)
    c(low=bt$conf.int[1], high=bt$conf.int[2])
  }
}

stopifnot(exists("df"))

df_work <- df

# 1) CPT depuis delai_sup_30 (ta règle) 
if (!"delai_sup_30" %in% names(df_work)) stop("Il manque 'delai_sup_30' dans df.")
delai_num <- suppressWarnings(as.numeric(as.character(df_work$delai_sup_30)))
df_work$CPT_bin <- ifelse(!is.na(delai_num) & delai_num == 0, 1, 0)

#2) BIO (biothérapies) : détecter colonnes pertinentes (robuste listes)
#2) BIO (biothérapies) : détecter colonnes pertinentes (robuste listes)
bio_patterns <- "(bio|bioth|anti\\s*tnf|antitnf|adalim|inflix|golim|certol|vedoliz|vedolizumab|usteki|ustekinumab|tofa|stelara)"

bio_cols <- grep(bio_patterns, names(df_work), ignore.case = TRUE, value = TRUE)

# util: aplatir list-columns -> character
flatten_col <- function(v) {
  if (is.list(v)) {
    return(vapply(v, function(el) {
      if (length(el) == 0 || all(is.na(el))) "" else paste(as.character(el), collapse = " ")
    }, character(1)))
  }
  v
}

BIO <- rep(0L, nrow(df_work))  # important: partir de 0, pas NA

if (length(bio_cols) > 0) {
  tmp <- df_work[, bio_cols, drop = FALSE]
  
  for (j in seq_along(bio_cols)) {
    v <- tmp[[j]]
    v <- flatten_col(v)
    
    if (is.character(v) || is.factor(v)) {
      v <- as.character(v)
      BIO <- pmax(BIO, as.integer(grepl(bio_patterns, v, ignore.case = TRUE)), na.rm = TRUE)
    } else if (is.logical(v)) {
      BIO <- pmax(BIO, as.integer(v), na.rm = TRUE)
    } else {
      # numeric / codé 0-1 / ou texte numérique
      BIO <- pmax(BIO, to_01(v), na.rm = TRUE)
    }
  }
  
} else {
  message("⚠ Aucune colonne biothérapie/antiTNF détectée dans df : BIO restera à 0 (si c’est faux, ajuste 'bio_patterns').")
}

# (optionnel) log de contrôle
cat("\n[DEBUG] Colonnes BIO détectées :", if (length(bio_cols)) paste(bio_cols, collapse = ", ") else "aucune", "\n")





# ---------- 3) Lignes MED (exclure chirurgie) 
line_cols <- grep("(^|_)\\d(st|nd|rd|th)_lign_ttt$", names(df_work), ignore.case=TRUE, value=TRUE)
if (length(line_cols) == 0) line_cols <- grep("lign_ttt", names(df_work), ignore.case=TRUE, value=TRUE)

surgery_patterns <- "(chir|colect|stomie|stoma|ileo|ilé|anastom|lapar|bloc|oper|opér|surgery)"
is_medical_line <- function(v){
  v <- tolower(trimws(as.character(v)))
  ifelse(is.na(v) | v=="", FALSE, !grepl(surgery_patterns, v))
}

if (length(line_cols) > 0) {
  nb_lignes_medicales <- apply(df_work[, line_cols, drop=FALSE], 1, function(row) {
    sum(vapply(as.list(row), is_medical_line, logical(1)))
  })
} else if ("n_lignes_tentees" %in% names(df_work)) {
  message("⚠ Pas de colonnes ‘xth_lign_ttt’. Utilisation de n_lignes_tentees (vérifie qu’elle EXCLUT la chirurgie).")
  nb_lignes_medicales <- suppressWarnings(as.numeric(df_work$n_lignes_tentees))
} else {
  stop("Impossible de compter les lignes : ni ‘xth_lign_ttt’ ni ‘n_lignes_tentees’ dans df.")
}
LIGNES2 <- as.integer(nb_lignes_medicales >= 2)

# ---------- 4) CRP et Albumine (chercher colonnes si noms différents)
pick_numeric <- function(df, pattern_primary, pattern_backup=NULL){
  cands <- grep(pattern_primary, names(df), ignore.case=TRUE, value=TRUE)
  if (length(cands)==0 && !is.null(pattern_backup)) {
    cands <- grep(pattern_backup, names(df), ignore.case=TRUE, value=TRUE)
  }
  # garder numériques
  cands <- cands[sapply(cands, function(v) is.numeric(df[[v]]) || all(grepl("^\\s*-?\\d+(\\.\\d+)?\\s*$", as.character(df[[v]]), perl=TRUE) | is.na(df[[v]])))]
  if (length(cands)==0) return(NULL)
  cands[1]
}

crp_col <- if ("CRP_admission" %in% names(df_work)) "CRP_admission" else pick_numeric(df_work, "CRP.*admi", "CRP")
alb_col <- if ("AlbuminLevel_admission" %in% names(df_work)) "AlbuminLevel_admission" else pick_numeric(df_work, "albu|albumin", "album")

if (is.null(crp_col)) stop("Colonne CRP (numérique) introuvable (ex. ‘CRP_admission’).")
if (is.null(alb_col)) stop("Colonne Albumine (numérique) introuvable (ex. ‘AlbuminLevel_admission’).")

CRP150 <- as.integer(suppressWarnings(as.numeric(df_work[[crp_col]])) > 150)
ALB25  <- as.integer(suppressWarnings(as.numeric(df_work[[alb_col]])) < 25)

# ---------- 5) RCH (si pas binaire déjà, tenter à partir de Crohn/RCH)
if ("RCH" %in% names(df_work)) {
  RCH_bin <- to_01(df_work$RCH)
} else if ("Crohn_RCH" %in% names(df_work)) {
  v <- tolower(as.character(df_work$Crohn_RCH))
  RCH_bin <- as.integer(grepl("rch|colite|ulc(é|e)reuse|uc", v))
} else {
  stop("Colonne RCH introuvable (ni ‘RCH’ ni ‘Crohn_RCH’).")
}

# ---------- 6) Construire SCORE1 et afficher ce qui a été utilisé)
df_work$BIO     <- BIO
df_work$LIGNES2 <- LIGNES2
df_work$CRP150  <- CRP150
df_work$ALB25   <- ALB25
df_work$RCH_bin <- RCH_bin

# Score avec NA -> on exige que les 5 items soient connus (retirer NA)
items <- c("BIO","LIGNES2","CRP150","ALB25","RCH_bin")
na_rows <- !stats::complete.cases(df_work[, items])
if (any(na_rows)) {
  message("ℹ ", sum(na_rows), " lignes ont des NA dans les items du score et seront exclues pour le calcul du % par score.")
}
df_score <- df_work[!na_rows, , drop=FALSE] %>%
  mutate(SCORE1 = BIO + LIGNES2 + CRP150 + ALB25 + RCH_bin)

cat("\n=== Items utilisés ===\n")
cat("BIO from cols: ", if(length(bio_cols)) paste(bio_cols, collapse=", ") else "aucune", "\n", sep="")
cat("LIGNES from cols: ", if(length(line_cols)) paste(line_cols, collapse=", ") else "n_lignes_tentees", "\n", sep="")
cat("CRP numeric col: ", crp_col, "\n", sep="")
cat("Albumine col: ", alb_col, "\n", sep="")
cat("RCH source: ", if("RCH" %in% names(df_work)) "RCH" else "Crohn_RCH (parsé)", "\n", sep="")

# ---------- 7) % CPT observé par total + IC exacts
risk_by_score <- df_score %>%
  group_by(SCORE1) %>%
  summarise(
    n_total = n(),
    n_CPT   = sum(CPT_bin, na.rm=TRUE),
    risk    = n_CPT/n_total,
    .groups = "drop"
  ) %>%
  rowwise() %>% mutate(ci = list(cp_ci(n_CPT, n_total))) %>%
  mutate(ci_low = ci["low"], ci_high = ci["high"]) %>%
  ungroup() %>%
  transmute(
    SCORE1, n_total, n_CPT,
    `Risque CPT %` = round(100*risk, 1),
    `IC95% bas %`  = round(100*as.numeric(ci_low), 1),
    `IC95% haut %` = round(100*as.numeric(ci_high), 1)
  ) %>%
  arrange(SCORE1)

cat("\n=== % CPT OBSERVÉ PAR SCORE (IC 95% exacts) ===\n")
print(risk_by_score)

# ---------- 8) Mapping logistique points -> probabilité (optionnel)
fit_pts <- glm(CPT_bin ~ SCORE1, data=df_score, family=binomial())
grid <- data.frame(SCORE1=0:5)
pred <- predict(fit_pts, newdata=grid, type="link", se.fit=TRUE)
grid$prob <- plogis(pred$fit); grid$low <- plogis(pred$fit-1.96*pred$se.fit); grid$high <- plogis(pred$fit+1.96*pred$se.fit)

cat("\n=== MAPPING (LOGISTIQUE) — Probabilité CPT par score (IC 95%) ===\n")
print( transform(grid,
                 `Prob. CPT % (modèle)`=round(100*prob,1),
                 `IC95% bas %`=round(100*low,1),
                 `IC95% haut %`=round(100*high,1)
)[,c("SCORE1","Prob. CPT % (modèle)","IC95% bas %","IC95% haut %")] )

# ---------- 9) (optionnel) Distribution brute
cat("\n=== DISTRIBUTION BRUTE SCORE x CPT ===\n")
print(addmargins(table(Score=df_score$SCORE1, CPT=df_score$CPT_bin)))









suppressPackageStartupMessages({library(ggplot2)})

# Si besoin, (re)fabriquer risk_by_score à partir de df_score
if (!exists("risk_by_score")) {
  risk_by_score <- df_score |>
    dplyr::group_by(SCORE1) |>
    dplyr::summarise(n_total = dplyr::n(),
                     n_CPT = sum(CPT_bin, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::rowwise() |>
    dplyr::mutate(ci = list(cp_ci(n_CPT, n_total)),
                  ci_low = ci["low"], ci_high = ci["high"],
                  risk = n_CPT/n_total) |>
    dplyr::ungroup() |>
    dplyr::mutate(`Risque CPT %` = 100*risk,
                  `IC95% bas %`  = 100*as.numeric(ci_low),
                  `IC95% haut %` = 100*as.numeric(ci_high))
}

ggplot(risk_by_score,
       aes(x = SCORE1, y = `Risque CPT %`)) +
  geom_pointrange(aes(ymin = `IC95% bas %`, ymax = `IC95% haut %`), size=.8) +
  geom_line() +
  scale_x_continuous(breaks = 0:5) +
  labs(title = "% CPT observé par score",
       x = "Score (0–5)",
       y = "Risque CPT (%)",
       caption = "Barres = IC95% exacts (Clopper–Pearson).") +
  theme_minimal(base_size = 12)


# Grid issu de ton glm (SCORE1 -> prob), sinon on le (re)crée
if (!exists("grid")) {
  fit_pts <- glm(CPT_bin ~ SCORE1, data = df_score, family = binomial())
  grid <- data.frame(SCORE1 = 0:5)
  pred <- predict(fit_pts, newdata = grid, type = "link", se.fit = TRUE)
  grid$prob <- plogis(pred$fit)
  grid$low  <- plogis(pred$fit - 1.96*pred$se.fit)
  grid$high <- plogis(pred$fit + 1.96*pred$se.fit)
}

ggplot() +
  # Modèle lissé (ligne + ruban IC)
  geom_ribbon(data = grid, aes(x = SCORE1, ymin = 100*low, ymax = 100*high),
              alpha = .15) +
  geom_line(data = grid, aes(x = SCORE1, y = 100*prob), linewidth = 1) +
  # Points observés (avec IC)
  geom_pointrange(data = risk_by_score,
                  aes(x = SCORE1, y = `Risque CPT %`,
                      ymin = `IC95% bas %`, ymax = `IC95% haut %`),
                  color = "black", size=.7) +
  scale_x_continuous(breaks = 0:5) +
  labs(title = "Calibration: observé vs prédiction logistique",
       x = "Score (0–5)", y = "Probabilité / Risque (%)",
       caption = "Ligne/ruban = modèle logistique (IC95%); points/barres = observé (IC95%).") +
  theme_minimal(base_size = 12)


suppressPackageStartupMessages(library(pROC))

roc_obj <- roc(df_score$CPT_bin, df_score$SCORE1, quiet = TRUE)
auc_val <- as.numeric(auc(roc_obj))
ci_auc  <- ci.auc(roc_obj)

# Version ggplot (pROC::ggroc)
p <- ggroc(roc_obj, colour = "#1f77b4", size = 1.2) +
  annotate("text", x = 0.7, y = 0.2,
           label = sprintf("AUC = %.3f (IC95%% %.3f–%.3f)", auc_val, ci_auc[1], ci_auc[3]),
           hjust = 0, size = 4) +
  labs(title = "Courbe ROC du score",
       x = "1 - Spécificité", y = "Sensibilité") +
  theme_minimal(base_size = 12)
print(p)

#même courbe ROC un peu lissée et avec ligne diagonale passant par 0.5
p + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  coord_fixed()  # carrée











tab_counts <- as.data.frame(table(Score = df_score$SCORE1, CPT = df_score$CPT_bin))
tab_counts$CPT <- factor(tab_counts$CPT, levels = c(0,1), labels = c("Non-CPT","CPT"))

ggplot(tab_counts, aes(x = factor(Score), y = Freq, fill = CPT)) +
  geom_col(width = .7) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5), size = 3.5) +
  scale_fill_manual(values = c("#bdbdbd", "#2ca02c")) +
  labs(title = "Distribution brute (comptes) par score",
       x = "Score", y = "Nombre de patients", fill = "") +
  theme_minimal(base_size = 12)




# Tableau perfs pour seuils entiers
seuils <- 0:5
perf_df <- lapply(seuils, function(k){
  pred <- as.integer(df_score$SCORE1 >= k)
  tp <- sum(pred==1 & df_score$CPT_bin==1)
  tn <- sum(pred==0 & df_score$CPT_bin==0)
  fp <- sum(pred==1 & df_score$CPT_bin==0)
  fn <- sum(pred==0 & df_score$CPT_bin==1)
  data.frame(
    seuil = k,
    Sens = ifelse((tp+fn)>0, tp/(tp+fn), NA),
    Spec = ifelse((tn+fp)>0, tn/(tn+fp), NA),
    PPV  = ifelse((tp+fp)>0, tp/(tp+fp), NA),
    NPV  = ifelse((tn+fn)>0, tn/(tn+fn), NA)
  )
}) |> dplyr::bind_rows()

perf_long <- tidyr::pivot_longer(perf_df, cols = c(Sens, Spec, PPV, NPV),
                                 names_to = "metrique", values_to = "val")

ggplot(perf_long, aes(x = seuil, y = 100*val, color = metrique)) +
  geom_line() + geom_point(size = 2) +
  scale_x_continuous(breaks = 0:5) +
  labs(title = "Performance selon le seuil (Score ≥ k)",
       x = "Seuil k", y = "Valeur (%)", color = "") +
  theme_minimal(base_size = 12)



df_cat <- df_score |>
  dplyr::mutate(cat = dplyr::case_when(
    SCORE1 <= 2 ~ "Faible (0–2)",
    SCORE1 == 3 ~ "Intermédiaire (3)",
    SCORE1 >= 4 ~ "Élevé (4–5)"
  ))

tab_cat <- as.data.frame(table(Catégorie = df_cat$cat, CPT = df_cat$CPT_bin))
tab_cat$CPT <- factor(tab_cat$CPT, levels = c(0,1), labels = c("Non-CPT","CPT"))

ggplot(tab_cat, aes(x = Catégorie, y = Freq, fill = CPT)) +
  geom_col(width = .7, position = "fill") +
  scale_y_continuous(labels = function(z) paste0(100*z, "%")) +
  scale_fill_manual(values = c("#bdbdbd", "#2ca02c")) +
  labs(title = "Risque CPT par catégorie (proportions empilées)",
       x = "", y = "Proportion", fill = "") +
  theme_minimal(base_size = 12)








#refaire les stats en fonction des patients qui ont eut ou non une 3e ligne de traitement médical
df <- df %>%
  mutate(at_least_3_medical_lines = ifelse(`3rd_lign` == 1 & `3rd_lign_surgery` != 1, 1, 0))




colnames(df)
df$combientieme_poussee
claudeAddin(
)
