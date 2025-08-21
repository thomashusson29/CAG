##-------PACKAGES DE BASE-------
if (!require("devtools")) install.packages("devtools")
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

# Chargement des packages
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

##-------Import de la base----

gs4_deauth()  # aucune fenêtre OAuth, accès read-only aux feuilles publiques

df_total <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1Y-BqL27PF0h6lDOmTLCJV8pXXeJ6gYbTL55NRnkOAa0/edit?gid=1505593793",
  sheet = "Feuille1"
)

##-----Stats pour patients opérés uniquement (df$opere = 1)----
df <- df_total %>% 
  filter(opere == 1.00)

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
  "CCI2_moderate_to_severe_renal_disease_DFG>60",
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
    by = delai_sup_30,
    include = all_of(cols_to_include_1),
    missing = "ifany",
    type = list(
      Charlson_Comorbidity_total ~ "continuous"   # <-- forcée en continue
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



