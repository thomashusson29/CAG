##-------INSTALLATION PACKAGES DE BASE-------
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

##-------LIBRARY-----
library(ClaudeR)
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
    missing = "no",
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

##


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
    by = delai_sup_30,
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
    by = delai_sup_30,
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



#date de traitement de la poussée
df$`1st_lign_date`
df$`2nd_lign_date`
df$`3rd_lign_date`
df$`4th_lign_date`
df$`5th_lign_date`
df$date_admission_hopital

df$date_debut_symptomes_episodes_actuel

df$date_diagnostic_colite_aiguë_actuelle


claudeAddin()
