#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: setup
#| include: false
#| echo: false
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
library(kableExtra)
library(stringr)
knitr::opts_chunk$set(echo = TRUE)
#
#
#
#
#| label: importation_donnees
#| echo: false
gs4_deauth()  # aucune fenêtre OAuth, accès read-only

df_total <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1Y-BqL27PF0h6lDOmTLCJV8pXXeJ6gYbTL55NRnkOAa0/edit?gid=1505593793",
    sheet = "Feuille1"
) %>%
    filter(vrai_CAG == 1)

df <- df_total
#
#
#
#
#
#| label: recodage_variables
#| echo: false
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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: pourcentage_opere
#| echo: false
#| results: "asis"
#| fig.cap: "Répartition des patients opérés vs non opérés"
library(ggplot2)
library(dplyr)

# 1. Préparer les données dans un data.frame (nécessaire pour ggplot)
df_pie <- df %>%
    count(opere) %>%
    mutate(
    pct = round(n / sum(n) * 100, 1),
    label = paste0(ifelse(opere == 1, "Opéré", "Non opéré"), "\n", pct, "%"),
    # Position des labels au milieu des parts
    ypos = cumsum(n) - 0.5 * n 
    )

# 2. Graphique
pie_chart_gg <- ggplot(df_pie, aes(x = "", y = n, fill = as.factor(opere))) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    theme_void() + # Enlève les axes gris
    scale_fill_manual(values = c("lightblue", "lightcoral")) +
    geom_text(aes(y = ypos, label = label), color = "black", size = 5) +
    labs(title = "Répartition des patients opérés vs non opérés", fill = "Statut")

# Afficher
print(pie_chart_gg)

# 3. Export avec ggsave (Maintenant ça marche !)
# Note : 'units' doit être "in", "cm", "mm", ou "px". "dpi" est un argument à part.
ggsave(
        "~/Documents/Projets/CAG/CAG_code/CAG_figures/pie_chart_operes.svg", 
        plot = pie_chart_gg, 
        width = 6, 
        height = 4, 
        units = "in"
        )
#
#
#
#
#
#| label: cols_to_include_1
#| echo: false
#| results: hide
#| message: false
cols_to_include_1 <- c(
    "age_at_surg_or_dg", 
    "BMI", 
    "sex", 
    "RCH_YN", 
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
    #"CCI1_Prior_Myocardial",
    #"CCI1_Congestive_HF",
    #"CCI1_Peripheral_vascular",
    #"CCI1_Cerebrovascular_disease",
    #"CCI1_Dementia",
    #"CCI1_Chronic_pulmonary_disease",
    #"CCI1_Rheumatologic_disease",
    #"CCI1_Peptic_ulcer_disease",
    #"CCI1_Mild_liver_disease_(legere)",
    #"CCI1_Diabetes",
    #"CCI2_cerebrovascular_hemiplegia_event",
    #"CCI2_moderate_to_severe_renal_disease_DFGsup60",
    #"CCI2_diabetes_chronic_complications",
    #"CCI2_cancer_without_metastases",
    #"CCI2_leukemia",
    #"CCI2_lymphoma_myeloma",
    #"CCI3_moderate_severe_liver_disease",
    #"CCI6_metastatic_solid_tumor",
    #"CCI6_SIDA",
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

tableau1 <- df %>%
    # Ajout du recodage pour la variable 'opere'
    mutate(
        opere = factor(opere, 
                        levels = c(0, 1), 
                        labels = c("Medical", "Surgery"))
    ) %>%
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
    by = opere,
    label = list(
        age_at_surg_or_dg ~ "Age (years)",
        BMI ~ "BMI (kg/m²)",
        sex ~ "Sex (n, %)",
        RCH_YN ~ "RCH (n, %)",
        ASAscore ~ "ASA score (n, %)",
        ASA_sup_2 ~ "ASA score > 2 (n, %)",
        active_smoker ~ "Active smoker",
        IBD_duration_years ~ "IBD duration (years)",
        IBD_age_of_diagnosis ~ "Age of IBD diagnosis (years)",
        Charlson_Comorbidity_total ~ "Charlson Comorbidity Score",
        combientieme_poussee ~ "Which flare number (continuous)",
        poussee_inaugurale_Y_N ~ "Inaugural flare (n, %)",
        hospitalisations_anterieures_pour_CAG_ou_corticothérapie ~ "Previous hospitalizations for CAG or corticosteroid therapy (n, %)",
        hospitalisations_anterieures_pourquoi ~ "Reason for previous hospitalizations",
        ttt_dernière_CAG_CTC_seuls ~ "Treatment of the last CAG: corticosteroids only (n, %)",
        ttt_dernière_CAG_CTC_et_2eligne ~ "Treatment of the last CAG: corticosteroids + second line (n, %)",
        smoker ~ "Smoking status (n, %)",
        IBD_duration_days ~ "IBD duration (days)",
        CSP_associee ~ "Associated PSC (n, %)",
        uveite_associee ~ "Associated uveitis (n, %)",
        psoriasis_ou_dermato_associee ~ "Associated psoriasis or dermatosis (n, %)",
        aphtose_associee ~ "Associated aphthosis (n, %)",
        appendicectomy_YN ~ "Appendicectomy (n, %)",
        SpA ~ "Ankylosing spondylitis (n, %)",
        LAP_associees ~ "Associated LAP (n, %)",
        MontrealClassA_A1sub16_A21740_A3sup40 ~ "Montreal Classification - Age class (n, %)",
        MontrealClassB_B1nistricturenipenetrate_B2stricturing_B3_penetrate_B4_both ~ "Montreal Classification - Disease type (n, %)"
    )
    ) %>%
    modify_header(label ~ "**Characteristic**") %>%
    modify_footnote(all_stat_cols() ~ "Median (Q1, Q3) or n (%)") %>%
    bold_labels() %>%
    add_p()
#
#
#
#| label: afficher_tableau1
#| echo: false
#| results: "asis"

library(kableExtra)

tableau1 %>%
    # Conversion en objet kable (LaTeX standard)
    as_kable_extra(booktabs = TRUE, longtable = TRUE) %>%
    # C'est ICI que la magie opère : on force la colonne 1 à faire 6cm de large
    # LaTeX va automatiquement faire des retours à la ligne pour le texte qui dépasse
    kableExtra::column_spec(1, width = "6cm") %>%
    # (Optionnel) Ajuste la taille de la police si le tableau est encore trop large
    kableExtra::kable_styling(latex_options = c("repeat_header"), font_size = 9)
#
#
#
#
#
