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