# -------- Stats clinique poussée ----)

tableau3 <- df %>%
  tbl_summary(
    by = delai_sup_30,
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
  add_p() %>%
  modify_header(label ~ "**Caractéristique**") %>%
  modify_footnote(all_stat_cols() ~ "Médiane (Q1, Q3) ou n (%)")

# Afficher
tableau3



library(gtsummary)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

export_tblsummary_csv <- function(tbl, file = "tbl_summary_export.csv") {
  # récupérer la version tibble
  df_tab <- as_tibble(tbl)
  
  # header (les "N = ...") → c'est dans l'attribut x$table_header
  header <- tbl$table_header %>%
    filter(column %in% names(df_tab)) %>%
    arrange(match(column, names(df_tab))) %>%
    pull(spanning_header) %>%
    replace_na("")
  
  # transformer colonnes de stats: séparer n et %
  df_clean <- df_tab %>%
    mutate(across(starts_with("stat_"), ~ str_trim(.))) %>%
    mutate(across(starts_with("stat_"), ~ ifelse(. == "", NA, .))) %>%
    tidyr::separate_wider_regex(
      cols = starts_with("stat_"),
      patterns = c("(?<n>[0-9]+)", "\\s*\\((?<pct>[^)]+)\\)"),
      too_few = "align_start"
    )
  
  # ajouter la ligne header (N=...) au dessus
  header_row <- c("Caractéristique", header)
  out <- bind_rows(
    as.list(header_row),
    df_clean
  )
  
  # écrire le CSV
  readr::write_csv(out, file)
  message("✅ exporté vers: ", file)
}

# exemple d’utilisation
export_tblsummary_csv(tableau3, "tableau3_export.csv")

