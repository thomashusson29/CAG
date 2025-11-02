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




