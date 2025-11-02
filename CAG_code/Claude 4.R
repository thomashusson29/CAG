# ANALYSE DES TRAJECTOIRES PRÉOPÉRATOIRES DANS LES CAG
# Tous les patients ont été opérés - Analyse des profils

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gtsummary)
library(broom)
library(gridExtra)

# 1. CRÉATION DES PROFILS DE SORTIE
df_analysis <- df %>%
  mutate(
    profil_sortie = case_when(
      sortie_pendant_traitement_YN == 1 & delai_sup_30 == 0 ~ "Sortie temporaire courte (<30j)",
      sortie_pendant_traitement_YN == 1 & delai_sup_30 == 1 ~ "Sortie temporaire longue (>30j)",
      sortie_pendant_traitement_YN == 0 ~ "Pas de sortie",
      TRUE ~ NA_character_
    ),
    delai_admission_chir = as.numeric(delai_chirurgie_admission),
    delai_symptomes_chir = as.numeric(as.Date(surg_or_dg_date) - as.Date(date_debut_symptomes_episodes_actuel))
  )

print(table(df_analysis$profil_sortie, useNA = "always"))

# 2. DÉLAIS JUSQU'À LA CHIRURGIE
delais_analyse <- df_analysis %>%
  filter(!is.na(profil_sortie)) %>%
  group_by(profil_sortie) %>%
  summarise(
    n = n(),
    delai_adm_med = median(delai_admission_chir, na.rm = TRUE),
    delai_adm_Q1 = quantile(delai_admission_chir, 0.25, na.rm = TRUE),
    delai_adm_Q3 = quantile(delai_admission_chir, 0.75, na.rm = TRUE),
    delai_sympt_med = median(delai_symptomes_chir, na.rm = TRUE),
    delai_sympt_Q1 = quantile(delai_symptomes_chir, 0.25, na.rm = TRUE),
    delai_sympt_Q3 = quantile(delai_symptomes_chir, 0.75, na.rm = TRUE)
  )
print(delais_analyse)

kw_delai <- kruskal.test(delai_admission_chir ~ profil_sortie, 
                         data = filter(df_analysis, !is.na(profil_sortie)))
print(kw_delai)

# 3. CARACTÉRISTIQUES CLINIQUES PAR PROFIL
caract_profil <- df_analysis %>%
  filter(!is.na(profil_sortie)) %>%
  group_by(profil_sortie) %>%
  summarise(
    n = n(),
    age_med = median(age_at_surg_or_dg, na.rm = TRUE),
    pct_RCH = mean(RCH == 1, na.rm = TRUE) * 100,
    charlson_med = median(Charlson_Comorbidity_total, na.rm = TRUE),
    lichtiger_med = median(Lichtiger, na.rm = TRUE),
    CRP_med = median(CRP_admission, na.rm = TRUE),
    albumine_med = median(AlbuminLevel_admission, na.rm = TRUE),
    pct_hospit_ant = mean(hospitalisations_anterieures_pour_CAG_ou_corticothérapie == 1, na.rm = TRUE) * 100,
    nb_hospit_ant_med = median(nombre_hospit_antérieures_pour_CAG, na.rm = TRUE)
  )
print(caract_profil)

# 4. ANALYSE DES TRAITEMENTS PERMETTANT LA SORTIE
lines <- c("1st", "2nd", "3rd", "4th", "5th")
df_long_ttt <- map_dfr(lines, function(l) {
  tibble(
    IPP = df$IPP,
    line_num = recode(l, "1st"=1L, "2nd"=2L, "3rd"=3L, "4th"=4L, "5th"=5L),
    CTC = df[[paste0(l, "_lign_CTC")]],
    IS = df[[paste0(l, "_lign_IS")]],
    TNF = df[[paste0(l, "_lign_TNF")]],
    bio = df[[paste0(l, "_lign_bio")]],
    surgery = df[[paste0(l, "_lign_surgery")]],
    sortie_apres = df[[paste0(l, "_lign_sortie_apres_ttt_med")]]
  )
}) %>%
  filter(surgery != 1, sortie_apres == 1) %>%
  mutate(
    type_ttt = case_when(
      bio == 1 ~ "Biothérapie nouvelle",
      TNF == 1 ~ "Anti-TNF",
      IS == 1 ~ "Immunosuppresseur",
      CTC == 1 ~ "Corticoïdes",
      TRUE ~ "Autre"
    )
  )

ttt_sortie <- df_long_ttt %>%
  group_by(type_ttt, line_num) %>%
  summarise(n = n()) %>%
  arrange(line_num, desc(n))
print(ttt_sortie)

# 5. IMPACT DES NOUVEAUX TRAITEMENTS
df_ttt_impact <- df_analysis %>%
  mutate(
    any_antiTNF = (infliximab_fond == 1 | adalimumab_YN == 1 | historique_antiTNF_YN == 1),
    any_new_bio = (vedolizumab_fond_ou_poussee_ancienne == 1 | tofacitinib_YN == 1 | 
                     stelara_YN == 1 | historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_golimumab_YN == 1),
    exposition_bio = case_when(
      any_new_bio ~ "Biothérapies nouvelles",
      any_antiTNF ~ "Anti-TNF",
      historique_immunosuppresseurs_MTX_AZT_purinethol_YN_ciclosporine == 1 ~ "IS classiques",
      TRUE ~ "Conventionnel"
    )
  )

trajectoires_bio <- df_ttt_impact %>%
  filter(!is.na(profil_sortie)) %>%
  group_by(exposition_bio, profil_sortie) %>%
  summarise(n = n()) %>%
  group_by(exposition_bio) %>%
  mutate(pct = round(n / sum(n) * 100, 1))
print(trajectoires_bio)

# 6. SUITES OPÉRATOIRES SELON LE PROFIL
suites_op <- df_analysis %>%
  filter(!is.na(profil_sortie)) %>%
  group_by(profil_sortie) %>%
  summarise(
    n = n(),
    duree_hospit_med = median(duree_hospit_postop, na.rm = TRUE),
    pct_hospit_longue = mean(duree_hospit_sup8 == 1, na.rm = TRUE) * 100,
    pct_dindo_sup2 = mean(Dindo_sup2 == 1, na.rm = TRUE) * 100,
    pct_morbi_severe = mean(Severe_Morbidity == 1, na.rm = TRUE) * 100,
    pct_reop = mean(reoperation_for_complication == 1, na.rm = TRUE) * 100,
    pct_readmis_30j = mean(readmission_within_30d == 1, na.rm = TRUE) * 100
  )
print(suites_op)

chi_morbi <- chisq.test(table(df_analysis$profil_sortie[!is.na(df_analysis$profil_sortie)], 
                              df_analysis$Severe_Morbidity[!is.na(df_analysis$profil_sortie)]))
print(chi_morbi)

# 7. ANALYSE COMPLÉMENTAIRE : NOMBRE DE LIGNES DE TRAITEMENT
df_lignes <- map_dfr(lines, function(l) {
  tibble(
    IPP = df$IPP,
    line_num = recode(l, "1st"=1L, "2nd"=2L, "3rd"=3L, "4th"=4L, "5th"=5L),
    attempted = df[[paste0(l, "_lign")]],
    surgery = df[[paste0(l, "_lign_surgery")]]
  )
}) %>%
  filter(attempted == 1, surgery != 1) %>%
  group_by(IPP) %>%
  summarise(n_lignes_med = n())

df_analysis_lignes <- df_analysis %>%
  left_join(df_lignes, by = "IPP") %>%
  mutate(n_lignes_med = replace_na(n_lignes_med, 0))

lignes_par_profil <- df_analysis_lignes %>%
  filter(!is.na(profil_sortie)) %>%
  group_by(profil_sortie) %>%
  summarise(
    n = n(),
    lignes_med = median(n_lignes_med, na.rm = TRUE),
    lignes_moy = mean(n_lignes_med, na.rm = TRUE),
    lignes_Q1 = quantile(n_lignes_med, 0.25, na.rm = TRUE),
    lignes_Q3 = quantile(n_lignes_med, 0.75, na.rm = TRUE)
  )
print(lignes_par_profil)

kw_lignes <- kruskal.test(n_lignes_med ~ profil_sortie, 
                          data = filter(df_analysis_lignes, !is.na(profil_sortie)))
print(kw_lignes)

# 8. VISUALISATION PRINCIPALE
# Préparer les données
group_data <- data.frame(
  Profil = c("Pas de\nsortie", "Sortie\ncourte\n(<30j)", "Sortie\nlongue\n(>30j)"),
  n = c(21, 10, 5),
  pct = c(58.3, 27.8, 13.9),
  delai_med = c(16, 7.5, 17),
  lignes_med = c(1, 2, 3)
)

# Figure 1: Répartition
p1 <- ggplot(group_data, aes(x = Profil, y = n, fill = Profil)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(n, "\n(", round(pct, 1), "%)")), 
            vjust = 1.5, size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("#2E7D32", "#FF6F00", "#1976D2")) +
  labs(title = "Répartition des profils préopératoires",
       y = "Nombre de patients", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# Figure 2: Délais
p2 <- ggplot(group_data, aes(x = Profil, y = delai_med, fill = Profil)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(delai_med, " j")), 
            vjust = 1.5, size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("#2E7D32", "#FF6F00", "#1976D2")) +
  labs(title = "Délai médian admission-chirurgie",
       y = "Jours", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# Figure 3: Intensité thérapeutique
p3 <- ggplot(df_analysis_lignes %>% filter(!is.na(profil_sortie)), 
             aes(x = profil_sortie, y = n_lignes_med, fill = profil_sortie)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("#2E7D32", "#FF6F00", "#1976D2")) +
  scale_x_discrete(labels = c("Pas de\nsortie", "Sortie\ncourte", "Sortie\nlongue")) +
  labs(title = "Nombre de lignes médicales",
       subtitle = paste0("p = ", round(kw_lignes$p.value, 3)),
       y = "Nombre de lignes", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# Figure 4: Suites opératoires
suites_data <- data.frame(
  Profil = rep(c("Pas de\nsortie", "Sortie\ncourte", "Sortie\nlongue"), 2),
  Metric = rep(c("Durée hospit", "Morbidité sévère"), each = 3),
  Value = c(9, 12.5, 7, 33.3, 20, 20)
)

p4 <- ggplot(filter(suites_data, Metric == "Durée hospit"), 
             aes(x = Profil, y = Value, fill = Profil)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(Value, " j")), 
            vjust = 1.5, size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("#2E7D32", "#FF6F00", "#1976D2")) +
  labs(title = "Durée d'hospitalisation postopératoire",
       y = "Jours", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

grid.arrange(p1, p2, p3, p4, ncol = 2, 
             top = "Analyse des trajectoires préopératoires dans les CAG")

# 9. SYNTHÈSE FINALE
cat("\n=== SYNTHÈSE DES RÉSULTATS ===\n")
cat("Profils identifiés:\n")
cat("- Pas de sortie: 21 patients (58.3%)\n")
cat("- Sortie courte (<30j): 10 patients (27.8%)\n")
cat("- Sortie longue (>30j): 5 patients (13.9%)\n\n")

cat("Points clés:\n")
cat("- 41.7% ont eu une sortie temporaire avant chirurgie\n")
cat("- 67% des sorties reviennent dans les 30 jours\n")
cat("- Anti-TNF en 2ème ligne = principal traitement de sortie\n")
cat("- Sortie courte associée à hospitalisation postop plus longue\n")
cat("- Pas d'amélioration de la morbidité avec sortie temporaire\n")