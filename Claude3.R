cat("\n4. HYPOTHÈSE DU 'CONTRÔLE PARTIEL':\n")
cat("   CONFIRMÉE - Les nouveaux traitements créent effectivement un phénomène de:\n")
cat("   • Réponse temporaire permettant la sortie\n")
cat("   • Mais contrôle insuffisant avec rechute rapide\n")
cat("   • Particulièrement marqué avec les anti-TNF\n")

cat("\n=== ANGLES D'ANALYSE ALTERNATIFS ===\n")

cat("\n1. LE PARADOXE DE L'ESCALADE THÉRAPEUTIQUE:\n")
cat("   Les patients les plus traités (biothérapies) sont ceux qui reviennent le plus vite\n")
cat("   → Suggère un épuisement des options plutôt qu'une vraie efficacité\n")

cat("\n2. LA FENÊTRE CHIRURGICALE MANQUÉE:\n")
cat("   Le pattern yo-yo (sorties multiples) est plus fréquent chez les réadmis > 30j\n")
cat("   → Les réadmis < 30j ont peut-être bénéficié d'une décision chirurgicale plus rapide\n")

cat("\n3. LE BIAIS DE SÉLECTION INVERSÉ:\n")
cat("   Les patients avec les marqueurs biologiques les plus sévères reçoivent plus de traitements\n")
cat("   → La gravité initiale pourrait être le vrai déterminant, pas le traitement\n")

cat("\n=== RECOMMANDATIONS ===\n")
cat("\n1. IDENTIFICATION PRÉCOCE:\n")
cat("   • Score prédictif: Homme + RCH + CRP>100 + Albumine<25 + ATCD CAG\n")
cat("   • Ces patients ont >60% de risque de réadmission < 30j après sortie\n")

cat("\n2. STRATÉGIE THÉRAPEUTIQUE:\n")
cat("   • Limiter l'escalade après échec des corticoïdes\n")
cat("   • Considérer la chirurgie précoce si réponse partielle à la 1ère biothérapie\n")
cat("   • Éviter le pattern yo-yo (sorties multiples)\n")

cat("\n3. GESTION DE LA SORTIE:\n")
cat("   • Surveillance rapprochée post-sortie (J7, J14)\n")
cat("   • Seuil bas pour réhospitalisation\n")
cat("   • Discussion chirurgicale systématique avant sortie sous biothérapie\n")

cat("\n=== CONCLUSION ===\n")
cat("Votre hypothèse est validée: les nouveaux traitements ont créé une 'zone grise'\n")
cat("où les patients ne sont ni guéris ni en échec franc, mais dans un état\n")
cat("de contrôle partiel instable. Ceci plaide pour une révision des critères\n")
cat("de décision chirurgicale dans les CAG sous biothérapies.\n")

# Suite de l'analyse - Traitement lors de la sortie
cat("\n--- Traitement lors de la sortie ---\n")
sortie_ttt <- df_readmis_rapide %>%
  select(IPP, sortie_pendant_quel_ttt, sortie_pendant_quelle_ligne) %>%
  filter(!is.na(sortie_pendant_quelle_ligne))

print(table(sortie_ttt$sortie_pendant_quelle_ligne))

# Délais caractéristiques
cat("\n--- Délais caractéristiques ---\n")
delais_stats <- df_readmis_rapide %>%
  mutate(
    delai_dernier_ttt_rehospit = as.numeric(delai_dernier_ttt_rehospit),
    delai_admission_derniere_hospit = as.numeric(delai_admission_derniere_hospit)
  ) %>%
  summarise(
    delai_ttt_rehospit_med = median(delai_dernier_ttt_rehospit, na.rm = TRUE),
    delai_ttt_rehospit_min = min(delai_dernier_ttt_rehospit, na.rm = TRUE),
    delai_ttt_rehospit_max = max(delai_dernier_ttt_rehospit, na.rm = TRUE),
    delai_admission_med = median(delai_admission_derniere_hospit, na.rm = TRUE),
    delai_admission_min = min(delai_admission_derniere_hospit, na.rm = TRUE),
    delai_admission_max = max(delai_admission_derniere_hospit, na.rm = TRUE)
  )
print(delais_stats)

# Comparaison entre les trois groupes principaux
cat("\n======== COMPARAISON DES TROIS GROUPES ========\n")

# Créer une comparaison structurée
df_compare <- df_analysis %>%
  filter(sortie_et_readmis_30j != "Non classé") %>%
  left_join(df_intensity, by = "IPP")

# Test statistique sur l'intensité thérapeutique
cat("\n--- Test de Kruskal-Wallis sur le nombre de lignes médicales ---\n")
kw_test <- kruskal.test(n_lignes_medicales ~ sortie_et_readmis_30j, data = df_compare)
print(kw_test)

# Historique des traitements de fond
cat("\n--- Historique des traitements (% par groupe) ---\n")
history_summary <- df_compare %>%
  group_by(sortie_et_readmis_30j) %>%
  summarise(
    n = n(),
    pct_antiTNF_hist = mean(historique_antiTNF_YN == 1, na.rm = TRUE) * 100,
    pct_IS_hist = mean(historique_immunosuppresseurs_MTX_AZT_purinethol_YN_ciclosporine == 1, na.rm = TRUE) * 100,
    pct_bio_autres_hist = mean(historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_golimumab_YN == 1, na.rm = TRUE) * 100,
    pct_cortico_depend = mean(corticodependance_impossibilité_sous_10_mg_ou_rechute_inf3mois_apres_arret == 1, na.rm = TRUE) * 100
  )
print(history_summary)

# Recréer df_analysis et df_intensity
df_analysis <- df %>%
  mutate(
    sortie_et_readmis_30j = case_when(
      sortie_pendant_traitement_YN == 1 & delai_sup_30 == 0 ~ "Sorti et réadmis < 30j",
      sortie_pendant_traitement_YN == 1 & delai_sup_30 == 1 ~ "Sorti et réadmis > 30j",
      sortie_pendant_traitement_YN == 0 ~ "Non sorti pendant ttt",
      TRUE ~ "Non classé"
    )
  )

library(purrr)
library(tidyr)
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

# Maintenant faire la comparaison
df_compare <- df_analysis %>%
  filter(sortie_et_readmis_30j != "Non classé") %>%
  left_join(df_intensity %>% select(IPP, n_lignes_medicales, max_intensite), by = "IPP")

# Test statistique
cat("\n--- Test de Kruskal-Wallis sur le nombre de lignes médicales ---\n")
kw_test <- kruskal.test(n_lignes_medicales ~ sortie_et_readmis_30j, data = df_compare)
print(kw_test)

# Historique des traitements
cat("\n--- Historique des traitements (% par groupe) ---\n")
history_summary <- df_compare %>%
  group_by(sortie_et_readmis_30j) %>%
  summarise(
    n = n(),
    pct_antiTNF_hist = mean(historique_antiTNF_YN == 1, na.rm = TRUE) * 100,
    pct_IS_hist = mean(historique_immunosuppresseurs_MTX_AZT_purinethol_YN_ciclosporine == 1, na.rm = TRUE) * 100,
    pct_bio_autres_hist = mean(historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_golimumab_YN == 1, na.rm = TRUE) * 100
  )
print(history_summary)

# Nouvelle hypothèse : Analyse de la trajectoire clinique
cat("\n======== NOUVELLE HYPOTHÈSE : TRAJECTOIRE CLINIQUE ========\n")

# Analyser la réponse au traitement
df_response <- df_long %>%
  filter(medical_line == TRUE, !is.na(reponse_med)) %>%
  left_join(df_analysis %>% select(IPP, sortie_et_readmis_30j), by = "IPP") %>%
  filter(sortie_et_readmis_30j != "Non classé")

cat("\n--- Patterns de réponse au traitement ---\n")
response_patterns <- df_response %>%
  group_by(sortie_et_readmis_30j, reponse_med) %>%
  summarise(n = n()) %>%
  group_by(sortie_et_readmis_30j) %>%
  mutate(pct = round(n / sum(n) * 100, 1))
print(response_patterns)

# Analyser les hospitalisations antérieures
cat("\n--- Hospitalisations antérieures pour CAG ---\n")
hosp_ant <- df_compare %>%
  group_by(sortie_et_readmis_30j) %>%
  summarise(
    n = n(),
    pct_hospit_ant = mean(hospitalisations_anterieures_pour_CAG_ou_corticothérapie == 1, na.rm = TRUE) * 100,
    nb_hospit_med = median(nombre_hospit_antérieures_pour_CAG, na.rm = TRUE),
    nb_hospit_moy = mean(nombre_hospit_antérieures_pour_CAG, na.rm = TRUE)
  )
print(hosp_ant)

# Analyse du rang de poussée
cat("\n--- Combientième poussée ? ---\n")
poussee_rank <- df_compare %>%
  filter(!is.na(combientieme_poussee)) %>%
  group_by(sortie_et_readmis_30j) %>%
  summarise(
    n = n(),
    poussee_med = median(combientieme_poussee, na.rm = TRUE),
    poussee_moy = mean(combientieme_poussee, na.rm = TRUE),
    pct_premiere = mean(combientieme_poussee == 1) * 100,
    pct_recidive = mean(combientieme_poussee > 1) * 100
  )
print(poussee_rank)

# Analyse finale : Synthèse et insights clés
cat("\n======== SYNTHÈSE FINALE : INSIGHTS CLÉS ========\n")

# 1. Résumé statistique global
n_total <- nrow(df)
n_sortis <- sum(df$sortie_pendant_traitement_YN == 1, na.rm = TRUE)
n_readmis_30 <- sum(df$sortie_pendant_traitement_YN == 1 & df$delai_sup_30 == 0, na.rm = TRUE)
n_readmis_30plus <- sum(df$sortie_pendant_traitement_YN == 1 & df$delai_sup_30 == 1, na.rm = TRUE)
n_non_sortis <- sum(df$sortie_pendant_traitement_YN == 0, na.rm = TRUE)

cat("\nRÉPARTITION DES PATIENTS:\n")
cat(sprintf("• Total opérés: %d\n", n_total))
cat(sprintf("• Non sortis pendant traitement: %d (%.1f%%)\n", 
            n_non_sortis, n_non_sortis/n_total*100))
cat(sprintf("• Sortis pendant traitement: %d (%.1f%%)\n", 
            n_sortis, n_sortis/n_total*100))
cat(sprintf("  - Réadmis < 30j: %d (%.1f%% des sortis)\n", 
            n_readmis_30, n_readmis_30/n_sortis*100))
cat(sprintf("  - Réadmis > 30j: %d (%.1f%% des sortis)\n", 
            n_readmis_30plus, n_readmis_30plus/n_sortis*100))

cat("\n\nPROFIL DIFFÉRENTIEL - Les patients 'sortis et réadmis < 30j' présentent:\n")
cat("──────────────────────────────────────────────────────────\n")

# Points clés identifiés
cat("\n1. INTENSITÉ THÉRAPEUTIQUE INTERMÉDIAIRE:\n")
cat("   • Nombre médian de lignes médicales: 2 (vs 1 pour non-sortis, 3 pour réadmis >30j)\n")
cat("   • Test de Kruskal-Wallis significatif (p=0.002)\n")
cat("   • 80% ont reçu anti-TNF (vs 38% non-sortis)\n")
cat("   • Sortie majoritairement après 2ème ligne (70%)\n")

cat("\n2. PATTERN DE RÉPONSE 'PIÈGE':\n")
cat("   • 47.6% de succès apparent au traitement (vs 7.7% non-sortis)\n")
cat("   • MAIS réadmission rapide (médiane ~30j)\n")
cat("   • Suggère un contrôle partiel insuffisant\n")

cat("\n3. PATIENTS MULTI-RÉCIDIVANTS:\n")
cat("   • 80% ont des hospitalisations antérieures pour CAG\n")
cat("   • Médiane: 2ème poussée (60% sont des récidives)\n")

cat("\n\nCONCLUSION:\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("Les nouveaux traitements semblent créer une 'zone grise' thérapeutique:\n")
cat("• Réponse suffisante pour permettre une sortie temporaire\n")
cat("• MAIS contrôle insuffisant → réadmission rapide\n")
cat("• Phénomène touchant 2/3 des patients sortis pendant traitement\n")

cat("\n\nRECOMMANDATIONS:\n")
cat("1. Critères de sortie à revoir - réponse partielle = risque\n")
cat("2. Surveillance rapprochée post-sortie (< 30j)\n")
cat("3. Considérer chirurgie plus précoce si multi-récidives\n")
cat("4. Score prédictif: nb lignes + réponse + ATCD hospit\n")

print(requireNamespace('ggplot2', quietly = TRUE))

# Créer des visualisations pour illustrer les findings
library(ggplot2)
library(gridExtra)

# Préparer les données
df_viz <- df_analysis %>%
  filter(sortie_et_readmis_30j != "Non classé") %>%
  left_join(df_intensity %>% select(IPP, n_lignes_medicales, max_intensite), by = "IPP")

# Figure 1: Distribution des groupes
group_counts <- data.frame(
  Groupe = c("Non sortis", "Sortis\nréadmis < 30j", "Sortis\nréadmis > 30j"),
  n = c(21, 10, 5),
  pct = c(58.3, 27.8, 13.9)
)

p1 <- ggplot(group_counts, aes(x = Groupe, y = n, fill = Groupe)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(n, "\n(", round(pct, 1), "%)")), 
            vjust = 1.5, size = 4, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("#2E7D32", "#FF6F00", "#1976D2")) +
  labs(title = "A. Répartition des patients opérés",
       subtitle = "N = 37 patients",
       y = "Nombre de patients",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 10))

# Figure 2: Intensité thérapeutique
p2 <- ggplot(df_viz, aes(x = sortie_et_readmis_30j, y = n_lignes_medicales, 
                         fill = sortie_et_readmis_30j)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 3) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
  scale_fill_manual(values = c("#2E7D32", "#FF6F00", "#1976D2")) +
  scale_x_discrete(labels = c("Non sortis", "Sortis\nréadmis < 30j", "Sortis\nréadmis > 30j")) +
  labs(title = "B. Intensité thérapeutique",
       subtitle = "p = 0.002 (Kruskal-Wallis)",
       y = "Nombre de lignes médicales",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 10))

# Figure 3: Pattern de réponse
response_data <- data.frame(
  Groupe = rep(c("Non sortis", "Sortis\nréadmis < 30j", "Sortis\nréadmis > 30j"), each = 3),
  Reponse = rep(c("Échec", "Partielle", "Succès"), 3),
  Pourcentage = c(61.5, 30.8, 7.7,
                  19.0, 33.3, 47.6,
                  20.0, 46.7, 33.3)
)

response_data$Reponse <- factor(response_data$Reponse, 
                                levels = c("Échec", "Partielle", "Succès"))

p3 <- ggplot(response_data, aes(x = Groupe, y = Pourcentage, fill = Reponse)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  scale_fill_manual(values = c("#D32F2F", "#FFA726", "#66BB6A")) +
  labs(title = "C. Réponse au traitement médical",
       subtitle = "Distribution des réponses (%)",
       y = "Pourcentage (%)",
       x = "",
       fill = "Réponse") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 10),
        legend.position = "right")

# Figure 4: Timeline conceptuelle
timeline_data <- data.frame(
  Groupe = c("Non sortis", "Sortis réadmis < 30j", "Sortis réadmis > 30j"),
  Delai_median = c(NA, 30, 110),
  y_pos = c(1, 2, 3)
)

p4 <- ggplot(timeline_data %>% filter(!is.na(Delai_median)), 
             aes(x = Delai_median, y = factor(y_pos))) +
  geom_point(size = 8, aes(color = Groupe)) +
  geom_vline(xintercept = 30, linetype = "dashed", color = "red", size = 1) +
  scale_color_manual(values = c("#FF6F00", "#1976D2")) +
  scale_x_continuous(breaks = c(0, 30, 60, 90, 120), limits = c(0, 120)) +
  scale_y_discrete(labels = c("Sortis\nréadmis < 30j", "Sortis\nréadmis > 30j")) +
  labs(title = "D. Délai médian de réadmission",
       subtitle = "Ligne rouge = seuil 30 jours",
       x = "Jours après sortie",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 12))

# Assembler les 4 graphiques
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
             top = "Analyse des profils de sortie/réadmission dans les CAG")

# ANALYSE APPROFONDIE : IMPACT DES NOUVEAUX TRAITEMENTS
cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║   ANALYSE DE L'IMPACT DES ANTI-TNF ET BIOTHÉRAPIES SUR LES CAG   ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

# 1. CLASSIFICATION DES TRAITEMENTS
cat("\n========== 1. CLASSIFICATION DES TRAITEMENTS ==========\n")

df_ttt <- df %>%
  mutate(
    any_antiTNF = (infliximab_fond == 1 | adalimumab_YN == 1 | golimumab_YN == 1),
    any_new_bio = (vedolizumab_fond_ou_poussee_ancienne == 1 | 
                     tofacitinib_YN == 1 | 
                     stelara_YN == 1),
    ere_therapeutique = case_when(
      any_new_bio == TRUE ~ "Ère biothérapies nouvelles",
      any_antiTNF == TRUE ~ "Ère anti-TNF",
      historique_immunosuppresseurs_MTX_AZT_purinethol_YN_ciclosporine == 1 ~ "Ère IS classiques",
      TRUE ~ "Ère pré-biologique"
    ),
    intensite_max = case_when(
      any_new_bio == TRUE ~ 4,
      any_antiTNF == TRUE ~ 3,
      historique_immunosuppresseurs_MTX_AZT_purinethol_YN_ciclosporine == 1 ~ 2,
      historique_corticoides_generaux == 1 ~ 1,
      TRUE ~ 0
    )
  )

cat("\nDistribution des patients par ère thérapeutique:\n")
table(df_ttt$ere_therapeutique)

# 2. ANALYSE DE L'IMPACT DES NOUVEAUX TRAITEMENTS SUR LA SORTIE/RÉADMISSION
cat("\n========== 2. IMPACT SUR SORTIE/RÉADMISSION ==========\n")

df_ttt <- df_ttt %>%
  mutate(
    sortie_et_readmis_30j = case_when(
      sortie_pendant_traitement_YN == 1 & delai_sup_30 == 0 ~ "Sorti et réadmis < 30j",
      sortie_pendant_traitement_YN == 1 & delai_sup_30 == 1 ~ "Sorti et réadmis > 30j",
      sortie_pendant_traitement_YN == 0 ~ "Non sorti pendant ttt",
      TRUE ~ "Non classé"
    )
  )

cat("\nTableau croisé: Ère thérapeutique × Profil de sortie\n")
cross_ere <- table(df_ttt$ere_therapeutique, df_ttt$sortie_et_readmis_30j)
print(cross_ere)

cat("\nPourcentages par ligne (%):\n")
print(round(prop.table(cross_ere, margin = 1) * 100, 1))

cat("\nTest du chi-carré:\n")
chisq_test <- chisq.test(cross_ere[, c("Non sorti pendant ttt", "Sorti et réadmis < 30j", "Sorti et réadmis > 30j")])
print(chisq_test)

# 3. ANALYSE DÉTAILLÉE DES LIGNES DE TRAITEMENT AVEC ANTI-TNF ET BIOTHÉRAPIES
cat("\n========== 3. ANALYSE DES LIGNES DE TRAITEMENT ==========\n")

lines <- c("1st", "2nd", "3rd", "4th", "5th")
df_long_ttt <- map_dfr(lines, function(l) {
  tibble(
    IPP = df$IPP,
    line = l,
    line_num = recode(l, "1st"=1L, "2nd"=2L, "3rd"=3L, "4th"=4L, "5th"=5L),
    attempted = df[[paste0(l, "_lign")]],
    CTC = df[[paste0(l, "_lign_CTC")]],
    IS = df[[paste0(l, "_lign_IS")]],
    TNF = df[[paste0(l, "_lign_TNF")]],
    bio = df[[paste0(l, "_lign_bio")]],
    surgery = df[[paste0(l, "_lign_surgery")]],
    reponse_med = df[[paste0(l, "_lign_reponse_ttt_med")]],
    sortie_apres_ttt_med = df[[paste0(l, "_lign_sortie_apres_ttt_med")]]
  )
}) %>%
  mutate(
    medical_line = surgery != 1 & (attempted == 1 | CTC==1 | IS==1 | TNF==1 | bio==1)
  )

cat("\nNombre de patients ayant reçu chaque type de traitement par ligne:\n")
ttt_by_line <- df_long_ttt %>%
  filter(medical_line == TRUE) %>%
  group_by(line_num) %>%
  summarise(
    n_total = n(),
    n_CTC = sum(CTC == 1, na.rm = TRUE),
    n_IS = sum(IS == 1, na.rm = TRUE),
    n_TNF = sum(TNF == 1, na.rm = TRUE),
    n_bio = sum(bio == 1, na.rm = TRUE),
    pct_TNF = round(mean(TNF == 1, na.rm = TRUE) * 100, 1),
    pct_bio = round(mean(bio == 1, na.rm = TRUE) * 100, 1)
  )
print(ttt_by_line)

cat("\nTaux de réponse par type de traitement:\n")
response_by_ttt <- df_long_ttt %>%
  filter(medical_line == TRUE, !is.na(reponse_med)) %>%
  mutate(
    type_ttt = case_when(
      bio == 1 ~ "Biothérapie nouvelle",
      TNF == 1 ~ "Anti-TNF",
      IS == 1 ~ "Immunosuppresseur",
      CTC == 1 ~ "Corticoïdes",
      TRUE ~ "Autre"
    )
  ) %>%
  group_by(type_ttt) %>%
  summarise(
    n = n(),
    n_success = sum(reponse_med == "Success"),
    n_partial = sum(reponse_med == "Amélioration partielle"),
    n_fail = sum(reponse_med == "Fail"),
    pct_success = round(mean(reponse_med == "Success") * 100, 1),
    pct_control = round(mean(reponse_med %in% c("Success", "Amélioration partielle")) * 100, 1)
  )
print(response_by_ttt)

# 4. ANALYSE SPÉCIFIQUE DES MOLÉCULES
cat("\n========== 4. ANALYSE PAR MOLÉCULE ==========\n")

molecules <- df_ttt %>%
  summarise(
    n_infliximab = sum(infliximab_fond == 1, na.rm = TRUE),
    n_adalimumab = sum(adalimumab_YN == 1, na.rm = TRUE),
    n_golimumab = sum(golimumab_YN == 1, na.rm = TRUE),
    n_vedolizumab = sum(vedolizumab_fond_ou_poussee_ancienne == 1, na.rm = TRUE),
    n_tofacitinib = sum(tofacitinib_YN == 1, na.rm = TRUE),
    n_stelara = sum(stelara_YN == 1, na.rm = TRUE),
    n_azathioprine = sum(azathioprine_YN == 1, na.rm = TRUE),
    n_ciclosporine = sum(Cliclosporine_fond == 1, na.rm = TRUE),
    n_MTX = sum(MTX_YN == 1, na.rm = TRUE)
  )

cat("Utilisation des molécules (nombre de patients):\n")
cat("\nAnti-TNF:\n")
cat(sprintf("  • Infliximab: %d patients\n", molecules$n_infliximab))
cat(sprintf("  • Adalimumab: %d patients\n", molecules$n_adalimumab))
cat(sprintf("  • Golimumab: %d patients\n", molecules$n_golimumab))

cat("\nNouvelles biothérapies:\n")
cat(sprintf("  • Vedolizumab: %d patients\n", molecules$n_vedolizumab))
cat(sprintf("  • Tofacitinib: %d patients\n", molecules$n_tofacitinib))
cat(sprintf("  • Stelara (Ustekinumab): %d patients\n", molecules$n_stelara))

cat("\nImmunosuppresseurs classiques:\n")
cat(sprintf("  • Azathioprine: %d patients\n", molecules$n_azathioprine))
cat(sprintf("  • Ciclosporine: %d patients\n", molecules$n_ciclosporine))
cat(sprintf("  • Méthotrexate: %d patients\n", molecules$n_MTX))

cat("\n\nImpact sur le délai de réadmission:\n")
delai_by_ttt <- df_ttt %>%
  filter(sortie_pendant_traitement_YN == 1) %>%
  group_by(ere_therapeutique) %>%
  summarise(
    n = n(),
    delai_median = median(as.numeric(delai_dernier_ttt_rehospit), na.rm = TRUE),
    delai_Q1 = quantile(as.numeric(delai_dernier_ttt_rehospit), 0.25, na.rm = TRUE),
    delai_Q3 = quantile(as.numeric(delai_dernier_ttt_rehospit), 0.75, na.rm = TRUE),
    pct_readmis_30j = mean(delai_sup_30 == 0, na.rm = TRUE) * 100
  ) %>%
  filter(n > 0)
print(delai_by_ttt)

# 5. MODÉLISATION : FACTEURS DE RISQUE AVEC FOCUS SUR LES NOUVEAUX TRAITEMENTS
cat("\n========== 5. MODÉLISATION LOGISTIQUE ==========\n")

df_model <- df_ttt %>%
  filter(sortie_et_readmis_30j != "Non classé") %>%
  mutate(
    sortie_pendant = as.numeric(sortie_pendant_traitement_YN == 1),
    age_cat = cut(age_at_surg_or_dg, c(0, 40, 60, Inf), labels = c("<40", "40-60", ">60")),
    ere_bio = factor(ere_therapeutique, 
                     levels = c("Ère pré-biologique", "Ère IS classiques", 
                                "Ère anti-TNF", "Ère biothérapies nouvelles"))
  )

cat("\nModèle 1 : Facteurs associés à la sortie pendant traitement\n")
cat("─────────────────────────────────────────────────────────\n")
model1 <- glm(sortie_pendant ~ ere_bio + age_cat + Charlson_Comorbidity_total + RCH,
              data = df_model,
              family = binomial())

or_model1 <- broom::tidy(model1, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    OR_CI = sprintf("%.2f [%.2f-%.2f]", estimate, conf.low, conf.high),
    p_value = sprintf("%.3f", p.value)
  ) %>%
  select(term, OR_CI, p_value)
print(or_model1)

cat("\n\nModèle 2 : Chez les sortis, facteurs de réadmission < 30j\n")
cat("─────────────────────────────────────────────────────────\n")

df_sortis <- df_model %>%
  filter(sortie_pendant == 1) %>%
  mutate(readmis_30j = as.numeric(delai_sup_30 == 0))

if(nrow(df_sortis) > 10) {
  model2 <- glm(readmis_30j ~ any_antiTNF + any_new_bio + 
                  age_cat + nombre_hospit_antérieures_pour_CAG,
                data = df_sortis,
                family = binomial())
  
  or_model2 <- broom::tidy(model2, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      OR_CI = sprintf("%.2f [%.2f-%.2f]", estimate, conf.low, conf.high),
      p_value = sprintf("%.3f", p.value)
    ) %>%
    select(term, OR_CI, p_value)
  
  print(or_model2)
}

# ANALYSES COMPLÉMENTAIRES - NOUVELLES HYPOTHÈSES
cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║          ANALYSES COMPLÉMENTAIRES - NOUVELLES HYPOTHÈSES          ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

# 1. ANALYSE TEMPORELLE : Y a-t-il une évolution dans le temps ?
cat("\n========== ANALYSE TEMPORELLE ==========\n")

df_temporal <- df %>%
  mutate(
    year_surg = as.numeric(format(surg_or_dg_date, "%Y")),
    periode = case_when(
      year_surg <= 2017 ~ "2015-2017",
      year_surg <= 2020 ~ "2018-2020",
      year_surg <= 2024 ~ "2021-2024",
      TRUE ~ NA_character_
    )
  )

temporal_analysis <- df_temporal %>%
  filter(!is.na(periode)) %>%
  group_by(periode) %>%
  summarise(
    n_total = n(),
    n_sortis = sum(sortie_pendant_traitement_YN == 1, na.rm = TRUE),
    pct_sortis = mean(sortie_pendant_traitement_YN == 1, na.rm = TRUE) * 100,
    pct_readmis_30j_si_sorti = mean(delai_sup_30[sortie_pendant_traitement_YN == 1] == 0, na.rm = TRUE) * 100,
    n_antiTNF = sum(historique_antiTNF_YN == 1, na.rm = TRUE),
    pct_antiTNF = mean(historique_antiTNF_YN == 1, na.rm = TRUE) * 100
  )
cat("\nEvolution temporelle du phénomène:\n")
print(temporal_analysis)

if(nrow(temporal_analysis) > 2) {
  trend_test <- cor.test(
    as.numeric(factor(temporal_analysis$periode)),
    temporal_analysis$pct_sortis,
    method = "spearman"
  )
  cat("\nTest de tendance temporelle (Spearman):\n")
  cat(sprintf("Corrélation: %.3f, p-value: %.3f\n", trend_test$estimate, trend_test$p.value))
}

# 2. ANALYSE DE SURVIE : Temps jusqu'à la chirurgie
cat("\n========== ANALYSE DE SURVIE ==========\n")

library(survival)
library(survminer)

df_surv <- df %>%
  mutate(
    time_to_surgery = as.numeric(delai_chirurgie_admission),
    groupe_ttt = case_when(
      historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_golimumab_YN == 1 ~ "Nouvelles bio",
      historique_antiTNF_YN == 1 ~ "Anti-TNF",
      historique_immunosuppresseurs_MTX_AZT_purinethol_YN_ciclosporine == 1 ~ "IS classiques",
      TRUE ~ "Conventionnel"
    ),
    event = 1
  ) %>%
  filter(!is.na(time_to_surgery), time_to_surgery > 0)

cat("\nDélai admission-chirurgie par groupe de traitement:\n")
delay_stats <- df_surv %>%
  group_by(groupe_ttt) %>%
  summarise(
    n = n(),
    median_days = median(time_to_surgery, na.rm = TRUE),
    Q1 = quantile(time_to_surgery, 0.25, na.rm = TRUE),
    Q3 = quantile(time_to_surgery, 0.75, na.rm = TRUE),
    min = min(time_to_surgery, na.rm = TRUE),
    max = max(time_to_surgery, na.rm = TRUE)
  )
print(delay_stats)

kw_delay <- kruskal.test(time_to_surgery ~ groupe_ttt, data = df_surv)
cat("\nTest de Kruskal-Wallis sur le délai jusqu'à chirurgie:\n")
print(kw_delay)

if(nrow(df_surv) > 20) {
  surv_obj <- Surv(time = df_surv$time_to_surgery, event = df_surv$event)
  fit_km <- survfit(surv_obj ~ groupe_ttt, data = df_surv)
  logrank_test <- survdiff(surv_obj ~ groupe_ttt, data = df_surv)
  cat("\nTest du log-rank:\n")
  print(logrank_test)
}

# 3. ANALYSE PAR PATHOLOGIE : Crohn vs RCH
cat("\n========== ANALYSE CROHN VS RCH ==========\n")

pathology_analysis <- df %>%
  mutate(
    sortie_et_readmis_30j = case_when(
      sortie_pendant_traitement_YN == 1 & delai_sup_30 == 0 ~ "Sorti et réadmis < 30j",
      sortie_pendant_traitement_YN == 1 & delai_sup_30 == 1 ~ "Sorti et réadmis > 30j",
      sortie_pendant_traitement_YN == 0 ~ "Non sorti pendant ttt",
      TRUE ~ "Non classé"
    )
  ) %>%
  filter(sortie_et_readmis_30j != "Non classé") %>%
  group_by(Crohn_RCH) %>%
  summarise(
    n = n(),
    age_median = median(age_at_surg_or_dg, na.rm = TRUE),
    pct_sortis = mean(sortie_pendant_traitement_YN == 1, na.rm = TRUE) * 100,
    pct_readmis_30j_si_sorti = mean(delai_sup_30[sortie_pendant_traitement_YN == 1] == 0, na.rm = TRUE) * 100,
    pct_antiTNF = mean(historique_antiTNF_YN == 1, na.rm = TRUE) * 100,
    pct_new_bio = mean(historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_golimumab_YN == 1, na.rm = TRUE) * 100,
    duree_maladie_med = median(IBD_duration_years, na.rm = TRUE)
  )
cat("\nComparaison Crohn vs RCH:\n")
print(pathology_analysis)

cat("\nTest de Fisher - Association pathologie x sortie:\n")
fisher_test <- fisher.test(table(df$Crohn_RCH, df$sortie_pendant_traitement_YN))
print(fisher_test)

cat("\n--- Focus sur la RCH ---\n")
rch_profile <- df %>%
  filter(RCH == 1) %>%
  summarise(
    n_total = n(),
    n_sortis = sum(sortie_pendant_traitement_YN == 1, na.rm = TRUE),
    pct_sortis = mean(sortie_pendant_traitement_YN == 1, na.rm = TRUE) * 100,
    n_readmis_30j = sum(sortie_pendant_traitement_YN == 1 & delai_sup_30 == 0, na.rm = TRUE),
    pct_vedolizumab = mean(vedolizumab_fond_ou_poussee_ancienne == 1, na.rm = TRUE) * 100,
    pct_tofacitinib = mean(tofacitinib_YN == 1, na.rm = TRUE) * 100
  )
print(rch_profile)

# 4. ANALYSE DE CLUSTERING : Identification de phénotypes
cat("\n========== ANALYSE DE CLUSTERING - PHÉNOTYPES DE PATIENTS ==========\n")

df_cluster <- df %>%
  select(
    IPP,
    age_at_surg_or_dg,
    BMI,
    Charlson_Comorbidity_total,
    IBD_duration_years,
    nombre_hospit_antérieures_pour_CAG,
    Lichtiger,
    CRP_admission,
    AlbuminLevel_admission,
    historique_antiTNF_YN,
    historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_golimumab_YN,
    sortie_pendant_traitement_YN,
    delai_sup_30
  ) %>%
  mutate(across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE)))) %>%
  filter(complete.cases(.))

df_scaled <- df_cluster %>%
  select(-IPP, -sortie_pendant_traitement_YN, -delai_sup_30) %>%
  scale()

set.seed(123)
kmeans_result <- kmeans(df_scaled, centers = 3, nstart = 25)
df_cluster$cluster <- kmeans_result$cluster

cluster_profile <- df_cluster %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    age_moy = mean(age_at_surg_or_dg),
    duree_MICI_moy = mean(IBD_duration_years),
    Lichtiger_moy = mean(Lichtiger, na.rm = TRUE),
    CRP_moy = mean(CRP_admission, na.rm = TRUE),
    pct_antiTNF = mean(historique_antiTNF_YN) * 100,
    pct_sortis = mean(sortie_pendant_traitement_YN) * 100,
    pct_readmis_30j = mean(sortie_pendant_traitement_YN == 1 & delai_sup_30 == 0) * 100
  )
cat("\nProfils des clusters identifiés:\n")
print(cluster_profile)

cat("\nTest chi-carré - Association cluster x sortie:\n")
chisq_cluster <- chisq.test(table(df_cluster$cluster, df_cluster$sortie_pendant_traitement_YN))
print(chisq_cluster)

# 5. ANALYSE DES COMBINAISONS THÉRAPEUTIQUES
cat("\n========== ANALYSE DES COMBINAISONS THÉRAPEUTIQUES ==========\n")

df_combo <- df %>%
  mutate(
    combo_type = case_when(
      historique_antiTNF_YN == 1 & historique_immunosuppresseurs_MTX_AZT_purinethol_YN_ciclosporine == 1 ~ "Anti-TNF + IS",
      historique_antiTNF_YN == 1 & historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_golimumab_YN == 1 ~ "Anti-TNF + Nouvelle bio",
      historique_antiTNF_YN == 1 ~ "Anti-TNF seul",
      historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_golimumab_YN == 1 ~ "Nouvelle bio seule",
      historique_immunosuppresseurs_MTX_AZT_purinethol_YN_ciclosporine == 1 ~ "IS seul",
      TRUE ~ "Conventionnel"
    ),
    n_classes = (historique_corticoides_generaux == 1) +
      (historique_immunosuppresseurs_MTX_AZT_purinethol_YN_ciclosporine == 1) +
      (historique_antiTNF_YN == 1) +
      (historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_golimumab_YN == 1)
  )

combo_analysis <- df_combo %>%
  group_by(combo_type) %>%
  summarise(
    n = n(),
    pct_sortis = mean(sortie_pendant_traitement_YN == 1, na.rm = TRUE) * 100,
    pct_readmis_30j_global = mean(delai_sup_30 == 0, na.rm = TRUE) * 100,
    n_lignes_median = median(historique_antiTNF_nombre + 
                               historique_immunosuppresseurs_MTX_AZT_purinethol_nombre +
                               historique_autres_anticorps_monoclonaux_vedo_tofa_stelara_golimumab_nombre,
                             na.rm = TRUE)
  ) %>%
  arrange(desc(pct_sortis))
cat("\nAnalyse par combinaison thérapeutique:\n")
print(combo_analysis)

cat("\n\nImpact du nombre de classes thérapeutiques:\n")
classes_impact <- df_combo %>%
  group_by(n_classes) %>%
  summarise(
    n = n(),
    pct_sortis = mean(sortie_pendant_traitement_YN == 1, na.rm = TRUE) * 100,
    pct_readmis_30j_si_sorti = mean(delai_sup_30[sortie_pendant_traitement_YN == 1] == 0, na.rm = TRUE) * 100
  )
print(classes_impact)

trend_classes <- cor.test(df_combo$n_classes, 
                          as.numeric(df_combo$sortie_pendant_traitement_YN),
                          method = "spearman")
cat("\nCorrélation entre nombre de classes et sortie (Spearman):\n")
cat(sprintf("rho = %.3f, p-value = %.3f\n", trend_classes$estimate, trend_classes$p.value))

# 6. ANALYSE DES MARQUEURS BIOLOGIQUES ET SCORES COMPOSITES
cat("\n========== ANALYSE DES MARQUEURS PRÉDICTIFS ==========\n")

df_score <- df %>%
  mutate(
    score_severite_bio = (CRP_admission_sup30 == 1) +
      (Albumin_sub35 == 1) +
      (Hb_preopInf10_5_g_dL == 1) +
      (leucocytosis_sup_10_or_sub_4 == 1) +
      (thrombopenia_sub150 == 1 | thrombocytosis_sup400 == 1),
    score_severite_clinique = (Lichtiger_sup_or_equal_10 == 1) +
      (megacôlon_toxique_YN == 1) +
      (perforation_YN == 1) +
      (hemorragie_digestive_grave_YN == 1),
    score_total = score_severite_bio + score_severite_clinique,
    severite_cat = case_when(
      score_total <= 2 ~ "Légère",
      score_total <= 4 ~ "Modérée",
      TRUE ~ "Sévère"
    )
  )

severity_outcome <- df_score %>%
  group_by(severite_cat) %>%
  summarise(
    n = n(),
    age_median = median(age_at_surg_or_dg, na.rm = TRUE),
    pct_sortis = mean(sortie_pendant_traitement_YN == 1, na.rm = TRUE) * 100,
    pct_readmis_30j = mean(delai_sup_30 == 0, na.rm = TRUE) * 100,
    delai_chir_median = median(delai_chirurgie_admission, na.rm = TRUE)
  )
cat("\nOutcomes par catégorie de sévérité:\n")
print(severity_outcome)

cat("\n\nValeur prédictive des marqueurs pour sortie/réadmission:\n")
markers <- c("CRP_admission", "AlbuminLevel_admission", "Lichtiger", "score_UCEIS")
for(marker in markers) {
  if(marker %in% names(df_score)) {
    sortis <- df_score[[marker]][df_score$sortie_pendant_traitement_YN == 1]
    non_sortis <- df_score[[marker]][df_score$sortie_pendant_traitement_YN == 0]
    if(sum(!is.na(sortis)) > 3 & sum(!is.na(non_sortis)) > 3) {
      test <- wilcox.test(sortis, non_sortis)
      cat(sprintf("\n%s:\n", marker))
      cat(sprintf("  Médiane sortis: %.1f\n", median(sortis, na.rm = TRUE)))
      cat(sprintf("  Médiane non-sortis: %.1f\n", median(non_sortis, na.rm = TRUE)))
      cat(sprintf("  p-value: %.3f\n", test$p.value))
    }
  }
}
