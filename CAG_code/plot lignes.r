# ---- Packages ----
library(tidyverse)
library(scales)

# ---- Données (adapter si besoin) ----
df <- tribble(
  ~groupe,                               ~N,  ~parcours,                         ~pct,
  "Sortie puis réadmis",                  15, "Chirurgie d’emblée",               0,
  "Sortie puis réadmis",                  15, "1ère ligne puis chirurgie",        6,
  "Sortie puis réadmis",                  15, "2e ligne puis chirurgie",         50,
  "Sortie puis réadmis",                  15, "≥ 3 lignes puis chirurgie",       45,
  "Opérés dans la même hospitalisation",  22, "Chirurgie d’emblée",              24,
  "Opérés dans la même hospitalisation",  22, "1ère ligne puis chirurgie",       33,
  "Opérés dans la même hospitalisation",  22, "2e ligne puis chirurgie",         50,
  "Opérés dans la même hospitalisation",  22, "≥ 3 lignes puis chirurgie",        5
) %>%
  mutate(
    parcours = factor(parcours,
                      levels = c("Chirurgie d’emblée", "1ère ligne puis chirurgie",
                                 "2e ligne puis chirurgie", "≥ 3 lignes puis chirurgie")
    ),
    groupe_lab = paste0(groupe, " (N=", N, ")"),
    n_approx = round(N * pct / 100)
  )

# =========================
# 1) DUMBBELL (comparaison)
# =========================
df_wide <- df %>%
  select(groupe_lab, parcours, pct) %>%
  distinct() %>%
  pivot_wider(names_from = groupe_lab, values_from = pct)

colA <- unique(df$groupe_lab[df$groupe == "Sortie puis réadmis"])
colB <- unique(df$groupe_lab[df$groupe == "Opérés dans la même hospitalisation"])

p1 <- ggplot(df_wide, aes(y = fct_rev(parcours))) +
  geom_segment(aes(x = .data[[colA]], xend = .data[[colB]], yend = parcours),
               linewidth = 2, alpha = 0.35) +
  geom_point(aes(x = .data[[colA]]), size = 3) +
  geom_point(aes(x = .data[[colB]]), size = 3) +
  scale_x_continuous(labels = label_percent(accuracy = 1, scale = 1), limits = c(0, 100)) +
  labs(
    title = "Parcours avant chirurgie — comparaison de 2 groupes (dumbbell)",
    subtitle = paste(colA, "vs", colB, sep = "  •  "),
    x = "% des patients", y = NULL
  ) +
  theme_minimal(base_size = 12)

# =========================================
# 2) BARRES GROUPÉES (avec % et n≈ annotés)
# =========================================
p2 <- ggplot(df, aes(x = parcours, y = pct, fill = groupe_lab)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(pct, "%\n(n≈", n_approx, ")")),
            position = position_dodge(width = 0.7), vjust = -0.35, size = 3) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100), expand = expansion(mult = c(0, .08))) +
  labs(
    title = "Parcours avant chirurgie par groupe — barres groupées",
    x = NULL, y = "% des patients", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# ==========================================================
# 3) EMPILÉ 100 % (normalisé à 100 % dans chaque groupe)
#    Utile si les % fournis ne somment pas exactement à 100
# ==========================================================
df_norm <- df %>%
  group_by(groupe_lab) %>%
  mutate(pct_norm = 100 * pct / sum(pct, na.rm = TRUE)) %>%
  ungroup()

p3 <- ggplot(df_norm, aes(x = groupe_lab, y = pct_norm, fill = parcours)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(pct_norm), "%")),
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Répartition des parcours (normalisée à 100 % par groupe)",
    x = NULL, y = "Part relative (100 %)", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# ---- Affichage ----
p1
p2
p3



# ---- Packages ----
library(tidyverse)
library(scales)

# ---- Données (reprend tes chiffres) ----
df <- tribble(
  ~groupe,                               ~N,  ~parcours,                         ~pct,
  "Sortie puis réadmis",                  15, "Chirurgie d’emblée",               0,
  "Sortie puis réadmis",                  15, "1ère ligne puis chirurgie",        6,
  "Sortie puis réadmis",                  15, "2e ligne puis chirurgie",         50,
  "Sortie puis réadmis",                  15, "≥ 3 lignes puis chirurgie",       45,
  "Opérés dans la même hospitalisation",  22, "Chirurgie d’emblée",              24,
  "Opérés dans la même hospitalisation",  22, "1ère ligne puis chirurgie",       33,
  "Opérés dans la même hospitalisation",  22, "2e ligne puis chirurgie",         50,
  "Opérés dans la même hospitalisation",  22, "≥ 3 lignes puis chirurgie",        5
) %>%
  mutate(
    parcours = factor(parcours,
                      levels = c("Chirurgie d’emblée","1ère ligne puis chirurgie",
                                 "2e ligne puis chirurgie","≥ 3 lignes puis chirurgie")
    ),
    groupe_lab = paste0(groupe, " (N=", N, ")"),
    n_approx = round(N * pct / 100)
  )

# Raccourcis utiles
colA <- unique(df$groupe_lab[df$groupe == "Sortie puis réadmis"])
colB <- unique(df$groupe_lab[df$groupe == "Opérés dans la même hospitalisation"])

# ==============================
# 4) PYRAMIDE (barres dos-à-dos)
# ==============================
df_bfly <- df %>%
  mutate(val = if_else(groupe == "Sortie puis réadmis", -pct, pct))

p4 <- ggplot(df_bfly, aes(x = val, y = fct_rev(parcours), fill = groupe_lab)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_x_continuous(
    labels = function(x) paste0(abs(x), "%"),
    breaks = pretty
  ) +
  labs(
    title = "Parcours avant chirurgie — pyramide par groupe",
    x = "% des patients (valeurs négatives = Sortie puis réadmis)", y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# ==========================
# 5) SLOPEGRAPH (2 colonnes)
# ==========================
df_slope <- df %>%
  select(groupe_lab, parcours, pct) %>%
  mutate(groupe_lab = factor(groupe_lab, levels = c(colA, colB)))

p5 <- ggplot(df_slope, aes(x = groupe_lab, y = pct, group = parcours, color = parcours)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(data = ~ filter(.x, groupe_lab == colA),
            aes(label = paste0(parcours, "  ", pct, "%")),
            hjust = 1.05, size = 3.2) +
  geom_text(data = ~ filter(.x, groupe_lab == colB),
            aes(label = paste0(pct, "%  ", parcours)),
            hjust = -0.05, size = 3.2) +
  scale_x_discrete(expand = expansion(mult = c(.15,.15))) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0,100)) +
  labs(
    title = "Parcours avant chirurgie — slopegraph par parcours",
    x = NULL, y = "% des patients", color = "Parcours"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# ==========================
# 6) HEATMAP (lecture flash)
# ==========================
p6 <- ggplot(df, aes(x = groupe_lab, y = parcours, fill = pct)) +
  geom_tile(width = .9, height = .9) +
  geom_text(aes(label = paste0(pct, "%\n(n≈", n_approx, ")")), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Parcours avant chirurgie — heatmap (%) et n≈",
    x = NULL, y = NULL, fill = "% patients"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# ==========================================
# 7) DELTA PLOT (différences entre groupes)
# ==========================================
df_wide <- df %>%
  select(groupe_lab, parcours, pct) %>%
  distinct() %>%
  pivot_wider(names_from = groupe_lab, values_from = pct) %>%
  mutate(delta = .data[[colB]] - .data[[colA]]) %>%
  arrange(desc(abs(delta)))

p7 <- ggplot(df_wide, aes(x = reorder(parcours, abs(delta)), y = delta)) +
  geom_hline(xintercept = NA) +  # (placeholder pour thème)
  geom_segment(aes(xend = parcours, y = 0, yend = delta), linewidth = 1.2, alpha = .5) +
  geom_point(size = 3) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  labs(
    title = "Différences absolues (opérés même hosp. − sortie puis réadmis)",
    x = NULL, y = "Δ en points de pourcentage"
  ) +
  theme_minimal(base_size = 12)

# ---- Affichage ----
p4; p5; p6; p7
