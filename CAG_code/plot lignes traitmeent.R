# --- Patch : reconstruire le tableau agrégé à partir des nombres -------

df_agg <- tibble::tribble(
  ~Ligne, ~Therapie,       ~n,
  "1",    "Chirurgie",      5,
  "1",    "Corticoïdes",   31,
  "1",    "AntiTNF",        1,
  "1",    "Ciclosporine",   0,
  "1",    "Védolizumab",    0,
  "1",    "Tofacitinib",    0,
  "1",    "Ustékinumab",    0,
  
  "2",    "Chirurgie",      8,
  "2",    "AntiTNF",       19,
  "2",    "Ciclosporine",   1,
  "2",    "Védolizumab",    2,
  "2",    "Tofacitinib",    1,
  "2",    "Ustékinumab",    1,
  "2",    "Corticoïdes",    0,
  
  "3",    "Chirurgie",     16,
  "3",    "AntiTNF",        2,
  "3",    "Ciclosporine",   4,
  "3",    "Védolizumab",    2,
  "3",    "Tofacitinib",    0,
  "3",    "Ustékinumab",    0,
  "3",    "Corticoïdes",    0,
  
  "4",    "Chirurgie",      5,
  "4",    "Ciclosporine",   1,
  "4",    "Védolizumab",    2,
  "4",    "AntiTNF",        0,
  "4",    "Tofacitinib",    0,
  "4",    "Ustékinumab",    0,
  "4",    "Corticoïdes",    0,
  
  "5",    "Chirurgie",      3,
  "5",    "AntiTNF",        0,
  "5",    "Ciclosporine",   0,
  "5",    "Védolizumab",    0,
  "5",    "Tofacitinib",    0,
  "5",    "Ustékinumab",    0,
  "5",    "Corticoïdes",    0
)

# -> Construit 'dat' avec colonnes attendues par la suite
dat <- prep_counts(df_agg, line_col = "Ligne", therapy_col = "Therapie", count_col = "n")

# Palette qui dépend de 'dat' (comme dans script)
therapies <- levels(dat$therapy)
pal <- setNames(
  rep(c("#2563EB","#10B981","#F59E0B","#EF4444","#8B5CF6","#14B8A6","#F43F5E","#0EA5E9"),
      length.out = length(therapies)),
  therapies
)
# ---- Packages ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggrepel)
  library(ggalluvial)   # alluvial / sankey-like
  library(treemapify)   # treemap
  library(scales)
  library(forcats)
})

# ---- Données -------
# df <- readr::read_csv("tableau traitement.csv")  # si besoin
line_col    <- "Ligne"      # ex: 1,2,3,4,5 (ou "L1","L2"...)
therapy_col <- "Therapie"   # ex: "Corticoides", "AntiTNF", "Vedo", ...
count_col   <- "n"          # mettre NULL si pas de colonne de comptage (format long)

# ---- Helper : mise en forme & pourcentages par ligne ----
prep_counts <- function(df, line_col, therapy_col, count_col = NULL) {
  if (is.null(count_col) || !count_col %in% names(df)) {
    dat <- df %>% count(.data[[line_col]], .data[[therapy_col]], name = "n")
  } else {
    dat <- df %>%
      group_by(.data[[line_col]], .data[[therapy_col]]) %>%
      summarise(n = sum(.data[[count_col]], na.rm = TRUE), .groups = "drop")
  }
  dat %>%
    group_by(.data[[line_col]]) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    group_by(.data[[therapy_col]]) %>%
    mutate(pct_overall = n / sum(n)) %>%
    ungroup() %>%
    mutate(
      line    = factor(.data[[line_col]], levels = sort(unique(.data[[line_col]]))),
      therapy = fct_reorder(.data[[therapy_col]], pct_overall, .desc = TRUE)
    ) %>%
    select(line, therapy, n, pct)
}


# Palette (modifie librement les couleurs si tu veux)
therapies <- levels(dat$therapy)
pal <- setNames(
  rep(c("#2563EB","#10B981","#F59E0B","#EF4444","#8B5CF6","#14B8A6","#F43F5E","#0EA5E9"), length.out = length(therapies)),
  therapies
)

theme_clean <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face="bold"),
        axis.title = element_text(face="bold"),
        panel.grid.minor = element_blank())

# =========================================================
# 1) Barres 100% empilées (répartition par ligne, lisible & classique)
# =========================================================
p_stack <- ggplot(dat, aes(x = line, y = n, fill = therapy)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = pal) +
  labs(title = "Répartition des traitements par ligne (100%)",
       x = "Ligne thérapeutique", y = "Proportion", fill = "Traitement") +
  theme_clean
p_stack

# Option : étiquettes en % dans les barres
p_stack_lbl <- ggplot(dat, aes(x = line, y = pct, fill = therapy)) +
  geom_col() +
  geom_text(aes(label = ifelse(pct >= 0.06, percent(pct,1), "")),
            position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = pal) +
  labs(title = "Poids de chaque traitement par ligne (avec étiquettes)",
       x = "Ligne thérapeutique", y = "Proportion", fill = "Traitement") +
  theme_clean
p_stack_lbl

# =========================================================
# 2) Alluvial (évolution visuelle des parts par traitement à travers les lignes)
# =========================================================
# Montre comment chaque thérapie “coule” d’une ligne à l’autre
p_alluvial <- ggplot(dat,
                     aes(x = line, stratum = therapy, alluvium = therapy,
                         y = n, fill = therapy, label = therapy)) +
  geom_flow(alpha = 0.5) +
  geom_stratum(color = "grey20") +
  scale_fill_manual(values = pal) +
  labs(title = "Variation des parts de chaque traitement selon la ligne",
       x = "Ligne thérapeutique", y = "Effectif (n)") +
  theme_clean
p_alluvial


# Option : étiquettes en % dans les strates

p_alluvial_pct <- ggplot(dat,
                         aes(x = line, stratum = therapy, alluvium = therapy,
                             y = n, fill = therapy)) +
  geom_flow(alpha = 0.5) +
  geom_stratum(color = "grey20") +
  geom_text(stat = "stratum",
            aes(label = scales::percent(pct, 1)),
            size = 4, color = "white", fontface = "bold") +
  scale_fill_manual(values = pal) +
  labs(title = "Variation des parts de chaque traitement selon la ligne",
       x = "Ligne thérapeutique", y = "Effectif (n)") +
  theme_clean

p_alluvial_pct

#Rajouter N= en haut
p_alluvial_n <- p_alluvial_pct +
  geom_text(data = dat %>% group_by(line) %>% summarise(n = sum(n)),
            aes(x = line, y = n + max(dat$n)*0.05, label = paste0("N=", n)),
            inherit.aes = FALSE, size = 4, fontface = "bold", color = "black")

p_alluvial_n

#export
ggsave("alluvial_therapies_by_line.png", p_alluvial_n, width = 12, height = 7, dpi = 1000)







# =========================================================
# 3) Heatmap (tableau % : lignes x traitements)
# =========================================================
p_heat <- ggplot(dat, aes(x = line, y = therapy, fill = pct)) +
  geom_tile() +
  geom_text(aes(label = percent(pct, 1)), fontface = "bold") +
  scale_fill_gradient(low = "#F1F5F9", high = "#2563EB",
                      labels = percent_format(accuracy = 1), name = "Proportion") +
  labs(title = "Heatmap des pourcentages par ligne et par traitement",
       x = "Ligne thérapeutique", y = "Traitement") +
  theme_clean
p_heat

# =========================================================
# 4) Slopegraph (variation intra-thérapie d’une ligne à l’autre)
# =========================================================
last_line <- tail(levels(dat$line), 1)
first_line <- head(levels(dat$line), 1)

dat_endpoints <- dat %>%
  filter(line %in% c(first_line, last_line)) %>%
  group_by(therapy) %>%
  mutate(lbl = if_else(line == last_line,
                       paste0(therapy, "  ", percent(pct,1)),
                       percent(pct,1))) %>%
  ungroup()

p_slope <- ggplot(dat, aes(x = line, y = pct, group = therapy, color = therapy)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = pal, guide = "none") +
  ggrepel::geom_text_repel(data = dat_endpoints,
                           aes(label = lbl),
                           nudge_x = ifelse(dat_endpoints$line == last_line, 0.15, -0.15),
                           size = 3.4, segment.color = "grey70", show.legend = FALSE) +
  labs(title = "Slopegraph : évolution de la part de chaque traitement",
       x = "Ligne thérapeutique", y = "Proportion") +
  theme_clean
p_slope

# =========================================================
# 5) Treemap facetté (un “camembert rectangulaire” par ligne)
# =========================================================
p_tree <- ggplot(dat, aes(area = n, fill = therapy,
                          label = paste0(therapy, "\n", percent(pct,1)))) +
  geom_treemap() +
  geom_treemap_text(fontface = "bold", place = "center", grow = TRUE, reflow = TRUE) +
  scale_fill_manual(values = pal) +
  facet_wrap(~ line) +
  labs(title = "Treemap : poids des traitements par ligne",
       fill = "Traitement") +
  theme_clean
p_tree

# Totaux par ligne
totaux <- dat %>%
  dplyr::group_by(line) %>%
  dplyr::summarise(N = sum(n), .groups = "drop")

# Totaux par ligne
totaux <- dat %>%
  dplyr::group_by(line) %>%
  dplyr::summarise(N = sum(n), .groups = "drop")

# Fusion avec dat pour avoir un label de facet enrichi
dat_withN <- dat %>%
  left_join(totaux, by = "line") %>%
  mutate(facet_lab = paste0("Ligne ", line, " (N = ", N, ")"))

# Treemap avec N dans le titre des facettes
p_tree <- ggplot(dat_withN, aes(area = n, fill = therapy,
                                label = paste0(therapy, "\n", percent(pct,1)))) +
  geom_treemap() +
  geom_treemap_text(fontface = "bold", place = "center", grow = TRUE, reflow = TRUE) +
  scale_fill_manual(values = pal) +
  facet_wrap(~ facet_lab) +
  labs(title = "Treemap : poids des traitements par ligne",
       fill = "Traitement") +
  theme_clean

p_tree

#export
ggsave("treemap.png", p_tree, width = 10, height = 5.5, dpi = 1000)






# =========================================================
# (Bonus) Delta L1 vs dernière ligne par traitement (qui change le plus ?)
# =========================================================
delta <- dat %>%
  filter(line %in% c(first_line, last_line)) %>%
  select(line, therapy, pct) %>%
  pivot_wider(names_from = line, values_from = pct, values_fill = 0) %>%
  mutate(delta = .data[[last_line]] - .data[[first_line]]) %>%
  arrange(desc(abs(delta)))

p_delta <- ggplot(delta, aes(x = reorder(therapy, delta), y = delta, fill = delta > 0)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#EF4444","#10B981"), guide = "none") +
  labs(title = paste0("Variation de part : ", first_line, " → ", last_line),
       x = "Traitement", y = "Δ proportion (points)") +
  theme_clean
p_delta

