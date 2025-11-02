#alluvial

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
