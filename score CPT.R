# =====================================================================
# Recalcul SCORE 1 à partir de 'df' uniquement (sans ever_sortie_med)
# CPT := 1 si delai_sup_30 == 0 ; sinon 0
# Items du score 1 : BIO + (>=2 lignes MED) + CRP>150 + Albumine<25 + RCH
# Sortie : % CPT par total (0..5) + IC95% + mapping logistique
# =====================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(stringr) })

# -- Helpers (si absents) --
if (!exists("to_01")) {
  to_01 <- function(x){
    if (is.character(x)) x <- trimws(x)
    if (is.logical(x))   x <- as.integer(x)
    if (is.factor(x))    x <- as.character(x)
    x <- suppressWarnings(as.numeric(x))
    x[is.na(x)] <- 0; x[x != 0] <- 1; x
  }
}
if (!exists("cp_ci")) {
  cp_ci <- function(events, n, conf.level = 0.95){
    bt <- binom.test(events, n, conf.level = conf.level)
    c(low=bt$conf.int[1], high=bt$conf.int[2])
  }
}

stopifnot(exists("df"))

df_work <- df

# ---------- 1) CPT depuis delai_sup_30 (ta règle) ----------
if (!"delai_sup_30" %in% names(df_work)) stop("Il manque 'delai_sup_30' dans df.")
delai_num <- suppressWarnings(as.numeric(as.character(df_work$delai_sup_30)))
df_work$CPT_bin <- ifelse(!is.na(delai_num) & delai_num == 0, 1, 0)

# ---------- 2) BIO (biothérapies) : détecter colonnes pertinentes (robuste listes) ----------
# ---------- 2) BIO (biothérapies) : détecter colonnes pertinentes (robuste listes) ----------
bio_patterns <- "(bio|bioth|anti\\s*tnf|antitnf|adalim|inflix|golim|certol|vedoliz|vedolizumab|usteki|ustekinumab|tofa|stelara)"

bio_cols <- grep(bio_patterns, names(df_work), ignore.case = TRUE, value = TRUE)

# util: aplatir list-columns -> character
flatten_col <- function(v) {
  if (is.list(v)) {
    return(vapply(v, function(el) {
      if (length(el) == 0 || all(is.na(el))) "" else paste(as.character(el), collapse = " ")
    }, character(1)))
  }
  v
}

BIO <- rep(0L, nrow(df_work))  # important: partir de 0, pas NA

if (length(bio_cols) > 0) {
  tmp <- df_work[, bio_cols, drop = FALSE]
  
  for (j in seq_along(bio_cols)) {
    v <- tmp[[j]]
    v <- flatten_col(v)
    
    if (is.character(v) || is.factor(v)) {
      v <- as.character(v)
      BIO <- pmax(BIO, as.integer(grepl(bio_patterns, v, ignore.case = TRUE)), na.rm = TRUE)
    } else if (is.logical(v)) {
      BIO <- pmax(BIO, as.integer(v), na.rm = TRUE)
    } else {
      # numeric / codé 0-1 / ou texte numérique
      BIO <- pmax(BIO, to_01(v), na.rm = TRUE)
    }
  }
  
} else {
  message("⚠ Aucune colonne biothérapie/antiTNF détectée dans df : BIO restera à 0 (si c’est faux, ajuste 'bio_patterns').")
}

# (optionnel) log de contrôle
cat("\n[DEBUG] Colonnes BIO détectées :", if (length(bio_cols)) paste(bio_cols, collapse = ", ") else "aucune", "\n")





# ---------- 3) Lignes MED (exclure chirurgie) ----------
line_cols <- grep("(^|_)\\d(st|nd|rd|th)_lign_ttt$", names(df_work), ignore.case=TRUE, value=TRUE)
if (length(line_cols) == 0) line_cols <- grep("lign_ttt", names(df_work), ignore.case=TRUE, value=TRUE)

surgery_patterns <- "(chir|colect|stomie|stoma|ileo|ilé|anastom|lapar|bloc|oper|opér|surgery)"
is_medical_line <- function(v){
  v <- tolower(trimws(as.character(v)))
  ifelse(is.na(v) | v=="", FALSE, !grepl(surgery_patterns, v))
}

if (length(line_cols) > 0) {
  nb_lignes_medicales <- apply(df_work[, line_cols, drop=FALSE], 1, function(row) {
    sum(vapply(as.list(row), is_medical_line, logical(1)))
  })
} else if ("n_lignes_tentees" %in% names(df_work)) {
  message("⚠ Pas de colonnes ‘xth_lign_ttt’. Utilisation de n_lignes_tentees (vérifie qu’elle EXCLUT la chirurgie).")
  nb_lignes_medicales <- suppressWarnings(as.numeric(df_work$n_lignes_tentees))
} else {
  stop("Impossible de compter les lignes : ni ‘xth_lign_ttt’ ni ‘n_lignes_tentees’ dans df.")
}
LIGNES2 <- as.integer(nb_lignes_medicales >= 2)

# ---------- 4) CRP et Albumine (chercher colonnes si noms différents) ----------
pick_numeric <- function(df, pattern_primary, pattern_backup=NULL){
  cands <- grep(pattern_primary, names(df), ignore.case=TRUE, value=TRUE)
  if (length(cands)==0 && !is.null(pattern_backup)) {
    cands <- grep(pattern_backup, names(df), ignore.case=TRUE, value=TRUE)
  }
  # garder numériques
  cands <- cands[sapply(cands, function(v) is.numeric(df[[v]]) || all(grepl("^\\s*-?\\d+(\\.\\d+)?\\s*$", as.character(df[[v]]), perl=TRUE) | is.na(df[[v]])))]
  if (length(cands)==0) return(NULL)
  cands[1]
}

crp_col <- if ("CRP_admission" %in% names(df_work)) "CRP_admission" else pick_numeric(df_work, "CRP.*admi", "CRP")
alb_col <- if ("AlbuminLevel_admission" %in% names(df_work)) "AlbuminLevel_admission" else pick_numeric(df_work, "albu|albumin", "album")

if (is.null(crp_col)) stop("Colonne CRP (numérique) introuvable (ex. ‘CRP_admission’).")
if (is.null(alb_col)) stop("Colonne Albumine (numérique) introuvable (ex. ‘AlbuminLevel_admission’).")

CRP150 <- as.integer(suppressWarnings(as.numeric(df_work[[crp_col]])) > 150)
ALB25  <- as.integer(suppressWarnings(as.numeric(df_work[[alb_col]])) < 25)

# ---------- 5) RCH (si pas binaire déjà, tenter à partir de Crohn/RCH) ----------
if ("RCH" %in% names(df_work)) {
  RCH_bin <- to_01(df_work$RCH)
} else if ("Crohn_RCH" %in% names(df_work)) {
  v <- tolower(as.character(df_work$Crohn_RCH))
  RCH_bin <- as.integer(grepl("rch|colite|ulc(é|e)reuse|uc", v))
} else {
  stop("Colonne RCH introuvable (ni ‘RCH’ ni ‘Crohn_RCH’).")
}

# ---------- 6) Construire SCORE1 et afficher ce qui a été utilisé ----------
df_work$BIO     <- BIO
df_work$LIGNES2 <- LIGNES2
df_work$CRP150  <- CRP150
df_work$ALB25   <- ALB25
df_work$RCH_bin <- RCH_bin

# Score avec NA -> on exige que les 5 items soient connus (retirer NA)
items <- c("BIO","LIGNES2","CRP150","ALB25","RCH_bin")
na_rows <- !stats::complete.cases(df_work[, items])
if (any(na_rows)) {
  message("ℹ ", sum(na_rows), " lignes ont des NA dans les items du score et seront exclues pour le calcul du % par score.")
}
df_score <- df_work[!na_rows, , drop=FALSE] %>%
  mutate(SCORE1 = BIO + LIGNES2 + CRP150 + ALB25 + RCH_bin)

cat("\n=== Items utilisés ===\n")
cat("BIO from cols: ", if(length(bio_cols)) paste(bio_cols, collapse=", ") else "aucune", "\n", sep="")
cat("LIGNES from cols: ", if(length(line_cols)) paste(line_cols, collapse=", ") else "n_lignes_tentees", "\n", sep="")
cat("CRP numeric col: ", crp_col, "\n", sep="")
cat("Albumine col: ", alb_col, "\n", sep="")
cat("RCH source: ", if("RCH" %in% names(df_work)) "RCH" else "Crohn_RCH (parsé)", "\n", sep="")

# ---------- 7) % CPT observé par total + IC exacts ----------
risk_by_score <- df_score %>%
  group_by(SCORE1) %>%
  summarise(
    n_total = n(),
    n_CPT   = sum(CPT_bin, na.rm=TRUE),
    risk    = n_CPT/n_total,
    .groups = "drop"
  ) %>%
  rowwise() %>% mutate(ci = list(cp_ci(n_CPT, n_total))) %>%
  mutate(ci_low = ci["low"], ci_high = ci["high"]) %>%
  ungroup() %>%
  transmute(
    SCORE1, n_total, n_CPT,
    `Risque CPT %` = round(100*risk, 1),
    `IC95% bas %`  = round(100*as.numeric(ci_low), 1),
    `IC95% haut %` = round(100*as.numeric(ci_high), 1)
  ) %>%
  arrange(SCORE1)

cat("\n=== % CPT OBSERVÉ PAR SCORE (IC 95% exacts) ===\n")
print(risk_by_score)

# ---------- 8) Mapping logistique points -> probabilité (optionnel) ----------
fit_pts <- glm(CPT_bin ~ SCORE1, data=df_score, family=binomial())
grid <- data.frame(SCORE1=0:5)
pred <- predict(fit_pts, newdata=grid, type="link", se.fit=TRUE)
grid$prob <- plogis(pred$fit); grid$low <- plogis(pred$fit-1.96*pred$se.fit); grid$high <- plogis(pred$fit+1.96*pred$se.fit)

cat("\n=== MAPPING (LOGISTIQUE) — Probabilité CPT par score (IC 95%) ===\n")
print( transform(grid,
                 `Prob. CPT % (modèle)`=round(100*prob,1),
                 `IC95% bas %`=round(100*low,1),
                 `IC95% haut %`=round(100*high,1)
)[,c("SCORE1","Prob. CPT % (modèle)","IC95% bas %","IC95% haut %")] )

# ---------- 9) (optionnel) Distribution brute ----------
cat("\n=== DISTRIBUTION BRUTE SCORE x CPT ===\n")
print(addmargins(table(Score=df_score$SCORE1, CPT=df_score$CPT_bin)))









suppressPackageStartupMessages({library(ggplot2)})

# Si besoin, (re)fabriquer risk_by_score à partir de df_score
if (!exists("risk_by_score")) {
  risk_by_score <- df_score |>
    dplyr::group_by(SCORE1) |>
    dplyr::summarise(n_total = dplyr::n(),
                     n_CPT = sum(CPT_bin, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::rowwise() |>
    dplyr::mutate(ci = list(cp_ci(n_CPT, n_total)),
                  ci_low = ci["low"], ci_high = ci["high"],
                  risk = n_CPT/n_total) |>
    dplyr::ungroup() |>
    dplyr::mutate(`Risque CPT %` = 100*risk,
                  `IC95% bas %`  = 100*as.numeric(ci_low),
                  `IC95% haut %` = 100*as.numeric(ci_high))
}

ggplot(risk_by_score,
       aes(x = SCORE1, y = `Risque CPT %`)) +
  geom_pointrange(aes(ymin = `IC95% bas %`, ymax = `IC95% haut %`), size=.8) +
  geom_line() +
  scale_x_continuous(breaks = 0:5) +
  labs(title = "% CPT observé par score",
       x = "Score (0–5)",
       y = "Risque CPT (%)",
       caption = "Barres = IC95% exacts (Clopper–Pearson).") +
  theme_minimal(base_size = 12)


# Grid issu de ton glm (SCORE1 -> prob), sinon on le (re)crée
if (!exists("grid")) {
  fit_pts <- glm(CPT_bin ~ SCORE1, data = df_score, family = binomial())
  grid <- data.frame(SCORE1 = 0:5)
  pred <- predict(fit_pts, newdata = grid, type = "link", se.fit = TRUE)
  grid$prob <- plogis(pred$fit)
  grid$low  <- plogis(pred$fit - 1.96*pred$se.fit)
  grid$high <- plogis(pred$fit + 1.96*pred$se.fit)
}

ggplot() +
  # Modèle lissé (ligne + ruban IC)
  geom_ribbon(data = grid, aes(x = SCORE1, ymin = 100*low, ymax = 100*high),
              alpha = .15) +
  geom_line(data = grid, aes(x = SCORE1, y = 100*prob), linewidth = 1) +
  # Points observés (avec IC)
  geom_pointrange(data = risk_by_score,
                  aes(x = SCORE1, y = `Risque CPT %`,
                      ymin = `IC95% bas %`, ymax = `IC95% haut %`),
                  color = "black", size=.7) +
  scale_x_continuous(breaks = 0:5) +
  labs(title = "Calibration: observé vs prédiction logistique",
       x = "Score (0–5)", y = "Probabilité / Risque (%)",
       caption = "Ligne/ruban = modèle logistique (IC95%); points/barres = observé (IC95%).") +
  theme_minimal(base_size = 12)


suppressPackageStartupMessages(library(pROC))

roc_obj <- roc(df_score$CPT_bin, df_score$SCORE1, quiet = TRUE)
auc_val <- as.numeric(auc(roc_obj))
ci_auc  <- ci.auc(roc_obj)

# Version ggplot (pROC::ggroc)
p <- ggroc(roc_obj, colour = "#1f77b4", size = 1.2) +
  annotate("text", x = 0.7, y = 0.2,
           label = sprintf("AUC = %.3f (IC95%% %.3f–%.3f)", auc_val, ci_auc[1], ci_auc[3]),
           hjust = 0, size = 4) +
  labs(title = "Courbe ROC du score",
       x = "1 - Spécificité", y = "Sensibilité") +
  theme_minimal(base_size = 12)
print(p)

#même courbe ROC un peu lissée et avec ligne diagonale passant par 0.5
p + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  coord_fixed()  # carrée











tab_counts <- as.data.frame(table(Score = df_score$SCORE1, CPT = df_score$CPT_bin))
tab_counts$CPT <- factor(tab_counts$CPT, levels = c(0,1), labels = c("Non-CPT","CPT"))

ggplot(tab_counts, aes(x = factor(Score), y = Freq, fill = CPT)) +
  geom_col(width = .7) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5), size = 3.5) +
  scale_fill_manual(values = c("#bdbdbd", "#2ca02c")) +
  labs(title = "Distribution brute (comptes) par score",
       x = "Score", y = "Nombre de patients", fill = "") +
  theme_minimal(base_size = 12)




# Tableau perfs pour seuils entiers
seuils <- 0:5
perf_df <- lapply(seuils, function(k){
  pred <- as.integer(df_score$SCORE1 >= k)
  tp <- sum(pred==1 & df_score$CPT_bin==1)
  tn <- sum(pred==0 & df_score$CPT_bin==0)
  fp <- sum(pred==1 & df_score$CPT_bin==0)
  fn <- sum(pred==0 & df_score$CPT_bin==1)
  data.frame(
    seuil = k,
    Sens = ifelse((tp+fn)>0, tp/(tp+fn), NA),
    Spec = ifelse((tn+fp)>0, tn/(tn+fp), NA),
    PPV  = ifelse((tp+fp)>0, tp/(tp+fp), NA),
    NPV  = ifelse((tn+fn)>0, tn/(tn+fn), NA)
  )
}) |> dplyr::bind_rows()

perf_long <- tidyr::pivot_longer(perf_df, cols = c(Sens, Spec, PPV, NPV),
                                 names_to = "metrique", values_to = "val")

ggplot(perf_long, aes(x = seuil, y = 100*val, color = metrique)) +
  geom_line() + geom_point(size = 2) +
  scale_x_continuous(breaks = 0:5) +
  labs(title = "Performance selon le seuil (Score ≥ k)",
       x = "Seuil k", y = "Valeur (%)", color = "") +
  theme_minimal(base_size = 12)



df_cat <- df_score |>
  dplyr::mutate(cat = dplyr::case_when(
    SCORE1 <= 2 ~ "Faible (0–2)",
    SCORE1 == 3 ~ "Intermédiaire (3)",
    SCORE1 >= 4 ~ "Élevé (4–5)"
  ))

tab_cat <- as.data.frame(table(Catégorie = df_cat$cat, CPT = df_cat$CPT_bin))
tab_cat$CPT <- factor(tab_cat$CPT, levels = c(0,1), labels = c("Non-CPT","CPT"))

ggplot(tab_cat, aes(x = Catégorie, y = Freq, fill = CPT)) +
  geom_col(width = .7, position = "fill") +
  scale_y_continuous(labels = function(z) paste0(100*z, "%")) +
  scale_fill_manual(values = c("#bdbdbd", "#2ca02c")) +
  labs(title = "Risque CPT par catégorie (proportions empilées)",
       x = "", y = "Proportion", fill = "") +
  theme_minimal(base_size = 12)

