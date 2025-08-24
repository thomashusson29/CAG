library(gtsummary)
library(huxtable)
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)

# Fonction unique pour exporter un tableau gtsummary en .xlsx avec deux feuilles
export_tblsummary <- function(tbl, path = "tbl_summary.xlsx") {
  
  # Créer le classeur Excel
  wb <- openxlsx::createWorkbook()
  
  # Feuille "Table" : version mise en forme
  ht <- gtsummary::as_hux_table(tbl)
  huxtable::as_Workbook(ht, wb, sheet = "Table")
  
  # ---
  
  # Feuille "Raw" : version brute des données
  
  # Conversion du tbl_summary en data.frame plat
  df_raw <- as_tibble(tbl, col_labels = FALSE)
  df_lab <- as_tibble(tbl, col_labels = TRUE)
  
  # Identification des colonnes de statistiques et de p-value
  stat_cols <- grep("^stat_", names(df_raw), value = TRUE)
  p_col <- intersect(names(df_raw), "p.value")
  
  # Extraction des N totaux pour chaque groupe
  group_info <- names(df_lab)[-1]
  group_Ns <- stringr::str_match(group_info, "N\\s*=\\s*([0-9]+)")[, 2]
  group_Ns <- as.numeric(group_Ns)
  
  # Séparation des valeurs (n et %) et renommage
  df_separated <- df_raw %>%
    rename(Caractéristique = !!names(df_raw)[1]) %>%
    mutate(across(all_of(stat_cols), ~ str_extract(., "^[^\\s\\(]+"), .names = "{.col}_n")) %>%
    mutate(across(all_of(stat_cols), ~ str_extract(., "(?<=\\()[^)]*"), .names = "{.col}_pct")) %>%
    mutate(across(matches("_n$"), as.numeric))
  
  # Calcul des pourcentages par rapport au total de chaque groupe
  for (i in seq_along(stat_cols)) {
    n_col_name <- paste0(stat_cols[i], "_n")
    pct_total_col_name <- paste0(stat_cols[i], "_pct_col")
    
    if (n_col_name %in% names(df_separated) && !is.na(group_Ns[i])) {
      df_separated <- df_separated %>%
        mutate(
          !!pct_total_col_name := paste0(round((!!sym(n_col_name) / group_Ns[i]) * 100, 1), "%")
        )
      df_separated[[pct_total_col_name]][is.na(df_separated[[n_col_name]])] <- NA_character_
    }
  }
  
  # Nettoyage et sélection des colonnes finales
  final_cols_order <- c("Caractéristique",
                        paste0(rep(stat_cols, each = 3), c("_n", "_pct", "_pct_col")),
                        p_col)
  
  df_final <- df_separated %>%
    select(any_of(final_cols_order)) %>%
    mutate(across(everything(), as.character))
  
  # Création de la ligne d'en-tête
  header_row <- as.list(rep(NA, ncol(df_final)))
  names(header_row) <- names(df_final)
  header_row[[1]] <- "Caractéristique"
  
  # Remplissage de l'en-tête avec les labels de groupes et les N
  group_info <- names(df_lab)[-1] 
  n_cols <- grep("_n$", names(df_final), value = TRUE)
  pct_cols <- grep("_pct$", names(df_final), value = TRUE)
  pct_col_cols <- grep("_pct_col$", names(df_final), value = TRUE)
  
  for (i in seq_along(n_cols)) {
    group_label <- str_trim(str_extract(group_info[i], "^[^,]+"))
    group_N <- str_extract(group_info[i], "N\\s*=\\s*\\d+")
    
    header_row[[n_cols[i]]] <- paste0(group_label, " ", group_N)
    header_row[[pct_cols[i]]] <- "% par groupe" 
    
    if (length(pct_col_cols) > 0) {
      header_row[[pct_col_cols[i]]] <- "% par colonne"
    }
  }
  
  if (length(p_col) > 0) {
    header_row[[p_col]] <- "p-value"
  }
  
  # Ajout de la ligne d'en-tête au tableau final
  df_with_header <- bind_rows(
    tibble::as_tibble_row(header_row),
    df_final
  )
  
  # ---
  
  # Écriture de la feuille "Raw" dans le classeur
  openxlsx::addWorksheet(wb, sheetName = "Raw")
  openxlsx::writeData(wb, sheet = "Raw", x = df_with_header,
                      headerStyle = openxlsx::createStyle(textDecoration = "bold"))
  openxlsx::setColWidths(wb, sheet = "Raw", cols = 1:ncol(df_with_header), widths = "auto")
  
  # Sauvegarde du fichier
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  message("✅ Exporté: ", path)
}
# Exemple d'utilisation (décommenter pour tester)
# Supposons que 'tableau3' est un objet tbl_summary déjà créé.
export_tblsummary(tableau3, "tableau3.xlsx")