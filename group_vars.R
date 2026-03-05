# library(rlist)

folder <- commandArgs(trailingOnly = TRUE)[1L]

mode <- strsplit(basename(folder), "_", fixed = TRUE)[[1L]][1L]
region <- commandArgs(trailingOnly = TRUE)[2L]

fichiers <- list.files(folder,
  full.names = TRUE, pattern = region
)

fichiers

dataframes <- lapply(fichiers, read.csv)

for (i in seq_along(fichiers)) {
  if (identical(
    colnames(dataframes[[i]]),
    c("X", "Y", "Nuit", "fortnight", "fortnight_year", "code")
  )) {
    base <- dataframes[[i]]
    dataframes <- dataframes[-i]
  }
}

base <- unique(base)
head(base)

for (df in dataframes) {
  if ("Nuit" %in% colnames(df)) {
    df <- unique(df)
    # df$Nuit <- "2024-06-08"
    by <- c("X", "Y", "Nuit")
    base <- dplyr::left_join(base, df, by = by)
  } else {
    df <- unique(df)
    by <- c("X", "Y")
    base <- dplyr::left_join(base, df, by = by)
  }
}

head(base)
base <- base |>
  dplyr::select(-geometry)

if (mode == "obs") {
  mode_name <- "train"
} else {
  mode_name <- "pred"
}

file <- file.path(folder, paste0("data_", mode_name, "_", region, ".csv"))
write.csv(base, file)

# colnames(dataframes[[1]])
# nrow(unique(base))
# base <- base |>
#   dplyr::left_join(dataframes[[1]], by = c("X", "Y", "Nuit"))
#
# write.csv(base, "/home/tsevere/Documents/mnhn/projet_git_BMRE/data/observations/donnees_vigie_chiro/GI_france_met_sites_localites.csv", row.names = FALSE)
#
#
#
# fichiers_names <- list.files(pred_vars_folder, include.dirs = FALSE)
# fichiers_names <- fichiers_names[grep("500", fichiers_names)]
# fichiers_names
#
# mot <- strsplit(fichiers_names, "_")
#
# mots <- lapply(seq_along(mot), function(i){ mot[[i]][1:3]})
# mots
#
# mots_ <- lapply(mots, paste0, collapse = "_")
# mots_
# unique_mots <- unique(mots_)
# unique_mots
#
# tables <- list()
# for (nom in unique_mots){
#   print(nom)
#   basename <- paste0(nom, "_cote_france_met.csv")
#   fichiers  <- list.files(pred_vars_folder, pattern = nom, full.names = TRUE)
#   fichiers <- fichiers[fichiers != file.path(pred_vars_folder, basename)]
#   print("files filtered")
#   dataframes <- lapply(fichiers, read.csv)
#   print("files_read")
#   base <- read.csv(file.path(pred_vars_folder, basename))
#   base <- unique(base)
#   print(head(base))
#   base$Nuit <- "2024-06-08"
#   for (df in dataframes) {
#     if ("Nuit" %in% colnames(df)) {
#       print("Nuit")
#       df <- unique(df)
#       print(head(df))
#       by <- c("X", "Y", "Nuit")
#       base <- dplyr::left_join(base, df, by = by)
#     } else {
#       df <- unique(df)
#       print(head(df))
#       by <- c("X", "Y")
#       base <- dplyr::left_join(base, df, by = by)
#     }
#   }
#   tables <- list.append(tables, base)
# }
#
# head(tables)
#
# list_colnames <- list()
# for (table in tables){
#   list_colnames <- append(list_colnames, colnames(table))
# }
#
#
# colonnes <- unlist(unique(list_colnames))
# tables_full <- list()
#
# for (table in tables) {
#   for (colonne in colonnes){
#     if (!(colonne %in% colnames(table))) {
#       print(colonne)
#       table[colonne] <- 0
#
#     }
#   }
#   tables_full <- list.append(tables_full, table)
# }
#
#
# colnames(tables[[13]])
# tablesr <- do.call("rbind", tables_full)
# head(tablesr)
# tablesr$SpRo_dist <- 0
# write.csv(tablesr,"/home/tsevere/Documents/mnhn/projet_git_BMRE/data/observations/pred_vars/data_pred_france_met.csv", row.names = FALSE )
# pref <- lapply(mot[
#
# dataframes <- lapply(fichiers, read.csv)
#
