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

