library(tidyr)
library(dplyr)
source("variables.R")

parti_non_confi_file <- file.path(data, "observations", "parti_unique_non_confi.csv")
p_export_file <- file.path(data, "p_export.csv")

parti_non_confi <- read.csv2(parti_non_confi_file)
p_export <- read.csv2(p_export_file)

compl <- p_export %>% select(c(commentaires, localite))
