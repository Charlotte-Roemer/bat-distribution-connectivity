library(data.table)
Minsch_file <- "/home/tsevere/Documents/mnhn/projet_git_BMRE/data/ModPred/VC50_2025-05-07/Minsch_datasp.txt"

loc_file <- "/home/tsevere/Documents/mnhn/projet_git_BMRE/data/observations/donnees_vigie_chiro/GI_FR_sites_localites.csv"

Minsch <- read.table(Minsch_file)
locs <- read.table(loc_file)

by <- c("Nuit", "participation", "num_micro")

merged <- Minsch[locs, by]
