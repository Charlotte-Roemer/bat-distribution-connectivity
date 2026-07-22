
library(tidyverse)

# Il faut adapter ici pour recevoir les arguments de la commande slurm
ModelDir = "/home/charlotte/Bureau/SDM/France/Evaluations"
DateModel="2026-07-17" #date of prediction (exactly same writing as the folder name)
Region = "france_met"
Tri = 90

# Load evaluation tables
List_tables <- fs::dir_ls(ModelDir, regexp="Evaluation_")
Concatenation <- read_csv(List_tables, id="path") %>% 
  filter(grepl(DateModel, path)) %>% 
  mutate(Species = gsub(".*\\_(.*)\\..*", "\\1", path)) %>% 
  select(-"...1", -impfeat) %>% 
  as.data.frame()

# Write
FileName = paste0(Region, "_", DateModel, "_Tri", Tri)
write_csv(Concatenation, paste0(ModelDir, paste0("/", FileName, ".csv")))







