
library(tidyverse)

# Il faut adapter ici pour recevoir les arguments de la commande slurm
HeadDir = "/home/charlotte/Bureau/SDM/"
ModelDir = "/home/charlotte/Bureau/SDM/IDF_acticlass_k5/seasons/evaluations"
DateModel="2026-06-27" #date of prediction (exactly same writing as the folder name)

# Load evaluation tables
List_tables <- fs::dir_ls(ModelDir, regexp="Evaluation_")
Concatenation <- read_csv(List_tables, id="path") %>% 
  filter(grepl(DateModel, path)) %>% 
  mutate(Species = gsub(".*\\_(.*)\\..*", "\\1", path)) %>% 
  select(-"...1", -impfeat) %>% 
  as.data.frame()

# Write
FileName = gsub(HeadDir, "", ModelDir)
FileName = gsub("/", "_", FileName)
write_csv(Concatenation, paste0(HeadDir, paste0(FileName, ".csv")))







