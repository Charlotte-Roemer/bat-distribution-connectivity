
library(tidyverse)


# Load tables
List_tables = list.files("/home/charlotte/Bureau/SDM/IDF_acticlass_k5", pattern = "evaluations.csv", full.names = T)
ld <- lapply(List_tables, function(x) read_csv(x))
Concat = Concatenation <- do.call("bind_rows", ld)
Concat$Model = ifelse(grepl("season", Concat$path), "season", "year")
Concat$Season = ifelse(grepl("summer", Concat$path), "summer", "spring")
Concat$Season = ifelse(grepl("autumn", Concat$path), "autumn", Concat$Season)
Concat$Season = ifelse(Concat$Model == "year", "year", Concat$Season)

# Plot
Concat = Concat %>% 
  rename(Accuracy = kNNDM_accuracy_ecography,
         Discrimination = kNNDM_discrimination,
         Precision = kNNDM_precision)

# Plot accuracy
Concat %>% 
  ggplot(aes(Model, Accuracy, col = Season)) +
  geom_point(aes(size = 2)) +
  facet_wrap(vars(Species)) +
  ylim(0,4) +
  theme_bw(base_size=20)

# Plot discrimination
Concat %>% 
  ggplot(aes(Model, Discrimination, col = Season)) +
  geom_point(aes(size = 2)) +
  facet_wrap(vars(Species)) +
  ylim(0,1) +
  theme_bw(base_size=20)

# Plot precision
Concat %>% 
  ggplot(aes(Model, Precision, col = Season)) +
  geom_point(aes(size = 2)) +
  facet_wrap(vars(Species)) +
  theme_bw(base_size=20)


