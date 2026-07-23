
library(tidyverse)
library(viridis)

# Load tables
Name = "france_met_2026-07-17_Tri90"
Concatenation = read_csv(paste0("/home/charlotte/Bureau/SDM/France/Evaluations/",  Name, ".csv"))
# List_tables = list.files("/home/charlotte/Bureau/SDM/France/Evaluations", pattern = "evaluations.csv", full.names = T)
# ld <- lapply(List_tables, function(x) read_csv(x))
# Concat = Concatenation <- do.call("bind_rows", ld)


# # Keep year?
# Concat = Concat %>% 
#   filter(Model == "season")

# Handle columns
# Concat = Concatenation %>% 
#   rename(Accuracy = kNNDM_accuracy_Waldock,
#          Discrimination_all = kNNDM_discrimination,
#          Discrimination_positive = kNNDM_discrimination_presence,
#          Precision = kNNDM_precision,
#          AUC_presence_absence = kNNDM_auc_pa,
#          Bias = kNNDM_bias)
Concat = Concatenation %>% 
  rename(Justesse = MAE,
         Justesse_relative = kNNDM_accuracy_Waldock,
         Discrimination_all = kNNDM_discrimination,
         Discrimination_positive = kNNDM_discrimination_presence,
         Precision = kNNDM_precision,
         AUC_presence_absence = kNNDM_auc_pa,
         Biais = kNNDM_bias)

Concat$Season = ifelse(grepl("summer", Concat$path), "été", "printemps")
Concat$Season = ifelse(grepl("autumn", Concat$path), "automne", Concat$Season)
Concat$Season = as.factor(Concat$Season )
Concat = Concat %>% 
  mutate(Season = fct_relevel(Season, "printemps", "été", "automne"))

# Plot accuracy relative
png(filename=(paste0("/home/charlotte/Bureau/SDM/Evaluation_Accuracy_relative_", Name, ".png")), height=1500, width=2000,res=150)
plot1 = Concat %>% 
  ggplot(aes(Season, Justesse_relative)) +
  geom_point(aes(fill = Justesse_relative), colour="black", shape=21, size = 8) +
  facet_wrap(vars(Species), ncol=4) +
  scale_fill_gradient2(low = 'darkgreen', mid = 'white', high = 'red', midpoint = 1) +
  ylim(0,4) +
  theme_classic(base_size=20) +
  theme(panel.grid.minor = element_line(colour="grey")) +
  guides(size = "none")
print(plot1)
dev.off()

# Plot accuracy
png(filename=(paste0("/home/charlotte/Bureau/SDM/Evaluation_MAE_", Name, ".png")), height=1500, width=2000,res=150)
plot1 = Concat %>% 
  ggplot(aes(Season, Justesse_)) +
  geom_point(aes(fill = Justesse), colour="black", shape=21, size = 8) +
  facet_wrap(vars(Species), ncol=4) +
  scale_fill_gradient2(low = 'darkgreen', mid = 'white', high = 'red', midpoint = 1) +
  ylim(0,4) +
  theme_classic(base_size=20) +
  theme(panel.grid.minor = element_line(colour="grey")) +
  guides(size = "none")
print(plot1)
dev.off()

# Plot precision
png(filename=(paste0("/home/charlotte/Bureau/SDM/Evaluation_Precision_", Name, ".png")), height=1500, width=2000,res=150)
plot1 = Concat %>% 
  ggplot(aes(Season, Precision)) +
  geom_point(aes(fill = Precision), colour="black", shape=21, size = 8) +
  facet_wrap(vars(Species), ncol=4) +
  scale_fill_gradient(low = 'white', high = 'red') +
  theme_classic(base_size=20) +
  theme(panel.grid.minor = element_line(colour="grey")) +
  guides(size = "none")
print(plot1)
dev.off()

# Plot bias
png(filename=(paste0("/home/charlotte/Bureau/SDM/Evaluation_Bias_", Name, ".png")), height=1500, width=2000,res=150)
plot1 = Concat %>% 
  ggplot(aes(Season, Biais)) +
  geom_point(aes(fill = Biais), colour="black", shape=21, size = 8) +
  facet_wrap(vars(Species), ncol=4) +
  scale_fill_gradient2(low = 'darkgreen', mid = 'white', high = 'red', midpoint = 1) +
  ylim(0,4) +
  theme_classic(base_size=20) +
  theme(panel.grid.minor = element_line(colour="grey")) +
  guides(size = "none")
print(plot1)
dev.off()

# Plot discrimination
Concat %>% 
  ggplot(aes(Model, Discrimination_all, col = Season)) +
  geom_point(aes(size = 2)) +
  facet_wrap(vars(Species)) +
  ylim(0,1) +
  theme_bw(base_size=20)

# Plot AUC_presence_absence
Concat %>% 
  ggplot(aes(Model, AUC_presence_absence, col = Season)) +
  geom_point(aes(size = 2)) +
  facet_wrap(vars(Species)) +
  ylim(0,1) +
  theme_bw(base_size=20)

# Plot Discrimination_positive
png(filename=(paste0("/home/charlotte/Bureau/SDM/Evaluation_Discrimination_positive_", Name, ".png")), height=1500, width=2000,res=150)
plot1 = Concat %>% 
  ggplot(aes(Season, Discrimination_positive)) +
  geom_point(aes(fill = Discrimination_positive), colour="black", shape=21, size = 8) +
  facet_wrap(vars(Species), ncol=4) +
  scale_fill_gradient2("Discrimination\n si présence", low = 'red', mid = 'white', high = 'blue', midpoint = 0.5) +
  ylim(0,1) +
  theme_classic(base_size=20) +
  theme(panel.grid.minor = element_line(colour="grey")) +
  guides(size = "none") +
  labs(y = "Discrimination si présence") 
print(plot1)
dev.off()



