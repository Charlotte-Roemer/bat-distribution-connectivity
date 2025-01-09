# install.packages("grf")
# Documentation for grf can be found there : https://grf-labs.github.io/grf/
# help(grf)
library(grf)
library(dplyr)
source("variables.R")

region <- "france_met"
future::plan("multisession")

obs_folder <- file.path(data, "observations", "obs_vars")
obs <- file.path(obs_folder, "loc_train_france_met.csv")
observations <- read.csv(obs)
head(observations)
observations$annee <- as.integer(sapply(strsplit(observations$Nuit, "-"), "[", 1))

observations <- observations[between(observations$annee, 2013, 2024), ]


csvs <- list.files(obs_folder, full.names = TRUE)

tabs <- lapply(csvs, read.csv)


date_d <- function(dataframe) {
  if (all(c("X", "Y", "Nuit") %in% colnames(dataframe))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



cond <- unlist(lapply(tabs, date_d))

bidule <- tabs[cond]
bidulo <- tabs[!cond]

for (tab in bidulo) {
  ## tab <- tab[1:1000,]
  tab <- unique(tab)
  observations <<- observations %>% left_join(tab, by = c("X", "Y"))
}


head(bidule)
df_dates <- purrr::reduce(bidule, full_join, by = c("X", "Y", "Nuit"))
df_dates <- df_dates  %>%  select(-FID)
df_dates <- unique(df_dates)

observations <- 
  observations %>%
  left_join(df_dates, by = c("X", "Y", "Nuit"))

head(df_dates)
tool_data <- read.csv("/home/bbk9/Documents/asellia/Barba_2024/data/dependances_fixes/vars_obs_Barbar.csv")
X_pred_aout <- read.csv("/home/bbk9/Documents/asellia/Barba_2024/data/dependances_fixes/vars_predict_2023-08.csv")

contacts <- tool_data$nb_contacts

vars <- tool_data %>% dplyr::select(starts_with("Sp"))
vars_aout <- X_pred_aout %>% dplyr::select(starts_with("Sp"))
columns <- colnames(vars)
vars_aout <- vars_aout[, columns]

data <- cbind(contacts, vars)

W <- rbinom(nrow(X_train), 1, 0.5)

vars_train <- mtcars[0:15, ]
vars_test <- mtcars[16:32, ]

vars_test

causal_forest <- grf::causal_forest(X_train, Y_train, W,
  seed = 5,
  num.trees = 20, honesty.prune.leaves = F, honesty.fraction = 0.7
)


forest <- grf::regression_forest(vars, contacts,
  num.trees = 5000,
  honesty = F, seed = 19, ci.group.size = 2
)

pred <- predict(forest, vars_aout, estimate.variance = TRUE)

variable_importance(forest)
forest$tuning.output$grid

forest$tuning.output$error
forest$debiased.error

# predict on out of bag
pred_oob <- predict(forest)
summary(pred_oob)

write.csv(forest$tuning.output$grid, "grille_cv.csv")
plot(forest)
summary(pred)
variable_importance(forest)

ordre <- order(variable_importance(forest), decreasing = T)

colnames(vars)[ordre]
