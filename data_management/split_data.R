library(dplyr)
library(tidyr)
grid_file <- "/home/tsevere/Nextcloud/tsevere/projet_git_BMRE/data/observations/pred_vars/SysGrid_500m_de_cote_france_met.csv"
library(data.table)
grid <- read.csv(grid_file)
folder <- "/home/tsevere/Nextcloud/tsevere/projet_git_BMRE/data/observations/pred_vars/"

n <- nrow(grid)
j <- floor(n/20)

for (i in 1:20){
  print(i)
}

for (i in 1:20) {
  print(i)
 if (i == 1) {
   a <- i
   b <- j
 } else {
    a <- i * j + 1
    b <- a + j
    }
  mini_grid <- grid[a:b,]
  mini_grid  %>% drop_na()
  write.csv(mini_grid, file.path(folder,
                                 paste0("SysGrid_500", i, "m_de_cote_france_met.csv")))
}

grid[a:b, ]
print(n)
