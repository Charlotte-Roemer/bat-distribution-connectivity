
# Stack multiple rasters and writer raster

library(raster)
library(tidyverse)
source("../variables.R")

option_list <- list(
  optparse::make_option(c("-t", "--threshold"),
    type = "character", default = "50",
    help = 'Choose sorting threshold between values : "0", "50", "90" and "weighted'
  ),
  optparse::make_option(c("-s", "--species"),
    type = "character", default = "Minsch",
    help = "Choose for which species you want to make predictions"
  ),
  optparse::make_option(c("-r", "--region"),
    type = "character", default = "france_met",
    help = "Which area do you want to predict on ?"
  ),
)

# Parse options to opt object
opt_parser <- optparse::OptionParser(option_list = option_list)
t <- optparse::parse_args(opt_parser)

print(opt$species)

Threshold = opt$threshold
Sp = opt$species
ListTimes = c("SPRING", "AUTUMN")

output_dir = paste0("/sps/mnhn/croemer/data/Connectivity/VC", Threshold, "all_acticlass_None_season/")

START = Sys.time()

for (k in 1:length(ListTimes)){
  print(ListTimes[k])
  files <- list.files(path=output_dir,
                      pattern=paste0(Sp, ".*", ".tif"), 
                      all.files=FALSE, full.names=TRUE,recursive=F)
    
  s <- stack(files) # stack rasters
  i <- (maxValue(s))>0 # select only rasters that succeeded (contain values > 0)
  s_no_0 = s[[which(i)]]
    
  rs1 <- calc(s_no_0, sum) # sum all rasters
    
  # Save
  dir.create(paste0(output_dir, "Stacked/"))
  writeRaster(rs1, paste0(output_dir, "Stacked/", 
                          paste0(Sp, "_", ListTimes[k]), "_TOTAL_n", 
                          dim(s_no_0)[3],  ".tif"), overwrite = T)
    
  #plot(rs1)
}


END=Sys.time()
TIMEDIFF=END-START
TIMEDIFF

beep(2)