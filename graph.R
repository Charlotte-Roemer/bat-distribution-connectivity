source("variables.R")
library(ggplot2)
library(data.table)
library(randomForest)
library(gdata)
library(tidyr)
library(dplyr)
library(stringr)
library(sf)
library(CAST)
# library(Boruta)
# library(caret)
# library(sfdep)
source("variables_sel.R")
source("RF_prepare_data.R")
# source("RF_functions.R")

# Place <- "local" # local, PCIA or IN2P3

folder_img <- "/home/tsevere/Documents/mnhn/projet_git_BMRE/analyse_donnees"
## Script is called with Rscript and options :
# option_list <- list(
#   optparse::make_option(c("-r", "--region"),
#     type = "character", default = "france_met",
#     help = 'Set region of interest between "france_met" (default),
#     "europe", "idf" or paca for testing purposes'
#   ),
#   optparse::make_option(c("-t", "--threshold"),
#     type = "character", default = "50",
#     help = 'Set sorting threshold between values : "0", "50", "90" and "weighted'
#   ),
#   optparse::make_option(c("-s", "--species"),
#     type = "character", default = "paper",
#     help = 'Set modelling species between "paper", "all" or a 6 character species code (e.g. "Pippip")'
#   ),
#   optparse::make_option(c("-b", "--boruta"),
#     type = "logical", default = FALSE,
#     help = "Do you want to execute boruta feature selection ? Default no (FALSE) "
#   ),
#   optparse::make_option(c("-c", "--cure"),
#     type = "logical", default = FALSE,
#     help = "Do you want to randomly remove close data (spatially and temporally) ?"
#   ),
#   optparse::make_option(c("-d", "--date"),
#     type = "character",
#     help = "Necessary : pass date when script is run with $(date +%Y-%m-%d)"
#   ),
#   optparse::make_option(c("-k", "--keep"),
#     type = "logical", default = FALSE,
#     help = "keep last year data as testing dataset and run tests"
#   ),
#   optparse::make_option(c("-p", "--period"),
#     type = "character", default = "year",
#     help = "Which activity are you modelling year, spring, summer or autumn"
#   )
# )
# # Parse options to opt object
# opt_parser <- optparse::OptionParser(option_list = option_list)
# opt <- optparse::parse_args(opt_parser)
region <- "france_met"
#
# #### Options ####--------------------------------------------------------
#
# Sorting threshold (weighted, 0, 50, 90)

ThresholdSort <- 50

cat(paste("Threshold :", ThresholdSort), fill = TRUE)

# Species to model
ListPaper <- c(
  "Minsch", "Barbar", "Nyclei", "Nycnoc", "Eptser", "Pipkuh", "Pipnat",
  "Pippip", "Pippyg", "Rhifer"
)



GroupSel <- "bat"
# GroupSel=NA #sorting according to the group column of Specieslist
# (args[3), NA if no sorting

# Filter data by date?
# e.g.as.Date("2021-12-31") only use  data before this date

date_limit <- format(Sys.Date(), format = "%Y-%m-%d")

# Predictors and model specs
# "EDF" = X + Y + Euclidian Distance Fields ;  "noCoord" = no coordinates
YearEffect <- TRUE # Add year?
# MTRY = "default"  # "default" or "npred" or "2-3" for 2/3 of npred
## NTREE <- 500

# Do variable selection?

#### Setting Directories ####--------------------------------------------------

if (Place == "local") {
  # bat activity table (not DI !! --> need the file where microphone
  # quality is sorted out) . file without csv extension
  args <- file.path(
    data_path,
    "observations",
    "donnees_vigie_chiro",
    paste0(
      "SpNuit2_",
      ThresholdSort,
      "_DataLP_PF_exportTot"
    )
  )

  # table with spatial variables (habitat and climate) :
  args[2] <- file.path(
    data_path,
    "observations",
    "donnees_vigie_chiro",
    paste0(
      "GI_",
      region,
      "_sites_localites"
    )
  )

  # Species list to build models :
  args[3] <- file.path(
    data_path,
    "observations",
    "donnees_vigie_chiro",
    "SpeciesList.csv"
  )

  # Study area limits file :
  args[4] <- file.path(
    data_path,
    "GIS",
    "regions.gpkg"
  )

  # folder to copy models to (fichiers .learner), no "_" else bug !!! :
  Output <- file.path(
    data_path,
    "ModPred",
    paste0(
      "VC",
      ThresholdSort,
      "_",
      Sys.Date()
    )
  )

  # the file with data about participations :
  Fpar <- file.path(
    data_path,
    "observations",
    "donnees_vigie_chiro",
    "p_export.csv"
  )

  # the file with the data about localities :
  Fsl <- file.path(
    data_path,
    "observations",
    "donnees_vigie_chiro",
    "sites_localites.txt"
  )
}

args[6] <- "participation" # name of sampling event
args[7] <- "localite" # name of locality in CoordSIG (if DataLoc=T)
args[8] <- "participation" # name of participation (=sampling event)

# the name of the parameter which gives the metric to predict:
args[10] <- "nb_contacts_nd"

# pass the limit date as argument
args[11L] <- as.character(date_limit)

# tag which will be written in the filename, no "_", else bug !!! :
Tag <- paste0("VC", ThresholdSort)

# name of columns with coordinates in the locality table (sites_localites.txt) :
coordinate_names <- c("X", "Y")
args[12] <- coordinate_names[1]
args[13] <- coordinate_names[2]

dir.create(Output)


#### Set season limits ####-----------------------------------------------------

period <- "summer"
p_start <- switch(period,
  year = 1L,
  spring = 5L,
  summer = 11L,
  autumn = 15L
)
p_end <- switch(period,
  year = 27,
  spring = 10L,
  summer = 14L,
  autumn = 20L
)

#### Prepare general dataset ####-----------------------------------------------

List_data_prepared <- prepare_data(args, Fpar, Fsl)

CoordPS <- List_data_prepared[[1]] # environmental variables
DataCPL3 <- List_data_prepared[[2]] # bat activity (without absence data)
SelParSL <- List_data_prepared[[3]] # list of sampling sessions to know when to add absence data

# remove na (if no better solution has been found)
CoordPS <- na.omit(CoordPS)
cat("General dataset prepared", fill = TRUE)

# Identify the variable to predict as nb_contacts
DataCPL3$nb_contacts <- subset(DataCPL3, select = args[10])[, 1]
# TEST

# write.csv(DataCPL3,
#      file.path(Output,
#        paste0(
#          "test_dat", "_datacpl3.txt"
#        )
#      )
#    )
test1 <- nrow(DataCPL3)
DataCPL3 <- subset(DataCPL3, !is.na(DataCPL3$nb_contacts))

# TEST
# write.csv(DataCPL3,
#      file.path(Output,
#        paste0(
#          "test_dat", "_datacpl3cleaned.txt"
#        )
#      )
#    )
test2 <- nrow(DataCPL3)
ifelse(test1 == test2, print("ok"), stop("NA present in activity data!"))

# List species to model
SpeciesList <- fread(args[3]) # read species list
ListSp <- levels(as.factor(DataCPL3$espece))
ListSp <- subset(ListSp, ListSp %in% SpeciesList$Esp)
if (!is.na(GroupSel)) {
  SpSel <- subset(SpeciesList, SpeciesList$Group %in% GroupSel)
  ListSp <- subset(ListSp, ListSp %in% SpSel$Esp)
}

if (Sp == "all" || Sp == "All") {
  ListSp <- ListSp
} else if (Sp == "paper") {
  ListSp <- ListPaper
} else {
  ListSp <- Sp
}

#### Prepare dataset for each species ####------------------------------------------------------

print(ListSp)

ListPaper <- "Pipkuh"
for (i in seq_along(ListPaper))
{
  DataSp <- subset(DataCPL3, DataCPL3$espece == ListPaper[i]) # subset species
  # DataSp=subset(DataCPL3,DataCPL3$espece==Sp) # subset species
  print(ListPaper[i])
  START1 <- Sys.time()
  DataCPL3_unique <- DataCPL3 |> # prepares the table of the complete set of sampled nights/sites
    select(participation, Nuit, num_micro) |>
    unique()

  DataCPL3_unique$Nuit <- as.Date(DataCPL3_unique$Nuit)

  DataSp$Nuit <- as.Date(DataSp$Nuit)
  DataCPL3$Nuit <- as.Date(DataCPL3$Nuit)
  DataSpSL_w0_2 <- full_join(DataSp, DataCPL3_unique) # Adds the nights with absence
  colnames(DataSpSL_w0_2)[which(colnames(DataSpSL_w0_2) == "point")] <- "nom"

  # performs a partial join (updates columns of DataSpSL_w0_2 with info of SelParSL)
  n <- names(SelParSL)
  DataSpSL_w0_2 <- DataSpSL_w0_2[SelParSL, on = .(participation), (n) := mget(paste0("i.", n))]

  DataSpSL_w0_2$nb_contacts[is.na(DataSpSL_w0_2$nb_contacts)] <- 0L
  DataSpSL_w0_2$score_max[is.na(DataSpSL_w0_2$score_max)] <- 0L
  DataSpSL_w0_2$groupe[is.na(DataSpSL_w0_2$groupe)] <- "bat"
  DataSpSL_w0_2$espece[is.na(DataSpSL_w0_2$espece)] <- ListPaper[i]


  # Exclude sites outside France limits (square) :
  DataSpSL_w0_2 <- subset(DataSpSL_w0_2, DataSpSL_w0_2$longitude < 10L &
    DataSpSL_w0_2$longitude > -6L &
    DataSpSL_w0_2$latitude < 52L & DataSpSL_w0_2$latitude > 41L)

  # Exclude data with obvious wrong date (<2010)
  DataSpSL_w0_2 <- DataSpSL_w0_2[which(DataSpSL_w0_2$Nuit > as.Date("2010-01-01")), ]

  DataSpSL_w0_2$Nuit <- as.Date(DataSpSL_w0_2$Nuit)
  CoordPS$Nuit <- as.Date(CoordPS$Nuit)
  DataSpSL_w0_2 <- unique(DataSpSL_w0_2)
  CoordPS <- unique(CoordPS)
  DataSaison <- inner_join(DataSpSL_w0_2,
    CoordPS,
    by = c("longitude", "latitude", "Nuit", "participation")
  ) # adds environmental variables to activity data ("participation added")
  cat("colonnes datasaison", fill = TRUE)
  print(ncol(DataSaison))

  print(Sys.time())

  cat("Absence data added", fill = TRUE)

  DataSaison$saison[dplyr::between(DataSaison$fortnight, 5, 10)] <- "spring"
  DataSaison$saison[dplyr::between(DataSaison$fortnight, 11, 14)] <- "summer"
  DataSaison$saison[dplyr::between(DataSaison$fortnight, 15, 20)] <- "autumn"
  DataSaison$saison <- factor(DataSaison$saison, levels = c("spring", "summer", "autumn"))

  DataSaison <- DataSaison[!is.na(DataSaison$saison), ]
  DataActivite <- DataSaison[DataSaison$nb_contacts != 0, ]
  DataActivite$logact <- log(DataActivite$nb_contacts)

  breaks <- unname(quantile(
    DataActivite$logact,
    c(0.25, 0.50, 0.75)
  ))
  #
  # breaks <- unname(quantile(
  #   DataActivite$nb_contacts,
  #   c(0.25, 0.50, 0.75)
  # ))

  # RColorBrewer::brewer.pal(n = 3, name = "Dark2")
  # scales::show_col(RColorBrewer::brewer.pal(n = 3, name = "Dark2"))

  # colors <- c("#7FC97F", "#BEAED4", "#FDC086")  # Accent
  # colors <- c("#1B9E77", "#7570B3", "#D95F02") # Dark2

  colors <- c("#4DAF4A", "#377EB8", "#E41A1C") # Set2
  text_color <- "black"
  p <- ggplot(DataActivite, aes(x = logact, fill = saison)) +
    geom_histogram(
      position = "dodge" # make bins next to one another (by saison)
    )
  max <- max(ggplot_build(p)$layout$panel_scales_y[[1]]$range$range) - 20
  p <- p + geom_vline(
    xintercept = breaks,
    color = "darkgrey",
    linetype = "dashed",
  ) +
    scale_fill_manual(values = colors) +
    labs(title = paste(ListPaper[i], ThresholdSort), x = "log(nb_contacts)", y = "Densité") +
    geom_text(aes(x = breaks[1] - 0.3, label = "\nfaible", y = max), colour = text_color, angle = 90) +
    geom_text(aes(x = breaks[2] - 0.3, label = "\nmoyenne", y = max), colour = text_color, angle = 90) +
    geom_text(aes(x = breaks[3] - 0.3, label = "\nhaute", y = max), colour = text_color, angle = 90) +
    geom_text(aes(x = breaks[3] + 0.1, label = "\ntrès haute", y = max), colour = text_color, angle = 90) +
    theme_bw()
  p
  ggsave(
    plot = p,
    width = 30,
    height = 20,
    dpi = 600,
    units = "cm",
    filename = file.path(folder_img, paste0(ListPaper[i], "_", ThresholdSort, ".png"))
  )
}
