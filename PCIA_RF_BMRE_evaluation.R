# To create random forest models of bat activity
# N.B. : il faudra faire tourner le modèle sur les données non confidentielles
# afin de pouvoir publier le script et les données

# This script is adapted from Mila et al. (preprint)

library(data.table)
library(randomForest)
library(gdata)
library(tidyr)
library(dplyr)
library(stringr)
library(sf)
library(CAST)
library(Boruta)
library(caret)
library(sfdep)
source("variables.R")
source("RF_prepare_data.R")
source("RF_functions.R")

Place <- "local" # local, PCIA or IN2P3

## Script is called with Rscript and options :
option_list <- list(
  optparse::make_option(c("-r", "--region"),
    type = "character", default = "france_met",
    help = 'Set region of interest between "france_met" (default),
    "europe", "idf" or paca for testing purposes'
  ),
  optparse::make_option(c("-t", "--threshold"),
    type = "character", default = "50",
    help = 'Set sorting threshold between values : "0", "50", "90" and "weighted'
  ),
  optparse::make_option(c("-s", "--species"),
    type = "character", default = "paper",
    help = 'Set modelling species between "paper", "all" or a 6 character species code (e.g. "Pippip")'
  ),
  optparse::make_option(c("-b", "--boruta"),
    type = "logical", default = FALSE,
    help = "Do you want to execute boruta feature selection ? Default no (FALSE) "
  ),
  optparse::make_option(c("-c", "--cure"),
    type = "logical", default = FALSE,
    help = "Do you want to randomly remove close data (spatially and temporally) ?"
  )
)
# Parse options to opt object
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)


#### Options ####--------------------------------------------------------

# Sorting threshold (weighted, 0, 50, 90)
ThresholdSort <- opt$threshold

opt$species <- "Barbar"
print(paste("Threshold :", ThresholdSort))

# Species to model
Sp <- opt$species # choose a species (e.g. "Pippip") or "all" or "paper"

GroupSel <- "bat"
# GroupSel=NA #sorting according to the group column of Specieslist
# (args[3), NA if no sorting
ListPaper <- c(
  "Minsch", "Barbar", "Nyclei", "Nycnoc", "Eptser", "Pipkuh", "Pipnat",
  "Pippip", "Pippyg", "Rhifer"
)
# Filter data by date?
# e.g.as.Date("2021-12-31") only use  data before this date

date_limit <- Sys.Date()
# Predictors and model specs
CoordType <- "EDF" # Spatial proxies in predictors: "LongLat" = X + Y ;
# "EDF" = X + Y + Euclidian Distance Fields ;  "noCoord" = no coordinates
YearEffect <- TRUE # Add year?
# MTRY = "default"  # "default" or "npred" or "2-3" for 2/3 of npred
## NTREE <- 500

# Do variable selection?
DoBoruta <- opt$boruta

#### Setting Directories ####--------------------------------------------------

if (Place == "local") {
  # bat activity table (not DI !! --> need the file where microphone
  # quality is sorted out) . file without csv extension
  args <- file.path(data_path,
    "observations",
    "donnees_vigie_chiro",
    paste0("SpNuit2_",
      ThresholdSort,
      "_DataLP_PF_exportTot"
    )
  )

  # table with spatial variables (habitat and climate) :
  args[2] <- file.path(data_path,
    "observations",
    "donnees_vigie_chiro",
    "GI_FR_sites_localites"
  )

  # Species list to build models :
  args[3] <- file.path(data_path,
    "observations",
    "donnees_vigie_chiro",
    "SpeciesList.csv"
  )

  # Study area limits file :
  args[4] <- file.path(data_path,
    "GIS",
    "regions.gpkg"
  )

  # folder to copy models to (fichiers .learner), no "_" else bug !!! :
  Output <- file.path(data_path,
    "ModPred",
    paste0("VC",
      ThresholdSort,
      "_",
      Sys.Date()
    )
  )

  # the file with data about participations :
  Fpar <- file.path(data_path,
    "observations",
    "donnees_vigie_chiro",
    "p_export.csv"
  )

  # the file with the data about localities :
  Fsl <- file.path(data_path,
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



#### Prepare general dataset ####-----------------------------------------------

List_data_prepared <- prepare_data(args, Fpar, Fsl)

CoordPS <- List_data_prepared[[1]] # environmental variables
DataCPL3 <- List_data_prepared[[2]] # bat activity (without absence data)
SelParSL <- List_data_prepared[[3]] # list of sampling sessions to know when to add absence data

print("General dataset prepared")

# Identify the variable to predict as nb_contacts
DataCPL3$nb_contacts <- subset(DataCPL3, select = args[10])[, 1]
test1 <- nrow(DataCPL3)
DataCPL3 <- subset(DataCPL3, !is.na(DataCPL3$nb_contacts))
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

if (Sp == "all" | Sp == "All") {
  ListSp <- ListSp
} else if (Sp == "paper") {
  ListSp <- ListPaper
} else {
  ListSp <- Sp
}

#### Prepare dataset for each species ####------------------------------------------------------

print(ListSp)
for (i in 1:length(ListSp))
{
  DataSp <- subset(DataCPL3, DataCPL3$espece == ListSp[i]) # subset species
  # DataSp=subset(DataCPL3,DataCPL3$espece==Sp) # subset species
  print(ListSp[i])
  START1 <- Sys.time()

  # Adds 0 counts using the observation table (avoids user errors but makes the
  # assumption that this table always contains at least 1 species per night)
  DataCPL3_unique <- DataCPL3 %>% # prepares the table of the complete set of sampled nights/sites
    select(participation, Nuit, num_micro) %>%
    unique()
  DataCPL3_unique$Nuit <- as.Date(DataCPL3_unique$Nuit)

  DataSp$Nuit <- as.Date(DataSp$Nuit)
  DataCPL3$Nuit <- as.Date(DataCPL3$Nuit)
  DataSpSL_w0_2 <- full_join(DataSp, DataCPL3_unique) # Adds the nights with absence
  colnames(DataSpSL_w0_2)[which(colnames(DataSpSL_w0_2) == "point")] <- "nom"

  # performs a partial join (updates columns of DataSpSL_w0_2 with info of SelParSL)
  n <- names(SelParSL)
  DataSpSL_w0_2 <- DataSpSL_w0_2[SelParSL, on = .(participation), (n) := mget(paste0("i.", n))]

  DataSpSL_w0_2$nb_contacts[is.na(DataSpSL_w0_2$nb_contacts)] <- 0
  DataSpSL_w0_2$score_max[is.na(DataSpSL_w0_2$score_max)] <- 0

  # Exclude sites outside France limits (square) :
  DataSpSL_w0_2 <- subset(DataSpSL_w0_2, DataSpSL_w0_2$longitude < 10 & DataSpSL_w0_2$longitude > -6 &
    DataSpSL_w0_2$latitude < 52 & DataSpSL_w0_2$latitude > 41)

  # Exclude data with obvious wrong date (<2010)
  DataSpSL_w0_2 <- DataSpSL_w0_2[which(DataSpSL_w0_2$Nuit > as.Date("2010-01-01")), ]

  DataSpSL_w0_2$Nuit <- as.Date(DataSpSL_w0_2$Nuit)
  CoordPS$Nuit <- as.Date(CoordPS$Nuit)
  DataSaison <- full_join(DataSpSL_w0_2, CoordPS) # adds environmental variables to activity data

  print(Sys.time())

  print("Absence data added")

  # add date of year
  if (grepl("/", DataSaison$Nuit[1])) {
    Date1 <- as.Date(substr(DataSaison$Nuit, 1, 10),
      format = "%Y/%m/%Y"
    )
  } else {
    Date1 <- as.Date(DataSaison$Nuit)
  }

  SpFDate <- yday(Date1)
  DataSaison$SpCDate <- cos(SpFDate / 365 * 2 * pi) # to create a circular variable for date
  DataSaison$SpSDate <- sin(SpFDate / 365 * 2 * pi) # to create a circular variable for date

  # If year effect must be accounted for
  if (YearEffect) {
    DataSaison$SpYear <- year(Date1)
  }

  DataSaison_sf <- st_as_sf(DataSaison, coords = c(x = "longitude", y = "latitude"), crs = 4326) %>%
    st_transform(2154)

  coords <- as.data.frame(st_coordinates(DataSaison_sf))

  # sf object with 5 points: the bounding box of the grid of points + the center
  EDF <- rbind(
    st_sf(geom = st_sfc(st_point(c(min(coords$X), min(coords$Y))))),
    st_sf(geom = st_sfc(st_point(c(min(coords$X), max(coords$Y))))),
    st_sf(geom = st_sfc(st_point(c(max(coords$X), min(coords$Y))))),
    st_sf(geom = st_sfc(st_point(c(max(coords$X), max(coords$Y))))),
    st_sf(geom = st_sfc(st_point(c(median(coords$X), median(coords$Y)))))
  )
  EDF <- st_set_crs(EDF, st_crs(DataSaison_sf))
  EDF <- st_distance(DataSaison_sf, EDF) / 1000 # calculate distance between the point and each of these 5 points
  EDF <- units::drop_units(EDF)
  EDF <- as.data.frame(EDF)
  names(EDF) <- paste0("EDF", 1:5)
  DataSaison$SpEDF1 <- EDF$EDF1
  DataSaison$SpEDF2 <- EDF$EDF2
  DataSaison$SpEDF3 <- EDF$EDF3
  DataSaison$SpEDF4 <- EDF$EDF4
  DataSaison$SpEDF5 <- EDF$EDF5

  DataSaison$Splatitude <- DataSaison$latitude
  DataSaison$Splongitude <- DataSaison$longitude

  # Add material as predictor
  DataSaison$SpRecorder <- DataSaison$detecteur_enregistreur_type

  # Identify predictors
  testPred <- (substr(names(DataSaison), 1, 2) == "Sp")
  Prednames <- names(DataSaison)[testPred]
  testPredLatLong <- substr(Prednames, 3, 5) != "EDF" 
  PrednamesLatLong <- Prednames[testPredLatLong] # for latlong only RF

  # Do not use species distribution area yet
  ListSpeciesDistribution <- c(
    "SpBarbar", "SpMinpal", "SpMinsch", "SpMyoalc", "SpMyobec", "SpMyobly",
    "SpMyobra", "SpMyocap", "SpMyodas", "SpMyodau", "SpMyodav", "SpMyoema",
    "SpMyoesc", "SpMyomyo", "SpMyomys", "SpMyonat", "SpMyopun", "SpMyosch",
    "SpNyclas", "SpNyclei", "SpNycnoc", "SpPiphan", "SpPipkuh", "SpPipnat",
    "SpPippip", "SpPippyg", "SpPleaur", "SpPleaus", "SpPlechr", "SpPlekol",
    "SpPlemac", "SpPlesar", "SpRhibla", "SpRhieur", "SpRhifer", "SpRhihip",
    "SpRhimeh", "SpTadten", "SpVesmur", "SpEptana", "SpEptbot", "SpEptisa",
    "SpEptnil", "SpEptser", "SpHypsav"
  )
  Prednames <- Prednames[which(!Prednames %in% ListSpeciesDistribution)]

  Predictors <- DataSaison[, ..Prednames]
  PredictorsLatLong <- DataSaison[, ..PrednamesLatLong]

  DataSaison <- DataSaison %>%
    drop_na(all_of(Prednames)) %>% # deletes rows without predictor (outdated GI table)
    drop_na(nb_contacts) # deletes rows without contacts (people did not upload their data)

  # select only one value per 500m square :
  # ... add code here
  print("Keeping only one night per 500sq/15days")
  DataTest <- DataSaison[duplicated(DataSaison$code), ]
  DataSaison <- DataSaison[!duplicated(DataSaison$code), ]
  print("Rows in training dataset")
  print(nrow(DataSaison))

  print("Rows removed")
  print(nrow(DataTest))

  # filtering excessive values
  # quant <- quantile(DataSaison$nb_contacts, probs = 0.98)
  # print(head(DataSaison[, "nb_contacts"]))

  # DataSaison <- DataSaison[DataSaison$nb_contacts <= quant, ]

  moran <- check_moran(DataSaison, "nb_contacts")

  print("Predictors identified")

  # Statistics for paper
  Stat1 <- DataSaison %>%
    group_by(latitude, longitude, nom) %>%
    count()
  print(paste0(
    "N opportunistic sites = ", length(which(grepl("Z", Stat1$nom))),
    " over a total of ", nrow(Stat1), " sites"
  ))
  print(paste0(
    "N opportunistic nights = ", length(which(grepl("Z", DataSaison$nom))),
    " over a total of ", nrow(DataSaison), " nights"
  ))

  testNA <- apply(Predictors, MARGIN = 2, FUN = function(x) sum(is.na(x)))
  print(summary(testNA))
  testNA2 <- apply(Predictors, MARGIN = 1, FUN = function(x) sum(is.na(x)))
  print(summary(testNA2))
  Sys.time()

  cat("Boruta", fill = TRUE)

  # Find Boruta formula (variable)
  if (DoBoruta == T) {
    cat("yes", fill = TRUE)
    Dataset.Boruta <- data.frame("ActLog10" = DataSaison$ActLog10, DataSaison[, ..Prednames])
    ModRFTemp.Boruta <- Boruta(formula("ActLog10 ~."), # Build model
      data = Dataset.Boruta,
      doTrace = 2, ntree = 500, maxRuns = 100
    )
    formula.Boruta <- try(getConfirmedFormula(ModRFTemp.Boruta)) # retrieve formula of selected variables if at least one was selected (error if none is selected)
    if (inherits(formula.Boruta, "try-error")) {
      formula.Boruta <- formula("ActLog10 ~.")
      errorlog <- data.frame(
        "message" = paste0(
          "Boruta ended by not selecting any predictor for model ",
          ListSp[i]
        )
      )
      fwrite(errorlog, file.path(Output, paste0(ListSp[i], "_", Tag, "_log.txt")))
    } else {
      formula.Boruta <- getConfirmedFormula(ModRFTemp.Boruta)
      names.Boruta <- getSelectedAttributes(ModRFTemp.Boruta)
    }
    print("Formula found")
  } else {
    cat("no", fill = TRUE)
    formula.Boruta <- formula("ActLog10 ~.")
    names.Boruta <- Prednames
  }
  #### Modelling ####-----------------------------------------------------------

  # Prepare random and spatial cross-validation indices
  cat("Preparing cross-validation indices", fill = TRUE)
  sfolds_source <- file.path(Output,
    paste0("VC",
      ThresholdSort,
      "_",
      ListSp[i],
      "_temp_sfolds.rds"
    )
  ) # quezaco?

  if (!file.exists(sfolds_source)) {
    DataSaison_sf <- st_as_sf(DataSaison,
      coords = c(x = "longitude", y = "latitude"),
      crs = 4326) %>%
      st_transform(2154)
    aoi <- sf::read_sf(
      dsn = args[4],
      layer = opt$region
    ) %>%
      st_transform(2154)
    set.seed(123)
    START <- Sys.time()
    sfolds <- knndm(DataSaison_sf, aoi, k = 10, maxp = 0.5) # k = number of folds
    END <- Sys.time()
    print(END - START) # 1 to 1.4 hours
    # beep(2)
    saveRDS(sfolds, sfolds_source)
  } else { 
    sfolds <- readRDS(sfolds_source)
  }

  DataSaison$sfold <- sfolds$clusters
  sindx <- CreateSpacetimeFolds(DataSaison,
    spacevar = "sfold",
    ## timevar = "fortnight",
    k = 10
  )
  sctrl <- caret::trainControl(method = "cv",
    index = sindx$index,
    savePredictions = "final"
  )

  print("Cross-validation indices prepared")

  # EDF model

  set.seed(123)
  EDFmod <- fitvalpred_rf(
    names.Boruta,
    sctrl,
    DataSaison
  )

  print("Model done")

  #### Save ####----------------------------------------------------------------

  if (DoBoruta == TRUE) {
    suffix <- paste0("_Boruta_", "EDF", "_", ListSp[i])
  } else {
    suffix <- paste0("EDF", "_", ListSp[i])
  }

  write.csv(DataTest,
    file.path(Output,
      paste0(
        ListSp[i], "_datatest.txt"
      )
    )
  )

  write.csv(
    EDFmod$tab,
    file.path(
      Output,
      paste0(
        "Evaluation_",
        ListSp[i],
        Tag, "_",
        date_limit,
        "_",
        suffix,
        ".csv"
      )
    )
  )

  data.table::fwrite(EDFmod$graphmod, paste0(Output, "_", suffix, ".csv"))

  write(ListSp[1], paste0(Output, "_", suffix, ".txt"))
  write("----", paste0(Output, "_", suffix, ".txt"), append = TRUE)
  write("Moran :", paste0(Output, "_", suffix, ".txt"), append = TRUE)
  write(moran, paste0(Output, "_", suffix, ".txt"), append = TRUE)
  write("EDF", paste0(Output, "_", suffix, ".txt"), append = TRUE)
  edf <- print(EDFmod$spatmod)
  write(edf, paste0(Output, ".txt"), append = TRUE)

  # saveRDS(EDFmod$tunemod, paste0(Output, "/RFtune_", ListSp[i]
  #                                ,Tag,"_", date_limit
  #                                ,"_", suffix, ".rds"))
  saveRDS(
    EDFmod$spatmod,
    file.path(
      Output,
      paste0(
        "RFspat_",
        ListSp[i],
        Tag,
        "_",
        date_limit,
        "_",
        suffix,
        ".rds"
      )
    )
  )
  rm("EDFmod")

  ## LatLong model


  # remove EDF variables from names.boruta
  testPred <- substr(names.Boruta, 1, 5) != "SpEDF"
  names.Boruta <- names.Boruta[testPred]

  LatLongmod <- fitvalpred_rf(
    names.Boruta,
    # rctrl,
    sctrl,
    DataSaison
    # tempstack[[c(basecovs, proxycovs)]]
  )

  print("Model done")

  #### Save ####----------------------------------------------------------------

  if (DoBoruta == T) {
    suffix <- paste0("_Boruta_", "LatLong", "_", ListSp[i])
  } else {
    suffix <- paste0("LatLong", "_", ListSp[i])
  }

  write.csv(
    LatLongmod$tab,
    file.path(
      Output,
      paste0(
        "Evaluation_",
        ListSp[i],
        Tag, "_",
        date_limit,
        "_",
        suffix,
        ".csv"
      )
    )
  )


  data.table::fwrite(LatLongmod$graphmod, paste0(Output, "_", suffix, ".csv"))

  write("LatLong", paste0(Output, "_", suffix, ".txt"), append = TRUE)
  latlng <- print(LatLongmod$spatmod)
  write(latlng, paste0(Output, "_", suffix, ".txt"), append = TRUE)

  # saveRDS(EDFmod$tunemod, paste0(Output, "/RFtune_", ListSp[i]
  #                                ,Tag,"_", date_limit
  #                                ,"_", suffix, ".rds"))
  saveRDS(
    LatLongmod$spatmod,
    file.path(
      Output,
      paste0(
        "RFspat_",
        ListSp[i],
        Tag,
        "_",
        date_limit,
        "_",
        suffix,
        ".rds"
      )
    )
  )
  rm("LatLongmod")

  # remove EDF variables from names.boruta
  testPred <- substr(names.Boruta, 1, 5) != "Splat"
  names.Boruta <- names.Boruta[testPred]
  testPred <- substr(names.Boruta, 1, 5) != "Splon"
  names.Boruta <- names.Boruta[testPred]

  noSpacemod <- fitvalpred_rf(
    names.Boruta,
    # rctrl,
    sctrl,
    DataSaison
    # tempstack[[c(basecovs, proxycovs)]]
  )

  print("Model done")

  #### Save ####----------------------------------------------------------------

  if (DoBoruta == T) {
    suffix <- paste0("_Boruta_", "noSpace", "_", ListSp[i])
  } else {
    suffix <- paste0("noSpace", "_", ListSp[i])
  }

  write.csv(
    noSpacemod$tab,
    file.path(
      Output,
      paste0(
        "Evaluation_",
        ListSp[i],
        Tag, "_",
        date_limit,
        "_",
        suffix,
        ".csv"
      )
    )
  )


  data.table::fwrite(noSpacemod$graphmod, paste0(Output, "_", suffix, ".csv"))

  write("noSpace", paste0(Output, "_", suffix, ".txt"), append = TRUE)
  nosp <- print(noSpacemod$spatmod)

  write(nosp, paste0(Output, "_", suffix, ".txt"), append = TRUE)

  # saveRDS(EDFmod$tunemod, paste0(Output, "/RFtune_", ListSp[i]
  #                                ,Tag,"_", date_limit
  #                                ,"_", suffix, ".rds"))
  saveRDS(
    noSpacemod$spatmod,
    file.path(
      Output,
      paste0(
        "RFspat_",
        ListSp[i],
        Tag,
        "_",
        date_limit,
        "_",
        suffix,
        ".rds"
      )
    )
  )
  rm("noSpacemod")


  END1 <- Sys.time()
  print(END1 - START1)
  print(paste("Model done for", ListSp[i]))

}


## print(ListSp[i])
## print(ThresholdSort)
## if (DoBoruta) {
##   print(paste0("Variables before selection = ", length(Predictors)))
##   print(paste0("Variables after selection = ", length(names.Boruta)))
## }
