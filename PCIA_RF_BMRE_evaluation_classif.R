# To create random forest models of bat activity
# N.B. : il faudra faire tourner le modèle sur les données non confidentielles
# afin de pouvoir publier le script et les données

# This script is adapted from Mila et al. (preprint)

source("variables.R")
library(data.table)
library(randomForest)
library(gdata)
library(tidyr)
library(dplyr)
library(stringr)
library(sf)
library(CAST)
library(caret)
library(sfdep)
source("variables_sel.R")
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
  optparse::make_option(c("-v", "--variableselection"),
    type = "character", default = "None",
    help = 'Choose which variable selection you want to make between values : "None", "VSURF", "Indispensable", "PCA", "PCA_decomp".'
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
  ),
  optparse::make_option(c("-d", "--date"),
    type = "character", default = Sys.Date(),
    help = "Necessary : pass date when script is run with $(date +%Y-%m-%d)"
  ),
  optparse::make_option(c("-k", "--keep"),
    type = "logical", default = FALSE,
    help = "keep last year data as testing dataset and run tests"
  ),
  optparse::make_option(c("-p", "--period"),
    type = "character", default = "year",
    help = "Which activity are you modelling year, spring, summer or autumn"
  )
)
# Parse options to opt object
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)


#### Options ####--------------------------------------------------------

# Sorting threshold (weighted, 0, 50, 90)
ThresholdSort <- opt$threshold

cat(paste("Threshold :", ThresholdSort), fill = TRUE)

# Species to model
Sp <- opt$species # choose a species (e.g. "Pippip") or "all" or "paper"
selection <- opt$variableselection

GroupSel <- "bat"
# GroupSel=NA #sorting according to the group column of Specieslist
# (args[3), NA if no sorting
ListPaper <- c(
  "Minsch", "Barbar", "Nyclei", "Nycnoc", "Eptser", "Pipkuh", "Pipnat",
  "Pippip", "Pippyg", "Rhifer"
)
# Filter data by date?
# e.g.as.Date("2021-12-31") only use  data before this date

date_limit <- opt$date
# Predictors and model specs
# "EDF" = X + Y + Euclidian Distance Fields ;  "noCoord" = no coordinates
YearEffect <- FALSE # Add year?
# MTRY = "default"  # "default" or "npred" or "2-3" for 2/3 of npred
## NTREE <- 500

# Do variable selection?
DoBoruta <- opt$boruta

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
  # if (opt$region == "france_met") {
  args[2] <- file.path(
    data_path,
    "observations",
    "donnees_vigie_chiro",
    paste0(
      "GI_",
      opt$region,
      "_sites_localites"
    )
  )


  # } else if (opt$region == "idf") {
  #   args[2] <- file.path(
  #     data_path,
  #     "observations",
  #     "donnees_vigie_chiro",
  #     "GI_idf_sites_localites"
  #   )
  # } else if (opt$region == "europe") {
  #   args[2] <- file.path(
  #     data_path,
  #     "observations",
  #     "donnees_vigie_chiro",
  #     "GI_europe_sites_localites"
  #   )
  # }


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

p_start <- switch(opt$period,
  year = 1L,
  spring = 5L,
  summer = 11L,
  autumn = 15L
)
p_end <- switch(opt$period,
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

if (Sp == "all" || Sp == "All") {
  ListSp <- ListSp
} else if (Sp == "paper") {
  ListSp <- ListPaper
} else {
  ListSp <- Sp
}

#### Prepare dataset for each species ####------------------------------------------------------

print(ListSp)
for (i in seq_along(ListSp))
{
  DataSp <- subset(DataCPL3, DataCPL3$espece == ListSp[i]) # subset species
  # DataSp=subset(DataCPL3,DataCPL3$espece==Sp) # subset species
  print(ListSp[i])
  START1 <- Sys.time()

  # Adds 0 counts using the observation table (avoids user errors but makes the
  # assumption that this table always contains at least 1 species per night)
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
  DataSpSL_w0_2$espece[is.na(DataSpSL_w0_2$espece)] <- ListSp[i]


  # Exclude sites outside France limits (square) :
  DataSpSL_w0_2 <- subset(DataSpSL_w0_2, DataSpSL_w0_2$longitude < 10L &
    DataSpSL_w0_2$longitude > -6L &
    DataSpSL_w0_2$latitude < 52L & DataSpSL_w0_2$latitude > 41L)

  # Exclude data with obvious wrong date (<2010)
  DataSpSL_w0_2 <- DataSpSL_w0_2[which(DataSpSL_w0_2$Nuit > as.Date("2010-01-01")), ]


  DataSpSL_w0_2$Nuit <- as.Date(DataSpSL_w0_2$Nuit)
  DataSpSL_w0_2 <- unique(DataSpSL_w0_2)
  CoordPS <- unique(CoordPS)

  CoordPS$Nuit <- as.Date(CoordPS$Nuit)

  DataSaison <- inner_join(DataSpSL_w0_2, CoordPS,
    by = c("longitude", "latitude", "Nuit", "participation")
  ) # adds environmental variables to activity data
  print("lignes datasaison :")
  print(nrow(DataSaison))
  print("colonnes datasaison")
  print(ncol(DataSaison))
  print(Sys.time())

  cat("Absence data added", fill = TRUE)
  # lets add the "gites" information

  # data_gites <- read.csv2(file_gites)
  #
  # data_gites <- data_gites |>
  #   dplyr::select(participation, espece, Nuit, num_micro, indice_gite)
  #
  # data_gites$Nuit <- as.Date(data_gites$Nuit)
  #
  # DataSaison <- left_join(
  #   DataSaison, data_gites,
  #   by = c("participation", "Nuit", "num_micro", "espece")
  # )
  #
  # print("lignes datasaison apres gite :")
  # print(nrow(DataSaison))
  #
  #
  # DataSaison$indice_gite <- as.numeric(DataSaison$indice_gite)
  #
  # add date of year,

  if (grepl("/", DataSaison$Nuit[1L], fixed = TRUE)) {
    Date1 <- as.Date(substr(DataSaison$Nuit, 1L, 10L),
      format = "%Y/%m/%Y"
    )
  } else {
    Date1 <- as.Date(DataSaison$Nuit)
  }

  SpFDate <- yday(Date1)

  print("calculating sin/cos for date")
  #
  # DataSaison$SpCDate <- cos(SpFDate / 365L * 2L * pi) # to create a circular variable for date
  # DataSaison$SpSDate <- sin(SpFDate / 365L * 2L * pi) # to create a circular variable for date
  #
  # If year effect must be accounted for
  if (YearEffect) {
    DataSaison$SpYear <- year(Date1)
  }

  DataSaison_sf <- sf::st_as_sf(
    DataSaison,
    coords = c(x = "longitude", y = "latitude"),
    crs = 4326L
  ) |>
    sf::st_transform(2154L)

  coords <- as.data.frame(st_coordinates(DataSaison_sf))

  # Add material as predictor
  DataSaison$SpRecorder <- DataSaison$detecteur_enregistreur_type
  print("lignes datasaison apres edf :")
  print(nrow(DataSaison))


  # Identify predictors
  # DataSaison <- DataSaison |> # removing medium and large buffers
  #   dplyr::select(!dplyr::ends_with("S"))
  # DataSaison <- DataSaison |>
  #   dplyr::select(!dplyr::ends_with("L"))
  #
  testPred <- startsWith(names(DataSaison), "Sp")
  Prednames <- names(DataSaison)[testPred]

  clc <- startsWith(Prednames, "SpHC")
  Prednames <- Prednames[!clc]
  Prednames <- Prednames[!(Prednames %in% variables_a_exclure)]
  print("prednames: ")
  print(Prednames)


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

  DataSaison <- DataSaison |>
    drop_na(all_of(Prednames)) |> # deletes rows without predictor (outdated GI table)
    drop_na(nb_contacts) # deletes rows without contacts (people did not upload their data)

  cat("Predictors identified", fill = TRUE)

  # Statistics for paper

  Stat1 <- DataSaison |>
    group_by(latitude, longitude, nom) |>
    count()
  cat(
    paste0(
      "N opportunistic sites = ", length(which(grepl("Z", Stat1$nom))),
      " over a total of ", nrow(Stat1), " sites"
    ),
    fill = TRUE
  )
  cat(
    paste0(
      "N opportunistic nights = ", length(which(grepl("Z", DataSaison$nom))),
      " over a total of ", nrow(DataSaison), " nights"
    ),
    fill = TRUE
  )

  testNA <- apply(Predictors, MARGIN = 2, FUN = function(x) sum(is.na(x)))
  print(summary(testNA))
  testNA2 <- apply(Predictors, MARGIN = 1, FUN = function(x) sum(is.na(x)))
  print(summary(testNA2))
  Sys.time()

  #### Modelling ####-----------------------------------------------------------

  # Prepare random and spatial cross-validation indices
  cat("Preparing cross-validation indices", fill = TRUE)
  sfolds_source <- file.path(
    Output,
    paste0(
      "VC",
      ThresholdSort,
      "_",
      ListSp[i],
      "_temp_sfolds.rds"
    )
  )

  print("Load Area of Interest:")
  aoi <- sf::read_sf(
    dsn = args[4],
    layer = opt$region
  ) |>
    st_transform(2154)

  print("Prep data saison as sf object :")
  DataSaison_sf <- st_as_sf(DataSaison,
    coords = c(x = "longitude", y = "latitude"),
    remove = FALSE,
    crs = 4326
  ) |>
    st_transform(2154)

  if (opt$keep) {
    # last_year <- max(DataSaison$SpYear)
    DataTest_sf <- DataSaison_sf[DataSaison_sf$SpYear == 2019, ]
    DataSaison_sf <- DataSaison_sf[DataSaison_sf$SpYear != 2019, ]
    DataTest <- DataTest_sf |>
      st_drop_geometry()
  }

  DataSaison_sf <- DataSaison_sf[aoi, ]
  DataSaison_sf$acti_class <- def_classes(DataSaison_sf)

  print("DataSaison filtered for season")

  DataSaison_sf <- DataSaison_sf[dplyr::between(DataSaison_sf$fortnight, p_start, p_end), ]

  DataSaison_sf <- filter_by_max_grid(DataSaison_sf, opt$region)

  DataSaison <- DataSaison_sf |>
    st_drop_geometry()


  set.seed(123)


  START <- Sys.time()
  print("Creating folds :")
  sfolds <- CAST::knndm(DataSaison_sf, aoi, k = 10, maxp = 0.5) # k = number of folds
  END <- Sys.time()
  print(END - START) # 1 to 1.4 hours
  # beep(2)
  saveRDS(sfolds, sfolds_source)
  print("sfolds written")

  DataSaison$sfold <- sfolds$clusters


  if ("acti_class" %in% colnames(DataSaison)) {
    print("acticlass ok")
  }

  if ("sfold" %in% colnames(DataSaison)) {
    print("sfold ok")
  }

  sindx <- CAST::CreateSpacetimeFolds(DataSaison,
    spacevar = "sfold",
    class = "acti_class",
    k = 10
  )
  print("checkpoint1")
  sctrl <- caret::trainControl(
    method = "cv",
    index = sindx$index,
    savePredictions = "final"
  )

  print("checkpoint2")
  cat("Cross-validation indices prepared", fill = TRUE)

  if (opt$keep) {
    print("keep")
    write.csv(
      DataTest,
      file.path(
        Output,
        paste0(
          ListSp[i], "_", opt$period, "_", opt$region, "_datatest.csv"
        )
      )
    )
  }

  write.csv(
    DataSaison,
    file.path(
      Output,
      paste0(
        ListSp[i], "_", opt$period, "_", opt$region, "_datatrain.csv"
      )
    )
  )
  DataSaison$acti_class <- factor(DataSaison$acti_class, levels = c("NoAct", "Faible", "Moyen", "Fort", "TresFort"))
  print("classes:")
  print(unique(DataSaison$acti_class))
  samp_sizes <- def_sample_vector(DataSaison, "acti_class", 0.66)
  DataSaison$SpRoAddM <- DataSaison$SpRo1M + DataSaison$SpRo2M +
    DataSaison$SpRo3M + DataSaison$SpRo4M

  if (selection == "PCA") {
    predictors <- DataSaison[, ..variables_acp]


    acp <- get_components(predictors, "PCA")
    acp_pc_vars <- acp$components

    saveRDS(acp$acp, file.path(Output, paste0("acp_", opt$period, ".rds")))

    DataSaison <- cbind(DataSaison, acp_pc_vars)
    vars_names <- names(acp_pc_vars)
    Prednames <- c(vars_names, vars_norm)
  } else if (selection == "PCA_decomp") {
    small_vars <- endsWith(names(DataSaison), "S")
    data <- DataSaison[, !small_vars]

    occsol_vars <- startsWith(names(data), "SpHOCS")
    occsol_vars <- names(data)[occsolvars]

    bioclim_vars <- startsWith(names(data), "SpBioC")
    bioclim_vars <- names(data)[bioclimvars]

    names_data <- names(data)
    names_data <- names_data[!(names_data %in% bioclimvars)]
    names_data <- names_data[!(names_data %in% occsolvars)]

    other_vars <- startsWith(names_data, "Sp")


    other_vars <- names_data[other_vars]


    predictors_occs <- data[, occsol_vars]
    predictors_bioc <- data[, bioclim_vars]
    predictors_other <- data[, other_vars]

    bioclim <- get_components(predictors_bioc, "bioclim")
    bioclim_pc_vars <- bioclim$components

    occsol <- get_components(predictors_occs, "occsol")
    occsol_pc_vars <- occsol$components

    others <- get_components(predictors_other, "autres")
    other_pc_vars <- others$components

    saveRDS(bioclim$acp, file.path(Output, paste0("acp_bioclim_", opt$period, ".rds")))
    saveRDS(occsol$acp, file.path(Output, paste0("acp_occsol_", opt$period, ".rds")))
    saveRDS(others$acp, file.path(Output, paste0("acp_occsol_", opt$period, ".rds")))

    vars <- cbind(occsol_pc_vars, bioclim_pc_vars, other_pc_vars)
    DataSaison <- cbind(DataSaison, vars)
    Prednames <- names(vars)
  } else if (selection == "VSURF") {
    selected_index <- get_prednames(DataSaison, Prednames, "acti_class", samp_sizes)
    Prednames <- Prednames[selected_index]
  } else if (selection == "bio") {
    Prednames <- c(variables_bio, vars_norm)
  }
  # print("end of test") # TODO: remove these two lines
  # stop()
  print("checkpoint4")
  noSpacemod <- fitvalpred_rf_cat(
    Prednames,
    # rctrl,
    sctrl,
    DataSaison,
    samp_sizes
    # tempstack[[c(basecovs, proxycovs)]]
  )

  print("Model done")

  #### Save ####----------------------------------------------------------------

  if (DoBoruta == T) {
    suffix <- paste0("_Boruta_", opt$period, "_noSpace", "_", ListSp[i])
  } else {
    suffix <- paste0(opt$period, "_", opt$region, "_noSpace", "_", ListSp[i])
  }

  write.csv(
    noSpacemod$tab,
    file.path(
      Output,
      paste0(
        "Evaluation_",
        ListSp[i],
        "_",
        Tag, "_",
        date_limit,
        "_",
        suffix,
        ".csv"
      )
    )
  )


  data.table::fwrite(noSpacemod$graphmod, file.path(Output, paste0(suffix, ".csv")))

  write("noSpace", file.path(Output, paste0(suffix, ".txt")), append = TRUE)
  nosp <- print(noSpacemod$spatmod)

  write(nosp, file.path(Output, paste0(suffix, ".txt")), append = TRUE)

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
        "_",
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
