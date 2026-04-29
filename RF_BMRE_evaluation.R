# To create random forest models of bat activity
# N.B. : il faudra faire tourner le modèle sur les données non confidentielles
# afin de pouvoir publier le script et les données

# This script is adapted from Mila et al. (2025)

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
    help = 'Choose which variable selection you want to make between values : "None", "VSURF", "indisp", "PCA", "PCAdecomp".'
  ),
  optparse::make_option(c("-s", "--species"),
    type = "character", default = "paper",
    help = 'Set modelling species between "paper", "all" or a 6 character species code (e.g. "Pippip")'
  ),
  optparse::make_option(c("-d", "--date"),
    type = "character",
    help = "Necessary : pass date when script is run with $(date +%Y-%m-%d)"
  ),
  optparse::make_option(c("-p", "--period"),
    type = "character", default = "year",
    help = "Which activity are you modelling year, spring, summer or autumn"
  ),
  optparse::make_option(c("--data_sel"),
    type = "character", default = "all",
    help = "Do you reduce data by the pixel (no = all, yes = median)"
  ),
  optparse::make_option(c("--acti"),
    type = "character", default = "nb_contacts",
    help = "Which value do you want to predict ? nbcontacts, acticlass"
  ),
  optparse::make_option(c("-k", "--keep"),
    action = "store_true", help = "Do you want to save a test dataset ?"
  )
)
# Parse options to opt object
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)
data_sel <- opt$data_sel
activite <- opt$acti
periode <- opt$period
print(opt$keep)

if (opt$keep == TRUE) {
  test_years <- c(2021, 2022, 2023, 2024) # here set the years you want to use as testing data
}


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
# ListPaper <- c(
#   "Minsch", "Barbar", "Nyclei", "Nycnoc", "Eptser", "Pipkuh", "Pipnat",
#   "Pippip", "Pippyg", "Rhifer"
# )
# Filter data by date?
# e.g.as.Date("2021-12-31") only use  data before this date

date_limit <- opt$date

YearEffect <- TRUE # Add year?

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
    "obs_vars",
    paste0(
      "data_train_",
      opt$region
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

  if (periode == "year") {
    period_mod <- "year"
  } else {
    period_mod <- "season"
  }

  Output <- file.path(
    data_path,
    "ModPred",
    activite,
    paste0(
      "VC",
      ThresholdSort,
      "_",
      data_sel,
      "_",
      activite,
      "_",
      selection,
      "_",
      period_mod
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

dir.create(Output, recursive = TRUE)


#### Set season limits ####-----------------------------------------------------

return_start <- function(period) {
  switch(period,
    year = 60L,
    spring = 60L,
    summer = 152L,
    autumn = 227L
  )
}

return_end <- function(period) {
  switch(period,
    year = 304L,
    spring = 135L,
    summer = 212L,
    autumn = 304L
  )
}

p_start <- return_start(opt$period)
p_end <- return_end(opt$period)
print(paste("starting day : ", p_start, " ending day : ", p_end))

#### Prepare general dataset ####-----------------------------------------------

List_data_prepared <- prepare_data(args, Fpar, Fsl)

CoordPS <- List_data_prepared[[1]] # environmental variables
DataCPL3 <- List_data_prepared[[2]] # bat activity (without absence data)
SelParSL <- List_data_prepared[[3]] # list of sampling sessions to know when to add absence data

print("summary of coordinates CoordPS")
print(summary(CoordPS$longitude))
print(summary(CoordPS$latitude))

print("summary of coordinates SelParSL")
print(summary(SelParSL$longitude))
print(summary(SelParSL$latitude))

# remove na (if no better solution has been found)
CoordPS <- na.omit(CoordPS)
cat("General dataset prepared", fill = TRUE)

# Identify the variable to predict as nb_contacts
DataCPL3$nb_contacts <- subset(DataCPL3, select = args[10])[, 1]
# TEST
test1 <- nrow(DataCPL3)

DataCPL3 <- subset(DataCPL3, !is.na(DataCPL3$nb_contacts))

test2 <- nrow(DataCPL3)

ifelse(test1 == test2, print("ok"), stop("NA present in activity data!"))

## List species to model
# SpeciesList <- fread(args[3]) # read species list
# ListSp <- levels(as.factor(DataCPL3$espece))
# ListSp <- subset(ListSp, ListSp %in% SpeciesList$Esp)
# if (!is.na(GroupSel)) {
#  SpSel <- subset(SpeciesList, SpeciesList$Group %in% GroupSel)
#  ListSp <- subset(ListSp, ListSp %in% SpSel$Esp)
# }
#
# if (Sp == "all" || Sp == "All") {
#  ListSp <- ListSp
# } else if (Sp == "paper") {
#  ListSp <- ListPaper
# } else {
#  ListSp <- Sp
# }

# Add possibility to model Myocry (use Myonat acoustic data but filter the area later)
Sp_real <- Sp
if (Sp_real == "Myocry") {
  Sp <- "Myonat"
}

#### Prepare dataset for each species ####------------------------------------------------------


# for (i in seq_along(ListSp))
# {
# DataSp <- subset(DataCPL3, DataCPL3$espece == ListSp[i]) # subset species

# subset species
if (Sp == "Plesp") {
  DataSp <- subset(DataCPL3, DataCPL3$espece %in% c("Pleaur", "Plemac", "Pleaus"))
} else {
  DataSp <- subset(DataCPL3, DataCPL3$espece == Sp)
}

print(Sp_real)
START1 <- Sys.time()

print("summary of coordinates DataSp")
print(summary(DataSp$longitude))
print(summary(DataSp$latitude))

# Adds 0 counts using the observation table (avoids user errors but makes the
# assumption that this table always contains at least 1 species per night)

# prepares the table of the complete set of sampled nights/sites

DataCPL3_unique <- DataCPL3 |>
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
# DataSpSL_w0_2$espece[is.na(DataSpSL_w0_2$espece)] <- ListSp[i]
DataSpSL_w0_2$espece[is.na(DataSpSL_w0_2$espece)] <- Sp

cat("Absence data added", fill = TRUE)

# Exclude sites outside region limits (square) :
print("summary of coordinates DataSpSL_w0_2")
print(summary(DataSpSL_w0_2$longitude))
print(summary(DataSpSL_w0_2$latitude))
if (opt$region == "idf") {
  DataSpSL_w0_2 <- subset(DataSpSL_w0_2, DataSpSL_w0_2$longitude < 5L &
    DataSpSL_w0_2$longitude > 0L &
    DataSpSL_w0_2$latitude < 50L & DataSpSL_w0_2$latitude > 48L)
}
if (opt$region == "france_met") {
  DataSpSL_w0_2 <- subset(DataSpSL_w0_2, DataSpSL_w0_2$longitude < 10L &
    DataSpSL_w0_2$longitude > -6L &
    DataSpSL_w0_2$latitude < 53L & DataSpSL_w0_2$latitude > 41L)
}
if (opt$region == "europe") {
  DataSpSL_w0_2 <- subset(DataSpSL_w0_2, DataSpSL_w0_2$longitude < 42L &
    DataSpSL_w0_2$longitude > -12L &
    DataSpSL_w0_2$latitude < 73L & DataSpSL_w0_2$latitude > 32L)
}

# Excludes data outside of known area for acoustically cryptic species (doesn't work for Europe yet !!!)
# Myonat
if (Sp_real == "Myonat" & opt$region != "corsica") {
  cat("Filtering myonat area", fill = TRUE)
  print(dim(DataSpSL_w0_2))
  DataSpSL_w0_2_sf <- st_as_sf(DataSpSL_w0_2, # convert acoustic data to sf
    coords = c("longitude", "latitude"), crs = 4326, remove = FALSE
  )
  Myonat_area_path <- file.path(data_path, "GIS", "regions.gpkg") # load myonat area
  Myonat_area <- st_read(dsn = Myonat_area_path, layer = "nattereri") %>%
    st_as_sf()
  inside <- st_intersects(DataSpSL_w0_2_sf, Myonat_area, sparse = FALSE)[, 1] # Convert all data outside of area to 0
  DataSpSL_w0_2_sf$nb_contacts[!inside] <- 0
  DataSpSL_w0_2 <- st_drop_geometry(DataSpSL_w0_2_sf) %>%
    as.data.table()
  print(dim(DataSpSL_w0_2))
  print(names(DataSpSL_w0_2))
}
# Myocry
if (Sp_real == "Myocry") {
  cat("Filtering myocry area", fill = TRUE)
  print(dim(DataSpSL_w0_2))
  DataSpSL_w0_2_sf <- st_as_sf(DataSpSL_w0_2, # convert acoustic data to sf
    coords = c("longitude", "latitude"), crs = 4326, remove = FALSE
  )
  Myocry_area_path <- file.path(data_path, "GIS", "regions.gpkg") # load myocry area
  Myocry_area <- st_read(dsn = Myocry_area_path, layer = "crypticus") %>%
    st_as_sf()
  inside <- st_intersects(DataSpSL_w0_2_sf, Myocry_area, sparse = FALSE)[, 1] # Convert all data outside of area to 0
  DataSpSL_w0_2_sf$nb_contacts[!inside] <- 0
  Corsica_area <- st_read(dsn = Myocry_area_path, layer = "corsica") %>% # Remove data from Corsica
    st_as_sf()
  DataSpSL_w0_2_sf <- st_difference(DataSpSL_w0_2_sf, Corsica_area)
  DataSpSL_w0_2 <- st_drop_geometry(DataSpSL_w0_2_sf) %>%
    select(-ID) %>%
    as.data.table()
  print(dim(DataSpSL_w0_2))
  print(names(DataSpSL_w0_2))
}
# Myobly & Myomyo
if (Sp == "MyoGT" & opt$region != "corsica") {
  cat("Filtering myonat area", fill = TRUE)
  print(dim(DataSpSL_w0_2))
  DataSpSL_w0_2_sf <- st_as_sf(DataSpSL_w0_2, # convert acoustic data to sf
    coords = c("longitude", "latitude"), crs = 4326, remove = FALSE
  )
  Corsica_area_path <- file.path(data_path, "GIS", "regions.gpkg") # Load Corsica area
  Corsica_area <- st_read(dsn = Corsica_area_path, layer = "corsica") %>%
    st_as_sf()
  DataSpSL_w0_2_sf <- st_difference(DataSpSL_w0_2_sf, Corsica_area)
  DataSpSL_w0_2 <- st_drop_geometry(DataSpSL_w0_2_sf) %>% # Remove data from Corsica
    select(-ID) %>%
    as.data.table()
  print(dim(DataSpSL_w0_2))
  print(names(DataSpSL_w0_2))
}

DataSpSL_w0_2 <- DataSpSL_w0_2 |>
  dplyr::slice_max(order_by = nb_contacts, by = c(participation, Nuit))

# Exclude data with obvious wrong date (<2010)
DataSpSL_w0_2 <- DataSpSL_w0_2[which(DataSpSL_w0_2$Nuit > as.Date("2010-01-01")), ]

DataSpSL_w0_2$Nuit <- as.Date(DataSpSL_w0_2$Nuit)
DataSpSL_w0_2 <- unique(DataSpSL_w0_2)

CoordPS <- unique(CoordPS)
CoordPS$Nuit <- as.Date(CoordPS$Nuit)

DataSaison <- dplyr::left_join(DataSpSL_w0_2,
  CoordPS,
  by = c("longitude", "latitude", "Nuit", "participation")
) # adds environmental variables to activity data ("participation added")


# lets add the "gites" information
#
# data_gites <- read.csv2(file_gites)
#
# data_gites <- data_gites |>
#   dplyr::select(participation, Nuit, num_micro, indice_gite)
#
# data_gites$Nuit <- as.Date(data_gites$Nuit)
#
# DataSaison <- left_join(
#   DataSaison, data_gites,
#   by = c("participation", "Nuit", "num_micro", "espece")
# )
#
# DataSaison$indice_gite <- as.numeric(DataSaison$indice_gite)
# DataSaison$gite <- 0L
# # DataSaison$gite[is.na(test$indice_gite)]  <- 0
# DataSaison$gite[DataSaison$indice_gite > 0.5] <- 1L
#
# let’s remove data close to a potential colony
# DataSaison <- DataSaison[DataSaison$gite != 0L, ]


DataSaison$week <- as.integer(strftime(DataSaison$Nuit, format = "%V"))
DataSaison$day <- as.integer(strftime(DataSaison$Nuit, format = "%j"))
# DataSaison <- DataSaison[dplyr::between(DataSaison$week, p_start, p_end), ]

spring_start <- return_start("spring")
spring_end <- return_end("spring")
summer_start <- return_start("summer")
summer_end <- return_end("summer")
autumn_start <- return_start("autumn")
autumn_end <- return_end("autumn")

DataSaison <- DataSaison |> mutate(SpSaison = case_when(
  between(day, spring_start, spring_end) ~ "spring",
  between(day, summer_start, summer_end) ~ "summer",
  between(day, autumn_start, autumn_end) ~ "autumn"
))

DataSaison <- DataSaison |>
  tidyr::drop_na(SpSaison)


# DataSaison <- DataSaison[dplyr::between(DataSaison$fortnight, p_start, p_end), ]

# add date of year
if (grepl("/", DataSaison$Nuit[1L], fixed = TRUE)) {
  Date1 <- as.Date(substr(DataSaison$Nuit, 1L, 10L),
    format = "%Y/%m/%Y"
  )
} else {
  Date1 <- as.Date(DataSaison$Nuit)
}

SpFDate <- yday(Date1)
# DataSaison$SpCDate <- cos(SpFDate / 365L * 2L * pi) # to create a circular variable for date
# DataSaison$SpSDate <- sin(SpFDate / 365L * 2L * pi) # to create a circular variable for date

# If year effect must be accounted for
DataSaison$SpYear <- year(Date1)

print("summary of coordinates DataSaison")
print(summary(DataSaison$longitude))
print(summary(DataSaison$latitude))

DataSaison_sf <- sf::st_as_sf(
  DataSaison,
  coords = c(x = "longitude", y = "latitude"),
  crs = 4326L
) |>
  sf::st_transform(2154L)

coords <- as.data.frame(st_coordinates(DataSaison_sf))


# sf object with 5 points: the bounding box of the grid of points + the center
# Add material as predictor
# DataSaison$SpRecorder <- DataSaison$detecteur_enregistreur_type

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

if (opt$region == "idf") {
  ocs <- startsWith(Prednames, "SpHOCS")
  Prednames <- Prednames[!ocs]
}

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


# DataSaison$SpGite <- NULL

# select only one value per 500m square :
# ... add code here
# print("Keeping only one night per 500sq/15days")
# DataTest <- DataSaison[duplicated(DataSaison$code), ]
# DataSaison <- DataSaison[!duplicated(DataSaison$code), ]
# print("Rows in training dataset")
# print(nrow(DataSaison))

# print("Rows removed")
# print(nrow(DataTest))

# filtering excessive values
# quant <- quantile(DataSaison$nb_contacts, probs = 0.98)
# print(head(DataSaison[, "nb_contacts"]))

# DataSaison <- DataSaison[DataSaison$nb_contacts <= quant, ]

# moran <- check_moran(DataSaison, "nb_contacts")


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
    # ListSp[i],
    Sp_real,
    "_temp_sfolds.rds"
  )
) # quezaco?

cat("Load Area of Interest:", fill = TRUE)
aoi <- sf::read_sf(
  dsn = args[4],
  layer = opt$region
) |>
  st_transform(2154L)

cat("Prep data saison as sf object :", fill = TRUE)
DataSaison_sf <- st_as_sf(DataSaison,
  coords = c(x = "longitude", y = "latitude"),
  remove = FALSE,
  crs = 4326L
) |>
  st_transform(2154L)

DataSaison_sf <- DataSaison_sf[aoi, ]
# we want to filter out nights with bad meteoroligical conditions
DataSaison_sf <- DataSaison_sf |>
  dplyr::filter(total_precipitations < 2L)
DataSaison_sf <- DataSaison_sf |>
  dplyr::filter(Spwind < 4L)
DataSaison_sf <- DataSaison_sf |>
  dplyr::filter(dplyr::between(Sptemp, -4L, 4L))

# last_year <- max(DataSaison$SpYear)
if (opt$keep == TRUE) {
  DataTest_sf <- DataSaison_sf[DataSaison_sf$SpYear %in% test_years, ]
  DataSaison_sf <- DataSaison_sf[!DataSaison_sf$SpYear %in% test_years, ]
  DataTest <- DataTest_sf |>
    st_drop_geometry()

  acti_class <- def_classes_test(DataSaison_sf, DataTest)
  acticlass <- def_int_classes_test(DataSaison_sf, DataTest)

  DataSaison_sf$acti_class <- acti_class$train
  DataSaison_sf$acticlass <- acticlass$train

  DataTest$acti_class <- acti_class$test
  DataTest$acticlass <- acticlass$test
} else {
  DataSaison_sf$acti_class <- def_classes(DataSaison_sf)
  DataSaison_sf$acticlass <- def_int_classes(DataSaison_sf)
}


print("DataSaison filtered for season")

print("saisons avant filtre")
print(unique(DataSaison_sf$SpSaison))

DataSaison_sf <- DataSaison_sf[dplyr::between(DataSaison_sf$day, p_start, p_end), ]
if (opt$keep == TRUE) {
  DataTest <- DataTest[dplyr::between(DataTest$day, p_start, p_end), ]
}
# DataSaison_sf <- subset(DataSaison_sf, DataSaison_sf$SpSaison == opt$period)

print("saisons apres filtre")
print(unique(DataSaison_sf$SpSaison))

# DataSp <- subset(DataCPL3, DataCPL3$espece == ListSp[i]) # subset species
# DataSp <- subset(DataCPL3, DataCPL3$espece == Sp) # subset species
DataSaison_sf <- filter_by_median_season_grid(DataSaison_sf, opt$region)

DataSaison <- DataSaison_sf

DataSaison_sf <- st_as_sf(DataSaison_sf,
  coords = c(x = "longitude", y = "latitude"),
  remove = FALSE,
  crs = 4326L
) |>
  st_transform(2154L)

set.seed(123)

START <- Sys.time()

cat("Creating folds :", fill = TRUE)

sfolds <- CAST::knndm(DataSaison_sf, aoi, k = 10, maxp = 0.5) # k = number of folds
END <- Sys.time()
print(END - START) # 1 to 1.4 hours
# beep(2)
saveRDS(sfolds, sfolds_source)
print("sfolds written")


DataSaison$sfold <- sfolds$clusters

sindx <- CreateSpacetimeFolds(DataSaison,
  spacevar = "sfold",
  class = "acti_class",
  ## timevar = "fortnight",
  k = 10L
)

sctrl <- caret::trainControl(
  method = "cv",
  index = sindx$index,
  savePredictions = "final"
)

cat("Cross-validation indices prepared", fill = TRUE)
if (opt$keep == TRUE) {
  write.csv(
    DataTest,
    file.path(
      Output,
      paste0(
        # ListSp[i], "_", opt$period, "_", opt$region, "_", ThresholdSort, "_datatest.csv"
        Sp_real, "_", opt$period, "_", opt$region, "_", ThresholdSort, "_datatest.csv"
      )
    )
  )
}

print("a)")

DataSaison$acti_class <- factor(DataSaison$acti_class, levels = c("NoAct", "Faible", "Moyen", "Fort", "TresFort"))

samp_sizes <- def_sample_vector(DataSaison, "acti_class", 0.66)
# DataSaison$SpRoAddM <- DataSaison$SpRo1M + DataSaison$SpRo2M +
#   DataSaison$SpRo3M + DataSaison$SpRo4M
#

print("b)")

if (selection == "PCA") {
  cat("selection : PCA", fill = TRUE)
  predictors <- DataSaison[, variables_acp]


  acp <- get_components(predictors, "PCA")
  acp_pc_vars <- acp$components

  # saveRDS(acp$acp, file.path(Output, paste0("acp_PCA_", ListSp[i], "_", opt$period, ".rds")))
  saveRDS(acp$acp, file.path(Output, paste0("acp_PCA_", Sp_real, "_", opt$period, ".rds")))


  DataSaison <- cbind(DataSaison, acp_pc_vars)

  vars_names <- names(acp_pc_vars)
  Prednames <- c(vars_names, vars_norm)
} else if (selection == "PCAdecomp") {
  cat("selection : PCA decomposée", fill = TRUE)

  small_vars <- endsWith(names(DataSaison), "S")
  small_vars <- names(DataSaison)[small_vars]

  data <- DataSaison |>
    select(!all_of(small_vars))


  occsol_vars <- startsWith(names(data), "SpHOCS")
  occsol_vars <- names(data)[occsol_vars]

  bioclim_vars <- startsWith(names(data), "SpBioC") # ajouter météo ? saison binaire ?
  bioclim_vars <- names(data)[bioclim_vars]

  names_data <- names(data)
  names_data <- names_data[!(names_data %in% bioclim_vars)]
  names_data <- names_data[!(names_data %in% occsol_vars)]

  other_vars <- startsWith(names_data, "Sp")


  other_vars <- names_data[other_vars]
  other_vars <- other_vars[!(other_vars %in% c("SpSaison", "SpAltiM"))]


  predictors_occs <- data[, occsol_vars]
  predictors_bioc <- data[, bioclim_vars]
  predictors_other <- data[, other_vars]

  bioclim <- get_components(predictors_bioc, "bioclim")
  bioclim_pc_vars <- bioclim$components

  occsol <- get_components(predictors_occs, "occsol")
  occsol_pc_vars <- occsol$components

  others <- get_components(predictors_other, "autres")
  other_pc_vars <- others$components

  # saveRDS(bioclim$acp, file.path(Output, paste0("acp_bioclim_", ListSp[i], "_", opt$period, ".rds")))
  # saveRDS(occsol$acp, file.path(Output, paste0("acp_occsol_", ListSp[i], "_", opt$period, ".rds")))
  # saveRDS(others$acp, file.path(Output, paste0("acp_autres_", ListSp[i], "_", opt$period, ".rds")))
  saveRDS(bioclim$acp, file.path(Output, paste0("acp_bioclim_", Sp_real, "_", opt$period, ".rds")))
  saveRDS(occsol$acp, file.path(Output, paste0("acp_occsol_", Sp_real, "_", opt$period, ".rds")))
  saveRDS(others$acp, file.path(Output, paste0("acp_autres_", Sp_real, "_", opt$period, ".rds")))

  vars <- cbind(occsol_pc_vars, bioclim_pc_vars, other_pc_vars)
  DataSaison <- cbind(DataSaison, vars)
  Prednames <- names(vars)
} else if (selection == "VSURF") {
  cat("selection : VSURF", fill = TRUE)
  selected_index <- get_prednames(DataSaison, Prednames, "acti_class", samp_sizes)
  Prednames <- Prednames[selected_index]
} else if (selection == "bio") {
  Prednames <- c(variables_bio, vars_norm)
} else if (selection == "indisp") {
  Prednames <- variables_indisp
}

print("c)")

if ("geometry" %in% colnames(DataSaison)) {
  DataSaison <- DataSaison |>
    select(-geometry)
}

if (opt$period == "year") {
  # setting binary season variables
  DataSaison <- DataSaison |>
    dplyr::mutate(SpSpring = dplyr::if_else(SpSaison == "spring", 1, 0)) |>
    dplyr::mutate(SpSummer = dplyr::if_else(SpSaison == "summer", 1, 0)) |>
    dplyr::mutate(SpAutumn = dplyr::if_else(SpSaison == "autumn", 1, 0))

  Prednames <- c(Prednames, "SpSpring", "SpSummer", "SpAutumn", "SpAltiM")
}

write.csv(
  DataSaison,
  file.path(
    Output,
    paste0(
      # ListSp[i], "_", opt$period, "_", opt$region, "_datatrain.csv"
      Sp_real, "_", opt$period, "_", opt$region, "_datatrain.csv"
    )
  ),
  row.names = FALSE
)

print("d)")

if (activite == "nbcontacts") {
  activite <- "nb_contacts"
}

noSpacemod <- fitvalpred_rf(
  Prednames,
  activite,
  # rctrl,
  sctrl,
  DataSaison
  # tempstack[[c(basecovs, proxycovs)]]
)

print("Model done")

#### Save ####----------------------------------------------------------------

# suffix <- paste0(opt$period, "_", opt$region, "_noSpace", "_", ListSp[i])
suffix <- paste0(opt$period, "_", opt$region, "_noSpace", "_", Sp_real)

write.csv(
  noSpacemod$tab,
  file.path(
    Output,
    paste0(
      "Evaluation_",
      # ListSp[i],
      Sp_real,
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

saveRDS(
  noSpacemod$spatmod,
  file.path(
    Output,
    paste0(
      "RFspat_",
      # ListSp[i],
      Sp_real,
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
# print(paste("Model done for", ListSp[i]))
print(paste("Model done for", Sp_real))

# parallel::stopCluster(cl)
# }


## print(ListSp[i])
## print(ThresholdSort)
## if (DoBoruta) {
##   print(paste0("Variables before selection = ", length(Predictors)))
##   print(paste0("Variables after selection = ", length(names.Boruta)))
## }
