library(dplyr)
# Set data folder location :
data_vc <- "/home/bbk9/Documents/mnhn/data/Vigie-Chiro"

# Observations
obs_file <- file.path(data_vc, "SpNuit2_0_DataLP_PF_exportTot.csv")
obs <- utils::read.csv(obs_file)
night <- subset(obs, select = c("participation", "Nuit"))
unique_nights <- unique(night)

# Users
users_file <- file.path(data_vc, "utilisateurs.txt")
users <- utils::read.delim(users_file, sep = "\t")
users <- subset(users, select = c("identifiant", "confidentiel"))

# Locations
loc_file <- file.path(data_vc, "sites_localites.txt")
loc <- utils::read.delim(loc_file, sep = "\t")
loc <- subset(loc, select = c(
  "id_site",
  "site",
  "nom",
  "id_protocole",
  "latitude",
  "longitude"
))

# Participations
parti_file <- file.path(data_vc, "p_export_forLinux.csv")
parti <- utils::read.csv2(parti_file)
parti <- subset(parti,
  select = c(
    "participation",
    "idobservateur",
    "idsite",
    "point"
  )
)

# Adding users informations to participations :
parti <- parti %>% left_join(users, by = c("idobservateur" = "identifiant"))


# Selecting only non-confidential participations :
parti_ok <- parti[parti$confidentiel != "oui", ]

# Getting location for non-confidential participations only :
parti_ok_loc <- dplyr::inner_join(parti_ok,
  loc,
  by = c("idsite" = "id_site", "point" = "nom")
)

# Adding location info to unique non-confidential nights :
unique_nights_ok_loc <- dplyr::inner_join(unique_nights, parti_ok_loc,
  by = "participation"
)

# Replacing french decimal separator "," with "." for later analysis :
unique_nights_ok_loc$latitude <- gsub(",", ".", unique_nights_ok_loc$latitude)
unique_nights_ok_loc$longitude <- gsub(",", ".", unique_nights_ok_loc$longitude)

# Saving unique nights to use for training Random Forest :
write.csv2(unique_nights_ok_loc, file.path(
  data_vc,
  "parti_unique_non_confi.csv"
))
