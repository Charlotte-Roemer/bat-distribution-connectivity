#------------------------------------------------------------------------------#
#                      Function to prepare data                                #
#------------------------------------------------------------------------------#

prepare_data <- function(args, fpar, fsl) {
  library(dplyr)
  # Read bat activity data
  cat("Reading data", fill = TRUE)
  data_cpl2 <- data.table::fread(paste0(args[1L], ".csv")) # SpNuit2
  data_cpl2$Nuit <- as.Date(data_cpl2$Nuit)

  data_cpl3 <- dplyr::filter(data_cpl2, Nuit < args[11L])
  rm(data_cpl2)

  parti_nuit <- unique(
    dplyr::select(
      data_cpl3,
      c("participation", "Nuit")
    )
  )

  # Read predictor table :
  coord_sig <- data.table::fread(paste0(args[2L], ".csv")) # GI_FR_sites_loc (variables)

  coord_sig <- rename(coord_sig,
    longitude = args[12L],
    latitude = args[13L]
  )

  # cleaning data in case duplicated columns remains :
  coord_sig <- coord_sig %>%
    rename_all(~ str_replace_all(., "\\.x", ""))
  coord_sig <- coord_sig %>%
    select(-contains(".y"))

  # Read participation and locality data
  cat("Reading participations...", fill = TRUE)
  particip <- readr::read_delim(fpar, delim = ";") # p_export.csv
  cat("Reading locations...", fill = TRUE)
  site_loc <- data.table::fread(fsl) # sites_localites.txt

  # Identifies sites recorded near bat roosts !!! Remove these sites ???
  cat("Identifying shelters", fill = TRUE)
  Gite <- mapply(
    function(x, y) {
      ((grepl(paste0(y, "="), x)) | (grepl(paste0(y, " ="), x)))
    },
    site_loc$commentaire,
    site_loc$localite
  )
  site_loc$SpGite <- as.numeric(Gite)
  site_loc <- site_loc %>%
    mutate_at(
      .vars = c("longitude", "latitude"),
      .fun = function(x) as.numeric(gsub(",", "\\.", x, fixed = TRUE))
    )

  # List coordinates existing in bat activity data to help add 0 in nb_contacts later
  cat("Listing unique locations", fill = TRUE) # Là il faut peut-être discuter
  list_par <- levels(as.factor(data_cpl3$participation))
  sel_par <- subset(particip, particip$participation %in% list_par)
  sel_par <- unique(sel_par)
  site_loc <- unique(site_loc)
  sel_par <- merge(sel_par, parti_nuit, by = "participation", all.x = TRUE)
  cat("Merging Site locations and participations", fill = TRUE)
  sel_par_sl <- merge(site_loc, sel_par, by.x = c("site", "nom"), by.y = c("site", "point"))

  cat("Merge done", fill = TRUE)
  coord_par <- aggregate(sel_par_sl$participation,
    by = c(
      list("longitude" = sel_par_sl$longitude),
      list("latitude" = sel_par_sl$latitude),
      list("Nuit" = sel_par_sl$Nuit),
      list("participation" = sel_par_sl$participation)
    ),
    FUN = length
  )
  coord_par$x <- NULL

  # Merge list of coordinates with environmental variables
  cat("Merging Coordinates")
  cat("coord_par")
  print(colnames(coord_par))
  cat("coord_sig")
  print(colnames(coord_sig))
  coord_ps <- merge(coord_par,
    coord_sig,
    by = c("longitude", "latitude", "Nuit")
  )
  cat("Merged")
  coord_ps[is.na(coord_ps)] <- 0
  testPar <- grepl(args[6L], names(coord_ps))
  cat("Subseting data", fill = TRUE)
  numPar <- subset(c(1L:length(testPar)), testPar)
  cat("data subseted", fill = TRUE)
  cat(numPar[1L], fill = TRUE)
  coord_ps$participation <- as.data.frame(coord_ps)[, numPar[1L]]

  cat("Done... ready to return data")
  return(list(
    coord_ps, # environmental variables
    data_cpl3, # bat activity (without absence data)
    sel_par_sl # list of sampling sessions to know when to add absence data
  ))
}
