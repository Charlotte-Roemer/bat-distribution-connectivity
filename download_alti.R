library(xml2)
library(rvest)
library("archive")


url_bd_alti <- "https://geoservices.ign.fr/bdalti"
path <- "/home/tsevere/Documents/mnhn/data/bdalti"
dl_path <- file.path(path, "temp")

if (dir.exists(dl_path)) {
  cat("Download path already exists", fill = TRUE)
} else {
  dir.create(dl_path)
}
page <- read_html(url_bd_alti)

links <- html_attr(html_nodes(page, "a"), "href")

i <- grep("data.geopf", links)

bd_alti_links <- links[i]

i <- grep("LAMB93", links)

bd_alti_links <- links[i]

dl <- function(link, path, overwrite = TRUE) {
  filename <- basename(link)
  file_path <- file.path(path, filename)
  files <- list.files(path, full.names = TRUE)

  if (!overwrite && file_path %in% files) {
    NA
  } else {
    cat(link, fill = TRUE)
    command <- paste0(
      "wget ", " -N ", link, " -P ", path, " -nv"
    )
    print(command)
    system(command)
  }
}


lapply(bd_alti_links, dl, path = dl_path, overwrite = FALSE)

zip_files <- list.files(dl_path, pattern = "7z", full.names = TRUE)

lapply(zip_files, archive::archive_extract, dir = path)

a <- readline("Do you want to remove temp downloaded files ?\n1:yes\n2:no\n")

if (!(a %in% c(1, 2))) {
  cli::cli_inform("please answer 1 or 2")
} else if (a == 1) {
  unlink(dl_path, recursive = TRUE)
} else if (a == 2) {
  cli::cli_inform(paste0("Folder ", dl_path, " and its content will stay on disk"))
}
