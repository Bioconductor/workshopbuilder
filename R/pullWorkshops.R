#' Take workshops from locations
#'
#' This will pull the appropriate Rmd files from the main Workshop book
#' repository and generate a bookdown.yml file
#'
#' @param repository
#'
pullWorkshops <- function(repository, location="github", branch="master") {
    baseurl <- switch(location, github = "https://raw.githubusercontent.com")
    desc <- file.path(baseurl, repository, branch, "DESCRIPTION")
    tf <- tempfile()
    download.file(desc, tf)
    dcf <- read.dcf(tf, "Remotes")
    remotes <- trimws(strsplit(dcf, "[, \n]+")[[1]])
## include rmd_files from _bookdown.yml
## wip:    file.path(baseurl, branch, remotes, "vignettes")
}
