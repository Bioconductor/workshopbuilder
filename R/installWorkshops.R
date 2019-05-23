.branch <- function(remotes) {
    hasBranch <- grepl("tree", remotes, fixed = TRUE)
    branches <- basename(remotes[hasBranch])
    bremotes <- gsub("\\/tree.*", "", remotes)
    bremotes <- file.path(basename(dirname(bremotes)), basename(bremotes))
    data.frame(repos = bremotes, branches = ifelse(hasBranch, branches, "master"))
}

.readIssues <- function(repository) {
    issues <- fromJSON(
        paste0("https://api.github.com/repos/", repository, "/issues")
    )
    iswork <- grepl("[Workshop]", issues$title, fixed = TRUE)
    issues <- lapply(issues, function(g) g[iswork])
    bodies <- strsplit(issues$body, "\\s")
    urlidx <- vapply(bodies, function(x) which(grepl("https", x))[[1L]], integer(1L))
    repos <- mapply(function(x, y) x[y], bodies, urlidx)
    repos <- gsub("\\.git", "", repos)
    .branch(repos)
}

#' Install workshops from locations as indicated by main repository
#'
#' This function looks at the 'issues' page of the main repository and installs
#' from the workshop URLs tagged as `[Workshop]`
#'
#' @param repository A single string indicating the 'username/repository' of the
#'   GitHub repository
#' @param location Website location of the main repository (default 'github')
#' @param branch Indicates the branch from where to pull the DESCRIPTION file
#'   (default 'master')
#'
#' @export
installWorkshops <- function(repository, location="github", branch="master") {
    baseurl <- switch(location, github = "https://raw.githubusercontent.com")
    desc <- file.path(baseurl, repository, branch, "DESCRIPTION")
    tf <- tempfile()
    download.file(desc, tf)
    dcf <- read.dcf(tf, "Remotes")
    remotes <- trimws(strsplit(dcf, "[, \n]+")[[1]])
    rebranch <- .readIssues(remotes)
    apply(rebranch, 1L, function(x) {
        remotes::install_github(x[[1]], x[[2]])
    })
}
