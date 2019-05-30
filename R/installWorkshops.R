.repobranch <- function(remotes) {
    hasBranch <- grepl("tree", remotes, fixed = TRUE)
    branches <- basename(remotes[hasBranch])
    bremotes <- gsub("\\/tree.*", "", remotes)
    bremotes <- file.path(basename(dirname(bremotes)), basename(bremotes))
    data.frame(repos = bremotes, refs = ifelse(hasBranch, branches, "master"))
}

.getRemotesOnline <- function(repository, location, branch) {
    baseurl <- switch(location, github = "https://raw.githubusercontent.com")
    desc <- file.path(baseurl, repository, branch, "DESCRIPTION")
    tf <- tempfile()
    download.file(desc, tf)
    .readRemotes(tf)
}

.readRemotes <- function(descfile) {
    dcf <- read.dcf(descfile, "Remotes")
    trimws(strsplit(dcf, "[, \n]+")[[1]])
}

.readIssues <- function(repository, location) {
    location <- switch(location, github = "https://api.github.com/repos/",
        stop("Non-GitHub locations not yet supported"))
    issues <- jsonlite::fromJSON(
        paste0(location, repository, "/issues")
    )
    iswork <- grepl("[Workshop]", issues$title, fixed = TRUE)
    issues <- lapply(issues, function(g) g[iswork])
    bodies <- strsplit(issues$body, "\\s")
    urlidx <- vapply(bodies, function(x) which(grepl("https", x))[[1L]], integer(1L))
    repos <- mapply(function(x, y) x[y], bodies, urlidx)
    repos <- gsub("\\.git", "", repos)
    .repobranch(repos)
}

## git2r::clone repository first and get local location
.addRemotes <- function(local_repo, reposrefs) {
    remotes <- apply(reposrefs, 1L, function(line) {
        if (!identical(line[[2]], "master"))
            paste(line[[1]], line[[2]], sep = "@")
        else
            line[[1]]
    })
    desc::desc_add_remotes(remotes, local_repo)
}

.addImports <- function(local_repo, reposrefs) {
    pkgNames <- basename(reposrefs)
    insivisible(
        lapply(pkgNames, function(pkg) {
            desc::desc_set_dep(pkg, local_repo)
        })
    )
}

.installIssues <- function(repository, location) {
    rebranch <- .readIssues(repository, location)
    apply(rebranch, 1L, function(x) {
        remotes::install_github(repo = x[[1]], ref = x[[2]])
    })
}

#' Install workshops from locations as indicated by main repository
#'
#' This function looks at the 'issues' page of the main repository and installs
#' from the workshop URLs tagged as `[Workshop]`
#   '
#' @param repository A single string indicating the 'username/repository' of the
#'   main book-building GitHub repository (defaults to package option
#'   'BOOK_REPO')
#' @param location Website location of the main repository (default 'github')
#'
#' @export
installWorkshops <-
    function(repository = workshopbuilder:::.options$get("BOOK_REPO"),
        location="github")
{
    .installIssues(repository, location)
}

#' @export
cloneBookRepo <-
    function(
        repository = workshopbuilder:::.options$get("BOOK_REPO"),
        local = workshopbuilder:::.options$get("LOCAL_REPO")
    )
{
    git2r::clone(repository, local)
    if (!dir.exists(dirname(local)))
        dir.create(dirname(local), recursive = TRUE)
    bookloc <- file.path(local, repository)
    workshopbuilder:::.options$set("LOCAL_REPO", bookloc)
    bookloc
}

#' @export
transferVignettes <- function(remotes) {
    bookloc <- workshopbuilder:::.options$get("LOCAL_REPO")
    pkgNames <- basename(remotes)
    vigfiles <- vapply(pkgNames, function(pkg) {
        instLoc <- system.file(package = pkg)
        list.files(instLoc, pattern = ".[Rr][Mm}[Dd]", full.names = TRUE)
    }, character(1L))
    file.copy(vigfiles, to = bookloc)
}

