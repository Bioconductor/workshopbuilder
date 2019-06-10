.repobranch <- function(remotes) {
    hasBranch <- grepl("tree", remotes, fixed = TRUE)
    branches <- basename(remotes[hasBranch])
    bremotes <- gsub("\\/tree.*", "", remotes)
    bremotes <- file.path(basename(dirname(bremotes)), basename(bremotes))
    data.frame(repos = bremotes, refs = ifelse(hasBranch, branches, "master"),
        stringsAsFactors = FALSE)
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

.readIssues <- function(repository, location = "github") {
    location <- switch(location, github = "https://api.github.com/repos/",
        stop("Non-GitHub locations not yet supported"))
    issues <- jsonlite::fromJSON(
        paste0(location, repository, "/issues")
    )
    issues <- issues[c("body", "title")]
    iswork <- grepl("[Workshop]", issues[["title"]], fixed = TRUE)
    issues <- lapply(issues, function(g) g[iswork])
    bodies <- strsplit(issues[["body"]], "\\s")
    urlidx <- vapply(bodies, function(x) which(grepl("https", x))[[1L]], integer(1L))
    repos <- mapply(function(x, y) x[y], bodies, urlidx)
    repos <- gsub("\\.git", "", repos)
    .repobranch(repos)
}

.warnNoDESC <- function(branchdf) {
    validPKGS <- .checkDESC(branchdf)
    invalid <- branchdf[!validPKGS, "repos"]
    warning(
        "Repositories without a valid DESCRIPTION file:\n",
        paste(paste0("  ", invalid), collapse = ",\n")
    )
    branchdf[validPKGS, , drop = FALSE]
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

.addImports <- function(local_repo, repos) {
    pkgNames <- basename(repos)
    descfile <- file.path(local_repo, "DESCRIPTION")
    invisible(
        lapply(pkgNames, function(pkg) {
            desc::desc_set_dep(pkg, type = "Imports", file = descfile)
        })
    )
}

.checkDESC <- function(branchdf) {
    apply(branchdf, 1L, function(x) {
        RCurl::url.exists(
        file.path("https://github.com", x[[1L]], "tree", x[[2L]], "DESCRIPTION")
        )
    })
}

.installIssues <- function(repository, location) {
    rebranch <- .readIssues(repository, location)
    ## check that DESCRIPTION file is there
    validPKGS <- .checkDESC(rebranch)
    apply(rebranch[validPKGS, , drop = FALSE], 1L, function(x) {
        repo <- paste(x[[1]], x[[2]], sep = "@")
        BiocManager::install(repo, ask = FALSE, build_vignettes = TRUE)
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
    if (!dir.exists(local))
        dir.create(local, recursive = TRUE)
    urlStart <- "https://github.com"
    git2r::clone(file.path(urlStart, repository), local)
    current <- workshopbuilder:::.options$get("LOCAL_REPO")

    if (!identical(local, current))
        workshopbuilder:::.options$set("LOCAL_REPO", local)

    workshopbuilder:::.options$get("LOCAL_REPO")
}

#' @export
getWorkshops <-
    function(repository = workshopbuilder:::.options$get("BOOK_REPO"),
        location="github")
{
    reposREF <- .readIssues(repository = repository, location = location)
    .warnNoDESC(reposREF)
}

#' @export
addWorkshops <-
    function(reposREF,
        local = workshopbuilder:::.options$get("LOCAL_REPO")
    )
{
        reposREF <- rbind.data.frame(reposREF,
            data.frame(repos="rstudio/bookdown", refs="master"))
        reposinREF <- paste0(
            reposREF[[1L]],
            ifelse(reposREF[[2L]] == "master", "", paste0("@", reposREF[[2]]))
        )
        remotes <- .readRemotes(file.path(local, "DESCRIPTION"))
        newremotes <- !(reposinREF %in% remotes)
        if (any(newremotes)) {
            reposREF <- reposREF[newremotes, , drop = FALSE]
            .addRemotes(local, reposREF)
            repoNames <- basename(reposREF[[1L]])
            .addImports(local, repoNames)
        }
        desc::desc(file = local)
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

