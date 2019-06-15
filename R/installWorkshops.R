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

.getIssues <-
    function(repository, location_url = "https://api.github.com/repos/") {
    jsonlite::fromJSON(
        paste0(location_url, repository, "/issues")
    )
}

.selectWorkshopElements <- function(issueList) {
    title <- issueList[["title"]]
    if (is.null(title))
        stop("<internal> Include issue list with title")
    isWorkshop <- grepl("[Workshop]", title, fixed = TRUE)
    lapply(issueList, function(x) x[isWorkshop])
}

.setRepoInBody <- function(issueList, schemes = "https") {
    issueBodies <- issueList[["body"]]
    bodies <- strsplit(issueBodies, "\\s")
    urlidx <- vapply(bodies, function(x) which(grepl(schemes, x))[[1L]], integer(1L))
    repos <- mapply(function(x, y) x[y], bodies, urlidx)
    issueList[["body"]] <- gsub("\\.git", "", repos)
    issueList
}

.readIssues <-
    function(repository, location_url = "https://api.github.com/repos/") {
    issues <- .getIssues(repository, location_url)
    issues <- issues[c("body", "title")]
    issues <- .selectWorkshopElements(issues)
    issues <- .setRepoInBody(issues)
    .repobranch(issues[["body"]])
}

.warnNoDESC <- function(branchdf) {
    validPKGS <- .checkDESC(branchdf)
    invalid <- branchdf[!validPKGS, "repos"]
    if (length(invalid))
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

.installIssues <- function(rebranch, local) {
    ## check that DESCRIPTION file is there
    validPKGS <- .checkDESC(rebranch)
    builddir <- file.path(local, "buildout")
    if (!dir.exists(builddir))
        dir.create(builddir)
    apply(rebranch[validPKGS, , drop = FALSE], 1L, function(x) {
        try({
            BiocManager::install(x[[1L]], ref = x[[2L]],
                build_opts = c("--no-resave-data", "--no-manual"),
                dependencies = TRUE)
            }, outFile = file.path(builddir, paste0(basename(x[[1L]]), ".out"))
        )
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
        local = workshopbuilder:::.options$get("REPOS_PATH"),
        location = "https://api.github.com/repos/")
{
    rebranch <- .readIssues(repository, location)
    cloneIssueRepos(rebranch, local)
    .installIssues(rebranch, local)
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

cloneIssueRepos <-
    function(
        repos,
        local = workshopbuilder:::.options$get("REPOS_PATH")
    )
{
    if (!dir.exists(local))
        dir.create(local, recursive = TRUE)
    urlStart <- "https://github.com"
    apply(repos, 1L, function(x) {
        local_repo <- file.path(local, basename(x[[1L]]))
        if (!dir.exists(local_repo))
            git2r::clone(url = file.path(urlStart, x[[1L]]),
                local_path = local_repo, branch = x[[2]])
    })
}


#' @export
getWorkshops <-
    function(repository = workshopbuilder:::.options$get("BOOK_REPO"),
        location="https://api.github.com/repos/")
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


