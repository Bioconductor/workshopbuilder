.repobranch <- function(remotes) {
    remotes <- remotes[["location"]]
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
    function(repository, location_url = "https://api.github.com/repos") {
    endpoint <- file.path(location_url, repository, "issues")
    ## assume there are only two pages
    unlist(lapply(1:2, function(n)
        httr::content(httr::GET(paste0(endpoint, "?page=", n)))
    ), recursive = FALSE)
}

.selectWorkshopElements <- function(issueFrame) {
    title <- issueFrame[["title"]]
    if (is.null(title))
        stop("<internal> Include issue data.frame with 'title' column")
    isWorkshop <- grepl("[Workshop]", title, fixed = TRUE)
    issueFrame[isWorkshop, ]
}

.setRepoMetadata <- function(issueFrame, schemes = "https") {
    issueBodies <- issueFrame[["body"]]
    bodies <- strsplit(issueBodies, "\\s")
    urlidx <- vapply(bodies, function(x)
        which(grepl(schemes, x))[[1L]], integer(1L))
    repos <- mapply(function(x, y) x[y], bodies, urlidx)
    issueFrame[["location"]] <- gsub("\\.git", "", repos)
    issueFrame[["repository"]] <- basename(issueFrame[["location"]])
    issueFrame[["owner"]] <- basename(dirname(issueFrame[["location"]]))

    remotes <- issueFrame[["location"]]
    hasBranch <- grepl("tree", remotes, fixed = TRUE)
    branches <- basename(remotes[hasBranch])
    bremotes <- gsub("\\/tree.*", "", remotes)

    issueFrame[["repoowner"]] <-
        file.path(basename(dirname(bremotes)), basename(bremotes))
    issueFrame[["refs"]] <- ifelse(hasBranch, branches, "master")
    issueFrame
}

.readIssues <-
    function(repository, location_url = "https://api.github.com/repos",
        fields = c("body", "title", "number")) {
    issues <- .getIssues(repository, location_url)
    issues <- do.call(function(...) {
        rbind.data.frame(..., stringsAsFactors = FALSE) },
        lapply(issues, `[`, fields)
    )
    issues <- .selectWorkshopElements(issues)
    .setRepoMetadata(issues)
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

.installIssues <- function(repos_data, local, ...) {
    builddir <- file.path(local, "buildout")
    if (!dir.exists(builddir))
        dir.create(builddir)
    apply(repos_data, 1L, function(x) {
        capture.output({
            tryCatch({
                x[["build"]] <- TRUE
                BiocManager::install(x[["repoowner"]], ref = x[["refs"]],
                    build_opts = c("--no-resave-data", "--no-manual"),
                    dependencies = TRUE, build_vignettes = TRUE, ask = FALSE,
                    ...)
                }, error = function(e) {
                    warning("Unable to install package: ", x[[1L]],
                        "\n", conditionMessage(e))
                    x[["build"]] <- FALSE
                })
            }, file = file.path(builddir, paste0(basename(x[[1L]]), ".out")),
            type = "output"
        )
        x
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
        location = "https://api.github.com/repos",
        ncpus = getOption("Ncpus", 1L), ...)
{
    on.exit(options(Ncpus = options("Ncpus")))
    options(Ncpus = ncpus)
    remotes <- .readIssues(repository, location)
    getIssueRepos(remotes, local)
    .installIssues(rebranch, local, ...)
}

#' @export
cloneBookRepo <-
    function(
        repository = workshopbuilder:::.options$get("BOOK_REPO"),
        local_repo = workshopbuilder:::.options$get("LOCAL_REPO")
    )
{
    if (!dir.exists(local_repo))
        dir.create(local_repo, recursive = TRUE)
    urlStart <- "https://github.com"
    git2r::clone(file.path(urlStart, repository), local_repo)
    current <- workshopbuilder:::.options$get("LOCAL_REPO")

    if (!identical(local, current))
        workshopbuilder:::.options$set("LOCAL_REPO", local)

    workshopbuilder:::.options$get("LOCAL_REPO")
}

#' @export
getIssueRepos <-
    function(
        repos,
        repos_path = workshopbuilder:::.options$get("REPOS_PATH")
    )
{
    if (!dir.exists(local))
        dir.create(local, recursive = TRUE)
    apply(repos, 1L, function(x) {
        local_repo <- file.path(repos_path, x[["repository"]])
        if (!dir.exists(local_repo))
            git2r::clone(url = x[["location"]],
                local_path = local_repo, branch = x[["refs"]])
        else
            git2r::pull(repo = local_repo)
    })
}


#' @export
getWorkshops <-
    function(repository = workshopbuilder:::.options$get("BOOK_REPO"),
        location="https://api.github.com/repos")
{
    remotes <- .readIssues(repository = repository, location = location)
    reposREF <- .repobranch(remotes)
    .warnNoDESC(reposREF)
}

#' @export
addWorkshops <-
    function(reposREF,
        local_repo = workshopbuilder:::.options$get("LOCAL_REPO")
    )
{
        reposREF <- rbind.data.frame(reposREF,
            data.frame(repos="rstudio/bookdown", refs="master"))
        reposinREF <- paste0(
            reposREF[[1L]],
            ifelse(reposREF[[2L]] == "master", "", paste0("@", reposREF[[2]]))
        )
        remotes <- .readRemotes(file.path(local_repo, "DESCRIPTION"))
        newremotes <- !(reposinREF %in% remotes)
        if (any(newremotes)) {
            reposREF <- reposREF[newremotes, , drop = FALSE]
            .addRemotes(local_repo, reposREF)
            repoNames <- basename(reposREF[[1L]])
            .addImports(local_repo, repoNames)
        }
        desc::desc(file = local_repo)
}

#' @export
transferVignettes <-
    function(
        remotes, local_repo = workshopbuilder:::.options$get("LOCAL_REPO"),
        repo_path = workshopbuilder:::.options$get("REPOS_PATH")
    )
{
    ## build results add a "build" TRUE / FALSE column
    remotes <- remotes[remotes[["build"]], ]
    pkgNames <- remotes[["repository"]]
    vigfiles <- vapply(pkgNames, function(pkg) {
        vigloc <- file.path(repo_path, pkg, "vignettes")
        list.files(vigloc, pattern = "\\.[Rr][Mm][Dd]", full.names = TRUE)
    }, character(1L))

    lapply(vigfiles, function(file) {
        viglines <- readLines(file)
        newVig <- file.path(local_repo, file)
        linerange <- seq(min(grep("^#\\s*\\w+", viglines)), length(viglines))
        writeLines(viglines[linerange], con = newVig)
    })
}

#' @export
getStatus <- function(local = workshopbuilder:::.options$get("REPOS_PATH"),
    buildFolder = "buildout") {
    outReport <- file.path(local, buildFolder)
    reportFiles <- list.files(outReport, full.names = TRUE, pattern = ".out")
    shopnames <- gsub("\\.out", "", basename(reportFiles))
    reportFiles <- setNames(reportFiles, shopnames)
    lapply(reportFiles, function(txt) {
        lines <- trimws(readLines(txt))
        lines[nchar(lines) != 0L]
    })
}

#' @export
postStatus <- function(repository = workshopbuilder:::.options$get("BOOK_REPO"),
    local = workshopbuilder:::.options$get("REPOS_PATH"),
    buildFolder = "buildout") {
    statList <- Filter(length, getStatus(local, buildFolder))
    if (!length(statList))
        stop("No install '.out' files found in directory:\n ",
            file.path(local, buildFolder), "\n 'installWorkshops()' first")
    remotes <- .readIssues(repository)
    buildout <- remotes[["repository"]] %in% names(statList)
    remotes <- lapply(remotes, `[`, buildout)
    remotes[["buildout"]] <-
        statList[match(names(statList), remotes[["repository"]])]
    len <- seq_len(unique(lengths(remotes)))
    for (i in len) {
        workshop <- lapply(remotes, `[`, i)
        issuenumber <- workshop[["number"]]
        feedback <- unname(unlist(workshop[["buildout"]]))
        httr::POST(
            url = file.path("https://api.github.com/repos",
                workshopbuilder:::.options$get("BOOK_REPO"), "issues",
                issuenumber, "comments"),
            body = jsonlite::toJSON(list(body = paste(feedback, sep = "\n"))),
            httr::content_type("application/json"),
            httr::accept_json(),
            httr::add_headers(Authorization = paste("token", .getToken()))
        )
    }
}

.getToken <- function() {
    Sys.getenv("GITHUB_TOKEN", Sys.getenv("GITHUB_PAT", ""))
}
