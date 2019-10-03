# API location of the workshop repositories
.github_repos <- "https://api.github.com/repos"
.github_url <- "https://github.com"

.readRemotes <- function(descfile) {
    dcf <- read.dcf(descfile, "Remotes")
    trimws(strsplit(dcf, "[, \n]+")[[1L]])
}

.getIssues <-
    function(repository) {
    endpoint <- file.path(.github_repos, repository, "issues")
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

.setRepoMetadata <- function(issueFrame, schemes = "http[s]*") {
    issueBodies <- issueFrame[["body"]]
    bodies <- strsplit(issueBodies, "\\s")
    urlidx <- vapply(bodies, function(x) min(grep(schemes, x)), integer(1L))

    repos <- mapply(`[`, bodies, urlidx)
    repoURL <- gsub("\\.git", "", repos)
    location <- gsub("\\/tree.*", "", repoURL)
    issueFrame[["location"]] <- location

    repo <- basename(location)
    owner <- basename(dirname(location))
    issueFrame[["repository"]] <- repo
    issueFrame[["owner"]] <- owner
    issueFrame[["ownerrepo"]] <- file.path(owner, repo)

    hasBranch <- grepl("tree", repoURL, fixed = TRUE)
    branches <- basename(repoURL[hasBranch])
    issueFrame[["refs"]] <- ifelse(hasBranch, branches, "master")
    issueFrame
}

.readIssues <-
    function(repository, fields = c("body", "title", "number"),
        local_repos = workshopbuilder:::.options$get("REPOS_PATH")) {
    issues <- .getIssues(repository)
    issues <- do.call(function(...) {
        rbind.data.frame(..., stringsAsFactors = FALSE) },
        lapply(issues, `[`, fields)
    )
    issues <- .selectWorkshopElements(issues)
    issues <- .setRepoMetadata(issues)
    repos <- Filter(function(x) {x != "buildout"}, list.files(local_repos))
    if (all(issues[["repository"]] %in% repos))
        issues <- .addPackageName(issues)
    issues
}

.addPackageName <- function(reposREF,
    repos_path = workshopbuilder:::.options$get("REPOS_PATH"))
{
    res <- apply(reposREF, 1L, function(x) {
        local_repo <- file.path(repos_path, x[["repository"]])
        descfile <- desc::description$new(file.path(local_repo, "DESCRIPTION"))
        x[["Package"]] <- descfile$get("Package")
        x
    })
    as.data.frame(t(res), stringsAsFactors = FALSE)
}

.checkDESC <- function(branchdf) {
    apply(branchdf, 1L, function(x) {
        RCurl::url.exists(
            file.path(
                "https://github.com", x[["ownerrepo"]], "tree",
                x[["refs"]], "DESCRIPTION"
            )
        )
    })
}

.warnNoDESC <- function(branchdf) {
    validPKGS <- .checkDESC(branchdf)
    invalid <- branchdf[!validPKGS, "ownerrepo"]
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
        if (!identical(line[["refs"]], "master"))
            paste(line[["ownerrepo"]], line[["refs"]], sep = "@")
        else
            line[["ownerrepo"]]
    })
    desc::desc_add_remotes(remotes, local_repo)
}

.addImports <- function(local_repo, pkgnames) {
    descfile <- file.path(local_repo, "DESCRIPTION")
    invisible(
        lapply(pkgnames, function(pkg) {
            desc::desc_set_dep(pkg, type = "Imports", file = descfile)
        })
    )
}

.installIssues <- function(repos_data, local, buildDir, ...) {
    builddir <- file.path(local, buildDir)
    if (!dir.exists(builddir))
        dir.create(builddir)
    res <- apply(repos_data, 1L, function(x) {
        xx <- system2(file.path(R.home(), "bin/R"),
            args = paste("CMD INSTALL", file.path(local, x[["repository"]])),
            stdout = TRUE, stderr = TRUE)
        if (any(grepl("DONE", xx))) {
            msg <- xx
            x[["install"]] <- TRUE
        } else if (attr(xx, "status") != 0) {
            msg <- paste0("Unable to install package: ", x[["ownerrepo"]])
            x[["install"]] <- FALSE
        }
        writeLines(text = msg,
            con = file.path(builddir, paste0(x[["repository"]], ".out"))
        )
        x
    })
    res <- as.data.frame(t(res), stringsAsFactors = FALSE)
    res[["install"]] <- as.logical(res[["install"]])
    res
}

#' Install workshops from locations as indicated by main repository
#'
#' This function looks at the 'issues' page of the main repository and installs
#' from the workshop URLs tagged as `[Workshop]`
#'
#' @param workshops character() An optional vector of workshop repositories to
#'     install. Default behavior installs all workshops.
#' @param exclude logical(1) Whether to include or exclude the workshops
#'     listed in the argument from the full list of workshops (default FALSE)
#' @param repository A single string indicating the 'username/repository' of the
#'     main book-building GitHub repository (defaults to package option
#'     'BOOK_REPO')
#' @param local character(1) The folder location for all cloned workshop
#'     repositories
#' @param buildDir character(1) The folder location for all workshop build
#'     reports (within the 'local' folder)
#' @param ncpus integer(1) The number of CPUs to use for the building and
#'     installation of workshop repositories (default: 1L)
#'
#' @export
installWorkshops <-
    function(workshops = NULL, exclude = FALSE,
        repository = workshopbuilder:::.options$get("BOOK_REPO"),
        local = workshopbuilder:::.options$get("REPOS_PATH"),
        buildDir = "buildout",
        ncpus = 1L, ...)
{
    ncp <- getOption("Ncpus", 1L)
    on.exit(options(Ncpus = ncp))
    options(Ncpus = ncpus)
    remotes <- .readIssues(repository)
    remotes <- getIssueRepos(remotes, local)
    if (!is.null(workshops)) {
        keep <- remotes[["repository"]] %in% workshops
        if (!any(keep))
            stop("'workshops' not found in repositories list")
        if (exclude)
            keep <- !keep
        remotes <- remotes[keep, , drop = FALSE]
    }

    .installIssues(remotes, local, buildDir, ...)
}

#' @export
getBookRepo <-
    function(
        repository = workshopbuilder:::.options$get("BOOK_REPO"),
        local_repo = workshopbuilder:::.options$get("LOCAL_REPO")
    )
{
    if (!dir.exists(local_repo)) {
        dir.create(local_repo, recursive = TRUE)
        git2r::clone(file.path(.github_url, repository), local_repo)
    } else
        git2r::pull(repo = local_repo)

    current <- workshopbuilder:::.options$get("LOCAL_REPO")

    if (!identical(local_repo, current))
        workshopbuilder:::.options$set("LOCAL_REPO", local_repo)

    workshopbuilder:::.options$get("LOCAL_REPO")
}

#' @export
getIssueRepos <-
    function(
        repos,
        repos_path = workshopbuilder:::.options$get("REPOS_PATH")
    )
{
    if (!dir.exists(repos_path))
        dir.create(repos_path, recursive = TRUE)
    apply(repos, 1L, function(x) {
        local_repo <- file.path(repos_path, x[["repository"]])
        if (!dir.exists(local_repo))
            git2r::clone(url = x[["location"]],
                local_path = local_repo, branch = x[["refs"]])
        else
            git2r::pull(repo = local_repo)
    })
    repos
}

#' @export
getWorkshops <-
    function(repository = workshopbuilder:::.options$get("BOOK_REPO"))
{
    remotes <- .readIssues(repository)
    .warnNoDESC(remotes)
}

#' @export
addWorkshops <-
    function(reposREF,
        local_repo = workshopbuilder:::.options$get("LOCAL_REPO")
    )
{
        bookdown <- data.frame(ownerrepo="rstudio/bookdown", refs="master")
        reposREF <- reposREF[reposREF[["install"]], ]
        branch <- ifelse(reposREF[["refs"]] == "master", "",
            paste0("@", reposREF[["refs"]]))
        pkg <- ifelse(reposREF[["Package"]] == reposREF[["repository"]], "",
            paste0(reposREF[["Package"]], "="))
        reposinREF <- paste0(pkg, reposREF[["ownerrepo"]], branch)
        localremotes <- .readRemotes(file.path(local_repo, "DESCRIPTION"))
        newremotes <- !(reposinREF %in% localremotes)
        if (any(newremotes)) {
            reposREF <- reposREF[newremotes, , drop = FALSE]
            .addRemotes(local_repo, reposREF)
            .addImports(local_repo, reposREF[["Package"]])
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
    ## install results add a "install" TRUE / FALSE column
    instres <- remotes[["install"]]
    if (is.null(instres))
        stop("'install' results column not found in 'remotes' argument")
    remotes <- remotes[instres, ]

    viglist <- Map(c,
        pkgName = remotes[["Package"]],
        vignette = vapply(remotes[["repository"]], function(pkg) {
            vigloc <- file.path(repo_path, pkg, "vignettes")
            list.files(vigloc, pattern = "\\.[Rr][Mm][Dd]",
                full.names = TRUE)[1L]
            }, character(1L)
        )
    )

    resources <- list.files(
        path = file.path(repo_path, remotes[["Package"]], "inst"),
        pattern = "[^Rmd]", recursive = TRUE)

    vapply(viglist, function(elem) {
        pkg <- elem[["pkgName"]]
        viglines <- suppressWarnings(readLines(elem[["vignette"]]))
        newVigName <- paste0(pkg, ".Rmd")
        newVig <- file.path(local_repo, newVigName)

        h1s <- grep("^#\\s*\\w+", viglines)
        if (!length(h1s)) {
            h1s <- grep("^##\\s*\\w+", viglines)
            if (length(h1s))
                viglines <- gsub("##", "#", viglines, fixed = TRUE)
            else
                stop("No H1/H2 ('#', '##') markdown headers found")
        }

        linerange <- seq(min(h1s), length(viglines))

        writeLines(viglines[linerange], con = newVig)
        newVig
    }, character(1L))
}

#' @export
getStatus <- function(local = workshopbuilder:::.options$get("REPOS_PATH"),
    buildDir = "buildout") {
    outReport <- file.path(local, buildDir)
    reportFiles <- list.files(outReport, full.names = TRUE, pattern = ".out")
    shopnames <- gsub("\\.out", "", basename(reportFiles))
    reportFiles <- setNames(reportFiles, shopnames)
    if (!length(reportFiles))
        stop("No files saved in 'buildDir'. Run 'installWorkshops()'")
    lapply(reportFiles, function(txt) {
        lines <- trimws(readLines(txt))
        lines[nchar(lines) != 0L]
    })
}

#' @export
postStatus <- function(
    repository = workshopbuilder:::.options$get("BOOK_REPO"),
    local = workshopbuilder:::.options$get("REPOS_PATH"),
    buildDir = "buildout") {

    statList <- Filter(length, getStatus(local, buildDir))
    if (!length(statList))
        stop("No install '.out' files found in directory:\n ",
            file.path(local, buildDir), "\n 'installWorkshops()' first")
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

.cleanBuildReport <-
    function(local = workshopbuilder:::.options$get("REPOS_PATH"),
        buildDir = "buildout") {
    outReport <- file.path(local, buildDir)
    unlink(outReport, recursive = TRUE)
}

#' @export
addRmdFiles <- function(rmdlist,
    local = workshopbuilder:::.options$get("LOCAL_REPO"), verbose = TRUE) {

    yamlpath <- file.path(local, "_bookdown.yml")
    bdyam <- yaml::read_yaml(yamlpath)
    bdyam <- bdyam[names(bdyam) != "rmd_files"]

    ## ToDo: add Rmd file ordering here
    rmds <- basename(unname(rmdlist))
    newyam <- c(bdyam, list(rmd_files = rmds))
    yaml::write_yaml(newyam, file = yamlpath)

    if (verbose)
        message(yaml::as.yaml(newyam))
}

#' @export
renderWorkshops <-
    function(local = workshopbuilder:::.options$get("LOCAL_REPO"),
    format = c("all", "gitbook", "pdf_book", "epub_book")) {

    curr <- getwd()
    setwd(local)
    on.exit(setwd(curr))
    format <- match.arg(format, several.ok = TRUE)
    if ("all" %in% format)
        format <- c("gitbook", "pdf_book", "epub_book")

    for (fmt in format)
        bookdown::render_book("index.Rmd", paste0("bookdown::", fmt))

}
