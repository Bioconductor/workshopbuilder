# issues <- .getIssues(repository = "Bioconductor/BiocWorkshops2019")
# issues <- issues[c("title", "body", "number")]
# issues <- .selectWorkshopElements(issues)
# issues <- .setRepoInBody(issues)

# rebranch <- .readIssues(repository = "Bioconductor/BiocWorkshops2019")
# local <- workshopbuilder:::.options$get("REPOS_PATH")
#
# .installIssues(rebranch, local)

# gh:::gh_build_request("/repos/:owner/:repo/issues/:issue_number/comments",
#     method = "POST",
#     params = list(owner = "Bioconductor", repo = "BiocWorkshops2019", issue_number = "1"))
#
#
# gh:::gh_build_request("/repos/:owner/:repo/issues", method = "GET",
#     params = list(owner = "Bioconductor", repo = "BiocWorkshops2019"))
#
# httr::content(
# httr::GET(
#     url = postComments$url,
#     httr::content_type("application/json"),
#     httr::accept_json(),
#     httr::add_headers(.headers = postComments$headers)
# )
# )
