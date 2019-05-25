.onLoad <- function(...) {
    .options$set("BOOK_REPO", "Bioconductor/BiocWorkshops2019")
    .options$set("LOCAL_REPO",
        file.path(normalizePath("~"), "BiocWorkshops2019"))
}

.options <- local({
    env <- new.env(parent = emptyenv())

    list(
        set = function(variable, value) {
            stopifnot(
                is.character(variable), length(variable) == 1L, !is.na(variable),
                is.character(value), length(value) == 1L, !is.na(value)
            )
            env[[variable]] <- value
        },
        get = function(variable) {
            env[[variable]]
        }
    )
})
