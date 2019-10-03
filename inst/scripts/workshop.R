library(workshopbuilder)

.libPaths()

work <- installWorkshops("MultiAssayWorkshop",
    exclude = FALSE, ncpus = 12)

getBookRepo()

work

addWorkshops(work)

rmds <- transferVignettes(work)

addRmdFiles(rmds)

renderWorkshops(format = "all")

