# workshopbuilder

A package to install and render workshop vignettes. The workshop
locations (on GitHub) are read from the main book repository's
(`Bioconductor/BiocWorkshops2019`) issues.

## Installation

```r
BiocManager::install("Bioconductor/workshopbuilder")
```

## Workflow

A typical workflow would involve setting a few package options:

1) Main book repository on GitHub

```r
.options$set("BOOK_REPO", "Bioconductor/BiocWorkshops2019")
```

2) Local workshop book directory

```r
.options$set("LOCAL_REPO", normalizePath("~/BiocWorkshops2019"))
```

3) Local workshop repositories directory

```r
.options$set("REPOS_PATH", normalizePath("~/Bioconductor/Workshops"))
```

After the initial setup, building a workshop is as follows: 

```r
library(workshopbuilder)

workshop_meta <- installWorkshops("MultiAssayWorkshop",
    exclude = FALSE, ncpus = 12)

getBookRepo()

addWorkshops(workshop_meta)

rmds <- transferVignettes(workshop_meta)

addRmdFiles(rmds)

renderWorkshops(format = "all")
```

