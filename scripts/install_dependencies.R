repos <- getOption("repos")
if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) {
  repos <- c(CRAN = "https://cloud.r-project.org")
}

description <- read.dcf("DESCRIPTION")
fields <- intersect(c("Depends", "Imports"), colnames(description))
packages <- unlist(strsplit(description[1, fields], ","))
packages <- trimws(gsub("\\s*\\([^)]*\\)", "", packages))
packages <- setdiff(packages[nzchar(packages)], c("R"))

missing_packages <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = repos)
} else {
  message("All required packages are already installed.")
}
