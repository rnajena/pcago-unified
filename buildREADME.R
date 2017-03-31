#'
#' Builds the README file
#' 

project.name <- "PCAGO"
project.description <- "Online tool to perform PCA on RNASeq results"
additional.credits <- c(
  "Uses code from https://github.com/daattali/advanced-shiny by Dean Attali. Licensed under the MIT license."
)

r.version <- sessionInfo()$R.version$version.string
r.packages <- as.vector(sapply(sessionInfo()$otherPkgs, function(x) { paste(x$Package, ">=", x$Version) }))
r.packages.install <- paste0("install.packages(c(", 
                             paste(as.vector(sapply(sessionInfo()$otherPkgs, function(x) { paste0("\"", x$Package, "\"") }))
                             ,collapse = ", "), "))")

# Write data
con <- file("README.md")

writeLines(
  c(
    paste("#", project.name),
    "",
    project.description,
    "",
    paste("## Requirements"),
    "",
    paste("*", r.version),
    sapply(r.packages, function(x) { paste("*", x) }),
    "",
    paste0("`", r.packages.install, "`"),
    "",
    paste("## Credits"),
    "",
    additional.credits
  ),
  con
)

close(con)