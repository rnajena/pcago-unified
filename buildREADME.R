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

r.packages.cran <- Filter(function(x) { "Repository" %in% names(x) && x$Repository == "CRAN" }, sessionInfo()$otherPkgs)
r.packages.bioc <- Filter(function(x) { !("Repository" %in% names(x)) }, sessionInfo()$otherPkgs)
r.packages.cran.install <- paste0("install.packages(c(", 
                             paste(as.vector(sapply(r.packages.cran, function(x) { paste0("\"", x$Package, "\"") }))
                             ,collapse = ", "), "))")
r.packages.bioc.install <-  paste0("biocLite(c(", paste(as.vector(sapply(r.packages.bioc, function(x) { paste0("\"", x$Package, "\"") })), collapse = ", ") ,
                                   "), type = \"source\")")

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
    "```",
    "# CRAN packages",
    r.packages.cran.install,
    "",
    "# BioConductor packages",
    "source(\"https://bioconductor.org/biocLite.R\")",
    r.packages.bioc.install,
    "```",
    "",
    paste("## Credits"),
    "",
    additional.credits
  ),
  con
)

close(con)