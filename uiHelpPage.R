library(shiny)

#' Creates a help entry for the help page. Returns a tabPanel for navigation with content loaded from markdown
#'
#' @param title Title of the help page
#' @param markdown Filename of the markdown file
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
uiHelpPageEntry <- function(title, filename) {
  
  return(tabPanel(title, tags$div(class = "help-page", includeMarkdown(filename))))
  
}

#' Creates the help page for the UI
#'
#' @return
#' @export Object of shiny page type
#'
#' @examples
uiHelpPage <- function() {
  return(navlistPanel(
    uiHelpPageEntry("Overview", "helppages/overview.md"),
    "Data",
    uiHelpPageEntry("Read counts", "helppages/data-readcounts.md"),
    uiHelpPageEntry("Annotations", "helppages/data-annotations.md"),
    uiHelpPageEntry("Conditions", "helppages/data-conditions.md"),
    "PCA",
    uiHelpPageEntry("Gene selection/count", "helppages/pca-genes.md"),
    uiHelpPageEntry("Principal components", "helppages/pca-pc.md"),
    uiHelpPageEntry("Cells", "helppages/pca-cells.md")
  ))
}