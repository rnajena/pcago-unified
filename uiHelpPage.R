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
    uiHelpPageEntry("Sample annotation", "helppages/data-samples-annotation.md"),
    uiHelpPageEntry("Gene annotation", "helppages/data-gene-annotation.md"),
    uiHelpPageEntry("Read count processing", "helppages/data-readcount-processing.md"),
    "Filter genes",
    uiHelpPageEntry("By annotation", "helppages/filter-genes-annotation.md"),
    uiHelpPageEntry("By gene variance", "helppages/filter-genes-gene-variance.md"),
    "PCA",
    uiHelpPageEntry("Parameters", "helppages/pca-parameters.md"),
    "Appendix",
    uiHelpPageEntry("Upload widget", "helppages/appendix-uploader-widget.md"),
    uiHelpPageEntry("Visuals editor widget", "helppages/appendix-visual-editor.md"),
    uiHelpPageEntry("Processing view widget", "helppages/appendix-processing-view.md"),
    uiHelpPageEntry("File formats", "helppages/appendix-fileformats.md")
  ))
}