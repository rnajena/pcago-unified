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
    uiHelpPageEntry("About", "helppages/about.md"),
    uiHelpPageEntry("Overview", "helppages/overview.md"),
    "Importing ",
    uiHelpPageEntry("Read counts", "helppages/import-readcounts.md"),
    uiHelpPageEntry("Sample annotation", "helppages/import-samples-annotation.md"),
    uiHelpPageEntry("Gene annotation", "helppages/import-gene-annotation.md"),
    uiHelpPageEntry("Upload widget", "helppages/import-uploader-widget.md"),
    uiHelpPageEntry("File formats", "helppages/import-fileformats.md"),
    "Processing",
    uiHelpPageEntry("Read count processing", "helppages/processing-readcount-processing.md"),
    uiHelpPageEntry("Filtering", "helppages/processing-filter-genes.md"),
    uiHelpPageEntry("GO Browser", "helppages/processing-go-browser.md"),
    uiHelpPageEntry("PCA", "helppages/processing-pca.md"),
    "Output",
    uiHelpPageEntry("Visuals editor widget", "helppages/output-visual-editor.md"),
    uiHelpPageEntry("Gradient editor widget", "helppages/output-gradient-editor.md"),
    uiHelpPageEntry("Processing view widget", "helppages/output-processing-view.md")
  ))
}