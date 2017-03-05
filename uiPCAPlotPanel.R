library(shiny)

#' Creates the plot settings panel that allows the user to select the plotted PCA axes, change colors etc.
#'
#' @return
#' @export
#'
#' @examples
uiPCAPlotPanel <- function() {
  return(bsCollapse(
    bsCollapsePanel("Axes")
  ))
}