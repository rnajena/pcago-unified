library(shiny)
library(shinyBS)

#' Creates the plot settings panel that allows the user to select the plotted PCA axes, change colors etc.
#'
#' @return
#' @export
#'
#' @examples
uiPCAPlotPanel <- function() {
  return(verticalLayout(
    conditionalPanel("input['pca.page.resultplots.tab'] == 'conditions'",
                     bsCollapse(
                       bsCollapsePanel("Axes"),
                       bsCollapsePanel("Output settings")
                     )),
    conditionalPanel("input['pca.page.resultplots.tab'] == 'variance'",
                     bsCollapse(
                       bsCollapsePanel("Output settings")
                     ))
  ))
}