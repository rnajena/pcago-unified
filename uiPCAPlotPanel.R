#'
#' User interface definition for the "Plot" sidebar
#' 

library(shiny)
library(shinyBS)
source("widgetInPlaceHelp.R")
source("widgetColorShapeInput.R")

#' Creates the plot settings panel that allows the user to select the plotted PCA axes, change colors etc.
#'
#' @return
#' @export
#'
#' @examples
uiPCAPlotPanel <- function() {
  return(verticalLayout(
    conditionalPanel("input['pca.page.resultplots.tab'] == 'cells'",
                     bsCollapse(
                       bsCollapsePanel("Axes",
                                       selectizeInput("pca.plot.cells.axes",
                                                      "Visible axes (x, y, z)",
                                                      choices = c(),
                                                      multiple = T,
                                                      options = list(maxItems = 3))),
                       bsCollapsePanel("Visualization",
                                       colorShapeEditorInput("pca.plot.visuals.editor")
                                       ),
                       bsCollapsePanel("Output settings")
                     )),
    conditionalPanel("input['pca.page.resultplots.tab'] == 'variance'",
                     bsCollapse(
                       bsCollapsePanel("Output settings")
                     ))
  ))
}