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
                                       radioButtons("pca.plot.visuals.source",
                                                    "Shape & color points",
                                                    c("Show editor" = "editor",
                                                      "Upload" = "upload")),
                                       conditionalPanel("input['pca.plot.visuals.source'] == 'editor'", 
                                                        headerPanel(header = helpIconText("Editor",
                                                                                          "By using this editor you can modify the visual parameters of each condition."), 
                                                                    colorShapeEditorInput("pca.plot.visuals.editor")))
                                       ),
                       bsCollapsePanel("Output settings")
                     )),
    conditionalPanel("input['pca.page.resultplots.tab'] == 'variance'",
                     bsCollapse(
                       bsCollapsePanel("Output settings")
                     ))
  ))
}