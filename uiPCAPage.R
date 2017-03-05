library(shiny)
source("uiPCADataPanel.R")
source("uiPCAPlotPanel.R")
source("uiPCAPCAPanel.R")
source("uiHelpers.R")

#' Creates the PCA analysis page for the UI
#'
#' @return Object of shiny page type
#' @export
#'
#' @examples
uiPCAPage <- function() {
  return(fillPage(
    sidebarLayout(sidebarPanel(
      tabsetPanel(
        tabPanel("Data", uiPCADataPanel()),
        tabPanel("PCA", uiPCAPCAPanel()),
        tabPanel("Plot", uiPCAPlotPanel()),
        type = "pills"
      )
    ),
      mainPanel(
        bsAlert("pca.alert"),
        tabsetPanel(
          tabPanel("Read counts", 
                   tabsetPanel(
                     tabPanel("Input", downloadableDataTableOutput("readcounts")),
                     tabPanel("Normalized", downloadableDataTableOutput("readcounts.normalized")),
                     type = "pills"
                   )),
          tabPanel("Annotation"),
          tabPanel("Result tables",
                   tabsetPanel(
                     tabPanel("Transformed conditions", downloadableDataTableOutput("transformedconditions")),
                     tabPanel("Principal components", downloadableDataTableOutput("pca.principalcomponents")),
                     tabPanel("Principal component variances", downloadableDataTableOutput("pca.var")),
                     tabPanel("Gene variance", downloadableDataTableOutput("genes.variance")),
                     type = "pills"
                   )),
          tabPanel("Result plots",
                   tabsetPanel(
                     tabPanel("Conditions", plotOutput("pca.conditionplot")),
                     tabPanel("Gene variance", plotOutput("genes.variance.plot")),
                     type = "pills"
                   ))
        )
      ))
  ))
}