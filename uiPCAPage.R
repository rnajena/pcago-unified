library(shiny)
source("uiPCADataPanel.R")
source("uiPCAPlotPanel.R")
source("uiPCAPCAPanel.R")
source("uiDownloadableDataTable.R")

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
        tabsetPanel(
          tabPanel("Read counts", 
                   tabsetPanel(
                     tabPanel("Input", downloadableDataTableOutput("readcounts")),
                     tabPanel("Normalized", downloadableDataTableOutput("readcounts.normalized")),
                     type = "pills"
                   )),
          tabPanel("Annotation",
                   tabsetPanel(
                     tabPanel("Gene variance", downloadableDataTableOutput("annotation.var")),
                     type = "pills"
                   )),
          tabPanel("Result tables",
                   tabsetPanel(
                     tabPanel("Transformed conditions", downloadableDataTableOutput("pca.transformed")),
                     tabPanel("Principal components", downloadableDataTableOutput("pca.pc")),
                     tabPanel("Principal component variances", downloadableDataTableOutput("pca.var")),
                     type = "pills"
                   )),
          tabPanel("Result plots",
                   tabsetPanel(
                     tabPanel("Conditions", plotOutput("pca.conditionplot"), value = "conditions"),
                     tabPanel("Gene variance", plotOutput("genes.variance.plot"), value = "variance"),
                     type = "pills",
                     id = "pca.page.resultplots.tab"
                   ))
        )
      ))
  ))
}