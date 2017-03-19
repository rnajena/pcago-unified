#'
#' User interface defintion for the PCA page
#' 

library(shiny)
source("readCountImporter.R")
source("annotation.R")
source("readCountNormalizer.R")
source("widgetDownloadableDataTable.R")
source("widgetDownloadablePlot.R")
source("widgetGenericImporter.R")
source("uiHelper.R")
source("uiPCASidebars.R")

#' Creates the PCA analysis page for the UI
#'
#' @return Object of shiny page type
#' @export
#'
#' @examples
uiPCAPage <- function() {
  
  
  return(sidebarLayout(
    sidebarPanel(tabsetPanel(
      tabPanel("Data", uiPCASidebarData()),
      tabPanel("PCA", uiPCASidebarPCA()),
      tabPanel("Plot", uiPCASidebarPlot()),
      type = "pills"
    )),
    mainPanel(tags$div(class = "pca-page", navbarPage(title = "",
                         id = "pca.nav",
                         navbarMenu("Data",
                                    "Read counts",
                                    tabPanel("Raw read counts", value = "pca.readcounts.raw", downloadableDataTableOutput("readcounts")),
                                    tabPanel("Processed read counts", value = "pca.readcounts.processed", 
                                             uiOutput("readcounts.processing.steps"),
                                             downloadableDataTableOutput("readcounts.processed")),
                                    tabPanel("Filtered read counts", value = "pca.readcounts.filtered",
                                             downloadableDataTableOutput("readcounts.filtered")),
                                    tabPanel("Top variant read counts", value = "pca.readcounts.top.variant",
                                             downloadableDataTableOutput("readcounts.top.variant")),
                                    "----",
                                    "Genes",
                                    tabPanel("Gene variances", value = "pca.genes.variances",
                                             downloadablePlotOutput("genes.variance.plot"),
                                             downloadableDataTableOutput("annotation.var")),
                                    "----",
                                    "Conditions",
                                    tabPanel("Cell condition assignments", value = "pca.conditions",
                                             downloadableDataTableOutput("conditions"))
                                    ),
                         navbarMenu("PCA",
                                    "Principal components",
                                    tabPanel("Principal components", value = "pca.pc.pc",
                                             downloadableDataTableOutput("pca.pc")),
                                    tabPanel("Importance", value = "pca.pc.importance",
                                             downloadablePlotOutput("pca.variance.plot"),
                                             downloadableDataTableOutput("pca.variance")),
                                    "----",
                                    "Cells",
                                    tabPanel("Transformed values", value = "pca.cells.transformed",
                                             downloadableDataTableOutput("pca.transformed")),
                                    tabPanel("Cell plot", value = "pca.cells.plot",
                                             downloadablePlotOutput("pca.cellplot",
                                                                    downloadButton("pca.cellplot.export.mp4", "Export *.mp4")))
                                    ),
                         tabPanel(faIconText("link", "Cell plot"), value = "pca.cells.plot.quicklink")))
  )))
}