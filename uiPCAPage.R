#'
#' User interface defintion for the PCA page
#' 

library(shiny)
source("readcounts.R")
source("annotation.R")
source("widgetDownloadableDataTable.R")
source("widgetDownloadablePlot.R")
source("widgetGenericImporter.R")
source("uiHelper.R")
source("uiPCASidebars.R")
source("plotCellPlot.R")
source("plotGeneVariancePlot.R")

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
                         selected = "pca.cells.plot",
                         navbarMenu("Data",
                                    "Read counts",
                                    tabPanel("Raw", value = "pca.readcounts.raw", downloadableDataTableOutput("readcounts")),
                                    tabPanel("Processed", value = "pca.readcounts.processed", 
                                             uiOutput("readcounts.processing.steps"),
                                             downloadableDataTableOutput("readcounts.processed")),
                                    tabPanel("Filtered", value = "pca.readcounts.filtered",
                                             downloadableDataTableOutput("readcounts.filtered")),
                                    tabPanel("Top variant", value = "pca.readcounts.top.variant",
                                             downloadableDataTableOutput("readcounts.top.variant")),
                                    "----",
                                    "Genes",
                                    tabPanel("Annotation", value = "pca.genes.annotation",
                                             downloadableDataTableOutput("genes.annotation")),
                                    tabPanel("Variances", value = "pca.genes.variances",
                                             plotGeneVariancePlotUI("pca.genes.variances.plot"),
                                             downloadableDataTableOutput("genes.variance")),
                                    tabPanel("Variances (filtered)", value = "pca.genes.variances.filtered",
                                             plotGeneVariancePlotUI("pca.genes.variances.filtered.plot")
                                             ),
                                    "----",
                                    "Conditions",
                                    tabPanel("Cell condition assignments", value = "pca.conditions",
                                             downloadablePlotOutput("conditions.plot"),
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
                                    tabPanel("Cell plot", value = "pca.cells.plot", plotCellPlotUI("pca.cells.plot"))
                                    ),
                         tabPanel(faIconText("link", "Cell plot"), value = "pca.cells.plot.quicklink")))
  )))
}