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
                                             downloadablePlotOutput("genes.variance.plot",
                                                                    custom.header.items = tagList(
                                                                      bsButton("genes.variance.plot.log", 
                                                                               "Logarithmic",
                                                                               icon = icon("superscript"),
                                                                               type = "toggle",
                                                                               value = T)
                                                                    )),
                                             downloadableDataTableOutput("genes.variance")),
                                    tabPanel("Variances (filtered)", value = "pca.genes.variances.filtered",
                                             downloadablePlotOutput("genes.variance.filtered.plot",
                                                                    custom.header.items = tagList(
                                                                      bsButton("pca.genes.variance.filtered.plot.log", 
                                                                               "Logarithmic",
                                                                               icon = icon("superscript"),
                                                                               type = "toggle",
                                                                               value = T)
                                                                    ))
                                             ),
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