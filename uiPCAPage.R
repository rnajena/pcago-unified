#'
#' User interface defintion for the PCA page
#' 

library(shiny)
source("readCountImporter.R")
source("annotationImporter.R")
source("readCountNormalizer.R")
source("widgetDownloadableDataTable.R")
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
    mainPanel(navbarPage(title = "",
                         id = "pca.nav",
                         navbarMenu("Data",
                                    "Read counts",
                                    tabPanel("Read counts", downloadableDataTableOutput("readcounts")),
                                    tabPanel("Processed read counts", downloadableDataTableOutput("readcounts.processed")),
                                    "----",
                                    "Genes",
                                    tabPanel("Gene variances", value = "pca.genes.variances", downloadableDataTableOutput("annotation.var"),
                                             plotOutput("genes.variance.plot")),
                                    "----",
                                    "Conditions",
                                    tabPanel("Cell condition assignments", downloadableDataTableOutput("conditions"))
                                    ),
                         navbarMenu("PCA",
                                    "Principal components",
                                    tabPanel("Principal components", downloadableDataTableOutput("pca.pc")),
                                    tabPanel("PC variances", downloadableDataTableOutput("pca.var")),
                                    "----",
                                    "Cells",
                                    tabPanel("Transformed values", downloadableDataTableOutput("pca.transformed")),
                                    tabPanel("Cell plot", value = "pca.cells.plot", plotOutput("pca.cellplot"))
                                    ),
                         tabPanel(faIconText("link", "Cell plot"), value = "pca.cells.plot.quicklink")))
  ))
}