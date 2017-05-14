#'
#' User interface defintion for the PCA page
#' 

library(shiny)
source("readcounts.R")
source("geneAnnotation.R")
source("widgetDownloadableDataTable.R")
source("widgetDownloadablePlot.R")
source("widgetGenericImporter.R")
source("widgetProcessingSteps.R")
source("uiHelper.R")
source("uiPCASidebars.R")
source("plotCellPlot.R")
source("plotGeneVariancePlot.R")
source("plotConditionsVennDiagramPlot.R")
source("plotPCAVariancePlot.R")
source("plotAgglomerativeClusteringPlot.R")

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
      tabPanel("Filter genes", uiPCASidebarFilterGenes()),
      tabPanel("PCA", uiPCASidebarPCA()),
      tabPanel("Plot", uiPCASidebarPlot()),
      type = "pills"
    )),
    mainPanel(tags$div(class = "pca-page", navbarPage(title = "",
                         id = "pca.nav",
                         selected = "pca.cells.plot",
                         navbarMenu("Data",
                                    "Read counts",
                                    tabPanel("Raw", value = "readcounts.raw", downloadableDataTableOutput("readcounts")),
                                    tabPanel("Processed", value = "readcounts.processed", 
                                             #uiOutput("readcounts.processing.steps"),
                                             processingStepsWidgetUI("readcounts.processing.steps", "Processing overview"),
                                             plotAgglomerativeClusteringPlotUI("readcounts.processed.hclust.plot"),
                                             downloadableDataTableOutput("readcounts.processed")),
                                    tabPanel("Filtered by annotation", value = "readcounts.filtered",
                                             processingStepsWidgetUI("readcounts.filtered.steps", "Processing overview"),
                                             plotAgglomerativeClusteringPlotUI("readcounts.filtered.hclust.plot"),
                                             downloadableDataTableOutput("readcounts.filtered")),
                                    tabPanel("Filtered by variance cut-off", value = "readcounts.top.variant",
                                             processingStepsWidgetUI("readcounts.top.variant.steps", "Processing overview"),
                                             plotAgglomerativeClusteringPlotUI("readcounts.top.variant.hclust.plot"),
                                             downloadableDataTableOutput("readcounts.top.variant")),
                                    "----",
                                    "Genes",
                                    tabPanel("Annotation", value = "genes.annotation",
                                             downloadableDataTableOutput("genes.annotation")),
                                    tabPanel("Variances (Processed counts)", value = "genes.variances",
                                             processingStepsWidgetUI("genes.variance.processing", "Processing overview"),
                                             plotGeneVariancePlotUI("genes.variances.plot"),
                                             downloadableDataTableOutput("genes.variance")),
                                    tabPanel("Variances (Filtered counts)", value = "genes.variances.filtered",
                                             processingStepsWidgetUI("genes.variance.filtered.processing", "Processing overview"),
                                             plotGeneVariancePlotUI("genes.variances.filtered.plot"),
                                             downloadableDataTableOutput("genes.variance.filtered")
                                             ),
                                    "----",
                                    "Cells",
                                    tabPanel("Conditions", value = "cells.conditions",
                                             plotConditionsVennDiagramPlotUI("cells.conditions.plot"),
                                             downloadableDataTableOutput("pca.cells.conditions")),
                                    tabPanel("Annotation", value = "cells.annotation",
                                             downloadableDataTableOutput("cells.annotation"))
                                    ),
                         navbarMenu("PCA",
                                    "Principal components",
                                    tabPanel("Principal components", value = "pca.pc.pc",
                                             processingStepsWidgetUI("pca.pc.processing", "Processing overview"),
                                             downloadableDataTableOutput("pca.pc")),
                                    tabPanel("Importance", value = "pca.pc.importance",
                                             processingStepsWidgetUI("pca.variance.processing", "Processing overview"),
                                             plotPCAVariancePlotUI("pca.variance.plot"),
                                             downloadableDataTableOutput("pca.variance")),
                                    "----",
                                    "Cells",
                                    tabPanel("Transformed values", value = "pca.cells.transformed",
                                             processingStepsWidgetUI("pca.transformed.processing", "Processing overview"),
                                             plotAgglomerativeClusteringPlotUI("pca.transformed.hclust.plot"),
                                             downloadableDataTableOutput("pca.transformed")),
                                    tabPanel("Cell plot", value = "pca.cells.plot", plotCellPlotUI("pca.cells.plot"))
                                    ),
                         tabPanel(faIconText("link", "Cell plot"), value = "pca.cells.plot.quicklink")))
  )))
}