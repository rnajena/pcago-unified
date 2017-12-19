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
source("plotSamplePlot.R")
source("plotGeneVariancePlot.R")
source("plotConditionsVennDiagramPlot.R")
source("plotPCAVariancePlot.R")
source("plotAgglomerativeClusteringPlot.R")
source("plotLoadingsPlot.R")

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
    ),
    hDivider(),
    tags$p(actionLink("quickio.load", "Load example data", icon = icon("folder-open"))),
    tags$p(actionLink("quickio.save", iconText(icon("download"), "Download all data")))
    ),
    mainPanel(tags$div(class = "pca-page", navbarPage(title = "",
                         id = "pca.nav",
                         selected = "pca.samples.plot",
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
                                    "Samples",
                                    tabPanel("Conditions", value = "samples.conditions",
                                             plotConditionsVennDiagramPlotUI("samples.conditions.plot"),
                                             downloadableDataTableOutput("samples.conditions")),
                                    tabPanel("Annotation", value = "samples.annotation",
                                             downloadableDataTableOutput("samples.annotation"))
                                    ),
                         navbarMenu("PCA",
                                    "Principal components",
                                    tabPanel("Principal components", value = "pca.pc.pc",
                                             processingStepsWidgetUI("pca.pc.processing", "Processing overview"),
                                             plotLoadingsPlotUI("pca.loadings.plot"),
                                             downloadableDataTableOutput("pca.pc")),
                                    tabPanel("Scree plot", value = "pca.pc.importance",
                                             processingStepsWidgetUI("pca.variance.processing", "Processing overview"),
                                             plotPCAVariancePlotUI("pca.variance.plot"),
                                             downloadableDataTableOutput("pca.variance")),
                                    "----",
                                    "Samples",
                                    tabPanel("Transformed values", value = "pca.samples.transformed",
                                             processingStepsWidgetUI("pca.transformed.processing", "Processing overview"),
                                             plotAgglomerativeClusteringPlotUI("pca.transformed.hclust.plot"),
                                             downloadableDataTableOutput("pca.transformed")),
                                    tabPanel("PCA samples plot", value = "pca.samples.plot", plotSamplePlotUI("pca.samples.plot"))
                                    ),
                         tabPanel(faIconText("link", "Samples plot"), value = "pca.samples.plot.quicklink")
                         ))
  )))
}