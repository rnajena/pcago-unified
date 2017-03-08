library(shiny)
source("readCountImporter.R")
source("annotationImporter.R")
source("readCountNormalizer.R")
source("uiPCAPlotPanel.R")
source("widgetDownloadableDataTable.R")

#' Creates the PCA analysis page for the UI
#'
#' @return Object of shiny page type
#' @export
#'
#' @examples
uiPCAPage <- function() {
  return(bootstrapPage(
    sidebarLayout(sidebarPanel(
      tabsetPanel(
        tabPanel("Data", bsCollapse(
          bsCollapsePanel("Read counts",
                          genericImporterInput("pca.data.readcounts",
                                               supportedReadcountFileTypes,
                                               supportedReadcountImporters)
          ),
          bsCollapsePanel("Annotation",
                          genericImporterInput("pca.data.annotation",
                                               supportedAnnotationFileTypes,
                                               supportedAnnotationImporters)
          ),
          bsCollapsePanel("Normalization",
                          radioButtons("pca.data.normalization",
                                       "Apply read count normalization:",
                                       supportedReadcountNormalizationTypes)),
          bsCollapsePanel("Conditions",
                          radioButtons("pca.data.conditions",
                                       "Source of cell conditions for visualization:",
                                       c("Column names",
                                         "Extract from columns",
                                         "Upload")))
        )),
        tabPanel("PCA", bsCollapse(
          bsCollapsePanel("Gene set"),
          bsCollapsePanel("Gene count",
                          plotOutput("pca.pca.genes.count.variance.plot", height = "120px"),
                          sliderInput("pca.pca.genes.count", "Gene count", min = 0, max = 0, value = 0, step = 1),
                          fixedRow(
                            column(width=3, actionButton("pca.pca.genes.count.lstepdecrease", label = "", icon = icon("fast-backward")) ),
                            column(width=3, actionButton("pca.pca.genes.count.stepdecrease", label = "", icon = icon("step-backward")) ),
                            column(width=3, actionButton("pca.pca.genes.count.stepincrease", label = "", icon = icon("step-forward")) ),
                            column(width=3, actionButton("pca.pca.genes.count.lstepincrease", label = "", icon = icon("fast-forward")) )
                          ))
        )),
        tabPanel("Plot", uiPCAPlotPanel()),
        type = "pills"
      )
    ),
      mainPanel(
        tabsetPanel(
          tabPanel("Read counts", 
                   tabsetPanel(
                     tabPanel("Input read counts", downloadableDataTableOutput("readcounts")),
                     tabPanel("Normalized read counts", downloadableDataTableOutput("readcounts.normalized")),
                     type = "pills"
                   )),
          tabPanel("Genes",
                   tabsetPanel(
                     tabPanel("Variance", downloadableDataTableOutput("annotation.var")),
                     type = "pills"
                   )),
          tabPanel("Conditions",
                   tabsetPanel(
                     tabPanel("Parameters", downloadableDataTableOutput("conditions")),
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
                     tabPanel("Cells", plotlyOutput("pca.cellplot", width = "auto", height = "auto"), value = "cells"),
                     tabPanel("Gene variance", plotOutput("genes.variance.plot"), value = "variance"),
                     type = "pills",
                     id = "pca.page.resultplots.tab"
                   ))
        )
      ))
  ))
}