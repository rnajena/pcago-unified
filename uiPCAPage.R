#'
#' User interface defintion for the PCA page
#' 

library(shiny)
source("readCountImporter.R")
source("annotationImporter.R")
source("readCountNormalizer.R")
source("uiPCAPlotPanel.R")
source("widgetDownloadableDataTable.R")
source("uiHelper.R")

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
                          value = "readcounts",
                          genericImporterInput("pca.data.readcounts",
                                               supportedReadcountFileTypes,
                                               supportedReadcountImporters,
                                               availableReadcountSamples),
                          radioButtons("pca.data.normalization",
                                       helpIconText("Apply read count normalization", 
                                                    "If you already have normalized read counts, set this to 'None'.",
                                                    "Read count normalization"),
                                       supportedReadcountNormalizationTypes)
          ),
          bsCollapsePanel("Annotation",
                          value = "annotation",
                          genericImporterInput("pca.data.annotation",
                                               supportedAnnotationFileTypes,
                                               supportedAnnotationImporters)
          ),
          bsCollapsePanel("Conditions",
                          value = "conditions",
                          radioButtons("pca.data.conditions.mode",
                                       "Source of cell conditions for visualization:",
                                       c("Column names" = "column",
                                         "Extract from columns" = "extract",
                                         "Upload" = "upload"),
                                       selected = "column"),
                          conditionalPanel(conditionalPanel.equals("pca.data.conditions.mode", "'extract'"),
                                           selectizeInput("pca.data.conditions.separator",
                                                          label = "Separator",
                                                          choices = c("_", ".", ":", "#"),
                                                          selected = "_",
                                                          options = list("create" = T))),
                          conditionalPanel(conditionalPanel.equals("pca.data.conditions.mode", "'upload'"),
                                           genericImporterInput("pca.data.conditions.upload",filetypes = c("text/csv"), importers = c("Default"))))
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