#'
#' UI definition for the sidebars
#' 

library(shiny)
library(shinyBS)
source("widgetInPlaceHelp.R")
source("widgetColorShapeInput.R")


#' Creates UI definition for the "data" sidebar
#' This sidebar allows the user to upload necessary data and transform them for later processing
#'
#' @return
#' @export
#'
#' @examples
uiPCASidebarData <- function() {
  
  return(bsCollapse(
    bsCollapsePanel("Read counts",
                    value = "readcounts",
                    bsCollapse(
                      bsCollapsePanel("Import",
                                      genericImporterInput("pca.data.readcounts",
                                                           supportedReadcountFileTypes,
                                                           supportedReadcountImporters,
                                                           availableReadcountSamples)),
                      bsCollapsePanel("Processing",
                                      checkboxGroupInput("pca.data.readcounts.processing",
                                                         helpIconText("Apply processing", includeMarkdown("helptooltips/pca-data-readcounts-processing.md")),
                                                         choices = c("Transpose table" = "transpose",
                                                                    "Remove genes with constant readcounts" = "remove.constant"),
                                                         selected = c("remove.constant")),
                                      radioButtons("pca.data.normalization",
                                                   helpIconText("Apply read count normalization", 
                                                                "If you already have normalized read counts, set this to 'None'.",
                                                                "Read count normalization"),
                                                   supportedReadcountNormalizationTypes))
                    )
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
  ))
  
}

#' Creates definition for the "PCA" sidebar
#' This sidebar allows the user to change parameters related to the PCA
#' Those parameters include prcomp() parameters and the selection of genes to analyze
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
uiPCASidebarPCA <- function() {
  
  return(bsCollapse(
    bsCollapsePanel("Gene set"),
    bsCollapsePanel("Gene count",
                    plotOutput("pca.pca.genes.count.variance.plot", height = "120px"),
                    sliderInput("pca.pca.genes.count", "Gene count", min = 0, max = 0, value = 0, step = 1, round = T),
                    fluidRow(style = "text-align: center;", tags$div(class = "col-centered", style = "display: inline-block;",
                                                                     actionButton("pca.pca.genes.count.lstepdecrease", label = "", icon = icon("fast-backward")),
                                                                     actionButton("pca.pca.genes.count.stepdecrease", label = "", icon = icon("step-backward")),
                                                                     bsButton("pca.pca.genes.count.animation.play", label = tags$span(tags$span(class = "play", icon("play")), tags$span(class = "pause", icon("pause"))), type = "toggle", style = "play-pause"),
                                                                     actionButton("pca.pca.genes.count.stepincrease", label = "", icon = icon("step-forward")),
                                                                     actionButton("pca.pca.genes.count.lstepincrease", label = "", icon = icon("fast-forward"))
                                                                     )),
                    hDivider(),
                    bsCollapse(
                      bsCollapsePanel("Settings",
                                      numericInput("pca.pca.genes.count.from", "Min. # of genes", min = 0, max = 0, value = 0),
                                      numericInput("pca.pca.genes.count.to", "Max. # of genes", min = 0, max = 0, value = 0),
                                      numericInput("pca.pca.genes.count.by", "Animation step", 1, 1000, value = 10),
                                      numericInput("pca.pca.genes.count.animation.speed", "Animation speed (ms)", 100, 1000, value = 100))
                    )),
    bsCollapsePanel("Settings",
                    checkboxInput("pca.pca.settings.center", "Center data", value = T),
                    checkboxInput("pca.pca.settings.scale", "Scale data", value = T))
  ))
  
}

#' Creates definition for the "PCA" sidebar
#' This sidebar allows users to change settings of the currently viewed plot
#' This includes general output settings (DPI, width, height, ...) als well
#' as plot-specific settings such as visible axes or data visualization
#'
#' @return
#' @export
#'
#' @examples
uiPCASidebarPlot <- function() {
  return(verticalLayout(
    conditionalPanel("input['pca.nav'] == 'pca.cells.plot'",
                     bsCollapse(
                       bsCollapsePanel("Axes",
                                       selectizeInput("pca.plot.cells.axes",
                                                      "Visible axes (x, y, z)",
                                                      choices = c(),
                                                      multiple = T,
                                                      options = list(maxItems = 3))),
                       bsCollapsePanel("Visualization",
                                       colorShapeEditorInput("pca.plot.visuals.editor")
                       ),
                       bsCollapsePanel("Output settings")
                     )),
    conditionalPanel("input['pca.nav'] == 'pca.genes.variances'",
                     bsCollapse(
                       bsCollapsePanel("Output settings")
                     )),
    conditionalPanel("input['pca.nav'] == 'pca.pc.importance'",
                     bsCollapse(
                       bsCollapsePanel("Output settings")
                     ))
  ))
}