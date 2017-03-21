#'
#' UI definition for the sidebars
#' 

library(shiny)
library(shinyBS)

source("uiHelper.R")
source("widgetInPlaceHelp.R")
source("widgetFilterSelection.R")
source("widgetColorShapeInput.R")
source("widgetGeneralPlotSettings.R")
source("widgetCellConditionImporter.R")
source("widgetNumericRangeInput.R")
source("widgetExtendedSliderInput.R")
source("widgetInPlaceHelp.R")

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
                                      genericImporterInput("pca.data.readcounts.importer",
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
                    bsCollapse(
                      bsCollapsePanel("Associated features", genericImporterInput("pca.data.annotation.importer",
                                                                                  supportedAnnotationFileTypes,
                                                                                  supportedAnnotationImporters,
                                                                                  availableAnnotationSamples)),
                      bsCollapsePanel("GO terms")
                    )
                    
    ),
    bsCollapsePanel("Conditions",
                    value = "conditions",
                    cellConditionImporterUI("conditions.importer"))
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
    bsCollapsePanel("Filter genes",
                    filterSelectionInput("pca.pca.genes.set", helpIconText("Limit set of genes", includeText("helptooltips/pca-pca-gene-set.md"))),
                    hDivider(),
                    textOutput("pca.pca.genes.set.count")),
    bsCollapsePanel("Gene count",
                    plotOutput("pca.pca.genes.count.variance.plot", height = "120px"),
                    extendedSliderInput("pca.genes.count", "Gene count")
                    
                    ),
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
                                       selectizeInput("pca.cells.plot.axes",
                                                      "Visible axes (x, y, z)",
                                                      choices = c(),
                                                      multiple = T,
                                                      options = list(maxItems = 3, plugins = c("remove_button", "drag_drop")))),
                       bsCollapsePanel("Visualization",
                                       colorShapeEditorInput("pca.cells.plot.visuals")
                       ),
                       bsCollapsePanel("General settings", generalPlotSettingsInput("pca.cells.plot.generalsettings"))
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