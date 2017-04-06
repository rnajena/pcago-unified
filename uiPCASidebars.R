#'
#' UI definition for the sidebars
#' 

library(shiny)
library(shinyBS)

source("uiHelper.R")
source("widgetInPlaceHelp.R")
source("widgetFilterSelection.R")
source("widgetVisualsEditor.R")
source("widgetGeneralPlotSettings.R")
source("widgetCellConditionImporter.R")
source("widgetNumericRangeInput.R")
source("widgetExtendedSliderInput.R")
source("widgetInPlaceHelp.R")
source("widgetIntegratingGenericImporter.R")
source("plotCellPlot.R")

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
                                      genericImporterInput("pca.data.readcounts.importer")),
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
                                                   choices = supportedReadcountNormalizationTypes),
                                      conditionalPanel("input['pca.data.normalization'] == 'tpm'",
                                                       numericInput("pca.data.normalization.tpm.mufld", "Mean fragment length", value = 0)))
                    )
    ),
    bsCollapsePanel("Annotation",
                    value = "annotation",
                    integratingGenericImporterInput("pca.data.annotation.importer")
                    
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
                    checkboxInput("pca.pca.settings.center", helpIconText("Center data", includeMarkdown("helptooltips/pca-pca-settings-center.md")), value = T),
                    checkboxInput("pca.pca.settings.scale", helpIconText("Scale data", includeMarkdown("helptooltips/pca-pca-settings-scale.md")), value = T),
                    checkboxInput("pca.pca.settings.relative", helpIconText("Relative cell positions", includeMarkdown("helptooltips/pca-pca-settings-relative.md")), value = F))
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
    # # Readcount clustering
    # conditionalPanel("input['pca.nav'] == 'pca.cluster.readcounts.processed'",
    #                  bsCollapse(
    #                    bsCollapsePanel("Clustering",
    #                                    selectizeInput("pca.cluster.readcounts.processed.plot.method.distance", "Distance method", choices = dist.methodsSelection),
    #                                    selectizeInput("pca.cluster.readcounts.processed.plot.method.clustering", "Clustering method", choices = hclust.methodsSelection)
    #                                    ),
    #                    bsCollapsePanel("Visualization", visualsEditorUI("pca.cluster.readcounts.processed.plot.visuals")),
    #                    bsCollapsePanel("General settings", generalPlotSettingsInput("pca.cluster.readcounts.processed.plot.generalsettings"))
    #                  )),
    # Cell plot
    conditionalPanel("input['pca.nav'] == 'pca.cells.plot'", plotCellPlotSettingsUI("pca.cells.plot")),
    # Cell conditions venn diagram plot
    conditionalPanel("input['pca.nav'] == 'pca.conditions'",
                     bsCollapse(
                       bsCollapsePanel("Venn diagram",selectizeInput("conditions.plot.sets", "Displayed sets", 
                                                                     choices = c(),
                                                                     multiple = T,
                                                                     options = list(maxItems = 5))),
                       bsCollapsePanel("Visualization", visualsEditorUI("pca.conditions.plot.visuals")),
                       bsCollapsePanel("General settings", generalPlotSettingsInput("pca.conditions.plot.generalsettings"))
                     )),
    # Gene variances plot
    conditionalPanel("input['pca.nav'] == 'pca.genes.variances'",
                     bsCollapse(
                       bsCollapsePanel("General settings", generalPlotSettingsInput("pca.genes.variances.generalsettings"))
                     )),
    # Gene variances plot (filtered genes)
    conditionalPanel("input['pca.nav'] == 'pca.genes.variances.filtered'",
                     bsCollapse(
                       bsCollapsePanel("General settings", generalPlotSettingsInput("pca.genes.variances.filtered.generalsettings"))
                     )),
    # PCA PC variances
    conditionalPanel("input['pca.nav'] == 'pca.pc.importance'",
                     bsCollapse(
                       bsCollapsePanel("General settings", generalPlotSettingsInput("pca.pc.importance.generalsettings"))
                     ))
  ))
}