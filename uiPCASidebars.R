#'
#' UI definition for the sidebars
#' 

library(shiny)
library(shinyBS)

source("uiHelper.R")
source("widgetInPlaceHelp.R")
source("widgetGeneAnnotationKeywordFilter.R")
source("widgetVisualsEditor.R")
source("widgetGeneralPlotSettings.R")
source("widgetGeneAnnotationImporter.R")
source("widgetSampleAnnotationImporter.R")
source("widgetNumericRangeInput.R")
source("widgetExtendedSliderInput.R")
source("widgetInPlaceHelp.R")
source("widgetReadCountPreprocessing.R")
source("widgetReadCountNormalization.R")
source("plotSamplePlot.R")
source("plotGeneVariancePlot.R")
source("plotConditionsVennDiagramPlot.R")
source("plotPCAVariancePlot.R")
source("plotGeneVarianceRangePlot.R")
source("plotAgglomerativeClusteringPlot.R")
source("widgetRelevantGenes.R")
source("widgetReadCountPostprocessing.R")
source("defaultParameters.R")
source("plotLoadingsPlot.R")

#' Creates UI definition for the "data" sidebar
#' This sidebar allows the user to upload necessary data and transform them for later processing
#'
#' @return
#' @export
#'
#' @examples
uiPCASidebarData <- function() {
  
  return(bsCollapse(
    id = "sidebardata",
    multiple = F,
    bsCollapsePanel(requiredDataText("Import read counts"), 
                    value = "data.readcounts.import",
                    genericImporterInput("pca.data.readcounts.importer")),
    bsCollapsePanel(requiredDataText("Import samples annotation"),
                    value = "data.sample.annotation",
                    sampleAnnotationImporterUI("data.sample.annotation.importer")),
    bsCollapsePanel(optionalDataText("Import gene annotation"),
                    value = "data.gene.annotation",
                    geneAnnotationImporterUI("data.gene.annotation.importer")),
    bsCollapsePanel(recommendedDataText("Read count processing"),
                    value = "data.readcounts.processing",
                    readCountPreprocessingUI("data.readcounts.preprocessing"),
                    readCountNormalizationUI("data.readcounts.normalization"),
                    readCountPostprocessingUI("data.readcounts.postprocessing"))
    
                    
    )
  )
  
}

#' Sidebar for filtering of genes based on various criteria
#'
#' @return
#' @export
#'
#' @examples
uiPCASidebarFilterGenes <- function() {
  return(bsCollapse(
    bsCollapsePanel(optionalDataText("by gene annotation"),
                    value = "pca.filter.bygenes",
                    geneAnnotationKeywordFilterInput("pca.pca.genes.set", helpIconText("Limit set of genes", includeText("helptooltips/pca-pca-gene-set.md"))),
                    hDivider(),
                    textOutput("pca.pca.genes.set.count")),
    bsCollapsePanel(optionalDataText("by gene variance"),
                    value = "pca.filter.byvariance",
                    plotGeneVarianceRangePlotUI("pca.pca.genes.count.variance.plot", height = "120px"),
                    extendedSliderInput("pca.genes.count", "Gene variance cut-off"),
                    bsCollapse(
                      bsCollapsePanel(helpIconText("Find minimal threshold", 
                                                   includeText("helptooltips/pca-pca-gene-set-variance-minimum.md")),
                                      value = "Find minimal threshold",
                                      relevantGenesUI("pca.pca.genes.count.findminimal"))
                    )
                    
    ),
    id = "sidebarfilter"))
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
    bsCollapsePanel(recommendedDataText("Data processing"),
                    value = "pca.pca.dataprocessing",
                    checkboxInput("pca.pca.settings.center", 
                                  helpIconText("Center data", includeMarkdown("helptooltips/pca-pca-settings-center.md")), 
                                  value = default.pca.settings.centering),
                    checkboxInput("pca.pca.settings.scale", 
                                  helpIconText("Scale data", includeMarkdown("helptooltips/pca-pca-settings-scale.md")), 
                                  value = default.pca.settings.scaling)
                   ),
    bsCollapsePanel(optionalDataText("Output transformations"),
                    value = "pca.pca.outputtransformations",
                    radioButtons("pca.pca.settings.relative", 
                                 helpIconText("Relative sample positions", includeMarkdown("helptooltips/pca-pca-settings-relative.md")), 
                                 choices = c("None" = "none", "Per dimension" = "dimension", "Global" = "global"),
                                 selected = default.pca.settings.relative))
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
    conditionalPanel("input['pca.nav'] == 'readcounts.processed'", plotAgglomerativeClusteringPlotSettingsUI("readcounts.processed.hclust.plot")),
    conditionalPanel("input['pca.nav'] == 'readcounts.filtered'", plotAgglomerativeClusteringPlotSettingsUI("readcounts.filtered.hclust.plot")),
    conditionalPanel("input['pca.nav'] == 'readcounts.top.variant'", plotAgglomerativeClusteringPlotSettingsUI("readcounts.top.variant.hclust.plot")),
    # Clustering based on PCA
    conditionalPanel("input['pca.nav'] == 'pca.samples.transformed'", plotAgglomerativeClusteringPlotSettingsUI("pca.transformed.hclust.plot")),
    # Sample plot
    conditionalPanel("input['pca.nav'] == 'pca.samples.plot'", plotSamplePlotSettingsUI("pca.samples.plot")),
    # Sample conditions venn diagram plot
    conditionalPanel("input['pca.nav'] == 'samples.conditions'", plotConditionsVennDiagramPlotSettingsUI("samples.conditions.plot")),
    # Gene variances plot
    conditionalPanel("input['pca.nav'] == 'genes.variances'", plotGeneVariancePlotSettingsUI("genes.variances.plot")),
    # Gene variances plot (filtered genes)
    conditionalPanel("input['pca.nav'] == 'genes.variances.filtered'", plotGeneVariancePlotSettingsUI("genes.variances.filtered.plot")),
    # PCA PC variances
    conditionalPanel("input['pca.nav'] == 'pca.pc.importance'", plotPCAVariancePlotSettingsUI("pca.variance.plot")),
    # PCA PC loadings
    conditionalPanel("input['pca.nav'] == 'pca.pc.pc'", plotLoadingsPlotSettingsUI("pca.loadings.plot"))
  ))
}