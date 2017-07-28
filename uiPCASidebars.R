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
source("widgetGeneAnnotationImporter.R")
source("widgetSampleAnnotationImporter.R")
source("widgetNumericRangeInput.R")
source("widgetExtendedSliderInput.R")
source("widgetInPlaceHelp.R")
source("plotSamplePlot.R")
source("plotGeneVariancePlot.R")
source("plotConditionsVennDiagramPlot.R")
source("plotPCAVariancePlot.R")
source("plotGeneVarianceRangePlot.R")
source("plotAgglomerativeClusteringPlot.R")
source("widgetRelevantGenes.R")

#' Creates UI definition for the "data" sidebar
#' This sidebar allows the user to upload necessary data and transform them for later processing
#'
#' @return
#' @export
#'
#' @examples
uiPCASidebarData <- function() {
  
  return(bsCollapse(
    bsCollapsePanel("Import read counts", 
                    value = "data.readcounts.import",
                    genericImporterInput("pca.data.readcounts.importer")),
    bsCollapsePanel("Import samples annotation",
                    value = "data.sample.annotation",
                    sampleAnnotationImporterUI("data.sample.annotation.importer")),
    bsCollapsePanel("Import gene annotation",
                    value = "data.gene.annotation",
                    geneAnnotationImporterUI("data.gene.annotation.importer")),
    bsCollapsePanel("Read count processing",
                    value = "data.readcounts.processing",
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
                                     checkboxInput("pca.data.normalization.tpm.effectivelength", "Use effective fragment length", value = T),
                                     checkboxInput("pca.data.normalization.tpm.exonlength", "Use feature exon length", value = T)),
                    conditionalPanel("input['pca.data.normalization'] == 'deseq2'",
                                     selectizeInput("pca.data.normalization.deseq2.conditions", 
                                                  helpIconText("Considered conditions", "You can choose which available conditions are considered during normalization."), 
                                                  choices = c(),
                                                  multiple = T,
                                                  options = list(plugins = list("remove_button", "drag_drop")))))
    
                    
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
    bsCollapsePanel("by annotation",
                    filterSelectionInput("pca.pca.genes.set", helpIconText("Limit set of genes", includeText("helptooltips/pca-pca-gene-set.md"))),
                    hDivider(),
                    textOutput("pca.pca.genes.set.count")),
    bsCollapsePanel("by gene variance",
                    plotGeneVarianceRangePlotUI("pca.pca.genes.count.variance.plot", height = "120px"),
                    extendedSliderInput("pca.genes.count", "Gene variance cut-off"),
                    bsCollapse(
                      bsCollapsePanel(helpIconText("Find minimal threshold", 
                                                   includeText("helptooltips/pca-pca-gene-set-variance-minimum.md")),
                                      value = "Find minimal threshold",
                                      relevantGenesUI("pca.pca.genes.count.findminimal"))
                    )
                    
    )))
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
    bsCollapsePanel("Data processing",
                    checkboxInput("pca.pca.settings.center", 
                                  helpIconText("Center data", includeMarkdown("helptooltips/pca-pca-settings-center.md")), 
                                  value = T),
                    checkboxInput("pca.pca.settings.scale", 
                                  helpIconText("Scale data", includeMarkdown("helptooltips/pca-pca-settings-scale.md")), 
                                  value = F)
                   ),
    bsCollapsePanel("Output transformations",
                    radioButtons("pca.pca.settings.relative", 
                                 helpIconText("Relative sample positions", includeMarkdown("helptooltips/pca-pca-settings-relative.md")), 
                                 choices = c("None" = "none", "Per dimension" = "dimension", "Global" = "global")))
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
    conditionalPanel("input['pca.nav'] == 'pca.pc.importance'", plotPCAVariancePlotSettingsUI("pca.variance.plot"))
  ))
}