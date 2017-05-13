
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# For production environment:
# require(compiler)
# enableJIT(3)

library(RColorBrewer)
library(scatterplot3d)
library(ggplot2)
library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
library(colourpicker)
source("readcounts.R")
source("geneAnnotation.R")
source("cellAnnotation.R")
source("cellAnnotationVisuals.R")
source("pca.R")
source("widgetGenericImporter.R")
source("widgetDownloadableDataTable.R")
source("widgetDownloadablePlot.R")
source("widgetVisualsEditor.R")
source("widgetGeneralPlotSettings.R")
source("widgetExtendedSliderInput.R")
source("widgetProcessingSteps.R")
source("serverFunctions.R")
source("helpers.R")
source("classPlotSettings.R")
source("plotCellPlot.R")
source("plotGeneVariancePlot.R")
source("plotConditionsVennDiagramPlot.R")
source("plotPCAVariancePlot.R")
source("plotGeneVarianceRangePlot.R")
source("plotAgglomerativeClusteringPlot.R")
source("readcountProcessing.R")

shinyServer(function(input, output, session) {
  
  # Read counts
  readcounts.raw <- genericImporterData("pca.data.readcounts.importer", 
                                    importers = reactive(supportedReadcountImporters),
                                    samples = reactive(availableReadcountSamples),
                                    generators = reactive(supportedReadcountGenerators),
                                    exprimport = importReadcount, 
                                    exprsample = importReadcountSample)
  
  # Readcount processing
  readcounts.preprocessing.output <- serverReadCountPreProcessing(readcounts.raw, input)
  readcounts.preprocessed <- reactive({ readcounts.preprocessing.output()$readcounts })
  
 
  cell.annotation <- cellAnnotationImporterValue("conditions.importer", readcounts = readcounts.preprocessed)
  conditions <- reactive({
    validate(need(cell.annotation(), "No cell annotation available!"))
    return(cell.annotation()$conditions)
  })
  
  observeEvent(conditions(), {
    
    validate(need(conditions(), "No conditions available!"))
    
    updateSelectizeInput(session, 
                         "pca.data.normalization.deseq2.conditions",
                         choices = colnames(conditions()),
                         selected = colnames(conditions()))
  })
  
  # Fetch gene info annotation with an integrating generic importer.
  # This allows the user to provide multiple data source with only one UI and feedback what was found
  gene.info.annotation <- serverGeneInfoAnnotation(readcounts.preprocessed)
  
  # Finish processing of read counts with normalization
  readcounts.normalization.output <- serverReadcountNormalization(readcounts.preprocessed, conditions, gene.info.annotation, input)
  readcounts.processed <- reactive({ readcounts.normalization.output()$readcounts })
  
  # Gene variances
  gene.variances <- reactive( { buildGeneVarianceTable(readcounts.processed()) } )
  
  # Obtain the list of genes the user wants to use
  genes.filtered <- serverFilteredGenes(readcounts.processed, gene.info.annotation)
  
  # The filtered read counts just intersects the list of genes returned by each filter
  readcounts.filtered <- serverFilterReadcounts(genes.filtered, readcounts.processed)
  
  gene.variances.filtered <- reactive( { buildGeneVarianceTable(readcounts.filtered()) } )
  
  # The next step is to filter our genes based on the annotation and then select the top n most variant genes
  pca.gene.count <- extendedSliderInputValue("pca.genes.count", 
                                             value.min = reactive({ 1 }),
                                             value.max = reactive({ nrow(readcounts.filtered()) }),
                                             value.default = reactive({ nrow(readcounts.filtered()) }))
  readcounts.top.variant <- reactive({ selectTopVariantGeneReadcounts(readcounts.filtered(), gene.variances(), pca.gene.count()$value) })
  
  # pca is applied to the selected genes and setup some values to be used by outputs
  pca <- serverPCA(input, readcounts.top.variant)
  
  #
  # Update input elements
  #
  
  # For navigation (links)
  serverNavigation(input, session)
  
  # Readcounts
  downloadableDataTable("readcounts", export.filename = "readcounts", data = readcounts.raw)
  
  downloadableDataTable("readcounts.processed", export.filename = "readcounts.processed", data = readcounts.processed)
  plotAgglomerativeClusteringPlot("readcounts.processed.hclust.plot", conditions, readcounts.processed, default.title = reactive({ "Processed read counts clustering" }))
  
  downloadableDataTable("readcounts.filtered", export.filename = "readcounts.filtered", data = readcounts.filtered)
  plotAgglomerativeClusteringPlot("readcounts.filtered.hclust.plot", conditions, readcounts.filtered, default.title = reactive({ "Filtered read counts clustering" }))
  
  downloadableDataTable("readcounts.top.variant", export.filename = "readcounts.top.variant", data = readcounts.top.variant)
  plotAgglomerativeClusteringPlot("readcounts.top.variant.hclust.plot", conditions, readcounts.top.variant, default.title = reactive({ "Top variant read counts clustering" }))
  
  # Cell condition assingments
  downloadableDataTable("pca.cells.conditions", export.filename = "conditions", data = conditions)
  plotConditionsVennDiagramPlot("pca.cells.conditions.plot", conditions = conditions)
  
  downloadableDataTable("genes.variance", export.filename = "variance", data = serverGeneVarianceTableData(gene.variances))
  downloadableDataTable("genes.variance.filtered", export.filename = "variance", data = serverGeneVarianceTableData(gene.variances.filtered))
  downloadableDataTable("genes.annotation", export.filename = "annotation", data = serverGeneAnnotationTableData(readcounts, gene.info.annotation))
  
  # Gene filtering
  output$pca.pca.genes.set.count <- renderText({
    validate(need(readcounts.filtered(), "0 genes selected"))
    return(paste(nrow(readcounts.filtered()), "genes selected"))
  })
 
  # Read count processing widget output
  readcountProcessing.at.readcounts.processed(input = input, 
                                              readcounts.raw = readcounts.raw,
                                              readcounts.processed = readcounts.processed, 
                                              readcounts.preprocessing.output = readcounts.preprocessing.output, 
                                              readcounts.normalization.output = readcounts.normalization.output)
  readcountProcessing.at.readcounts.filtered(input = input, 
                                             readcounts.raw = readcounts.raw,
                                             readcounts.processed = readcounts.processed, 
                                             readcounts.filtered = readcounts.filtered,
                                             readcounts.preprocessing.output = readcounts.preprocessing.output, 
                                             readcounts.normalization.output = readcounts.normalization.output,
                                             genes.filtered = genes.filtered)
  readcountProcessing.at.readcounts.top.variant(input = input, 
                                                readcounts.raw = readcounts.raw,
                                             readcounts.processed = readcounts.processed, 
                                             readcounts.filtered = readcounts.filtered,
                                             readcounts.top.variant = readcounts.top.variant,
                                             readcounts.preprocessing.output = readcounts.preprocessing.output, 
                                             readcounts.normalization.output = readcounts.normalization.output,
                                             genes.filtered = genes.filtered)
  readcountProcessing.at.pca(input = input, 
                             readcounts.raw = readcounts.raw,
                            readcounts.processed = readcounts.processed, 
                            readcounts.filtered = readcounts.filtered,
                            readcounts.top.variant = readcounts.top.variant,
                            readcounts.preprocessing.output = readcounts.preprocessing.output, 
                            readcounts.normalization.output = readcounts.normalization.output,
                            genes.filtered = genes.filtered,
                             pca = pca)
  
  # Fill tables & plots
  downloadableDataTable("pca.transformed", export.filename = "pca.transformed", data = reactive({ pca()$transformed })) 
  downloadableDataTable("pca.pc", export.filename = "pca.pc", data = reactive({ pca()$pc }))
  downloadableDataTable("pca.variance", export.filename = "pca.var", data = reactive({ pca()$var }))
  
  plotGeneVariancePlot("pca.genes.variances.plot", gene.variances = gene.variances)
  plotGeneVariancePlot("pca.genes.variances.filtered.plot", gene.variances = gene.variances.filtered)
  
  plotGeneVarianceRangePlot("pca.pca.genes.count.variance.plot", pca.gene.count, gene.variances.filtered)
  
  plotPCAVariancePlot("pca.variance.plot", pca)
  
  plotAgglomerativeClusteringPlot("pca.transformed.hclust.plot", 
                                  conditions, 
                                  reactive({ t(pca()$transformed) }),
                                  default.title = reactive({ "PCA transformed values clustering" }))
  
  plotCellPlot("pca.cells.plot",
               readcounts.processed = readcounts.processed,
               readcounts.filtered = readcounts.filtered,
               readcounts.top.variant = readcounts.top.variant,
               gene.variances = gene.variances,
               animation.params = pca.gene.count,
               conditions = conditions,
               pca = pca)
})
