
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
library(svglite)
source("readcounts.R")
source("geneAnnotation.R")
source("sampleAnnotation.R")
source("sampleAnnotationVisuals.R")
source("pca.R")
source("widgetGenericImporter.R")
source("widgetDownloadableDataTable.R")
source("widgetDownloadablePlot.R")
source("widgetVisualsEditor.R")
source("widgetGeneralPlotSettings.R")
source("widgetExtendedSliderInput.R")
source("widgetProcessingSteps.R")
source("widgetSampleAnnotationImporter.R")
source("widgetGeneAnnotationImporter.R")
source("widgetRelevantGenes.R")
source("widgetReadCountPreprocessing.R")
source("widgetReadCountNormalization.R")
source("serverFunctions.R")
source("helpers.R")
source("classPlotSettings.R")
source("plotSamplePlot.R")
source("plotGeneVariancePlot.R")
source("plotConditionsVennDiagramPlot.R")
source("plotPCAVariancePlot.R")
source("plotGeneVarianceRangePlot.R")
source("plotAgglomerativeClusteringPlot.R")
source("processingReports.R")
source("classDataSet.R")

options(shiny.maxRequestSize=30*1024^2) 

shinyServer(function(input, output, session) {
  
  variables <- reactiveValues(dataset = NULL)
  
  
  dataset.imported.raw <- genericImporterData("pca.data.readcounts.importer", 
                                 importers = reactive(supportedReadcountImporters),
                                 samples = reactive(availableReadcountSamples),
                                 generators = reactive(supportedReadcountGenerators),
                                 exprimport = importReadcount, 
                                 exprsample = importReadcountSample)
  
  observeEvent(dataset.imported.raw(), {
    variables$dataset <- dataset.imported.raw()
  })
  
  dataset.raw <- reactive( { return(variables$dataset) })
  
  # Read counts
  readcounts.raw <- reactive(
    { 
      validate(need(dataset.raw(), "No datset loaded."))
      return(dataset.raw()$readcounts.raw)
    })
  
  # Readcount processing
  
  dataset.preprocessed <- readCountPreprocessingData("data.readcounts.preprocessing", dataset.raw)
  
  readcounts.preprocessing.output <- reactive({ dataset.preprocessed()$readcounts.preprocessing.parameters })
  readcounts.preprocessed <- reactive({ dataset.preprocessed()$readcounts.preprocessed })
  
  dataset.sampleannotation <- sampleAnnotationImporterValue("data.sample.annotation.importer", dataset = dataset.preprocessed)
  
  sample.annotation <- reactive({ 
    validate(need(dataset.sampleannotation(), "No datset loaded."))
    validate(need(dataset.sampleannotation()$sample.annotation, "No sample annotation available."))
    
    return(dataset.sampleannotation()$sample.annotation)
    
    })
    
  conditions <- reactive({
    validate(need(sample.annotation(), "No samples annotation available!"))
    return(sample.annotation()@conditions)
  })
  
  # Fetch gene info annotation with an integrating generic importer.
  # This allows the user to provide multiple data source with only one UI and feedback what was found
  dataset.gene.annotation <- geneAnnotationImporterValue("data.gene.annotation.importer", dataset = dataset.sampleannotation)
  gene.annotation <- reactive({
    validate(need(dataset.gene.annotation(), "No gene annotation available!"))
    return(dataset.gene.annotation()$gene.annotation)
  })
  
  # Finish processing of read counts with normalization
  dataset.normalized <- readCountNormalizationData("data.readcounts.normalization", 
                                                   dataset = dataset.gene.annotation)
  readcounts.normalization.output <- reactive({ 
    validate(need(dataset.normalized(), "No normalized read counts available!"))
    return(dataset.normalized()$readcounts.normalization.parameters)
    })
  readcounts.normalized <- reactive({ 
    validate(need(dataset.normalized(), "No normalized read counts available!"))
    return(dataset.normalized()$readcounts.normalized )
    })
  
  # Additional postprocessing after normalization
  dataset.postprocessed <- readCountPostprocessingData("data.readcounts.postprocessing", dataset = dataset.normalized)
  
  readcounts.processed <- reactive({
    validate(need(dataset.postprocessed(), "No processed read counts available!"))
    return(dataset.postprocessed()$readcounts.processed )
  })
  
  readcounts.postprocessing.output <- reactive({
    validate(need(dataset.postprocessed(), "No processed read counts available!"))
    return(dataset.postprocessed()$readcounts.postprocessing.parameters )
  })
  
  # Obtain the list of genes the user wants to use
  # The filtered read counts just intersects the list of genes returned by each filter
  dataset.filtered <- serverFilterReadcountsByAnnotation(dataset.postprocessed)
  genes.filtered <- reactive({
    validate(need(dataset.filtered(), "No filtered read counts available!"))
    return(dataset.filtered()$readcounts.filtered.parameters.genes)
  })
  readcounts.filtered <- reactive({
    validate(need(dataset.filtered(), "No filtered read counts available!"))
    return(dataset.filtered()$readcounts.filtered)
  })
  
  # Annotate gene variances
  dataset.variances <- reactive({
    validate(need(dataset.filtered(), "No filtered read counts available!"))
    
    dataset <- dataset.filtered()
    dataset$variances.processed <- buildGeneVarianceTable(dataset.filtered()$readcounts.processed)
    dataset$variances.filtered <- buildGeneVarianceTable(dataset.filtered()$readcounts.filtered)
    
    return(dataset)
  })
  
  readcounts.processed.variances <- reactive( { 
    validate(need(dataset.variances(), "No filtered read counts available!")) 
    return(dataset.variances()$variances.processed)
  })
  readcounts.filtered.variances <- reactive( { 
    validate(need(dataset.variances(), "No filtered read counts available!")) 
    return(dataset.variances()$variances.filtered)
  })
  
  # The next step is to filter our genes based on the annotation and then select the top n most variant genes
  # Here we also include the hook for the minimal gene set (threshold) calculation
  dataset.top.variant <- serverFilterReadCountsByVariance(dataset.variances)
  pca.gene.count <- reactive({
    validate(need(dataset.top.variant(), "No top variant read counts available!")) 
    return(dataset.top.variant()$readcounts.top.variant.parameters.count)
  })
  readcounts.top.variant <- reactive({
    validate(need(dataset.top.variant(), "No top variant read counts available!")) 
    return(dataset.top.variant()$readcounts.top.variant)
  })
  
  # pca is applied to the selected genes and setup some values to be used by outputs
  pca <- serverPCA(reactive({input$pca.pca.settings.center}),
                   reactive({input$pca.pca.settings.scale}),
                   reactive({input$pca.pca.settings.relative}), 
                   readcounts.top.variant)
  
  dataset.pca <- reactive({
    validate(need(dataset.top.variant(), "No top variant read counts available"))
    validate(need(pca(), "No PCA results available"))
    
    dataset <- dataset.top.variant()
    dataset$pca.top.variant <- pca()
    return(dataset)
  })
  
  #
  # Update input elements
  #
  
  # For navigation (links)
  serverNavigation(input, session)
  
  # Auto navigation
  serverAutoNavigation(input, session)
  
  # Readcounts
  downloadableDataTable("readcounts", export.filename = "readcounts", data = readcounts.raw)
  
  downloadableDataTable("readcounts.processed", export.filename = "readcounts.processed", data = readcounts.processed)
  plotAgglomerativeClusteringPlot("readcounts.processed.hclust.plot", conditions, readcounts.processed, default.title = reactive({ "Processed read counts clustering" }))
  
  downloadableDataTable("readcounts.filtered", export.filename = "readcounts.filtered", data = readcounts.filtered)
  plotAgglomerativeClusteringPlot("readcounts.filtered.hclust.plot", conditions, readcounts.filtered, default.title = reactive({ "Filtered read counts clustering" }))
  
  downloadableDataTable("readcounts.top.variant", export.filename = "readcounts.top.variant", data = readcounts.top.variant)
  plotAgglomerativeClusteringPlot("readcounts.top.variant.hclust.plot", conditions, readcounts.top.variant, default.title = reactive({ "Top variant read counts clustering" }))
  
  # Sample annotation
  downloadableDataTable("samples.annotation", export.filename = "annotation", data = reactive({ sampleAnnotationToTable(sample.annotation()) }))
  downloadableDataTable("samples.conditions", export.filename = "conditions", data = conditions)
  plotConditionsVennDiagramPlot("samples.conditions.plot", conditions = conditions)
  
  
  downloadableDataTable("genes.variance", export.filename = "variance", data = serverGeneVarianceTableData(readcounts.processed.variances))
  downloadableDataTable("genes.variance.filtered", export.filename = "variance", data = serverGeneVarianceTableData(readcounts.filtered.variances))
  downloadableDataTable("genes.annotation", export.filename = "annotation", data = serverGeneAnnotationTableData(readcounts, gene.annotation))
  
  # Gene filtering
  output$pca.pca.genes.set.count <- renderText({
    validate(need(readcounts.filtered(), "0 genes selected"))
    return(paste(nrow(readcounts.filtered()), "genes selected"))
  })
  
  # Read count processing widget output
  readcountProcessing.at.readcounts.processed(dataset.pca)
  readcountProcessing.at.readcounts.filtered(dataset.pca)
  readcountProcessing.at.readcounts.top.variant(dataset.pca)
  readcountProcessing.at.pca(dataset.pca)
  
  # Fill tables & plots
  downloadableDataTable("pca.transformed", export.filename = "pca.transformed", data = reactive({ pca()$transformed })) 
  downloadableDataTable("pca.pc", export.filename = "pca.pc", data = reactive({ pca()$pc }))
  downloadableDataTable("pca.variance", export.filename = "pca.var", data = reactive({ pca()$var }))
  
  plotGeneVariancePlot("genes.variances.plot", gene.variances = readcounts.processed.variances)
  plotGeneVariancePlot("genes.variances.filtered.plot", gene.variances = readcounts.filtered.variances)
  
  plotGeneVarianceRangePlot("pca.pca.genes.count.variance.plot", dataset.top.variant)
  
  plotPCAVariancePlot("pca.variance.plot", pca)
  
  plotAgglomerativeClusteringPlot("pca.transformed.hclust.plot", 
                                  conditions, 
                                  reactive({ t(pca()$transformed) }),
                                  default.title = reactive({ "PCA transformed values clustering" }))
  
  plotSamplePlot("pca.samples.plot",
               readcounts.processed = readcounts.processed,
               readcounts.filtered = readcounts.filtered,
               readcounts.top.variant = readcounts.top.variant,
               gene.variances = readcounts.filtered.variances, # Important! Needed for movie function!
               animation.params = pca.gene.count,
               conditions = conditions,
               pca.center = reactive({input$pca.pca.settings.center}),
               pca.scale = reactive({input$pca.pca.settings.scale}),
               pca.relative = reactive({input$pca.pca.settings.relative}))
})
