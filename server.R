
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
source("plotLoadingsPlot.R")

options(shiny.maxRequestSize=30*1024^2) 

shinyServer(function(input, output, session) {
  
  # xautovars used for automatic i/o
  xautovars <- reactiveValues(import.readcounts.raw = NULL,
                              import.sample.annotation = NULL,
                              import.stage = "null",
                              import.ask = F,
                              export.readcounts.raw = NULL,
                              export.readcounts.processed = NULL,
                              export.readcounts.filtered = NULL,
                              export.readcounts.top.variant = NULL,
                              export.readcounts.pca.transformed = NULL,
                              export.sample.annotation.conditions = NULL,
                              export.sample.annotation.sampleinfo = NULL,
                              export.gene.annotation = NULL,
                              export.gene.variances.processed = NULL,
                              export.gene.variances.filtered = NULL,
                              export.pca.pc = NULL,
                              export.pca.variances = NULL,       
                              export.plot.clustering.readcounts.processed = NULL, 
                              export.plot.clustering.readcounts.filtered = NULL,        
                              export.plot.clustering.readcounts.top.variant = NULL,     
                              export.plot.clustering.readcounts.pca.transformed = NULL,         
                              export.plot.venn.conditions = NULL,
                              export.plot.pca.sampleplot = NULL,
                              export.plot.pca.variance = NULL,
                              export.plot.pca.loadings = NULL,
                              export.plot.variances.readcounts.processed = NULL,
                              export.plot.variances.readcounts.filtered = NULL,
                              export.count = 0)
  
  
  dataset.raw <- genericImporterData("pca.data.readcounts.importer", 
                                 importers = reactive(supportedReadcountImporters),
                                 samples = reactive(availableReadcountSamples),
                                 generators = reactive(supportedReadcountGenerators),
                                 exprimport = importReadcount, 
                                 exprsample = importReadcountSample,
                                 xauto = reactive({xautovars$import.readcounts.raw }))
  
  observeEvent(dataset.raw(), {
    xautovars$import.ask <- T
  })
  
  # Read counts
  readcounts.raw <- reactive(
    { 
      validate(need(dataset.raw(), "No datset loaded."))
      return(dataset.raw()$readcounts.raw)
    })
  
  # Readcount processing
  
  dataset.preprocessed <- readCountPreprocessingData("data.readcounts.preprocessing", dataset.raw)
  readcounts.preprocessed <- reactive({ dataset.preprocessed()$readcounts.preprocessed })
  
  dataset.sampleannotation <- sampleAnnotationImporterValue("data.sample.annotation.importer", 
                                                            dataset = dataset.preprocessed,
                                                            xauto = reactive({xautovars$import.sample.annotation }))
  
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
  
  # Obtain the list of genes the user wants to use
  # The filtered read counts just intersects the list of genes returned by each filter
  dataset.filtered.keywords <- serverFilterReadcountsByAnnotationKeywords(dataset.postprocessed)
  
  genes.filtered.keywords <- reactive({
    validate(need(dataset.filtered.keywords(), "No filtered read counts available!"))
    return(dataset.filtered()$readcounts.filtered.keywords.parameters.genes)
  })
  
  readcounts.filtered <- reactive({
    validate(need(dataset.filtered(), "No filtered read counts available!"))
    return(dataset.filtered()$readcounts.filtered)
  })
  
  # Final filtered data set (just link it together nicely)
  dataset.filtered <- reactive({
    dataset <- dataset.filtered.keywords()
    dataset$readcounts.filtered <- dataset$readcounts.filtered.keywords
    return(dataset)
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
  
  animation.top.variant <- extendedSliderInputValue("pca.genes.count", 
                                             value.min = reactive({ 1 }),
                                             value.max = reactive({ nrow(readcounts.filtered()) }),
                                             value.default = reactive({ nrow(readcounts.filtered()) }))
  
  dataset.top.variant <- serverFilterReadCountsByVariance(dataset.variances, animation.top.variant = animation.top.variant, input = input)
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
  xauto.export.readcounts.raw <- downloadableDataTable("readcounts", export.filename = "readcounts", data = readcounts.raw, xauto = reactive({ xautovars$export.readcounts.raw }))
  
  xauto.export.readcounts.processed <- downloadableDataTable("readcounts.processed", export.filename = "readcounts.processed", data = readcounts.processed, xauto = reactive({ xautovars$export.readcounts.processed }))
  xauto.export.plot.clustering.readcounts.processed <- plotAgglomerativeClusteringPlot("readcounts.processed.hclust.plot", conditions, readcounts.processed, default.title = reactive({ "Processed read counts clustering" }), xauto = reactive({ xautovars$export.plot.clustering.readcounts.processed }))
  
  xauto.export.readcounts.filtered <- downloadableDataTable("readcounts.filtered", export.filename = "readcounts.filtered", data = readcounts.filtered, xauto = reactive({ xautovars$export.readcounts.filtered }))
  xauto.export.plot.clustering.readcounts.filtered <- plotAgglomerativeClusteringPlot("readcounts.filtered.hclust.plot", conditions, readcounts.filtered, default.title = reactive({ "Filtered read counts clustering" }), xauto = reactive({ xautovars$export.plot.clustering.readcounts.filtered }))
  
  xauto.export.readcounts.top.variant <- downloadableDataTable("readcounts.top.variant", export.filename = "readcounts.top.variant", data = readcounts.top.variant, xauto = reactive({ xautovars$export.readcounts.top.variant }))
  xauto.export.plot.clustering.readcounts.top.variant <- plotAgglomerativeClusteringPlot("readcounts.top.variant.hclust.plot", conditions, readcounts.top.variant, default.title = reactive({ "Top variant read counts clustering" }), xauto = reactive({ xautovars$export.plot.clustering.readcounts.top.variant }))
  
  # Sample annotation
  xauto.export.sample.annotation.sampleinfo <- downloadableDataTable("samples.annotation", export.filename = "annotation", data = reactive({ sampleAnnotationToTable(sample.annotation()) }), xauto = reactive({ xautovars$export.sample.annotation.sampleinfo }))
  xauto.export.sample.annotation.conditions <- downloadableDataTable("samples.conditions", export.filename = "conditions", data = conditions, xauto = reactive({ xautovars$export.sample.annotation.conditions }))
  xauto.export.plot.venn.conditions <- plotConditionsVennDiagramPlot("samples.conditions.plot", conditions = conditions, xauto = reactive({ xautovars$export.plot.venn.conditions }))
  
  # Gene annotation
  xauto.export.gene.variances.processed <- downloadableDataTable("genes.variance", export.filename = "variance", data = serverGeneVarianceTableData(readcounts.processed.variances), xauto = reactive({ xautovars$export.gene.variances.processed }))
  xauto.export.gene.variances.filtered <- downloadableDataTable("genes.variance.filtered", export.filename = "variance", data = serverGeneVarianceTableData(readcounts.filtered.variances), xauto = reactive({ xautovars$export.gene.variances.filtered }))
  xauto.export.gene.annotation <- downloadableDataTable("genes.annotation", export.filename = "annotation", data = serverGeneAnnotationTableData(readcounts, gene.annotation), xauto = reactive({ xautovars$export.gene.annotation }))
  
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
  xauto.export.readcounts.pca.transformed <- downloadableDataTable("pca.transformed", export.filename = "pca.transformed", data = reactive({ pca()$transformed }), xauto = reactive({ xautovars$export.readcounts.pca.transformed })) 
  xauto.export.pca.pc <- downloadableDataTable("pca.pc", export.filename = "pca.pc", data = reactive({ pca()$pc }), xauto = reactive({ xautovars$export.pca.pc }))
  xauto.export.pca.variances <- downloadableDataTable("pca.variance", export.filename = "pca.var", data = reactive({ pca()$var }), xauto = reactive({ xautovars$export.pca.variances }))
  
  xauto.export.plot.variances.readcounts.processed <- plotGeneVariancePlot("genes.variances.plot", gene.variances = readcounts.processed.variances, xauto = reactive({ xautovars$export.plot.variances.readcounts.processed }))
  xauto.export.plot.variances.readcounts.filtered <- plotGeneVariancePlot("genes.variances.filtered.plot", gene.variances = readcounts.filtered.variances, xauto = reactive({ xautovars$export.plot.variances.readcounts.filtered }))
  
  plotGeneVarianceRangePlot("pca.pca.genes.count.variance.plot", dataset.top.variant)
  
  xauto.export.plot.pca.variance <- plotPCAVariancePlot("pca.variance.plot", pca, xauto = reactive({ xautovars$export.plot.pca.variance }))
  
  xauto.export.plot.pca.loadings <- plotLoadingsPlot("pca.loadings.plot",
                                                   dataset = dataset.pca,
                                                   pca.center = reactive({input$pca.pca.settings.center}),
                                                   pca.scale = reactive({input$pca.pca.settings.scale}),
                                                   pca.relative = reactive({input$pca.pca.settings.relative}),
                                                   xauto = reactive({ xautovars$export.plot.pca.loadings }))
  
  xauto.export.plot.clustering.readcounts.pca.transformed <- plotAgglomerativeClusteringPlot("pca.transformed.hclust.plot", 
                                  conditions, 
                                  reactive({ t(pca()$transformed) }),
                                  default.title = reactive({ "PCA transformed values clustering" }),
                                  xauto = reactive({ xautovars$export.plot.clustering.readcounts.pca.transformed }))
  
  xauto.export.plot.pca.sampleplot <- plotSamplePlot("pca.samples.plot",
               dataset = dataset.pca,
               animation.params = animation.top.variant,
               pca.center = reactive({input$pca.pca.settings.center}),
               pca.scale = reactive({input$pca.pca.settings.scale}),
               pca.relative = reactive({input$pca.pca.settings.relative}),
               xauto = reactive({ xautovars$export.plot.pca.sampleplot }))
  
  # QuickIO
  serverQuickIO(input =  input, 
                output =  output, 
                session =  session, 
                xautovars =  xautovars, 
                dataset.preprocessed = dataset.preprocessed, 
                dataset.pca = dataset.pca,
                export.targets = list(
    xauto.export.readcounts.raw,
    xauto.export.readcounts.processed,
    xauto.export.readcounts.filtered,
    xauto.export.readcounts.top.variant,
    xauto.export.readcounts.pca.transformed,
    xauto.export.sample.annotation.conditions,
    xauto.export.sample.annotation.sampleinfo,
    xauto.export.gene.variances.processed,
    xauto.export.gene.variances.filtered,
    xauto.export.gene.annotation,
    xauto.export.pca.pc,
    xauto.export.pca.variances,
    xauto.export.plot.clustering.readcounts.processed,
    xauto.export.plot.clustering.readcounts.filtered,
    xauto.export.plot.clustering.readcounts.top.variant,
    xauto.export.plot.clustering.readcounts.pca.transformed,
    xauto.export.plot.venn.conditions,
    xauto.export.plot.pca.sampleplot,
    xauto.export.plot.variances.readcounts.processed,
    xauto.export.plot.variances.readcounts.filtered,
    xauto.export.plot.pca.variance,
    xauto.export.plot.pca.loadings
  ))
})
