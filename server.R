
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(RColorBrewer)
library(scatterplot3d)
library(ggplot2)
library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
library(colourpicker)
source("readcounts.R")
source("annotation.R")
source("conditions.R")
source("pca.R")
source("plots.R")
source("widgetGenericImporter.R")
source("widgetDownloadableDataTable.R")
source("widgetDownloadablePlot.R")
source("widgetVisualsEditor.R")
source("widgetGeneralPlotSettings.R")
source("widgetExtendedSliderInput.R")
source("serverFunctions.R")
source("helpers.R")
source("classPlotSettings.R")
source("plotCellPlot.R")
source("plotGeneVariancePlot.R")
source("plotConditionsVennDiagramPlot.R")

shinyServer(function(input, output, session) {
  
  # observe({
  #   showNotification("Test.Default", duration = NULL, type = "default")
  #   showNotification("Test.Message", duration = NULL, type = "message")
  #   showNotification("Test.Warning", duration = NULL, type = "warning")
  #   showNotification("Test.Error", duration = NULL, type = "error")
  #   progress <- shiny::Progress$new()
  #   progress$set(message = "Test.Progress", value = 0.5)
  #   progressNotification("test.progress.notify", "Test.ProgressNotify")
  # })
  
  # Read counts
  readcounts <- genericImporterData("pca.data.readcounts.importer", 
                                    importers = reactive(supportedReadcountImporters),
                                    samples = reactive(availableReadcountSamples),
                                    generators = reactive(supportedReadcountGenerators),
                                    exprimport = importReadcount, 
                                    exprsample = importReadcountSample)
  
 
  
  # Gene variances
  gene.variances <- reactive( { buildGeneVarianceTable(readcounts.processed()) } )
  
  # Fetch gene info annotation with an integrating generic importer.
  # This allows the user to provide multiple data source with only one UI and feedback what was found
  gene.info.annotation <- serverGeneInfoAnnotation(readcounts)
  
  # Readcount processing
  readcounts.processing.output <- serverReadCountProcessing(readcounts, gene.info.annotation, input)
  readcounts.processed <- reactive({ readcounts.processing.output()$readcounts })
  
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
  
  #' Build a list of all visual parameters
  #' 1. We have for each cell CELL -> Is in condition true/false? mapping (conditions)
  #' 2. Then we build a table that assigns visual parameters (shape, color, custom label, ...) to each condition
  #' 3. Based on this determine the visual conditions for each cell
  conditions <- cellConditionImporterValue("conditions.importer", readcounts = readcounts.processed)
  
  #
  # Update input elements
  #
  
  # For navigation (links)
  serverNavigation(input, session)
  
  # Readcounts
  downloadableDataTable("readcounts", export.filename = "readcounts", data = readcounts)
  downloadableDataTable("readcounts.processed", export.filename = "readcounts.processed", data = readcounts.processed)
  downloadableDataTable("readcounts.filtered", export.filename = "readcounts.filtered", data = readcounts.filtered)
  downloadableDataTable("readcounts.top.variant", export.filename = "readcounts.top.variant", data = readcounts.top.variant)
  
  # Cell condition assingments
  downloadableDataTable("conditions", export.filename = "conditions", data = conditions)
  plotConditionsVennDiagramPlot("pca.conditions.plot", conditions)
  
  downloadableDataTable("genes.variance", export.filename = "variance", data = serverGeneVarianceTableData(gene.variances))
  downloadableDataTable("genes.annotation", export.filename = "annotation", data = serverGeneAnnotationTableData(readcounts, gene.info.annotation))
  
  # Gene filtering
  output$pca.pca.genes.set.count <- renderText({
    validate(need(readcounts.filtered(), "0 genes selected"))
    return(paste(nrow(readcounts.filtered()), "genes selected"))
  })
  
  output$readcounts.processing.steps <- renderUI(serverReadCountsProcessingOutput(
    input,
    readcounts.processed,
    readcounts.processing.output
  ))
  
  downloadableDataTable("pca.transformed", export.filename = "pca.transformed", data = reactive({ pca()$transformed })) 
  downloadableDataTable("pca.pc", export.filename = "pca.pc", data = reactive({ pca()$pc }))
  downloadableDataTable("pca.variance", export.filename = "pca.var", data = reactive({ pca()$var }))
  
  plotGeneVariancePlot("pca.genes.variances.plot", gene.variances = gene.variances)
  plotGeneVariancePlot("pca.genes.variances.filtered.plot", gene.variances = gene.variances.filtered)
  
  output$pca.pca.genes.count.variance.plot <- renderPlot({
   
    validate(need(gene.variances.filtered(), "No gene variances to display!"))
    
    logarithmic <- input$pca.genes.variance.filtered.plot.log # Use the setting from the equivalent plot
    genes.count <- pca.gene.count()$value
    data <- gene.variances.filtered()
    data$logvar <- log(data$var)
    data.selection <- data[1:genes.count,]
    
    
    
    if(logarithmic) {
      p <- ggplot(data, aes(x=1:nrow(data), y=logvar))
      p <- p + geom_vline(xintercept = genes.count, color = "red")
      p <- p + geom_ribbon(data = data.selection, aes(ymin = min(data$logvar), ymax = logvar, x = 1:genes.count), fill = "#da4453")
      p <- p + geom_point()
      p <- p + labs(x = "Top n-th variant gene", y = expression(log(sigma^2)))
      
      return(p)
    }
    else {
      p <- ggplot(data, aes(x=1:nrow(data), y=var))
      p <- p + geom_vline(xintercept = genes.count, color = "red")
      p <- p + geom_ribbon(data = data.selection, aes(ymin = min(data$var), ymax = var, x = 1:genes.count), fill = "#da4453")
      p <- p + geom_point()
      p <- p + labs(x = "Top n-th variant gene", y = expression(sigma^2))
      
      return(p)
    }
  })
  
  pca.variance.plot.settings <- generalPlotSettings("pca.pc.importance.generalsettings")
  
  downloadablePlot("pca.variance.plot", 
                   plot.settings = pca.variance.plot.settings, 
                   exprplot = function( plot.settings, format, filename ){
    return(savePCAVariancePlot(pca(), plot.settings, format, filename))
  })
  
  plotCellPlot("pca.cells.plot",
               readcounts.processed = readcounts.processed,
               readcounts.filtered = readcounts.filtered,
               readcounts.top.variant = readcounts.top.variant,
               gene.variances = gene.variances,
               animation.params = pca.gene.count,
               conditions = conditions,
               pca = pca)
})
