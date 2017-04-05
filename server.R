
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
source("readcounts.R")
source("annotation.R")
source("conditions.R")
source("pca.R")
source("plots.R")
source("movie.R")
source("widgetGenericImporter.R")
source("widgetDownloadableDataTable.R")
source("widgetDownloadablePlot.R")
source("widgetVisualsEditor.R")
source("widgetGeneralPlotSettings.R")
source("widgetExtendedSliderInput.R")
source("serverFunctions.R")
source("helpers.R")
source("classPlotSettings.R")

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
  readcounts.processing.output <- serverReadCountProcessing(readcounts, input)
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
  visuals.conditions <- visualsEditorValue("pca.cells.plot.visuals", reactive({colnames(conditions())}))
  visuals.cell <- reactive({ calculatCellVisuals(colnames(readcounts.processed()), conditions(), visuals.conditions()) })
  
  # Readcount cluster plot
  readcounts.cluster.plot.generalsettings <- generalPlotSettings("readcounts.cluster.plot.generalsettings")
  readcounts.cluster.plot.condition.visuals <- visualsEditorValue("readcounts.cluster.plot.visuals", reactive({colnames(conditions())}))
  readcounts.cluster.plot.cell.visuals <-  reactive({ calculatCellVisuals(colnames(readcounts()), conditions(), readcounts.cluster.plot.condition.visuals()) })
  
  downloadablePlot(id = "readcounts.cluster.plot",
                   plot.settings = readcounts.cluster.plot.generalsettings,
                   exprplot = function(plot.settings, format, filename) {
                     plot.settings <- plotSettingsSetNA(plot.settings, PlotSettings(title = "Clustering based on raw read counts"))
                     saveClusterPlot(readcounts, 
                                     plot.settings, 
                                     readcounts.cluster.plot.cell.visuals,
                                     format, 
                                     filename,
                                     method.distance = input$readcounts.cluster.plot.method.distance,
                                     method.cluster = input$readcounts.cluster.plot.method.clustering)
                   })
  
  #
  # Update input elements
  #
  
  # For navigation (links)
  serverNavigation(input, session)
  
  observeEvent(readcounts.top.variant(), {
    
    validate(need(readcounts.top.variant(), "Cannot update input without read counts!"))
    
    # New method: We know how many PCx we will get. So allow them. Remove them at plot step
    components <- sapply(1:ncol(readcounts.top.variant()), function(x) { paste0("PC", x) })
    selection <- input$pca.cells.plot.axes
    
    if(length(selection) == 0) {
      selection <- intersect(c("PC1", "PC2"), components)
    }
    
    updateSelectizeInput(session, "pca.cells.plot.axes", choices = components, selected = selection)
    
  })

 
  #
  # Render plots & tables
  #
  
  # Readcounts
  downloadableDataTable("readcounts", export.filename = "readcounts", data = readcounts)
  downloadableDataTable("readcounts.processed", export.filename = "readcounts.processed", data = readcounts.processed)
  downloadableDataTable("readcounts.filtered", export.filename = "readcounts.filtered", data = readcounts.filtered)
  downloadableDataTable("readcounts.top.variant", export.filename = "readcounts.top.variant", data = readcounts.top.variant)
  
  # Cell condition assingments
  downloadableDataTable("conditions", export.filename = "conditions", data = conditions)
  observeEvent(conditions(), {
    
    validate(need(conditions(), "No conditions available!"))
    
    available.conditions <- colnames(conditions())
    selected.conditions <- if(length(available.conditions) > 0) available.conditions[1:min(5,length(available.conditions))] else c()
    
    updateSelectizeInput(session, "conditions.plot.sets", 
                         choices = available.conditions, 
                         selected = selected.conditions)
    
  })
  
  pca.conditions.plot.generalsettings <- generalPlotSettings("pca.conditions.plot.generalsettings")
  pca.conditions.plot.visuals <- visualsEditorValue("pca.conditions.plot.visuals", reactive({colnames(conditions())}), has.shape = F)
  
  downloadablePlot("conditions.plot",
                   plot.settings = pca.conditions.plot.generalsettings,
                   exprplot = function(plot.settings, format, filename) {
                     return(saveCellConditionVennDiagramPlot(
                       selected.conditions = input$conditions.plot.sets,
                       conditions = conditions,
                       pca.conditions.plot.visuals = pca.conditions.plot.visuals,
                       plot.settings = plot.settings,
                       format = format,
                       filename = filename
                     ))
                   })
  
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
  
  pca.genes.variances.settings <- generalPlotSettings("pca.genes.variances.generalsettings")
  downloadablePlot("genes.variance.plot", 
                   plot.settings = pca.genes.variances.settings, 
                   exprplot = function(plot.settings, format, filename) 
                   { 
                     return(saveGeneVariancePlot(gene.variances(), plot.settings, format, filename, 
                                                 logarithmic = input$genes.variance.plot.log))
                   })
  
  pca.genes.variances.filtered.settings <- generalPlotSettings("pca.genes.variances.filtered.generalsettings")
  downloadablePlot("genes.variance.filtered.plot",
                   plot.settings = pca.genes.variances.filtered.settings,
                   exprplot = function(plot.settings, format, filename) 
                   { 
                     return(saveGeneVariancePlot(gene.variances.filtered(), plot.settings, format, filename, 
                                                 logarithmic = input$pca.genes.variance.filtered.plot.log))
                   })
  
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
  
  pca.cellplot.settings <- generalPlotSettings("pca.cells.plot.generalsettings")
  
  downloadablePlot("pca.cellplot", plot.settings = pca.cellplot.settings, exprplot = function( plot.settings, format, filename ){
    
    validate(need(pca(), "No PCA results to plot!"),
             need(visuals.cell(), "No visual parameters!"))
    
    plot.settings <- plotSettingsSetNA(plot.settings, PlotSettings(subtitle = paste(nrow(readcounts.top.variant()), "genes")))
    
    return(savePCACellPlot(pca = pca(),
                visuals.conditions = visuals.conditions(),
                visuals.cell = visuals.cell(),
                axes = input$pca.cells.plot.axes,
                plot.settings = plot.settings,
                format = format,
                filename = filename))
  })
  
  # Handler for movie export of cell plot
  output$pca.cellplot.export.mp4 <- downloadHandler("cell.pca.mp4", function(file) {
    
    validate(
      need(readcounts.filtered(), "No filtered read counts!")
    )
    
    # Disable the export button, so the user doesn't spam it
    on.exit({
      shinyjs::enable("pca.cellplot.export.mp4")
    })
    shinyjs::disable("pca.cellplot.export.mp4")
  
    
    withProgressCustom(function(updateProgress) {
      
      savePCACellPlotMovie(
        filename = file,
        animation.params = pca.gene.count(),
        axes = input$pca.cells.plot.axes,
        plot.settings = pca.cellplot.settings(),
        visuals.conditions = visuals.conditions(),
        visuals.cell = visuals.cell(),
        readcounts.filtered = readcounts.filtered(),
        gene.variances = gene.variances(),
        pca.center = input$pca.pca.settings.center,
        pca.scale = input$pca.pca.settings.scale,
        pca.relative = input$pca.pca.settings.relative,
        updateProgress = updateProgress
      )
      
    }, message = "Creating movie")
    
  })

})
