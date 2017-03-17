
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(RColorBrewer)
library(Cairo)
library(scatterplot3d)
library(ggplot2)
library(dplyr)
library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
source("readCountImporter.R")
source("readCountProcessing.R")
source("annotation.R")
source("readCountNormalizer.R")
source("conditions.R")
source("pca.R")
source("plots.R")
source("movie.R")
source("widgetGenericImporter.R")
source("widgetDownloadableDataTable.R")
source("widgetDownloadablePlot.R")
source("widgetColorShapeInput.R")
source("widgetGeneralPlotSettings.R")

shinyServer(function(input, output, session) {
  
  # Read counts
  readcounts <- genericImporterData("pca.data.readcounts", exprimport = importReadcount, exprsample = importReadcountSample)
  readcounts.processing.output <- serverReadCountProcessing(readcounts, input)
  readcounts.processed <- reactive({ readcounts.processing.output()$readcounts })
  
  gene.variances <- reactive( { buildGeneVarianceTable(readcounts.processed()) } )
 
  
  conditions <- cellConditionImporterValue("conditions.importer", readcounts = readcounts.processed)
  
  # The next step is to filter our genes based on the annotation and then select the top n most variant genes
  readcounts.filtered <- reactive({ readcounts.processed() })
  readcounts.selected <- reactive({ selectTopVariantGeneReadcounts(readcounts.filtered(), gene.variances(), input$pca.pca.genes.count) })
  
  # pca is applied to the selected genes and setup some values to be used by outputs
  pca <- serverPCA(input, readcounts.selected)
  
  # Visualizing the data
  visuals.conditions <- colorShapeEditorValue("pca.cells.plot.visuals", conditions)
  
  #' Build a list of all visual parameters
  #' Return a table with factors for color and symbol for each cell
  #' Return a palette that correspond to the factors
  visuals.cell <- reactive({ serverGetCellVisualsTable(input, readcounts.processed, conditions, visuals.conditions) })
  
  #
  # Update input elements
  #
  
  #' Start page button. User can click it to go to the "Analyze" section
  observeEvent(input$about.goto.analyze, {
    updateNavbarPage(session, "main.nav", "analyze")
  })
  
  # Navigation quick links
  # Offer quick links in the navigation as compromise between hierarchical layout and discoverability
  observeEvent(input$pca.nav, {
    if(input$pca.nav == "pca.cells.plot.quicklink") {
      updateNavbarPage(session, "pca.nav", selected = "pca.cells.plot")
    }
  })
  
  #' 
  #' Fetch gene count parameters
  #' Usually using input$ would be sufficient,
  #' but numericInput is broken
  #' 
  
  pca.genes.count.from <- reactive({
    
    # Additional checks as the numeric input is broken (accepts values outside of range)
    # This bug is from 2015 (sic!) https://github.com/rstudio/shiny/issues/927
    
    genes_max <- nrow(readcounts.filtered())
    genes_min <- min(1, genes_max)
    genes_from <- input$pca.pca.genes.count.from
    genes_to <- input$pca.pca.genes.count.to
    
    validate(need(genes_max == 0 || genes_from < genes_to, "Invalid range given!"))
    
    genes_from <- max(genes_min, genes_from)
    
    return(genes_from)
  })
  
  pca.genes.count.to <- reactive({
    
    # Additional checks as the numeric input is broken (accepts values outside of range)
    # This bug is from 2015 (sic!) https://github.com/rstudio/shiny/issues/927
    
    genes_max <- nrow(readcounts.filtered())
    genes_from <- input$pca.pca.genes.count.from
    genes_to <- input$pca.pca.genes.count.to
    
    validate(need( genes_max == 0 || genes_from < genes_to, "Invalid range given!"))
    
    genes_to <- min(genes_max, genes_to)
    
    return(genes_to)
    
  })
  
  pca.genes.count.by <- reactive({
    return(max(0, input$pca.pca.genes.count.by))
  })
  
  #' Handles the animation of the gene counts
  #' This works by invalidating itself automatically if the play button is toggled
  #' 
  #' Info: There's a native animation feature in the slider. But it does not allow
  #' changing the animation parameters without renderUI; which is slow and fragile due to missing inputs
  observe({
    
    if(input$pca.pca.genes.count.animation.play) {
      # Separate the actual animation from the environment
      isolate({
        
        from <- pca.genes.count.from()
        to <- pca.genes.count.to()
        by <- input$pca.pca.genes.count.by
        current <- input$pca.pca.genes.count
        
        validate(need(from < to, "Wrong animation parameters!"))
        
        if(current == to) {
          current <- from
        }
        else if(current < to) {
          current <- min(to, current + by)
        }
        else {
          current <- from
        }
        
        updateSliderInput(session, "pca.pca.genes.count", value = current)
        
      })
      
      invalidateLater(isolate({input$pca.pca.genes.count.animation.speed}))
    }
  })
  
  observeEvent(pca(), {
    
    validate(need(pca(), "Cannot update input wihout PCA result!"))
    
    components <- colnames(pca()$pc) # Get PC1, PC2, PC3, ...
    selection <- input$pca.cells.plot.axes
    
    # Preserve the current selection if it's possible. Otherwise select the two first principal components
    if(is.null(selection) || !all(selection %in% components)) {
      selection <- components[1:min(2, length(components))]
    }
    
    updateSelectizeInput(session, "pca.cells.plot.axes", choices = components, selected = selection)
    
  })
  
  
  
  observeEvent(pca.genes.count.from(), {
    updateSliderInput(session, "pca.pca.genes.count", min = pca.genes.count.from())
  })
  
  observeEvent(input$pca.genes.count.to, {
    updateSliderInput(session, "pca.pca.genes.count", max = pca.genes.count.to())
  })
  
  observeEvent(readcounts.filtered(), {
    genes_max <- nrow(readcounts.filtered())
    updateSliderInput(session, "pca.pca.genes.count", min = 1, max = genes_max, value = genes_max)
    updateNumericInput(session, "pca.pca.genes.count.from", min = min(1, genes_max), max = genes_max, value = min(2, genes_max))
    updateNumericInput(session, "pca.pca.genes.count.to", min = min(1, genes_max), max = genes_max, value = genes_max)
  })

  #
  # Input events
  #
  
  # User clicks fine-grained controls in gene count panel
  observeEvent(input$pca.pca.genes.count.lstepdecrease, {
    current <- input$pca.pca.genes.count
    updateSliderInput(session, "pca.pca.genes.count", value = current - input$pca.pca.genes.count.by)
  })
  
  observeEvent(input$pca.pca.genes.count.lstepincrease, {
    current <- input$pca.pca.genes.count
    updateSliderInput(session, "pca.pca.genes.count", value = current + input$pca.pca.genes.count.by)
  })
  
  observeEvent(input$pca.pca.genes.count.stepdecrease, {
    current <- input$pca.pca.genes.count
    updateSliderInput(session, "pca.pca.genes.count", value = current - 1)
  })
  
  observeEvent(input$pca.pca.genes.count.stepincrease, {
    current <- input$pca.pca.genes.count
    updateSliderInput(session, "pca.pca.genes.count", value = current + 1)
  })

  #
  # Render plots & tables
  #
  
  # Tables
  downloadableDataTable("readcounts", export.filename = "readcounts", data = readcounts)
  downloadableDataTable("readcounts.processed", export.filename = "readcounts.processed", data = readcounts.processed)
  downloadableDataTable("conditions", export.filename = "conditions", data = conditions)
  downloadableDataTable("annotation.var", export.filename = "variance", data = reactive({
    validate(need(gene.variances(), "No annotation available!"))
    
    table <- data.frame(row.names = rownames(gene.variances()), 
                        Variance = gene.variances()$var,
                        "Relative variance" = gene.variances()$var / sum(gene.variances()$var))
    table <- table[order(table$Variance, decreasing = T), ,drop = F]
    
    return(table)
    
    }))
  
  # Texts
  output$readcounts.processing.steps <- renderUI(serverReadCountsProcessingOutput(
    input,
    readcounts.processed,
    readcounts.processing.output
  ))
  
  # PCA results
  downloadableDataTable("pca.transformed", export.filename = "pca.transformed", data = reactive({ pca()$transformed })) 
  downloadableDataTable("pca.pc", export.filename = "pca.pc", data = reactive({ pca()$pc }))
  downloadableDataTable("pca.variance", export.filename = "pca.var", data = reactive({ pca()$var }))
  
  # Gene variance plots
  
  downloadablePlot("genes.variance.plot", exprplot = function(width, height, format, filename) 
    { 
    saveGeneVariancePlot(gene.variances(), width, height, 96, format, filename) 
    })
  
  output$pca.pca.genes.count.variance.plot <- renderPlot({
   
    validate(need(gene.variances(), "No gene variances to display!"))
    
    p <- ggplot(gene.variances(), aes(x=1:nrow(gene.variances()), y=log(var))) + geom_point() 
    p <- p + geom_vline(xintercept = input$pca.pca.genes.count, color = "red")
    p <- p + labs(x = "Top n-th variant gene", y = "log(σ²)")
    
    return(p)
  })
  
  # PCA plots
  downloadablePlot("pca.variance.plot", exprplot = function( width, height, format, filename ){
    
    dpi <- 96
    
    p <- ggplot(pca()$var, aes(x=rownames(pca()$var), y=var.relative)) + geom_point()
    p <- p + labs(x = "Principal component", y = "Relative variance")
    ggsave(filename, p, width = width / dpi, height = height / dpi, device = format)
    
  })
  
  pca.cellplot.settings <- generalPlotSettings("pca.cells.plot.generalsettings")
  
  downloadablePlot("pca.cellplot", exprplot = function( width, height, format, filename ){
    
    validate(need(pca(), "No PCA results to plot!"),
             need(visuals.cell(), "No visual parameters!"))
    
    plot.settings <- pca.cellplot.settings()
    plot.width <- if(plot.settings$width < 50) { width } else { plot.settings$width }
    plot.height <- if(plot.settings$height < 50) { height } else { plot.settings$height }
    plot.dpi <- plot.settings$dpi
    plot.title <- if(plot.settings$title == "") { "Cell PCA" } else { plot.settings$title }
    plot.subtitle <- if(plot.settings$title == "") { paste(nrow(pca()$pc), "genes") } else { plot.settings$subtitle }
    
    savePCACellPlot(pca(),
                visuals.conditions(),
                visuals.cell(),
                input$pca.cells.plot.axes,
                plot.width,
                plot.height,
                plot.dpi,
                format,
                filename,
                title = plot.title,
                subtitle = plot.subtitle)
  })
  
  output$pca.cellplot.export.mp4 <- downloadHandler("cell.pca.mp4", function(file) {
    
    validate(
      need(readcounts.selected(), "No processed read counts!"),
      need(input$pca.pca.genes.count.from < input$pca.pca.genes.count.to, "Gene count settings wrong!")
    )
    
    progress <- shiny::Progress$new()
    
    # When this function exits, close the progress and re-enable the button
    on.exit({
      shinyjs::enable("pca.cellplot.export.mp4")
      progress$close()
    })
    
    progress$set(message = "Creating movie ...", value = 0)
    shinyjs::disable("pca.cellplot.export.mp4")
    
    # Status callback function
    updateProgress <- function(detail = NULL, value = NULL) {
      progress$set(value = value, detail = detail)
    }
    
    plot.settings <- pca.cellplot.settings()
    plot.width <- if(plot.settings$width < 50) { 640 } else { plot.settings$width }
    plot.height <- if(plot.settings$height < 50) { 480 } else { plot.settings$height }
    plot.dpi <- plot.settings$dpi
    
    savePCACellPlotMovie(
      filename = file,
      plot.width,
      plot.height,
      plot.dpi,
      genes.count.from = pca.genes.count.from(),
      genes.count.to = pca.genes.count.to(),
      genes.count.by = input$pca.pca.genes.count.by,
      time.per.frame = input$pca.pca.genes.count.animation.speed,
      axes = input$pca.cells.plot.axes,
      visuals.conditions = visuals.conditions(),
      visuals.cell = visuals.cell(),
      readcounts.filtered = readcounts.filtered(),
      gene.variances = gene.variances(),
      pca.center = input$pca.pca.settings.center,
      pca.scale = input$pca.pca.settings.scale,
      updateProgress = updateProgress
    )
    
  })

})
