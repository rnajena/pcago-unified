
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
source("readcounts.R")
source("annotation.R")
source("conditions.R")
source("pca.R")
source("plots.R")
source("movie.R")
source("widgetGenericImporter.R")
source("widgetDownloadableDataTable.R")
source("widgetDownloadablePlot.R")
source("widgetColorShapeInput.R")
source("widgetGeneralPlotSettings.R")
source("widgetExtendedSliderInput.R")
source("serverFunctions.R")

shinyServer(function(input, output, session) {
  
  # Read counts
  readcounts <- genericImporterData("pca.data.readcounts.importer", exprimport = importReadcount, exprsample = importReadcountSample)
  readcounts.processing.output <- serverReadCountProcessing(readcounts, input)
  readcounts.processed <- reactive({ readcounts.processing.output()$readcounts })
  
  # Gene variances
  gene.variances <- reactive( { buildGeneVarianceTable(readcounts.processed()) } )
  
  # Gene annotation
  gene.info.annotation <- genericImporterData("pca.data.annotation.importer", exprimport = function(con, importer) {
    return(importGeneInformationFromAnnotation(con, importer, readcounts.processed()))
  },
  exprsample = function(sample) {
    return(importSampleGeneInformationFromAnnotation(sample, readcounts.processed()))
  })
  
  
  gene.info.annotation.features <- filterSelectionValues("pca.pca.genes.set",  reactive({
    
    gene.criteria <- list() # This list contains Category -> list of [ Criterion -> Vector of genes ]
    unused.genes <- rownames(readcounts.processed())
    
    if(!is.null(gene.info.annotation())) {
      criteria <- gene.info.annotation()$features
      
      gene.criteria[["Associated features"]] <- criteria #TODO get rid of genes only in annotation
      covered.genes <- unlist(criteria)
      unused.genes <- setdiff(unused.genes, covered.genes)
    }
    
    # Now look for genes that haven't been covered and create a list
    if(length(unused.genes) > 0) {
      gene.criteria[["Misc"]] <- list("Without criteria" = unused.genes)
    }
    
    return(gene.criteria)
    
  }))
  
  # The filtered read counts just intersects the list of genes returned by each filter
  readcounts.filtered <- reactive({
    
    keep.genes <- rownames(readcounts.processed())
    keep.genes <- intersect(keep.genes, gene.info.annotation.features())
    
    keep.readcounts <- readcounts.processed()[keep.genes,]
    
    return(keep.readcounts)
    })
  
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
  visuals.conditions <- colorShapeEditorValue("pca.cells.plot.visuals", conditions)
  visuals.cell <- reactive({ serverGetCellVisualsTable(input, readcounts.processed, conditions, visuals.conditions) })
  
  #
  # Update input elements
  #
  
  # For navigation (links)
  serverNavigation(input, session)
  
  # All server side of UI handling top variant gene count
  #pca.gene.count <- serverPCATopVariantGeneCount(input, session, readcounts.filtered)
  
  
  
  observeEvent(readcounts.top.variant(), {
    
    validate(need(readcounts.top.variant(), "Cannot update input wihout read counts!"))
    
    # components <- colnames(pca()$pc) # Get PC1, PC2, PC3, ...
    # selection <- input$pca.cells.plot.axes
    # 
    # # Preserve the current selection if it's possible. Otherwise select the two first principal components
    # if(is.null(selection) || !all(selection %in% components)) {
    #   selection <- components[1:min(2, length(components))]
    # }
    # Note does work, but is annoying AF in initialization step as it's IMPOSSIBLE to only send events WHEN I NEED!
    
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
  
  # Tables
  downloadableDataTable("readcounts", export.filename = "readcounts", data = readcounts)
  downloadableDataTable("readcounts.processed", export.filename = "readcounts.processed", data = readcounts.processed)
  downloadableDataTable("readcounts.filtered", export.filename = "readcounts.filtered", data = readcounts.filtered)
  downloadableDataTable("readcounts.top.variant", export.filename = "readcounts.top.variant", data = readcounts.top.variant)
  downloadableDataTable("conditions", export.filename = "conditions", data = conditions)
  downloadableDataTable("annotation.var", export.filename = "variance", data = reactive({
    validate(need(gene.variances(), "No annotation available!"))
    
    table <- data.frame(row.names = rownames(gene.variances()), 
                        Variance = gene.variances()$var,
                        "Relative variance" = gene.variances()$var / sum(gene.variances()$var))
    table <- table[order(table$Variance, decreasing = T), ,drop = F]
    
    return(table)
    
    }))
  
  output$pca.pca.genes.set.count <- renderText({
    validate(need(readcounts.filtered(), "0 genes selected"))
    return(paste(nrow(readcounts.filtered()), "genes selected"))
  })
  
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
  downloadablePlot("genes.variance.filtered.plot", exprplot = function(width, height, format, filename) 
  { 
    saveGeneVariancePlot(gene.variances.filtered(), width, height, 96, format, filename) 
  })
  
  output$pca.pca.genes.count.variance.plot <- renderPlot({
   
    validate(need(gene.variances.filtered(), "No gene variances to display!"))
    
    genes.count <- pca.gene.count()$value
    
    p <- ggplot(gene.variances.filtered(), aes(x=1:nrow(gene.variances.filtered()), y=log(var))) + geom_point() 
    p <- p + geom_vline(xintercept = genes.count, color = "red")
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
  
  # Handler for cell plot rendering
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
  
  # Handler for movie export of cell plot
  output$pca.cellplot.export.mp4 <- downloadHandler("cell.pca.mp4", function(file) {
    
    validate(
      need(readcounts.filtered(), "No filtered read counts!")
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
      genes.count.from = pca.gene.count()$from,
      genes.count.to = pca.gene.count()$to,
      genes.count.by = pca.gene.count()$by,
      time.per.frame = pca.gene.count()$delay,
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
