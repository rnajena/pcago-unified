
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
source("widgetVisualsEditor.R")
source("widgetGeneralPlotSettings.R")
source("widgetExtendedSliderInput.R")
source("serverFunctions.R")
source("helpers.R")
source("classPlotSettings.R")

shinyServer(function(input, output, session) {
  
  # Read counts
  readcounts <- genericImporterData("pca.data.readcounts.importer", exprimport = importReadcount, exprsample = importReadcountSample)
  
  # Gene variances
  gene.variances <- reactive( { buildGeneVarianceTable(readcounts.processed()) } )
  
  # Fetch gene info annotation with an integrating generic importer.
  # This allows the user to provide multiple data source with only one UI and feedback what was found
  gene.info.annotation <- integratingGenericImporterData("pca.data.annotation.importer", exprimport = function(con, importer) {
      return(importGeneInformationFromAnnotation(con, importer, readcounts()))
    },
    exprsample = function(sample) {
      return(importSampleGeneInformationFromAnnotation(sample, readcounts()))
    },
    exprintegrate = function(data, callback) {
      output <- list()
      genes <- rownames(readcounts())
      
      choices = c("sequence.info",
                  "features", 
                  "go")
      selected = c()
      
      for(current.data in data) {
        output <- append(output, current.data) # merge lists
        selected <- c(selected, names(current.data))
      }
      
      sequence.info.genes <- if(!is.null(output$sequence.info)) intersect(rownames(output$sequence.info), genes) else c()
      feature.genes <- if(!is.null(output$features)) intersect(unlist(output$features), genes) else c()
      go.genes <- c()
      
      names(choices) <- c(
        if(length(sequence.info.genes) == 0) "Sequence info" else paste0("Sequence info (", length(sequence.info.genes), "/", length(genes), ")"),
        if(length(feature.genes) == 0) "Associated features" else paste0("Associated features (", length(feature.genes), "/", length(genes), ")"),
        if(length(go.genes) == 0) "GO terms" else paste0("GO terms (", length(go.genes), "/", length(genes), ")")
      )
      
      
      callback(choices, selected)
      return(output)
    })
  
  # Readcount processing
  readcounts.processing.output <- serverReadCountProcessing(readcounts, input)
  readcounts.processed <- reactive({ readcounts.processing.output()$readcounts })
  
  gene.info.annotation.features <- filterSelectionValues("pca.pca.genes.set",  reactive({
    
    gene.criteria <- list() # This list contains Category -> list of [ Criterion -> Vector of genes ]
    unused.genes <- rownames(readcounts.processed())
    
    if(!is.null(gene.info.annotation())) {
      criteria <- gene.info.annotation()$features
      
      gene.criteria[["Associated features"]] <- criteria 
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
  visuals.conditions <- visualsEditorValue("pca.cells.plot.visuals", reactive({colnames(conditions())}))
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
  downloadableDataTable("genes.variance", export.filename = "variance", data = reactive({
    validate(need(gene.variances(), "No annotation available!"))
    
    table <- data.frame(row.names = rownames(gene.variances()), 
                        Variance = gene.variances()$var,
                        "Relative variance" = gene.variances()$var / sum(gene.variances()$var))
    table <- table[order(table$Variance, decreasing = T), ,drop = F]
    
    return(table)
    
    }))
  downloadableDataTable("genes.annotation", export.filename = "annotation", data = reactive({
    validate(need(gene.info.annotation(), "No annotation available!"))
    
    genes <- rownames(readcounts())
    table <- data.frame(row.names = genes,
                        "Sequence" = rep(NA, length(genes)),
                        "Start" = rep(NA, length(genes)),
                        "End" = rep(NA, length(genes)),
                        "Length" = rep(NA, length(genes)),
                        "Features" = rep(NA, length(genes)))
    
    if("sequence.info" %in% names(gene.info.annotation())) {
      sequence.info <- gene.info.annotation()$sequence.info
      indices <- match(genes, rownames(sequence.info))
      
      table$Sequence <- sapply(indices, function(i) { if(is.na(i)) NA else sequence.info[i, "sequence"] })
      table$Start <- sapply(indices, function(i) { if(is.na(i)) NA else sequence.info[i, "start"] })
      table$End <- sapply(indices, function(i) { if(is.na(i)) NA else sequence.info[i, "end"] })
      table$Length <- sapply(indices, function(i) { if(is.na(i)) NA else sequence.info[i, "length"] })
    }
    if("features" %in% names(gene.info.annotation())) {
      features <- gene.info.annotation()$features
      
      # Note: This is expensive as the features are structured in inverse way to increase filter performance
      table[["Features"]] <- sapply(genes, function(gene) { 
        
        features <- Filter(function(feature) { gene %in% features[[feature]] }, names(features))
        return(paste(features, collapse = "; "))
        
        })
    }
    
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
  
  downloadablePlot("genes.variance.plot", exprplot = function(plot.settings, format, filename) 
    { 
    saveGeneVariancePlot(gene.variances(), plot.settings, format, filename) 
    })
  downloadablePlot("genes.variance.filtered.plot", exprplot = function(plot.settings, format, filename) 
  { 
    saveGeneVariancePlot(gene.variances.filtered(), plot.settings, format, filename) 
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
  downloadablePlot("pca.variance.plot", exprplot = function( plot.settings, format, filename ){
    
    plot.settings <- setNA(plot.settings, 
                           width = 640, 
                           height = 480,
                           dpi = 96,
                           title = "Principal component variances",
                           subtitle = "")
    
    width <- plot.settings@width
    height <- plot.settings@height
    dpi <- plot.settings@dpi
    title <- plot.settings@title
    subtitle <- plot.settings@subtitle
    
    p <- ggplot(pca()$var, aes(x=rownames(pca()$var), y=var.relative)) + geom_point()
    p <- p + labs(x = "Principal component", y = "Relative variance")
    ggsave(filename, p, width = width / dpi, height = height / dpi, device = format)
    
  })
  
  pca.cellplot.settings <- generalPlotSettings("pca.cells.plot.generalsettings")
  
  # Handler for cell plot rendering
  downloadablePlot("pca.cellplot", exprplot = function( plot.settings, format, filename ){
    
    validate(need(pca(), "No PCA results to plot!"),
             need(visuals.cell(), "No visual parameters!"))
   
    
    savePCACellPlot(pca = pca(),
                visuals.conditions = visuals.conditions(),
                visuals.cell = visuals.cell(),
                axes = input$pca.cells.plot.axes,
                plot.settings = plot.settings,
                format = format,
                filename = filename)
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
        plot.settings = PlotSettings(width = 640, height = 480, dpi = 96),
        visuals.conditions = visuals.conditions(),
        visuals.cell = visuals.cell(),
        readcounts.filtered = readcounts.filtered(),
        gene.variances = gene.variances(),
        pca.center = input$pca.pca.settings.center,
        pca.scale = input$pca.pca.settings.scale,
        updateProgress = updateProgress
      )
      
    }, message = "Creating movie")
    
    
    
  })

})
