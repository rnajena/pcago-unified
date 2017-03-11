
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
source("annotationImporter.R")
source("readCountNormalizer.R")
source("visuals.R")
source("gene.R")
source("pca.R")
source("widgetGenericImporter.R")
source("widgetDownloadableDataTable.R")
source("widgetDownloadablePlot.R")
source("widgetColorShapeInput.R")

options(shiny.usecairo=TRUE)

shinyServer(function(input, output, session) {
  
  readcounts <- callModule(genericImporter, "pca.data.readcounts", exprimport = importReadcount, exprsample = importReadcountSample)
  
  #
  # Calculations
  #
  
  # The starting values are normalized read counts, the annotation table and the condition table
  readcounts.processed <- reactive(
    { 
      rc <- readcounts()
      
      # Remove constant read genes
      if("remove.constant" %in% input$pca.data.readcounts.processing) {
        processed <- removeConstantReads(rc)
        rc <- processed$readcounts
      }
      
      
      # Apply normalization
      rc <- applyReadcountNormalization(rc, input$pca.data.normalization)
      
      return(rc) 
    })
  
  annotation <- reactive( { annotateGenes(readcounts.processed()) } )
  annotation.var <- reactive({ if(is.null(annotation())) { NULL } else { annotation()["var"] } })
  conditions <- reactive({ serverGetConditionTable(input, readcounts.processed) })
  
  # The next step is to filter our genes based on the annotation and then select the top n most variant genes
  readcounts.selected <- reactive({ return(selectTopVariantGenes(readcounts.processed(), annotation(), input$pca.pca.genes.count)) })
  
  # pca is applied to the selected genes and setup some values to be used by outputs
  pca <- reactive( {
    
    no.constant <- "remove.constant" %in% input$pca.data.readcounts.processing
    center <-input$pca.pca.settings.center
    scale <- input$pca.pca.settings.scale
    
    validate(
      need(readcounts.selected(), "No data to apply PCA to!"),
      need(!scale || no.constant, "Constant read count genes must be removed for scaling!")
    )
    
    applyPCA(readcounts.selected(), center = center, scale = scale) 
    
    })
  
  # Visualizing the data
  visuals.conditions <- callModule(colorShapeEditor, "pca.plot.visuals.editor", conditions = conditions)
  
  #' Build a list of all visual parameters
  #' Return a table with factors for color and symbol for each cell
  #' Return a palette that correspond to the factors
  visuals.cell <- reactive({ serverGetCellVisualsTable(input, readcounts.processed, conditions, visuals.conditions) })
  
  #
  # Update input elements
  #
  
  #' Start page button. User can click it to go to the "Analyze" section
  observeEvent(input$about.goto.analyze, {
    updateNavbarPage(session, "navigation", "analyze")
  })
  
  observeEvent(pca(), {
    
    validate(need(pca(), "Cannot update input wihout PCA result!"))
    
    components <- colnames(pca()$pc) # Get PC1, PC2, PC3, ...
    selection <- input$pca.plot.cells.axes
    
    # Preserve the current selection if it's possible. Otherwise select the two first principal components
    if(is.null(selection) || !all(selection %in% components)) {
      selection <- components[1:min(2, length(components))]
    }
    
    updateSelectizeInput(session, "pca.plot.cells.axes", choices = components, selected = selection)
    
  })
  
  observeEvent(readcounts.processed(), {
    
    genes_max <- nrow(readcounts.processed())
    
    updateSliderInput(session, "pca.pca.genes.count", min = 1, max = genes_max, value = genes_max)
  })

  #
  # Input events
  #
  
  # Navigation quick links
  # Offer quick links in the navigation as compromise between hierarchical layout and discoverability
  observeEvent(input$pca.nav, {
    
    if(input$pca.nav == "pca.cells.plot.quicklink") {
      updateNavbarPage(session, "pca.nav", selected = "pca.cells.plot")
    }
    
  })
  
  # User clicks fine-grained controls in gene count panel
  observeEvent(input$pca.pca.genes.count.lstepdecrease, {
    
    current <- input$pca.pca.genes.count
    updateSliderInput(session, "pca.pca.genes.count", value = current - 50)
    
  })
  
  observeEvent(input$pca.pca.genes.count.lstepincrease, {
    
    current <- input$pca.pca.genes.count
    updateSliderInput(session, "pca.pca.genes.count", value = current + 50)
    
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
  
  # Input tables
  observeEvent ( readcounts(), { callModule(downloadableDataTable, "readcounts", filename = "readcounts.csv", data = readcounts) })
  observeEvent ( readcounts.processed(), { callModule(downloadableDataTable, "readcounts.processed.csv", filename = "readcounts.processed.csv", data = readcounts.processed) })
  observeEvent ( conditions(),  { callModule(downloadableDataTable, "conditions", filename = "conditions.csv", data = conditions) })
  observeEvent ( annotation.var(), { callModule(downloadableDataTable, "annotation.var", filename = "variance.csv", data = annotation.var) })
  
  # PCA results
  observeEvent ( pca(), 
                 { 
                   validate(need(pca(), "No PCA results to show!"))
                   
                   callModule(downloadableDataTable, "pca.transformed", filename = "pca.transformed.csv", data = reactive({ pca()$transformed })) 
                   callModule(downloadableDataTable, "pca.pc", filename = "pca.pc.csv", data = reactive({ pca()$pc }))
                   callModule(downloadableDataTable, "pca.variance", filename = "pca.var.csv", data = reactive({ pca()$var }))
                   
                 })
  
  # Gene variance plots

  observeEvent(annotation(), {
    callModule(downloadablePlot, "genes.variance.plot", exprplot = function( width, height, format, filename ){
      
      dpi <- 96
      
      p <- ggplot(annotation(), aes(x=1:nrow(annotation()), y=log(var))) + geom_point()
      ggsave(filename, p, width = width / dpi, height = height / dpi, device = format)
      
    })
  })
  
  output$pca.pca.genes.count.variance.plot <- renderPlot({
    if(is.null(annotation())) {
      return(NULL)
    }
    
    p <- ggplot(annotation(), aes(x=1:nrow(annotation()), y=log(var))) + geom_point() 
    p <- p + geom_vline(xintercept = input$pca.pca.genes.count, color = "red")
    
    return(p)
  })
  
  # PCA plots
  
  observeEvent(pca(), {
    callModule(downloadablePlot, "pca.variance.plot", exprplot = function( width, height, format, filename ){
      
      dpi <- 96
      
      p <- ggplot(pca()$var, aes(x=rownames(pca()$var), y=percentage)) + geom_point()
      ggsave(filename, p, width = width / dpi, height = height / dpi, device = format)
      
    })
  })
  
  observeEvent(pca(), {
    callModule(downloadablePlot, "pca.cellplot", exprplot = function( width, height, format, filename ){
      
      dpi <- 96
      validate(need(input$pca.plot.cells.axes, "No axes to draw!"))
      
      # Fetch needed variables from PCA and visual parameters
      pca.transformed <- pca()$transformed
      
      pca.transformed$color <- visuals.cell()$factors$color
      pca.transformed$shape <- visuals.cell()$factors$shape
      
      palette.colors <- visuals.cell()$palette.colors
      palette.shapes <- visuals.cell()$palette.shapes
      
      # Determine how many dimensions should be drawn
      dimensions.available <- ncol(pca.transformed)
      dimensions.requested <- input$pca.plot.cells.axes
      
      dimensions.plot <- min(length(dimensions.requested), dimensions.available) 
      
      # Plot based on dimensions
      if(dimensions.plot == 1) {
        
        x <- list(title = dimensions.requested[1])
        
        p <- ggplot(pca.transformed, aes_string(x = dimensions.requested[1])) + 
          geom_histogram(aes(fill = factor(color)), bins = 100)
        
        ggsave(filename, p, width = width / dpi, height = height / dpi)
        
      }
      else if(dimensions.plot == 2) {
        
        x <- list(title = dimensions.requested[1])
        y <- list(title = dimensions.requested[2])
        
        p <- ggplot(pca.transformed, aes_string(x = dimensions.requested[1],
                                                y = dimensions.requested[2])) + 
          geom_point(aes(colour = color, shape = shape))
        p <- p + scale_color_manual(values = palette.colors)
        p <- p + scale_shape_manual(values = palette.shapes)
        
        ggsave(filename, p, width = width / dpi, height = height / dpi)
        
      }
      else if(dimensions.plot == 3) {
        
       
        
        # if(format == "svg") {
        #   svg(filename = filename,
        #       width = width / dpi,
        #       height = height / dpi)
        # }
        # else if(format == "png") {
        #   png(filename = filename,
        #       width = width / dpi,
        #       height = height / dpi)
        # }
        
        png(filename = filename,
            width = width / dpi,
            height = height / dpi)
        
        browser() 
        print("TODO: Broken")
        
        scatterplot3d(
          x = pca.transformed[[dimensions.requested[1]]],
          y = pca.transformed[[dimensions.requested[2]]],
          z = pca.transformed[[dimensions.requested[3]]],
          color = palette.colors[as.numeric(pca.transformed$color)],
          pch = 16,
          xlab = dimensions.requested[1],
          ylab = dimensions.requested[2],
          zlab = dimensions.requested[3],
          type = "h"
          
        ) 
        legend("right",
               legend = levels(pca.transformed$color),
               col = palette.colors,
               pch = 16,
               xpd = T)
        dev.off()
        
      }
      
    })
  })

})
