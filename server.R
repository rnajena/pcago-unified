
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(Cairo)
library(ggplot2)
library(plotly)
library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
source("readCountImporter.R")
source("annotationImporter.R")
source("readCountNormalizer.R")
source("conditionTable.R")
source("gene.R")
source("pca.R")
source("widgetGenericImporter.R")
source("widgetDownloadableDataTable.R")
source("widgetColorShapeInput.R")

options(shiny.usecairo=TRUE)

shinyServer(function(input, output, session) {
  
  readcounts <- reactive ( { callModule(genericImporter, "pca.data.readcounts", exprimport = importReadcount, exprsample = importReadcountSample) } )
  
  #
  # Calculations
  #
  
  # The starting values are normalized read counts, the annotation table and the condition table
  readcounts.normalized <- reactive({ return(applyReadcountNormalization(readcounts(), input$pca.data.normalization)) })
  annotation <- reactive( { annotateGenes(readcounts.normalized()) } )
  annotation.var <- reactive({ if(is.null(annotation())) { NULL } else { annotation()["var"] } })
  conditions <- reactive({ 
    
    if(input$pca.data.conditions.mode == "column") {
      return(generateConditionTable(readcounts.normalized(), sep = ""))
    }
    else if(input$pca.data.conditions.mode == "extract") {
      return(generateConditionTable(readcounts.normalized(), sep = input$pca.data.conditions.separator))
    }
    else if(input$pca.data.conditions.mode == "upload") {
      return(NULL) #todo
    }
    else {
      return(NULL)
    }
    
    
    })
  
  # The next step is to filter our genes based on the annotation and then select the top n most variant genes
  readcounts.selected <- reactive(
    { 
      return(selectTopVariantGenes(readcounts.normalized(), annotation(), input$pca.pca.genes.count))
    })
  
  # pca is applied to the selected genes and setup some values to be used by outputs
  pca <- reactive( { applyPCA(readcounts.selected()) } )
  pca.transformed <- reactive({ if(is.null(pca())) { NULL } else { pca()$transformed } })
  pca.pc <- reactive({ if(is.null(pca())) { NULL } else { pca()$pc } })
  pca.var <- reactive({ if(is.null(pca())) { NULL } else { pca()$var } })
  
  # Visualizing the data
  conditions.visuals.table <- reactive({
    return(data.frame(
      row.names = c("mono6", "ctr", "n2", "n3", "n5", "n6", "atra"),
      color = c("", "#FF0000", "", "", "", "", "#00FF00"),
      shape = c("", "", "", "", "", "", ""),
      stringsAsFactors = F
    ))
  })
  
  # Builds color palette from visual definition
  pca.transformed.visuals.palette <- reactive({
    
    if(is.null(conditions.visuals.table())) {
      return(NULL)
    }
    
    colors <- conditions.visuals.table()$color
    palette <- c("#000000", colors[grep("#[0-9A-F]{6}", colors, ignore.case = T)])
    
    return(palette)
    
  })
  
  # Build table of cell -> visual parameters that can be used by plots
  pca.transformed.visuals <- reactive({
    
    if(is.null(readcounts.normalized()) || is.null(pca.transformed.visuals.palette())) {
      return(NULL)
    }
    
    cells <- colnames(readcounts.normalized())
    cells.conditions <- conditions()
    conditions.mapping <- conditions.visuals.table()
    palette <- pca.transformed.visuals.palette()
    visuals <- data.frame(row.names = cells, 
                          color = rep(1, length(cells)), 
                          shape = rep("circle", length(cells)))
    
    for(i in 1:nrow(visuals)) {
      
      cell.name <- cells[i]
      cell.conditions <- cells.conditions[cell.name, ]
      
      for(condition in colnames(cells.conditions)) {
        
        if(cells.conditions[i, condition] == F) {
          next()
        }
        
        if(!(condition %in% rownames(conditions.mapping))) {
          next()
        }
        
        mapping.color <- conditions.mapping[condition,"color"]
        
        if(mapping.color != "") {
          visuals[cell.name,"color"] <- match(mapping.color, palette)
        }
        
      }
      
    }
    
    return(visuals)
    
  })
  
  #
  # Update input elements
  #
  
  observeEvent(input$about.goto.analyze, {
    updateNavbarPage(session, "navigation", "analyze")
  })
  
  observeEvent(pca(), {
    
    if(is.null(pca())) {
      return()
    }
    
    components <- colnames(pca()$pc) # Get PC1, PC2, PC3, ...
    selection <- input$pca.plot.cells.axes
    
    # Preserve the current selection if it's possible. Otherwise select the two first principal components
    if(is.null(selection) || !all(selection %in% components)) {
      selection <- components[1:min(2, length(components))]
    }
    
    updateSelectizeInput(session, "pca.plot.cells.axes", choices = components, selected = selection)
    
  })
  
  observeEvent(readcounts.normalized(), {
    
    genes_max <- nrow(readcounts.normalized())
    
    updateSliderInput(session, "pca.pca.genes.count", min = 1, max = genes_max, value = genes_max)
  })
  
  output$pca.plot.visuals <- renderUI({
    
    if(is.null(conditions())) {
      return(NULL)
    }
    
    cells.conditions <- conditions()
    ui <- tagList()
    
    for(condition in colnames(cells.conditions)) {
      ui <- tagAppendChild(ui, colorShapeInput(paste0("pca.plot.visuals.", condition), condition))
    }
    
    return(ui)
    
  })
  
  #
  # Input events
  #
  
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
  observeEvent ( readcounts.normalized(), { callModule(downloadableDataTable, "readcounts.normalized.csv", filename = "readcounts.norm.csv", data = readcounts.normalized) })
  observeEvent ( conditions(),  { callModule(downloadableDataTable, "conditions", filename = "conditions.csv", data = conditions) })
  observeEvent ( annotation.var(), { callModule(downloadableDataTable, "annotation.var", filename = "variance.csv", data = annotation.var) })
  
  # PCA results
  observeEvent ( pca.transformed(), { callModule(downloadableDataTable, "pca.transformed", filename = "pca.transformed.csv", data = pca.transformed) })
  observeEvent ( pca.pc(), { callModule(downloadableDataTable, "pca.pc", filename = "pca.pc.csv", data = pca.pc) })
  observeEvent ( pca.var(), { callModule(downloadableDataTable, "pca.var", filename = "pca.var.csv", data = pca.var) })
  
  # Variance plots
  output$genes.variance.plot <- renderPlot({
    if(is.null(annotation())) {
      return(NULL)
    }
    
    ggplot(annotation(), aes(x=1:nrow(annotation()), y=log(var))) + geom_point()
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
  output$pca.cellplot <- renderPlotly({
    
    if(is.null(pca.transformed()) || is.null(pca.transformed.visuals()) || is.null(input$pca.plot.cells.axes)) {
      return(NULL)
    }
    
    dimensions.available <- ncol(pca.transformed())
    dimensions.requested <- input$pca.plot.cells.axes
    
    dimensions.plot <- min(length(dimensions.requested), dimensions.available) 
    
    if(dimensions.plot == 1) {
      # ggplot(pca()$transformed,
      #        aes_string(dimensions.requested[1])) + geom_histogram(bins = 500)
      
      #plot_ly()
      
      plot_ly(type = "histogram",
             x = pca.transformed()[[dimensions.requested[1]]])
      
    }
    else if(dimensions.plot == 2) {
      # ggplot(pca()$transformed, aes_string(x=dimensions.requested[1], y=dimensions.requested[2])) + geom_point(shape = 1)
      
      print(pca.transformed.visuals.palette())
      
      #todo: separate traces here -> for each a legend
      
      plot_ly(type = "scatter",
              mode = "markers",
              x = pca.transformed()[[dimensions.requested[1]]],
              y = pca.transformed()[[dimensions.requested[2]]],
              color = pca.transformed.visuals()$color,
              colors = pca.transformed.visuals.palette())
      
    }
    else if(dimensions.plot == 3) {
      
      plot_ly(type = "scatter3d",
              mode = "markers",
              x = pca.transformed()[[dimensions.requested[1]]],
              y = pca.transformed()[[dimensions.requested[2]]],
              z = pca.transformed()[[dimensions.requested[3]]])
      
    }
    
   
  })
})
