
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(RColorBrewer)
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
  
  # Visualizing the data
  conditions.visuals.table <- reactive({
    
    validate(need(conditions(), "Need list of conditions to build visual table"))
    
    #todo loading/saving of this table
    
    return(data.frame(
      row.names = colnames(conditions()),
      color = colorRampPalette(brewer.pal(9, "Set1"))(ncol(conditions())),
      symbol = rep("circle", ncol(conditions())),
      stringsAsFactors = F
    ))
    
  })
  
  #' Build a list of all visual parameters
  #' Return a table with factors for color and symbol for each cell
  #' Return a palette that correspond to the factors
  pca.transformed.visuals <- reactive({
    
    validate(
      need(readcounts.normalized(), "No data to build visual parameter table from!"),
      need(conditions.visuals.table(), "No condition visual mapping!")
    )
    
    cells <- colnames(readcounts.normalized())
    cells.conditions <- conditions()
    conditions.mapping <- conditions.visuals.table()
    
    # Setup output
    factors <- data.frame(row.names = cells,
                          color = rep("#000000", length(cells)),
                          symbol = rep("circle", length(cells)),
                          stringsAsFactors = F)
    
    palette.colors <- c("#000000")
    palette.colors.conditions <- c("Default")
    palette.symbols <- c("circle")
    palette.symbols.conditions <- c("Default")
    
    # Go through each cell and select the color & shape based on the first condition providing it
    for(cell in cells) {
      
      color <- ""
      color.condition <- ""
      symbol <- ""
      symbol.condition <- ""
      
      for(condition in colnames(cells.conditions)) {
        
        if(!cells.conditions[cell, condition]) {
          next()
        }
        
        if(color == "") {
          
          mapping.color <- conditions.mapping[condition, "color"]
          
          color <- mapping.color
          color.condition <- condition
        }
        
        if(symbol == "") {
          symbol <- conditions.mapping[condition, "symbol"]
          symbol.condition <- condition
        }
        
      }
      
      if(color == "") {
        color = "#000000"
        color.condition <- cell
      }
      if(symbol == "") {
        symbol = "circle"
        symbol.condition <- cell
      }
      
      factors[cell, "color"] <- color.condition # todo: user name for condition
      factors[cell, "symbol"] <- symbol.condition
      
      if(!(color.condition %in% palette.colors.conditions)) { palette.colors <- c(palette.colors, color) }
      if(!(symbol.condition %in% palette.symbols.conditions)) { palette.symbols <- c(palette.symbols, symbol) }
      
    }
    
    #Convert to factors
    factors$color <- as.factor(factors$color)
    factors$symbol <- as.factor(factors$symbol)
    
    print(factors)

    return(list("factors" = factors, "palette.colors" = palette.colors, "palette.symbols" = palette.symbols))
    
  })
  
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
  
  observeEvent(readcounts.normalized(), {
    
    genes_max <- nrow(readcounts.normalized())
    
    updateSliderInput(session, "pca.pca.genes.count", min = 1, max = genes_max, value = genes_max)
  })
  
  output$pca.plot.visuals <- renderUI({
    
    validate(need(conditions(), "Needing conditions for determining plot visuals!"))
  
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
  observeEvent ( pca(), 
                 { 
                   validate(need(pca(), "No PCA results to show!"))
                   
                   callModule(downloadableDataTable, "pca.transformed", filename = "pca.transformed.csv", data = reactive({ pca()$transformed })) 
                   callModule(downloadableDataTable, "pca.pc", filename = "pca.pc.csv", data = reactive({ pca()$pc }))
                   callModule(downloadableDataTable, "pca.var", filename = "pca.var.csv", data = reactive({ pca()$var }))
                 })
  
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
    
    validate(
      need(try(pca()), "No data to plot!"),
      need(try(input$pca.plot.cells.axes), "No axes to draw!")
    )
    
    # if(is.null(pca.transformed()) || is.null(pca.transformed.visuals()) || is.null(input$pca.plot.cells.axes)) {
    #   return(NULL)
    # }
    
    pca.transformed <- pca()$transformed
    pca.transformed.visuals <- pca.transformed.visuals()$factors
    palette.colors <- pca.transformed.visuals()$palette.colors
    palette.symbols <- pca.transformed.visuals()$palette.symbols
    
    dimensions.available <- ncol(pca.transformed)
    dimensions.requested <- input$pca.plot.cells.axes
    
    dimensions.plot <- min(length(dimensions.requested), dimensions.available) 
    
    if(dimensions.plot == 1) {
    
      plot_ly(type = "histogram",
             x = pca.transformed[[dimensions.requested[1]]])
      
    }
    else if(dimensions.plot == 2) {
      
      #todo: separate traces here -> for each a legend
      
      plot_ly(type = "scatter",
              mode = "markers",
              x = pca.transformed[[dimensions.requested[1]]],
              y = pca.transformed[[dimensions.requested[2]]],
              color = pca.transformed.visuals$color,
              colors = palette.colors)
      
    }
    else if(dimensions.plot == 3) {
      
      plot_ly(type = "scatter3d",
              mode = "markers",
              x = pca.transformed[[dimensions.requested[1]]],
              y = pca.transformed[[dimensions.requested[2]]],
              z = pca.transformed[[dimensions.requested[3]]])
      
    }
    
   
  })
})
