
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(Cairo)
library(ggplot2)
library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
source("readCountImporter.R")
source("annotationImporter.R")
source("readCountNormalizer.R")
source("gene.R")
source("pca.R")
source("uiImporterWidget.R")
source("uiDownloadableDataTable.R")

options(shiny.usecairo=TRUE)

shinyServer(function(input, output, session) {
  
  readcounts <- reactive ( { callModule(genericImporter, "pca.data.readcounts", exprimport = importReadcount) } )
  
  #
  # Calculations
  #
  
  # The starting values are normalized read counts and the annotation table
  readcounts.normalized <- reactive({ return(applyReadcountNormalization(readcounts(), input$pca.data.normalization)) })
  annotation <- reactive( { annotateGenes(readcounts.normalized()) } )
  annotation.var <- reactive({ if(is.null(annotation())) { NULL } else { annotation()[c("id", "var")] } })
  
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
  
  #
  # Update input elements
  #
  observeEvent(readcounts.normalized(), {
    
    genes_max <- nrow(readcounts.normalized())
    
    updateSliderInput(session, "pca.pca.genes.count", min = 1, max = genes_max, value = genes_max)
  })

  #
  # Render plots & tables
  #
  
  # Input tables
  observeEvent ( readcounts(), { callModule(downloadableDataTable, "readcounts", data = readcounts) })
  observeEvent ( readcounts.normalized(), { callModule(downloadableDataTable, "readcounts.normalized", data = readcounts.normalized) })
  
  # Annotation tables
  observeEvent ( annotation.var(), { callModule(downloadableDataTable, "annotation.var", data = annotation.var) })
  
  # PCA results
  observeEvent ( pca.transformed(), { callModule(downloadableDataTable, "pca.transformed", data = pca.transformed) })
  observeEvent ( pca.pc(), { callModule(downloadableDataTable, "pca.pc", data = pca.pc) })
  observeEvent ( pca.var(), { callModule(downloadableDataTable, "pca.var", data = pca.var) })
  
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
  output$pca.conditionplot <- renderPlot({
    
    if(is.null(pca())) {
      return(NULL)
    }
    
    dimensions.available <- ncol(pca()$transformed) - 1
    dimensions.requested <- c("PC1", "PC2")
    
    dimensions.plot <- min(length(dimensions.requested), dimensions.available) 
    
    print(dimensions.plot)
    
    if(dimensions.plot == 1) {
      ggplot(pca()$transformed,
             aes_string(dimensions.requested[1])) + geom_histogram(bins = 500)
    }
    else if(dimensions.plot == 2) {
      ggplot(pca()$transformed, aes_string(x=dimensions.requested[1], y=dimensions.requested[2])) + geom_point(shape = 1)
    }
    
   
  })
})
