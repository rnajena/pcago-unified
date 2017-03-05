
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

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


shinyServer(function(input, output, session) {
  
  #
  # Define reactive values that are used by the plots
  #
  calculationdata <- reactiveValues()
  calculationdata$readcounts <- NULL
  
  # 
  # Define events handled by action buttons
  #
  
  # Handles click on "Submit" and "Reset" in PCA->Data->Read counts
  observeEvent(input$pca.data.readcounts.submit, {
    
    widget.input <- importerWidgetData("pca.data.readcounts", input)
    con <- widget.input$connection
    data <- importReadcount(con, widget.input$importer)
    close(con)
    
    calculationdata$readcounts <- data
  })
  observeEvent(input$pca.data.readcounts.reset, {
    reset("pca.data.readcounts.input")
    reset("pca.data.readcounts.fileinput")
  })
  
  #
  # Calculations
  #
  
  # The starting values are normalized read counts and the annotation table
  readcounts.normalized <- reactive({ return(applyReadcountNormalization(calculationdata$readcounts, input$pca.data.normalization)) })
  annotation <- reactive( { annotateGenes(readcounts.normalized()) } )
  
  # The next step is to filter our genes based on the annotation and then select the top n most variant genes
  readcounts.selected <- reactive(
    { 
      return(selectTopVariantGenes(readcounts.normalized(), annotation(), input$pca.pca.genes.count))
    })
  
  # pca is applied to the selected genes
  pca <- reactive( { applyPCA(readcounts.selected()) } )
  
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
  
  # Input
  output$readcounts <- DT::renderDataTable(calculationdata$readcounts, options = list(scrollX = TRUE))
  output$readcounts.normalized <- DT::renderDataTable(readcounts.normalized(), options = list(scrollX = TRUE))
  output$genes.variance <- DT::renderDataTable(annotation()[c("id", "var")], options = list(scrollX = TRUE))
  
  # PCA results
  output$transformedconditions <- DT::renderDataTable(pca()$transformed, options = list(scrollX = TRUE))
  output$pca.principalcomponents <- DT::renderDataTable(pca()$pc, options = list(scrollX = TRUE))
  
  # PCA plots
  output$pca.conditionplot <- renderPlot({
    
    dimensions.available <- ncol(pca()$transformed) - 1
    dimensions.requested <- c("PC1", "PC2")
    
    dimensions.plot <- min(length(dimensions.requested), dimensions.available) 
    
    if(dimensions.plot == 1) {
      ggplot(pca()$transformed,
             aes_string(dimensions.requested[1])) + geom_histogram(binwidth = 1)
    }
    else if(dimensions.plot == 2) {
      ggplot(pca()$transformed, aes_string(x=dimensions.requested[1], y=dimensions.requested[2])) + geom_point(shape = 1)
    }
    
   
  })
})
