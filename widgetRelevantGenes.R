#'
#' Contains a plot output with download buttons
#'

library(DT)
library(shiny)
source("uiHelper.R")
source("plotAgglomerativeClusteringPlot.R")
source("relevantGenes.R")
source("helpers.R")

relevantGenesUI <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(
    selectizeInput(ns("fdistance"), "Distance method", choices = plotAgglomerativeClusteringPlotUI.dist.methodsSelection),
    selectizeInput(ns("flink"), "Clustering method", choices = plotAgglomerativeClusteringPlotUI.hclust.methodsSelection),
    tags$p("Result depends on PCA parameters."),
    actionButton(ns("calculate"), "Calculate threshold")
  ))
}



relevantGenesValue_ <- function(input, 
                              output, 
                              session,
                              readcounts,
                              pca.center,
                              pca.scale) {
  
  values <- reactiveValues(threshold = NULL)
  
  observeEvent(input$calculate, {
    shinyjs::disable("calculate")
    progress <- progressNotification("Calculating minimal set of genes ...")
    on.exit({
      removeNotification(progress) 
      shinyjs::enable("calculate")
      })
    if(is.null(readcounts())) {
      showNotification("No read counts to process!", type = "error")
      return()
    }
    
    values$threshold <- NULL
    
    data <- assay(readcounts())
    data <- data[order(rowVars(data), decreasing = T),]
    
    reference.clustering <- clustering(data, 
                                       method.dist = input$fdistance, 
                                       method.link = input$flink,
                                       pca.enable = T,
                                       pca.center = pca.center(),
                                       pca.scale = pca.scale())
    
    values$threshold <- find.minimal.clustering.genes.index(data = data, 
                                                            reference.clustering = reference.clustering,
                                                            method.dist = input$fdistance, 
                                                            method.link = input$flink,
                                                            pca.enable = T,
                                                            pca.center = pca.center(),
                                                            pca.scale = pca.scale())
      })
  
  return(reactive({ values$threshold }))
  
}

relevantGenesValue <- function(id, readcounts, pca.center, pca.scale) {
  
  return(callModule(relevantGenesValue_,
                    id,
                    readcounts = readcounts,
                    pca.center = pca.center,
                    pca.scale = pca.scale))
  
}