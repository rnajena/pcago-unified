#'
#' Defines widget that allows finding the clustering relevant genes using
#' a heuristic.
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
    checkboxInput(ns("pca.enable"), "Apply PCA", value = F),
    conditionalPanel(conditionalPanel.equals(ns("pca.enable"), "true"),
                     tags$p("Result depends on PCA parameters.")),
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
    
    if(is.null(readcounts())) {
      showNotification("No read counts to process!", type = "error")
      return()
    }
    
    shinyjs::disable("calculate")
    progress <- shiny::Progress$new()
    progress$set(message = "Running calculations ...", value = 0)
    
    # Status callback function
    updateProgress <- function(detail = NULL, value = NULL) {
      progress$set(value = value, detail = detail)
    }
    values$threshold <- NULL
    
    data <- assay(readcounts())
    data <- data[order(rowVars(data), decreasing = T),]
    
    reference.clustering <- clustering(data, 
                                       method.dist = input$fdistance, 
                                       method.link = input$flink,
                                       pca.enable = input$pca.enable,
                                       pca.center = pca.center(),
                                       pca.scale = pca.scale())
  
    parallel.expr <- function() {
      return(find.minimal.clustering.genes.index(data = data, 
                                                 reference.clustering = reference.clustering,
                                                 method.dist = input$fdistance, 
                                                 method.link = input$flink,
                                                 pca.enable = input$pca.enable,
                                                 pca.center = pca.center(),
                                                 pca.scale = pca.scale(),
                                                 updateProgress = updateProgress))
    }
    
    withParallel(session, input,
                 expr = parallel.expr(),
                 exprsuccess = function(result) {
                   values$threshold <- result
                 },
                 exprfinally = function() {
                   progress$close()
                   shinyjs::enable("calculate")
                 })
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