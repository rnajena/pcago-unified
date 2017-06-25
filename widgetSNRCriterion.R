#'
#' Contains a plot output with download buttons
#'

library(DT)
library(shiny)
source("uiHelper.R")
source("plotAgglomerativeClusteringPlot.R")
source("pca_criterion_snr.R")
source("helpers.R")

snrCriterionUI <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(
    checkboxInput(ns("userdefined"), "User defined clustering parameters", value = F),
    conditionalPanel(conditionalPanel.equals(ns("userdefined"), "true"),
                     selectizeInput(ns("fdistance"), "Distance method", choices = plotAgglomerativeClusteringPlotUI.dist.methodsSelection),
                     selectizeInput(ns("flink"), "Clustering method", choices = plotAgglomerativeClusteringPlotUI.hclust.methodsSelection)),
    tags$p("Result depends on PCA parameters."),
    actionButton(ns("calculate"), "Calculate threshold")
  ))
}



snrCriterionValue_ <- function(input, 
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
      
    # Allow the user to find the minimal gene set for a defined distance and link function
    if(input$userdefined) {
      data <- assay(readcounts())
      data <- data[order(rowVars(data), decreasing = T),]
      values$threshold <- find.minimal.clustering.genes.improved.index(data = data, 
                                                                       method.dist = input$fdistance, 
                                                                       method.link = input$flink,
                                                                       pca.center = pca.center(),
                                                                       pca.scale = pca.scale())
    }
    else {
      stop("Not implemented")
    }
    
  })
  
  return(reactive({ values$threshold }))
  
}

snrCriterionValue <- function(id, readcounts, pca.center, pca.scale) {
  
  return(callModule(snrCriterionValue_,
                    id,
                    readcounts = readcounts,
                    pca.center = pca.center,
                    pca.scale = pca.scale))
  
}