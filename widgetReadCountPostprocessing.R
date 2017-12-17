#'
#' Defines widget that contains the functions to process read counts.
#'

library(DT)
library(shiny)
source("uiHelper.R")
source("helpers.R")
source("readcounts.R")
source("defaultParameters.R")

readCountPostprocessingUI <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(
    checkboxInput(ns("postprocessing.remove.constant"), "Remove genes with constant readcounts", value = default.data.postprocessing.removeconstant)
  ))
}

readCountPostprocessingData_ <- function(input, 
                              output, 
                              session,
                              dataset) {
  
  return(reactive({
    
    validate(need(dataset(), "[Read count processing] No read counts to process!"))
    validate(need(dataset()$readcounts.normalized, "[Read count processing] No read counts to process!"))
    
    dataset <- dataset()
    
    output <- list(removed.genes = c(), operation.remove.constant = F)
    
    # Remove constant read genes
    if(input$postprocessing.remove.constant) {
      processed <- removeConstantReads(dataset$readcounts.normalized)
      output$removed.genes <- processed$genes.removed
      output$operation.remove.constant <- T
      dataset$readcounts.processed <- processed$readcounts
    }
    
    dataset$readcounts.postprocessing.parameters <- output
    
    return(dataset)
    
  }))
  
}

readCountPostprocessingData <- function(id, dataset) {
  
  return(callModule(readCountPostprocessingData_,
                    id,
                    dataset = dataset))
}