#'
#' Defines widget that contains the functions to process read counts.
#'

library(DT)
library(shiny)
source("uiHelper.R")
source("helpers.R")
source("readcounts.R")

readCountPostprocessingUI <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(
    checkboxInput(ns("postprocessing.remove.constant"), "Remove genes with constant readcounts", value = T)
  ))
}

readCountPostprocessingData_ <- function(input, 
                              output, 
                              session,
                              readcounts) {
  
  return(reactive({
    
    validate(need(readcounts(), "[Read count processing] No read counts to process!"))
    
    output <- list(removed.genes = c(), readcounts = readcounts(), operation.transpose = F, operation.remove.constant = F)
    
    # Remove constant read genes
    if(input$postprocessing.remove.constant) {
      processed <- removeConstantReads(output$readcounts)
      output$readcounts <- processed$readcounts
      output$removed.genes <- processed$genes.removed
      output$operation.remove.constant <- T
    }
    
    return(output)
    
  }))
  
}

readCountPostprocessingData <- function(id, readcounts) {
  
  return(callModule(readCountPostprocessingData_,
                    id,
                    readcounts = readcounts))
}