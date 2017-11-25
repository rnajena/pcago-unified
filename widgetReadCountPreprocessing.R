#'
#' Defines widget that contains the functions to process read counts.
#'

library(DT)
library(shiny)
source("uiHelper.R")
source("helpers.R")
source("readcounts.R")

readCountPreprocessingUI <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(
    checkboxInput(ns("preprocessing.transpose"), "Transpose table", value = F),
    checkboxInput(ns("preprocessing.zero"), "Remove genes with zero readcounts", value = T)
  ))
}

readCountPreprocessingData_ <- function(input, 
                                        output, 
                                        session,
                                        dataset) {
  
  return(reactive({
    
    validate(need(dataset(), "[Read count processing] No read counts to process!"))
    validate(need(dataset()$readcounts.raw, "[Read count processing] No read counts to process!"))
    
    dataset <- dataset()
    
    readcounts.preprocessing.parameters <- list(removed.genes = c(), operation.transpose = F, operation.remove.zero = F)
    dataset$readcounts.preprocessed <- dataset$readcounts.raw
    
    # Transpose read counts
    if(input$preprocessing.transpose) {
      readcounts.preprocessing.parameters$operation.transpose <- T
      dataset$readcounts.preprocessed <- transposeReadCounts(output$readcounts)
    }
    
    # Prevent too many samples
    if(ncol(dataset$readcounts.preprocessed) > 100) {
      showNotification("There are over 100 samples! I won't calculate with that! Maybe you need to transpose your data?", type = "error")
      dataset()$readcounts.preprocessed <- NULL
      return(dataset)
    }
    
    # Remove zero
    if(input$preprocessing.zero) {
      processed <- removeZeroReads(dataset()$readcounts.raw)
      dataset$readcounts.preprocessed <- processed$readcounts
      readcounts.preprocessing.parameters$removed.genes <- processed$genes.removed
      readcounts.preprocessing.parameters$operation.remove.zero <- T
    }
    
    dataset$readcounts.preprocessing.parameters <- readcounts.preprocessing.parameters
    
    return(dataset)
    
  }))
  
}

readCountPreprocessingData <- function(id, dataset) {
  
  return(callModule(readCountPreprocessingData_,
                    id,
                    dataset = dataset))
}