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
                                        readcounts) {
  
  return(reactive({
    
    validate(need(readcounts(), "[Read count processing] No read counts to process!"))
    
    output <- list(removed.genes = c(), readcounts = readcounts(), operation.transpose = F, operation.remove.zero = F)
    
    # Transpose read counts
    if(input$preprocessing.transpose) {
      output$operation.transpose <- T
      output$readcounts <- transposeReadCounts(output$readcounts)
    }
    
    # Prevent too many samples
    if(ncol(output$readcounts) > 100) {
      showNotification("There are over 100 samples! I won't calculate with that! Maybe you need to transpose your data?", type = "error")
      return(NULL)
    }
    
    # Remove constant read genes
    if(input$preprocessing.zero) {
      processed <- removeConstantReads(output$readcounts)
      output$readcounts <- processed$readcounts
      output$removed.genes <- processed$genes.removed
      output$operation.remove.zero <- T
    }
    
    return(output)
    
  }))
  
}

readCountPreprocessingData <- function(id, readcounts) {
  
  return(callModule(readCountPreprocessingData_,
                    id,
                    readcounts = readcounts))
}