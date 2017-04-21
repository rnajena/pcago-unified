#' 
#' A widget that displays optional processing steps
#' 

library(shiny)

#' Creates a widget that displays a linear process in a tabbed interface
#'
#' @param id 
#' @param title 
#'
#' @return
#' @export
#'
#' @examples
processingStepsWidgetUI <- function(id, title) {
  
  ns <- NS(id)
  
  return(tags$div(class = "processing-steps-widget", bsCollapsePanel(title, uiOutput(ns("steps.ui")))))
  
}

#' Displays the output of the reactives in ... in the UI
#' #' 
#' Processing step info functions must return NULL or a list with title and content entries.
#' 
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param id 
#' @param ... Reactives that return the processing information as list. Return NULL to skip the processing step.
#'
#' @return
#' @export
#'
#' @examples
processingStepsWidgetData_ <- function(input, output, session, ...) {
  
  output$steps.ui <- renderUI({
    
    steps <- list()
    
    for(processing.output in list(...)) {
      
      output <- processing.output()
      
      if(!is.null(output)) {
        
        title <- paste0(length(steps) + 1, ". ", output$title)
        steps[[length(steps) + 1]] <- tabPanel(title, wellPanel(output$content), icon = icon("chevron-right"))
        
      }
      
    }
    
    parameters <- steps
    parameters$type <- "pills"
    return(do.call(tabsetPanel, parameters))
    
  })
  
}

#' Displays the output of the reactives in ... in the UI
#' 
#' Processing step info functions must return NULL or a list with title and content entries.
#'
#' @param id 
#' @param ... Reactives that return the processing information as list. Return NULL to skip the processing step.
#'
#' @return
#' @export
#'
#' @examples
processingStepsWidgetData <- function(id, ...) {
  
  return(callModule(processingStepsWidgetData_, id, ...))
  
}