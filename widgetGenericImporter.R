#
# Contains an importer widget and necessary functions
#

library(shiny)
library(shinyBS)
library(shinyjs)

#' Creates a generic import widget UI
#'
#' @param id Id of the control
#' @param filetypes Accepted filetypes of fileInput sub-control
#' @param importers Vector of importer names that will be passed to the import function
#'
#' @return Shiny UI controls
#' @export
#'
#' @examples
genericImporterInput <- function(id, filetypes, importers) {
  
  ns <- NS(id)
  
  tagList(
    verticalLayout(
      textAreaInput(ns("input"), "Manual input"),
      fileInput(ns("fileinput"), "From file"),
      selectInput(ns("importer"), "Importer", importers),
      fluidPage(fluidRow(
        actionButton(ns("submit"), "Submit"),
        actionButton(ns("reset"), "Reset"))                                   
      ))
  )
}

#' Server function of generic importer. Use within callModule and reactive context.
#'
#' @param input 
#' @param output 
#' @param session 
#' @param exprimport The expression to be called if the submit button is clicked. Parameters are (connection, importer)
#'
#' @return Data imported by the importer
#' @export
#'
#' @examples
genericImporter <- function(input, output, session, exprimport) {
  
  data <- eventReactive(input$submit, {
    
      importer <- input$importer
      inFile <- input$fileinput
      
      if(is.null(inFile))
      {
        con <- textConnection(input$input)
        data <- exprimport(con, importer)
        close(con)
        
        return(data)
      }
      else
      {
        con <- file(inFile$datapath, "r")
        data <- exprimport(con, importer)
        close(con)
        
        return(data)
      }

  })

  return(data())
}