#'
#' Contains an importer widget that allows the user to upload a file,
#' input the data manually or choose a sample data set
#'

library(shiny)
library(shinyBS)
library(shinyjs)
source("uiHelper.R")

#' Creates a generic import widget UI
#'
#' @param id Id of the control
#' @param filetypes Accepted filetypes of fileInput sub-control
#' @param importers Vector of importer names that will be passed to the import function
#' @param samples Vector of sample data
#'
#' @return Shiny UI controls
#' @export
#'
#' @examples
genericImporterInput <- function(id, filetypes, importers, samples = c()) {
  
  ns <- NS(id)
  
  tags <- tagList(verticalLayout(
    radioButtons(ns("source"), "Load from ...", c("uploaded file" = "upload", "manual input" = "manual", "sample data" = "sample"), selected = "upload"),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'manual'"), 
                     textAreaInput(ns("input"), "Manual input")),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'upload'"), 
                     fileInput(ns("fileinput"), "Upload file")),
    conditionalPanel(paste(conditionalPanel.equals(ns("source"), "'manual'"), "||", conditionalPanel.equals(ns("source"), "'upload'")), 
                     selectInput(ns("importer"), "Importer", importers)),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'sample'"), 
                     selectInput(ns("sample"), "Sample data", samples)),
    fluidPage(fluidRow(
      actionButton(ns("submit"), "Submit"),
      actionButton(ns("reset"), "Reset"))                                   
    )))
    
  return(tags)
    
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
genericImporter <- function(input, output, session, exprimport, exprsample) {
  
  data <- eventReactive(input$submit, {
    
    if(input$source == "upload") {
      inFile <- input$fileinput
      importer <- input$importer
      
      if(!is.null(inFile)) {
        con <- file(inFile$datapath, "r")
        data <- tryCatch({exprimport(con, importer)}, 
                         error = function(e){
                           print(e)
                           return(NULL)
                           }, 
                         warning = function(w)
                           {
                           print(w)
                           return(NULL)
                           })
        close(con)
        
        if(is.null(data)) {
          showNotification("Error while importing the data!", type = "error")
        } 
        else {
          showNotification("Data has been successfully imported.", type = "message")
        }
        
        return(data)
      }
      else
      {
        return(NULL)
      }
        
    }
    else if(input$source == "manual") {
      importer <- input$importer
      con <- textConnection(input$input)
      data <- tryCatch({exprimport(con, importer)}, 
                       error = function(e){
                         print(e)
                         return(NULL)
                       }, 
                       warning = function(w)
                       {
                         print(w)
                         return(NULL)
                       })
      close(con)
      
      if(is.null(data)) {
        showNotification("Error while importing the data!", type = "error")
      } 
      else {
        showNotification("Data has been successfully imported.", type = "message")
      }
      
      return(data)
    }
    else if(input$source == "sample") {
      
      sample <- input$sample
      showNotification(paste("Loaded sample", sample), type = "message")
      return(exprsample(sample))
      
    }

  })

  return(data)
}