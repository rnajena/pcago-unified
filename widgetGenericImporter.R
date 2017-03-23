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
#' @param reset.button Show reset button
#'
#' @return Shiny UI controls
#' @export
#'
#' @examples
genericImporterInput <- function(id, 
                                 filetypes, 
                                 importers, 
                                 samples = c(), 
                                 generators = c(),
                                 submit.button.text = "Submit", 
                                 reset.button.text = "Reset",
                                 reset.button = T, 
                                 additional.buttons = tagList(),
                                 additional.content = tagList()) {
  
  ns <- NS(id)
  
  sources <- c("uploaded file" = "upload", 
               "manual input" = "manual")
  if(length(samples) > 0) {
    sources <- c(sources, "sample data" = "sample")
  }
  if(length(generators) > 0) {
    sources <- c(sources, "generate" = "generate")
  }
  
  tags <- tagList(verticalLayout(
    radioButtons(ns("source"), "Load from ...", sources, selected = "upload"),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'manual'"), 
                     textAreaInput(ns("input"), "Manual input")),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'upload'"), 
                     fileInput(ns("fileinput"), "Upload file")),
    conditionalPanel(paste(conditionalPanel.equals(ns("source"), "'manual'"), "||", conditionalPanel.equals(ns("source"), "'upload'")), 
                     selectInput(ns("importer"), "Importer", importers)),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'sample'"), 
                     selectInput(ns("sample"), "Sample data", samples)),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'generate'"), 
                     selectInput(ns("generator"), "Use generator", generators)),
    additional.content,
    fluidPage(fluidRow(
       actionButton(ns("submit"), submit.button.text),
       if(reset.button) actionButton(ns("reset"), reset.button.text) else tagList(),
       additional.buttons)
    )))
    
  return(tags)
    
}

#' Server function of generic importer. Use within callModule and reactive context.
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param input 
#' @param output 
#' @param session 
#' @param exprimport The expression to be called if the submit button is clicked. Parameters are (connection, importer)
#' @param exprsample The expression to be called if the submit button is clicked, but the user wants to select a sample. Parameters are (sample)
#' @param exprgenerator install The expression to be called if the submit button is clicked, but the user wants to select a generator Parameters are (generator)
#'
#' @return Data imported by the importer
#' @export
#'
#' @examples
genericImporterData_ <- function(input, output, session, exprimport, exprsample, exprgenerator) {
  
  variables <- reactiveValues(data = NULL)
  
  observeEvent(input$reset, {
    
    variables$data <- NULL
    showNotification("Data has been reset.", type = "message")
    
  })
  
  observeEvent(input$submit, {
    
    showNotification("Please wait ... importing data", 
                     duration = NULL,
                     closeButton = F,
                     id = "genericimporter.importing")
    shinyjs::disable("submit")
    on.exit({ 
      shinyjs::enable("submit")
      removeNotification(id = "genericimporter.importing") 
      })
    
    if(input$source == "upload") {
      inFile <- input$fileinput
      importer <- input$importer
      
      if(!is.null(inFile)) {
        con <- file(inFile$datapath, "r")
        data <- tryCatch({exprimport(con, importer)}, 
                         error = function(e){
                           showNotification(paste(e), type = "error", duration = NULL)
                           return(NULL)
                           }, 
                         warning = function(w)
                           {
                           showNotification(paste(w), type = "warning", duration = NULL)
                           return(NULL)
                           })
        close(con)
        
        if(!is.null(data)) {
          showNotification("Data has been successfully imported.", type = "message")
        } 
        else {
          showNotification("Error while importing the data", type = "error")
        }
        
        variables$data <- data
      }
      else
      {
        showNotification("Error while importing the data: No file uploaded!", type = "error")
        return(NULL)
      }
        
    }
    else if(input$source == "manual") {
      importer <- input$importer
      con <- textConnection(input$input)
      data <- tryCatch({exprimport(con, importer)}, 
                       error = function(e){
                         showNotification(paste(e), type = "error", duration = NULL)
                         return(NULL)
                       }, 
                       warning = function(w)
                       {
                         showNotification(paste(w), type = "warning", duration = NULL)
                         return(NULL)
                       })
      close(con)
      
      if(!is.null(data)) {
        showNotification("Data has been successfully imported.", type = "message")
      } 
      else {
        showNotification("Error while importing the data", type = "error")
      }
      
      variables$data <- data
    }
    else if(input$source == "sample") {
      
      sample <- input$sample
      
      data <- tryCatch({exprsample(sample)}, 
                       error = function(e){
                         showNotification(paste(e), type = "error", duration = NULL)
                         return(NULL)
                       }, 
                       warning = function(w)
                       {
                         showNotification(paste(w), type = "warning", duration = NULL)
                         return(NULL)
                       })
      
      if(!is.null(data)) {
        showNotification(paste("Loaded sample", sample), type = "message")
      } 
      else {
        showNotification("Error while importing sample!", type = "error")
      }
      
      variables$data <- data
      
    }
    else if(input$source == "generator") {
      
      generator <- input$generator
      
      data <- tryCatch({exprgenerator(generator)}, 
                       error = function(e){
                         showNotification(paste(e), type = "error", duration = NULL)
                         return(NULL)
                       }, 
                       warning = function(w)
                       {
                         showNotification(paste(w), type = "warning", duration = NULL)
                         return(NULL)
                       })
      
      if(!is.null(data)) {
        showNotification(paste("Loaded sample", sample), type = "message")
      } 
      else {
        showNotification("Error while importing sample!", type = "error")
      }
      
      variables$data <- data
      
    }

  })

  return(reactive( { variables$data } ))
}

#' Server function of generic importer. Use within callModule and reactive context.
#'
#' @param id UI element ID
#' @param exprimport The expression to be called if the submit button is clicked. Parameters are (connection, importer)
#' @param exprsample The expression to be called if the submit button is clicked, but the user wants to select a sample. Parameters are (sample)
#' @param exprgenerator install The expression to be called if the submit button is clicked, but the user wants to select a generator Parameters are (generator)
#'
#' @return Data imported by the importer
#' @export
#'
#' @examples
genericImporterData <- function(id, exprimport, exprsample, exprgenerator) {
  
  return(callModule(genericImporterData_, id, exprimport = exprimport, exprsample = exprsample, exprgenerator = exprgenerator))
  
}