#
# Contains an importer widget and necessary functions
#

library(shiny)
library(shinyBS)
library(shinyjs)

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