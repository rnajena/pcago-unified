#'
#' Widget that is used for importing/generating conditions
#' 

library(shiny)
source("widgetGenericImporter.R")

cellConditionImporterUI <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(
    radioButtons(ns("mode"),
                 "Source of cell conditions for visualization:",
                 c("Column names" = "column",
                   "Extract from columns" = "extract",
                   "Upload" = "upload"),
                 selected = "column"),
    conditionalPanel(conditionalPanel.equals(ns("mode"), "'extract'"),
                     selectizeInput(ns("separator"),
                                    label = "Separator",
                                    choices = c("_", ".", ":", "#"),
                                    selected = "_",
                                    options = list("create" = T))),
    conditionalPanel(conditionalPanel.equals(ns("mode"), "'upload'"),
                     genericImporterInput(ns("importer"),filetypes = c("text/csv"), importers = c("Default")))
  ))
  
}

cellConditionImporter <- function(input, output, session, readcounts) {
  
  return(reactive({
    
    validate(need(readcounts(), "Cannot get condition table without read counts!"))
    
    if(input$mode == "column") {
      return(generateConditionTable(readcounts(), sep = ""))
    }
    else if(input$mode == "extract") {
      return(generateConditionTable(readcounts(), sep = input$separator))
    }
    else if(input$mode == "upload") {
      return(NULL) #todo
    }
    else {
      return(NULL)
    }
    
  }))
  
}