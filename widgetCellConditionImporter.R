#'
#' Widget that is used for importing/generating conditions
#' 

library(shiny)
source("widgetGenericImporter.R")
source("conditions.R")

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
                     genericImporterInput(ns("importer"),filetypes = supportedCellConditionFileTypes, importers = supportedCellConditionImporters))
  ))
  
}

cellConditionImporterValue_ <- function(input, output, session, readcounts) {
  
  cell.conditions.imported <- genericImporterData("importer", exprimport = function(con, importer) {
    
    validate(need(readcounts(), "Cannot import condition table without read counts!"))
    
    cells <- colnames(readcounts())
    return(importCellConditions(con, importer, cells))
  })
  
  observeEvent(cell.conditions.imported(), { })
  
  return(reactive({
    
    validate(need(readcounts(), "Cannot get condition table without read counts!"))
    
    if(input$mode == "column") {
      return(generateConditionTable(readcounts(), sep = ""))
    }
    else if(input$mode == "extract") {
      return(generateConditionTable(readcounts(), sep = input$separator))
    }
    else if(input$mode == "upload") {
      validate(need(cell.conditions.imported(), "No imported data available."))
      return(cell.conditions.imported())
    }
    else {
      return(NULL)
    }
    
  }))
  
}

cellConditionImporterValue <- function(id, readcounts) {
  
  return(callModule(cellConditionImporterValue_, id, readcounts = readcounts))
  
}