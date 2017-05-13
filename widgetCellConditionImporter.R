#'
#' Widget that is used for importing/generating conditions
#' 

library(shiny)
source("widgetGenericImporter.R")
source("widgetIntegratingGenericImporter.R")
source("cellAnnotation.R")
source("cellAnnotationVisuals.R")
source("uiHelper.R")

#' Creates a widget that allows the user to select a method of extracting cell conditions
#'
#' @param id 
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
cellConditionImporterUI <- function(id) {
  
  if(!is.character(id)) {
    stop("Invalid arguments!")
  }
  
  ns <- NS(id)
  
  return(tagList(
    integratingGenericImporterInput(ns("importer")),
    hDivider(),
    selectizeInput(ns("conditioneditor"), 
                   label = "Rearrange conditions", 
                   choices = c(),
                   multiple = T,
                   options = list(
                     plugins = c("drag_drop", "remove_button")
                   ))
  ))
  
}

#' Extracts the cell conditions based on the user input.
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param input 
#' @param output 
#' @param session 
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
cellConditionImporterValue_ <- function(input, output, session, readcounts) {
  
  # cell.conditions.imported <- genericImporterData("importer",
  #                                                 importers = reactive(supportedCellConditionImporters),
  #                                                 samples = reactive(availableCellConditionSamples),
  #                                                 generators = reactive(supportedConditionVisualsGenerators),
  #                                                 exprimport = function(con, importer, parameters) {
  # 
  #                                                   validate(need(readcounts(), "Cannot import condition table without read counts!"))
  # 
  #                                                   cells <- colnames(readcounts())
  #                                                   return(importCellConditions(con, importer, cells))
  #                                                 },
  #                                                 exprsample = function(sample, parameters) {
  # 
  #                                                   validate(need(readcounts(), "Cannot import condition table without read counts!"))
  #                                                   cells <- colnames(readcounts())
  # 
  #                                                   return(importCellConditionsSample(sample, cells))
  # 
  #                                                 })
  
  cell.annotation.imported <- integratingGenericImporterData("importer",
                                                             importers = reactive(supportedCellConditionImporters),
                                                             samples = reactive(availableCellConditionSamples),
                                                             generators = reactive(supportedConditionVisualsGenerators),
                                                             exprimport = function(con, importer, parameters) {
                                                               validate(need(readcounts(), "Cannot import cell annotation without read counts!"))
                                                               cells <- colnames(readcounts())
                                                               return(importCellConditions(con, importer, cells))
                                                             },
                                                             exprsample = function(sample, parameters) {
                                                               
                                                               validate(need(readcounts(), "Cannot import cell annotation without read counts!"))
                                                               cells <- colnames(readcounts())
                                                               return(importCellConditionsSample(sample, cells))
                                                               
                                                             },
                                                             exprgenerator = function(generator, parameters) {
                                                               return(NULL)
                                                             },
                                                             exprintegrate = function(data, callback) {
                                                               
                                                               if(length(data) == 0) {
                                                                 return(NULL)
                                                               }
                                                               else {
                                                                 return(data[[1]])
                                                               }
                                                               
                                                             })
  
  # observeEvent(cell.conditions.imported(), { })
  
  # Build conditions
  conditions <- reactive({
    
    validate(need(readcounts(), "Cannot get condition table without read counts!"))
    validate(need(cell.annotation.imported(), "No cell annotation available!"))
    
    return(cell.annotation.imported())
    
  })
  
  # Allow the conditions to be rearranged/changed
  # Update the selectize that allows removing/rearranging conditions
  observeEvent(conditions(), {
    
    validate(need(conditions(), "Need conditions to update UI!"))
    
    updateSelectInput(session, "conditioneditor", 
                      choices = colnames(conditions()), 
                      selected = colnames(conditions()))
  })
  
  # Build new, reordered conditions
  conditions.modified <- reactive({
    
    validate(need(conditions(), "No conditions loaded!"),
             need(ncol(conditions()) >= 2, "There should be at least 2 conditions!"))
    
    selected <- intersect(input$conditioneditor, colnames(conditions()))
    
    validate(need(length(selected) >= 2, "here should be at least 2 conditions!"))
    
    return(conditions()[, selected])
  })
  
  return(conditions.modified)
  
}

#' Extracts the cell conditions based on the user input.
#'
#' @param id 
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
cellConditionImporterValue <- function(id, readcounts) {
  
  return(callModule(cellConditionImporterValue_, id, readcounts = readcounts))
  
}