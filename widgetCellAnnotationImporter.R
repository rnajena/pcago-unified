#'
#' Widget that is used for importing/generating conditions
#' 

library(shiny)
source("widgetGenericImporter.R")
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
cellAnnotationImporterUI <- function(id) {
  
  if(!is.character(id)) {
    stop("Invalid arguments!")
  }
  
  ns <- NS(id)
  
  return(tagList(
    genericImporterInput(ns("importer")),
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
cellAnnotationImporterValue_ <- function(input, output, session, readcounts) {
  
  cell.annotation.imported <- genericImporterData("importer",
                                                   importers = reactive(supportedCellAnnotationImporters),
                                                   samples = reactive(availableCellAnnotationSamples),
                                                   generators = reactive(supportedCellAnnotationGenerators),
                                                   exprimport = function(con, importer, parameters) {
                                                     validate(need(readcounts(), "Cannot import cell annotation without read counts!"))
                                                     cells <- colnames(readcounts())
                                                     
                                                     return(importCellAnnotation(con, importer, cells))
                                                   },
                                                   exprsample = function(sample, parameters) {
                                                     
                                                     validate(need(readcounts(), "Cannot import cell annotation without read counts!"))
                                                     cells <- colnames(readcounts())
                                                     
                                                     return(importCellAnnotationSample(sample, cells))
                                                     
                                                   },
                                                   exprgenerator = function(generator, parameters) {
                                                     
                                                     validate(need(readcounts(), "Cannot import cell annotation without read counts!"))
                                                     cells <- colnames(readcounts())
                                                     
                                                     return(importCellAnnotationFromGenerator(generator, cells, parameters))
                                                   },
                                                   exprintegrate = function(data, callback) {
                                                     
                                                     output <- list(conditions = NULL,
                                                                    fragmentlengths = NULL)
                                                     
                                                     choices <- c("Conditions" = "conditions",
                                                                  "Library fragment lengths" = "fragmentlengths")
                                                     selected <- c()
                                                     
                                                     for(entry in data) {
                                                       
                                                       if(is.null(entry)) {
                                                         next()
                                                       }
                                                       
                                                       # We only allow one of each data type
                                                       if(entry$type == "conditions" || entry$type == "fragmentlengths") {
                                                         
                                                         if(entry$type %in% selected) {
                                                           stop(paste(entry$type, "already in set of data! Please remove the existing definition."))
                                                         }
                                                         
                                                         output[[entry$type]] <- entry$data
                                                         selected <- c(selected, entry$type)
                                                       }
                                                       else {
                                                         stop(paste("Unknown entry type", entry$type))
                                                       }
                                                     }
                                                     
                                                     # Check if readcounts are matching the data
                                                     if(!is.null(output$conditions)) {
                                                       if(!setequal(colnames(readcounts()), rownames(output$conditions))) {
                                                         stop("Cell conditions don't match with data in read count table!")
                                                       }
                                                     }
                                                     
                                                     callback(choices, selected)
                                                     
                                                     return(output)
                                                     
                                                   })
  
  # Build conditions
  conditions <- reactive({
    
    validate(need(readcounts(), "Cannot get condition table without read counts!"),
             need(cell.annotation.imported(), "No cell annotation available!"))
    
    validate(need(cell.annotation.imported()$conditions, "No cell conditions available!"))
    
    return(cell.annotation.imported()$conditions)
    
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
cellAnnotationImporterValue <- function(id, readcounts) {
  
  return(callModule(cellAnnotationImporterValue_, id, readcounts = readcounts))
  
}