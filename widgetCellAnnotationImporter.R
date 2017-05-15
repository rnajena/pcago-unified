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
                                                     
                                                     output <- CellAnnotation()
                                                     
                                                     choices <- c("Conditions" = "conditions",
                                                                  "Library fragment lengths" = "meanfragmentlengths")
                                                     selected <- c()
                                                     
                                                     if(length(data) == 0) {
                                                       callback(choices, selected)
                                                       return(output)
                                                     }
                                                     
                                                     for(entry in data) {
                                                       
                                                       if(is.null(entry)) {
                                                         next()
                                                       }
                                                      
                                                       output <- mergeCellAnnotation(output, entry)
                                                      
                                                     }
                                                     
                                                     # Populate callback list
                                                     cells <- colnames(readcounts())

                                                     for(annotation.type in choices) {
                                                       covered.cells <- intersect(cellAnnotationAnnotatedCells(output, annotation.type), cells)

                                                       if(length(covered.cells) > 0) {
                                                         names(choices)[choices == annotation.type] <- sprintf("%s (%d/%d)",
                                                                                                               names(choices)[choices == annotation.type],
                                                                                                               length(covered.cells),
                                                                                                               length(cells))
                                                         selected <- c(selected, annotation.type)
                                                       }

                                                     }
                                                     
                                                     
                                                     # Check if readcounts are matching the data
                                                     if(!cellAnnotationMatchesCells(output, cells)) {
                                                       stop("Cell annotation doesn't match with data in read count table!")
                                                     }
                                                     
                                                     callback(choices, selected)
                                                     
                                                     return(output)
                                                     
                                                   })
  
  # Build conditions
  conditions <- reactive({
    
    validate(need(readcounts(), "Cannot get condition table without read counts!"),
             need(cell.annotation.imported(), "No cell annotation available!"))
    # ! Needs to be separate. Thanks, R Shiny for ALWAYS TESTING ALL F**** CHECKS!!!
    validate(need(cellAnnotationHasConditions(cell.annotation.imported()), "No cell conditions available!"))
    
    return(cell.annotation.imported()@conditions)
    
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
  
  return(reactive({
    return(mergeCellAnnotation(cell.annotation.imported(), CellAnnotation(conditions = conditions.modified())))
  }))
  
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