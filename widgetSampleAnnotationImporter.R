#'
#' Widget that is used for importing/generating conditions
#' 

library(shiny)
source("widgetGenericImporter.R")
source("sampleAnnotation.R")
source("sampleAnnotationVisuals.R")
source("uiHelper.R")

#' Creates a widget that allows the user to select a method of extracting sample conditions
#'
#' @param id 
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
sampleAnnotationImporterUI <- function(id) {
  
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

#' Extracts the sample conditions based on the user input.
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
sampleAnnotationImporterValue_ <- function(input, output, session, readcounts) {
  
  sample.annotation.imported <- genericImporterData("importer",
                                                   importers = reactive(supportedSampleAnnotationImporters),
                                                   samples = reactive(availableSampleAnnotationSamples),
                                                   generators = reactive(supportedSampleAnnotationGenerators),
                                                   exprimport = function(con, importer, parameters) {
                                                     validate(need(readcounts(), "Cannot import sample annotation without read counts!"))
                                                     samples <- colnames(readcounts())
                                                     
                                                     return(importSampleAnnotation(con, importer, samples))
                                                   },
                                                   exprsample = function(sample, parameters) {
                                                     
                                                     validate(need(readcounts(), "Cannot import sample annotation without read counts!"))
                                                     samples <- colnames(readcounts())
                                                     
                                                     return(importSampleAnnotationSample(sample, samples))
                                                     
                                                   },
                                                   exprgenerator = function(generator, parameters) {
                                                     
                                                     validate(need(readcounts(), "Cannot import sample annotation without read counts!"))
                                                     samples <- colnames(readcounts())
                                                     
                                                     return(importSampleAnnotationFromGenerator(generator, samples, parameters))
                                                   },
                                                   exprintegrate = function(data, callback) {
                                                     
                                                     output <- SampleAnnotation()
                                                     
                                                     choices <- c("Conditions" = "conditions",
                                                                  "Mean fragment lengths" = "meanfragmentlength")
                                                     selected <- c()
                                                     
                                                     if(length(data) == 0) {
                                                       callback(choices, selected)
                                                       return(output)
                                                     }
                                                     
                                                     for(entry in data) {
                                                       
                                                       if(is.null(entry)) {
                                                         next()
                                                       }
                                                      
                                                       output <- mergeSampleAnnotation(output, entry)
                                                      
                                                     }
                                                     
                                                     # Populate callback list
                                                     samples <- colnames(readcounts())

                                                     for(annotation.type in choices) {
                                                       covered.samples <- intersect(sampleAnnotationAnnotatedSamples(output, annotation.type), samples)

                                                       if(length(covered.samples) > 0) {
                                                         names(choices)[choices == annotation.type] <- sprintf("%s (%d/%d)",
                                                                                                               names(choices)[choices == annotation.type],
                                                                                                               length(covered.samples),
                                                                                                               length(samples))
                                                         selected <- c(selected, annotation.type)
                                                       }

                                                     }
                                                     
                                                     
                                                     # Check if readcounts are matching the data
                                                     if(!sampleAnnotationMatchesSamples(output, samples)) {
                                                       stop("Sample annotation doesn't match with data in read count table!")
                                                     }
                                                     
                                                     callback(choices, selected)
                                                     
                                                     return(output)
                                                     
                                                   })
  
  # Build conditions
  conditions <- reactive({
    
    validate(need(readcounts(), "Cannot get condition table without read counts!"),
             need(sample.annotation.imported(), "No sample annotation available!"))
    # ! Needs to be separate. Thanks, R Shiny for ALWAYS TESTING ALL F**** CHECKS!!!
    validate(need(sampleAnnotationHasConditions(sample.annotation.imported()), "No sample conditions available!"))
    
    return(sample.annotation.imported()@conditions)
    
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
    return(mergeSampleAnnotation(sample.annotation.imported(), SampleAnnotation(conditions = conditions.modified())))
  }))
  
}

#' Extracts the sample conditions based on the user input.
#'
#' @param id 
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
sampleAnnotationImporterValue <- function(id, readcounts) {
  
  return(callModule(sampleAnnotationImporterValue_, id, readcounts = readcounts))
  
}