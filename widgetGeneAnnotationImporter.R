#'
#' Widget that is used for importing/generating conditions
#' 

library(shiny)
source("widgetGenericImporter.R")
source("geneAnnotation.R")
source("geneAnnotationBioMart.R")
source("geneAnnotationGRanges.R")
source("geneAnnotationHub.R")
source("uiHelper.R")

#' Creates a widget that allows the user to select a method of extracting gene annotations
#'
#' @param id 
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
geneAnnotationImporterUI <- function(id) {
  
  if(!is.character(id)) {
    stop("Invalid arguments!")
  }
  
  ns <- NS(id)
  
  return(tagList(
    genericImporterInput(ns("importer")))
  )
  
}

#' Extracts the gene annotation based on the user input.
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
geneAnnotationImporterValue_ <- function(input, output, session, readcounts) {
  
  return(genericImporterData("importer", 
                             importers = reactive(supportedGeneAnnotationImporters),
                             samples = reactive(availableGeneAnnotationSamples),
                             generators = reactive(supportedGeneAnnotationGenerators),
                             exprimport = function(con, importer, parameters) {
                               return(importGeneInformationFromAnnotation(con, importer, readcounts(), parameters))
                             },
                             exprsample = function(sample, parameters) {
                               return(importSampleGeneInformation(sample, readcounts(), parameters))
                             },
                             exprgenerator = function(generator, parameters) {
                               return(generateGeneInformation(generator, readcounts(), parameters))
                             },
                             exprintegrate = function(data, callback) {
                               output <- GeneAnnotation()
                               genes <- rownames(readcounts())
                               
                               # Merge data into output
                               for(current.data in data) {
                                 if(!is.null(data)) {
                                   output <- mergeGeneAnnotation(output, current.data)
                                 }
                               }
                               
                               # Build our callback for the user
                               choices = GeneAnnotationEntryNames
                               selected <- c()
                               
                               for(annotation.type in choices) {
                                 covered.genes <- intersect(geneAnnotationAnnotatedGenes(output, annotation.type), genes)

                                 if(length(covered.genes) > 0) {
                                   names(choices)[choices == annotation.type] <- sprintf("%s (%d/%d)",
                                                                                         names(choices)[choices == annotation.type],
                                                                                         length(covered.genes),
                                                                                         length(genes))
                                   selected <- c(selected, annotation.type)
                                 }

                               }
                             
                               
                               callback(choices, selected)
                               return(output)
                             }))
  
}

#' Extracts the gene annotation based on the user input.
#'
#' @param id 
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
geneAnnotationImporterValue <- function(id, readcounts) {
  
  return(callModule(geneAnnotationImporterValue_, id, readcounts = readcounts))
  
}