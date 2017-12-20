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
source("environment.R")

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
geneAnnotationImporterValue_ <- function(input, output, session, dataset) {
  
  readcounts <- reactive({
    validate(need(dataset(), "No preprocessed read counts available!"))
    return(dataset()$readcounts.preprocessed)
  })
  
  annotation <- (genericImporterData("importer", 
                             importers = reactive(supportedGeneAnnotationImporters),
                             samples = reactive(availableGeneAnnotationSamples),
                             generators = reactive(supportedGeneAnnotationGenerators),
                             parallel = parallelized.gene.annotation.importer,
                             exprimport = function(con, importer, parameters) {
                               return(importGeneInformationFromAnnotation(con, importer, dataset(), parameters))
                             },
                             exprsample = function(sample, parameters) {
                               return(importSampleGeneInformation(sample, dataset(), parameters))
                             },
                             exprgenerator = function(generator, parameters) {
                               return(generateGeneInformation(generator, dataset(), parameters))
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
  
  return(reactive({
    dataset <- dataset()
    dataset$gene.annotation <- annotation()
    return(dataset)
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
geneAnnotationImporterValue <- function(id, dataset) {
  
  return(callModule(geneAnnotationImporterValue_, id, dataset = dataset))
  
}