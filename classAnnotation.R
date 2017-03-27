#'
#' This class contains the annotation results
#' As they have an exactly defined structure, use a class
#' 

library(shiny)
source("classGeneFilter.R")

#' Contains the annotation of various genes
#'
#' @slot sequence.info data.frame. 
#' @slot gene.features GeneFilter. 
#' @slot gene.go.terms GeneFilter. 
#'
#' @return
#' @export
#'
#' @examples
Annotation <- setClass(
  "Annotation",
  slots = signature(
    sequence.info = "data.frame",
    gene.features = "GeneFilter",
    gene.go.terms = "GeneFilter"
  ),
  prototype = list(
    sequence.info = data.frame(),
    gene.features = GeneFilter(),
    gene.go.terms = GeneFilter()
  ),
  validity = function(object) {
    return(T)
  }
)

#' Merges two Annotation objects
#'
#' @param object1 Annotation object
#' @param object2 Annotation object
#'
#' @return
#' @export
#' @rdname mergeAnnotation
#'
#' @examples
setGeneric(name = "mergeAnnotation",
           def = function(object1, object2) {
             standardGeneric("mergeAnnotation")
           })

#' @rdname mergeAnnotation
setMethod(f = "mergeAnnotation",
          signature = signature(object1 = "Annotation", object2 = "Annotation"),
          definition = function(object1, object2) {
            
            # Merge sequence info
            if(nrow(object2@sequence.info) != 0) {
              
              #TODO
              if(nrow(object1@sequence.info) != 0) {
                stop("Not implemented!")
              }
              
              object1@sequence.info <- object2@sequence.info
              
            }
            
            # Merge gene filters
            object1@gene.features <- mergeGeneFilter(object1@gene.features, object2@gene.features)
            object1@gene.go.terms <- mergeGeneFilter(object1@gene.go.terms, object2@gene.go.terms)
            
            return(object1)
           
          })

#' Returns a new Annotation object that only contains the genes defined in the restrict.genes vector
#'
#' @param object Annotation object
#' @param restrict.gene Vector of gene names
#'
#' @return
#' @export
#' @rdname annotationRestrictToGenes
#'
#' @examples
setGeneric(name = "annotationRestrictToGenes",
           def = function(object, restrict.gene) {
             standardGeneric("annotationRestrictToGenes")
           })

#' @rdname annotationRestrictToGenes
setMethod(f = "annotationRestrictToGenes",
          signature = signature(object = "Annotation", restrict.gene = "character"),
          definition = function(object, restrict.gene) {
            
            #TODO: sequence.info
            
            object@gene.features <- geneFilterRestrictToGenes(object@gene.features, restrict.gene)
            object@gene.go.terms <- geneFilterRestrictToGenes(object@gene.go.terms, restrict.gene)
            
            return(object)
          })