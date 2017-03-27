#'
#' Used for annotation.
#' Contains a class that maps each filter string to a list of genes
#' 

library(shiny)


#' Holds multiple filter strings. Maps each filter string to a list of genes where it applies.
#'
#' @slot data list. 
#'
#' @return
#' @export
#'
#' @examples
GeneFilter <- setClass(
  "GeneFilter",
  slots = signature(
    data = "list"
  ),
  prototype = list(
    data = list()
  ),
  validity = function(object) {
    
    if(!is.null(names(object@data)) && !is.character(names(object@data))) {
      return("Keys must be strings!")
    }
    
    if(any(unlist(lapply(object@data, function(x) { !is.character(x) })))) {
      return("Data must have following structure: Filter maps to vector of gene names")
    }
    
    return(T)
  }
)

#' Merges two GeneFilter objects
#'
#' @param object1 GeneFilter object
#' @param object2 GeneFilter object
#'
#' @return
#' @export
#' @rdname mergeGeneFilter
#'
#' @examples
setGeneric(name = "mergeGeneFilter",
           def = function(object1, object2) {
             standardGeneric("mergeGeneFilter")
           })

#' @rdname mergeGeneFilter
setMethod(f = "mergeGeneFilter",
          signature = signature(object1 = "GeneFilter", object2 = "GeneFilter"),
          definition = function(object1, object2) {
            
            for(key in names(object2@data)) {
              if(key %in% names(object1@data)) {
                object1@data[[key]] <- unique(c(object1@data[[key]], object2@data[[key]]))
              }
              else {
                object1@data[[key]] <- object2@data[[key]]
              }
            }
            
            validObject(object1)
            return(object1)
          })

#' Inverts the data in the GeneFilter object.
#' Returns a list that maps gene name to a list of features
#'
#' @param object GeneFilter object
#'
#' @return
#' @export
#' @rdname invertGeneFilter
#'
#' @examples
setGeneric(name = "invertGeneFilter",
           def = function(object) {
             standardGeneric("invertGeneFilter")
           })

#' @rdname invertGeneFilter
setMethod(f = "invertGeneFilter",
          signature = signature(object = "GeneFilter"),
          definition = function(object) {
            
            output <- list()
            
            # TODO: Find better algorithm (nested apply so far is worse!)
            for(key in names(object@data)) {
              for(gene in object@data[[key]]) {
                output[[gene]] <- c(output[[gene]], key)
              }
            }
            
            return(output)
            
          })

#' Returns the filter keys in the GeneFilter object
#'
#' @param object GeneFilter object
#'
#' @return
#' @export
#' @rdname geneFilterKeys
#'
#' @examples
setGeneric(name = "geneFilterKeys",
           def = function(object) {
             standardGeneric("geneFilterKeys")
           })

#' @rdname geneFilterKeys
setMethod(f = "geneFilterKeys",
          signature = signature(object = "GeneFilter"),
          definition = function(object) {
            return(names(object@data))
          })

#' Returns the genes in the GeneFilter object
#'
#' @param object GeneFilter object
#'
#' @return
#' @export
#' @rdname geneFilterGenes
#'
#' @examples
setGeneric(name = "geneFilterGenes",
           def = function(object) {
             standardGeneric("geneFilterGenes")
           })

#' @rdname geneFilterGenes
setMethod(f = "geneFilterGenes",
          signature = signature(object = "GeneFilter"),
          definition = function(object) {
            return(unique(unlist((object@data))))
          })

#' Returns the vector of genes that belongs to the filter entries
#'
#' @param object GeneFilter object
#' @param filter.keys Vector of filter keys
#'
#' @return
#' @export
#' @rdname geneFilterSelectGenes
#'
#' @examples
setGeneric(name = "geneFilterSelectGenes",
           def = function(object, filter.keys) {
             standardGeneric("geneFilterSelectGenes")
           })

#' @rdname geneFilterSelectGenes
setMethod(f = "geneFilterSelectGenes",
          signature = signature(object = "GeneFilter", filter.keys = "character"),
          definition = function(object, filter.keys) {
            return(unlist(object@data[filter.keys]))
          })

#' Returns a new GeneFilter object that only contains the genes defined in the restrict.genes vector
#'
#' @param object GeneFilter object
#' @param restrict.gene Vector of gene names
#'
#' @return
#' @export
#' @rdname geneFilterRestrictToGenes
#'
#' @examples
setGeneric(name = "geneFilterRestrictToGenes",
           def = function(object, restrict.gene) {
             standardGeneric("geneFilterRestrictToGenes")
           })

#' @rdname geneFilterRestrictToGenes
setMethod(f = "geneFilterRestrictToGenes",
          signature = signature(object = "GeneFilter", restrict.gene = "character"),
          definition = function(object, restrict.gene) {
            
            object@data <- lapply(object@data, function(x) { intersect(x, restrict.gene)  })
            object@data <- object@data[lapply(object@data, length) > 0]
            
            return(object)
          })