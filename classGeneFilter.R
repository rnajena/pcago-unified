#'
#' Used for annotation.
#' Contains a class that maps each filter string to a list of genes
#' 

library(shiny)
library(R6)

# TODO: buildGeneFilter, mergeGeneFilter, geneFilterKeys, geneFilterGenes, geneFilterSelectGenes, geneFilterRestrictToGenes


#' Holds multiple filter strings. Maps each filter string to a list of genes where it applies.
#'
#' @slot data list. 
#'
#' @return
#' @export
#'
#' @examples
GeneFilter <- R6Class("GeneFilter",
                      public = list(data = list(),
                                    initialize = function(data = list()) {
                                      self$data <- data
                                    }))

GeneFilter$set("public", "is_valid", function() {
  if (length(self$data) == 0) {
    return(T)
  }
  
  if (!is.null(names(self$data)) &&
      (!is.character(names(self$data))) ||
      any(is.na(names(self$data)))) {
    return("Keys must be strings and not NA!")
  }
  
  if (any(unlist(lapply(self$data, function(x) {
    !is.character(x)  || is.na(x)
  })))) {
    return(
      "Data must have following structure: Filter maps to vector of gene names that are not NA!"
    )
  }
  
  return(T)
})

#' Loads data from a named vector where the names are the criteria
#'
#' @param vector named character vector
#'
#' @return
#' @export
#' @rdname buildGeneFilter
#'
#' @examples
GeneFilter$set("public", "load_from", function(vector) {
  
  output <- list()
  
  for(crit in names(vector)) {
    
    if(is.na(crit)) {
      next()
    }
    
    values <- na.omit(vector[names(vector) == crit])
    
    if(length(values) > 0) {
      output[[setNames(crit, NULL)]] <- as.vector(setNames(values, NULL))
    }
    
    
  }
  
  self$data <- output
  
})

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
GeneFilter$set("public", "merge_with", function(object2) {
  
  for(key in names(object2$data)) {
    if(key %in% names(self$data)) {
      self$data[[key]] <- unique(c(self$data[[key]], object2$data[[key]]))
    }
    else {
      self$data[[key]] <- object2$data[[key]]
    }
  }
  
  return(self)
  
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
GeneFilter$set("public", "invert_data", function() {
  
  output <- list()
  
  # TODO: Find better algorithm (nested apply so far is worse!)
  for(key in names(self$data)) {
    for(gene in self$data[[key]]) {
      output[[gene]] <- c(output[[gene]], key)
    }
  }
  
  return(output)
  
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
GeneFilter$set("public", "invert", function() {
  
  data <- self$invert_data()
  return(GeneFilter$new(data = data))
  
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
GeneFilter$set("public", "get_filter_keys", function() {
  return(names(self$data))
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
GeneFilter$set("public", "get_genes", function() {
  return(unique(unlist((self$data))))
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
GeneFilter$set("public", "get_genes_for", function(filter.keys) {
  return(unlist(self$data[filter.keys]))
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
GeneFilter$set("public", "restrict_to", function(restrict.gene) {
  object <- self$clone()
  object$data <- lapply(object$data, function(x) { intersect(x, restrict.gene)  })
  object$data <- object$data[lapply(object$data, length) > 0]
  
  return(object)
})
