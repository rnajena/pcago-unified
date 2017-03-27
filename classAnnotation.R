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