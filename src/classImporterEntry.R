#'
#' Class that defines an importer entry
#' 

library(shiny)
source("classImporterParameter.R")

#' Entry of generic importer
#'
#' @slot name character. 
#' @slot label character. 
#' @slot parameters List of ImporterParameter objects 
#'
#' @return
#' @export
#'
#' @examples
ImporterEntry <- setClass(
  "ImporterEntry",
  slots = signature(
    name = "character",
    label = "character",
    parameters = "list",
    sample.importer = "character"
  ),
  prototype = list(
    name = NA_character_,
    label = NA_character_,
    parameters = list(),
    sample.importer = NA_character_
  ),
  validity = function(object) {
    
    if(is.na(object@name)) {
      return("No valid name!")
    }
    if(is.na(object@label)) {
      return("No valid label!")
    }
    for(entry in object@parameters) {
      if(!is(entry, "ImporterParameter")) {
        return("Invalid entries in parameter list!")
      }
    }
    
    return(T)
  }
)