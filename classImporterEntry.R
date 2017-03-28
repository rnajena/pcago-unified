#'
#' Class that defines an importer entry
#' 

library(shiny)
source("classImporterParameter.R")

ImporterEntry <- setClass(
  "ImporterEntry",
  slots = signature(
    name = "character",
    label = "character",
    parameters = "list"
  ),
  prototype = list(
    name = NA_character_,
    label = NA_character_,
    parameters = list()
  ),
  validity = function(object) {
    
    if(is.na(object@name)) {
      return("No valid name!")
    }
    for(entry in object@parameters) {
      if(!is(entry, "ImporterParameter")) {
        return("Invalid entries in parameter list!")
      }
    }
    
    return(T)
  }
)