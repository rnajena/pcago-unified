#' 
#' Class that defines a parameter for the generic importer
#' The problem is that we may want additional arguments from the user
#' without designing an UI for each individual importer
#' This class will solve this.
#' 

library(shiny)

ImporterParameter <- setClass(
  "ImporterParameter",
  slots = signature(
    "name" = "character",
    "label" = "character",
    "type" = "character",
    "select.values" = "ANY"
  ),
  prototype = list(
    "name" = NA_character_,
    "label" = NA_character_,
    "type" = NA_character_,
    "select.values" = NA
  ),
  validity = function(object) {
    
    if(is.na(object@name)) {
      return("No name defined!")
    }
    if(!(object@type %in% c("select"))) {
      return(paste("Unknown type", object@type))
    }
    
    return(T)
  }
)