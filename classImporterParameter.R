#' 
#' Class that defines a parameter for the generic importer
#' The problem is that we may want additional arguments from the user
#' without designing an UI for each individual importer
#' This class will solve this.
#' 

library(shiny)

#' An additional parameter for generic importer
#'
#' @slot name character. 
#' @slot label character. 
#' @slot type Currently supported: select (A selectize input), checkboxes (Multiple checkboxes), lineedit (A text string editor), checkbox (One single checkbox)
#' @slot select.values A list, vector or a reactive that returns one of those. Used if type = select
#' @slot checkboxes.options A list, vector or a reactive that returns one of those. Options for type = checkboxes
#' @slot checkboxes.selected A list, vector or a reactive that returns one of those. Selected options for type = checkboxes
#' @slot checkbox.selected A boolean value indicating if the checbox is selected. Only for type = checkbox
#' @slot lineedit.default A character string or a reactive that returns a character string. Default text for type = lineedit
#'
#' @return
#' @export
#'
#' @examples
ImporterParameter <- setClass(
  "ImporterParameter",
  slots = signature(
    "name" = "character",
    "label" = "character",
    "type" = "character",
    "select.values" = "ANY",
    "checkboxes.options" = "ANY",
    "checkboxes.selected" = "ANY",
    "checkbox.selected" = "logical",
    "lineedit.default" = "ANY"
  ),
  prototype = list(
    "name" = NA_character_,
    "label" = NA_character_,
    "type" = NA_character_,
    "select.values" = NA,
    "checkboxes.options" = NA,
    "checkboxes.selected" = NA,
    "checkbox.selected" = F,
    "lineedit.default" = ""
  ),
  validity = function(object) {
    
    if(is.na(object@name)) {
      return("No name defined!")
    }
    if(is.na(object@label)) {
      return("No label defined!")
    }
    if(!(object@type %in% c("select", "checkboxes", "lineedit", "checkbox"))) {
      return(paste("Unknown type", object@type))
    }
    
    return(T)
  }
)