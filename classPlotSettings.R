#'
#' A class to manage plot settings
#' As there are multiple steps, default values and more
#' a class becomes viable.
#' 

library(shiny)

#' Defines the plot settings class that holds various plot settings
#' and supports overwriting non-existing values with default values
#'
#' @slot width numeric. 
#' @slot height numeric. 
#' @slot dpi numeric. 
#' @slot title character. 
#' @slot subtitle character. 
#' @slot legend.color character. 
#' @slot legend.shape character. 
#'
#' @return
#' @export
#'
#' @examples
PlotSettings <- setClass(
  "PlotSettings",
  slots = c(
    width = "numeric",
    height = "numeric",
    dpi = "numeric",
    title = "character",
    subtitle = "character",
    legend.color = "character",
    legend.shape = "character"
  ),
  prototype = list(
    width = NA_integer_,
    height = NA_integer_,
    dpi = NA_integer_,
    title = NA_character_,
    subtitle = NA_character_,
    legend.color = NA_character_,
    legend.shape = NA_character_
  ),
  
  #' Define the validity function.
  #' It is called if a non-empty object is created
  validity = function(object) {
    
    if(!is.na(object@width) && object@width <= 0) {
      return("Invalid width!")
    }
    if(!is.na(object@height) && object@height <= 0) {
      return("Invalid height!")
    }
    if(!is.na(object@dpi) && object@dpi <= 0) {
      return("Invalid dpi!")
    }
    
    return(T)
  }
)

#' Overwrites NA plot settings with values provided in ellipsis
#'
#' @param object A PlotSettings object
#' @param ... Settings to overwrite with default settings if they are NA
#'
#' @return
#' @rdname setNA
#' @export
#'
#' @examples
setGeneric(name = "setNA",
           def = function(object, ...) {
             standardGeneric("setNA")
           })

#' @rdname setNA
setMethod(f = "setNA",
          signature = "PlotSettings",
          definition = function(object, ...) {
            
            overwrite <- list(...)
            
            if(!all(names(overwrite) %in% slotNames(object))) {
              stop("Unknown keys!")
            }
            
            for(key in names(overwrite)) {
              if(key == "width") { if(is.na(object@width)) object@width <- overwrite$width }
              else if(key == "height") { if(is.na(object@height)) object@height <- overwrite$height }
              else if(key == "dpi") { if(is.na(object@dpi)) object@dpi <- overwrite$dpi }
              else if(key == "title") { if(is.na(object@title)) object@title <- overwrite$title }
              else if(key == "subtitle") { if(is.na(object@subtitle)) object@subtitle <- overwrite$subtitle }
              else if(key == "legend.color") { if(is.na(object@legend.color)) object@legend.color <- overwrite$legend.color }
              else if(key == "legend.shape") { if(is.na(object@legend.shape)) object@legend.shape <- overwrite$legend.shape }
            }
            
            validObject(object)
            return(object)
          })