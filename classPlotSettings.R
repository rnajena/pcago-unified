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
  slots = signature(
    width = "numeric",
    height = "numeric",
    dpi = "numeric",
    scale = "numeric",
    title = "character",
    subtitle = "character",
    legend.color = "character",
    legend.shape = "character"
  ),
  prototype = list(
    width = NA_integer_,
    height = NA_integer_,
    scale = NA_real_,
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
    if(!is.na(object@scale) && object@scale <= 0) {
      return("Invalid scale!")
    }
    
    return(T)
  }
)

#' Overwrites NA plot settings with values provided in ellipsis
#'
#' @param object.target A PlotSettings object
#' @param object.source A PlotSettings object
#'
#' @return
#' @rdname plotSettingsSetNA
#' @export
#'
#' @examples
setGeneric(name = "plotSettingsSetNA",
           def = function(object.target, object.source) {
             standardGeneric("plotSettingsSetNA")
           })

#' @rdname plotSettingsSetNA
setMethod(f = "plotSettingsSetNA",
          signature = signature(object.target = "PlotSettings", object.source = "PlotSettings"),
          definition = function(object.target, object.source) {
            
            if(is.numeric(object.source@width) && is.na(object.target@width)) { object.target@width <- object.source@width }
            if(is.numeric(object.source@height) && is.na(object.target@height)) { object.target@height <- object.source@height }
            if(is.numeric(object.source@dpi) && is.na(object.target@dpi)) { object.target@dpi <- object.source@dpi }
            if(is.numeric(object.source@scale) && is.na(object.target@scale)) { object.target@scale <- object.source@scale }
            if(is.character(object.source@title) && is.na(object.target@title)) { object.target@title <- object.source@title }
            if(is.character(object.source@subtitle) && is.na(object.target@subtitle)) { object.target@subtitle <- object.source@subtitle }
            if(is.character(object.source@legend.color) && is.na(object.target@legend.color)) { object.target@legend.color <- object.source@legend.color }
            if(is.character(object.source@legend.shape) && is.na(object.target@legend.shape)) { object.target@legend.shape <- object.source@legend.shape }
            
            validObject(object.target)
            return(object.target)
          })

#' Overwrites values in target from non-NA values in source
#'
#' @param object.target A PlotSettings object
#' @param object.source A PlotSettings object
#'
#' @return
#' @rdname plotSettingsOverwrite
#' @export
#'
#' @examples
setGeneric(name = "plotSettingsOverwrite",
           def = function(object.target, object.source) {
             standardGeneric("plotSettingsOverwrite")
           })

#' @rdname plotSettingsOverwrite
setMethod(f = "plotSettingsOverwrite",
          signature = signature(object.target = "PlotSettings", object.source = "PlotSettings"),
          definition = function(object.target, object.source) {
            
            if(!is.na(object.source@width)) { object.target@width <- object.source@width }
            if(!is.na(object.source@height)) { object.target@height <- object.source@height }
            if(!is.na(object.source@dpi)) { object.target@dpi <- object.source@dpi }
            if(!is.na(object.source@scale)) { object.target@scale <- object.source@scale }
            if(!is.na(object.source@title)) { object.target@title <- object.source@title }
            if(!is.na(object.source@subtitle)) { object.target@subtitle <- object.source@subtitle }
            if(!is.na(object.source@legend.color)) { object.target@legend.color <- object.source@legend.color }
            if(!is.na(object.source@legend.shape)) { object.target@legend.shape <- object.source@legend.shape }
            
            validObject(object.target)
            return(object.target)
          })