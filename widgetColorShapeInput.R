#' 
#' Contains a widget that allows the user to select a color or a shape
#' for a condition.
#' The user also can choose to ignore a condition by selecting "None"
#' 

library(shiny)
library(colourpicker)
library(RColorBrewer)

colorShapeInput.symbols <-  c("circle", 
                              "circle-open", 
                              "circle-dot", 
                              "circle-open-dot", 
                              "square", 
                              "square-open", 
                              "square-dot", 
                              "square-open-dot", 
                              "diamond", 
                              "diamond-open", 
                              "diamond-dot", 
                              "diamond-open-dot", 
                              "cross", 
                              "cross-open", 
                              "cross-dot", 
                              "cross-open-dot", 
                              "x", 
                              "x-open", 
                              "x-dot", 
                              "x-open-dot", 
                              "triangle-up", 
                              "triangle-up-open", 
                              "triangle-up-dot", 
                              "triangle-up-open-dot", 
                              "triangle-down", 
                              "triangle-down-open", 
                              "triangle-down-dot", 
                              "triangle-down-open-dot", 
                              "triangle-left", 
                              "triangle-left-open", 
                              "triangle-left-dot", 
                              "triangle-left-open-dot", 
                              "triangle-right", 
                              "triangle-right-open", 
                              "triangle-right-dot", 
                              "triangle-right-open-dot", 
                              "triangle-ne", 
                              "triangle-ne-open", 
                              "triangle-ne-dot", 
                              "triangle-ne-open-dot", 
                              "triangle-se", 
                              "triangle-se-open", 
                              "triangle-se-dot", 
                              "triangle-se-open-dot", 
                              "triangle-sw", 
                              "triangle-sw-open", 
                              "triangle-sw-dot", 
                              "triangle-sw-open-dot", 
                              "triangle-nw", 
                              "triangle-nw-open", 
                              "triangle-nw-dot", 
                              "triangle-nw-open-dot", 
                              "pentagon", 
                              "pentagon-open", 
                              "pentagon-dot", 
                              "pentagon-open-dot", 
                              "hexagon", 
                              "hexagon-open", 
                              "hexagon-dot", 
                              "hexagon-open-dot", 
                              "hexagon2", 
                              "hexagon2-open", 
                              "hexagon2-dot", 
                              "hexagon2-open-dot", 
                              "octagon", 
                              "octagon-open", 
                              "octagon-dot", 
                              "octagon-open-dot", 
                              "star", 
                              "star-open", 
                              "star-dot", 
                              "star-open-dot", 
                              "hexagram", 
                              "hexagram-open", 
                              "hexagram-dot", 
                              "hexagram-open-dot", 
                              "star-triangle-up", 
                              "star-triangle-up-open", 
                              "star-triangle-up-dot", 
                              "star-triangle-up-open-dot", 
                              "star-triangle-down", 
                              "star-triangle-down-open", 
                              "star-triangle-down-dot", 
                              "star-triangle-down-open-dot", 
                              "star-square", 
                              "star-square-open", 
                              "star-square-dot", 
                              "star-square-open-dot", 
                              "star-diamond", 
                              "star-diamond-open", 
                              "star-diamond-dot", 
                              "star-diamond-open-dot", 
                              "diamond-tall", 
                              "diamond-tall-open", 
                              "diamond-tall-dot", 
                              "diamond-tall-open-dot", 
                              "diamond-wide", 
                              "diamond-wide-open", 
                              "diamond-wide-dot", 
                              "diamond-wide-open-dot", 
                              "hourglass", 
                              "hourglass-open", 
                              "bowtie", 
                              "bowtie-open", 
                              "circle-cross", 
                              "circle-cross-open", 
                              "circle-x", 
                              "circle-x-open", 
                              "square-cross", 
                              "square-cross-open", 
                              "square-x", 
                              "square-x-open", 
                              "diamond-cross", 
                              "diamond-cross-open", 
                              "diamond-x", 
                              "diamond-x-open", 
                              "cross-thin", 
                              "cross-thin-open", 
                              "x-thin", 
                              "x-thin-open", 
                              "asterisk", 
                              "asterisk-open", 
                              "hash", 
                              "hash-open", 
                              "hash-dot", 
                              "hash-open-dot", 
                              "y-up", 
                              "y-up-open", 
                              "y-down", 
                              "y-down-open", 
                              "y-left", 
                              "y-left-open", 
                              "y-right", 
                              "y-right-open", 
                              "line-ew", 
                              "line-ew-open", 
                              "line-ns", 
                              "line-ns-open", 
                              "line-ne", 
                              "line-ne-open", 
                              "line-nw", 
                              "line-nw-open")

colorShapeInput <- function(id, label) {
  
  ns <- NS(id)
  
  return(fixedRow(
    column(width = 8, selectizeInput(ns("symbol"), label = label, choices = colorShapeInput.symbols)),
    column(width = 4, tags$div(class = "color-shape-input-color", colourInput(ns("color"), label = label, value = "#000000")))
  ))
  

}

# return(selectizeInput(ns("input"), 
#                       label = id,
#                       choices = colorShapeInput.choices,
#                       options = list("render" = colorShapeInput.on_render,
#                                      "create" = colorShapeInput.on_create)))
# return(selectizeInput(ns("input"), 
#                       label = id,
#                       choices = colorShapeInput.choices,
#                       options = list("render" = colorShapeInput.on_render,
#                                      "create" = colorShapeInput.on_create)))
# return(selectizeInput(ns("input"), 
#                       label = id,
#                       choices = colorShapeInput.choices,
#                       options = list("render" = colorShapeInput.on_render,
#                                      "create" = colorShapeInput.on_create)))
#'   "Colors" = brewer.pal(9, "Set1")
#' )
#' 
#' #' The function responsible for rendering an item
#' #' Colors items by the selected color
#' colorShapeInput.on_render <- I("{
#'     option: function(item, escape) {
#'       var style = item.value.startsWith('#') ? 'style=\"background-color: ' + escape(item.value) +'\"' : ''
#'       return '<div ' + style + ' >' + escape(item.label) + '</div>'
#'     },
#'     item: function(item, escape) {
#'       var style = item.value.startsWith('#') ? 'style=\"background-color: ' + escape(item.value) +'\"' : ''
#'       return '<div ' + style + ' >' + escape(item.label) + '</div>'
#'     }
#'   }")
#' 
#' #' The function that is called if the user creates an item
#' #' Checks if it's a valid color
#' colorShapeInput.on_create <- I("
#'     function(input, callback) {
#'       
#'       var isOk  =  (input.length == 6 + 1) && /^#[0-9A-F]+$/i.test(input)
#' 
#'       console.log(isOk)
#' 
#'       if(isOk) {
#'         callback({ label : input, value : input })
#'       }
#'       else {
#'         callback()
#'       }
#' 
#'     }")