
library(shiny)
library(colourpicker)
library(RColorBrewer)

colorShapeInput <- function(id) {
  
  ns <- NS(id)
  
  choices <- list(
    "Misc" = c("None" = "*"),
    "Colors" = brewer.pal(9, "Set1")
  )
  
  render <- I("{
    option: function(item, escape) {
      var style = item.label.startsWith('#') ? 'style=\"background-color: ' + escape(item.label) +'\"' : ''
      return '<div ' + style + ' >' + escape(item.label) + '</div>'
    },
    item: function(item, escape) {
      var style = item.label.startsWith('#') ? 'style=\"background-color: ' + escape(item.label) +'\"' : ''
      return '<div ' + style + ' >' + escape(item.label) + '</div>'
    }
              }")
  
  return(selectizeInput(ns("input"), 
                        label = id,
                        choices = choices,
                        options = list("render" = render)))
  
}