#'
#' Widget that provides standard output settings for plots
#' 

library(shiny)

generalPlotSettingsInput <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(
    textInput(ns("title"), "Custom title", value = ""),
    textInput(ns("subtitle"), "Custom subtitle",  value = ""),
    numericInput(ns("width"), "Width", value = -1, min = -1, max = 10000),
    numericInput(ns("height"), "Height", value = -1, min = -1, max = 10000),
    sliderInput(ns("dpi"), "DPI", min = 50, max = 300, value = 96)
  ))
  
}

generalPlotSettings_ <- function(input, output, session) {
  
  return(reactive({
    
    return(list(
      width = input$width,
      height = input$height,
      dpi = input$dpi,
      title = input$title,
      subtitle = input$subtitle
    ))
    
  }))
  
}

generalPlotSettings <- function(id) {
  return(callModule(generalPlotSettings_, id))
}