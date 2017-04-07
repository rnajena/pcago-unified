#'
#' Widget that provides standard output settings for plots
#' 

library(shiny)
source("uiHelper.R")
source("classPlotSettings.R")

generalPlotSettingsInput <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(
    textInput(ns("title"), "Custom title", value = ""),
    textInput(ns("subtitle"), "Custom subtitle",  value = ""),
    hDivider(),
    textInput(ns("legend.color"), "Custom color legend", value = ""),
    textInput(ns("legend.shape"), "Custom shape legend", value = ""),
    hDivider(),
    numericInput(ns("width"), "Width", value = -1, min = -1, max = 10000),
    numericInput(ns("height"), "Height", value = -1, min = -1, max = 10000),
    sliderInput(ns("dpi"), "DPI", min = 50, max = 300, value = 96)
  ))
  
}

generalPlotSettings_ <- function(input, output, session) {
  
  return(reactive({
    
    return(PlotSettings(
      width = if(!is.null(input$width) && input$width > 0) input$width else NA_integer_,
      height = if(!is.null(input$height) && input$height > 0) input$height else NA_integer_,
      dpi = if(!is.null(input$dpi) && input$dpi > 0) input$dpi else NA_integer_,
      title = if(nchar(input$title) > 0) input$title else NA_character_,
      subtitle = if(nchar(input$subtitle) > 0) input$subtitle else NA_character_,
      legend.color = if(nchar(input$legend.color) > 0) input$legend.color else NA_character_,
      legend.shape = if(nchar(input$legend.shape) > 0) input$legend.shape else NA_character_
    ))
    
  }))
  
}

generalPlotSettings <- function(id) {
  return(callModule(generalPlotSettings_, id))
}