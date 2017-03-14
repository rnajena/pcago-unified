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
    numericInput(ns("dpi"), "DPI", min = 10, max = 1200, value = 96)
  ))
  
}

generalPlotSettings <- function(input, output, session) {
  
}