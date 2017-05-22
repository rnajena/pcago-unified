#'
#' Widget that provides standard output settings for plots
#' 

library(shiny)
source("uiHelper.R")
source("classPlotSettings.R")

generalPlotSettingsInput <- function(id, legend.color = T, legend.shape = T) {
  
  ns <- NS(id)
  
  ui.legends <- tagList()
  if(legend.color || legend.shape) {
    ui.legends <- tagAppendChild(ui.legends, hDivider())
  }
  if(legend.color) {
    ui.legends <- tagAppendChild(ui.legends, textInput(ns("legend.color"), "Custom color legend", value = ""))
  }
  if(legend.shape) {
    ui.legends <- tagAppendChild(ui.legends, textInput(ns("legend.shape"), "Custom shape legend", value = ""))
  }
  
  return(tagList(
    textInput(ns("title"), "Custom title", value = ""),
    textInput(ns("subtitle"), "Custom subtitle",  value = ""),
    ui.legends,
    hDivider(),
    tags$label("Width"),
    tags$div(class = "row general-plot-settings-size",
             column(width = 3, bsButton(ns("auto.width"), "Auto", value = T, type = "toggle")),
             column(width = 9, numericInput(ns("width"), label = "", value = 640, min = -1, max = 10000))
    ),
    tags$label("Height"),
    tags$div(class = "row general-plot-settings-size",
             column(width = 3, bsButton(ns("auto.height"), "Auto", value = T, type = "toggle")),
             column(width = 9, numericInput(ns("height"), label = "", value = 480, min = -1, max = 10000))
             
    ),
    sliderInput(ns("scale"), "Scale", min = 0.025, max = 2, value = 1, step = 0.025),
    sliderInput(ns("dpi"), "DPI", min = 50, max = 300, value = 120)
  ))
  
}

generalPlotSettings_ <- function(input, output, session) {
  
  observeEvent(input$auto.width, {
    if(input$auto.width) {
      shinyjs::disable("width")
    }
    else {
      shinyjs::enable("width")
    }
  }) 
  observeEvent(input$auto.height, {
    if(input$auto.height) {
      shinyjs::disable("height")
    }
    else {
      shinyjs::enable("height")
    }
  }) 
  
  return(reactive({
    return(PlotSettings(
      width = if(is.numeric(input$width) && input$width > 0 && !input$auto.width) input$width else NA_integer_,
      height = if(is.numeric(input$height) && input$height > 0 && !input$auto.height) input$height else NA_integer_,
      dpi = if(is.numeric(input$dpi) && input$dpi > 0) input$dpi else NA_integer_,
      scale = if(is.numeric(input$scale) && input$scale > 0) input$scale else NA_real_,
      title = if(is.character(input$title) && nchar(input$title) > 0) input$title else NA_character_,
      subtitle = if(is.character(input$subtitle) && nchar(input$subtitle) > 0) input$subtitle else NA_character_,
      legend.color = if(is.character(input$legend.color) && nchar(input$legend.color) > 0) input$legend.color else NA_character_,
      legend.shape = if(is.character(input$legend.shape) && nchar(input$legend.shape) > 0) input$legend.shape else NA_character_
    ))
    
  }))
  
}

generalPlotSettings <- function(id) {
  return(callModule(generalPlotSettings_, id))
}