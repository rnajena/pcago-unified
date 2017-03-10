#' 
#' Contains a widget that allows the user to select a color or a shape
#' for a condition.
#' The user also can choose to ignore a condition by selecting "None"
#' 

library(shiny)
library(colourpicker)
library(RColorBrewer)
source("uiHelper.R")

colorShapeEditorInput <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(tags$div(class="color-shape-editor",
                          tags$div(class="condition-list", radioButtons(ns("conditions"), "Available conditions", choices = c("None"))),
                          checkboxGroupInput(ns("used.parameters"),
                                             "Set color/shape of this condition?",
                                             c("Has color" = "color",
                                               "Has shape" = "shape"))
                          )))
  
}

colorShapeEditor <- function(input, output, session, data) {
  
}