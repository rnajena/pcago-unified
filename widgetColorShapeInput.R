#' 
#' Contains a widget that allows the user to select a color or a shape
#' for a condition.
#' The user also can choose to ignore a condition by selecting "None"
#' 

library(shiny)
library(colourpicker)
library(RColorBrewer)
source("uiHelper.R")
source("visuals.R")

colorShapeInput.shapes <- c(
  "No shape" = -1,
  "● Filled circle" = 16,
  "■ Filled square" = 15,
  "▲ Filled triangle" = 17,
  "◆ Filled rhombus" = 18,
  "⬤ Large filled circle" = 19,
  "• Small filled circle" = 20,
  "□ Box" = 0,
  "○ Circle" = 1,
  "△ Triangle" = 2,
  "+ Plus" = 3,
  "⨯ Cross" = 4,
  "◇ Diamond" = 5,
  "▽ Down pointing triangle" = 6,
  "⊠ Box with cross" = 7,
  "✳ Asterisk" = 8,
  "◇ Diamond with cross" = 9,
  "⊕ Circle with plus" = 10,
  "✡ Up- and down pointing triangle" = 11,
  "⊞ Box with plus" = 12,
  "⊗ Circle with cross" = 13,
  "⟎ Box with down pointing triangle" = 14
)

#' Creates a widget that allows editing of visual parameters of a condition visual mapping table.
#'
#' @param id ID of the widget
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
colorShapeEditorInput <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(tags$div(class="color-shape-editor",
                          tags$div(
                            class="condition-list",
                            radioButtons(ns("conditions"),
                                         "Available conditions",
                                         choices = c("None"))),
                          colourInput(ns("color"), "Color", value = "transparent", palette = "square", allowTransparent = T, transparentText = "No color"),
                          selectizeInput(ns("shape"), "Shape", choices = colorShapeInput.shapes, selected = -1))
                          ))
  
}

#' Creates a condition visual mapping based on the settings the user specified in the input
#'
#' @param input 
#' @param output 
#' @param session 
#' @param conditions List of conditions
#'
#' @return
#' @export
#'
#' @examples
colorShapeEditor <- function(input, output, session, conditions) {
  
  variables <- reactiveValues(visuals.table = NULL)
  
  # Get the current visual table or make a new one based on the current conditions
  visual.table <- reactive({ 
    
    validate(need(conditions(), "Cannot output visual parameters withoout conditions!"))
    
    if(is.null(variables$visuals.table) || !identical(rownames(variables$visuals.table), colnames(conditions()))) {
      variables$visuals.table <- generateDefaultConditionVisualsTable(colnames(conditions()))
    }
    
    return(variables$visuals.table)
    
  })

  # Change the list of choices when the conditions update
  observeEvent(conditions(), {
    
    validate(need(conditions(), "Cannot output visual parameters withoout conditions!"))
    
    updateRadioButtons(session, "conditions", choices = colnames(conditions()))
    
  })
  
  # Based on selected choice update the current color and shape settings
  observeEvent(input$conditions, {
    
    validate(need(visual.table(), "No visual table to update input!"))
    
    condition <- input$conditions
    color <- visual.table()[condition, "color"]
    shape <- visual.table()[condition, "shape"]
    
    if(is.null(color) || color == "") {
      color <- "transparent"
    }
    if(!is.numeric(shape) || shape < 0) {
      shape <- -1
    }
    
    updateColourInput(session, "color", value = color)
    updateSelectizeInput(session, "shape", selected = shape)
    
  })
  
  # Change color/shape based on input
  observeEvent(input$color, {
    
    validate(need(visual.table(), "No visual table to write to!"))
    
    condition <- input$conditions
    color <- input$color
    
    if(color == "transparent") {
      color <- ""
    }
  
    variables$visuals.table[condition, "color"] <- color
    
  })
  
  observeEvent(input$shape, {
    
    validate(need(visual.table(), "No visual table to write to!"))
    
    condition <- input$conditions
    shape <- as.numeric(input$shape)
    
    variables$visuals.table[condition, "shape"] <- shape
    
  })
  
  return(visual.table)
  
}