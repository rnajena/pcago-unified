#'
#' A color input that can return null
#' 

library(shiny)
library(colourpicker)

#' Creates a control that allows the user to select a range of numbers with precision of numericInput
#'
#' @param id 
#' @param label.from 
#' @param label.to 
#'
#' @return
#' @export
#'
#' @examples
optionalColorInput <- function(id, label = NULL) {
  
  if(!is.character(id)) {
    stop("Invalid arguments!")
  }
  
  ns <- NS(id)
  
  return(tagList(
    fixedRow(
      column(width = 6, colourInput(ns('color'), label = label, allowTransparent = F)),
      column(width = 6, checkboxGroupInput(ns('hascolor'), label = " ", choices = c("No color" = "no_color")))
    )
  ))
}

#' Returns value of optional color input
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return A color or an empty string
#' @export
#'
#' @examples
optionalColorInputValue_ <- function(input, output, session) {
  
  result.data <- reactiveValues(color = "")
  
  observeEvent({ input$hascolor
    input$color },
    {
      if("no_color" %in% input$hascolor) {
          result.data$color = ""
      }
      else {
        result.data$color = input$color
      }
    })
  
  return(reactive({result.data$color}))
  
}

#' Returns value of optional color input
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'#'
#' @return A color or an empty string
#' @export
#'
#' @examples
optionalColorInputValue <- function(id) {
  return(callModule(optionalColorInputValue_, 
                    id))
}

#' Updates an optional color input
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return A color or an empty string
#' @export
#'
#' @examples
updateOptionalColorInput_ <- function(input, output, session, value) {
  
  if(is.null(value) || value == "") {
    updateCheckboxGroupInput(session, "hascolor", selected = c("no_color"))
  }
  else {
    updateCheckboxGroupInput(session, "hascolor", selected = c())
    updateColourInput(session, "color", value = value)
  }
  
}

#' Returns value of optional color input
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'#'
#' @return A color or an empty string
#' @export
#'
#' @examples
updateOptionalColorInput <- function(id, value) {
  return(callModule(updateOptionalColorInput_, 
                    id, 
                    value = value))
}
