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
  
  return(tags$div(class = "optional-color-input",
    fixedRow(
      column(width = 8, colourInput(ns('color'), label = label, allowTransparent = T)),
      column(width = 4, bsButton(ns('nocolor'), label = "No color"))
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
  
  observeEvent(input$color,
    {
      if(col2rgb(input$color, alpha = T)["alpha", ] == 0) {
          result.data$color = ""
      }
      else {
        result.data$color = input$color
      }
    })
  
  observeEvent(input$nocolor, {
    updateColourInput(session, "color", value = "#FFFFFF00")
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
    updateColourInput(session, "color", value = "#FFFFFF00")
  }
  else {
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
