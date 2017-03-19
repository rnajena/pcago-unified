#'
#' Range numeric input. Because the slider is too unprecise
#' 

library(shiny)

numericRangeInput <- function(id, label.from, label.to) {
  
  ns <- NS(id)
  
  return(tagList(
    fixedRow(
      column(width = 6, numericInput(ns("from"), label.from, value = 1)),
      column(width = 6, numericInput(ns("to"), label.to, value = 1))
    ),
    textOutput(ns("message"))
  ))
}

numericRangeInputValue_ <- function(input, output, session, value.min, value.max, value.default.min = NULL, value.default.max = NULL) {
  
  observe({

    validate(need(value.min(), "No minimum defined!"),
             need(value.max(), "No maximum defined!"))
    
    default.min <- value.min()
    default.max <- value.max()
    
    
    if(!is.null(value.default.min)) {
      default.min <- value.default.min()
    }
    if(!is.null(value.default.max)) {
      default.max <- value.default.max()
    }
    
    updateNumericInput(session, "from",value = default.min, min = value.min(), max = value.max())
    updateNumericInput(session, "to", value = default.max, min = value.min(), max = value.max())
  })
  
  from <- reactive({ 
      return(min(value.max(), max(value.min(), input$from)))
    })
  to <- reactive({ 
      return(min(value.max(), max(value.min(), input$to)))
    })
  
  range <- reactive( {
    
    if(from() < to()) {
      return(c("from" = from(), "to" = to()))
    }
    else {
      return(c("from" = value.min(), "to" = value.max()))
    }
    
  })
  
  output$message <- renderText({
    if(from() < to()) {
      return("")
    }
    else {
      return("Invalid range!")
    }
  })
  
  return(range)
  
}

numericRangeInputValue <- function(id, value.min, value.max, value.default.min = NULL, value.default.max = NULL) {
  return(callModule(numericRangeInputValue_, 
                    id, 
                    value.min = value.min, 
                    value.max = value.max,
                    value.default.min = value.default.min,
                    value.default.max = value.default.max))
}