#'
#' Slider that allows changing animation parameters
#' 

library(shiny)
library(shinyBS)
source("uiHelper.R")
source("widgetNumericRangeInput.R")

#' Creates a slider input that allows chaning of animation parameters and range of the slider
#'
#' @param id 
#' @param header 
#'
#' @return
#' @export
#'
#' @examples
extendedSliderInput <- function(id, header = "") {
  
  if(!is.character(id)) {
    stop("Invalid arguments!")
  }
  
  ns <- NS(id)
  
  return(tagList(sliderInput(ns("count"), header, min = 0, max = 0, value = 0, step = 1, round = T),
                 fluidRow(style = css("text-align" = "center"), tags$div(class = "col-centered", style = css(display = "inline-block"),
                                                                  actionButton(ns("large.step.decrease"), label = "", icon = icon("fast-backward")),
                                                                  actionButton(ns("small.step.decrease"), label = "", icon = icon("step-backward")),
                                                                  bsButton(ns("play"), label = tags$span(tags$span(class = "play", icon("play")), tags$span(class = "pause", icon("pause"))), type = "toggle", style = "play-pause"),
                                                                  actionButton(ns("small.step.increase"), label = "", icon = icon("step-forward")),
                                                                  actionButton(ns("large.step.increase"), label = "", icon = icon("fast-forward")),
                                                                  numericInput(ns("exact.value"), label = "", value = 0)
                 )),
                 hDivider(),
                 bsCollapse(
                   bsCollapsePanel("Animation parameters",
                                   numericRangeInput(ns("anim.range"), "From", "To"),
                                   numericInput(ns("anim.by"), "Animation step", 1, 1000, value = 10),
                                   numericInput(ns("anim.delay"), helpIconText("Animation speed (ms)", 
                                                                               includeText("helptooltips/extended-slider-anim-speed.md")), 100, 1000, value = 100))
                 )))
  
}


#' Returns the value and animation steps from an extended slider input.
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param input 
#' @param output 
#' @param session 
#' @param value.min Reactive returning the min. value
#' @param value.max Reactive returning the max. value
#' @param value.default Optional reactive returning the default value (NULL for min. value)
#' @param value.default.min Optional reactive returning the default min value of the slider range (NULL for value.min)
#' @param value.default.max Optional reactive returning the default max value of the slider range (NULL for value.max)
#'
#' @return List with value (value), slider range start (from), slider range stop (to), animation step (by), animation delay (delay)
#' @export
#'
#' @examples
extendedSliderInputValue_ <- function(input, output, session, value.min, value.max, value.default = NULL, value.default.min = NULL, value.default.max = NULL) {
  
  # Wrap those broken numeric inputs https://github.com/rstudio/shiny/issues/927
  anim.range <- numericRangeInputValue("anim.range", value.min, value.max, value.default.min, value.default.max)
  anim.by <- reactive({ min(1000, max(1, input$anim.by)) })
  anim.delay <- reactive({ min(1000, max(100, input$anim.delay)) })
  
  # Update slider based on range
  observeEvent(anim.range(), {
    
    default <- anim.range()[["from"]]
    
    if(!is.null(value.default)) {
      default <- value.default()
    }
    
    updateSliderInput(session, "count", min = anim.range()[["from"]], max = anim.range()[["to"]], value = default)
  })
  
  # Step increase/decrease buttons
  observeEvent(input$large.step.decrease, {
    current <- input$count
    updateSliderInput(session, "count", value = current - anim.by())
  })
  
  observeEvent(input$large.step.increase, {
    current <- input$count
    updateSliderInput(session, "count", value = current + anim.by())
  })
  
  observeEvent(input$small.step.decrease, {
    current <- input$count
    updateSliderInput(session, "count", value = current - 1)
  })
  
  observeEvent(input$small.step.increase, {
    current <- input$count
    updateSliderInput(session, "count", value = current + 1)
  })
  
  # Exact value 
  observeEvent(input$exact.value, {
    #if(!input$play) { # Prevent react loops.
      current <- input$count
      requested <- min(value.max(), max(value.min(), input$exact.value))
      
      if(requested != current) {
        updateSliderInput(session, "count", value = requested)
      }
    #}
  })
  observeEvent(input$count, {
    current <- input$count
    current.exact <- input$exact.value
    
    if(current != current.exact) {
      updateNumericInput(session, "exact.value", value = current)
    }
  })
  
  #' Handles the animation of the slider
  #' This works by invalidating itself automatically if the play button is toggled
  #' 
  #' Info: There's a native animation feature in the slider. But it does not allow
  #' changing the animation parameters without renderUI; which is slow and fragile due to missing inputs
  observe({
    
    if(input$play) {
      # Separate the actual animation from the environment
      isolate({
        
        from <- anim.range()[["from"]]
        to <- anim.range()[["to"]]
        by <- anim.by()
        current <- input$count
        
        if(current == to) {
          current <- from
        }
        else if(current < to) {
          current <- min(to, current + by)
        }
        else {
          current <- from
        }
        
        #updateSliderInput(session, "count", value = current)
        updateNumericInput(session, "exact.value", value = current)
        
      })
      
      invalidateLater(isolate({anim.delay()}))
    }
  })
  
  return(reactive( { 
    return(list("value" = input$count, 
                "from" = anim.range()[["from"]], 
                "to" = anim.range()[["to"]], 
                "by" = anim.by(),
                "delay" = anim.delay()))
    }))
  
}

#' Updates the slider input values
#'
#' @param value.min 
#' @param value.max 
#' @param value 
#'
#' @return
#' @export
#'
#' @examples
updateExtendedSliderInput_ <- function(input, output, session, value.min = NULL, value.max = NULL, value = NULL) {
  updateSliderInput(session, "count", min = value.min, max = value.max, value = value)
}

#' Returns the value and animation steps from an extended slider input.
#'
#' @param value.min Reactive returning the min. value
#' @param value.max Reactive returning the max. value
#' @param value.default Optional reactive returning the default value (NULL for min. value)
#' @param value.default.min Optional reactive returning the default min value of the slider range (NULL for value.min)
#' @param value.default.max Optional reactive returning the default max value of the slider range (NULL for value.max)
#'
#' @return List with value (value), slider range start (from), slider range stop (to), animation step (by), animation delay (delay)
#' @export
#'
#' @examples
extendedSliderInputValue <- function(id, value.min, value.max, value.default = NULL, value.default.min = NULL, value.default.max = NULL) {
  return(callModule(extendedSliderInputValue_, 
                    id,
                    value.min = value.min,
                    value.max = value.max,
                    value.default = value.default,
                    value.default.min = value.default.min,
                    value.default.max = value.default.max))
}


#' Updates the slider input values
#'
#' @param id 
#' @param value.min 
#' @param value.max 
#' @param value 
#'
#' @return
#' @export
#'
#' @examples
updateExtendedSliderInput <- function(id, value.min = NULL, value.max = NULL, value = NULL) {
  return(callModule(updateExtendedSliderInput_, 
                    id,
                    value.min = value.min,
                    value.max = value.max,
                    value = value))
}