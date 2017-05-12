#' 
#' A widget that displays optional processing steps
#' 

library(shiny)

#' Creates a widget that displays a linear process in a tabbed interface
#'
#' @param id 
#' @param title 
#'
#' @return
#' @export
#'
#' @examples
processingStepsWidgetUI <- function(id, title) {
  
  ns <- NS(id)
  
  export.buttons <- tagList(downloadButton(ns("export.html"), label = "as *.html"))
  export.dropdown <- dropdownButton(ns("export"), "Export report", export.buttons, icon = icon("download"))
  showdetails <- bsButton(ns("showdetails"), "Show details", type = "toggle", icon = icon("list-ul"))
  
  return(tags$div(class = "processing-steps-widget", headerPanel(header = tagList(export.dropdown,
                                                                                  showdetails),
                                                                 uiOutput(ns("steps.ui")))))
  
  # titlewidget <- tags$div(class = "header", title, export)
  # 
  # return(tags$div(class = "processing-steps-widget", bsCollapsePanel(title = title,
  #                                                                    uiOutput(ns("steps.ui")),
  #                                                                    hDivider(),
  #                                                                    export)))
  
}

#' Displays the output of the reactives in ... in the UI
#' #' 
#' Processing step info functions must return NULL or a list with title and content entries.
#' 
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param id 
#' @param ... Reactives that return the processing information as list. Return NULL to skip the processing step.
#'
#' @return
#' @export
#'
#' @examples
processingStepsWidgetData_ <- function(input, output, session, ...) {
  
  output$steps.ui <- renderUI({
    
    steps <- list()
    steps.titles <- c()
    
    for(processing.output in list(...)) {
      
      output <- processing.output()
      
      if(!is.null(output)) {
        
        title <- paste0(length(steps) + 1, ". ", output$title)
        content <- if(input$showdetails) wellPanel(output$content) else tags$div()
        
        steps[[length(steps) + 1]] <- tabPanel(title, content, icon = icon("chevron-right"))
        steps.titles[length(steps.titles) + 1] <- title
      }
      
    }
    
    parameters <- steps
    parameters$type <- "pills"
    parameters$selected <- if(length(steps.titles) > 0) steps.titles[length(steps.titles)] else NULL
    return(do.call(tabsetPanel, parameters))
    
  })
  
  output$export.html <- downloadHandler("report.html", function(filename) {
    
    steps <- tagList()
    
    for(processing.output in list(...)) {
      
      output <- processing.output()
      
      if(!is.null(output)) {
        
        title <- paste0(length(steps) + 1, ". ", output$title)
        steps <- tagAppendChild(steps, h1(title))
        steps <- tagAppendChild(steps, output$content)
      }
      
    }
    
    conn <- file(filename)
    on.exit({
      close(conn)
    })
    
    writeLines(paste(steps), conn)
    
  })
  
}

#' Displays the output of the reactives in ... in the UI
#' 
#' Processing step info functions must return NULL or a list with title and content entries.
#'
#' @param id 
#' @param ... Reactives that return the processing information as list. Return NULL to skip the processing step.
#'
#' @return
#' @export
#'
#' @examples
processingStepsWidgetData <- function(id, ...) {
  
  return(callModule(processingStepsWidgetData_, id, ...))
  
}