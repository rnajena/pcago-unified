#'
#' Contains a plot output with download buttons
#'

library(DT)
library(shiny)
source("uiHelper.R")

#' Creates a UI with a plot output and download buttons
#'
#' @param id ID of the control
#' @param ... Additional items to include into header bar
#'
#' @return
#' @export
#'
#' @examples
downloadablePlotOutput <- function(id, ...) {
  
  if(!is.character(id)) {
    stop("Invalid arguments!")
  }
  
  ns <- NS(id)
  
  return(tags$div(class = "downloadable-plot",
                   headerPanel(header = tags$span(class="headerbar-row",
                                                  downloadButton(ns("export.svg"), "Export *.svg"),
                                                  downloadButton(ns("export.png"), "Export *.png"),
                                                  ...),
                               plotOutput(ns("plot")))))
}


#' Plots a downloadable plot
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param input 
#' @param output 
#' @param session 
#' @param exprplot Function that takes width, height, format and filename and saves a plot to filename
#' @param render.format Format of data sent to user (default: png)
#' @param render.alt Alt text for img object (default: "Plot")
#' @param export.filename Base filename of exported plot
#'
#' @return
#' @export
#'
#' @examples
downloadablePlot_ <- function(input, output, session, exprplot, render.format = "png", render.alt = "Plot", export.filename = "plot") {
  
  out.width <- reactive({ session$clientData[[paste0("output_", session$ns("plot"), "_width")]] })
  out.height <- reactive({ session$clientData[[paste0("output_", session$ns("plot"), "_height")]] })
  
  output$plot <- renderImage({
    
    out.file <- tempfile(fileext=paste0(".", render.format))
    
    exprplot(width = out.width(), height = out.height(), filename = out.file, format = render.format)
    
    return(list(src = out.file,
         width = out.width(),
         height = out.height(),
         alt = render.alt))
  })
  
  output$export.svg <- downloadHandler(filename = paste0(export.filename, ".svg"),
                                       content = function(file) {
                                         exprplot(width = out.width(), height = out.height(), filename = file, format = "svg")
                                       },
                                       contentType = "image/svg")

  output$export.png <- downloadHandler(filename = paste0(export.filename, ".png"),
                                       content = function(file) {
                                         exprplot(width = out.width(), height = out.height(), filename = file, format = "png")
                                       },
                                       contentType = "image/png")
  
}

#' Plots a downloadable plot
#'
#' @param exprplot Function that takes width, height, format and filename and saves a plot to filename
#' @param render.format Format of data sent to user (default: png)
#' @param render.alt Alt text for img object (default: "Plot")
#' @param export.filename Base filename of exported plot
#'
#' @return
#' @export
#'
#' @examples
downloadablePlot <- function(id, exprplot, render.format = "png", render.alt = "Plot", export.filename = "plot") {
  
  return(callModule(downloadablePlot_,
                    id,
                    exprplot = exprplot,
                    render.format = render.format,
                    render.alt = render.alt,
                    export.filename = export.filename))
  
}