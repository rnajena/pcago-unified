#
# Contains a data table widget with download buttons
#

library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)

#' Creates a UI with a data table output and download buttons
#'
#' @param id ID of the control
#'
#' @return
#' @export
#'
#' @examples
downloadableDataTableOutput <- function(id) {
  
  ns <- NS(id)
  
  tagList(verticalLayout(
    wellPanel(fluidPage(
      fluidRow(downloadButton(ns("download"), "Export *.csv")))),
    DT::dataTableOutput(ns("table"))
  ))
}

#' Fills the data table with given data output. Use within observeEvent(<data>)
#'
#' @param input 
#' @param output 
#' @param session 
#' @param data Data to be displayed. Should be a reactive
#' @param filename Filename of the downloaded file
#'
#' @return
#' @export
#'
#' @examples
downloadableDataTable <- function(input, output, session, data, filename) {
  
  
  output$table <- DT::renderDataTable(data(), options = list(scrollX = TRUE))
  
}