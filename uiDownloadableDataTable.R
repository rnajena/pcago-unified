#
# Contains a data table widget with download buttons
#

library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)

downloadableDataTableOutput <- function(id) {
  
  ns <- NS(id)
  
  tagList(verticalLayout(
    wellPanel(fluidPage(
      fluidRow(downloadButton(ns("download"), "Export *.csv")))),
    DT::dataTableOutput(ns("table"))
  ))
}

downloadableDataTable <- function(input, output, session, data, filename) {
  
  
  output$table <- DT::renderDataTable(data(), options = list(scrollX = TRUE))
  
}