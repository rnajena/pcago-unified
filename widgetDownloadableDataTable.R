#'
#' Contains a data table widget with download buttons
#'

library(DT)
library(shiny)

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
      fluidRow(downloadButton(ns("export.csv"), "Export *.csv")))),
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
#' @param rownames Export row names, too. Default = TRUE
#'
#' @return
#' @export
#'
#' @examples
downloadableDataTable <- function(input, output, session, data, filename, rownames = T) {
  
  
  output$table <- DT::renderDataTable(data(), options = list(scrollX = TRUE))
  output$export.csv <- downloadHandler(filename, 
                                       function(file) {
                                         
                                        write.table(data(),
                                                  file,
                                                  sep = ",",
                                                  row.names = rownames)
                                         
                                       })
  
}