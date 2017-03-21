#'
#' Contains a data table widget with download buttons
#'

library(DT)
library(shiny)
source("uiHelper.R")

#' Creates a UI with a data table output and download buttons
#'
#' @param id ID of the control
#' @param ... Additional items to include into header bar
#'
#' @return
#' @export
#'
#' @examples
downloadableDataTableOutput <- function(id, ...) {
  
  if(!is.character(id)) {
    stop("Invalid arguments!")
  }
  
  ns <- NS(id)
  
  return(headerPanel(header = tags$span(class="headerbar-row",
                                        downloadButton(ns("export.csv"), "Export *.csv"),
                                        downloadButton(ns("export.tsv"), "Export *.tsv"),
                                        ...),
                     DT::dataTableOutput(ns("table"))))
}

#' Fills the data table with given data output.
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param input 
#' @param output 
#' @param session 
#' @param data Data to be displayed. Should be a reactive
#' @param export.filename Filename of the downloaded file
#' @param export.rownames Export row names, too. Default = TRUE
#' @param export.colnames Parameter of write.table.
#'
#' @return
#' @export
#'
#' @examples
downloadableDataTable_ <- function(input, output, session, data, export.filename, export.rownames = T, export.colnames = NA) {
  
  table.data <- reactive({
    
    validate(need(data(), "No data to display!"),
             need(is.data.frame(data()) || is.matrix(data()), "Data is not a data frame!"))
    return(data())
    
  })
  
  
  
  output$table <- DT::renderDataTable(table.data(), options = list(scrollX = TRUE))
  output$export.csv <- downloadHandler(paste0(export.filename, ".csv"), 
                                       function(file) {
                                         
                                        write.table(table.data(),
                                                  file,
                                                  sep = ",",
                                                  row.names = export.rownames,
                                                  col.names = export.colnames)
                                         
                                       })
  output$export.tsv <- downloadHandler(paste0(export.filename, ".csv"), 
                                       function(file) {
                                         
                                         write.table(table.data(),
                                                     file,
                                                     sep = "\t",
                                                     row.names = export.rownames,
                                                     col.names = export.colnames)
                                         
                                       })
  
}

#' Fills the data table with given data output.
#' 
#' @param data Data to be displayed. Should be a reactive
#' @param export.filename Filename of the downloaded file
#' @param export.rownames Export row names, too. Default = TRUE
#' @param export.colnames Parameter of write.table.
#'
#' @return
#' @export
#'
#' @examples
downloadableDataTable <- function(id, data, export.filename = "table", export.rownames = T, export.colnames = NA) {
  
  return(callModule(downloadableDataTable_,
                    id, 
                    data = data, 
                    export.filename = export.filename,
                    export.rownames = export.rownames,
                    export.colnames = export.colnames))
  
}