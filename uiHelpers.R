#
# Contains some UI helpers
#

library(DT)
library(shiny)

#' Adds a data table output widget and a download button.
#' The download button has the outputid <data>.download
#'
#' @param data Server output data id for dataTableOutput
#'
#' @return Shiny UI elements
#' @export
#'
#' @examples
downloadableDataTableOutput <- function(data) {
  
  return(verticalLayout(
    wellPanel(fluidPage(
      fluidRow(downloadButton(paste0(data, ".download"), "Download *.csv")))),
    DT::dataTableOutput(data)
  ))
  
}