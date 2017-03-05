#
# Contains an importer widget and necessary functions
#

library(shiny)
library(shinyBS)
library(shinyjs)

#' Creates an importer widget that supports pasting content manually or uploading a file.
#' The user can select an importer algorithm that will be used to load the data.
#' Use importerWidgetData() to get the input from this widget
#'
#' @param namespace The prefix of all subwidgets
#' @param filetypes 
#' @param importers 
#'
#' @return Shiny widget
#' @export
#'
#' @examples
importerWidget <- function(namespace, filetypes, importers) {
  
  return(verticalLayout(
    textAreaInput(paste0(namespace ,".input"), "Manual input"),
    fileInput(paste0(namespace, ".fileinput"), "From File", accept = filetypes),
    selectInput(paste0(namespace, ".importer"), "Importer", supportedReadcountDataTypes),
    fluidPage(fluidRow(
      actionButton(paste0(namespace, ".submit"), "Submit"),
      actionButton(paste0(namespace, ".reset"), "Reset"))                                   
    )))
  
}

#' Extracts the data from an importer widget
#'
#' @param namespace Namespace used in importerWidget()
#' @param input Shiny input
#'
#' @return a connection to the data ($connection) and the importer ($importer)
#' @export
#'
#' @examples
importerWidgetData <- function(namespace, input) {
  
  importer <- input[[paste0(namespace, ".importer")]]
  inFile <- input[[paste0(namespace, ".fileinput")]]
  
  if(is.null(inFile))
  {
    con <- textConnection(input$pca.data.readcounts.input)
    return(list("connection" = con, "importer" = importer))
  }
  else
  {
    con <- file(inFile$datapath, "r")
    return(list("connection" = con, "importer" = importer))
  }
}