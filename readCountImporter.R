#
# Contains methods that will import the read count table
#

library(shiny)
source("uiImporterWidget.R")

# A list of all read count data types that will be supported
# The user selects one of those types, which will then invoke the corresponding importer
supportedReadcountDataTypes <- c("CSV (Comma)" = "csv_comma",
                                 "CSV (Whitespace/Tab)" = "csv_whitespace")

#' Imports readcount from filehandle with importer definded by datatype
#'
#' @param filehandle Either a filename or a connection
#' @param datatype One value in supportedReadcountDataTypes
#'
#' @return Data frame containing the read data
#' @export
#'
#' @examples
importReadcount <- function(filehandle, datatype) {
  
  
  print("Importing")
  
  sep = ","
  
  if(datatype == "csv_whitespace") {
    sep = ""
  }
  
  data <- read.csv(filehandle, sep = sep)
  names(data)[1] <- "id" # First column is always 'id'
  
  return(data)
}