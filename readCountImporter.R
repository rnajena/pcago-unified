#
# Contains methods that will import the read count table
#

# A list of all read count data types that will be supported
# The user selects one of those types, which will then invoke the corresponding importer
supportedReadcountImporters <- c("CSV (Comma)" = "csv_comma",
                                 "CSV (Whitespace/Tab)" = "csv_whitespace")
supportedReadcountFileTypes <- c("text/csv", "text/comma-separated-values,text/plain", ".csv")

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
  
  data <- read.csv(filehandle, sep = sep, stringsAsFactors = F)
  rownames(data) <- data[,1]
  data <- data[,-1]
  
  return(data)
}