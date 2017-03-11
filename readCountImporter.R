#'
#' Contains methods that will import the read count table
#' 

#' A list of all read count data types that will be supported
#' The user selects one of those types, which will then invoke the corresponding importer
supportedReadcountImporters <- c("CSV (Comma)" = "csv_comma",
                                 "CSV (Whitespace/Tab)" = "csv_whitespace")
supportedReadcountFileTypes <- c("text/csv", "text/comma-separated-values,text/plain", ".csv")

availableReadcountSamples <- c("Vitamins (small)" = "vitamins.small.csv",
                               "Vitamins" = "vitamins.csv")

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
  
  sep = ","
  
  if(datatype == "csv_whitespace") {
    sep = ""
  }
  
  data <- try(read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F))
  
  return(data)
}

#' Imports sample with given sample id
#'
#' @param sample 
#'
#' @return Data frame containing the read data
#' @export
#'
#' @examples
importReadcountSample <- function(sample) {
  
  if(sample == "vitamins.small.csv") {
    
    con <- file("sampledata/vitamins.small.csv", "r")
    data <- importReadcount(con, "csv_comma")
    close(con)
    return(data)
    
  }
  else if(sample == "vitamins.csv") {
    
    con <- file("sampledata/vitamins.csv", "r")
    data <- importReadcount(con, "csv_comma")
    close(con)
    return(data)
    
  }
  else {
    return(NULL)
  }
  
}