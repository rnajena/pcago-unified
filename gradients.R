#' 
#' Functions for handling gradients
#' 

library(shiny)
source("helpers.R")

# Importers for condition visuals
supportedGradientImporters <- list(
  ImporterEntry(name = "csv", label = "PCAGO gradients CSV (*.csv)", parameters = list(ImporterParameter.csv))
)
supportedGradientGenerators <- list()
availableGradientSamples <- list(
  ImporterEntry(name = "Gradients/HeatmapRdBu.csv", label = "Heatmap RdBu"),
  ImporterEntry(name = "Gradients/LoadingPlotDefault.csv", label = "PCA Loading plot default"))

importGradient.CSV <- function(filehandle, parameters) {
  
  data <- read.csv(filehandle, sep = parameters$separator, row.names = NULL, stringsAsFactors = F, check.names = F)
  return(data)
}

#' Imports visual definitions from filehandle with importer defined by datatype
#'
#' @param filehandle Either a filename or a connection
#' @param datatype One value in supportedReadcountDataTypes
#' @param conditions Vector of condition names
#'
#' @return Data frame containing the visual parameters for each condition
#' @export
#'
#' @examples
importGradient <- function(filehandle, importer, parameters) {
  
  expected.columns <- c("value", "color")
  
  data <- NULL
  
  if(importer == "csv") {
    data <- importGradient.CSV(filehandle, parameters)
  }
  else {
    stop(paste("Unknown importer", importer))
  }
  
  if(length(expected.columns) == 0) {
    stop("No data stored!")
  }
  
  # Handle errors
  data$value <- as.numeric(data$value)
  data$color <- as.character(data$color)
  data <- data[order(data[,"value"], decreasing = F),]
  
  if(!all(isColor(data$color[data$color != ""]))) {
    stop("Imported gradient has invalid colors!")
  }
  if(!isStrictlySorted(data[, "value"])) {
    stop("Values of gradient are not strictly sorted!")
  }
  
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
importGradientSample <- function(sample, parameters) {
  
  if(!is.character(sample)) {
    stop("Invalid arguments!")
  }
  
  con <- file(paste0("sampledata/", sample), "r")
  on.exit({ close(con) })
  
  parameters$separator <- ","
  
  data <- importGradient(filehandle = con, 
                         importer = "csv", 
                         parameters = parameters)
  
  return(data)
  
}
